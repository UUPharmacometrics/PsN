package nonmemrun::slurm;

use include_modules;
use Mouse;
use MouseX::Params::Validate;
use OSspecific;
use Cwd;

extends 'nonmemrun';

has 'partition' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'account' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'cluster' => ( is => 'rw', isa => 'Maybe[Str]' );       # The SLURM cluster
has 'check_modfile' => ( is => 'rw', isa => 'Bool', default => 1 );


sub submit
{
    my $self = shift;
    my $jobId = -1;

    $self->pre_compile_cleanup;

    my $jobname = $self->model->filename;
    $jobname = 'psn_' . $jobname if ($jobname =~ /^[0-9]/);

    #cwd default in slurm
    # -J jobname
    #need to check translation for -b y

    my $flags = ' -J ' . $jobname;
    $flags .= ' -o ' . $self->nmfe_output_file . ' -e ' . $self->nmfe_output_file . ' ';
    if (defined $self->account) {
        $flags .= ' -A ' . $self->account;
    } else {
        if ($PsN::config -> {'default_options'} -> {'uppmax'}) {
            croak("slurm account must be defined on uppmax");
        }
    }
    if (defined $self->max_runtime) {
        #Acceptable time formats include #minutes",
        #minutes:seconds", #hours:minutes:seconds", #days-hours",
        #days-hours:minutes¡ and ´days-hours:minutes:seconds".
        unless (($self->max_runtime =~ /^[0-9]+$/) or
                ($self->max_runtime =~ /^[0-9]+\:[0-9]+\:[0-9]+$/) or
                ($self->max_runtime =~ /^[0-9]+\-[0-9]+$/)) {
            croak("max_runtime must have format minutes, hours:minutes:seconds, or days-hours");
        }
        $flags .= ' -t ' . $self->max_runtime ;
    }
    if (defined $self->partition) {
        $flags .= ' -p '.$self->partition;
    }
    #at most 3GB RAM
    if ($PsN::config->{'default_options'}->{'uppmax'}) {
        my $ntasks = 1;
        if ($self->nodes > 0) {
            $ntasks = $self->nodes;
        }
        $flags .= ' -n '.$ntasks;
    }

    if (defined $self->send_email and defined $self->email_address) {
        if ($self->send_email eq 'ALL') {
            $flags .= ' --mail-user='.$self->email_address . ' --mail-type=ALL ';
        } else {
            $flags .= ' --mail-user='.$self->email_address . ' --mail-type=END ';
        }
    }

    #-t "hours:minutes:seconds", "days-hours"

    #sbatch -J psn:pheno.mod -o nmfe.output -e nmfe.output -p core -n 1 -t 0:3:0 -A p2011021 /bubo/sw/apps/nonmem/nm_7.1.0_g_reg/run/nmfe7 pheno.mod pheno.lst -background

    if (defined $self->prepend_flags) {
        $flags = ' ' . $self->prepend_flags . $flags;
    }

    if (defined $self->cluster) {
        $flags = ' -M ' . $self->cluster . $flags;
    }

    my $command = $self->create_command;
    my $modfile = 'psn.mod';
    my $lstfile = 'psn.lst';

    if ($self->check_modfile){
        unless (-e $modfile){
            #this is a fatal error
            my $work_dir = cwd();
            print "\n Error: cannot run ".$self->model->full_name." in proper NM_run\n".
                "$modfile does not exist in $work_dir before slurm job submission,\n".
                "mkdir or chdir in PsN must have failed. Considering this model failed.\n".
                "Please report this error\n";
            $jobId = -1;
            $self->job_id($jobId);
            return $jobId;
        }
    }
    system("echo sbatch $flags $command \"2>&1\" > sbatchcommand");

    for (my $i = 0; $i < 4; $i++) {
        #make sure input file psn.mod is visible, i.e. files are synced, before calling nmfe
        #also make sure psn.lst is NOT here, i.e. moving of old output is finished
        my $outp = readpipe("sbatch $flags 2>&1 <<EOF
#!/bin/bash  -l
printenv | sed \'/^HOSTNAME=/!d; s///;q\' > hostname

for J in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
do
if test -f $lstfile
then
echo \"found $lstfile, wait\"
sleep 0.5
else
echo \"did not find $lstfile, ok\"
break
fi
done

for J in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
do
if test -f $modfile -a -r $modfile
then
echo \"found $modfile, ok\"
break
else
echo \"did not find $modfile, wait\"
sleep 0.5
fi
done
$command
EOF
");
        chomp($outp);
        my $sleep_time;
        if ($i == 3) {
            $sleep_time = 20;
        } else {
            $sleep_time = 1;
        }

        if ($outp =~ /Submitted batch job (\d+)/) {
            $jobId = $1;
            last;
        } elsif($outp =~ /Socket timed out/) {
            #try again. jobId is -1 by initiation
            sleep($sleep_time);
            next;
        } elsif($outp =~ /Invalid user id/) {
            #try again. jobId is -1 by initiation
            sleep($sleep_time);
            next;
        } else {
            print "Slurm submit failed.\nSystem error message: $outp\nConsidering this model failed.\n";
            system('echo ' . $outp . '  > job_submission_error');
            $jobId = -1;
            last;
        }
    }
    system('echo '.$jobId.' > jobId');

    $self->job_id($jobId);
    return $jobId;
}

sub monitor
{
    my $self = shift;
    my $jobId = $self->job_id;

    #squeue -j 12345, --jobs

    #list only completed, cancelled... job with right id without header allowing for any width numbers without truncation
    my $cluster_option = "";
    if (defined $self->cluster) {
        $cluster_option = "-M " . $self->cluster . " ";
    }
    my $outp = `squeue $cluster_option-h --states CA,CD,F,NF,TO -j $jobId -o" %i " 2>&1`;
    $outp =~ s/CLUSTER: .*\n//;
    if (defined $outp) {
        if ($outp =~ /(i|I)nvalid/) {
            #this is either because the job finished so long ago (MinJobAge)
            #that is has disappeared,
            #or due to some Slurm error. We sleep for 3 sec to make sure there was not an error
            #due to too early polling, and then try again. If message persists then assume job
            #id will never be valid, i.e. finished. That definitely can happen.
            sleep(3);
            my $outp2 = `squeue $cluster_option-h --states CA,CD,F,NF,TO -j $jobId -o" %i " 2>&1`;
            $outp2 =~ s/CLUSTER: .*\n//;
            if (defined $outp2) {
                if ($outp2 =~ /(i|I)nvalid/) {
                    return $jobId; # Give up. This job is finished since not in queue
                } elsif ($outp2 =~ /^\s*$jobId\s/) {
                    #assume jobId first item in string, possibly with leading whitespace
                    #job is finished
                    return $jobId;
                } else {
                    return 0; # Assume some error message, not finished
                }
            } else {
                return 0; # not finished since empty output when asking for finished jobs
            }
        } elsif ($outp =~ /^\s*$jobId\s/) {
            #assume jobId first item in string, possibly with leading whitespace
            #job in set of finished ones
            return $jobId;
        } else {
            return 0; #Assume some error message, not finished
        }
    } else {
        return 0; # Not finished since empty output
    }
}

1;
