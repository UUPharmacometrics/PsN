package nonmemrun::slurm;

use include_modules;
use Moose;
use MooseX::Params::Validate;


sub monitor
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		jobId => { isa => 'Int', optional => 1 }
	);
	my $jobId = $parm{'jobId'};

	#squeue -j 12345, --jobs

	#list only completed, cancelled... job with right id without header
	my $outp = `squeue -h --states CA,CD,F,NF,TO -j $jobId 2>&1`;
	if (defined $outp) {
		if ($outp =~ /(i|I)nvalid/) {
			#this is either because the job finished so long ago (MinJobAge)
			#that is has disappeared, 
			#or due to some Slurm error. We sleep for 3 sec to make sure there was not an error
			#due to too early polling, and then try again. If message persists then assume job
			#id will never be valid, i.e. finished. That definitely can happen.
			sleep(3);
			my $outp2 = `squeue -h --states CA,CD,F,NF,TO -j $jobId 2>&1`;
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

no Moose;
__PACKAGE__->meta->make_immutable;
1;
