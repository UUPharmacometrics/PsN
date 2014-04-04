package nonmemrun::torque;

use include_modules;
use Moose;
use MooseX::Params::Validate;

extends 'nonmemrun';

has 'torque_queue' => ( is => 'rw', isa => 'Maybe[Str]' );

sub submit
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model => { isa => 'model', optional => 1 },
		 nm_version => { isa => 'Str', optional => 1 },
		 queue_info => { isa => 'Ref', optional => 1 }
	);
	my $jobId = -1;
	my $model = $parm{'model'};
	my $nm_version = $parm{'nm_version'};
	my $queue_info = $parm{'queue_info'};

	$self->pre_compile_cleanup;

	my $command = $self->create_command;

  open(JOBSCRIPT, ">JobScript") or croak("Couldn't open Torque JobScript file for writing: $!");
  print JOBSCRIPT $command;
  close(JOBSCRIPT);

  my $jobname= "psn:" . $self->model->filename;
  $jobname =~ s/\ /_/g;
  my $prepend = '';
  if (defined $self->prepend_flags) {
    $prepend = ' ' . $self->prepend_flags . ' ';
  }

  my $queue_string = ' ';
  $queue_string = ' -q ' . $PsN::config->{'_'}->{'torque_queue'} . ' ' 
      if ($PsN::config->{'_'}->{'torque_queue'});
  $queue_string = ' -q ' . $self->torque_queue . ' ' if (defined $self->torque_queue);
  if (system('qsub ' . $prepend . ' -N ' . $jobname . $queue_string . ' JobScript > JobId')) {
	  my $error = "$!";
	  print "Torque submit failed.\nSystem error message: $error";
	  chomp($error);
	  system('echo ' . $error . ' > job_submission_error');

	  $jobId = -1;
	} else {
		open(JOBFILE, "JobId") or croak("Couldn't open torque JobId file for reading: $!");
		while (<JOBFILE>) {
			if (/(\d+.[0-9A-Za-z\-\.]*)/) {
				$jobId = $1;
			}
		}
		close(JOBFILE);
	}

	$self->job_id($jobId);
	return $jobId;
}


sub monitor
{
	my $self = shift;
	my $jobId = $self->job_id;

	carp("Checking Torque queue for $jobId");
	my $response = `qstat $jobId 2>&1`;

	carp("Result: (OUT+ERR) $response");
	if($response =~ /Unknown Job Id/ ){ # regexp to find finished jobs.
		# The job is completed by default
		return $jobId; # Return the jobId found.
	}
	elsif ($response =~ /Job id/) { # regexp to find running jobs
		# The job is not completed
		return 0;
	}
	else {
		# something else happened, FIXME: there should probably be something different here
		return 0;
	}
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
