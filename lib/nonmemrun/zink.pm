package nonmemrun::zink;

use include_modules;
use Moose;
use MooseX::Params::Validate;

extends 'nonmemrun';

sub submit
{
	my $self = shift;
	my $jobId = -1;

	$self->pre_compile_cleanup;
	my $command = $self->create_command;

  require File::Temp;# qw/tempfile tempdir/;
  require Sys::Hostname;
  require LockFile::Simple;# qw/lock unlock trylock/;  # Non-standard module
  
  # Specifies the top level directory of the Zink directory structure. Should be specified via psn.conf
  my $ZinkDir = $PsN::config->{'_'}->{'zink_dir'};

  # Specify the queing directory/Job Specification drop zone.
  my $ZinkJobDir = $ZinkDir . "/ZinkJobs";
  
  # $JobName: Name of job. Default could be model file name.
  # $JobPriority: Priority of job. Between 0-5 (0=low). Default should be 3.
  # $ExePath: Directory in which the run is to be executed.
  # $Command: String with command to be executed, e.g. "nmfe6 run1.mod run1.lst"
  
  my $host = Sys::Hostname::hostname();

  my $path = getcwd();
  my $jobname = $self->model->filename;
  (my $FH, $jobId) = File::Temp::tempfile("$host-XXXXXXXXXXXXXXXXX", DIR => $ZinkJobDir, SUFFIX => '.znk');
  LockFile::Simple::lock $jobId;
  print $FH "SUBMITHOST: $host\n";
  print $FH "JOBNAME:  $jobname\n";
  print $FH "PRIORITY: 3\n";
  print $FH "EXEPATH: $path\n";
  print $FH "COMMAND: $command\n";
  close $FH;
  LockFile::Simple::unlock $jobId;
  $jobId = OSspecific::nopath($jobId);

	$self->job_id($jobId);
	return $jobId;
}

sub monitor
{
	my $self = shift;
	my $jobId = $self->job_id;

  ## Specifies the top level directory of the Zink directory structure. Should be specified via psn.conf
  my $ZinkDir = $PsN::config->{'_'}->{'zink_dir'};
  
  ## Specify the queing directory/Job Specification drop zone
  my $ZinkDoneDir = $ZinkDir . "/ZinkDone";

  if (-e "$ZinkDoneDir/$jobId") {
    return $jobId;
  } else {
    return 0;
  }
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
