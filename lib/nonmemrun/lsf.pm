package nonmemrun::lsf;

use include_modules;
use Moose;
use MooseX::Params::Validate;

extends 'nonmemrun';

has 'lsf_job_name' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'lsf_project_name' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'lsf_queue' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'lsf_resources' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'lsf_ttl' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'lsf_sleep' => ( is => 'rw', isa => 'Maybe[Int]', default => 3 );
has 'lsf_options' => ( is => 'rw', isa => 'Maybe[Str]' );

sub submit
{
	my $self = shift;
	my $jobId = -1;

	$self->pre_compile_cleanup;

  #only support nmfe here, not nmqual or PsN compile

  my $jobname = $self->model->filename;
  $jobname = 'psn_' . $jobname if ($jobname =~ /^[0-9]/);
  $jobname = $self->lsf_job_name if (defined $self->lsf_job_name);

	my $command = $self->create_command;

  open(SUB, '>lsf_jobscript');
  print SUB ("#BSUB -J $jobname\n");
  print SUB ("#BSUB -e lsf_stderr_stdout\n");
  print SUB ("#BSUB -o lsf_stderr_stdout\n");
  print SUB ("#BSUB -q " . $self->lsf_queue . "\n")
      if (defined $self->lsf_queue);
  print SUB ("#BSUB -P " . $self->lsf_project_name . "\n")
      if (defined $self->lsf_project_name);
  print SUB ("#BSUB -c " . $self->lsf_ttl . "\n")
      if (defined $self->lsf_ttl);
  print SUB ("#BSUB -R " . $self->lsf_resources . "\n")
      if (defined $self->lsf_resources);

	#TODO add loop here to wait for input files???
  print SUB ("$command\n");

  close(SUB);

  my $submitstring = 'bsub ' . $self->lsf_options . ' < lsf_jobscript 2>&1'; 

	#TODO add loop here to handle transient user id errors etc by retrying a few times
  my $lsf_out = readpipe("$submitstring");
  if ($lsf_out =~ /Job \<(\d+)\> is submitted/) {
    $jobId = $1;
  } else {
	  chomp($lsf_out);
    open(ERR, '>job_submission_error');
    print ERR ("COMMAND: $submitstring\n");
    print ERR ("SYSTEM RESPONSE: $lsf_out\n");
    print ERR ("RESULT: bsub command was not successful, could not submit nmfe run\n");
    close(ERR);
    ui->print(category => 'all', message  => $lsf_out, newline => 1);
    ui->print(category => 'all', message  => "bsub command was not successful, could not submit nmfe run", newline => 1);
    #now jobId is -1, handled outside, setting job to failed and never running bjobs.
  }

  sleep($self->lsf_sleep) if (defined $self->lsf_sleep);

	$self->job_id($jobId);
	return $jobId;
}


sub monitor
{
	my $self = shift;
	my $jobId = $self->job_id;

	my $string = "bjobs $jobId 2>&1";
	my $stdout = `$string`;

	# /m flag in regex:
	#Treat string as multiple lines. That is, change "^" and "$" from matching 
	#the start or end of the string to matching the start or end of any line 
	#anywhere within the string. Skip /m when looking for DONE, unless using also
	#jobID?

	if ($stdout =~ /DONE/m) {
		return $jobId; # Return the jobId found.
	} elsif (($stdout=~/is not found/) or
		($stdout =~ /illegal option/) or
		($stdout =~ /Illegal job ID/) or
		($stdout =~ /No unfinished job found/)){
		ui -> print(category => 'all', message  => $stdout, newline => 1);
		ui -> print(category => 'all', message  => "lsf run error, jobID $jobId not recognized by system:", newline => 1);
		return $jobId; # Return the jobId so that do not get infinite loop, 
		#let restart_needed detect and handle error (no psn.lst, no NMtran etc)
	} else {
		return 0;
	}
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
