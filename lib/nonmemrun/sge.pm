package nonmemrun::sge;

use include_modules;
use Moose;
use MooseX::Params::Validate;

extends 'nonmemrun';

has 'resource' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'queue' => ( is => 'rw', isa => 'Maybe[Str]' );


sub submit
{
	my $self = shift;
	my $jobId = -1;

	$self->pre_compile_cleanup;

	#only support nmfe here, not nmqual

	my $command = $self->create_command;

	my $jobname = $self->model->filename;
	$jobname = 'psn_' . $jobname if ($jobname =~ /^[0-9]/);

	my $flags = ' -N ' . $jobname . ' -j y -cwd -b y';
	if (defined $self->prepend_flags) {
		$flags = ' ' . $self->prepend_flags . $flags;
	}
	
	my $submitstring = $flags . 
		($self->resource ? ' -l ' . $self->resource . ' ' : ' ') .
		($self->queue ? '-q ' . $self->queue . ' ' : ' ') . $command;

	system('echo qsub '.$submitstring.' > qsubcommand');

	my $outp = readpipe("qsub $submitstring 2>&1");
	chomp($outp);

	if ($outp =~ /^Your job (\d+)/) {
		$jobId = $1;
	} else {
		print "SGE submit failed.\nSystem error message: $outp\nConsidering this model failed.\n";
		system('echo ' . $outp . ' > job_submission_error');
		$jobId = -1;
	}
	system('echo '.$jobId.' > jobId');

	$self->job_id($jobId);
	return $jobId;
}


sub monitor
{
	my $self = shift;
	my $jobId = $self->job_id;

	my $response = `qstat -j $jobId 2>&1`;

	if ($response =~ /Following jobs do not exist/) {
		return $jobId;
	} elsif ($response =~ /^usage: qstat/) {
		return $jobId;
	}

	return 0;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
