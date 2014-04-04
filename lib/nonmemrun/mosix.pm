package nonmemrun::mosix;

use include_modules;
use POSIX ":sys_wait_h";
use Moose;
use MooseX::Params::Validate;

extends 'nonmemrun';

has '_nr_wait' => ( is => 'rw', isa => 'Int', default => 0 );
has 'nice' => ( is => 'rw', isa => 'Maybe[Int]' );
has 'display_iterations' => ( is => 'rw', isa => 'Bool', default => 0 ); 

sub submit
{
	my $self = shift;

	$self->pre_compile_cleanup;

	my $command = $self->create_command;

	if ($self->nice) {
		$command = 'nice -n '. $self->nice . " $command";
	}

	if (not $self->display_iterations) {
		$command .= ' >' . $self->nmfe_output_file;
	}

	my $pid = fork();
	if ($pid == 0) {
		$command = 'mosenv -e ' . $command;
		if (-e "/proc/self/lock") {
			open(my $fh, ">", "/proc/self/lock") || die "Could not unlock myself!\n";
			print $fh "0";
			close($fh);
		}
		system($command);
		if (-e "/proc/self/lock") {
			open(my $fh, ">", "/proc/self/lock") || die "Could not lock myself!\n";
			print $fh "1";
			close($fh);
		}
		exit;
	}

	$self->job_id($pid);
	return $pid;
}


sub monitor
{
	my $self = shift;
	my $jobId = $self->job_id;

	my $pid = waitpid($jobId, WNOHANG);

	# Waitpid will return $check_pid if that process has
	# finished and 0 if it is still running.

	if ($pid == -1) {
		# If waitpid return -1 the child has probably finished
		# and has been "Reaped" by someone else. We treat this
		# case as the child has finished. If this happens many
		# times in the same NM_runX directory, there is probably
		# something wrong and we die(). (I [PP] suspect that we
		# never/seldom reach 10 reruns in one NM_runX directory)

		$self->_nr_wait($self->_nr_wait + 1);

		if ($self->_nr_wait > 10) {
			croak("Nonmem run was lost\n");
		}
		$pid = $jobId;
	}

	return $pid;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
