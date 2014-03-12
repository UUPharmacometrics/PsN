package nonmemrun::localunix;

use include_modules;
use POSIX ":sys_wait_h";
use Moose;
use MooseX::Params::Validate;

extends 'nonmemrun';

has '_nr_wait' => ( is => 'rw', isa => 'Int', default => 0 );

sub monitor
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		check_pid => { isa => 'Any' }
	);
	my $check_pid = $parm{'check_pid'};

	my $pid = waitpid($check_pid, WNOHANG);

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
		$pid = $check_pid;
	}

	return $pid;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
