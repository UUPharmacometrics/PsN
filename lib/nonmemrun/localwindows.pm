package nonmemrun::localwindows;

use include_modules;
use Moose;
use MooseX::Params::Validate;

has 'windows_process' => ( is => 'rw', isa => 'Win32::Process' );

sub monitor
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		check_pid => { isa => 'Any' }
	);
	my $check_pid = $parm{'check_pid'};

	require Win32::Process;
	require Win32;

	my $exit_code;

	# GetExitCode is supposed to return a value indicating
	# if the process is still running, however it seems to
	# allways return 0. $exit_code however is update and
	# seems to be nonzero if the process is running.

	$self->windows_process->GetExitCode($exit_code);

	if ($exit_code == 0) {
		return $check_pid;
	} else {
		return 0;
	}
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
