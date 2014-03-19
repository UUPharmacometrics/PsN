package nonmemrun::localwindows;

use include_modules;
use Moose;
use MooseX::Params::Validate;

extends 'nonmemrun';

has 'windows_process' => ( is => 'rw', isa => 'Win32::Process' );
has 'display_iterations' => ( is => 'rw', isa => 'Bool', default => 0 );

sub submit
{
	my $self = shift;

	$self->pre_compile_cleanup;
	my $nmfe_command = $self->create_nmfe_command;

	require Win32::Process;
	require Win32;
	sub ErrorReport{ print Win32::FormatMessage(Win32::GetLastError()); }
	my $proc;

	if (not $self->display_iterations) {
		$nmfe_command .= ' >' . $self->nmfe_output_file;
	}

	Win32::Process::Create($proc, $self->full_path_nmfe, $nmfe_command, 0, $Win32::Process::NORMAL_PRIORITY_CLASS, '.') || die ErrorReport();
	$self->windows_process($proc);

	my $pid = $proc->GetProcessID();
	$self->job_id($pid);
	return $pid;
}

sub monitor
{
	my $self = shift;
	my $jobId = $self->job_id;

	require Win32::Process;
	require Win32;

	my $exit_code;

	# GetExitCode is supposed to return a value indicating
	# if the process is still running, however it seems to
	# allways return 0. $exit_code however is update and
	# seems to be nonzero if the process is running.

	$self->windows_process->GetExitCode($exit_code);

	if ($exit_code == 0) {
		return $jobId;
	} else {
		return 0;
	}
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
