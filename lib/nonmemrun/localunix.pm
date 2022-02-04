package nonmemrun::localunix;

use include_modules;
use POSIX ":sys_wait_h";
use Mouse;
use MouseX::Params::Validate;

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
    system("echo $command > nmqualcommand") if ($self->nmqual);

    my $pid;
    my $errmess;
    my $tries=2;
    for (my $i=0; $i<$tries; $i++){
        $pid = fork();
        if (defined $pid){
            last;
        }else{
            $errmess="$!";
        }
        if ($i<($tries-1)){
            ui->print(category=> 'all', message => 'Perl fork() failed:'.$errmess.
                      "\n".', sleep 30s and try again');
            sleep(30);
        }else{
            ui->print(category=> 'all', message => 'Perl fork() failed: '.$errmess."\n".
                      ', considering this model failed');
            system('echo ' . 'Perl fork\(\) failed: '.$errmess. '  > job_submission_error');
            $pid = -1;
        }
    }
    if ($pid == 0) {
        system($command);
        exit;
    }

    $self->job_id($pid);
    return $pid;
}

sub monitor
{
    my $self = shift;

    my $pid = waitpid($self->job_id, WNOHANG);

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
        $pid = $self->job_id;
    }

    return $pid;
}

1;
