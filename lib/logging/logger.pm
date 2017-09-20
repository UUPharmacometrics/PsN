package logging::logger;

use ext::Carp;
use Moose;
use MooseX::Params::Validate;

has 'name' => ( is => 'rw', isa => 'Str' );
has 'level' => ( is => 'rw', isa => 'Int', default => 30 );

sub set_level
{
    my $self = shift;
    my $level = shift;
    $self->level($level);
}

sub debug
{
    my $self = shift;
    my $msg = shift;
    $self->log(10, $msg);
}

sub info
{
    my $self = shift;
    my $msg = shift;
    $self->log(20, $msg);
}

sub warning
{
    my $self = shift;
    my $msg = shift;
    $self->log(30, $msg);
}

sub error
{
    my $self = shift;
    my $msg = shift;
    $self->log(40, $msg);
}

sub critical
{
    my $self = shift;
    my $msg = shift;
    $self->log(50, $msg);
}

sub log
{
    my $self = shift;
    my $level = shift;
    my $msg = shift;

    if ($self->level <= $level) {
        my $print_string = get_level_name($level) . ":" . $self->name . ":" . $msg . "\n";
        if ($level >= 50) {
            croak($print_string);
        } else {
            print $print_string;
        }
    }
}

sub get_level_name
{
    # Get the name of the level DEBUG, INFO, WARNING etc or Level n for inbetween levels
    my $level = shift;

    if ($level == 10) {
        return 'DEBUG';
    } elsif ($level == 20) {
        return 'INFO';
    } elsif ($level == 30) {
        return 'WARNING';
    } elsif ($level == 40) {
        return 'ERROR';
    } elsif ($level == 50) {
        return 'CRITICAL';
    } else {
        return "Level $level";
    }
}

1;
