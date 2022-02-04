package model::problem::record_order;

# Class to take care of the record_order for output of modelfiles.

use Mouse;
use MouseX::Params::Validate;
use List::Util qw(first);

has 'order' => ( is => 'rw', isa => 'ArrayRef[Str]' );


my @code_records = ( 'pred', 'pk', 'error', 'des', 'aes', 'aesinitial', 'mix', 'infn','tol');
my @parameter_records = ('theta','thetap','thetapv','omega','omegap','omegapd','sigma','sigmap','sigmapd');
# The rules
my %must_come_before = (
    'sizes' => [ 'problem' ],
    'abbreviated' => [@code_records],
    'subroutine' => [@code_records],
);


#must repeat what have above, i.e. if A must come before B then B must come after A

my %must_come_after = (
    'msfi' => [ 'data', 'input', 'bind' ],
    'prior' => [ 'data', 'input', 'bind' ],
    'contr' => [ 'data', 'input', 'bind','msfi','abbreviated' ],
    'bind' => [ 'input' ],
    'pred' => [ 'input','data','bind','subroutine', 'abbreviated','msfi','prior' ],
    'pk' => [ 'input','data','bind','subroutine','abbreviated','model' ],
    'error' => [ 'input','data','bind','subroutine','abbreviated','tol', 'model', 'pk', 'des', 'aes', 'aesinitial' ],
    'des' => ['input','data','bind','subroutine','abbreviated'],
    'aes' => ['input','data','bind','subroutine','abbreviated'],
    'aesinitial' => ['input','data','bind','subroutine','abbreviated'],
    'mix' => ['input','data','bind','subroutine','abbreviated'],
    'infn' => ['input','data','bind','subroutine','abbreviated','tol','model'],
    'tol' => ['input','data','bind','subroutine','abbreviated'],
    'theta' => [ @code_records ],
    'omega' => [ 'theta','thetap','thetapv',@code_records ],
    'sigma' => [ 'theta','thetap','thetapv','omega','omegap','omegapd',@code_records ],
    'thetap' => [ 'theta' ],
    'thetapv' => [ 'theta', 'thetap' ],
    'omegap' => [ 'omega' ],
    'omegapd' => [ 'omega', 'omegap' ],
    'sigmap' => [ 'sigma' ],
    'sigmapd' => [ 'sigma', 'sigmap' ],
    'simulation' => [@parameter_records,@code_records],
    'estimation' => ['simulation',@parameter_records,@code_records],
    'covariance' => ['simulation','estimation',@parameter_records,@code_records],
    'nonparametric' => ['simulation','estimation','covariance',@parameter_records,@code_records],
);
#no rule for table, will put it at the end

sub insert
{
    my $self = shift;
    my ( $record ) = pos_validated_list(\@_,
        { isa => 'Str' },
    );

    my $placed = 0;

    # Check if the record already has an order
    if (defined $self->_find_records(records => [ $record ])) {
        return;
    }

    # Must come before
    if (exists $must_come_before{$record}) {
        my $index = $self->_find_records(records => $must_come_before{$record}, find_last => 0); #find first
        if (defined $index) {
            splice @{$self->order}, $index, 0, $record;
            $placed = 1;
        }
    }

    # Must come after
    if (not $placed) {
        if (exists $must_come_after{$record}) {
            my $index = $self->_find_records(records => $must_come_after{$record}, find_last => 1);
            if (defined $index) {
                splice @{$self->order}, $index + 1, 0, $record;
                $placed = 1;
            }
        }
    }

    # Final rule. Place at the end.
    if (not $placed) {
        push @{$self->order}, $record;
    }
}

sub _find_records
{
    # Get the position of the first or last record
    my $self = shift;
    my %parm = validated_hash(\@_,
        records => { isa => 'ArrayRef' },
        find_last => { isa => 'Bool', default => 0 },
    );
    my $records = $parm{'records'};
    my $find_last = $parm{'find_last'};

    my $min = scalar(@{$self->order});
    my $max = -1;
    foreach my $r (@$records) {
        my $position = $self->_get_pos($r);
        if (defined $position) {
            if ($position < $min) {
                $min = $position;
            }
            if ($position > $max) {
                $max = $position;
            }
        }
    }

    if ($find_last) {
        if ($max == -1) {
            return undef;
        } else {
            return $max;
        }
    } else {
        if ($min == scalar(@{$self->order})) {
            return undef;
        } else {
            return $min;
        }
    }
}

sub _get_pos
{
    # Get the position of a record
    my $self = shift;
    my ( $record ) = pos_validated_list(\@_,
        { isa => 'Str' },
    );

    my $pos = first { $self->order->[$_] eq $record } 0 .. scalar(@{$self->order}) - 1;
    return $pos;
}

1;
