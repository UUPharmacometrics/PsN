package model::problem::record_order;

# Class to take care of the record_order for output of modelfiles.
 
use Moose;
use MooseX::Params::Validate;
use List::Util qw(first);

has 'order' => ( is => 'rw', isa => 'ArrayRef[Str]' );

# The rules
my %must_come_before = (
    'sizes' => [ 'problem' ],
    'simulation' => [ 'estimation' ],
    'abbreviated' => [ 'pred', 'pk', 'error', 'des', 'aes', 'aesinitial', 'mix', 'infn' ],
    'subroutines' => [ 'pred', 'pk', 'error' ],
);

my %must_come_after = (
    'msfi' => [ 'data', 'input', 'bind' ],
    'prior' => [ 'data', 'input', 'bind' ],
    'bind' => [ 'input' ],
    'pred' => [ 'subroutines', 'input' ],
    'pk' => [ 'subroutines', 'input', 'model' ],
    'error' => [ 'subroutines', 'input', 'model', 'pk' ],
);

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
        my $index = $self->_find_records(records => $must_come_before{$record});
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

no Moose;
__PACKAGE__->meta->make_immutable;
1;
