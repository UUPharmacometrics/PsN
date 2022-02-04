package model::problem::problem;

use Mouse;
use MouseX::Params::Validate;

extends 'model::problem::record';

sub add_comment
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        new_comment  => { isa => 'Str' },
    );
    my $new_comment = $parm{'new_comment'};

    my @print_order = @{$self->print_order};
    my @comments = @{$self->comment};
    my @options = defined($self->options) ? @{$self->options} : ();

    my $numopts = scalar(@options);

    #if have no original opts, add a dummy one to get new comment after $PROB instead of before
    if ($numopts == 0) {
        $self->_add_option(option_string => 'UPDATED');
        $numopts = 1;
    }

    push(@comments, "\n" . ';' . $new_comment);
    push(@print_order, $numopts);
    $comments[$#comments] .= "\n";

    $self->comment(\@comments);
    $self->print_order(\@print_order);
}

1;
