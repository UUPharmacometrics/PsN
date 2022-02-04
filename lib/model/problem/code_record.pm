package model::problem::code_record;

use Mouse;
use MouseX::Params::Validate;

extends 'model::problem::record';

has 'code' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'pseudo_assignments' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'pre_verbatim' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'verbatim_last' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'verbatim_first' => ( is => 'rw', isa => 'ArrayRef[Str]' );

sub _format_record
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        number_format => { isa => 'Maybe[Int]', optional => 1 },
    );
    my $number_format = $parm{'number_format'};
    my @formatted;

    my $fname;
    if ( defined $self->verbatim_first
      or defined $self->pseudo_assignments
      or defined $self->pre_verbatim
      or defined $self->code
      or defined $self->verbatim_last ) {
        my @class_names = split('::',ref($self));
        $fname = uc(pop(@class_names));
        $fname = "\$".$fname; #and then prepend to $formatted[0] at the very end so that do not get line break
    }

    if ( defined $self->pseudo_assignments ) {
        # can be put after first verbatim (only non-verbatim, non-pseudo-assignment must preceed it), but very first is simpler
        push( @formatted, @{$self->pseudo_assignments} );
    }
    if ( defined $self->pre_verbatim ) {
        push( @formatted, @{$self->pre_verbatim} );
    }
    if ( defined $self->verbatim_first ) {
        push( @formatted, '"FIRST' );
        push( @formatted, @{$self->verbatim_first} );
    }
    if ( defined $self->code ) {
        push( @formatted, @{$self->code} );
    }
    if ( defined $self->verbatim_last ) {
        push( @formatted, '"LAST' );
        push( @formatted, @{$self->verbatim_last} );
    }
    if (scalar(@formatted)>0){
            my $space;
            if ($formatted[0] !~ /^\s/ and $formatted[0] ne "") {
                $space = ' ';
            } else {
                $space = '';
            }
            $formatted[0] = $fname . $space . $formatted[0];
    }

    return \@formatted;
}

sub _read_options
{
    my $self = shift;

    my $in = 0;
    if ( defined $self->record_arr ) {
        $self->code([]);
        my ( $first, $last, $have_first, $have_nonverbatim ) = ( 0, 0, 0, 0 );
        my @pre_verbatim = (); # pre-"FIRST verbatim statement
        for ( @{$self->record_arr} ) {
            if( /^\s*\$\w+/ ) {
                # get rid of $RECORD and unwanted spaces
                s/^\s*\$\w+//;
                # skip empty line (if $RECORD was on lone line)
                #next if( $_ eq '');
            }
            if ( /\" (\w+) = EVTREC\((\d+),(\d+)\)/ ) {
                next;
            }
            if ( (! $have_nonverbatim and /^\s*\(/ && /\)\s*$/) or /^\s*CALLFL\s*=/ or /^\s*COMRES\s*=/ ) {
                # found pseudo-assignment statement: (..) or CALLFL=.. or COMRES=..
                # this is NOT code and should not be treated as such
                # (exception: after non-verbatim code; likely expected to stay put)
                $self->pseudo_assignments([]) unless defined $self->pseudo_assignments;
                push( @{$self->pseudo_assignments}, $_ );
                next;
            }
            if( ! /^\"\s*/ ) {
                # pseudo-assignments here after should not move
                $have_nonverbatim = 1;
            } else {
                if( /^\"\s*FIRST/ ) {
                    $first = 1;
                    $have_first = 1;
                    next;
                }
                if( /^\"\s*LAST/ ) {
                    $first = 0;
                    $last  = 1;
                    next;
                }
            }
            if( $first or $last ) {
                if( /^\"/ ) {
                    if( $first ) {
                        $self->verbatim_first([]) unless defined $self->verbatim_first;
                        push( @{$self->verbatim_first}, $_ );
                    } else {
                        $self->verbatim_last([]) unless defined $self->verbatim_last;
                        push( @{$self->verbatim_last}, $_ );
                    }
                } else {
                    $first = 0;
                    $last  = 0;
                    push @{$self->code}, $_;
                }
            } else {
                if ($have_first) {
                    push @{$self->code}, $_;
                } else {
                    push (@pre_verbatim,$_);
                }
            }
        }
        if ($have_first) {
            # keep pre-code separate if there's a FIRST
            $self->pre_verbatim([]) unless defined $self->pre_verbatim;
            push( @{$self->pre_verbatim}, @pre_verbatim);
        } else {
            # regard all collected code as ordinary code
            unshift @{$self->code}, @pre_verbatim;
        }
    }
}

1;
