package nmtable;

# A class representing a table output from NONMEM

use include_modules;
use Moose;
use MooseX::Params::Validate;
use table;

extends 'table'; 

has 'table_number' => ( is => 'rw', isa => 'Int' );
has 'method' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'goal_function' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'problem' => ( is => 'rw', isa => 'Maybe[Int]' );
has 'subproblem' => ( is => 'rw', isa => 'Maybe[Int]' );
has 'superproblem1' => ( is => 'rw', isa => 'Maybe[Int]' );
has 'iteration1' => ( is => 'rw', isa => 'Maybe[Int]' );
has 'superproblem2' => ( is => 'rw', isa => 'Maybe[Int]' );
has 'iteration2' => ( is => 'rw', isa => 'Maybe[Int]' );

sub read_table_row
{
    # Parses the TABLE NO. row
    my $self = shift;
	my %parm = validated_hash(\@_,
		row => { isa => 'Str' },
	);
    my $row = $parm{'row'};

    if ($row =~ /TABLE NO.\s+(\d+):\s*(.*):\s*Problem=(\d+)\s+Subproblem=(\d+)\s+Superproblem1=(\d+)\s+Iteration1=(\d+)\s+Superproblem2=(\d+)\s+Iteration2=(\d+)/) {
        $self->table_number($1);
        $self->problem($3);
        $self->subproblem($4);
        $self->superproblem1($5);
        $self->iteration1($6);
        $self->superproblem2($7);
        $self->iteration2($8);
	}elsif ($row =~ /TABLE NO.\s+(\d+):\s*(.*)\s*$/) {
        $self->table_number($1);
    }elsif ($row =~ /TABLE NO.\s+(\d+)/) {
        $self->table_number($1);
    }

    $self->_parse_method_string($2); 
}

sub _parse_method_string
{
    my $self = shift;
    my $string = shift;

    if ($string =~ /\s*(.*):\s*Goal Function=(.*)/) {
        $self->method($1);
        $self->goal_function($2);
    } elsif ($string !~ /:/) {
        $self->method($string);
    }
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
