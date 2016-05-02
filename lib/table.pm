package table;

# A class representing a generic table

use include_modules;
use Moose;
use MooseX::Params::Validate;

has 'columns' => ( is => 'rw', isa => 'ArrayRef[ArrayRef]', default => sub { [] } );
has 'header' => ( is => 'rw', isa => 'HashRef[Str]' , default => sub { {} });
has 'delimiter' => ( is => 'rw', isa => 'Str', default => '\s+');   # Delimiter of elements on a row
has 'skip_leading_whitespace' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'header_array' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );

sub set_header
{
    # Set the header from a string
	my $self = shift;
	my %parm = validated_hash(\@_,
		header => { isa => 'Str' },
	);
    my $header = $parm{'header'};


    my @array = $self->_split_row($header);
	$self->header_array(\@array);
    my %hash;
    my $i = 0;
    foreach my $e (@array) {
        if (not exists $hash{$e}) {
            $hash{$e} = $i++;
        }
		#we allow duplicate names, okay if table directly from nonmem
    }

    $self->header(\%hash);
}

sub get_header
{
	my $self = shift;
	#	return [sort {$self->header->{$a} <=> $self->header->{$b}} (keys %{$self->header})];
	return $self->header_array;
}


sub add_row
{
    # Add one row to the table from a string
	my $self = shift;
	my %parm = validated_hash(\@_,
		row => { isa => 'Str' },
	);
    my $row = $parm{'row'};

    my @array = $self->_split_row($row);

    my $i = 0;
    foreach my $value (@array) {
        push @{$self->columns->[$i]}, $value;
        $i++;
    }
}

sub get_column
{
    # Get reference to a column array given the column index or name
	my $self = shift;
	my %parm = validated_hash(\@_,
	    name => { isa => 'Str', optional => 1 },
		index => { isa => 'Str', optional => 1 },
	);
    my $name = $parm{'name'};
    my $index = $parm{'index'};

    if (not defined $name and not defined $index) {
        croak("One of name and index must be specified\n");
    }

    my $my_index = $index;
    if (defined $name) {
		unless (defined $self->header->{$name}){
			croak("header does not contain $name");
		}
        $my_index = $self->header->{$name};
    }
	if ($my_index >= scalar(@{$self->columns})){
		croak("no table column with index $my_index");
	}
	
    return $self->columns->[$my_index];
}

sub _split_row
{
    # Split a row and skip columns
    my $self = shift;
    my $row = shift;

    if ($self->skip_leading_whitespace) {
        $row =~ s/^\s+//;
    }

#    my @array = split "\Q" . $self->delimiter . "\E", $row; #warning
    my @array = split "\Q" . $self->delimiter , $row; # no warning

    return @array;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
