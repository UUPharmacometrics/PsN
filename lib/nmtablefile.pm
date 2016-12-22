package nmtablefile;

# A class representing a nonmem table file

use include_modules;
use Moose;
use MooseX::Params::Validate;
use nmtable;

has 'filename' => ( is => 'rw', isa => 'Str' );
has 'tables' => ( is => 'rw', isa => 'ArrayRef[nmtable]', default => sub { [] } );
has 'is_ext_file' => ( is => 'rw', isa => 'Bool', default => 0);
has 'has_evaluation' => ( is => 'rw', isa => 'Bool', default => 0);
has 'table_lookup' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'problem_lookup' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );

sub BUILD
{
    my $self = shift;

    if (defined $self->filename) {
        $self->read_nmtable(filename => $self->filename);
    } 
}

sub get_table
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		index => { isa => 'Int', optional => 1 },
		number => { isa => 'Int', optional => 1 },
		problem => { isa => 'Int', optional => 1 },
		subproblem => { isa => 'Int', optional => 1 },
	);
    my $index = $parm{'index'};
	my $number = $parm{'number'};
	my $problem = $parm{'problem'};
	my $subproblem = $parm{'subproblem'};

	if (defined $problem and defined $subproblem and 
		defined $self->problem_lookup->{$problem} and
		defined $self->problem_lookup->{$problem}->{$subproblem}){
		return $self->tables->[$self->problem_lookup->{$problem}->{$subproblem}];
	}
	if (defined $number and defined $self->table_lookup->{$number}){
		return $self->tables->[$self->table_lookup->{$number}];
	}
	if (defined $index and $index < scalar(@{$self->tables})){
		#works for -1 (last table) also
		return $self->tables->[$index];
	}
	return undef;
}


sub add_table
{
	my $self = shift;
	my $table = shift;

	push @{$self->tables}, $table;
	if (defined $table->table_number){
		$self->table_lookup->{$table->table_number} = (scalar(@{$self->tables})-1);
	}
	if (defined $table->problem and defined $table->subproblem){
		#if multiple $EST they will have same subproblem number, we will store the last one only
		$self->problem_lookup->{$table->problem}->{$table->subproblem} = (scalar(@{$self->tables})-1);
	}
}

sub read_nmtable
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		filename => { isa => 'Str' },
	);
    my $filename = $parm{'filename'};

    $self->filename($filename);

    open my $fh, '<', $filename or die("Could not open table $filename\n");

    my $table_row = <$fh>;
	
    TABLE: while (defined $table_row) {
        my $table = nmtable->new();
		if ($self->is_ext_file){
			if (($table_row =~ /\(Evaluation\)/) or ($table_row =~ /\(EVALUATION\)/)){
				$self->has_evaluation(1);
			}
		}
        $table->read_table_row(row => $table_row);
        my $header_row = <$fh>;
		if (not defined $header_row){ #table row without header row following
			$self->add_table($table);
			last TABLE;
		}elsif ($header_row =~ /^TABLE NO./) {   #Header row is actually new table row
            $table_row = $header_row;
            $self->add_table($table); #previously read table
            next TABLE;
		}
        $table->set_header(header => $header_row);

        my $row;

        ROW: while (1) {
            $row = <$fh>;
            if (not defined $row) {
				$self->add_table($table);
                last TABLE;
            }
            if ($row =~ /^TABLE NO./) {
                $table_row = $row;
				$self->add_table($table);
                last ROW;
            }
			if ($self->is_ext_file){
				next unless ($row =~ /\s*-100000000[0-6]/);
			}
            $table->add_row(row => $row);
        }
    }
    
    close $fh;
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
