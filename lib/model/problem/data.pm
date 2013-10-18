package model::problem::data;

use Moose;
use MooseX::Params::Validate;

extends 'model::problem::record';

has 'ignoresign' => ( is => 'rw', isa => 'Str' );
has 'ignore_list' => ( is => 'rw', isa => 'ArrayRef[Str]' );

sub BUILD
{
	my $this  = shift;

	foreach my $option ( @{$this->options} ) {
		if ( defined $option and $option->name eq 'IGNORE') {
			my $value = $option->value;
			chomp( $value );
			if ( $value =~ /\(*.\)/ ) {
				$value =~ s/\(//g;
				$value =~ s/\)//g;
				my @raw_list = split(',',$value);
				$this->ignore_list([]) unless defined $this->ignore_list;		# In case the reference is undef
				push( @{$this->ignore_list}, @raw_list );
			} else {
				$this->ignoresign($value);
			}
		}
	}
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
