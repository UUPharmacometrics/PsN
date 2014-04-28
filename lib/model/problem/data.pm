package model::problem::data;

use Moose;
use MooseX::Params::Validate;
use include_modules;

extends 'model::problem::record';

has 'ignoresign' => ( is => 'rw', isa => 'Str' );
has 'ignore_list' => ( is => 'rw', isa => 'ArrayRef[Str]' );

sub BUILD
{
	my $this  = shift;

	foreach my $option ( @{$this->options} ) {
		if ( defined $option and ($option->name eq 'IGNORE' or index('IGNORE',$option ->name ) == 0)) {
			my $value = $option->value;
			chomp( $value );
			if (defined $value and length($value)>0){
				if ( $value =~ /\(*.\)/ ) {
					$value =~ s/\(//g;
					$value =~ s/\)//g;
					my @raw_list = split(',',$value);
					$this->ignore_list([]) unless defined $this->ignore_list;		# In case the reference is undef
					push( @{$this->ignore_list}, @raw_list );
				} else {
					if (($value =~ /^'/) or ($value =~ /^"/)){
						croak("PsN does not support quoted IGNORE signs in \$DATA");
					}
					$this->ignoresign($value);
				}
			}else{
				print "\nWarning: empty value of IGNORE option in \$DATA\n";
			}
		}
	}
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
