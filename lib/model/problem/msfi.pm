package model::problem::msfi;

use Moose;
use MooseX::Params::Validate;

extends 'model::problem::record';

sub renumber_msfi{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  numberstring => { isa => 'Str', optional => 0 }
		);
	my $numberstring = $parm{'numberstring'};

	#file name is always first option
	if (defined($self->options) and scalar(@{$self->options})>0){
		my $line = $self->options->[0]->name;
		#everything up to but not including optional dot
		$line =~ s/[0-9]+[^0-9.]*/$numberstring/ ;
		$self->options->[0]->name($line);
	}

}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
