package model::problem::estimation;

use Moose;
use MooseX::Params::Validate;

extends 'model::problem::record';

sub renumber_msfo{
	my $self = shift;
	my %parm = validated_hash(\@_,
		numberstring => { isa => 'Str', optional => 0 }
		);
	my $numberstring = $parm{'numberstring'};

	my @options = defined($self->options) ? @{$self->options} : ();
	foreach my $opt (@options){
		if ($opt->name =~ /^\s*MSF/  and (defined $opt->value and $opt->value ne '')){
			my $line = $opt->value;
			$line =~ s/[0-9]+/$numberstring/ ;
			$opt->value($line);
		}
	}

}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
