package model::problem::table;

use Moose;
use MooseX::Params::Validate;

extends 'model::problem::record';

sub renumber_file{
	my $self = shift;
	my %parm = validated_hash(\@_,
		numberstring => { isa => 'Str', optional => 0 }
		);
	my $numberstring = $parm{'numberstring'};

	my @options = defined($self->options) ? @{$self->options} : ();
	foreach my $opt (@options){
		if ($opt->name =~ /^\s*FIL/  and (defined $opt->value and $opt->value ne '')){
			my $line = $opt->value;
			#everything up to but not including optional dot
			$line =~ s/[0-9]+[^0-9.]*/$numberstring/ ;
			$opt->value($line);
		}
	}

}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
