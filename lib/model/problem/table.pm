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
			$line =~ s/tab[0-9]+/tab$numberstring/;
			$opt->value($line);
		}
	}

}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
