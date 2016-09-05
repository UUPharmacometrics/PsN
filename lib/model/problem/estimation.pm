package model::problem::estimation;

use include_modules;
use Moose;
use MooseX::Params::Validate;

extends 'model::problem::record';

sub BUILD
{
    my $self = shift;

    # Warn if FORMAT|DELIM has a comma as separator
    # as this would not work well with the PsN parser
    if (defined $self->options) {
        for my $option (@{$self->options}) {
            if ($option->name =~ /FORMAT|FORMA|FORM|FOR|DELIM|DELI|DEL/) {
                if ($option->value =~ /^,/) {
                    warn_once("est_format_comma", "*** Warning ***\n" .
                       "The delimeter for FORMAT in \$ESTIMATION was set to \",\" (comma).\n" .
                       "PsN will not be able to parse any results files most probably\n" .
                       "causing a crash or undefined behaviour.\n\n");
                }
            }
        }
    }
}

sub get_method
{
	my $self = shift;

	my $method='0'; #this is the default, FO
	my @options = defined($self->options) ? @{$self->options} : ();
	foreach my $opt (@options){
		if (defined $opt and ((uc($opt->name) eq 'METHOD') || index('METHOD',uc($opt ->name )) == 0)){
			$method = uc($opt->value) if (defined $opt->value and length($opt->value)>0);
		}
	}
	return $method;
}

sub accepts_mceta
{
	my $self = shift;

	my $method=$self->get_method;
	my $accepts_mceta;
	if ($method eq '0' or
		$method =~ /^ZE/ or
		$method =~ /^FO/ or
		$method =~ /^CON/ or
		$method eq '1' or
		$method =~ /^HYB/ or
		$method =~ /^ITS/ or
		$method =~ /^IMP/ or
		$method =~ /^MAP/ 
 		){
		$accepts_mceta = 1;
	}else{
		#BAYES,SAEM,DIRECT
		$accepts_mceta = 0;
	}
	return $accepts_mceta;
}

sub is_classical
{
	my $self = shift;

	my $method=$self->get_method;

	my $classical;
	if ($method eq '0' or
		$method =~ /^ZE/ or
		$method =~ /^FO/ or
		$method =~ /^CON/ or
		$method eq '1' or
		$method =~ /^HYB/ 
		){
		$classical = 1;
	}else{
		$classical = 0;
	}
	return $classical;
}

sub renumber_msfo
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		numberstring => { isa => 'Str', optional => 0 }
		);
	my $numberstring = $parm{'numberstring'};

	my @options = defined($self->options) ? @{$self->options} : ();
	foreach my $opt (@options){
		if ($opt->name =~ /^\s*MSF/  and (defined $opt->value and $opt->value ne '')){
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
