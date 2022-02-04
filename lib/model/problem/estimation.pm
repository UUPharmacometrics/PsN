package model::problem::estimation;

use include_modules;
use Mouse;
use MouseX::Params::Validate;

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

sub is_eval_only
{
    # return true if estimation is MAXEVAL=0 for classical or EONLY>0 for DIRECT/IMP/IMPMAP/SAEM,
    # false otherwise (other methods cause return of undef until properly implemented)
    my $self = shift;

    my @options = defined($self->options) ? @{$self->options} : ();
    if ($self->is_classical) {
        my $maxeval = 1; # default is "a generous number"
        foreach my $opt (@options) {
            if (defined $opt and uc($opt->name) =~ 'MAXE') {
                $maxeval = uc($opt->value) if (defined $opt->value and length($opt->value)>0);
            }
        }
        return 1 if ($maxeval == 0);
    } else {
        my $method=$self->get_method;
        if ($method =~ /^DIR/ or $method =~ /^IMP/ or $method =~ /^SAE/) {
            my $eonly = 0; # default is to not only evaluate
            foreach my $opt (@options) {
                if (defined $opt and uc($opt->name) =~ 'EON') {
                    $eonly = uc($opt->value) if (defined $opt->value and length($opt->value)>0);
                }
            }
            return 1 if ($eonly > 0);
        } else {
            return undef;
        }
    }
    return 0;
}

1;
