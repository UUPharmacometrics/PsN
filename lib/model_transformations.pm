package model_transformations;

use include_modules;
use Cwd;
use model;
use PsN;
use MooseX::Params::Validate;


sub boxcox_etas
{
    # Boxcox transform all ETAs of model
    # Assume only one $PROBLEM
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        etas => { isa => 'ArrayRef', optional => 1 },       # An array of the etas to transform or unspecified for all etas
    );
    my $model = $parm{'model'};
	my $etas = $parm{'etas'};

    my $netas = $model->nomegas->[0];
    if (not defined $etas) {
        $etas = [1 .. $netas];
    }
    my $nthetas = $model->nthetas;

    # Transform ETAs
    for my $eta (@$etas) {
        for my $record (('pk', 'pred', 'error', 'des', 'aes', 'aesinitial', 'mix', 'infn')) {
		    if ($model->has_code(record => $record)) {  
			    my $code = $model->get_code(record => $record);
                for (my $i = 0; $i < scalar(@$code); $i++) {
                    $code->[$i] =~ s/(?<!\w)ETA\($eta\)/ETAT$eta/g;
                }
                $model->set_code(record => $record, code => $code);
            }
        }
	}

    # Prepend transformation code and add thetas
	my @code;
	my $code_record;
	if ($model->has_code(record => 'pk')) {
		@code = @{$model->get_code(record => 'pk')};
		$code_record = 'pk';
	} elsif ($model->has_code(record => 'pred')) {
		@code = @{$model->get_code(record => 'pred')};
		$code_record = 'pred';
	} else {
		croak("Neither PK nor PRED defined in " . $model->filename . "\n");
	}

    my $next_theta = $nthetas + 1;
    for my $i (@$etas) {
        my $line = "ETAT$i = (EXP(ETA($i))**THETA($next_theta) - 1) / (THETA($next_theta))";
        $next_theta++;
        unshift @code, $line;
        $model->add_records(type => 'theta', record_strings => [ '$THETA (-3, 0.01, 3)']); 
    }

    $model->set_code(record => $code_record, code => \@code);
}

sub remove_iiv
{
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
    );
    my $model = $parm{'model'};

    my $omegas = $model->problems->[0]->omegas;

    for (my $i = 0; $i < scalar(@$omegas); $i++) {
        my $last = 0;
        if ($i == scalar(@$omegas) - 1) {
            $last = 1;
        }
        unless ($omegas->[$i]->same or (not $last and $omegas->[$i + 1]->same) or $omegas->[$i]->fix) {    # Keep if IOV or block FIX
            if ($omegas->[$i]->type eq 'BLOCK') {
                $omegas->[$i]->fix(1);
            }
            for my $option (@{$omegas->[$i]->options}) {
                $option->init(0);
                if ($omegas->[$i]->type ne 'BLOCK') {
                    $option->fix(1);
                }
            }
        }
    }
}

1;
