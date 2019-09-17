package model_approximations;

use strict;
use warnings;
use include_modules;
use MooseX::Params::Validate;
use model_transformations;
use code;

sub derivatives_model
{
    # Create a model that can generate the first order derivatives
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
    );
    my $model = $parm{'model'};

    my $derivatives_model = $model;#->copy(filename => 'derivatives.mod', write_copy => 0);

    # Check if D_EPSETA in input. If so we need to rename the table output columns by appending an underscore
    my $column_names = $derivatives_model->problems->[0]->columns_list();
    my $depseta_suffix = '';
    for my $col (@$column_names) {
        if ($col =~ /^D_EPSETA/) {
           $depseta_suffix = '_';
           last;
       }
    }

    #can look for ADVAN<any number> this way
    my ($advan, $junk) = $derivatives_model->problems->[0]->_option_val_pos(
        record_name => 'subroutine',
        name => 'ADVAN',
        exact_match => 0
    );
    my $have_advan = scalar(@{$advan}) > 0;

    my $code_record_name;
    my $H;
    if ($have_advan) {
        # We have and ADVAN option in $SUBROUTINE, get $ERROR code
        $code_record_name = 'error';
        $H = 'HH';
    } else {
        # No ADVAN subroutine, we should modify $PRED code
        $code_record_name = 'pred';
        $H = 'H';
    }

    my $nETA = $derivatives_model->nomegas(with_correlations => 0, with_same => 1)->[0];
    my $nEPS = $derivatives_model->nsigmas(with_correlations => 0, with_same => 1)->[0];

    my @tablestrings;
    for (my $j = 1; $j <= $nEPS; $j++) {
        if ($j < 10) {
            push(@tablestrings, "H0${j}1");
        } else {
            push(@tablestrings, "H${j}1");
        }
    }

    for (my $i = 1; $i <= $nETA; $i++) {
        if ($i < 10) {
            push(@tablestrings, "G0${i}1");
        } else {
            push(@tablestrings, "G${i}1");
        }
    }

    # Get code array reference, so we can update the code inplace.
    my @abbreviated_code;
    my @verbatim_code;
    for (my $i = 1; $i <= $nEPS; $i++) {
        for (my $j = 1; $j <= $nETA; $j++) {
            push @abbreviated_code, "D_EPSETA${i}_$j$depseta_suffix = 0";
            push(@verbatim_code, "D_EPSETA${i}_$j$depseta_suffix=$H($i," . ($j + 1) . ")");
            push(@tablestrings, "D_EPSETA${i}_$j$depseta_suffix");
        }
    }

    model_transformations::append_code(model => $derivatives_model, code => \@abbreviated_code, record => $code_record_name);
    code::append_verbatim_code(model => $derivatives_model, code_record => $code_record_name, pos => 'LAST', code => \@verbatim_code);

    push(@tablestrings, 'NOPRINT','NOAPPEND','ONEHEADER');
    push(@tablestrings, 'FILE=derivatives.tab');
    push(@tablestrings, 'FORMAT=s1PE15.8');
    $derivatives_model->add_records(type => 'table', record_strings => \@tablestrings);

    return $derivatives_model;
}

sub linearize_error_terms
{
    # Create the error terms for a linearized model
    my %parm = validated_hash(\@_,
        neta => { isa => 'Int' },
        neps => { isa => 'Int' },
    );
    my $neta = $parm{'neta'};
    my $neps = $parm{'neps'};

    my @code;
    for (my $i = 1; $i <= $neps; $i++) {
        push @code, "ERR${i}_0 = D_EPS_${i}";
        for (my $j = 1; $j <= $neta; $j++) {
            push @code, "ERR${i}_${j} = D_EPSETA${i}_${j} * (ETA(${j}) - OETA${j})";
        }
    }

    return \@code;
}

sub linearize_covariate_error_terms
{
    # Create the covariate error terms for a linearized model
    my %parm = validated_hash(\@_,
        neps => { isa => 'Int' },
        eta_parameter => { isa => 'HashRef' },      # Hash from eta-number to parameter name
    );
    my $neps = $parm{'neps'};
    my $eta_parameter = $parm{'eta_parameter'};

    my @code;
    my @keys = sort keys %$eta_parameter;

    for (my $i = 1; $i <= $neps; $i++) {
        for my $j (@keys) {
            my $param = $eta_parameter->{$j};
            next if not defined($param) or ($param eq "ETA$j");
            push @code, "ERRC${i}_${j} = D_EPSETA${i}_${j} * OGK_$param * (GZ_$param - OGZ_$param)";
        }
    }

    return \@code;
}

sub linearize_error_sums
{
    # Sum error terms for each epsilon
    my %parm = validated_hash(\@_,
        neps => { isa => 'Int' },
        neta => { isa => 'Int' },
        eta_parameter => { isa => 'HashRef' },      # Hash from eta-number to parameter name
    );
    my $neps = $parm{'neps'};
    my $neta = $parm{'neta'};
    my $eta_parameter = $parm{'eta_parameter'};

    my @keys = sort keys %$eta_parameter;

    my @code;
    for (my $i = 1; $i <= $neps; $i++) {
        my @addends = ();
        push @addends, "ERR${i}_0";
        for (my $j = 1; $j <= $neta; $j++) {
            push @addends, "ERR${i}_${j}";
        }

        for my $key (@keys) {
            push @addends, "ERRC${i}_$key";
        }

        push @code, code::generate_sum(name => "ESUM$i", terms => \@addends);
    }

    return \@code;
}

sub second_order_derivatives_model
{
    # Create a model that can generate the second order derivatives
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
    );
    my $model = $parm{'model'};

    my $have_evid = $model->problems->[0]->inputs->[0]->have_column(column => 'EVID');
    if ($model->has_code(record => 'pk')) {
        $have_evid = 1;
    }

    my $derivatives_model = $model->copy(filename => 'derivatives.mod', write_copy => 0);
    my $netas = $derivatives_model->nomegas->[0];

    my @reset_code;
    push @reset_code, "IF (NEWIND.LT.2) THEN\n";
    for (my $i = 1; $i <= $netas; $i++) {
        push @reset_code, "    DYDETA${i}_ = 0\n";
    }
    for (my $i = 1; $i <= $netas; $i++) {
        for (my $j = 1; $j <= $i; $j++) {
            push @reset_code, "    D2YDETA$j${i}_ = 0\n";
        }
    }
    push @reset_code, "    MYY_ = 0\n";
    push @reset_code, "ENDIF\n";

    model_transformations::prepend_code(model => $derivatives_model, code => \@reset_code);

    my @derivatives_code;
    push @derivatives_code, "TMP2_ = Y\n";
    push @derivatives_code, "\"LAST\n";
    if ($have_evid) {
        push @derivatives_code, "\"    IF (EVID.EQ.0) THEN !Only obs records\n";
    }
    for (my $i = 1; $i <= $netas; $i++) {
        push @derivatives_code, "\"        DYDETA${i}_ = DYDETA${i}_ + G($i,1)\n";
    }
    for (my $i = 1; $i <= $netas; $i++) {
        for (my $j = 1; $j <= $i; $j++) {
            push @derivatives_code, "\"        D2YDETA$j${i}_ = D2YDETA$j${i}_ + G($i," . ($j + 1) . ")\n";
        }
    }

    push @derivatives_code, "\"        MYY_ = MYY_ + TMP2_  ! The log likelihood\n";
    if ($have_evid) {
        push @derivatives_code, "\"    ENDIF\n";
    }

    my $code_record;
    if ($model->has_code(record => 'pk')) {
        $code_record = 'error';
    } else {
        $code_record = 'pred';
    }
    model_transformations::append_code(model => $derivatives_model, code => \@derivatives_code, record => $code_record);

    $derivatives_model->remove_records(type => 'estimation');
    $derivatives_model->add_records(type => 'estimation', record_strings => [ "MAXEVAL=0 METHOD=1 LAPLACE -2LL" ]);
    $derivatives_model->remove_records(type => 'table');
    my @table;
    push @table, "FORMAT=s1PE30.15", "ID", "DV", "TIME", "MYY_";
    for (my $i = 1; $i <= $netas; $i++) {
        push @table, "ETA$i";
    }
    for (my $i = 1; $i <= $netas; $i++) {
        push @table, "DYDETA${i}_";
    }
    for (my $i = 1; $i <= $netas; $i++) {
        for (my $j = 1; $j <= $i; $j++) {
            push @table, "D2YDETA$j${i}_";
        }
    }
    if ($have_evid) {
        push @table, "EVID";
    }
    push @table, "NOPRINT", "ONEHEADER", "FILE=2nd_order.dta";
    $derivatives_model->add_records(type => 'table', record_strings => [ join ' ', @table ]);

    return $derivatives_model;
}

sub second_order_approximation_model
{
    # Create the second order approximation of a model
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
    );
    my $model = $parm{'model'};

    my $have_evid = $model->problems->[0]->inputs->[0]->have_column(column => 'EVID');
    if ($model->has_code(record => 'pk')) {
        $have_evid = 1;
    }

    my $netas = $model->nomegas->[0];

    my $input = "\$INPUT ID DV TIME MYY ";
    for (my $i = 1; $i <= $netas; $i++) {
        $input .= "EBE$i ";
    }
    for (my $i = 1; $i <= $netas; $i++) {
        $input .= "DYDETA$i ";
    }
    for (my $i = 1; $i <= $netas; $i++) {
        for (my $j = 1; $j <= $i; $j++) {
            $input .= "D2YDETA$j$i ";
        }
    }
    if ($have_evid) {
        $input .= "EVID";
    }

    my $sh_mod = model::shrinkage_module->new(
        nomegas => 1,
        directory => '',
        problem_number => 1
    );

    my $ignore = "";
    if ($have_evid) {
        $ignore = " IGNORE=(EVID.GT.0)";
    }

    my $problem = model::problem->new(
        ignore_missing_files=> 1,
        prob_arr => [
            '$PROBLEM Second order approximation',
            $input,
            '$DATA 2nd_order.dta IGNORE=@' . $ignore,
        ],
        shrinkage_module => $sh_mod,
    );
    my $approximation_model = model->new(
        filename => 'approximated.mod',
        problems => [ $problem ],
        ignore_missing_files => 1,
        psn_record_order => 1,
    );

    $approximation_model->add_records(type => 'estimation', record_strings => [ 'MAXEVAL=9999 METHOD=1 LAPLACE -2LL' ]);
    $approximation_model->problems->[0]->omegas($model->problems->[0]->omegas);

    my @pred;

    push @pred, "Y = 0  ; No contribution to log like for the other rows\n";
    push @pred, "IF (NDREC.EQ.LIREC) THEN   ; Only for last record/subject\n";
    for (my $i = 1; $i <= $netas; $i++) {
        push @pred, "DELTA_ETA_$i = (ETA($i) - EBE$i)\n";
    }
    # 0th and 1st order
    my @first_order;
    push @first_order, "MYY";       # 0th order
    for (my $i = 1; $i <= $netas; $i++) {
        my $term = "TERM_O1_$i";
        push @pred, "$term = DYDETA$i * DELTA_ETA_$i\n";
        push @first_order, $term;
    }
    push @pred, "TMP1 = " . join(' + ', @first_order) . "\n";

    # 2nd order
    my @second_order1;
    for (my $i = 1; $i <= $netas; $i++) {
        my $term = "TERM_O2_$i";
        push @pred, "$term = DELTA_ETA_$i**2 * D2YDETA$i$i\n";
        push @second_order1, $term;
    }
    push @pred, "TMP2 = TMP1 + 1/2*(" . join(' + ', @second_order1) . ")\n";

    my @second_order2;
    for (my $i = 1; $i < $netas; $i++) {
        for (my $j = $i + 1; $j <= $netas; $j++) {
            my $term = "TERM_O3_$i$j";
            push @pred, "$term = DELTA_ETA_$i * DELTA_ETA_$j * D2YDETA$i$j\n";
            push @second_order2, $term;
        }
    }
    unshift @second_order2, 'TMP2';
    push @pred, "Y=" . join("+", @second_order2) . "\n";

    push @pred, "ENDIF\n";

    $approximation_model->add_records(type => 'pred', record_strings => \@pred);


    return $approximation_model;
}

1;
