package model_transformations;

use strict;
use warnings;
use include_modules;
use Cwd;
use model;
use PsN;
use MooseX::Params::Validate;
use utils::file;
use array qw(numerical_in max unique);
use data;
use Storable;

sub add_tv
{
    # Add TV (typical value) variable if it doesn't already exist for list of parameters
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        parameters => { isa => 'ArrayRef' },
    );
    my $model = $parm{'model'};
    my $parameters = $parm{'parameters'};

    (my $code_record, my $code) = $model->get_pk_or_pred_code();

    my @add_params;     # Parameters that don't already have TVxx
    for my $param (@$parameters) {
        my $found = 0;
        for my $line (@$code) {
            if ($line =~ /^\s*TV$param\s*=/) {
                $found = 1;
            }
        }
        if (not $found) {
            push @add_params, $param;
        }
    }

    my @newcode;
    for my $line (@$code) {
        my $found = 0;
        for my $param (@add_params) {
            if ($line =~ /^(\s*)$param\s*=/) {
                push @newcode, "$1TV$param = 1";
                push @newcode, $line;
                push @newcode, "$1$param = $param * TV$param";
                $found = 1;
                last;
            }
        } 
        if (not $found) {
            push @newcode, $line;
        }
    }

    $model->set_code(record => $code_record, code => \@newcode);
}

sub unique_occs
{
    # Get a list of all unique occasions
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        occ => { isa => 'Str', default => 'OCC' },
    );
    my $model = $parm{'model'};
    my $occ = $parm{'occ'};

    my $data = data->new(
        filename => $model->problems->[0]->datas->[0]->get_absolute_filename(),
        ignoresign => $model->problems->[0]->datas->[0]->ignoresign,
        idcolumn => $model->idcolumn, 
    );

    my $occ_column = $data->column_to_array(column => $occ);
    if (scalar(@$occ_column) == 0) {
        return;
    }
    my $unique_occs = array::unique($occ_column);

    return $unique_occs;
}

sub add_iov
{
    # FIXME: Add initial value is 10% of corresponding IIV omega.
    # Add IOV on each listed parameter
    # If no parameters listed add iov on all iiv etas
    # Return 0 if ok and something was added. 1 if no occ column found
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        occ => { isa => 'Str', default => 'OCC' },
        parameters => { isa => 'ArrayRef[Str]', optional => 1 },
    );
    my $model = $parm{'model'};
    my $occ = $parm{'occ'};
    my $parameters = $parm{'parameters'};

    my $netas = $model->nomegas->[0];

    my $unique_occs = unique_occs(model => $model, occ => $occ);
    return 1 if (not defined $unique_occs);

  	my @model_code;
	my $code_record;
    if ($model->has_code(record => 'pk')) {
        @model_code = @{$model->get_code(record => 'pk')};
        $code_record = 'pk';
    } elsif ($model->has_code(record => 'pred')) {
        @model_code = @{$model->get_code(record => 'pred')};
        $code_record = 'pred';
    } else {
        croak("Neither PK nor PRED defined in " . $model->filename . "\n");
    }

    if (defined $parameters) {
        my %relation;       # parameter->eta no
        for my $p (@$parameters) {
            for my $line (@model_code) {
                if ($line =~ /^(\s*$p\s*=.*)(ETA\((\d+)\))(.*)/) {
                    $line = $1 . '(' . $2 . " + IOV_$p)" . $4;
                    $relation{$p} = $3;
                }
            }
        }
        $model->set_code(record => $code_record, code => \@model_code);

        if (scalar(keys %relation) == 0) {
            return 2;
        }

        my @pre_code;
        my $current_eta = $netas;
        for (my $i = 0; $i < scalar(@$parameters); $i++) {
            if (exists $relation{$parameters->[$i]}) {
                push @pre_code, "IOV_" . $parameters->[$i] . " = 0";
                for my $unique_occ (@$unique_occs) {
                    $current_eta++;
                    push @pre_code, "IF ($occ.EQ.$unique_occ) IOV_" . $parameters->[$i] . " = ETA($current_eta)";
                }
                my $init = $model->initial_values(parameter_type => 'omega', parameter_numbers => [[ $relation{$parameters->[$i]} ]]);
                $model->add_records(type => 'omega', record_strings => [ '$OMEGA BLOCK(1) ' . $init->[0]->[0] * 0.1]); 
                for (my $i = 0; $i < scalar(@$unique_occs) - 1; $i++) {
                    $model->add_records(type => 'omega', record_strings => [ "\$OMEGA BLOCK(1) SAME" ]); 
                }
            }
        }

        prepend_code(model => $model, code => \@pre_code);
    } else {
        diagonal_to_block(model => $model);
        my $records = find_omega_records(model => $model, type => 'iiv');
        my $etas = find_etas(model => $model, type => 'iiv');
        _rename_etas(model => $model, etas => $etas, prefix => 'ETAI');
        my @pre_code;
        my $current_eta = $netas + 1;
        my $current_iov = 1;
        for my $record (@$records) {
            my $size = $record->get_size();
            for (my $i = 0; $i < $size; $i++) {
                push @pre_code, "IOV_$current_iov = 0";
                my $current_iov_eta = $current_eta;
                for my $unique_occ (@$unique_occs) {
                    push @pre_code, "IF ($occ.EQ.$unique_occ) IOV_$current_iov = ETA($current_iov_eta)";
                    $current_iov_eta += $size;
                }
                $current_eta++;
                $current_iov++;
            }
            my $record_clone = Storable::dclone($record);
            for my $option (@{$record_clone->options}) {
                $option->init($option->init * 0.1);
            }
            $model->add_records(type => 'omega', record_strings => $record_clone->_format_record());
            for (my $i = 0; $i < scalar(@$unique_occs) - 1; $i++) {
                $model->add_records(type => 'omega', record_strings => [ "\$OMEGA BLOCK($size) SAME" ]); 
                $current_eta += $size;  # Pass BLOCK SAME
            }
        }

        for (my $i = 0; $i < scalar(@$etas); $i++) {
            push @pre_code, 'ETAI' . $etas->[$i] . ' = ETA(' . $etas->[$i] . ')' . ' + IOV_' . ($i + 1);
        }
        prepend_code(model => $model, code => \@pre_code);
    }

    return 0;
}

sub marge_two_hashes 
{
	#marge two hashes and replace undef values with values from other hash if they are not undef
	my %parm = validated_hash(\@_,
        hash_1 => { isa => 'HashRef' },
        hash_2 => { isa => 'HashRef' },
    );
	my $hash_1 = $parm{'hash_1'};
	my $hash_2 = $parm{'hash_2'};
	
	my %hash_1 = %{$hash_1};
	my %hash_2 = %{$hash_2};
	my %new_hash = %hash_1;
	foreach my $keys_2 ( keys %hash_2) {
		if(exists $hash_1{$keys_2}) {
			if (not(defined $hash_1{$keys_2}) && defined $hash_2{$keys_2}) {
				$new_hash{$keys_2} = $hash_2{$keys_2};
			}
		} else {
			$new_hash{$keys_2} = $hash_2{$keys_2};
		}			
	}
	return(\%new_hash);
}

sub add_etas_to_parameters
{
    # Add etas to the parameters listed
    # Returns a hash of what was added
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        parameters => { isa => 'ArrayRef[Str]' },
    );
    my $model = $parm{'model'};
    my $parameters = $parm{'parameters'};

  	my $model_code;
	my $added_etas;
	if ($model->has_code(record => 'pk')) {
        my($model_code,$added_etas1) = add_etas_in_model_record(model => $model, parameters => $parameters, code_record => 'pk');
		$model->set_code(record => 'pk', code => $model_code);
		$added_etas = $added_etas1;
    } elsif ($model->has_code(record => 'pred')) {
        my($model_code,$added_etas1) = add_etas_in_model_record(model => $model, parameters => $parameters, code_record => 'pred');
		$model->set_code(record => 'pred', code => $model_code);
		$added_etas = $added_etas1;
    } else {
        croak("Neither PK nor PRED defined in " . $model->filename . "\n");
    }
	if ($model->has_code(record => 'error')) {
		
        my($model_code,$added_etas2) = add_etas_in_model_record(model => $model, parameters => $parameters, code_record => 'error');
		$model->set_code(record => 'error', code => $model_code);
		$added_etas = marge_two_hashes(hash_1 => $added_etas, hash_2 => $added_etas2);
    }
	return($added_etas);
}

sub add_etas_in_model_record
{
    # Add etas to the parameters listed in specific record
    # Returns a hash of what was added, and model code
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        parameters => { isa => 'ArrayRef[Str]' },
		code_record => {isa => 'Str'},
    );
    my $model = $parm{'model'};
    my $parameters = $parm{'parameters'};
	my $code_record = $parm{'code_record'};

    my @model_code = @{$model->get_code(record => $code_record)};

    my %done;   # List what etas where added to which paramete to which parameter (par->etano|undef)
    my $next_eta = $model->nomegas->[0] + 1;
    for my $p (@$parameters) {
        my $i;
        my $found_definition = 0;
        for ($i = 0; $i < scalar(@model_code); $i++) {
            if ($model_code[$i] =~ /^\s*$p\s*=/) {
                $found_definition = 1;
                last;
            }
        }
        if ($found_definition) {
            my $lhs_line_no;
            for (my $j = $i + 1; $j < scalar(@model_code); $j++) {
                if ($model_code[$j] =~ /=.*\b$p\b/) {
                    $lhs_line_no = $j;
                }
            }
            my $code_line = "$p = $p * EXP(ETA($next_eta))";
            if (defined $lhs_line_no) {
                splice @model_code, $lhs_line_no, 0, $code_line;
            } else {
                push @model_code, $code_line;
            }
            $model->add_records(type => 'omega', record_strings => [ '$OMEGA 0.0001']); 
            $done{$p} = $next_eta;
            $next_eta++;
        } else {
            $done{$p} = undef;
        }
    }
    return (\@model_code,\%done);
}

sub diagonal_to_block
{
    # Convert all diagonal omegas into BLOCK(1)
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
    );
    my $model = $parm{'model'};

    my $omegas = $model->problems->[0]->omegas;
    my @records;
    for my $omega (@$omegas) {
        if ($omega->is_block()) {
			push @records, $omega;
        } else {
			my $i = 0;
            for my $option (@{$omega->options}) {
                my $new_record = model::problem::omega->new(
                    corr => $omega->corr,
                    size => 1,
                    prior => $omega->prior,
                    sd => $omega->sd,
                    chol => $omega->chol,
                    fix => $option->fix,
                    same => 0,
                    comment => [],
                    type => 'BLOCK',
                    n_previous_rows => $omega->n_previous_rows + $i,
                    print_order => $omega->print_order,
                    options => [ $option ],
                );
                $i++;
                push @records, $new_record;
            }   
        }
    }

    $model->problems->[0]->omegas(\@records);
}

sub full_omega_block
{
    # Replace all omegas into one big full block
    # FIXed and SAME omegas are assumed to be at the end and will be kept
    # Return 1 if model is already full block else 0
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
    );
    my $model = $parm{'model'};

    my $omegas = $model->problems->[0]->omegas;

    my $numetas = 0;
    my $keep_rest = 0;
    my @keep;
    for (my $i = 0; $i < scalar(@$omegas); $i++) {
        my $last = 0;
        if ($i == scalar(@$omegas) - 1) {
            $last = 1;
        }
        if (not $last and $omegas->[$i + 1]->same) {    # The next omega record is SAME
            $keep_rest = 1;
        }
        my $anyfix = 0;
        for my $option (@{$omegas->[$i]->options}) {
            if ($option->fix) {
                $anyfix = 1;
                last;
            }
        }
        if ($anyfix or $omegas->[$i]->fix) {        # Is record FIX or any option FIX
            $keep_rest = 1;
        }

        if ($keep_rest) {
            push @keep, $omegas->[$i];
        } else {
            if ($omegas->[$i]->is_block()) {
                $numetas += $omegas->[$i]->size;
            } else {
                $numetas += scalar(@{$omegas->[$i]->options});
            }
        }
    }

    if ($numetas == 0 or ($omegas->[0]->is_block() and $omegas->[0]->size == $numetas)) {  # No ETAS left or only one BLOCK
        return 1;
    }

    my $new_omega_block = omega_block(model => $model, start_eta => 1, end_eta => $numetas);

    $model->problems->[0]->omegas([ $new_omega_block, @keep ]);

    return 0;
}

sub omega_block
{
    # Transform a number of omegas into block
    # Return the new block without side effects
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        start_eta => { isa => 'Int' },
        end_eta => { isa => 'Int' },
    );
    my $model = $parm{'model'};
    my $start_eta = $parm{'start_eta'};
    my $end_eta = $parm{'end_eta'};

    my $omega_matrix = $model->problems->[0]->get_filled_omega_matrix(start_eta => $start_eta, end_eta => $end_eta);
    my $size = @{$omega_matrix};
    my @record_arr = ( "\$OMEGA BLOCK($size)" );
    for (my $i = 0; $i < $size; $i++) {
        my $row = "";
        for (my $j = 0; $j <= $i; $j++) {
            $row .= $omega_matrix->[$i]->[$j] . ' ';
        }
        push @record_arr, "$row\n";
    }

    my $new_omega_block = model::problem::omega->new(record_arr => \@record_arr);

    return $new_omega_block;
}

sub _rename_etas
{
    # Rename all or some ETAs of model
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        etas => { isa => 'ArrayRef', optional => 1 },   # Array of the etas to rename or unspecified for all etas
		prefix => { isa => 'Str', default => 'ETAT' },	# The name to use for the transformed eta
    );
    my $model = $parm{'model'};
	my $etas = $parm{'etas'};
	my $prefix = $parm{'prefix'};

    if (not defined $etas) {
        my $netas = $model->nomegas->[0];
        $etas = [1 .. $netas];
    }

    for my $eta (@$etas) {
        for my $record (('pk', 'pred', 'error', 'des', 'aes', 'aesinitial', 'mix', 'infn')) {
		    if ($model->has_code(record => $record)) {  
			    my $code = $model->get_code(record => $record);
                for (my $i = 0; $i < scalar(@$code); $i++) {
                    $code->[$i] =~ s/(?<!\w)ETA\($eta\)/$prefix$eta/g;
                }
                $model->set_code(record => $record, code => $code);
            }
        }
	}
}

sub rename_symbol
{
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        from => { isa => 'Str' },
        to => { isa => 'Str' },
    );
    my $model = $parm{'model'};
    my $from = $parm{'from'};
    my $to = $parm{'to'};

    for my $record (('pk', 'pred', 'error', 'des', 'aes', 'aesinitial', 'mix', 'infn')) {
        if ($model->has_code(record => $record)) {  
            my $code = $model->get_code(record => $record);
            for (my $i = 0; $i < scalar(@$code); $i++) {
                $code->[$i] =~ s/\b$from\b/$to/g;
            }
            $model->set_code(record => $record, code => $code);
        }
    }
}

sub prepend_code
{
    # Add code to beginning of $PRED or $PK, or to specific record
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        code => { isa => 'ArrayRef' },
        record => { isa => 'Str', optional => 1 },
    );
    my $model = $parm{'model'};
	my $code = $parm{'code'};
    my $record = $parm{'record'};

	my @model_code;
	my $code_record;
    if (not defined $record) {
        if ($model->has_code(record => 'pk')) {
            @model_code = @{$model->get_code(record => 'pk')};
            $code_record = 'pk';
        } elsif ($model->has_code(record => 'pred')) {
            @model_code = @{$model->get_code(record => 'pred')};
            $code_record = 'pred';
        } else {
            croak("Neither PK nor PRED defined in " . $model->filename . "\n");
        }
    } else {
        @model_code = @{$model->get_code(record => $record)};
        $code_record = $record;
    }

	@model_code = (@$code, @model_code); 

    $model->set_code(record => $code_record, code => \@model_code);
}

sub append_code
{
    # Add code to the end of $PRED or $PK, or to specific record
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        code => { isa => 'ArrayRef' },
        record => { isa => 'Str', optional => 1 },
    );
    my $model = $parm{'model'};
	my $code = $parm{'code'};
    my $record = $parm{'record'};

	my @model_code;
	my $code_record;
    if (not defined $record) {
        if ($model->has_code(record => 'pk')) {
            @model_code = @{$model->get_code(record => 'pk')};
            $code_record = 'pk';
        } elsif ($model->has_code(record => 'pred')) {
            @model_code = @{$model->get_code(record => 'pred')};
            $code_record = 'pred';
        } else {
            croak("Neither PK nor PRED defined in " . $model->filename . "\n");
        }
    } else {
        my $precord = $record . 's';
        my $record_array = $model->problems->[0]->$precord;
        if (defined $record_array and scalar @{$record_array} > 0) {
            @model_code = @{$model->get_code(record => $record)};
        } else {
            $model->add_records(type => $record, record_strings => []); 
        }

        @model_code = @{$model->get_code(record => $record)};
        $code_record = $record;
    }

	@model_code = (@model_code, @$code); 

    $model->set_code(record => $code_record, code => \@model_code);
}

sub insert_code
{
    # Add code after specified line number in $PRED or $PK, or to specific record
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        code => { isa => 'ArrayRef' },
        record => { isa => 'Str', optional => 1 },
        line => { isa => 'Int' },
    );
    my $model = $parm{'model'};
	my $code = $parm{'code'};
    my $record = $parm{'record'};
    my $line = $parm{'line'};
    
	my @model_code;
	my $code_record;
    if (not defined $record) {
        if ($model->has_code(record => 'pk')) {
            @model_code = @{$model->get_code(record => 'pk')};
            $code_record = 'pk';
        } elsif ($model->has_code(record => 'pred')) {
            @model_code = @{$model->get_code(record => 'pred')};
            $code_record = 'pred';
        } else {
            croak("Neither PK nor PRED defined in " . $model->filename . "\n");
        }
    } else {
        @model_code = @{$model->get_code(record => $record)};
        $code_record = $record;
    }

    my @result_code;
    for (my $i = 0; $i <= $line; $i++) {
        push @result_code, $model_code[$i];
    }
	@result_code = (@result_code, @$code); 
    for (my $i = $line + 1; $i < scalar(@model_code); $i++) {
        push @result_code, $model_code[$i];
    }

    $model->set_code(record => $code_record, code => \@result_code);
}

sub boxcox_etas
{
    # Boxcox transform all or some ETAs of model
    # Assume only one $PROBLEM
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        etas => { isa => 'ArrayRef', optional => 1 },       # An array of the etas to transform or unspecified for all etas
    );
    my $model = $parm{'model'};
	my $etas = $parm{'etas'};

    if (not defined $etas) {
        my $netas = $model->nomegas->[0];
        $etas = [1 .. $netas];
    }
    my $nthetas = $model->nthetas;

    _rename_etas(model => $model, etas => $etas, prefix => 'ETAB');

    my $next_theta = $nthetas + 1;
    my @code;
    for my $i (@$etas) {
        push @code, "ETAB$i = (EXP(ETA($i))**THETA($next_theta) - 1) / (THETA($next_theta))";
        $next_theta++;
        $model->add_records(type => 'theta', record_strings => [ '$THETA (-3, 0.01, 3)']); 
    }

    prepend_code(model => $model, code => \@code);
}

sub uniform_etas
{
    # Change all or some ETAs to a uniform distribution

	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        etas => { isa => 'ArrayRef', optional => 1 },       # An array of the etas to transform or unspecified for all etas
    );
    my $model = $parm{'model'};
	my $etas = $parm{'etas'};

    if (not defined $etas) {
        my $netas = $model->nomegas->[0];
        $etas = [1 .. $netas];
    }
    my $nthetas = $model->nthetas;

    _rename_etas(model => $model, etas => $etas, prefix => 'ETAU');

    my $omega_options = omega_options_from_etas(model => $model, etas => $etas);
    my @inits;
    for my $option (@$omega_options) {
        push @inits, $option->init;
        $option->init(0.29 * 0.29);
        $option->fix(1);
    }

    my $next_theta = $nthetas + 1;
    my @code;
    my $index = 0;
    for my $i (@$etas) {
        push @code, "ETAU$i = (PHI(ETA($i)) - 0.5) * THETA($next_theta)";
        $next_theta++;
        my $init = $inits[$index] / 0.29;
        $model->add_records(type => 'theta', record_strings => [ "\$THETA $init"]); 
        $index++;
    }

    prepend_code(model => $model, code => \@code);
}

sub tdist_etas
{
    # Tdist transform all or some ETAs of model
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

    _rename_etas(model => $model, etas => $etas, prefix => 'ETAT');

    my $next_theta = $nthetas + 1;
    my @code;
    for my $i (@$etas) {
        push @code,
			"ETAT$i = ETA($i)*(1+((ETA($i)**2+1)/(4*THETA($next_theta)))&\n" .
			"	+((5*ETA($i)**4+16*ETA($i)**2+3)/(96*THETA($next_theta)**2))&\n" .
			"	+((3*ETA($i)**6+19*ETA($i)**4+17*ETA($i)**2-15)/(384*THETA($next_theta)**3)))\n";
        $next_theta++;
        $model->add_records(type => 'theta', record_strings => [ '$THETA (3,,100)']); 
    }

    prepend_code(model => $model, code => \@code);
}

sub remove_iiv
{
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        fix => { isa => 'Bool', default => 0 },     # Set to fix removed iiv $OMEGAs else remove them
    );
    my $model = $parm{'model'};
    my $fix = $parm{'fix'};

    my $omegas = $model->problems->[0]->omegas;

    my $remove = find_omega_records(model => $model, type => 'iiv');

    if ($fix) {
        _fix_omegas(model => $model, omegas => $remove);
    } else {
        my $etas = _etas_from_omega_records(model => $model, omegas => $remove);
        _remove_omegas(model => $model, omegas => $etas);
    }
}

sub remove_iov
{
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        fix => { isa => 'Bool', default => 0 },     # Set to fix removed iiv $OMEGAs else remove them
    );
    my $model = $parm{'model'};
    my $fix = $parm{'fix'};

    my $omegas = $model->problems->[0]->omegas;

    my $remove = find_omega_records(model => $model, type => 'iov');

    if ($fix) {
        _fix_omegas(model => $model, omegas => $remove);
    } else {
        my $etas = _etas_from_omega_records(model => $model, omegas => $remove);
        _remove_omegas(model => $model, omegas => $etas);
    }
}

sub find_omega_records
{
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        type => { isa => 'Str' },     # Set to either 'iov' or 'iiv'
    );
    my $model = $parm{'model'};
    my $type = $parm{'type'};

    my $omegas = $model->problems->[0]->omegas;

    my @found;
    my $last = 0;
    for (my $i = 0; $i < scalar(@$omegas); $i++) {
        if ($i == scalar(@$omegas) - 1) {
            $last = 1;
        }
        unless ($omegas->[$i]->same or (not $last and $omegas->[$i + 1]->same) or $omegas->[$i]->fix) {    # Keep if IOV or block FIX
            if ($type eq 'iiv') {
                push @found, $omegas->[$i];
            }
        } else {
            if ($type eq 'iov') {
                push @found, $omegas->[$i];
            }
        }
    }

    return \@found;
}

sub find_etas
{
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        type => { isa => 'Str' },     # Set to either 'iov' or 'iiv'
    );
    my $model = $parm{'model'};
    my $type = $parm{'type'};

    my $omega_records = find_omega_records(model => $model, type => $type);

    my @etas;
    for my $record (@$omega_records) {
        for (my $i = 0; $i < $record->get_size(); $i++) {
            push @etas, $record->n_previous_rows + 1 + $i;
        }
    }

    return \@etas;
}

sub find_iov_structure
{
    # Generate an array of arrays over which iov etas are connected to each occasion
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
    );
    my $model = $parm{'model'};

    my $iov_records = find_omega_records(model => $model, type => 'iov');
    my @structure;

    my $i = 0;
    for my $record (@$iov_records) {
        if ($record->same) {
            $i++; 
        } else {
            $i = 0;
        }
        $structure[$i] = [] if (not defined $structure[$i]);
        push @{$structure[$i]}, $record->n_previous_rows + 1 .. $record->n_previous_rows + $record->get_size();
    }

    return \@structure;
}

sub find_zero_fix_omegas
{
    # Find all DIAGONAL omegas 0 FIX and all BLOCK omegas all 0 FIX
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
    );
    my $model = $parm{'model'};

    my @found;
    
    my $omegas = $model->problems->[0]->omegas;
    for my $record (@$omegas) {
        my $current = $record->n_previous_rows + 1;
        if (defined $record->type and $record->type eq 'BLOCK') {
            next if $record->same;
            next if not $record->fix;
            my $all_zero = 1;
            for my $option (@{$record->options}) {
                if ($option->init != 0) {
                    $all_zero = 0;
                    next;
                }
            }
            if ($all_zero) {
                push @found, ($current .. $current + $record->size - 1);
            }
        } else {
            for my $option (@{$record->options}) {
                if ($option->on_diagonal) {
                    if ($option->fix and $option->init == 0) {
                        push @found, $current;
                    }
                    $current++;
                }
            }
        }
    }
    return \@found;
}

sub remaining_omegas
{
    # Given a list of omegas return a list of the remaining omegas in a model
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        omegas => { isa => 'ArrayRef[Int]' },
    );
    my $model = $parm{'model'};
    my $omegas = $parm{'omegas'};

    my $nomegas = $model->problems->[0]->nomegas;
    my @remaining;
    if (scalar(@$omegas) == 0) {
        return [ 1 .. $nomegas ];
    }

    my $max = array::max($omegas);

    for (my $i = 1; $i <= $max; $i++) {
        if (not numerical_in($i, $omegas)) {
            push @remaining, $i;
        }
    }

    if ($nomegas > $max) {
        push @remaining, ($max + 1 .. $nomegas)  
    }

    return \@remaining;
}

sub _fix_omegas
{
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        omegas => { isa => 'ArrayRef[model::problem::omega]' },
    );
    my $model = $parm{'model'};
    my $omegas = $parm{'omegas'};

    for my $omega (@$omegas) {
        if (defined $omega->type and $omega->type eq 'BLOCK' and not $omega->same) {
            $omega->fix(1);
        }
        for my $option (@{$omega->options}) {
            $option->init(0);
            if (not defined $omega->type or $omega->type ne 'BLOCK') {
                $option->fix(1);
            }
        }
    }
}

sub _remove_omegas
{
    # Remove omegas from model by removing both the
    # omega records, renumbering etas and setting
    # removed etas to constant zero.
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        omegas => { isa => 'ArrayRef[Int]' },
    );
    my $model = $parm{'model'};
    my $omegas = $parm{'omegas'};

    _remove_etas(model => $model, etas => $omegas);
    _remove_omega_records(model => $model, omegas => $omegas);
}

sub _remove_omega_records
{
    # Remove specific omega records from model
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        omegas => { isa => 'ArrayRef[Int]' },
    );
    my $model = $parm{'model'};
    my $omegas = $parm{'omegas'};
    
    my @records = @{$model->problems->[0]->omegas};
    my @kept_records;
    my $current = 1;
    for (my $i = 0; $i < scalar(@records); $i++) {
        my @options = @{$records[$i]->options};
        my @kept_options;
        for (my $j = 0; $j < scalar(@options); $j++) {
            if ($options[$j]->on_diagonal) {
                if (not numerical_in($current, $omegas)) {
                    push @kept_options, $options[$j];
                }
                $current++;
            } else {
                my $coord = $options[$j]->coordinate_string;
                $coord =~ /OMEGA\((\d+),(\d+)\)/;
                if (not numerical_in($1, $omegas) and not numerical_in($2, $omegas)) {    # Should this correlation be kept?
                    push @kept_options, $options[$j];
                }
            }
        }
        if ($records[$i]->same and not numerical_in($records[$i]->n_previous_rows + 1, $omegas)) {  # Check if first omega in SAME should be deleted
            push @kept_records, $records[$i];
        }
        if (scalar(@kept_options) > 0) {
            $records[$i]->options(\@kept_options);
            push @kept_records, $records[$i];
        }
    }
    $model->problems->[0]->omegas(\@kept_records);

    # Update coordinate_strings, n_previous_rows and size
    _update_omegas(model => $model);
}

sub _update_omegas
{
    # For each omega update coordinate_strings, n_previous_rows and size
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
    );
    my $model = $parm{'model'};

    my $current = 1;
    for my $record (@{$model->problems->[0]->omegas}) {
        $record->n_previous_rows($current - 1);
        my $size = 0;
        my $col = 1;        # Column in the omega matrix
        for my $option (@{$record->options}) {
            if ($option->on_diagonal) {
                $option->coordinate_string("OMEGA($current,$current)");
                $size++;
                $current++;
                $col = 1;
            } else {
                $option->coordinate_string("OMEGA($current,$col)");
                $col++;
            }
        }
        $record->size($size);
    }
}

sub _remove_etas
{
    # Remove etas by changing the numbering and setting removed etas to constant 0.
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        etas => { isa => 'ArrayRef[Int]' },
    );
    my $model = $parm{'model'};
    my $etas = $parm{'etas'};

    my $num_etas = _number_of_etas(model => $model);
    my %replace_hash; 
    my $current = 1;
    for (my $i = 1; $i <= $num_etas; $i++) {
        if (grep { $_ == $i } @$etas) {
            $replace_hash{$i} = 0;
        } else {
            $replace_hash{$i} = "ETA($current)";
            $current++;
        }
    }

    for my $record (('pk', 'pred', 'error', 'des', 'aes', 'aesinitial', 'mix', 'infn')) {
        if ($model->has_code(record => $record)) {  
            my $code = $model->get_code(record => $record);
            for (my $i = 0; $i < scalar(@$code); $i++) {
                $code->[$i] =~ s/(?<!\w)ETA\((\d+)\)/$replace_hash{$1}/g;
            }
            $model->set_code(record => $record, code => $code);
        }
    }
}

sub _number_of_etas
{
    # Return the number of etas in the model
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
    );
    my $model = $parm{'model'};
 
    my @all_omegas = @{$model->problems->[0]->omegas};

    my $num_etas = 0;
    for my $omega (@all_omegas) {
        my $record_size = $omega->get_size();

        $num_etas += $record_size;
    }
    return $num_etas;
}

sub _etas_from_omega_records
{
    # Return a list of eta numbers from a list of omega records
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        omegas => { isa => 'ArrayRef[model::problem::omega]' },
    );
    my $model = $parm{'model'};
    my $omegas = $parm{'omegas'};

    my @all_omegas = @{$model->problems->[0]->omegas};
    my @etas;

    my $current_eta = 1;
    my $remove_index = 0;
    for my $record (@all_omegas) {
        my $record_size = $record->get_size();
        for my $remove_record (@$omegas) {
            if ($remove_record == $record) {
                push @etas, $current_eta .. $current_eta + $record_size - 1;
            }
        }
        $current_eta += $record_size;
    }

    return \@etas;
}

sub omega_options_from_etas
{
    # Return a list of omega options from a list of eta/omega numbers
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        etas => { isa => 'ArrayRef[Int]' },
    );
    my $model = $parm{'model'};
    my $etas = $parm{'etas'};

    my @omegas;
    my $current_eta = 1;
    for my $record (@{$model->problems->[0]->omegas}) {
        if (defined $record->options) {
            for my $option (@{$record->options}) {
                if (numerical_in($current_eta, $etas)) {
                    push @omegas, $option;
                }
                $current_eta++;
            }
        } else {
            $current_eta += $record->get_size();
        }
    }

    return \@omegas;
}

sub omit_ids
{
    # Omit one or more ids from a model or dataset
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        ids => { isa => 'ArrayRef' },       # An array of the ids to omnit from model or dataset
        ignore => { isa => 'Bool', default => 0 },
    );
    my $model = $parm{'model'};
	my $ids = $parm{'ids'};
	my $ignore = $parm{'ignore'};

    # Check if ids are available
    my $data = $model->problems->[0]->datas->[0];
    my $data_table = data->new(
        directory => $data->get_directory,
        filename => $data->get_filename,
        ignoresign => $data->ignoresign,
        parse_header => 1
    );

    my $found;
    my @to_remove;
    for my $id (@$ids) {
        $found = 0;
        for (my $i = 0; $i < scalar(@{$data_table->individuals}); $i++) {
            my $individual = $data_table->individuals->[$i];
            if ($id == $individual->idnumber) {
                $found = 1;
                if ($ignore) {
                    $model->add_option(record_name => 'data', option_name => 'IGNORE', option_value => "(ID.EQ.$id)");
                } else { 
                    push @to_remove, $i;
                }
                last;
            }
        }
        if (not $found) {
            print "Warning: ID=$id was not found in dataset. No need to omit it\n";
        }
    }

    if (not $ignore) {
        my @keep;
        my $found;
        for (my $i = 0; $i < scalar(@{$data_table->individuals}); $i++) {
            $found = 0;
            for my $remidx (@to_remove) {
                if ($remidx == $i) {
                    $found = 1;
                    last;
                }
            }
            if (not $found) {
                push @keep, $data_table->individuals->[$i];
            }
        }
        $data_table->individuals(\@keep);
        my $filename = utils::file::replace_extension($model->filename, "dta");
        $data_table->filename($filename);
        $data_table->_write();
        $data->set_filename(filename => $filename, directory => $model->directory );
    }
}

sub _rename_epsilons
{
    # Rename all or some EPSs of model
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        epsilons => { isa => 'ArrayRef', optional => 1 },   # Array of the epsilons to rename or unspecified for all epsilons
		prefix => { isa => 'Str', default => 'EPST' },	# The name to use for the transformed epsilon
    );
    my $model = $parm{'model'};
	my $epsilons = $parm{'epsilons'};
	my $prefix = $parm{'prefix'};

    if (not defined $epsilons) {
        my $neps = $model->nsigmas->[0];
        $epsilons = [1 .. $neps];
    }

    for my $eps (@$epsilons) {
        if ($model->has_code(record => 'error')) {  
            my $code = $model->get_code(record => 'error');
            for (my $i = 0; $i < scalar(@$code); $i++) {
                $code->[$i] =~ s/(?<!\w)(EPS|ERR)\($eps\)/$prefix$eps/g;
            }
            $model->set_code(record => 'error', code => $code);
        }
	}
}

sub iiv_on_ruv
{
    # Add an eta for each epsilon or only to selected epsilons
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        epsilons => { isa => 'ArrayRef', optional => 1 },       # An array of the epsilons to transform or unspecified for all epsilons
    );
    my $model = $parm{'model'};
	my $epsilons = $parm{'epsilons'};

    if (not defined $epsilons) {
        my $nepsilons = $model->nsigmas->[0];
        return if ($nepsilons == 0);
        $epsilons = [1 .. $nepsilons];
    }
    return if (scalar(@$epsilons) == 0);
    my $netas = $model->nomegas->[0];

    _rename_epsilons(model => $model, epsilons => $epsilons, prefix => 'EPST');

    my $next_eta = $netas + 1;
    my @code;
    for my $i (@$epsilons) {
        push @code, "EPST$i = EPS($i) * EXP(ETA($next_eta))";
        $next_eta++;
        $model->add_records(type => 'omega', record_strings => [ '$OMEGA 0.01']); 
    }

    prepend_code(model => $model, code => \@code, record => 'error');
}

sub power_on_ruv
{
    # Add IPRED**THETA for each epsilon or only to selected epsilons
	my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        epsilons => { isa => 'ArrayRef', optional => 1 },       # An array of the epsilons to transform or unspecified for all epsilons
    );
    my $model = $parm{'model'};
	my $epsilons = $parm{'epsilons'};

    my @error_code = @{$model->get_code(record => 'error')};
    my $found = 0;
    for my $line (@error_code) {
        if ($line =~ /\s*IPRED\s*=/) {
            $found = 1;
            last;
        }
    }
    if (not $found) {
        print "Warning: No IPRED definition found in \$ERROR. No transformation will be done.\n";
        return;
    }

    if (not defined $epsilons) {
        my $nepsilons = $model->nsigmas->[0];
        return if ($nepsilons == 0);
        $epsilons = [1 .. $nepsilons];
    }
    return if (scalar(@$epsilons) == 0);
    my $nthetas = $model->nthetas;

    _rename_epsilons(model => $model, epsilons => $epsilons, prefix => 'EPSP');
    @error_code = @{$model->get_code(record => 'error')};

    my $next_theta = $nthetas + 1;
    my @code;
    for my $i (@$epsilons) {
        push @code, "EPSP$i = EPS($i) * (IPRED ** THETA($next_theta))";
        $next_theta++;
        $model->add_records(type => 'theta', record_strings => [ '$THETA 0.01']); 
    }
    
    my @result_code;
    for my $line (@error_code) {
        push @result_code, $line;
        if ($line =~ /\s*IPRED\s*=/) {
            push @result_code, @code;
        }
    }
    $model->set_code(record => 'error', code => \@result_code);
}

1;
