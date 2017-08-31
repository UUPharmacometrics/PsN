package model_transformations;

use strict;
use warnings;
use List::Util qw(any);
use include_modules;
use Cwd;
use model;
use PsN;
use MooseX::Params::Validate;
use utils::file;

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

sub find_zero_fix_omegas
{
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
    );
    my $model = $parm{'model'};

    my @found;
    
    my $omegas = $model->problems->[0]->omegas;
    for my $record (@$omegas) {
        for my $option (@{$record->options}) {
            if ($option->fix and $option->init == 0) {
                push @found, $option;
            }
        }
    }
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
                if (not any { $_ == $current } @$omegas) {
                    push @kept_options, $options[$j];
                }
                $current++;
            } else {
                my $coord = $options[$j]->coordinate_string;
                $coord =~ /OMEGA\((\d+),(\d+)\)/;
                if (not any { $_ == $1 or $_ == $2 } @$omegas) {    # Should this correlation be kept?
                    push @kept_options, $options[$j];
                }
            }
        }
        if ($records[$i]->same and not any { $_ == $records[$i]->n_previous_rows + 1 } @$omegas) {  # Check if first omega in SAME should be deleted
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
        my $record_size;
        if (defined $omega->size) {
            $record_size = $omega->size;
        } else {
            $record_size = scalar(@{$omega->options});
        }
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
        my $record_size;
        if (defined $record->size) {
            $record_size = $record->size;
        } else {
            $record_size = scalar(@{$record->options});
        }
        for my $remove_record (@$omegas) {
            if ($remove_record == $record) {
                push @etas, $current_eta .. $current_eta + $record_size - 1;
            }
        }
        $current_eta += $record_size;
    }

    return \@etas;
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
