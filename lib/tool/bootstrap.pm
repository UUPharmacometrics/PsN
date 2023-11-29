package tool::bootstrap;

use include_modules;
use strict;
use File::Copy 'cp';
use Statistics::Distributions 'udistr', 'uprob';
use random;
use Mouse;
use MouseX::Params::Validate;
use array;
use data;
use OSspecific;
use tool::llp;
use tool::cdd::jackknife;

extends 'tool';

has 'bca_print_order' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['diagnostic_means','means','bias','bca_confidence_intervals','standard_error_confidence_intervals','standard_errors','medians','jackknife_means','percentile_confidence_intervals'] } );
has 'result_parameters' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'bca_calculation_order' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['diagnostic_means','means','medians','percentile_confidence_intervals','standard_errors','standard_error_confidence_intervals','jackknife_means','bca_confidence_intervals'] } );
has 'bootstrap_raw_results' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'jackknife_raw_results' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'bootstrap_diagnostics' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'jackknife_diagnostics' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'bootstrap_estimates' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'jackknife_estimates' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'skip_minimization_terminated' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'keep_tables' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'keep_covariance' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'allow_ignore_id' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'copy_data' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'jackknife' => ( is => 'rw', isa => 'ArrayRef' );
has 'skip_covariance_step_terminated' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'skip_with_covstep_warnings' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'skip_estimate_near_boundary' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'calculation_order' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['diagnostic_means','means','medians','percentile_confidence_intervals','standard_errors','standard_error_confidence_intervals'] } );
has 'print_order' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['diagnostic_means','means','bias','standard_error_confidence_intervals','standard_errors','medians','percentile_confidence_intervals'] } );
has 'samples' => ( is => 'rw', isa => 'Int', default => 200 );
has 'run_base_model' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'update_inits' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'dofv' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'dofv_samples' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'mceta' => ( is => 'rw', isa => 'Int', default => 0 );
has 'subjects' => ( is => 'rw', isa => 'HashRef' );
has 'stratify_on' => ( is => 'rw', isa => 'Str' );
has 'type' => ( is => 'rw', isa => 'Str', default => 'bootstrap' );
has 'confidence_limits' => ( is => 'rw', isa => 'HashRef[Num]', default => sub { {'0.1' => 3.2905, '1' => 2.5758, '5' => 1.96, '10' => 1.6449} } );
has 'parameters' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['ofv', 'theta', 'omega', 'sigma'] } );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['bootstraplog.csv'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'bootstrap_results.csv' );
has 'se_confidence_intervals_check' => ( is => 'rw', isa => 'Num', default => 0 );
has 'percentile_confidence_intervals_check' => ( is => 'rw', isa => 'Num', default => 0 );
has 'bca_confidence_intervals_check' => ( is => 'rw', isa => 'Num', default => 0 );
has 'large_bias_limit' => ( is => 'rw', isa => 'Num', default => 0.05 );
has 'raw_results_dofv' => ( is => 'rw', isa => 'Str', default => 'raw_results_dofv.csv');
has '_number_of_columns' => ( is => 'rw', isa => 'Int' );

sub BUILD
{
    my $self = shift;

    for my $accessor ('logfile','raw_results_file','raw_nonp_file') {
        my @new_files = ();
        my @old_files = @{$self->$accessor};
        for (my $i = 0; $i < scalar(@old_files); $i++) {
            my $name;
            my $ldir;
            ($ldir, $name) = OSspecific::absolute_path($self->directory, $old_files[$i]);
            push(@new_files, $ldir . $name);
        }
        $self->$accessor(\@new_files);
    }

    check_ignore_id(model => $self ->models()->[0], allow_ignore_id => $self->allow_ignore_id);

}

sub check_ignore_id
{
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              allow_ignore_id => {isa => 'Bool',optional => 0},
        );
    my $model = $parm{'model'};
    my $allow_ignore_id = $parm{'allow_ignore_id'};

    # If certain ID:s are ignored, this will interfere with bootstrap. Warn user, exit
    #look for synonym
    my $id_synonym;
    $id_synonym = $model-> get_option_value(record_name=>'input',
                                            option_name=>'ID');
    #we do not look for <synonym>=ID since PsN won't accept it anyway
    #look for ignore/accept of ID/synonym

    my @check_list=();
    my $ignore_list = $model-> get_option_value(record_name=>'data',
                                                option_name=>'IGNORE',
                                                option_index => 'all',
                                                fuzzy_match => 1);
    push (@check_list,@{$ignore_list});
    my $accept_list = $model-> get_option_value(record_name=>'data',
                                                option_name=>'ACCEPT',
                                                option_index => 'all',
                                                fuzzy_match => 1);
    push (@check_list,@{$accept_list});
    my $warning = "Dangerous IGNORE/ACCEPT statement found in \$DATA.\n".
        "Bootstrap program cannot at present safely handle IGNORE or\n".
        "ACCEPT statements involving the ID column, since individuals are\n".
        "renumbered during resampling. ";
    foreach my $igval (@check_list){
        if (($igval =~ /[^a-zA-Z0-9_]+(ID)[^a-zA-Z0-9_]+/ ) ||
            ($id_synonym && ($igval =~ /[^a-zA-Z0-9_]+($id_synonym)[^a-zA-Z0-9_]+/ ) )){
            if ($allow_ignore_id){
                ui->print(category=> 'all',message => "\nWarning:\n".
                          $warning."It is recommended to edit the datafile\n".
                          "manually instead, before running the bootstrap.\n\n");
            } else {
                croak($warning."Please edit the datafile ".
                      "manually instead, before running the bootstrap.\n");
            }
        }
    }

    my $options = $model-> problems->[0]->datas->[0]->options;
    if (defined $options){
        foreach my $opt (@{$options}){
            unless (defined $opt->value and length($opt->value)>0 ){
                if (($opt->name =~ /^(IGN|ACC)[^(]*\(ID\./) or
                    ($id_synonym and ($opt->name =~ /^(IGN|ACC)[^(]*\($id_synonym\./))){
                    if ($allow_ignore_id){
                        ui->print(category=> 'all',message => "\nWarning:\n".
                                  $warning."It is recommended to edit the datafile\n".
                                  "manually instead, before running the bootstrap.\n\n");
                    } else {
                        croak($warning."Please edit the datafile ".
                              "manually instead, before running the bootstrap.\n");
                    }
                }
            }
        }
    }
    #warn if code records with ID/synonym detected
    @check_list = ();
    my $record_ref = $model->record(record_name => 'pk');
    push (@check_list, @{$model->get_code(record => 'pk')})
        if (scalar(@{$record_ref}) > 0);

    $record_ref = $model->record(record_name => 'pred');
    push (@check_list, @{$model->get_code(record => 'pred')})
    if (scalar(@{$record_ref}) > 0);

    $record_ref = $model->record(record_name => 'error');
    push (@check_list, @{$model->get_code(record => 'error')})
    if (scalar(@{$record_ref}) > 0);

    foreach my $line (@check_list){
        next if ($line =~ /^\s*;/); #skip comments
        $line =~ s/;.*//; #remove comments
        if (($line =~ /(\bID\b)/ ) ||
            ($id_synonym && ($line =~ /\b($id_synonym)\b/ ) )){
            ui->print(category=> 'all',message=> "\nWarning:\nID/ID-synonym found in \$PK/\$PRED/\$ERROR:\n".$line.
                      "Bootstrap script renumbers individuals, which means that code\n".
                      "based on ID-value might not give the expected results.\n\n");
            last;
        }
    }

}

sub modelfit_setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    $self -> general_setup( model_number => $model_number,
        class        => 'tool::modelfit');
}

sub cleanup
{
    my $self = shift;

    #remove datafiles in modelfit_dirX/NM_runX
    #leave in m1

    my $prob = 1;
    while (1) {
        my $dir = $self ->directory()."modelfit_dir$prob/";
        last unless (-e $dir);
        my $sample=1;
        while (1){
            my $file = $dir."NM_run".$sample."/bs_pr".$prob."_".$sample.".dta";
            last unless (-e $file);
            unlink $file;
            $sample++;
        }
        $prob++;
    }
}

sub llp_setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    $self->general_setup(model_number => $model_number, class => 'tool::llp');
}

sub calculate_diagnostic_means
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 },
        parameter_names => { isa => 'ArrayRef[Str]', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my @parameter_names = defined $parm{'parameter_names'} ? @{$parm{'parameter_names'}} : ();
    #FIXME jackknife diagnostics?
    my @means=();
    # Loop the columns
    for (my $l = 0; $l < scalar @{$self -> diagnostic_parameters()}; $l++) {
        my @parameter_array=();
        # From 1 to get rid of original model
        for (my $k = 1; $k < scalar @{$self->bootstrap_diagnostics->[$model_number - 1]}; $k++) {
            next unless ((defined $self->bootstrap_diagnostics->[$model_number - 1][$k]) and
                         (defined $self->bootstrap_diagnostics->[$model_number - 1][$k][$l]) and
                         ($self->bootstrap_diagnostics->[$model_number - 1][$k][$l] ne ''));
            push(@parameter_array, $self->bootstrap_diagnostics->[$model_number - 1][$k][$l]);
        }
        if (scalar(@parameter_array)>0){
            $means[$l] = array::mean(\@parameter_array);
        }else{
            $means[$l] = undef;
        }
    }

    for ( my $l = 0; $l < scalar(@means); $l++ ) {
        $self->result_parameters->{'diagnostic_means'} -> [$model_number-1][0][$l] = $means[$l];
    }
    $self->result_parameters->{'diagnostic_means_labels'} -> [$model_number-1] =[[],\@parameter_names];
}

sub calculate_means
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 },
        parameter_names => { isa => 'ArrayRef[Str]', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my @parameter_names = defined $parm{'parameter_names'} ? @{$parm{'parameter_names'}} : ();
    #FIXME jackknife estimates
    no warnings qw(uninitialized numeric);

    my @means=();
    # Loop the parameters
    for (my $l = 0; $l < $self->_number_of_columns; $l++) {
        my @parameter_array=();
        # From 1 to get rid of original model
        for (my $k = 1; $k < scalar @{$self->bootstrap_estimates->[$model_number - 1]}; $k++) {
            next unless (defined $self->bootstrap_estimates->[$model_number - 1][$k][$l]);
            push(@parameter_array, $self->bootstrap_estimates->[$model_number - 1][$k][$l]);
        }
        if (scalar(@parameter_array)>0){
            $means[$l] = array::mean(\@parameter_array);
        }else{
            $means[$l] = undef;
        }
    }

    for (my $l = 0; $l < $self->_number_of_columns; $l++) {
        my $mean = $means[$l];
        $self->result_parameters->{'means'} -> [$model_number-1][0][$l] = $mean;
        my $bias;
        my $estimated_value = $self->bootstrap_estimates->[$model_number - 1][0][$l];
        if (defined $estimated_value and (defined $mean) and (length($mean)>0) and (length($estimated_value)>0)) {
            $bias = $mean - $estimated_value;
        }
        $self->result_parameters->{'bias'} -> [$model_number-1][0][$l] = $bias;
        if ( length($self->bootstrap_estimates -> [$model_number-1][0][$l])>0 and
             $self->bootstrap_estimates -> [$model_number-1][0][$l] != 0 and
             $bias/$self->bootstrap_estimates -> [$model_number-1][0][$l]
             > $self -> large_bias_limit() ) {
            $self->result_parameters->{'large_bias'} -> [$model_number-1][0][$l] = 1;
        } else {
            $self->result_parameters->{'large_bias'} -> [$model_number-1][0][$l] = 0;
        }
    }
    $self->result_parameters->{'means_labels'} -> [$model_number-1] = [[], \@parameter_names];

    $self->result_parameters->{'bias_labels'} -> [$model_number-1] = [[], \@parameter_names];
}

sub calculate_bca_confidence_intervals
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 },
        parameter_names => { isa => 'ArrayRef[Str]', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my @parameter_names = defined $parm{'parameter_names'} ? @{$parm{'parameter_names'}} : ();

    sub c_get_z0 {
        my $arr_ref  = shift;
        my $orig_value = shift;
        my $num_less_than_orig = 0;
        my $nvalues = 0;
        my $z0;
        foreach my $value ( @{$arr_ref} ) {
            if ( defined $value and $value ne '' ) {
                $num_less_than_orig++ if ( $value < $orig_value );
                $nvalues ++;
            }
        }

        unless ( $nvalues == 0 ) {
            if ( ($num_less_than_orig / $nvalues ) == 0 ) {
                $z0 = -100;
            } elsif ( $num_less_than_orig == $nvalues  ) {
                $z0 = 100;
            } elsif ( (2*$num_less_than_orig) == $nvalues  ) {
                #on 32bit windows have seen that udistr(0.5), which should be simply 0, returns NaN
                $z0 = 0;
            } else {
                $z0 = udistr( 1 - ($num_less_than_orig / $nvalues ) );
            }
        }
        return $z0;
    }

    sub c_get_acc {
        my $arr_ref = shift;
        my $jk_mean = shift;
        my $acc_upper = 0;
        my $acc_lower = 0;
        my $nvalues = 0;
        my $acc;
        foreach my $value ( @{$arr_ref} ){
            if ( defined $value and $value ne '' ) {
                $acc_upper = $acc_upper + ($jk_mean-$value)**3;
                $acc_lower = $acc_lower + ($jk_mean-$value)**2;
                $nvalues ++;
            }
        }
        $acc_lower = 6*($acc_lower**(3/2));
        unless ( $acc_lower == 0 ) {
            $acc = $acc_upper / $acc_lower;
        } else {
            $acc = $acc_upper / 0.001;
        }
        return $acc;
    }

    sub c_get_alphas {
        my $old_alphas = shift;
        my $acc = shift;
        my $z0 = shift;
        my $denom;
        my @new_alphas = ();
        foreach my $position ( @{$old_alphas} ) {
            if ( $position == 0 ){
                $denom = -100;
            } elsif ( $position == 100 ) {
                $denom = 100;
            } elsif ( $position == 50 ) {
                #on 32bit windows have seen that udistr(0.5), which should be simply 0, returns NaN
                #                $denom = $z0 + udistr( 1 - $position/100 );
                $denom = $z0;
            } else {
                $denom = $z0 + udistr( 1 - $position/100 );
            }
            my $nom     = 1 - $acc * $denom;
            my $lim = 100*uprob( - ( $z0 + $denom / $nom ) );
            push( @new_alphas, $lim );
        }
        return \@new_alphas;
    }

    my @limits = sort { $a <=> $b } keys %{$self -> confidence_limits};
    # Add the upper limits
    my $limnum = $#limits;
    for ( my $i = $limnum; $i >= 0; $i-- ) {
        $limits[$i] = $limits[$i]/2;
        push( @limits, 100-$limits[$i] );
    }
    my ( @bootstrap_array, @jackknife_array, @new_alphas, @z0, @acc );
    # Loop the estimates of the first (original) model
    no warnings qw(numeric);
    for ( my $l = 0; $l < scalar @{$self -> bootstrap_estimates->[$model_number-1][0]}; $l++ ) {
        my ( @unsorted_array1, @unsorted_array2 );
        # Loop the bootstrap samples from 1 to get rid of original model
        for ( my $k = 1; $k < scalar @{$self -> bootstrap_estimates->[$model_number-1]}; $k++ ) {
            $unsorted_array1[$k-1] = $self -> bootstrap_estimates->[$model_number-1][$k][$l]; #can be undef
        }
        @{$bootstrap_array[$l]} = sort {$a <=> $b} @unsorted_array1;

        # Loop the jackknife samples from 1 to get rid of original model
        for ( my $k = 1; $k < scalar @{$self -> jackknife_estimates->[$model_number-1]}; $k++ ) {
            $unsorted_array2[$k-1] =$self -> jackknife_estimates->[$model_number-1][$k][$l]; #can be undef
        }
        @{$jackknife_array[$l]} = sort {$a <=> $b} @unsorted_array2;
        $z0[$l]         = c_get_z0     ( $bootstrap_array[$l],
                                         $self -> bootstrap_estimates -> [$model_number-1][0][$l] );
        $acc[$l]        = c_get_acc    ( $jackknife_array[$l],
                                         $self->result_parameters->{'jackknife_means'} ->[$model_number-1][0][$l] );
        $new_alphas[$l] = c_get_alphas ( \@limits, $acc[$l], $z0[$l] );
    }
    # Loop limits
    for ( my $lim_idx = 0; $lim_idx <= $#limits; $lim_idx++ ) {
        my @percentiles;
        # Loop parameters
        for ( my $l = 0; $l <= $#bootstrap_array; $l++ ) {
            my $limit = $new_alphas[$l][$lim_idx]/100;
            my $position = ( scalar @{$bootstrap_array[$l]} + 1 ) * $limit;
            my $percentile;
            if ( $position < 1 ) {
                $percentile = undef;
            } elsif ( $position > scalar @{$bootstrap_array[$l]} ) {
                $percentile = undef;
            } else {
                my ($int_med,$frac_med)   = split(/\./, $position );
                $frac_med = eval("0.".$frac_med);
                my $percentile_low  = $bootstrap_array[$l][ $int_med - 1];
                my $percentile_high = ( $bootstrap_array[$l][ $int_med ] -
                    $bootstrap_array[$l][ $int_med - 1] ) * $frac_med;
                $percentile = $percentile_low + $percentile_high;
            }
            push( @percentiles, $percentile );
        }
        push( @{$self->result_parameters->{'bca_confidence_intervals'} -> [$model_number-1]},
            \@percentiles );
        push( @{$self->result_parameters->{'bca_confidence_intervals_labels'}->[$model_number-1][0]},
            $limits[$lim_idx].'%');
    }
    # Check the intervals
    no warnings qw(uninitialized);
    for ( my $lim_idx = 0; $lim_idx <= $limnum; $lim_idx++ ) {
        my @within_ci;
        for ( my $l = 0; $l <= $#bootstrap_array; $l++ ) {
            my $lower_limit = $self->result_parameters->{'bca_confidence_intervals'} ->
            [$model_number-1][$lim_idx][$l];
            my $upper_limit = $self->result_parameters->{'bca_confidence_intervals'} ->
            [$model_number-1][($limnum*2+1)-$lim_idx][$l];
            if ( $self -> bca_confidence_intervals_check < $upper_limit and
                 $self -> bca_confidence_intervals_check > $lower_limit ) {
                push( @within_ci , 1 );
            } else {
                push( @within_ci , 0 );
            }
        }
        $self->result_parameters->{'within_bca_confidence_intervals'} ->
        [$model_number-1]{$limits[$lim_idx]*2} = \@within_ci;
    }
    $self->result_parameters->{'bca_confidence_intervals_labels'} -> [$model_number-1][1] =
    \@parameter_names;
}

sub calculate_jackknife_means
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 },
        parameter_names => { isa => 'ArrayRef[Str]', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my @parameter_names = defined $parm{'parameter_names'} ? @{$parm{'parameter_names'}} : ();

    my $maxcol=0;
    return unless (defined $self -> jackknife_estimates);
    unless (defined $self -> jackknife_estimates->[$model_number-1]){
        print "Jackknife estimates missing, cannot compute jackknife means.\n";
        return;
    }
    for ( my $k = 1; $k < scalar @{$self -> jackknife_estimates->[$model_number-1]}; $k++ ) {
        unless (defined $self -> jackknife_estimates->[$model_number-1][$k]){
            print "Jackknife estimates missing for model ".($k+1).", cannot compute jackknife means.\n";
            return;
        }
        if (scalar(@{$self -> jackknife_estimates->[$model_number-1][$k]})>$maxcol){
            $maxcol = scalar(@{$self -> jackknife_estimates->[$model_number-1][$k]});
        }
    }

    my @means;
    for (my $l = 0; $l < $maxcol; $l++) {
        my @parameter_array=();
        # From 1 to get rid of original model
        for (my $k = 1; $k < scalar @{$self->jackknife_estimates->[$model_number - 1]}; $k++) {
            next unless (defined $self->jackknife_estimates->[$model_number - 1][$k][$l]);
            push(@parameter_array, $self->jackknife_estimates->[$model_number - 1][$k][$l]);
        }
        if (scalar(@parameter_array)>0){
            $means[$l] = array::median(\@parameter_array);
        }else{
            $means[$l] = undef;
        }
        $self->result_parameters->{'jackknife_means'} -> [$model_number-1][0][$l] = $means[$l];
    }

    $self->result_parameters->{'jackknife_means_labels'} -> [$model_number-1] = [[],\@parameter_names];

}

sub calculate_medians
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 },
        parameter_names => { isa => 'ArrayRef[Str]', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my @parameter_names = defined $parm{'parameter_names'} ? @{$parm{'parameter_names'}} : ();
    #FIXME jackknife estimates?

    my @medians;
    # Loop the parameters
    for (my $l = 0; $l < $self->_number_of_columns; $l++) {
        my @parameter_array=();
        # From 1 to get rid of original model
        for (my $k = 1; $k < scalar @{$self->bootstrap_estimates->[$model_number - 1]}; $k++) {
            next unless (defined $self->bootstrap_estimates->[$model_number - 1][$k][$l]);
            push(@parameter_array, $self->bootstrap_estimates->[$model_number - 1][$k][$l]);
        }
        if (scalar(@parameter_array)>0){
            $medians[$l] = array::median(\@parameter_array);
        }else{
            $medians[$l] = undef;
        }
    }

    # The [0] in the index is there to indicate the 'model' level. Mostly used for printing
    $self->result_parameters->{'medians'} -> [$model_number - 1][0] = \@medians;
    $self->result_parameters->{'medians_labels'} -> [$model_number - 1] = [[],\@parameter_names];
}

sub calculate_standard_errors
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 },
        parameter_names => { isa => 'ArrayRef[Str]', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my @parameter_names = defined $parm{'parameter_names'} ? @{$parm{'parameter_names'}} : ();

    my @se;
    for (my $l = 0; $l < $self->_number_of_columns; $l++) {
        my @parameter_array=();
        # From 1 to get rid of original model
        for (my $k = 1; $k < scalar @{$self->bootstrap_estimates->[$model_number - 1]}; $k++) {
            next unless (defined $self->bootstrap_estimates->[$model_number - 1][$k][$l]);
            push(@parameter_array, $self->bootstrap_estimates->[$model_number - 1][$k][$l]);
        }
        if (scalar(@parameter_array)>0){
            $se[$l] = array::stdev(\@parameter_array);
        }else{
            $se[$l] = undef;
        }
    }
    # The [0] in the index is there to indicate the 'model' level. Mostly used for printing
    $self->result_parameters->{'standard_errors'} -> [$model_number - 1][0] = \@se;
    $self->result_parameters->{'standard_errors_labels'} -> [$model_number-1] = [[],\@parameter_names];
}

sub calculate_standard_error_confidence_intervals
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 },
        parameter_names => { isa => 'ArrayRef[Str]', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my @parameter_names = defined $parm{'parameter_names'} ? @{$parm{'parameter_names'}} : ();
    no warnings qw(uninitialized numeric);

    # Sort the limits from the inside out
    my @limits = sort { $b <=> $a } keys %{$self -> confidence_limits()};
    foreach my $limit ( @limits ) {
        my ( @lower_limits, @upper_limits, @within_ci );
        # Loop the estimates of the first (original) model
        for ( my $l = 0; $l < $self->_number_of_columns; $l++ ) {
            my $lower_limit =
                $self -> bootstrap_estimates->[$model_number-1][0][$l] -
                $self->result_parameters->{'standard_errors'}->[$model_number-1][0][$l] *
                $self -> confidence_limits ->{$limit};
            my $upper_limit =
                $self -> bootstrap_estimates->[$model_number-1][0][$l] +
                $self->result_parameters->{'standard_errors'}->[$model_number-1][0][$l] *
                $self -> confidence_limits ->{$limit};
            push( @lower_limits, $lower_limit );
            push(  @upper_limits, $upper_limit );
            if ( $self -> se_confidence_intervals_check < $upper_limit and
                $self -> se_confidence_intervals_check > $lower_limit ) {
                push( @within_ci , 1 );
            } else {
                push( @within_ci , 0 );
            }
        }
        unshift( @{$self->result_parameters->{'standard_error_confidence_intervals'} ->
            [$model_number-1]}, \@lower_limits );
        push( @{$self->result_parameters->{'standard_error_confidence_intervals'} ->
            [$model_number-1]}, \@upper_limits );
        $self->result_parameters->{'within_se_confidence_intervals'} ->
        [$model_number-1]{$limit} = \@within_ci;
        unshift( @{$self->result_parameters->{'standard_error_confidence_intervals_labels'} ->
            [$model_number-1][0]}, ($limit/2).'%' );
        push( @{$self->result_parameters->{'standard_error_confidence_intervals_labels'} ->
            [$model_number-1][0]}, (100-($limit/2)).'%' );
        push( @{$self->result_parameters->{'within_se_confidence_intervals_labels'} ->
            [$model_number-1][0]}, $limit.'%' );
    }
    $self->result_parameters->{'standard_error_confidence_intervals_labels'} -> [$model_number-1][1] =
    \@parameter_names;
    $self->result_parameters->{'within_se_confidence_intervals_labels'} -> [$model_number-1][1] =
    \@parameter_names;
}

sub calculate_percentile_confidence_intervals
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 },
        parameter_names => { isa => 'ArrayRef[Str]', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my @parameter_names = defined $parm{'parameter_names'} ? @{$parm{'parameter_names'}} : ();

    no warnings qw(uninitialized numeric);

    # Sort the limits from the inside out
    my @limits = sort { $b <=> $a } keys %{$self -> confidence_limits};
    foreach my $limit ( @limits ) {
        my ( @lower_limits, @upper_limits, @within_ci );
        # Loop the estimates of the first (original) model
        for ( my $l = 0; $l < $self->_number_of_columns; $l++ ) {
            my @parameter_array;
            # Loop the bootstrap samples from 1 to get rid of original model
            for ( my $k = 1; $k < scalar @{$self -> bootstrap_estimates->[$model_number-1]}; $k++ ) {
                my $val = $self -> bootstrap_estimates->[$model_number-1][$k][$l];
                # get rid of undefined values (these were probably deleted
                # when the bootstrap_estimates was created
                push( @parameter_array, $val ) if( defined $val and length($val)>0);
            }
            my @sorted = sort {$a <=> $b} @parameter_array;
            for my $side ( 'lower', 'upper' ) {
                my $use_limit = $side eq 'lower' ? $limit/200 : 1-($limit/200);
                # percentile postition is:
                my $percentile_position = ( $#sorted + 2 ) * $use_limit;
                my $percentile;
                if ( $percentile_position < 1 ) {
                    $percentile = undef;
                } elsif ( $percentile_position > $#sorted +1) {
                    $percentile = undef;
                } else {
                    my ($int_med,$frac_med)   = split(/\./, $percentile_position );
                    $frac_med = eval("0.".$frac_med);
                    my $percentile_low  = $sorted[ $int_med - 1];
                    my $percentile_high = ( $sorted[ $int_med ] - $sorted[ $int_med - 1] ) * $frac_med;
                    $percentile = $percentile_low + $percentile_high;
                }
                push( @lower_limits, $percentile ) if ( $side eq 'lower' );
                push( @upper_limits, $percentile ) if ( $side eq 'upper' );
            }
            if ( $self -> percentile_confidence_intervals_check() < $upper_limits[$#upper_limits] and
                $self -> percentile_confidence_intervals_check() > $lower_limits[$#lower_limits] ) {
                push( @within_ci , 1 );
            } else {
                push( @within_ci , 0 );
            }
        }
        unshift( @{$self->result_parameters->{'percentile_confidence_intervals'} ->
            [$model_number-1]}, \@lower_limits );
        push( @{$self->result_parameters->{'percentile_confidence_intervals'} ->
            [$model_number-1]}, \@upper_limits );
        unshift( @{$self->result_parameters->{'percentile_confidence_intervals_labels'}->
            [$model_number-1][0]}, ($limit/2).'%' );
        push( @{$self->result_parameters->{'percentile_confidence_intervals_labels'}->
            [$model_number-1][0]},(100-($limit/2)).'%');
        $self->result_parameters->{'within_percentiles'}->[$model_number-1]{$limit}=\@within_ci;
    }
    $self->result_parameters->{'percentile_confidence_intervals_labels'} ->
    [$model_number-1][1] = \@parameter_names;
}

sub general_setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 },
        class => { isa => 'Str', optional => 1 },
        subm_threads => { isa => 'Any' , optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my $class = $parm{'class'};

    my $subm_threads = $self->threads();
    my $own_threads = $self ->threads();
    # More threads than models?
    my $num = scalar @{$self ->models()};
    $own_threads = $num if ( $own_threads > $num );

    my $model = $self ->models() -> [$model_number-1];

    # Check which models that hasn't been run and run them This
    # will be performed each step but will only result in running
    # models at the first step, if at all.

    # If more than one process is used, there is a VERY high risk
    # of interaction between the processes when creating
    # directories for model fits. Therefore the directory
    # attribute is given explicitly below.

    # ------------------------  Run original run  -------------------------------

    unless ( $model -> is_run or (not $self->run_base_model)) {
        my %subargs = ();
        if ( defined $self -> subtool_arguments() ) {
            %subargs = %{$self -> subtool_arguments()};
        }

        if( $self -> nonparametric_etas() or
            $self -> nonparametric_marginals() ) {
            $model -> add_nonparametric_code;
        }

        my $orig_fit = tool::modelfit->new(
            %{common_options::restore_options(@common_options::tool_options)},
            base_directory => $self->base_directory,
            directory => $self->directory() . '/orig_modelfit_dir' . $model_number,
            models    => [ $model ],
            threads => $subm_threads,
            parent_threads => $own_threads,
            parent_tool_id => $self->tool_id(),
            logfile    => undef,
            raw_results => undef,
            prepared_models => undef,
            top_tool => 0,
            %subargs,
            copy_up => 1,
        );

        ui -> print( category => 'bootstrap',
            message => 'Executing base model.' );

        $orig_fit -> run;
        $self->metadata->{'copied_files'} = $orig_fit->metadata->{'copied_files'};
    }

    my $output = $model->outputs->[0];
    if (not $output->have_output) {
        if ($self->update_inits){
            ui -> print( category => 'bootstrap',
                         message => 'There is no output from the base model, will use base model initial values for bootstrap models.' );
        }
    }
    # }}} orig run


    my @problems   = @{$model -> problems};
    my @new_models;


    # TODO: In this loop we loose one dimension (problem) in that
    # all new models with new data sets are put in the same array,
    # regardless of which problem the initially belonged to. Fix
    # this.

    my $datafilenames = $model->datafiles(absolute_path => 1);
    my $done;
    for ( my $i = 1; $i <= scalar @problems; $i++ ) {
        my $ignoresign = defined $model -> ignoresigns ? $model -> ignoresigns -> [$i-1] : undef;
        my ( $junk, $idcol ) = $model -> _get_option_val_pos( name            => 'ID',
                                                              record_name     => 'input',
                                                              problem_numbers => [$i]);
        unless (defined $idcol->[0][0]){
            croak( "Error finding column ID in \$INPUT of model\n");
        }

        my $stratify_on;
        if (defined $self->stratify_on ) {
            my $found=0;
            # must be string
            my $counter = 1;
            foreach my $opt (@{$model->problems->[0]->inputs->[0]->options()}){
                if ($opt->name() eq $self->stratify_on){
                    $stratify_on = $counter;
                    $found=1;
                    last;
                }
                $counter++;
            }
            unless ($found){
                croak("Could not find any column with name ".$self->stratify_on." in \$INPUT of the model, ".
                    "set with option -stratify_on");
            }
        }

        my ( @seed, $new_datas, $incl_ids, $incl_keys, $new_mod, $new_subjects, $orig_count_ind, $all_individuals );

        $done = ( -e $self ->directory()."/m$model_number/done.$i" ) ? 1 : 0;
        if ( not $done ) {
            ui -> print( category => 'bootstrap',
                message  => "Resampling from ".$datafilenames->[$i-1]);

            ( $new_datas, $incl_ids, $incl_keys, $new_subjects, $orig_count_ind, $all_individuals )
                = data::bootstrap_create_datasets( output_directory   => $self ->directory().'/m'.$model_number,
                                                   name_stub   => 'bs_pr'.$i,
                                                   samples     => $self->samples(),
                                                   subjects    => $self->subjects(),
                                                   stratify_on => $stratify_on, #always a number
                                                   input_filename => $datafilenames->[$i-1],
                                                   ignoresign => $ignoresign,
                                                   idcolumn => $idcol->[0][0],  #number not index
                                                   missing_data_token => $self->missing_data_token
                                                       );
            $self->subjects($new_subjects);

            if (defined $output and (defined $output->nind) and (defined $output->nind->[($i-1)])){
                if ($output->nind->[($i-1)] > $orig_count_ind){
                    ui->print(category => 'all',
                              message => "\n\nWARNING: The number of individuals in the input dataset ".
                              "is $orig_count_ind, but in the original model lst-file NONMEM says number of individuals is ".$output->nind->[($i-1)].
                              ". This is an indication of IGNORE statements that cause all data records for some individuals to be ".
                              "excluded. It is recommended to filter the original dataset first with relevant IGNORE statements ".
                              "and then run bootstrap afterwards.");
                }elsif ($output->nind->[($i-1)] < $orig_count_ind){
                    ui->print(category => 'all',
                              message => "\n\nWARNING: The number of individuals in the input dataset ".
                              "is $orig_count_ind, but in the original model lst-file NONMEM says number of individuals is ".$output->nind->[($i-1)].
                              ". This is an indication of a different data file used for the bootstrap run, or an error in the data file parsing");
                }
            }

            for ( my $j = 0; $j < $self->samples(); $j++ ) {
                my ($model_dir, $filename) = OSspecific::absolute_path( $self ->directory().'/m'.$model_number,
                    'bs_pr'.$i.'_'.($j+1).'.mod' );
                my $prob_copy = Storable::dclone($problems[$i-1]); #bug here, is from undropped model
                $new_mod = model -> new( %{common_options::restore_options(@common_options::model_options)},
                                         sde                  => 0,
                                         outputs              => undef,
                                         active_problems      => undef,
                                         directory            => $model_dir,
                                         filename             => $filename,
                                         outputfile           => undef,
                                         problems             => [$prob_copy],
                                         extra_files          => $model -> extra_files,
                                         ignore_missing_files => 1 );

                unless ($self->keep_tables){
                    $new_mod -> remove_records( type => 'table' );
                }
                unless ($self->keep_covariance) {
                    $new_mod->remove_records(type => 'covariance');
                }
                if( $self -> shrinkage() ) {
                    $new_mod -> shrinkage_stats( enabled => 1 );
                    $new_mod -> shrinkage_modules( $model -> shrinkage_modules );
                }

                $new_mod -> datafiles( new_names => [$new_datas -> [$j]] );

                if( $self -> nonparametric_etas() or
                    $self -> nonparametric_marginals() ) {
                    $new_mod -> add_nonparametric_code;
                }

                if ($output -> have_output and $self->update_inits){
                    $new_mod -> update_inits( from_output => $output );
                }
                $new_mod -> _write;

                push( @new_models, $new_mod );
            }

            # Create a checkpoint. Log the samples and individuals.
            open( DONE, ">".$self ->directory()."/m$model_number/done.$i" ) ;
            print DONE "Resampling from ",$datafilenames->[$i-1], " performed\n";
            print DONE $self->samples()." samples\n";
            while( my ( $strata, $samples ) = each %{$self->subjects()} ) {
                print DONE "Strata $strata: $samples sample_size\n";
            }
            print DONE "Included individuals:\n";
            @seed = random_get_seed;
            print DONE "seed: @seed\n";
            for( my $k = 0; $k < scalar @{$incl_ids}; $k++ ) {
                print DONE join(',',@{$incl_ids -> [$k]}),"\n";
            }
            print DONE "Included keys:\n";
            for( my $k = 0; $k < scalar @{$incl_keys}; $k++ ) {
                print DONE join(',',@{$incl_keys -> [$k]}),"\n";
            }
            close( DONE );

            open my $all_fh, '>', $self->directory() . "all_individuals$model_number.csv";
            print $all_fh join(',', @{$all_individuals}), "\n";
            close $all_fh;

            open( INCL, ">".$self ->directory()."included_individuals".$model_number.".csv" ) ;
            for( my $k = 0; $k < scalar @{$incl_ids}; $k++ ) {
                print INCL join(',',@{$incl_ids -> [$k]}),"\n";
            }
            close( INCL );
            open( KEYS, ">".$self ->directory()."included_keys".$model_number.".csv" ) ;
            open( SAMPLEKEYS, ">".$self ->directory()."sample_keys".$model_number.".csv" ) ;
            my $ninds= $orig_count_ind;
            for( my $k = 0; $k < scalar @{$incl_keys}; $k++ ) {
                my %sample_keys;
                my $sample_size = scalar @{$incl_keys -> [$k]};
                for ( my $l = 0; $l < $ninds; $l++ ) {
                    $sample_keys{$incl_keys -> [$k][$l]}++;
                }
                for ( my $l = 0; $l < $ninds; $l++ ) {
                    my $val   = defined $sample_keys{$l} ? $sample_keys{$l} : 0;
                    my $extra = ($l == ($ninds-1)) ? "\n" : ',';
                    print SAMPLEKEYS $val,$extra;
                }
                print KEYS join(',',@{$incl_keys -> [$k]}),"\n";
            }
            close( KEYS );
            close( SAMPLEKEYS );
        } else {
            ui -> print( category => 'bootstrap',
                message  => "Recreating bootstrap from previous run." );

            # Recreate the datasets and models from a checkpoint
            my ($stored_filename, $stored_samples, %stored_subjects);
            my @seed;
            my ($stored_filename_found, $stored_samples_found, $stored_subjects_found, $stored_seed_found);
            open( DONE, $self ->directory()."/m$model_number/done.$i" );
            while( <DONE> ){
                if( /^Resampling from (.+) performed$/ ){
                    $stored_filename = $1;
                    $stored_filename_found = 1;
                    next;
                }
                if( /^(\d+) samples$/ ){
                    ui -> print( category => 'bootstrap',
                        message  => "Samples saved: $1" );
                    $stored_samples = $1;
                    $stored_samples_found = 1;
                    next;
                }
                if( /^(\d+) subjects$/ ){
                    # Old format (pre 2.2.2)
                    $stored_subjects{'default'} = $1;
                    $stored_subjects_found = 1;
                    next;
                }
                if( /^Strata (\w+): (\d+) sample_size$/ ){
                    ui -> print( category => 'bootstrap',
                        message  => "Strata $1, samples size: $2" );
                    $stored_subjects{$1} = $2;
                    $stored_subjects_found = 1;
                    next;
                }
                if( /^seed: (\d+) (\d+)$/ ){
                    @seed = ($1, $2);
                    $stored_seed_found = 1;
                    next;
                }

                if( $stored_filename_found and $stored_samples_found
                        and $stored_subjects_found and $stored_seed_found ){
                    last;
                }
            }
            close( DONE );
            unless( $stored_filename_found and $stored_samples_found
                    and $stored_samples_found and $stored_seed_found ){
                croak("The bootstrap/m1/done file could not be parsed.");
            }

            if ( $stored_samples < $self->samples() ) {
                croak("The number of samples saved in previous run ($stored_samples) ".
                    "is smaller than the number of samples specified for this run (".
                    $self->samples().")" );
            }
            while( my ( $strata, $samples ) = each %{$self->subjects()} ) {
                if ( $stored_subjects{$strata} != $samples ) {
                    croak("The number of individuals sampled i strata $strata ".
                        "in previous run (".
                        $stored_subjects{$strata}.
                        ") does not match the number of individuals specified ".
                        "for this run (".$samples.")" );
                }
            }
            no warnings qw (uninitialized);
            while( my ( $strata, $samples ) = each %stored_subjects ) {
                if ( $self->subjects()->{$strata} != $samples ) {
                    $self->subjects()->{$strata} = $samples;
                }
            }

            # Reinitiate the model objects
            for ( my $j = 1; $j <= $self->samples(); $j++ ) {
                my ($model_dir, $filename) = OSspecific::absolute_path( $self ->directory().'/m'.
                    $model_number,
                    'bs_pr'.$i.'_'.$j.'.mod' );
                $new_mod = model ->    new( directory   => $model_dir,
                                         filename    => $filename,
                                         extra_files => $model -> extra_files,
                                         ignore_missing_files => 1
                );

                unless ($self->keep_tables){
                    $new_mod -> remove_records( type => 'table' );
                }
                push( @new_models, $new_mod );
            }
            random_set_seed( @seed );
            ui -> print( category => 'bootstrap',
                message  => "Using $stored_samples previously resampled ".
                "bootstrap sets from $stored_filename" )
            unless $self -> parent_threads() > 1;
        }

    }
    $self -> prepared_models -> [$model_number-1]{'own'} = \@new_models;

    # ---------------------  Create the sub tools  ------------------------------

    my $subdir = $class;
    $subdir =~ s/tool:://;
    my @subtools = ();
    @subtools = @{$self -> subtools()} if (defined $self->subtools());
    shift( @subtools );
    my %subargs = ();
    if ( defined $self -> subtool_arguments() ) {
        %subargs = %{$self -> subtool_arguments()};
    }
    if ($subdir eq 'modelfit'){
        $subargs{'resume'}=$done; #do not rerun models that have lst-file in m1
    }

    $self->tools([]) unless (defined $self->tools());

    push( @{$self -> tools()},
        $class ->new( %{common_options::restore_options(@common_options::tool_options)},
            models         => \@new_models,
             copy_data            => 0,
            threads               => $subm_threads,
            directory             => $self ->directory().'/'.$subdir.'_dir'.$model_number,
            _raw_results_callback => $self ->
            _modelfit_raw_results_callback( model_number => $model_number ),
            subtools              => \@subtools,
            nmtran_skip_model => 2,
            parent_threads        => $own_threads,
            parent_tool_id        => $self -> tool_id(),
            logfile         => [$self -> logfile()->[$model_number-1]],
            raw_results           => undef,
            prepared_models       => undef,
            top_tool              => 0,
            %subargs,
            model_subdir => 0) );
}

sub modelfit_analyze
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    my @params = @{$self -> parameters()};
    my @diagnostic_params = @{$self -> diagnostic_parameters()};
    my ( @print_order, @calculation_order );
    my $jackknife;

    if ( $self -> type() eq 'bca' ) {
        #TODO support resume here for modelfit, do not rerun if lst-file exists for individual jackknife models
        # --------------------------  BCa method  ---------------------------------

        @calculation_order = @{$self -> bca_calculation_order()};
        @print_order = @{$self -> bca_print_order()};
        my $jk_threads = $self ->threads();
        my $done = ( -e $self ->directory()."/jackknife_done.$model_number" ) ? 1 : 0;
        if ( not $done ) {
            my ( $junk, $idcol ) = $self->models->[$model_number-1]->_get_option_val_pos(name=> 'ID',
                                                                                         record_name     => 'input',
                                                                                         problem_numbers => [1]);
            unless (defined $idcol->[0][0]){
                croak( "Error finding column ID in \$INPUT of model\n");
            }

            ui -> print( category => 'bootstrap',
                message  => "Running a Jackknife for the BCa estimates" );
            $jackknife =tool::cdd::jackknife ->    new( %{common_options::restore_options(@common_options::tool_options)},
                                                     directory        => undef,
                                                     models           => [$self->models->[$model_number-1]],
                                                     case_column     => $idcol->[0][0],
                                                     _raw_results_callback => $self ->
                                                     _jackknife_raw_results_callback( model_number => $model_number ),
                                                     nm_version       => $self -> nm_version(),
                                                     parent_tool_id   => $self -> tool_id(),
                                                     threads          => $jk_threads,
                                                     bca_mode         => 1,
                                                     shrinkage        => $self -> shrinkage(),
                                                     nonparametric_marginals => $self -> nonparametric_marginals(),
                                                     nonparametric_etas => $self -> nonparametric_etas(),
                                                     adaptive         => $self -> adaptive(),
                                                     verbose          => $self -> verbose(),
                                                     cross_validate   => 0,
                                                     model_subdir => 0 );

            # Create a checkpoint. Log the samples and individuals.
            open( DONE, ">".$self ->directory()."/jackknife_done.$model_number" ) ;
            print DONE "Jackknife directory:\n";
            print DONE $jackknife -> directory,"\n";
            my @seed = random_get_seed;
            print DONE "seed: @seed\n";
            close( DONE );


        } else {

            open( DONE, $self ->directory()."/jackknife_done.$model_number" );
            my @rows = <DONE>;
            close( DONE );
            my ( $stored_directory ) = $rows[1];
            chomp( $stored_directory );
            if ( not -e $stored_directory ) {
                croak("The Jackknife directory ".$stored_directory.
                    "indicated by ".$self ->directory().
                    "/jackknife_done.$model_number".
                    " from the old bootstrap run in ".
                    $self ->directory()." does not exist" );
            }
            my @seed = split(' ',$rows[2]);
            shift( @seed ); # get rid of 'seed'-word
            #set bins here to number of ids so that do not have to do it in jackknife.pm
            $jackknife = tool::cdd::jackknife ->new( %{common_options::restore_options(@common_options::tool_options)},
                                                     models           => [$self -> models -> [$model_number -1]],
                                                     case_column     => $self -> models -> [$model_number -1]-> idcolumn,
                                                     _raw_results_callback => $self ->    _jackknife_raw_results_callback( model_number => $model_number ),
                                                     threads          => $jk_threads,
                                                     parent_tool_id   => $self -> tool_id(),
                                                     directory        => $stored_directory,
                                                     bca_mode         => 1,
                                                     shrinkage        => $self -> shrinkage(),
                                                     nm_version       => $self -> nm_version(),
                                                     nonparametric_marginals => $self -> nonparametric_marginals(),
                                                     nonparametric_etas => $self -> nonparametric_etas(),
                                                     adaptive         => $self -> adaptive(),
                                                     verbose          => $self -> verbose(),
                                                     cross_validate   => 0,
                                                     model_subdir => 0 );

            random_set_seed( @seed );
            ui -> print( category => 'bootstrap',
                message  => "Restarting BCa Jackknife from ".
                $stored_directory )
            unless $self -> parent_threads() > 1;

            # }}} Recreate Jackknife

        }

        $jackknife -> run;

        $self -> jackknife_raw_results-> [$model_number-1] =
        $jackknife -> raw_results;

        # }}} BCa

    } else {
        #not bca
        @calculation_order = @{$self -> calculation_order()};
        @print_order = @{$self -> print_order()};
        $self -> bootstrap_raw_results() ->[$model_number-1] =
        $self -> tools() -> [0] -> raw_results;
    }


    my $problem_count= scalar(@{$self -> models -> [$model_number -1] -> problems});
    my ( @diagnostic_names, @tmp_names );
    foreach my $param ( @diagnostic_params ) {
        push( @tmp_names, $param );
        $tmp_names[$#tmp_names] =~ s/_/\./g;
    }

    for ( my $i = 0; $i < $problem_count; $i++ ) {
        push( @{$diagnostic_names[$i]}, @tmp_names );
    }

    if ($self->dofv()){
        my $model = $self -> models -> [$model_number -1];
        if (scalar(@{$model->problems()}) >1){
            print "\nbootstrap program does not support option -dofv for models with more than one \$PROBLEM.\n";
            return;
        }
        unless (-e $self->raw_results_file()->[$model_number-1]){
            print "\nNo raw results file found, nothing to do for -dofv.\n";
            return;
        }
        unless ($model->is_run and (defined $model->outputs->[0]->get_single_value(attribute => 'ofv'))){
            print "\nHave no ofv output from original model, nothing to do for -dofv.\n";
            return;
        }
        #skip original model ests, only use samples where est gave ofv (and params, we assume), not jackknife method
        my $filter;
        if ( $self -> type() eq 'bca' ) {
            $filter = ['method.eq.bootstrap'];
        }
        my ($sampled_params_arr,$href) = model::get_rawres_params(filename => $self->raw_results_file()->[$model_number-1],
                                                                  string_filter => $filter,
                                                                  require_numeric_ofv => 1,
                                                                  offset => 1,
                                                                  model => $model);

        if (scalar(@{$sampled_params_arr})<1){
            print "\nNo bootstrap samples gave ofv value, nothing to do for -dofv.\n";
            return;
        }

        #in is mceta, sampled_params_arr, model, m1directory, base_directory out is array of models
        #static method in model

        my $modelsarr = model::create_maxeval_zero_models_array(
            sampled_params_arr => $sampled_params_arr,
            mceta => $self->mceta(),
            basedirectory => $self->directory,
            subdirectory => $self->directory().'m'.$model_number.'/',
            model => $model,
            purpose => 'dofv'
            );

        my $samples_done=0;
        while ($samples_done < scalar(@{$sampled_params_arr})){
            push(@{$self->dofv_samples},$sampled_params_arr->[$samples_done]->{'model'});
            $samples_done++;
        }

        #Handle $ETAS
        my $phi_file = $model->get_phi_file();
        if (defined $phi_file) {
            for my $model (@$modelsarr) {
                my @extra_files;
                if (defined $model->extra_files) {
                    @extra_files = @{$model->extra_files};
                }

                @extra_files = ( @extra_files, $phi_file );
                $model->extra_files(\@extra_files);
            }
        }

        #we use original data set here, use input copy_data
        my $modelfit = tool::modelfit -> new(    %{common_options::restore_options(@common_options::tool_options)},
                                                top_tool         => 0,
                                                models           => $modelsarr,
                                                base_directory   => $self -> directory,
                                                directory        => $self -> directory.'compute_dofv_dir'.$model_number,
                                                raw_results_file => [$self->directory.$self->raw_results_dofv],
                                                nmtran_skip_model => 2,
                                                parent_tool_id   => $self -> tool_id,
                                                _raw_results_callback => $self ->_dofv_raw_results_callback( model_number => $model_number ),
                                                logfile             => undef,
                                                copy_data          => $self->copy_data,
                                                threads          => $self->threads,
                                                model_subdir => 0,
                                            );

        $modelfit->run;

        #if clean >=3 this file has been deleted, but the raw_results is ok thanks to explicit setting
        #of filename above.
        if (-e $modelfit->directory.'raw_results_structure'){
            cp ($modelfit->directory.'raw_results_structure',$modelfit->base_directory.'raw_results_structure_dofv');
        }
    }
}

sub _modelfit_raw_results_callback
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my $subroutine;

    # Use the bootstrap's raw_results file.
    my ($dir,$file) =
    OSspecific::absolute_path( $self ->directory(),
        $self -> raw_results_file()->[$model_number-1] );
    my ($dir2,$nonp_file) = OSspecific::absolute_path( $self ->directory(),
                                                      $self -> raw_nonp_file()->[$model_number-1] );
    my $orig_mod = $self ->models()->[$model_number-1];
    my $type = $self -> type();
    $subroutine = sub {
        my $modelfit = shift;
        my $mh_ref   = shift;
        my %max_hash = %{$mh_ref};
        $modelfit -> raw_results_file([$dir.$file] );
        $modelfit -> raw_nonp_file( [$dir2.$nonp_file] );

        # The prepare_raw_results in the modelfit will fix the
        # raw_results for each bootstrap sample model, we must add
        # the result for the original model.

        my %dummy;
        my ($raw_results_row, $nonp_rows);
        if ($orig_mod -> is_run){
            ($raw_results_row, $nonp_rows) = $self -> create_raw_results_rows( max_hash => $mh_ref,
                                                                               model => $orig_mod,
                                                                               raw_line_structure => \%dummy );
        }else{
            #copy first existing row and put NA everywhere
            my @row = ();
            push(@row, @{$modelfit -> raw_results()->[0]});
            $row[0] = '0'; #model number
            for (my $i=3; $i<scalar(@row); $i++){
                $row[$i]=undef;
            }
            $raw_results_row = [];
            push(@{$raw_results_row},\@row);
        }
        unshift( @{$modelfit -> raw_results()}, @{$raw_results_row} );

        $self->raw_line_structure($modelfit -> raw_line_structure());
        if ( $type eq 'bca' ) {
            my $first = 1;
            foreach my $row ( @{$modelfit -> raw_results()} ) {
                if ($first){
                    unshift( @{$row}, 'original' );
                    $first=0;
                }else{
                    unshift( @{$row}, 'bootstrap' );
                }
            }
            unshift( @{$modelfit -> raw_results_header()}, 'method' );

            foreach my $mod (sort({$a <=> $b} keys %{$self->raw_line_structure()})){
                foreach my $category (keys %{$self->raw_line_structure() -> {$mod}}){
                    next if ($category eq 'line_numbers');
                    my ($start,$len) = split(',',$self->raw_line_structure() -> {$mod}->{$category});
                    $self->raw_line_structure() -> {$mod}->{$category} = ($start+1).','.$len; #+1 for method
                }
                $self->raw_line_structure() -> {$mod}->{'method'} = '0,1';
            }
        }
        $self->raw_line_structure() -> {'0'} = $self->raw_line_structure() -> {'1'}; #FIXME, rely on sample 1 not crash
        $self->raw_line_structure() -> write( $dir.'raw_results_structure' );

        $self -> raw_results_header($modelfit -> raw_results_header());

    };
    return $subroutine;
}

sub _dofv_raw_results_callback
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my $subroutine;

    my $orig_mod = $self ->models()->[$model_number-1];
    my $orig_ofv;
    if ($orig_mod->is_run){
        #in dofv we only handle one $PROB, but could do array over probs here
        $orig_ofv=$orig_mod->outputs->[0]->get_single_value(attribute => 'ofv');
    }
    my @dofv_samples = @{$self->dofv_samples()};
    unshift (@dofv_samples,'original'); #for original model

    $subroutine = sub {
        my $modelfit = shift;
        my $mh_ref   = shift;
        my %max_hash = %{$mh_ref};

        # The prepare_raw_results in the modelfit will fix the
        # raw_results for each dofv sample model, we must add
        # the result for the original model.

        my %dummy;

        my ($raw_results_row, $nonp_rows) = $self -> create_raw_results_rows( max_hash => $mh_ref,
            model => $orig_mod,
            raw_line_structure => \%dummy );

        $raw_results_row->[0]->[0] = 'input';

        unshift( @{$modelfit -> raw_results()}, @{$raw_results_row} );

        if ( defined $orig_ofv ) {
            my ($start,$len) = split(',',$modelfit->raw_line_structure() -> {1}->{'problem'});
            my $problemindex = $start;
            ($start,$len) = split(',',$modelfit->raw_line_structure() -> {1}->{'ofv'});
            my $ofvindex=$start;
            croak("could not find ofv in raw results header") unless (defined $ofvindex);

            my $si=0;
            foreach my $row ( @{$modelfit -> raw_results()} ) {
                my $delta_ofv;
                if (defined $row->[$ofvindex]){
                    $delta_ofv = $row->[$ofvindex] - $orig_ofv;
                }
                my @oldrow =@{$row};
                $row = [@oldrow[0 .. $problemindex],$dofv_samples[$si],@oldrow[$problemindex+1 .. $ofvindex],$delta_ofv,@oldrow[$ofvindex+1 .. $#oldrow]];
                $si++;
            }

            my @old_header = @{$modelfit -> raw_results_header()};
            my $headerindex;
            my $probheadindex;
            for (my $k=0; $k<scalar(@old_header);$k++){
                $headerindex = $k if ($old_header[$k] eq 'ofv');
                $probheadindex = $k if ($old_header[$k] eq 'problem');
            }
            $modelfit -> raw_results_header(
                [@old_header[0 .. $probheadindex],'bs_data_id',@old_header[$probheadindex+1 .. $headerindex],'deltaofv',@old_header[$headerindex+1 .. $#old_header]]);

            foreach my $mod (sort({$a <=> $b} keys %{$modelfit->raw_line_structure()})){
                foreach my $category (keys %{$modelfit->raw_line_structure() -> {$mod}}){
                    next if ($category eq 'line_numbers');
                    my ($start,$len) = split(',',$modelfit->raw_line_structure() -> {$mod}->{$category});
                    #we know model comes before ofv
                    if ($start > $ofvindex){
                        $modelfit->raw_line_structure() -> {$mod}->{$category} = ($start+2).','.$len; #+2 for bs_sample, ofv
                    }elsif ($start > $problemindex){
                        $modelfit->raw_line_structure() -> {$mod}->{$category} = ($start+1).','.$len; #+1 for bs_sample
                    }
                }
                $modelfit->raw_line_structure() -> {$mod}->{'deltaofv'} = ($ofvindex+2).',1'; #+2 to handle bs_sample
                $modelfit->raw_line_structure() -> {$mod}->{'bs_sample'} = ($problemindex+1).',1';
            }
        }
        $modelfit->raw_line_structure() -> {'input'} = $modelfit->raw_line_structure() -> {'1'}; #input model
        my ($dir,$file) =
        OSspecific::absolute_path( $modelfit ->directory(),
            $modelfit -> raw_results_file()->[$model_number-1] );
        $modelfit->raw_line_structure() -> write( $dir.'raw_results_structure' );


    };

    return $subroutine;
}

sub _jackknife_raw_results_callback
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my $subroutine;

    # Use the bootstrap's raw_results file.
    my ($dir,$file) =
    OSspecific::absolute_path( $self ->directory(),
        $self -> raw_results_file()->[$model_number-1] );
    my ($dir2,$nonp_file) =
    OSspecific::absolute_path( $self ->directory(),
        $self -> raw_nonp_file()->[$model_number-1] );
    $subroutine = sub {
        my $jackknife = shift;
        my $modelfit  = shift;
        $modelfit -> raw_results_file( [$dir.$file] );
        $modelfit -> raw_nonp_file( [$dir2.$nonp_file] );
        $modelfit -> raw_results_append( 1 );
        my ( @new_header, %param_names );
        unless (defined $modelfit -> raw_results()){
            $modelfit -> raw_results([]);
        }
        foreach my $row ( @{$modelfit -> raw_results()} ) {
            unshift( @{$row}, 'jackknife' );
        }

        $modelfit -> raw_results_header([]);
    };
    return $subroutine;
}

sub bca_read_raw_results
{
    my $self = shift;

    $self -> raw_results_header([]);
    for ( my $i = 1; $i <= scalar @{$self->models()}; $i++ ) { # All models
        if ( defined $self -> raw_results_file()
                and -e $self -> raw_results_file()->[$i-1] ) {
            open( RRES, $self -> raw_results_file()->[$i-1] );

            my @read_file = <RRES>;
            close( RRES );
            my @file;

            foreach (@read_file){
                chomp;
                if (/\"\,\".*/ ){
                    s/^\"//;
                    s/\"$//;
                    my @tmp = split('\"\,\"',$_);
                    push (@file,\@tmp);
                } else {
                    my @tmp = split(',',$_);
                    push (@file,\@tmp);
                }
            }

            my $header = shift @file;

            # Get rid of 'method' column
            my $cols = scalar(@{$header})-1;
            @{$self -> raw_results_header()->[$i-1]} = @{$header}[1..$cols];
            $self->raw_results([]) unless (defined $self->raw_results);
            $self -> raw_results() -> [$i-1] = \@file;
            for( my $j = 0; $j <= $#file; $j++ ) {
                if ( $file[$j][0] eq 'jackknife' ) {
                    shift( @{$file[$j]} );
                    push( @{$self -> jackknife_raw_results()->[$i-1]}, $file[$j]);
                } else {
                    shift( @{$file[$j]} );
                    push( @{$self -> bootstrap_raw_results()->[$i-1]}, $file[$j] );
                }
            }
        }
    }
}

sub prepare_results
{
    my $self = shift;

    my ( @calculation_order, @print_order, %diag_idx );
    if ( $self -> type() eq 'bca' ) {
        @calculation_order = @{$self -> bca_calculation_order()};
        @print_order = @{$self -> bca_print_order()};
    } else {
        @calculation_order = @{$self -> calculation_order()};
        @print_order = @{$self -> print_order()};
    }
    if ( $self -> type() eq 'bca' ) {
        $self -> bca_read_raw_results();
    } else {
        $self -> read_raw_results();
        $self -> bootstrap_raw_results ($self -> raw_results());
    }
    unless (defined $self->raw_line_structure){
        #not tested for Bca...
        $self->raw_line_structure(ext::Config::Tiny -> read($self->directory.'raw_results_structure'));
    }

    #make sure that we have valid raw_line_structure and not from crashed run here
    my $valid_raw_line;
    for (my $i=1;$i <= $self->samples; $i++){
        if (defined $self->raw_line_structure()->{$i}->{'ofv'}) {
            $valid_raw_line = $i;
            last;
        }
    }

    my %rawres_structure = %{$self->raw_line_structure->{$valid_raw_line}};
    # Check if the raw_results_structure file was created with -dofv. In that case it has to be adjusted
    if (exists $rawres_structure{'bs_sample'} and exists $rawres_structure{'deltaofv'}) {   # bs_sample corresponds to the bs_data_id column
        _adjust_rawres_structure(\%rawres_structure);
    }
    my ($startcolumn, $l) = split(',', $rawres_structure{'ofv'});

    for ( my $i = 0; $i < scalar @{$self -> diagnostic_parameters()}; $i++ ) {
        my ($start,$length) = split(',', $rawres_structure{$self -> diagnostic_parameters() -> [$i]});
        $diag_idx{$self -> diagnostic_parameters() -> [$i]} = $start;
    }
    my $start_check;
    ($start_check,$l) = split(',', $rawres_structure{'theta'});

    # ---------------------  Get data from raw_results  -------------------------

    # Divide the data into diagnostics and estimates. Included in estimates are
    # the parametric estimates, the standard errors of these, the nonparametric
    # estimates, the shrinkage in eta and the shrinkage in wres
    # The diagnostics end up in {'bootstrap_diagnostics'} and
    # {'jackknife_diagnostics'}. The estimates in bootstrap_estimates and
    # jackknife_estimates.
    # The number of runs that are selected for calculation of the results is
    # saved.

    foreach my $tool ( 'bootstrap', 'jackknife' ) {
        my $rawres = $tool.'_raw_results';
        my $diagnostics = $tool.'_diagnostics';
        my $estimates = $tool.'_estimates';
        if ( defined $self -> $rawres ) {
            for ( my $i = 0; $i < scalar @{$self->$rawres}; $i++ ) { # All models

                my $estimate_columns=0;

                my $cols_orig = 0;
                foreach my $param ( 'theta', 'omega', 'sigma' ) {
                    my $labels =
                    $self ->models() -> [$i] -> labels( parameter_type => $param );
                    # we can't use labels directly since different models may have different
                    # labels (still within the same modelfit)
                    my $numpar = scalar @{$labels -> [0]} if ( defined $labels and defined $labels -> [0] );
                    $estimate_columns += $numpar if (defined $numpar);
                    $cols_orig += ( $numpar*3 ); # est + SE + eigen values
                }
                # nonparametric omegas and shrinkage
                my $nomegas = $self ->models() -> [$i] -> nomegas;
                my $numpar = $nomegas -> [0];

                # shrinkage omega + wres shrinkage
                $cols_orig += $numpar + 1;

                $cols_orig++; # OFV

                my %return_section;
                $return_section{'name'} = 'Warnings';
                my ( $skip_term, $skip_cov, $skip_warn, $skip_bound, $skip_crash );
                my $included = 0;
                for ( my $j = 0; $j < scalar @{$self->$rawres->[$i]}; $j++ ) { # orig model + prepared_models
                    my $totalcolumns = scalar @{$self->$rawres->[$i][$j]};

                    # -----------------------  Diagnostics  -----------------------------

                    for ( my $m=0; $m < scalar @{$self -> diagnostic_parameters()}; $m++ ) { # value
                        $self->$diagnostics->[$i][$j][$m] =
                        $self->$rawres->[$i][$j][$diag_idx{$self -> diagnostic_parameters() -> [$m]}];
                    }

                    my $use_run = 1;
                    if ($j != 0) {  # Don't check for failures on original model
                        if ( not defined $self->$rawres->[$i][$j][$diag_idx{'minimization_successful'}] ) {
                            $skip_crash++;
                            $use_run = 0;
                        }else{
                            for (my $m=$start_check; $m< ($start_check+$estimate_columns);$m++){
                                #start at first theta col, all estimates
                                if (not defined $self->$rawres->[$i][$j][$m] ){
                                    $skip_crash++;
                                    $use_run = 0;
                                    last;
                                }
                            }
                        }

                        if (not $use_run){
                            #do not bother with the rest of the filtering
                        }elsif ( $self -> skip_minimization_terminated and
                            ( not $self->$rawres->[$i][$j][$diag_idx{'minimization_successful'}] ) ) {
                            $skip_term++;
                            $use_run = 0;
                        } elsif ( $self -> skip_covariance_step_terminated and not
                            $self->$rawres->
                            [$i][$j][$diag_idx{'covariance_step_successful'}] ) {
                            $skip_cov++;
                            $use_run = 0;
                        } elsif ( $self -> skip_with_covstep_warnings and
                            $self->$rawres->
                            [$i][$j][$diag_idx{'covariance_step_warnings'}] ) {
                            $skip_warn++;
                            $use_run = 0;
                        } elsif ( $self -> skip_estimate_near_boundary and
                            $self->$rawres->
                            [$i][$j][$diag_idx{'estimate_near_boundary'}] ) {
                            $skip_bound++;
                            $use_run = 0;
                        }
                    }
                    # ------------------------  Estimates  ------------------------------

                    if ($use_run) {
                        unless ($j==0){#do not use original row ($j==0)
                            if (not defined $self->_number_of_columns){
                                $self->_number_of_columns($totalcolumns-$startcolumn);
                            }elsif ($self->_number_of_columns < ($totalcolumns-$startcolumn)){
                                #if e.g. this has cov step successful but first included did not
                                $self->_number_of_columns($totalcolumns-$startcolumn);
                            }
                            $included++;
                        }
                        for (my $m = $startcolumn; $m < $totalcolumns ; $m++) {
                            $self->$estimates->[$i][$included][($m-$startcolumn)] = $self->$rawres->[$i][$j][$m];
                        }
                    }
                }

                if ($included < 1){
                    croak("No runs passed the run exclusion critera. Statistics cannot be calculated by PsN. ".
                        "Run bootstrap again with option -summarize and different exclusion criteria.");
                }

                my %run_info_return_section;
                my @run_info_labels=('Date','PsN version','NONMEM version');
                my @datearr=localtime;
                my $the_date=($datearr[5]+1900).'-'.($datearr[4]+1).'-'.($datearr[3]);
                my @run_info_values =($the_date,'v'.$PsN::version,$self->nm_version());
                $run_info_return_section{'labels'} =[[],\@run_info_labels];
                $run_info_return_section{'values'} = [\@run_info_values];

                if ( defined $skip_crash ) {
                    push( @{$return_section{'values'}}, "$skip_crash crashed runs ".
                        "were skipped when calculating the $tool results" );
                }
                if ( defined $skip_term ) {
                    push( @{$return_section{'values'}}, "$skip_term runs with miminization ".
                        "terminated were skipped when calculating the $tool results" );
                }
                if ( defined $skip_cov ) {
                    push( @{$return_section{'values'}}, "$skip_cov runs with aborted ".
                        "covariance steps were skipped when calculating the $tool results" );
                }
                if ( defined $skip_warn ) {
                    push( @{$return_section{'values'}}, "$skip_warn runs with errors from ".
                        "the covariance step were skipped when calculating the $tool results" );
                }
                if ( defined $skip_bound ) {
                    push( @{$return_section{'values'}}, "$skip_bound runs with estimates ".
                        "near a boundary were skipped when calculating the $tool results" );
                }
                $return_section{'labels'} = [];
                push( @{$self -> results->[$i]{'own'}},\%return_section );

                # }}} push #runs to results

            }
        }
    }

    # ----------------------  Calculate the results  ----------------------------

    unless (defined $self -> bootstrap_raw_results()){
        croak("No bootstrap_raw_results array");
    }
    for ( my $i = 0; $i < scalar @{$self -> bootstrap_raw_results()} ; $i++ ) { # All models

        # diagonstic_parameters,
        # which is one more for
        # the method column added
        # with a bca run.
        my ($start, $l) = split(',', $rawres_structure{'ofv'});

        my @param_names = @{$self -> raw_results_header()->[$i]}[$start .. (scalar @{$self -> raw_results_header->[$i]} - 1)];
        my ( @diagnostic_names, @tmp_names );
        foreach my $param ( @{$self -> diagnostic_parameters()} ) {
            push( @tmp_names, $param );
            $tmp_names[$#tmp_names] =~ s/_/\./g;
        }

        @diagnostic_names = @tmp_names;
        foreach my $result_type ( @calculation_order ) {
            my @names = $result_type eq 'diagnostic_means' ?
            @diagnostic_names : @param_names;
            my $calc = 'calculate_'.$result_type;
            $self -> $calc( model_number    => ($i+1),parameter_names => \@names );
        }
        foreach my $result_type ( @print_order ) {
            my $name = $result_type;
            $name =~ s/_/\./g;
            my %return_section;
            $return_section{'name'} = $name;
            $return_section{'values'} = $self -> result_parameters -> {$result_type} -> [$i];
            $return_section{'labels'} = $self -> result_parameters -> {$result_type.'_labels'} -> [$i];
            push( @{$self -> results->[$i]{'own'}},\%return_section );
        }
    }

    my $directory = $self->directory;
    PsN::call_pharmpy("results bootstrap $directory");
    if ($self->rplots > 0) {
        PsN::call_pharmpy("results report $directory");
    }
}

sub create_R_plots_code
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              rplot => { isa => 'rplots', optional => 0 }
        );
    my $rplot = $parm{'rplot'};

    my $inclIdFile = "included_individuals1.csv";
    #TODO check that these options are not inverted!!
    my $minFailed = $self->skip_minimization_terminated() ? 'TRUE' : 'FALSE';
    my $covFailed = $self->skip_covariance_step_terminated() ? 'TRUE' : 'FALSE';
    my $covWarnings = $self->skip_with_covstep_warnings() ? 'TRUE' : 'FALSE';
    my $boundary = $self->skip_estimate_near_boundary() ? 'TRUE' : 'FALSE';

    #todo add bool dofv
    my $estim_params = '';
    my $labelref = $self->models->[0]->problems->[0]->get_estimated_attributes(parameter => 'all',
                                                                               attribute => 'labels');

    if (defined $labelref){
        #we should not filter out off-diagonals like in sse. Verified.
        $estim_params = "'".join("','",@{$labelref})."'";
    }
    my $paramstring = 'ESTIMATED.PARAMS <- c('.$estim_params.')';

    $rplot->add_preamble(code => [
                             '#bootstrap-specific preamble',
                             $paramstring,
                             "included.ids.file <- '".$inclIdFile."'",
                             "skip.minimization.terminated=$minFailed",
                             "skip.covariance.step.terminated=$covFailed",
                             "skip.with.covstep.warnings=$covWarnings",
                             "skip.estimate.near.boundary=$boundary",
                         ]);
}

sub _adjust_rawres_structure
{
    # Adjust rawres_structure for extra columns after dofv run.

    my $struct = shift;

    sub getind
    {
        my $pair = shift;
        my @a = split /,/, $pair;
        return $a[0];
    }

    sub decrement
    {
        my $pair = shift;
        my $dec = shift;

        my @a = split /,/, $pair;
        $a[0] -= $dec;
        return $a[0] . "," . $a[1];
    }

    my $bs_sample_ind = getind($struct->{'bs_sample'});
    my $dofv_ind = getind($struct->{'deltaofv'});

    foreach my $header (keys %$struct) {
        next if ($header eq 'line_numbers');
        my $dec;
        if (getind($struct->{$header}) > $dofv_ind) {
            $dec = 2;
        } elsif (getind($struct->{$header}) > $bs_sample_ind) {
            $dec = 1;
        } else {
            $dec = 0;
        }
        $struct->{$header} = decrement($struct->{$header}, $dec);
    }
}

1;
