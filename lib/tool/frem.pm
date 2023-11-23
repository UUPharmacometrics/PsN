package tool::frem;

use PsN;
use include_modules;
use tool::modelfit;
use Config;
use linear_algebra;

use ui;
use logging;
use File::Copy qw/cp mv/;
use File::Path 'rmtree';
use File::Spec;
use nmtablefile;
use utils::phitable;
use array;
use tool::sir;
use model_approximations;
use input_checking;
use POSIX qw/floor/;

use Mouse;
use MouseX::Params::Validate;

extends 'tool';

#FIXME dv synonym automatic handling

my $fremtype = 'FREMTYPE';
my $small_correlation = 0.01;

has 'deriv2_nocommon_maxeta'  => ( is => 'rw', isa => 'Int', default=> 60);
has 'skip_omegas' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'run_sir' => ( is => 'rw', isa => 'Bool', default=> 0);
has 'always_proposal_density' => ( is => 'rw', isa => 'Bool', default=> 1);
has 'mu' => ( is => 'rw', isa => 'Bool', default=> 0);
has 'skip_etas' => ( is => 'rw', isa => 'Int', default=> 0);
has 'rse' => ( is => 'rw', isa => 'Num', default=> 30);
has 'start_omega_record' => ( is => 'rw', isa => 'Int', default=> 1);
has 'estimate' => ( is => 'rw', isa => 'Int', default => 3 );
has 'mceta' => ( is => 'rw', isa => 'Int', default => 0 );
has 'occasionlist' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'extra_input_items' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'invariant_median' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'invariant_mean' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'timevar_median' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'invariant_covmatrix' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'timevar_covmatrix' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'input_model_fix_thetas' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'input_model_fix_omegas' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'input_model_fix_sigmas' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'check' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'rescale' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'rescale_data' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'dv' => ( is => 'rw', isa => 'Str', default => 'DV' );
has 'occasion' => ( is => 'rw', isa => 'Str');
has 'time_varying' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'covariates' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'categorical' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'log' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'regular' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef', default => sub { ['frem.log'] } );
has 'use_pred' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'estimate_means' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'estimate_covariates' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'has_missingness' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'cholesky' => ( is => 'rw', isa => 'Bool', default => 0 );

has 'results_file' => ( is => 'rw', isa => 'Str', default => 'frem_results.csv' );
has 'model_1' => ( is => 'rw', isa => 'model' );
has 'model_2' => ( is => 'rw', isa => 'model' );
has 'model_3' => ( is => 'rw', isa => 'model' );
has 'final_numbers' => ( is => 'rw', isa => 'ArrayRef[Int]', default => sub { [] } );
has 'final_models' => ( is => 'rw', isa => 'ArrayRef[model]', default => sub { [] } );
has 'cov_summary' => ( is => 'rw', isa => 'Str' );
has 'tool_options' => ( is => 'rw', isa => 'HashRef' );     # tool options hash to override all tool options from the command line.
has 'bipp' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'force_posdef_covmatrix' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'force_posdef_samples' => ( is => 'rw', isa => 'Int', default => 500 );


has '_intermediate_models_path' => ( is => 'rw', isa => 'Str' );
has 'etas_reorder_mapping' => ( is => 'rw', isa => 'HashRef' );
has 'derivatives' => ( is => 'rw', isa => 'Bool', default => 0 );
has '_final_models_path' => ( is => 'rw', isa => 'Str' );
has '_likelihood' => ( is => 'rw', isa => 'Bool', default => 0 );
has '_loglikelihood' => ( is => 'rw', isa => 'Bool', default => 0 );
has '_have_fflag' => ( is => 'rw', isa => 'Bool', default => 0 );


my $logger = logging::get_logger("frem");


sub BUILD
{
    my $self = shift;
    my $model = $self->models->[0];
    my $problem = $model->problems->[0];
    my $datafiles = $model->datafiles(absolute_path => 1);
    my $data = data->new(
        filename => $datafiles->[0],
        ignoresign => defined $model->ignoresigns ? $model->ignoresigns->[0] : undef,
        missing_data_token => $self->missing_data_token,
        idcolumn => $model->idcolumns->[0],
    );

    if (not defined $self->tool_options) {
        $self->tool_options(common_options::restore_options(@common_options::tool_options));
    }

    # Check if any covariate column has all same value
    # In that case warn and remove column
    my $code = "from pharmpy.tools.frem.tool import setup; print(setup(" . PsN::path_literal($model->full_name) . ',' . PsN::python_array($self->covariates) . "))";
    my $filtered_covariates = PsN::call_pharmpy_wrapper($code);
    $filtered_covariates = PsN::from_python_array($filtered_covariates);
    my @filtered_categorical;
    for my $cov (@$filtered_covariates) {
        if (grep { $_ eq $cov } @{$self->categorical}) {
            push @filtered_categorical, $cov;
        }
    }
    $self->covariates($filtered_covariates);
    $self->categorical(\@filtered_categorical);

    for my $accessor ('logfile', 'raw_results_file', 'raw_nonp_file') {
        my @new_files;
        my @old_files = @{$self->$accessor};
        for (my $i = 0; $i < scalar(@old_files); $i++) {
            my ($ldir, $name) = OSspecific::absolute_path($self->directory, $old_files[$i]);
            push(@new_files,$ldir.$name);
        }
        $self->$accessor(\@new_files);
    }

    $self->check_model_features();

    if (scalar(@{$self->covariates}) == 0) {
        croak("Must have at least one covariate");
    }

    if (scalar(@{$self->log}) > 0) {
        my $indices = array::get_array_positions(target => $self->covariates,
                                                 keys=> $self->log,
                                                 R_indexing => 0);
        if (scalar(@{$indices}) != scalar(@{$self->log})) {
            croak("-log list:" . join(',', @{$self->log}) . " is not a subset of " .
                " -covariates:" . join(',', @{$self->covariates}));
        }
    }
    if (scalar(@{$self->categorical}) > 0) {
        my $indices = array::get_array_positions(target => $self->covariates,
                                                 keys=> $self->categorical,
                                                 R_indexing => 0);
        if (scalar(@{$indices}) != scalar(@{$self->categorical})) {
            croak("-categorical list:" . join(',', @{$self->categorical}) . " is not a subset of " .
                  " -covariates:" . join(',', @{$self->covariates}));
        }
    }
    if (scalar(@{$self->log}) > 0) {
        my $indices = array::get_array_positions(target => $self->categorical,
                                                 keys=> $self->log,
                                                 R_indexing => 0);
        if (scalar(@{$indices}) > 0) {
            croak("-log list:" . join(',', @{$self->log}) . " must have no elements in common with " .
                  " -categorical:" . join(',', @{$self->categorical}));
        }
    }

    my $regular = get_regular_covariates(covariates => $self->covariates,
                                         categorical => $self->categorical,
                                         log => $self->log);
    $self->regular($regular);

    my $dv_ok = 0;

    my $prob = $self->models->[0]->problems->[0];
    if (defined $prob->inputs and defined $prob->inputs->[0]->options) {
        foreach my $option (@{$prob->inputs->[0]->options}) {
            unless (($option->value eq 'DROP' or $option->value eq 'SKIP'
                        or $option->name eq 'DROP' or $option->name eq 'SKIP')) {
                $dv_ok = 1 if ($option->name() eq $self->dv());
            }
        }
        croak("dependent column " . $self->dv() . " not found in \$INPUT") if not $dv_ok;
    } else {
        croak('Trying to check parameters in input model but no headers were found in $INPUT');
    }

    my @code = @{$self->models->[0]->get_code(record => 'pk')};
    my $use_pred = 0;
    unless ($#code > 0) {
        @code = @{$self->models->[0]->get_code(record => 'pred')};
        $use_pred = 1;
    }
    if ($#code <= 0) {
        croak("Neither PK or PRED defined in model");
    }
    $self->use_pred($use_pred);

    # Stop if all omegas are FIXed
    my $fix = $self->input_model_fix_omegas(get_or_set_fix(model => $model, type => 'omegas'));
    my $all_fix = 1;
    OUTER:
    for my $rows (@$fix) {
        for my $val (@$rows) {
            if (not $val) {
                $all_fix = 0;
                last OUTER;
            }
        }
    }
    if ($all_fix) {
        croak("All omegas of input model are fixed. Cannot continue.\n")
    }
    $self->rprofile(0);
}

sub get_phi_coltypes
{
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 },
    );
    my $model = $parm{'model'};

    my $is_classical = $model->problems->[0]->estimations->[-1]->is_classical;

    my $diagonal;
    my $offdiagonal;

    if ($is_classical) {
        $diagonal = 'ETA';
        $offdiagonal = 'ETC';
    } else {
        $diagonal = 'PHI';
        $offdiagonal = 'PHC';
    }
    return ($diagonal, $offdiagonal);

}

sub get_or_set_fix
{
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 },
        type  => { isa => 'Str', optional => 0 },
        set_array  => { isa => 'ArrayRef', optional => 1 },
        stop_record => { isa => 'Int', optional => 1 },
    );
    my $model = $parm{'model'};
    my $type = $parm{'type'};
    my $set_array = $parm{'set_array'};
    my $stop_record = $parm{'stop_record'};

    my @input_model_fix = ();
    unless (($type eq 'thetas') or ($type eq 'omegas') or ($type eq 'sigmas')){
        croak("unknown type $type");
    }
    if (defined $model->problems->[0]->$type){
        $stop_record = scalar(@{$model->problems->[0]->$type}) unless (defined $stop_record);
    }else{
        $stop_record = 0;
    }
    if ($type eq 'thetas'){
        my $index=0;
        for (my $i=0; $i<$stop_record ; $i++){
            for (my $j=0; $j< scalar(@{$model->problems->[0]->thetas->[$i]->options}); $j++){
                if (defined $set_array){
                    if (scalar(@{$set_array}) > $index){
                        $model->problems->[0]->thetas->[$i]->options->[$j]->fix($set_array->[$index]);
                    } #else assume did not exist in input model
                    $index++;
                }else{
                    push(@input_model_fix,$model->problems->[0]->thetas->[$i]->options->[$j]->fix);
                }
            }
        }
    }else{
        for (my $i=0; $i< $stop_record; $i++){
            push(@input_model_fix,[]) if (defined $set_array);
            if ($model->problems->[0]->$type->[$i]->is_block){
                if (defined $set_array){
                    if (scalar(@{$set_array}) > $i){
                        $model->problems->[0]->$type->[$i]->fix($set_array->[$i]->[0]) unless
                            $model->problems->[0]->$type->[$i]->same;
                    }
                }else{
                    push(@{$input_model_fix[$i]},$model->problems->[0]->$type->[$i]->fix); #ok SAME?
                }
            }else{#diagonal
                for (my $j=0; $j< scalar(@{$model->problems->[0]->$type->[$i]->options}); $j++){
                    if (defined $set_array){
                        if (scalar(@{$set_array}) > $i){
                            $model->problems->[0]->$type->[$i]->options->[$j]->fix($set_array->[$i]->[$j]);
                        }
                    }else{
                        push(@{$input_model_fix[$i]},$model->problems->[0]->$type->[$i]->options->[$j]->fix);
                    }
                }
            }
        }
    }
    return \@input_model_fix;
}

sub get_parcov_blocks
{
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 },
        skip_etas  => { isa => 'Int', optional => 0 },
        start_cov_eta => { isa => 'Int' },
    );
    my $model = $parm{'model'};
    my $skip_etas = $parm{'skip_etas'};
    my $start_cov_eta = $parm{'start_cov_eta'};

    my @omega_records;
    my $n_previous_rows = $skip_etas;

    my @labels;
    for (my $k = 0; $k < scalar(@{$model->problems->[0]->omegas}); $k++) {
        my $record = $model->problems->[0]->omegas->[$k];
        if ($record->same) {
            push @labels, ((undef) x $record->size);
        } else {
            foreach my $opt (@{$record->options}) {
                if ($opt->on_diagonal) {
                    if (defined $opt->label) {
                        push(@labels, $opt->label);
                    } else {
                        push(@labels, undef);
                    }
                }
            }
        }
    }
    splice @labels, 0, $skip_etas;

    my ($initblock, $message) = get_filled_omega_block(
        model => $model,
        problem_index => 0,
        start_etas => [ $skip_etas + 1 ],
        end_etas => [ $model->nomegas->[0] ],
        start_cov_eta => $start_cov_eta,
    );

    my $size = scalar(@{$initblock});
    if ($size == 0) {
        croak("size of initblock is 0, message is $message\n");
    }
    my $omega_lines = get_omega_lines(new_omega => $initblock, labels => \@labels);
    push(@omega_records, model::problem::omega->new(record_arr => $omega_lines, n_previous_rows => $n_previous_rows));

    return \@omega_records;
}

sub get_filled_omega_block
{
    #must have already done update inits on model so that get_matrix is estimated values, where available
    #This function can be simplified. Now start_etas and end_etas always have length 1
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 },
        problem_index  => { isa => 'Int', default => 0 },
        table_index  => { isa => 'Int', default => -1 },
        start_etas => { isa => 'ArrayRef', optional => 0 },
        end_etas => { isa => 'ArrayRef', optional => 0 },
        start_cov_eta => { isa => 'Int', optional => 0 },  # A bandaid before simplification of the function
    );
    my $model = $parm{'model'};
    my $problem_index = $parm{'problem_index'};
    my $table_index = $parm{'table_index'};
    my $start_etas = $parm{'start_etas'};
    my $end_etas = $parm{'end_etas'};
    my $start_cov_eta = $parm{'start_cov_eta'};

    if (scalar(@{$start_etas}) == 0) {
        croak("start_etas array must be larger than 0");
    }
    if (scalar(@{$start_etas}) != scalar(@{$end_etas})){
        croak("start_etas array must equal length to end_etas");
    }
    my $si = ($end_etas->[0] - $start_etas->[0] + 1);
    my $total_size = $si;

    my $start_eta_1 = $start_etas->[0];
    my $end_eta_1 = $end_etas->[0];
    my $start_eta_2;
    my $end_eta_2;
    my $end_eta_top = $end_eta_1;

    if (scalar(@{$start_etas}) > 1) {
        $start_eta_2 = $start_etas->[-1]; #last
        $end_eta_2  = $end_etas->[-1]; #last
        $end_eta_top = $end_etas->[-2]; #second to last, can be $end_eta_1. This usage assumes all consecutive
    }

    my $error = 0;

    my @sd;
    my @mergematrix;

    #local coords
    my ($corrmatrix, $message) = get_correlation_matrix_from_phi(
        start_eta_1 => $start_eta_1,
        end_eta_1 => $end_eta_top, #can be from multiple blocks here, or end_eta_1
        start_eta_2 => $start_eta_2,
        end_eta_2 => $end_eta_2,
        problem_index => $problem_index,
        table_index => $table_index,
        model => $model,
    );
    my $have_corrmatrix = 0;
    $have_corrmatrix = 1 if (length($message) == 0);

    @sd = (0) x ($total_size);
    for (my $i = 0; $i < ($total_size); $i++) {
        push(@mergematrix, [(0) x $total_size]);
    }

    #omega block. Do not assume all that are nonzero are estimated
    #get inits from model. local coords

    my $init_matrix = $model->problems->[$problem_index]->get_matrix(type => 'omega',
                                                                     start_row => $start_etas->[0],
                                                                     end_row => $end_etas->[0]);
    for (my $i = 0; $i < $si; $i++) {
        for (my $j = 0; $j < $si; $j++) {
            $mergematrix[$i]->[$j] = $init_matrix->[$i][$j];
        }
        $sd[$i] = sqrt($init_matrix->[$i][$i]) if ($init_matrix->[$i][$i] > 0);
    }

    # Now mergematrix contains the PAR and the COV blocks except for possible offdiagonals in the PAR block.

    #now we have sd and valuematrix that are inits/estimates or 0.
    #for each value in mergematrix that is still 0, compute covar using correlation and sd,
    #or set very small

    # Make symmetric
    for (my $i = 0; $i < $total_size; $i++) {
        for (my $j = 0; $j < $i; $j++) {
            $mergematrix[$j]->[$i] = $mergematrix[$i]->[$j];
        }
    }

    # Set PARCOV block to 0 so that it gets updated
    for (my $i = $start_cov_eta - $start_etas->[0] + 1; $i < $total_size; $i++) {
        for (my $j = 0; $j <= $start_cov_eta - $start_etas->[0]; $j++) {
            $mergematrix[$i]->[$j] = 0;
        }
    }

    for (my $i = 0; $i < $total_size; $i++) {
        for (my $j = 0; $j < $total_size; $j++) {
            next if ($mergematrix[$i]->[$j] != 0);

            if ((not $have_corrmatrix) or $corrmatrix->[$i][$j] == 0 or ($j < $start_cov_eta and $i < $start_cov_eta)) {
                $mergematrix[$i]->[$j] = $small_correlation * $sd[$i] * $sd[$j];
            } else {
                $mergematrix[$i]->[$j] = $corrmatrix->[$i][$j] * $sd[$i] * $sd[$j];
            }
            $mergematrix[$j]->[$i] = $mergematrix[$i]->[$j];
        }
    }

    my $newmatrix = replace_0_correlation(old_matrix => \@mergematrix,
                                          is_covariance => 1,
                                          low_correlation => $small_correlation);

    my $rounded = round_off_omega(omega => $newmatrix);
    #get posdef is necessary, pheno will crash without it
    my ($posdefmatrix, $count) = linear_algebra::get_symmetric_posdef(matrix => $rounded);

    return($posdefmatrix, '');
}

sub get_correlation_matrix_from_phi
{
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 },
        problem_index  => { isa => 'Int', default => 0 },
        table_index  => { isa => 'Int', default => -1 },
        start_eta_1 => { isa => 'Int', optional => 0 },
        end_eta_1 => { isa => 'Int', optional => 0 },
        start_eta_2 => { isa => 'Maybe[Int]', optional => 1 },
        end_eta_2 => { isa => 'Maybe[Int]', optional => 1 },
    );
    my $model = $parm{'model'};
    my $problem_index = $parm{'problem_index'};
    my $table_index = $parm{'table_index'};
    my $start_eta_1 = $parm{'start_eta_1'};
    my $end_eta_1 = $parm{'end_eta_1'};
    my $start_eta_2 = $parm{'start_eta_2'};
    my $end_eta_2 = $parm{'end_eta_2'};

    my $error = 0;
    my $message = '';

    $model->outputs->[0]->load;
    my $filename = $model->outputs->[0]->problems->[$problem_index]->full_name_NM7_file(file_type => 'phi');

    if (not length($filename) > 0) {
        $error = 2;
        $message .= 'Empty phi file name';
    }
    if (not -e $filename) {
        $error = 2;
        $message .= ' File '.$filename.' does not exist';
    }
    unless (($start_eta_1 > 0) and ($end_eta_1 >=$start_eta_1)) {
        $error = 2;
        $message .= " Input error start, end eta 1: $start_eta_1, $end_eta_1";
    }
    if (defined $start_eta_2 and defined $end_eta_2) {
        unless (($start_eta_2 > 0) and ($end_eta_2 >=$start_eta_2) and ($start_eta_2 > $end_eta_1)) {
            $error = 2;
            $message .= " Input error end_eta_1, start 2, end eta 2: $end_eta_1,$start_eta_2, $end_eta_2";
        }
    }
    return([], $message) unless ($error == 0);

    my $nmtablefile = nmtablefile->new(filename => $filename);
    my @matrix;
    my $covariance = [];
    my $sdcorr = [];
    my ($diagonal, $offdiagonal) = get_phi_coltypes(model => $model);

    for (my $i = $start_eta_1; $i <= $end_eta_1; $i++) {
        push(@matrix,$nmtablefile->tables->[$table_index]->get_column(name=> $diagonal.'('.$i.')'));
    }
    if (defined $start_eta_2 and defined $end_eta_2){
        for (my $i = $start_eta_2; $i <= $end_eta_2; $i++){
            push(@matrix,$nmtablefile->tables->[$table_index]->get_column(name=> $diagonal.'('.$i.')'));
        }
    }

    for (my $i=0; $i< scalar(@matrix); $i++){
        unless (array::any_nonzero($matrix[$i])){
            return([],"$diagonal column in phi-file only zeros");
        }
    }

    $error = linear_algebra::column_cov(\@matrix,$covariance);
    unless ($error == 0){
        if ($error == 1){
            $message = 'Numerical error column_cov';
        }else{
            $message = 'Input error column_cov';
        }
        return([],$message);
    }

    $error = linear_algebra::covar2sdcorr($covariance,$sdcorr);
    unless ($error == 0){
        print "cov\n";
        foreach my $line (@{$covariance}){
            print join("\t",@{$line})."\n";
        }
        if ($error == 1){
            $message = 'Numerical error covar2sdcorr';
        }else{
            $message = 'Input error covar2sdcorr';
        }
        return([],$message);
    }

    return ($sdcorr, '');
}

sub replace_0_correlation
{
    my %parm = validated_hash(\@_,
        old_matrix => { isa => 'ArrayRef', optional => 0 },
        low_correlation => { isa => 'Num', optional => 0 },
        is_covariance => { isa => 'Bool', optional => 0 },
    );
    my $old_matrix = $parm{'old_matrix'};
    my $low_correlation = $parm{'low_correlation'};
    my $is_covariance = $parm{'is_covariance'};

    my @new_matrix = ();
    my $size = scalar(@{$old_matrix});

    for (my $i=0; $i<($size); $i++){
        push(@new_matrix,[(0) x $size]);
    }

    for (my $row=0; $row< $size; $row++){
        for (my $col=0; $col<=$row; $col++){
            my $number = $old_matrix->[$row][$col];
            if (($number == 0) and (abs($low_correlation) > 0) and ($col < $row)){
                if ($is_covariance){
                    $number = $low_correlation *(sqrt($old_matrix->[$row][$row]))*(sqrt($old_matrix->[$col][$col])) ;
                }else{
                    $number = $low_correlation;
                }
            }
            $new_matrix[$row][$col] = $number;
            $new_matrix[$col][$row] = $number;
        }
    }

    return \@new_matrix;

}

sub round_off_omega
{
    my %parm = validated_hash(\@_,
        omega => { isa => 'ArrayRef', optional => 0 },
    );
    my $omega = $parm{'omega'};
    my $size = scalar(@{$omega});
    return [] if ($size < 1);
    my @new_lines=();

    my $form = '%.12G';
    for (my $row=0; $row< $size; $row++){
        push(@new_lines,[]);
        for (my $col=0; $col<$size; $col++){
            my $str= sprintf("$form",$omega->[$row][$col]);
            push(@{$new_lines[-1]},$str);
        }
    }
    return \@new_lines;
}

sub get_omega_lines
{
    my %parm = validated_hash(\@_,
        new_omega => { isa => 'ArrayRef', optional => 0 },
        labels => { isa => 'ArrayRef', optional => 0 },
    );
    my $new_omega = $parm{'new_omega'};
    my $labels = $parm{'labels'};

    #input is $new_omega as $new_omega->[$row][$col]
    #
    # add new BLOCK(size)

    my $size = scalar(@{$new_omega});
    return () if ($size < 1);
    my @record_lines;
    push(@record_lines, 'BLOCK(' . $size . ') ');
    my $form = '  %.12G';
    for (my $row = 0; $row < $size; $row++) {
        my $line = '';
        for (my $col = 0; $col <= $row; $col++) {
            my $str = sprintf("$form", $new_omega->[$row][$col]);
            $line = "$line $str";
        }
        my $comment = '';
        $comment = '; ' . $labels->[$row] if (defined $labels and scalar(@{$labels}) > $row and (defined $labels->[$row]));
        push(@record_lines, $line . $comment);
    }
    return \@record_lines;
}

sub set_model2_omega_blocks
{
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 },
        rescale => {isa => 'Bool', optional => 0},
        covariate_covmatrix => {isa => 'ArrayRef', optional => 0},
        covariate_labels => {isa => 'ArrayRef', optional => 0},
    );
    my $model = $parm{'model'};
    my $rescale = $parm{'rescale'};
    my $covariate_covmatrix = $parm{'covariate_covmatrix'};
    my $covariate_labels = $parm{'covariate_labels'};

    my $covariate_size = scalar(@{$covariate_covmatrix});
    croak("too few labels") if (scalar(@{$covariate_labels}) != $covariate_size);

    my @covariate_etanumbers;
    my @covariate_code;

    my $n_previous_rows =  $model->problems()->[0]->nomegas(with_correlations => 0, with_same => 1);

    for (my $i = 0; $i < scalar(@{$model->problems->[0]->omegas}); $i++) {
        if ($model->problems->[0]->omegas->[$i]->is_block) {
            $model->problems->[0]->omegas->[$i]->fix(1) unless ($model->problems->[0]->omegas->[$i]->same);
        } else {
            for (my $j = 0; $j < scalar(@{$model->problems->[0]->omegas->[$i]->options}); $j++) {
                $model->problems->[0]->omegas->[$i]->options->[$j]->fix(1);
            }
        }
    }

    my $matrix;
    if ($rescale) {
        my $sdcorr = [];
        my $err = linear_algebra::covar2sdcorr($covariate_covmatrix, $sdcorr);
        for (my $row = 0; $row < scalar(@{$sdcorr}); $row++) {
            $sdcorr->[$row][$row] = 1;
        }

        $matrix = replace_0_correlation(
            old_matrix => $sdcorr,
            is_covariance => 0,
            low_correlation => $small_correlation
        );
    } else {
        $matrix = replace_0_correlation(
            old_matrix => $covariate_covmatrix,
            is_covariance => 1,
            low_correlation => $small_correlation
        );
    }

    my $rounded = round_off_omega(omega => $matrix);
    my ($posdefmatrix, $count) = linear_algebra::get_symmetric_posdef(matrix => $rounded);
    if ($count > 0) {
        ui->print(category => 'frem',
            message => "\nWarning: The covariate covariance matrix has $count ".
            "(essentially) non-positive eigenvalue(s). Modified Model 2 covariate \$OMEGA block to make ".
            "is positive definite.");
    }
    my $omega_lines = get_omega_lines(new_omega => $posdefmatrix, labels => $covariate_labels);
    push(@{$model->problems->[0]->omegas}, model::problem::omega->new(record_arr => $omega_lines,
            n_previous_rows => $n_previous_rows));
}

sub get_covmatrix
{
    my %parm = validated_hash(\@_,
        output => { isa => 'output', optional => 0 },
    );
    my $output = $parm{'output'};

    my ($error, $message) = check_covstep(output => $output);
    return [] if $error;
    ui->print(category => 'frem', message => $message) if (length($message) > 0);
    my $lower_covar = $output->get_single_value(attribute => 'covariance_matrix');

    if (not defined $lower_covar) {
        croak("Trying get_covmatrix but the covariance matrix is undefined. Parsing error?\n");
    }

    my $covar = output::problem::subproblem::make_square($lower_covar);

    return $covar;
}

sub check_covstep
{
    my %parm = validated_hash(\@_,
        output => { isa => 'output', optional => 0 },
    );

    my $output = $parm{'output'};

    my $message= '';

    unless ($output->have_output){
        $message = "output object is empty, cannot contintue constructing proposal density, output file\n".
            $output->full_name."\n";
        return(1,$message);
    }
    unless( $output -> parsed_successfully ){
        $message = "unable to read everything from outputfile, cannot continue constructing proposal density,".
            " parser error message:\n".$output -> parsing_error_message();
        return(1,$message);
    }
    unless ($output-> get_single_value(attribute => 'covariance_step_run')){
        $message = "cannot continue constructing proposal density, the covariance step was not run";
        return(1,$message);
    }
    unless ($output-> get_single_value(attribute => 'covariance_step_successful')){
        $message = "cannot continue constructing proposal density,the covariance step was not successful";
        return(1,$message);
    }
    if ($output-> get_single_value(attribute => 'covariance_step_warnings')){
        $message  = "there were covariance step warnings in the lst-file. This would give ".
            " errors in proposal density";
        return(1,$message);
    }
    return (0,$message);
}

sub join_covmats
{
    my %parm = validated_hash(\@_,
        partial_covmats => { isa => 'ArrayRef', optional => 0 },
        partial_strings => { isa => 'ArrayRef', optional => 0 },
        full_strings => { isa => 'ArrayRef', optional => 0 },
        variance_guess_hash => { isa => 'HashRef', optional => 0 },
        rse_guess_hash => { isa => 'HashRef', optional => 0 },
    );

    my $partial_covmats = $parm{'partial_covmats'};
    my $partial_strings = $parm{'partial_strings'};
    my $full_strings = $parm{'full_strings'};
    my $variance_guess_hash = $parm{'variance_guess_hash'};
    my $rse_guess_hash = $parm{'rse_guess_hash'};

    my $dimension = scalar(@{$full_strings});
    my @full_covmat = ();
    my $verbose=0;
    for (my $i=0; $i< $dimension; $i++){
        push(@full_covmat,[(0) x $dimension]);
    }

    for (my $k=0; $k<scalar(@{$partial_strings}); $k++){
        next unless (scalar(@{$partial_covmats->[$k]}) > 0); #if covstep unsuccessful
        my $smalldim = scalar(@{$partial_strings->[$k]});
        my @mapping = ();
        for (my $i=0; $i< $smalldim; $i++){
            for (my $j=0; $j< $dimension; $j++){
                if ($partial_strings->[$k]->[$i] eq $full_strings->[$j]){
                    push(@mapping,$j);
                    last;
                }
            }
        }
        unless (scalar(@mapping) == $smalldim){
            croak("error finding mapping $k dimension $smalldim");
        }
        for (my $i=0; $i< $smalldim; $i++){
            for (my $j=0; $j< $smalldim; $j++){
                $full_covmat[($mapping[$i])]->[(($mapping[$j]))] = $partial_covmats->[$k]->[$i]->[$j];
            }
        }
    }
    for (my $i=0; $i< $dimension; $i++){
        if ($full_covmat[$i]->[$i] == 0){
            if (defined $variance_guess_hash->{$full_strings->[$i]}){
                $full_covmat[$i]->[$i] = $variance_guess_hash->{$full_strings->[$i]};
            }else{
                ui->print(category => 'all',
                      message => 'problem creating proposal density: no guess for variance of '.$full_strings->[$i].
                      ', using rse-based guess '.$rse_guess_hash->{$full_strings->[$i]}) if $verbose;
                $full_covmat[$i]->[$i] = $rse_guess_hash->{$full_strings->[$i]};
            }
        }
    }

    return \@full_covmat;
}

sub print_proposal_density
{
    my %parm = validated_hash(\@_,
        partial_outputs => { isa => 'ArrayRef', optional => 0 },
        omega_orders => { isa => 'ArrayRef', optional => 0 },
        full_model => { isa => 'model', optional => 0 },
        reordered_model1 => { isa => 'model', optional => 0 },
        directory => { isa => 'Str', optional => 0 },
        filename => { isa => 'Str', optional => 0 },
        rse => { isa => 'Num', optional => 0 },
        etas_mapping => { isa => 'HashRef' },
    );
    my $partial_outputs = $parm{'partial_outputs'};
    my $omega_orders = $parm{'omega_orders'};
    my $full_model = $parm{'full_model'};
    my $reordered_model1 = $parm{'reordered_model1'};
    my $directory = $parm{'directory'};
    my $filename = $parm{'filename'};
    my $rse = $parm{'rse'};
    my $etas_mapping = $parm{'etas_mapping'};

    $partial_outputs->[0]->load;
    $partial_outputs->[1]->load;

    my $full_strings = $full_model->problems->[0]->get_estimated_attributes(parameter => 'all',
                                                                            attribute => 'coordinate_strings');
    my $covmat1 = get_covmatrix(output => $partial_outputs->[0]);

    my $strings1 = $reordered_model1->problems->[0]->get_estimated_attributes(
        parameter => 'all', attribute => 'coordinate_strings');   #after possible reordering

    my $covmat2 = get_covmatrix(output => $partial_outputs->[1]);
    my $strings2 = $partial_outputs->[1]->problems->[0]->input_problem->
        get_estimated_attributes(parameter => 'all',
                                 attribute => 'coordinate_strings');

    my $full_values = $full_model->outputs->[0]->get_filtered_values(category => 'estimate',
                                                                     parameter => 'all',
                                                                     problem_index => 0,
                                                                     subproblem_index => 0);
    unless (defined $full_values and scalar(@{$full_values}) > 0 and defined $full_values->[0]) {
        $full_values = $full_model->problems->[0]->get_estimated_attributes(parameter => 'all',
                                                                            attribute => 'inits');
    }

    my $perfect_ids_hash = perfect_individuals(output1 => $partial_outputs->[0],
                                               output2 => $partial_outputs->[1]);

    my $variance_hash = get_variance_guesses(values => $full_values,
                                          strings => $full_strings,
                                          is_omega => 1,
                                          perfect_individuals => $perfect_ids_hash);

    my $rse_hash = get_rse_guesses(output => $full_model->outputs->[0],
                                   rse => $rse);

    my $fullmat = join_covmats(full_strings => $full_strings,
                               variance_guess_hash => $variance_hash,
                               rse_guess_hash => $rse_hash,
                               partial_strings =>[$strings1,$strings2],
                               partial_covmats => [$covmat1,$covmat2]);

    my ($posdefmatrix, $count) = linear_algebra::get_symmetric_posdef(matrix => $fullmat);

    my $formatted = tool::format_covmatrix(
        matrix => $posdefmatrix,
        header => $full_strings,
        comma => 0,
        print_labels => 1
    );
    open (RES, ">" . $directory . $filename);
    foreach my $line (@{$formatted}) {
        print RES $line;
    }
    close(RES);
}

sub get_rse_guesses
{
    my %parm = validated_hash(\@_,
        output => { isa => 'output', optional => 0 },
        rse => { isa => 'Num', optional => 0 },
    );

    my $output = $parm{'output'};
    my $rse = $parm{'rse'};

    my $parameter_hash = output::get_nonmem_parameters(output => $output);

    my $full_values = $output->get_filtered_values(category => 'estimate',
                                                   parameter => 'all',
                                                   problem_index => 0,
                                                   subproblem_index => 0);
    unless (defined $full_values and scalar(@{$full_values})>0 and (defined $full_values->[0])){
        $full_values = $output->problems->[0]->input_problem->get_estimated_attributes(parameter => 'all',
                                                                                       attribute => 'inits');
        for (my $i=0; $i< scalar(@{$full_values}); $i++){
            $parameter_hash->{'values'}->[$i] = $full_values->[$i];
        }
    }


    my $variances = tool::sir::setup_variancevec_from_rse(rse_theta => $rse,
                                                          rse_omega => $rse,
                                                          rse_sigma => $rse,
                                                          parameter_hash => $parameter_hash);

    my %hash;
    for (my $i=0; $i< scalar(@{$parameter_hash->{'coordinate_strings'}}); $i++){
        $hash{$parameter_hash->{'coordinate_strings'}->[$i]} = $variances->[$i];
    }
    return \%hash;
}

sub get_variance_guesses
{
    my %parm = validated_hash(\@_,
        values => { isa => 'ArrayRef', optional => 0 },
        strings => { isa => 'ArrayRef', optional => 0 },
        is_omega => { isa => 'Bool', optional => 0 },
        perfect_individuals => { isa => 'HashRef', optional => 0 },
    );

    my $values = $parm{'values'};
    my $strings = $parm{'strings'};
    my $is_omega = $parm{'is_omega'};
    my $perfect_individuals = $parm{'perfect_individuals'};

    my $type = 'OMEGA';
    $type = 'SIGMA' unless $is_omega;

    my %valueshash = ();
    for (my $i=0; $i< scalar(@{$strings}); $i++){
        $valueshash{$strings->[$i]} = $values->[$i];
    }

    my %variancehash = ();
    for (my $i=0; $i< scalar(@{$strings}); $i++){
        next unless ($strings->[$i] =~ /^$type/);
        if ($strings->[$i] =~ /\((\d+),(\d+)\)/ ){
            my $x= $1;
            my $y= $2;
            my $N=0;
            if (defined $perfect_individuals->{$x} and defined( $perfect_individuals->{$y})){
                #take minimum
                if ($perfect_individuals->{$x} < $perfect_individuals->{$y}){
                    $N = $perfect_individuals->{$x};
                }else{
                    $N = $perfect_individuals->{$y};
                }
            }elsif (defined $perfect_individuals->{$x}){
                $N = $perfect_individuals->{$x};
            }elsif (defined $perfect_individuals->{$y}){
                $N = $perfect_individuals->{$y};
            }
            if ($N> 0){
                $variancehash{$strings->[$i]} =
                    (($valueshash{$strings->[$i]})**2 + $valueshash{"$type($x,$x)"}*$valueshash{"$type($y,$y)"})/$N;
            }
        }else{
            croak('error regexp '.$strings->[$i]);
        }
    }
    return \%variancehash;
}

sub perfect_individuals
{
    my %parm = validated_hash(\@_,
        output1 => { isa => 'output', optional => 0 },
        output2 => { isa => 'output', optional => 0 },
    );
    my $output1 = $parm{'output1'};
    my $output2 = $parm{'output2'};

    my ($error, $message) = check_covstep(output => $output1);

    ($error, $message) = check_covstep(output => $output2);
    my %hash;

    my $is_output1 = 1;
    foreach my $output ($output1, $output2) {
        my $hashref = $output->perfect_individual_count();    #can be empty
        foreach my $key (keys %{$hashref}) {
            my $etanum = $key;
            if (defined $hash{$etanum}) {
                croak("perfect count for ETA $etanum already read from model 1, this is a coding error");
            }
            $hash{$etanum} = $hashref->{$key};
        }
        $is_output1 = 0;
    }
    return \%hash;
}

sub prepare_results
{
    my $self = shift;
    my $directory = $self->directory;
    my $pdcov = '';
    if ($self->force_posdef_covmatrix) {
        $pdcov = ' --force_posdef_covmatrix';
    }
    my $pdsamp = ' --force_posdef_samples=' . $self->force_posdef_samples;
    my $meth = '';
    if ($self->bipp) {
        $meth = ' --method=bipp'
    }
    PsN::call_pharmpy("results frem $directory$pdcov$pdsamp$meth");      # Generate results.json and results.csv
    return '';
}

sub do_model1
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         model => { isa => 'Ref', optional => 0 }
    );
    my $model = $parm{'model'};

    my $model1_name = 'model_1.mod';
    my $model_path = File::Spec->catfile($self->_intermediate_models_path, $model1_name);
    my $frem_model;

    if (not -f $model_path) {      # No rerun in the same directory
        if (not $model->is_run()) {
            my $orig_fit = tool::modelfit->new(
                %{common_options::restore_options(@common_options::tool_options)},
                base_directory => $self->base_directory,
                directory => File::Spec->catfile($self->directory, "model1_modelfit"),
                models => [ $model ],
                parent_tool_id => $self->tool_id(),
                logfile => undef,
                raw_results => undef,
                prepared_models => undef,
                top_tool => 0,
                copy_up => 1,
            );
            $orig_fit->add_to_nmoutput(extensions => ['phi', 'ext', 'cov']);
            ui->print(category => 'frem', message => 'Fitting the base model.');
            $orig_fit->run;
            $self->metadata->{'copied_files'} = $orig_fit->metadata->{'copied_files'};
        }

        # copy base model lst, cov and ext to intermediate models
        my $model_dir = $model->directory;
        my $model_filename = $model->filename;
        my @extra_extensions = ('lst', 'cov', 'ext', 'phi');
        foreach my $ext (@extra_extensions) {
            my $extra_file_orig = utils::file::replace_extension($model_filename, $ext);
            my $extra_file_copy = utils::file::replace_extension($model1_name, $ext);
            my $extra_file_orig_path = File::Spec->catfile($model_dir, $extra_file_orig);
            if (-f $extra_file_orig_path) {
                cp($extra_file_orig_path, File::Spec->catfile($self->_intermediate_models_path, $extra_file_copy));
            }
        }

        $frem_model = $model->copy(filename => $model_path,
                                   output_same_directory => 1,
                                   write_copy => 1,
                                   copy_datafile => 0,
                                   copy_output => 0);
    } else {
        $frem_model = model->new(
            %{common_options::restore_options(@common_options::model_options)},
            filename => $model_path,
            parse_output => 1,
        );
    }

    if ($frem_model->is_option_set(record => 'estimation', name => 'LIKELIHOOD', record_number => -1, fuzzy_match => 1)) {
        $self->_likelihood(1);
        $frem_model->remove_option(record_name => 'estimation', option_name => 'LIKELIHOOD', fuzzy_match => 1);
    } elsif ($frem_model->is_option_set(record => 'estimation', name => '-2LOGLIKELIHOOD', record_number => -1, fuzzy_match => 1) or
             $frem_model->is_option_set(record => 'estimation', name => '-2LLIKELIHOOD', record_number => -1, fuzzy_match => 1)) {
        $self->_loglikelihood(1);
        $frem_model->remove_option(record_name => 'estimation', option_name => '-2LOGLIKELIHOOD', fuzzy_match => 1);
        $frem_model->remove_option(record_name => 'estimation', option_name => '-2LLIKELIHOOD', fuzzy_match => 1);
    }

    if ($model->defined_variable(name => "F_FLAG")) {
        $self->_have_fflag(1);
    }

    my $output = $frem_model->outputs->[0];

    if (not defined $output) {
        croak("No output from Model 1, cannot proceed with frem");
    }

    $frem_model->update_inits(from_output => $output);

    # Do reordering of OMEGAS and create a reordered version of model_1 called model_1b
    my $reorder_mapping = $self->reorder_etas_mapping($frem_model);
    $self->etas_reorder_mapping($reorder_mapping);
    my $new_phi_path = File::Spec->catfile($self->_intermediate_models_path, 'model_1b.phi');
    model_transformations::reorder_etas(model => $frem_model, order => $reorder_mapping, phi_file => $new_phi_path, reorder_output => 1); 
    model_transformations::split_omegas(model => $frem_model, split_after => scalar(@{$self->skip_omegas}));
    $frem_model->filename('model_1b.mod');
    $frem_model->_write();

    my $reordering = 0;
    while (my ($key, $value) = each %$reorder_mapping) {
        if ($key != $value) {
            $reordering = 1;
        }
    }
    if ($reordering) {
        print("Warning: Frem did automatical reordering of some ETAs and OMEGAs.\n    The reordered model is in m1/model_1b.mod.\n    Although PsN presents its results using the original ordering, all NONMEM output will be in reordered form.\n");
        # Print initial estimates of original model reordered for Pharmpy to read
        open my $fh, '>', 'm1/model_1.inits';
        my $dummy = $model->copy(filename => $model_path,
                                 output_same_directory => 1,
                                 write_copy => 0,
                                 copy_datafile => 0,
                                 copy_output => 0);

        model_transformations::reorder_etas(model => $dummy, order => $reorder_mapping); 
        model_transformations::split_omegas(model => $dummy, split_after => scalar(@{$self->skip_omegas}));
        my @omegacoords = @{$dummy->indexes(parameter_type => 'omega', problem_numbers => [1])};
        my @omegainits = @{$dummy->initial_values(parameter_type => 'omega', problem_numbers  => [1])};
        my %coordsinits;
        for (my $i=0;$i<scalar(@{$omegainits[0]});$i++){
            $coordsinits{$omegacoords[0]->[$i]} = $omegainits[0]->[$i];
        }
        use Data::Dumper;
        print $fh Dumper(\%coordsinits);
        close $fh;
    }

    return ($frem_model, $output, $new_phi_path);
}

sub get_regular_covariates
{
    my %parm = validated_hash(\@_,
        categorical => { isa => 'ArrayRef', optional => 0 },
        log => { isa => 'ArrayRef', optional => 0 },
        covariates => { isa => 'ArrayRef', optional => 0 },
    );
    my $categorical = $parm{'categorical'};
    my $log = $parm{'log'};
    my $covariates = $parm{'covariates'};

    my @regular = ();
    my @special = @{$log};
    push(@special,@{$categorical});

    foreach my $cov (@{$covariates}){
        my $matched = 0;
        foreach my $new (@special){
            if ($new eq $cov){
                $matched = 1;
                last;
            }
        }
        push(@regular,$cov) unless ($matched);
    }
    return \@regular;
}

sub get_indices
{
    my %parm = validated_hash(\@_,
        target => { isa => 'ArrayRef', optional => 0 },
        keys => { isa => 'ArrayRef', optional => 0 },
    );
    my $target = $parm{'target'};
    my $keys = $parm{'keys'};

    my %indices;
    foreach my $col (@{$keys}){
        my $pos = array::get_positions(target => $target,
                                       keys=> [$col]);
        $indices{$col}=$pos->[0];
    }
    return \%indices;
}

sub do_filter_dataset_and_append_binary
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model => { isa => 'Ref', optional => 0 },
    );
    my $model = $parm{'model'};

    my $filtered_datafile = 'filtered_plus_type0.dta';

    my ($filtered_data_model, $data_set_headers, $extra_input_items, $message) = create_data2_model(
        model => $model,
        filename => File::Spec->catfile($self->_intermediate_models_path, 'filter_data_model.mod'),
        filtered_datafile => $filtered_datafile,
        use_pred => $self->use_pred,
        dv => $self->dv,
        covariates => $self->covariates,
    );

    $self->extra_input_items($extra_input_items);

    if (not -e File::Spec->catfile($self->_intermediate_models_path, $filtered_datafile)) {
        $filtered_data_model->_write();
        my $rundir = File::Spec->catfile($self->directory, 'create_fremdata_dir');
        rmtree([ "$rundir" ]) if (-e $rundir);
        my $filter_fit = tool::modelfit->new(
            %{$self->tool_options},
            base_directory => $self->directory,
            directory => $rundir,
            models => [$filtered_data_model],
            top_tool => 0,
            copy_data => 1,
            clean => 2
        );
        ui->print(category => 'all', message  => $message, newline => 1);
        $filter_fit->run;
    }

    my $filtered_data = data->new(
        filename => File::Spec->catfile($filtered_data_model->directory, $filtered_datafile),
        ignoresign => '@',
        idcolumn => $model->idcolumns->[0],
        missing_data_token => $self->missing_data_token
    );

    my $indices = get_indices(target => $data_set_headers,
        keys => ['EVID', 'MDV', $fremtype, $self->dv, 'L2']);

    my @cov_indices;
    my @is_log;
    my @cov_names;

    if (scalar(@{$self->log}) > 0) {
        #we assume all found already, error check in createdata2model
        my $log_indices = array::get_positions(target => $data_set_headers, keys => $self->log);

        my @new_log;
        foreach my $cov (@{$self->log}) {
            push(@new_log, "LN$cov");
        }
        $self->log(\@new_log);
        push(@cov_indices, @{$log_indices});
        push(@cov_names, @new_log);
        push(@is_log, (1) x scalar(@new_log));
    }

    if (scalar(@{$self->regular}) > 0) {
        my $regular_indices = array::get_positions(target => $data_set_headers, keys => $self->regular);
        push(@cov_indices, @{$regular_indices});
        push(@cov_names, @{$self->regular});
        push(@is_log, (0) x scalar(@{$self->regular}));
    }

    if (scalar(@{$self->categorical}) > 0) {
        my $categorical_indices = array::get_positions(target => $data_set_headers, keys => $self->categorical);
        my @mdv_evid_indices;
        push(@mdv_evid_indices,$indices->{'MDV'}) if (defined $indices->{'MDV'});
        push(@mdv_evid_indices,$indices->{'EVID'}) if (defined $indices->{'EVID'});
        my ($mapping, $new_indices, $new_categorical, $warn_multiple) =
            $filtered_data->append_binary_columns(indices => $categorical_indices,
                                                  baseline_only => 1,
                                                  mdv_evid_indices => \@mdv_evid_indices,
                                                  start_header => $data_set_headers);
        for my $col (@$new_categorical) {
            if (not array::string_in($col, $self->categorical)) {
                push @{$self->extra_input_items}, $col;
            }
        }
        if (scalar(@{$warn_multiple}) > 0) {
            ui->print(category => 'all',
                      message => "\nWarning: Individuals were found to have multiple values in the " . join(' ', @{$warn_multiple}) .
                      " column(s),".
                      " but the frem script will just use the first value for the individual.\n");
        }

        $categorical_indices = $new_indices;
        $self->categorical($new_categorical); #these are now binary
        push(@cov_indices, @{$categorical_indices});
        push(@cov_names, @{$new_categorical});
        push(@is_log, (0) x scalar(@{$new_categorical}));
    }

    $self->covariates(\@cov_names);

    $indices->{'cov_indices'} = \@cov_indices;
    $indices->{'is_log'} = \@is_log;

    return ($filtered_data, $indices);
}

sub do_frem_dataset
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model => { isa => 'Ref', optional => 0 },
        filtered_data => { isa => 'data', optional => 0 },
        indices => { isa => 'HashRef', optional => 0 },
        mod1_ofv => { isa => 'Num', optional => 0 },
        N_parameter_blocks => {isa => 'Int', optional => 0},
        fremdataname => { isa => 'Str', optional => 0 },
    );
    my $model = $parm{'model'};
    my $mod1_ofv = $parm{'mod1_ofv'};
    my $indices = $parm{'indices'};
    my $filtered_data = $parm{'filtered_data'};
    my $N_parameter_blocks = $parm{'N_parameter_blocks'};
    my $fremdataname = $parm{'fremdataname'};

    my $do_check = $self->check;
    my $fremdata_path = File::Spec->catfile($self->directory, $fremdataname);
    if (-e $fremdata_path) {
        unlink($fremdata_path);
        $do_check = 0; #assume get same result second time
    }

    #this writes dataset to disk
    my $resultref = data::frem_compute_covariate_properties(
        filtered_data => $filtered_data,
        invariant_covariates => $self->covariates,
        N_parameter_blocks => $N_parameter_blocks,
        is_log => $indices->{'is_log'},
        occ_index => undef,
        directory => $self->directory,
        data2name => $fremdataname,
        evid_index => $indices->{'EVID'},
        mdv_index => $indices->{'MDV'},
        dv_index => $indices->{$self->dv},
        type_index => $indices->{$fremtype},
        cov_indices => $indices->{'cov_indices'},
        l2_index => $indices->{'L2'}
    );

    if ($do_check) {
        my $name_check_model = 'check_data.mod';
        my $data_check_model = $model->copy(
            filename => File::Spec->catfile($self->_intermediate_models_path, $name_check_model),
            output_same_directory => 1,
            write_copy => 0,
            copy_datafile => 0,
            copy_output => 0
        );

        # have filtered data so can skip old accept/ignores. Need ignore=@ since have a header
        #have only one $PROB by input check
        $data_check_model->datafiles(problem_numbers => [ 1 ], new_names => [ $fremdata_path ]);
        $data_check_model->problems->[0]->datas->[0]->ignoresign('@');
        $data_check_model->remove_option(record_name => 'data', option_name => 'ACCEPT', fuzzy_match => 1);
        $data_check_model->set_option(
            record_name => 'data',
            option_name => 'IGNORE',
            option_value => "($fremtype.GT.0)",
            fuzzy_match => 1
        );
        $data_check_model->problems->[0]->psn_record_order(1);

        foreach my $input (@{$data_check_model->problems->[0]->inputs}) {
            $input->remove_drop_column_names;
        }

        foreach my $item (@{$self->extra_input_items()}) {
            $data_check_model->add_option(problem_numbers => [1],
                                          record_name => 'input',
                                          option_name => $item);
        }
        $data_check_model->_write(overwrite => 1);

        my $rundir = $self->directory . '/datacheck_modelfit_dir1';
        rmtree([ "$rundir" ]) if (-e $rundir);
        my $run = tool::modelfit->new(%{$self->tool_options},
                                      base_directory => $self->directory,
                                      directory => $rundir,
                                      copy_data => 0,
                                      models => [$data_check_model],
                                      top_tool => 0);

        $run->add_to_nmoutput(extensions => ['ext']);
        ui->print(category => 'all', message => 'Running data check model');
        $run->run;
        #compare ofv. print this to log file
        my $check_ofv = 'undefined';
        if ($data_check_model->is_run()) {
            $check_ofv = $data_check_model->outputs->[0]->get_single_value(attribute => 'ofv');
        }
        print "\nModel 1 ofv is    $mod1_ofv\n";
        print   "Data check ofv is $check_ofv\n";
    }

    return $resultref;
}

sub get_pred_error_pk_code
{
    my %parm = validated_hash(\@_,
        covariates => { isa => 'ArrayRef', optional => 0 },
        maxeta => {isa => 'Int', optional => 0},
        rescale => { isa => 'Bool', optional => 0 },
        mu => { isa => 'Bool', optional => 0 },
        use_pred => { isa => 'Bool', optional => 0 },
        invariant_covmatrix => { isa => 'ArrayRef', optional => 0 },
        invariant_mean => { isa => 'ArrayRef', optional => 0 },
        estimate_mean => { isa => 'ArrayRef', optional => 0 },
        ntheta => {isa => 'Int', optional => 0},
        N_parameter_blocks => {isa => 'Int', optional => 0},
        epsnum => {isa => 'Int', optional => 0},
        indent => {isa => 'Str', optional => 0},
        likelihood => { isa => 'Bool', default => 0},
        loglikelihood => { isa => 'Bool', default => 0},
        have_fflag  => { isa => 'Bool', default => 0},
    );
    my $covariates = $parm{'covariates'};
    my $maxeta = $parm{'maxeta'};
    my $rescale = $parm{'rescale'};
    my $mu = $parm{'mu'};
    my $use_pred = $parm{'use_pred'};
    my $invariant_covmatrix = $parm{'invariant_covmatrix'};
    my $invariant_mean = $parm{'invariant_mean'};
    my $estimate_mean = $parm{'estimate_mean'};
    my $ntheta = $parm{'ntheta'};
    my $N_parameter_blocks = $parm{'N_parameter_blocks'};
    my $epsnum = $parm{'epsnum'};
    my $indent = $parm{'indent'};
    my $likelihood = $parm{'likelihood'};
    my $loglikelihood = $parm{'loglikelihood'};
    my $have_fflag = $parm{'have_fflag'};

    my @pkcode;
    my @pred_error_code = (';;;FREM CODE BEGIN COMPACT',';;;DO NOT MODIFY');
    if ($likelihood or $loglikelihood) {
        push @pred_error_code, 'IF (FREMTYPE.NE.0) THEN';
        push @pred_error_code, '    F_FLAG=0';
        if (not $have_fflag) {
            push @pred_error_code, 'ELSE';
            if ($likelihood) {
                push @pred_error_code, '    F_FLAG=1';
            } else {
                push @pred_error_code, '    F_FLAG=2';
            }
        }
        push @pred_error_code, "END IF";
    }

    my @eta_labels;
    my @eta_strings;
    my @rescale_strings;
    my @rescale_factors;

    for (my $j = 0; $j < scalar(@{$covariates}); $j++) {
        my $label = 'BSV_' . $covariates->[$j];
        my $etanum = ($maxeta + 1 + $j);
        my $sd = '1';
        if ($rescale) {
            $sd = sprintf("%.12G",sqrt($invariant_covmatrix->[$j][$j]));
        }
        push(@rescale_factors, $sd);
        push(@rescale_strings, "SDC$etanum");
        push(@eta_strings, [ "ETA($etanum)" ]);
        push(@eta_labels, $label);
    }

    my $newtheta = 0;

    my @theta_record_strings;
    my @theta_strings;
    for (my $i = 0; $i < scalar(@{$covariates}); $i++) {
        my $thetalabel = 'TV_' . $covariates->[$i];
        my $val=$invariant_mean->[$i];
        my $fixed = '';
        if ($estimate_mean->[$i]) {
            $val = 0.001 if ($val == 0);
        }else{
            # #can be 0 since FIXed
            $fixed = ' FIX';
        }
        push(@theta_record_strings, ' ' . sprintf("%.12G", $val) . "$fixed ; $thetalabel");
        $newtheta++;
        my $num = ($ntheta + $newtheta);
        push(@theta_strings, "THETA($num)");
    }

    my @code;
    if ($rescale) {
        my @rescalecode;
        for (my $j = 0; $j < scalar(@{$covariates}); $j++) {
            push(@rescalecode, $indent . $rescale_strings[$j] . ' = ' . $rescale_factors[$j]);
        }
        push(@code, @rescalecode);
    }
    if ($mu) {
        #PK/PRED changes for mu modelling
        my @mucode;
        for (my $j = 0; $j < scalar(@{$covariates}); $j++) {
            my $etanum = ($maxeta + 1 + $j);
            if ($rescale) {
                push(@mucode, $indent.'MU_'.$etanum.' = '.$theta_strings[$j].'/SDC'.$etanum);
                push(@mucode, $indent.'COV'.$etanum.' = (MU_'.$etanum.' + '.$eta_strings[$j]->[0].')*SDC'.$etanum);
            } else {
                push(@mucode,$indent.'MU_'.$etanum.' = '.$theta_strings[$j]);
                push(@mucode,$indent.'COV'.$etanum.' = MU_'.$etanum.' + '.$eta_strings[$j]->[0]);
            }
        }
        push(@code,@mucode);
    }
    if ($use_pred) {
        push(@pred_error_code,@code);
    } else {
        push(@pkcode,@code);
    }

    for (my $i=0; $i< scalar(@{$covariates}); $i++){
        for (my $j=0; $j< $N_parameter_blocks; $j++){
            my $comment = ';'.$indent.'  '.$covariates->[$i].'  '.$rescale_factors[$i];
            my $ipred;
            if ($mu){ #no iov handled
                $ipred = 'COV'.($maxeta+1+$i);
            }else{
                my $rescale_expr = $rescale ? '*'.$rescale_strings[$i] : '';
                $ipred = $theta_strings[$i].' + '.$eta_strings[$i]->[$j].$rescale_expr;
            }
            if ($N_parameter_blocks > 1){
                $comment .= ' occasion '.($j+1);
            }
            my $num = 100*($i+1)+$j;
            push(@pred_error_code,$indent.'IF ('.$fremtype.'.EQ.'.$num.') THEN' );
            push(@pred_error_code,$comment);
            push(@pred_error_code,$indent.'   Y = '.$ipred.' + EPS('.$epsnum.')' );
            push(@pred_error_code,$indent.'   IPRED = '.$ipred );
            push(@pred_error_code,$indent.'END IF' );
        }
    }
    push(@pred_error_code,';;;FREM CODE END COMPACT' );

    return (\@eta_labels, \@theta_record_strings, \@pred_error_code, \@pkcode);
}

sub prepare_model2
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 },
        skip_etas => {isa => 'Int', optional => 0},
        fremdataname => { isa => 'Str', optional => 0 },
        invariant_mean => { isa => 'ArrayRef', optional => 0 },
        invariant_covmatrix => { isa => 'ArrayRef', optional => 0 },
        update_existing_model_files => { isa => 'Bool', optional => 0 },
        etas_file => { isa => 'Maybe[Str]', optional => 0 }
    );
    my $model = $parm{'model'};
    my $fremdataname = $parm{'fremdataname'};
    my $skip_etas = $parm{'skip_etas'};
    my $invariant_mean = $parm{'invariant_mean'};
    my $invariant_covmatrix = $parm{'invariant_covmatrix'};
    my $update_existing_model_files = $parm{'update_existing_model_files'};
    my $etas_file = $parm{'etas_file'};

    my $name_model = 'model_2.mod';

    my $frem_model;
    my $maxeta = $model->nomegas->[0];

    my $ntheta = $model->nthetas(problem_number => 1);
    my $epsnum = 1 + $model->problems->[0]->nsigmas(with_correlations => 0, with_same => 1);

    my @estimate_mean;
    if ($self->estimate_means) {
        if (scalar(@{$self->has_missingness}) == scalar(@{$self->covariates})) {
            @estimate_mean = @{$self->has_missingness};
        } else {
            croak("No information about missing covariate values, this is a bug");
        }
    } else {
        @estimate_mean = (0) x scalar(@{$self->covariates});
    }

    my ($etalabels, $theta_strings, $pred_error_code, $pk_code) = get_pred_error_pk_code(
        covariates => $self->covariates,
        maxeta => $maxeta,
        rescale => $self->rescale,
        mu => $self->mu,
        use_pred => $self->use_pred,
        invariant_covmatrix => $invariant_covmatrix,
        invariant_mean => $invariant_mean,
        estimate_mean => \@estimate_mean,
        ntheta => $ntheta,
        N_parameter_blocks => 1,
        epsnum => $epsnum,
        indent => '    ',
        likelihood => $self->_likelihood,
        loglikelihood => $self->_loglikelihood,
        have_fflag => $self->_have_fflag,
    );

    cleanup_outdated_model(modelname => File::Spec->catfile($self->_intermediate_models_path, $name_model),
                           need_update => $update_existing_model_files);

    # do estimation record changes even if this is a restart, to save records pre-set_maxeval_zero (for model 3 generation)
    $frem_model = $model->copy(
        filename => File::Spec->catfile($self->_intermediate_models_path, $name_model),
        output_same_directory => 1,
        write_copy => 0,
        copy_datafile => 0,
        copy_output => 0
    );
    if ($frem_model->problems->[0]->estimations->[-1]->is_classical) {
            if ((($PsN::nm_major_version == 7) and ($PsN::nm_minor_version > 2)) or ($PsN::nm_major_version > 7)){
                    $frem_model->problems->[0]->estimations->[-1]->remove_option(name => 'NONINFETA', fuzzy_match => 1);
                    $frem_model->problems->[0]->estimations->[-1]->_add_option(option_string => 'NONINFETA=1');
            }
    }
    if ($self->mceta > 0) {
            #input checking that mceta ok NM version and est method
            $frem_model->problems->[0]->estimations->[-1]->remove_option(name => 'MCETA', fuzzy_match => 1);
            $frem_model->problems->[0]->estimations->[-1]->_add_option(option_string => 'MCETA=' . $self->mceta);
    }
    my $est_records = $frem_model->problems->[0]->estimations;

    my $im_dir = $self->_intermediate_models_path;
    if (not -e File::Spec->catfile($self->_intermediate_models_path, $name_model)) {
        # if $ETAS FILE= used, M2 needs modified file with new omegas (initialized to 0)
        if ($etas_file) {

            # load etas file and get number of new ETAs
            my $phi = phitable->new(path => $etas_file);
            my $num_new_etas = scalar(@{$self->covariates});

            # add new ETAs and construct new filename and path
            $phi->add_zero_etas(num_etas => $num_new_etas);
            ($etas_file = $name_model) =~ s/(.*)\..*/$1_input.phi/;
            my (undef, $etas_filename) = OSspecific::absolute_path($im_dir, $etas_file);

            # update FILE in model to new path (just filename since same directory) and write file to disk
            my $phi_path = File::Spec->catfile($im_dir, $etas_filename);
            $phi->write(path => $phi_path);
            $frem_model->init_etas(phi_name => $phi_path);

            # update etas file to model 2 output (for downstream model 3 usage)
            (my $phi_filename = $name_model) =~ s/(.*)\..*/$1.phi/;
            $etas_file = File::Spec->catfile($im_dir, $phi_filename);
        }

        #DATA changes
        #we want to clear all old options from DATA
        $frem_model->problems->[0]->datas->[0]->options([]);
        $frem_model->problems->[0]->datas->[0]->ignoresign('@');
        $frem_model->datafiles(problem_numbers => [1], new_names => [$self->directory . $fremdataname]);

        #INPUT changes
        #remove names of DROP items, in case have special meaning like DATE=DROP
        foreach my $input (@{$frem_model->problems->[0]->inputs}) {
            $input->remove_drop_column_names;
        }

        foreach my $item (@{$self->extra_input_items}) {
            #mdv, fremtype and binarized covariates
            $frem_model->add_option(problem_numbers => [1], record_name => 'input', option_name => $item);
        }

        #SIGMA changes
        if (defined $frem_model->problems->[0]->sigmas) {
            foreach my $record (@{$frem_model->problems->[0]->sigmas}) {
                if ($record->is_block) {
                    $record->fix(1) unless ($record->same);
                } else {
                    for (my $j = 0; $j < scalar(@{$record->options}); $j++) {
                        $record->options->[$j]->fix(1);
                    }
                }
            }
        }

        $frem_model->add_records(type => 'sigma',
                                 problem_numbers => [1],
                                 record_strings => ['0.0000001 FIX ; EPSCOV']);

        set_model2_omega_blocks(model => $frem_model,
                                rescale => $self->rescale,
                                covariate_covmatrix => $invariant_covmatrix,
                                covariate_labels => $etalabels);

        #THETA changes
        #FIX all existing
        if (defined $frem_model->problems->[0]->thetas) {
            for (my $i = 0; $i < scalar(@{$frem_model->problems->[0]->thetas}); $i++) {
                for (my $j = 0; $j < scalar(@{$frem_model->problems->[0]->thetas->[$i]->options}); $j++) {
                    $frem_model->problems->[0]->thetas->[$i]->options->[$j]->fix(1);
                }
            }
        }

        $frem_model->add_records(type => 'theta',
                                 problem_numbers => [1],
                                 record_strings => $theta_strings);


        add_pred_error_code(model=>$frem_model,
                            pred_error_code => $pred_error_code,
                            pk_code => $pk_code,
                            mu => $self->mu,
                            use_pred => $self->use_pred);

        unless (defined $frem_model->problems->[0]->covariances and
                scalar(@{$frem_model->problems->[0]->covariances})>0) {
            $frem_model->problems->[0]->add_records(record_strings => ['PRINT=R UNCONDITIONAL'],
                                                    type => 'covariance');
        }

        my $totaletas = $frem_model->problems->[0]->nomegas(with_correlations => 0, with_same => 1);
        if ($totaletas > $self->deriv2_nocommon_maxeta) {
            if (defined $frem_model->problems->[0]->abbreviateds and scalar(@{$frem_model->problems()->[0]->abbreviateds})>0) {
                unless ($frem_model->problems->[0]->is_option_set(name => 'DERIV2',
                                                                  record => 'abbreviated',
                                                                  fuzzy_match => 1)) {
                    $frem_model->set_option(option_name => 'DERIV2',
                                            record_name => 'abbreviated',
                                            option_value => 'NOCOMMON',
                                            problem_numbers => [1],
                                            fuzzy_match => 1);
                }
            } else {
                $frem_model->problems->[0]->set_records(record_strings => ['DERIV2=NOCOMMON'],
                                                        type => 'abbreviated' );
            }
        }

        my $message = $frem_model->check_and_set_sizes('all' => 1);
        if (length($message) > 0) {
            ui->print(category => 'all', message =>  $message.' However this NONMEM version does not support $SIZES. '.
                      'There may be NMtran errors when running the model');
        }

        if (not $self->estimate_covariates) {
            $frem_model->set_maxeval_zero(print_warning => 1,
                                          last_est_complete => $self->last_est_complete,
                                          niter_eonly => $self->niter_eonly,
                                          need_ofv => 0);
        }

        $frem_model->_write();
    } else {
        if (defined $etas_file) {
            (my $phi_filename = $name_model) =~ s/(.*)\..*/$1.phi/;
            $etas_file = File::Spec->catfile($im_dir, $phi_filename);
        }
    }

    return ($est_records, $ntheta, $epsnum, $etas_file);
}

sub prepare_model3
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 },
        parcov_blocks => { isa => 'ArrayRef', optional => 0 },
        update_existing_model_files => { isa => 'Bool', optional => 0 },
        est_records => { isa => 'ArrayRef', optional => 0 },
        etas_file => { isa => 'Maybe[Str]', optional => 0 }
    );
    my $model = $parm{'model'};
    my $parcov_blocks = $parm{'parcov_blocks'};
    my $update_existing_model_files = $parm{'update_existing_model_files'};
    my $est_records = $parm{'est_records'};
    my $etas_file = $parm{'etas_file'};

    my $name_model = 'model_3.mod';
    my $frem_model;
    my $covrecordref = [];
    if (defined $model->problems->[0]->covariances and scalar(@{$model->problems->[0]->covariances}) > 0) {
        $covrecordref = $model->problems->[0]->covariances->[0]->_format_record();
        for (my $i = 0; $i < scalar(@{$covrecordref}); $i++) {
            $covrecordref->[$i] =~ s/^\s*\$CO[A-Z]*\s*//; #get rid of $COVARIANCE
            $covrecordref->[$i] =~ s/\s*$//; #get rid of newlines
        }
    }

    cleanup_outdated_model(modelname => File::Spec->catfile($self->_intermediate_models_path, $name_model),
                           need_update => $update_existing_model_files);

    my $im_dir = $self->_intermediate_models_path;
    if (not -e File::Spec->catfile($im_dir, $name_model)) {
        # input model  inits have already been updated
        $frem_model = $model->copy(
            filename => File::Spec->catfile($self->_intermediate_models_path, $name_model),
            output_same_directory => 1,
            write_copy => 0,
            copy_datafile => 0,
            copy_output => 0
        );

        # if $ETAS FILE= used, M3 needs M2 phi output
        if ($etas_file) {
            if (not -f $etas_file) {
                croak "\$ETAS file $etas_file could not be read for model 3, this is a bug";
            }

            # copy M2 output to M3 input phi file
            (my $etas_filename = $name_model) =~ s/(.*)\..*/$1_input.phi/;
            cp($etas_file, File::Spec->catfile($im_dir, $etas_filename));
            (undef, $etas_filename) = OSspecific::absolute_path($im_dir, $etas_filename);

            # update FILE in model to new path (just filename since same directory)
            $frem_model->get_or_set_etas_file(problem_number => 1, new_file => $etas_filename);

            # update etas file to model 3 output (for downstream model 4 usage)
            (my $phi_filename = $name_model) =~ s/(.*)\..*/$1.phi/;
            $etas_file = File::Spec->catfile($im_dir, $phi_filename);
        }

        my @omega_records;
        for my $record (@{$frem_model->problems->[0]->omegas}) {
            if ($record->n_previous_rows + 1 <= scalar(@{$self->skip_omegas})) {
                push @omega_records, $record;
            }
        }

        for (my $i = 0; $i < scalar(@{$parcov_blocks}); $i++) {
            push(@omega_records, $parcov_blocks->[$i]);
        }

        $frem_model->problems->[0]->omegas(\@omega_records);
        $frem_model->set_maxeval_zero(
            print_warning => 1,
            last_est_complete => $self->last_est_complete,
            niter_eonly => $self->niter_eonly,
            need_ofv => 0
        );

        $frem_model->problems->[0]->remove_records(type => 'covariance');

        $frem_model->_write();
        $self->model_3($frem_model);
    } else {
        if (defined $etas_file) {
            (my $phi_filename = $name_model) =~ s/(.*)\..*/$1.phi/;
            $etas_file = File::Spec->catfile($im_dir, $phi_filename);
        }
    }

    return ($est_records, $covrecordref, $etas_file);
}

sub prepare_model4
{
    # Prepare the final frem model
    # Using model3b as a starting point.
    # * Taking initial estimates and phi from one of model2, model3 and model3b depending on which one had the best fit
    # * Change to full estimation
    # * Add $COV
    my $self = shift;
    my %parm = validated_hash(\@_,
        model2 => { isa => 'model', optional => 0 },
        model3 => { isa => 'model', optional => 0 },
        model3b => { isa => 'model', optional => 0 },
        est_records => { isa => 'ArrayRef', optional => 0},
        cov_records => { isa => 'ArrayRef', optional => 0},
        update_existing_model_files => { isa => 'Bool', optional => 0 },
    );
    my $model2 = $parm{'model2'};
    my $model3 = $parm{'model3'};
    my $model3b = $parm{'model3b'};
    my $est_records = $parm{'est_records'};
    my $cov_records = $parm{'cov_records'};
    my $update_existing_model_files = $parm{'update_existing_model_files'};

    my $name_model = 'model_4.mod';
    my $frem_model;

    my $fin_dir = $self->directory . 'final_models/';
    cleanup_outdated_model(modelname => $fin_dir . $name_model,
                           need_update => $update_existing_model_files);

    return if (-e $fin_dir . $name_model);

    $frem_model = $model3b->copy(
        filename => $fin_dir . $name_model,
        output_same_directory => 1,
        write_copy => 0,
        copy_datafile => 0,
        copy_output => 0
    );

    my $output2 = $model2->outputs->[0];
    my $mod2_ofv = $output2->get_single_value(attribute => 'ofv');
    my $output3 = $model3->outputs->[0];
    my $mod3_ofv = $output3->get_single_value(attribute => 'ofv');
    my $output3b = $model3b->outputs->[0];
    my $mod3b_ofv = $output3b->get_single_value(attribute => 'ofv');

    my @ofvs = ($mod2_ofv, $mod3_ofv, $mod3b_ofv);
    @ofvs = grep defined, @ofvs;
    @ofvs = sort { $a <=> $b } @ofvs;
    my $best_ofv = $ofvs[0];

    if ($best_ofv == $mod2_ofv) {
        print "Starting model4 from model2\n";
        $frem_model->update_inits(from_output => $output2);
        my $block = $frem_model->problems->[0]->omegas->[-1];
        for my $option (@{$block->options}) {
            if (not $option->on_diagonal and $option->init == 0) {
                $option->init(0.000001);
            }
        }
        cp(File::Spec->catfile($self->_intermediate_models_path, 'model_2.phi'), File::Spec->catfile($self->_final_models_path, 'model_4_input.phi'));
    } elsif ($best_ofv == $mod3_ofv) {
        print "Starting model4 from model3\n";
        $frem_model->update_inits(from_output => $output3);
        cp(File::Spec->catfile($self->_intermediate_models_path, 'model_3.phi'), File::Spec->catfile($self->_final_models_path, 'model_4_input.phi'));
    } else {
        print "Starting model4 from model3b\n";
        cp(File::Spec->catfile($self->_intermediate_models_path, 'model_3b.phi'), File::Spec->catfile($self->_final_models_path, 'model_4_input.phi'));
    }

    $frem_model->get_or_set_etas_file(problem_number => 1, new_file => 'model_4_input.phi');

    if (not $self->bipp) {
        my $new_cov_records;
        my $uncond;
        foreach my $opt (@{$cov_records}) {
            if ($opt =~ /^OMIT/) {
                $logger->info("Removed OMIT option from \$COV in $name_model\n");
            } else {
                push @{$new_cov_records}, $opt;
            }
            $uncond = 1 if ($opt =~ /^UNC/);
        }
        if (not $uncond) {
            push @{$new_cov_records}, "UNCONDITIONAL";
        }
        push @{$new_cov_records}, "PRECOND=1";
        $frem_model->problems->[0]->add_records(
            record_strings => $new_cov_records,
            type => 'covariance'
        );
    }

    $frem_model->problems->[0]->estimations($est_records);
    if ($self->derivatives) {
        model_approximations::derivatives_model(model => $frem_model);   # Output the derivatives to be able to make VA-plot
    }
    $frem_model->_write();
}

sub prepare_model5
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        start_omega_record => { isa => 'Int', optional => 0 },
        first_cholesky_theta => { isa => 'Int', optional => 0 },
        parameter_etanumbers => { isa => 'ArrayRef', optional => 0 },
        update_existing_model_files => { isa => 'Bool', optional => 0 },
    );
    my $start_omega_record = $parm{'start_omega_record'};
    my $first_cholesky_theta = $parm{'first_cholesky_theta'};
    my $parameter_etanumbers = $parm{'parameter_etanumbers'};
    my $update_existing_model_files = $parm{'update_existing_model_files'};

    my $modnum = 5;

    my $name_model = "model_$modnum.mod";
    my $frem_model;

    my $model_path = File::Spec->catfile($self->_intermediate_models_path, $name_model);

    cleanup_outdated_model(modelname => $model_path, need_update => $update_existing_model_files);

    if (not -e $model_path) {
        #read model 4 from disk, then copy it
        my $model = model->new(%{common_options::restore_options(@common_options::model_options)},
                               parse_output => 0,
                               filename => 'final_models/model_4.mod',
                               ignore_missing_output_files => 1);

        $frem_model = $model->copy(filename => $model_path,
                                   output_same_directory => 1,
                                   write_copy => 0,
                                   copy_datafile => 0,
                                   copy_output => 0);

        #SIGMA fix all existing
        if (defined $frem_model->problems->[0]->sigmas) {
            foreach my $record (@{$frem_model->problems->[0]->sigmas}) {
                if ($record->is_block) {
                    $record->fix(1) if (not $record->same);
                } else {
                    for (my $j = 0; $j < scalar(@{$record->options}); $j++) {
                        $record->options->[$j]->fix(1);
                    }
                }
            }
        }

        #OMEGA fix all before $start_omega
        for (my $i = 0; $i < ($start_omega_record - 1); $i++) {
            my $record = $frem_model->problems->[0]->omegas->[$i];
            if ($record->is_block) {
                $record->fix(1) if (not $record->same);
            } else {
                for (my $j = 0; $j < scalar(@{$record->options}); $j++) {
                    $record->options->[$j]->fix(1);
                }
            }
        }

        #THETA changes
        #FIX all existing
        if (defined $frem_model->problems->[0]->thetas) {
            for (my $i = 0; $i < scalar(@{$frem_model->problems->[0]->thetas}); $i++) {
                for (my $j = 0; $j < scalar(@{$frem_model->problems->[0]->thetas->[$i]->options}); $j++) {
                    $frem_model->problems->[0]->thetas->[$i]->options->[$j]->fix(1);
                }
            }
        }
        my $dimension = $frem_model->problems->[0]->omegas->[$start_omega_record-1]->size;
        my $top_size = $dimension - scalar(@{$self->covariates});
        #do cholesky
        my $warnings = $frem_model->problems->[0]->cholesky_reparameterize(
                what => 'o' . $start_omega_record,
                bounded_theta => 0,
                correlation_cutoff => 0,
                correlation_limit => 0.9, #if higher then warn
            );
        #correlation cutoff $smallnum would automatically gives 0 FIX for correlations not in input model, but
        #might give some extra.
        #Fix all parameter-parameter and covariate-covariate correlations, and all SD

        my @last_zero_col;
        my $cumulative = 0;
        for (my $i = 0; $i < scalar(@{$parameter_etanumbers}); $i++) {
            my $new_size = scalar(@{$parameter_etanumbers->[$i]});
            push(@last_zero_col, ( ($cumulative) x $new_size ));
            $cumulative += $new_size;
        }

        my $thetaindex = $first_cholesky_theta;
        my $row = 1;

        while ($row <= $dimension) {
            #do the row
            #first comes SD, always fix
            $frem_model->problems->[0]->thetas->[$thetaindex]->options->[0]->fix(1);
            $thetaindex++;
            #then the correlations left to right
            for (my $col = 1; $col < $row; $col++) {
                #if an inserted parameter-parameter correlation
                if (($row <= $top_size) and ($col <= $last_zero_col[($row-1)])) {
                    $frem_model->problems->[0]->thetas->[$thetaindex]->options->[0]->clear_upbnd;
                    $frem_model->problems->[0]->thetas->[$thetaindex]->options->[0]->clear_lobnd;
                    $frem_model->problems->[0]->thetas->[$thetaindex]->options->[0]->init('0');
                }
                if (($row <= $top_size) or ($col > $top_size)) { #not a paramater-covariate correlation
                    $frem_model->problems->[0]->thetas->[$thetaindex]->options->[0]->fix(1);
                }
                $thetaindex++;
            }
            $row++;
        }

        my $message = $frem_model->check_and_set_sizes(LTH => 1); #set LTH if too many thetas.
        if (length($message) > 0) {
            ui->print(category => 'all', message =>  $message . ' However this NONMEM version does not support $SIZES. ' .
                      'There may be NMtran errors when running the model');
        }
        $frem_model->_write();
    }
}

sub prepare_model6
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 },
        start_omega_record => { isa => 'Int', optional => 0 },
        first_cholesky_theta => { isa => 'Int', optional => 0 },
        parameter_etanumbers => { isa => 'ArrayRef', optional => 0 },
        update_existing_model_files => { isa => 'Bool', optional => 0 },
    );
    my $model = $parm{'model'};
    my $start_omega_record = $parm{'start_omega_record'};
    my $first_cholesky_theta = $parm{'first_cholesky_theta'};
    my $parameter_etanumbers = $parm{'parameter_etanumbers'};
    my $update_existing_model_files = $parm{'update_existing_model_files'};

    my $modnum=6;
    my $name_model = 'model_'.$modnum.'.mod';
    my $frem_model;

    cleanup_outdated_model(modelname => $self -> directory().'final_models/'.$name_model,
                           need_update => $update_existing_model_files);

    unless (-e $self -> directory().'final_models/'.$name_model){
        $frem_model = $model ->  copy( filename    => $self -> directory().'final_models/'.$name_model,
                                       output_same_directory => 1,
                                       write_copy => 0,
                                       copy_datafile   => 0,
                                       copy_output => 0);

        #add values for covariate thetas that are not covered by
        #input model fix thetas.
        my @array = @{$self->input_model_fix_thetas};
        push(@array,((1) x scalar(@{$self->covariates})));

        get_or_set_fix(model => $frem_model,
                       type => 'thetas',
                       set_array => \@array);
        get_or_set_fix(model => $frem_model,
                       type => 'sigmas',
                       set_array => $self->input_model_fix_sigmas);

        get_or_set_fix(model => $frem_model,
                       type => 'omegas',
                       set_array => $self->input_model_fix_omegas);

        my $dimension = $frem_model->problems->[0]->omegas->[$start_omega_record-1]->size;
        my $top_size = $dimension - scalar(@{$self->covariates});

        #unfix everything that is not exactly zero
        my $thetaindex = $first_cholesky_theta;
        my $row = 1;
        while ($row <= $dimension){
            #do the row
            #first comes SD, always unfix
            $frem_model->problems->[0]->thetas->[$thetaindex]->options->[0]->fix(0);
            $thetaindex++;
            #then the correlations left to right
            for (my $col=1; $col< $row; $col++){
                unless ($frem_model->problems->[0]->thetas->[$thetaindex]->options->[0]->init == 0){
                    $frem_model->problems->[0]->thetas->[$thetaindex]->options->[0]->fix(0);
                }
                $thetaindex++;
            }
            $row++;
        }
        $frem_model->_write();
    }
}

sub prepare_model7
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 },
        update_existing_model_files => { isa => 'Bool', optional => 0 },
    );
    my $model = $parm{'model'};
    my $update_existing_model_files = $parm{'update_existing_model_files'};

    my $modnum = 7;

    my $name_model = 'model_7.mod';
    my $frem_model;

    cleanup_outdated_model(modelname => $self -> directory().'final_models/'.$name_model,
                           need_update => $update_existing_model_files);

    if (not -e $self->directory . 'final_models/' . $name_model) {
        $frem_model = $model->copy(
            filename => $self->directory . 'final_models/' . $name_model,
            output_same_directory => 1,
            write_copy => 0,
            copy_datafile => 0,
            copy_output => 0
        );

        $frem_model->set_maxeval_zero(print_warning => 0,
                                      last_est_complete => $self->last_est_complete,
                                      niter_eonly => $self->niter_eonly,
                                      need_ofv => 1);

        $frem_model->problems->[0]->remove_records(type => 'covariance');
        $frem_model->_write();
    }
}

sub cleanup_outdated_model
{
    my %parm = validated_hash(\@_,
        modelname => { isa => 'Str', optional => 0 },
        need_update => { isa => 'Bool', optional => 0 },
    );
    my $modelname = $parm{'modelname'};
    my $need_update = $parm{'need_update'};

    if ($need_update){
        #we have run a model earlier in the sequence
        if (-e $modelname){
            ui -> print( category => 'all', message =>  "Removing existing $modelname and output ".
                         "because it needs updating after rerun of preceeding model.");
            unlink($modelname);
            my $base = $modelname;
            $base =~ s/(\.[^.]+)$// ;
            foreach my $extension (@PsN::nm7_extensions,'.lst','.out','.res'){
                unlink ($base.$extension) if (-e $base.$extension);
            }
        }
    }
}

sub run_unless_run
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        numbers => { isa => 'ArrayRef', optional => 0 },
        final => { isa => 'Bool', default => 0 },
        subdirectory => {isa => 'Str', default => 'm1'}
    );
    my $numbers = $parm{'numbers'};
    my $final = $parm{'final'};
    my $subdirectory = $parm{'subdirectory'};

    croak("no numbers to run") unless (scalar(@{$numbers})>0 and defined $numbers->[0]);

    my @models = ();
    my $do_run = 0;
    my $update_existing = 0;

    for (my $i=0; $i<scalar(@{$numbers}); $i++){
        #reread from disk so that omegas are properly stored
        my $name_model = 'model_'.$numbers->[$i].'.mod';
        push(@models,model->new( %{common_options::restore_options(@common_options::model_options)},
                                 filename                    => $subdirectory.'/'.$name_model,
                                 parse_output => 0,
                                 ignore_missing_output_files => 1 ));
        unless ($models[$i]->is_run){
            $do_run = 1;
        }
    }
    if ($do_run){
        my $rundir = $self -> directory().'model'.join('_',@{$numbers}).'_modelfit_dir1';
        if (-e $rundir){
            $logger->warning("Removing old $rundir before rerun of model ".
                             join(' and ',@{$numbers}));
            rmtree([ "$rundir" ]);
            if (-e $rundir){
                croak("failed to remove $rundir");
            }
        }
        my $run = tool::modelfit ->new(%{$self->tool_options},
                                        base_directory     => $self -> directory(),
                                        directory         => $rundir,
                                        copy_data     => 0,
                                        models         => \@models,
                                        top_tool              => 0);
        $run->add_to_nmoutput(extensions => ['phi','ext','cov']);

        # output message
        my @msg;
        for (my $i=0; $i< scalar(@models); $i++){
            my $msg = $numbers->[$i];
            my $eval_only = 1;
            foreach my $est (@{$models[$i]->problems->[0]->estimations}) {
                if (!defined $est->is_eval_only) {
                    $eval_only = undef;
                    last;
                } elsif (!$est->is_eval_only) {
                    $eval_only = 0;
                    last;
                }
            }
            if (!defined $eval_only) {
                $msg = "Running model ".$msg;
            } elsif ($eval_only) {
                $msg = "Evaluating model ".$msg;
            } else {
                $msg = "Estimating model ".$msg;
            }
            push @msg, $msg;
        }
        my $text = join(' and ', @msg);
        ui->print(category => 'all', message => $text);

        $run-> run;
        $update_existing = 1; #any later models in sequence need to be recreated
    }

    my $message;
    if ($final){
        return (\@models,$message); #final estimation
    }else{
        if (defined $models[0]->outputs and (defined $models[0]->outputs->[0])){
            $models[0]->outputs->[0]->load;
            my $from_coordval = $models[0]->outputs->[0]-> thetacoordval( subproblems => [1] );
            if (defined $from_coordval->[0]->[0] and scalar(keys %{$from_coordval->[0]->[0]})>0){
                $models[0]->update_inits(from_output=> $models[0]->outputs->[0]) ;
            }else{
                $message = "No parameter estimates from Model ".$numbers->[0].", cannot proceed with frem";
            }
        }else{
            $message = "No output from Model ".$numbers->[0].", cannot proceed with frem";
        }
        return ($models[0],$message,$update_existing);
    }
}

sub save_covresults
{
    my $self = shift;
    my $resultref = shift;
    my @covnames = @{$self->covariates};

    # open file for storing covariate summary (for post-processing usage)
    my $filename = $self->directory.'covariates_summary.csv';
    $self->cov_summary($filename);
    my $header = join ',', ('', @covnames);
    unless (open(FILE, '>', $filename)) {
        $logger->critical("cannot open $filename: $!"); die;
    }
    print FILE "summary_statistics\n";
    print FILE "$header\n";

    if (defined $resultref) {
        if (defined $resultref->{'has_missingness'}) {
            my $missing = $resultref->{'has_missingness'};
            $self->has_missingness($missing);
            print FILE join(',', ("has_missingness", @{$missing}))."\n";
        }
        if (defined $resultref->{'occasionlist'}) {
            $self->occasionlist($resultref->{'occasionlist'});
        }
        my ($covmat, @variance, @stdev);
        if (defined $resultref->{'invariant_covmatrix'}) {
            $covmat = $resultref->{'invariant_covmatrix'};
            $self->invariant_covmatrix($covmat);
            for (my $i=0; $i<scalar(@covnames); $i++) {
                push @variance, $covmat->[$i]->[$i];
                push @stdev, sqrt($covmat->[$i]->[$i]);
            }
        } else {
            $logger->critical("Cannot save covresults (invariant covmat undef), this is a bug"); die;
        }
        if (defined $resultref->{'invariant_mean'} and defined $resultref->{'invariant_median'}) {
            my $mean = $resultref->{'invariant_mean'};
            my $median = $resultref->{'invariant_median'};
            for (my $i=0; $i<scalar(@covnames); $i++) {
                my $amean = abs($resultref->{'invariant_mean'}->[$i]);
                if ($amean < 0.01) {
                    $logger->warning("Warning: abs(mean) for " . $self->covariates->[$i] . " is $amean," .
                                     "the additive error may not be appropriate for this covariate");
                }
            }
            $self->invariant_mean($mean);
            $self->invariant_median($median);
            print FILE join(',', ("invariant_mean", @{$mean}))."\n";
            print FILE join(',', ("invariant_median", @{$median}))."\n";
            print FILE join(',', ("invariant_variance", @variance))."\n";
            print FILE join(',', ("invariant_stdev", @stdev))."\n";
        } else {
            $logger->critical("Cannot save covresults (invariant mean undef), this is a bug"); die;
        }
        if (defined $resultref->{'timevar_median'}) {
            $self->timevar_median($resultref->{'timevar_median'});
        }
        if (defined $resultref->{'timevar_covmatrix'}) {
            $self->timevar_covmatrix($resultref->{'timevar_covmatrix'});
        }

        print FILE "\ninvariant_covmatrix\n";
        print FILE join(',', ("", @covnames))."\n";
        for (my $i=0; $i<scalar(@covnames); $i++) {
            print FILE join(',', ($covnames[$i], @{$covmat->[$i]}))."\n";
        }
        close(FILE);
    } else {
        $logger->critical("Cannot save covresults (resultref undef), this is a bug"); die;
    }
}

sub modelfit_setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    my $model = $self->models->[$model_number - 1];
    my ($mod1_ofv, $output_model2_fullname);
    my ($skip_etas, $fix_omegas);
    my ($covariate_etanumbers, $ntheta, $epsnum, $maxeta);
    my ($est_records, $cov_records);
    my $finaldir= $self->directory . 'final_models';
    my ($mes,$sir_model, $sir_model_text);
    my $proposal_filename = 'proposal_density.cov';
    my $update_existing_model_files = 0;
    my $etas_file; # $ETAS FILE= if present (continuously updated; new files created downstream)

    my $inter = File::Spec->catfile($self->directory, 'm1');
    $self->_intermediate_models_path($inter);

    my $final_path = File::Spec->catfile($self->directory, 'final_models');
    $self->_final_models_path($final_path);

    #this runs input model, if necessary, and updates inits
    (my $frem_model1, my $output_model1, $etas_file) = $self->do_model1(model => $model);

    $self->input_model_fix_thetas(get_or_set_fix(model => $frem_model1, type => 'thetas'));
    $self->input_model_fix_omegas(get_or_set_fix(model => $frem_model1, type => 'omegas'));
    $self->input_model_fix_sigmas(get_or_set_fix(model => $frem_model1, type => 'sigmas'));

    $self->model_1($frem_model1);
    $mod1_ofv = $output_model1->get_single_value(attribute => 'ofv');

    my ($filtered_data, $indices) = $self->do_filter_dataset_and_append_binary(model => $frem_model1);

    my $frem_datasetname = 'frem_dataset.dta';
    my $covresultref = $self->do_frem_dataset(
        model => $model, #must be input model here, not updated with final ests
        N_parameter_blocks => 1,
        filtered_data => $filtered_data,
        indices => $indices,
        mod1_ofv => $mod1_ofv,
        fremdataname => $frem_datasetname
    );

    $self->save_covresults($covresultref);

    ($est_records, $ntheta, $epsnum, $etas_file) = $self->prepare_model2(
        model => $frem_model1,
        fremdataname => $frem_datasetname,
        skip_etas => $self->skip_etas,
        invariant_mean => $self->invariant_mean,
        invariant_covmatrix => $self->invariant_covmatrix,
        update_existing_model_files => $update_existing_model_files,
        etas_file => $etas_file
    );

    my ($frem_model2, $message, $need_update) = $self->run_unless_run(numbers => [2]);
    if (defined $message and length($message) > 0) {
        ui->print(category => 'frem', message => $message);
        die;
    }

    $update_existing_model_files = 1 if ($need_update);

    my $mod3_parcov_block = get_parcov_blocks(
        model => $frem_model2,
        skip_etas => scalar(@{$self->skip_omegas}),
        start_cov_eta => $model->nomegas->[0] - scalar(@{$self->skip_omegas}),
    );

    $self->model_2($frem_model2); # used by prepare_results

    ($est_records, $cov_records, $etas_file) = $self->prepare_model3(
        model => $frem_model2,
        parcov_blocks => $mod3_parcov_block,
        update_existing_model_files => $update_existing_model_files,
        est_records => $est_records,
        etas_file => $etas_file
    );

    $message = undef;

    (my $frem_model3, $message, $need_update) = $self->run_unless_run(numbers => [3]);
    if (defined $message and length($message) > 0) {
        ui->print(category => 'frem', message => $message);
        die;
    }
    $update_existing_model_files = 1 if ($need_update);

    mkdir($finaldir) unless (-d $finaldir);

    my $frem_dir = $self->directory;
    my $ncov = scalar(@{$self->covariates});
    my $code = "from pharmpy.tools.frem.tool import update_model3b_for_psn; update_model3b_for_psn(" . PsN::path_literal($frem_dir) . ", $ncov)";
    PsN::call_pharmpy_wrapper($code);

    (my $frem_model3b, $message, $need_update) = $self->run_unless_run(numbers => ['3b']);

    $self->prepare_model4(
        model2 => $frem_model2,
        model3 => $frem_model3,
        model3b => $frem_model3b,
        est_records => $est_records,
        cov_records => $cov_records,
        update_existing_model_files => $need_update,
    );

    push(@{$self->final_numbers}, 4);

    if ($self->cholesky) {
        my $parameter_etanumbers = [ [ (scalar(@{$self->skip_omegas}) + 1) .. $model->nomegas->[0]  ] ];
        print "Warning: The cholesky option has not been tested after the automatic reordering was implemented. It will most probably not work\n";
        $self->prepare_model5(
            start_omega_record => $self->start_omega_record,
            first_cholesky_theta => scalar(@{$frem_model3->problems->[0]->thetas}),
            parameter_etanumbers => $parameter_etanumbers,
            update_existing_model_files => $update_existing_model_files
        );

        (my $frem_model5, $message, $need_update) = $self->run_unless_run(numbers => [5]);
        if (defined $message) {
            ui->print(category => 'frem',
                      message => "estimation of model 5 failed, cannot prepare model 6 (final cholesky model)");
        } else {
            $self->prepare_model6(
                model => $frem_model5,
                first_cholesky_theta => scalar(@{$frem_model3->problems->[0]->thetas}),
                start_omega_record => $self->start_omega_record,
                parameter_etanumbers => $parameter_etanumbers,
                update_existing_model_files => ($need_update or $update_existing_model_files)
            );

            push(@{$self->final_numbers}, 6);
        }
    }

    (my $final_models, $mes) = $self->run_unless_run(numbers => $self->final_numbers,
                                                     subdirectory => 'final_models',
                                                     final => 1) if (scalar(@{$self->final_numbers}) > 0);
    push(@{$self->final_models}, @{$final_models});

    #model 4
    my $mod4ofv = $self->final_models->[0]->outputs->[0]->get_single_value(attribute => 'ofv');
    if (not defined $mod4ofv) {
        ui->print(category => 'frem',
            message => 'estimation of model 4 failed to give ofv value. creating model 7.');
        $self->prepare_model7(model => $self->final_models->[0],update_existing_model_files => $update_existing_model_files);
        (my $frem_model7, $message, $need_update) = $self->run_unless_run(numbers => [7],
            subdirectory => 'final_models');
        if (defined $message) {
            ui->print(category => 'frem', message => $message);
            die;
        } else {
            $sir_model = $frem_model7;
            $sir_model_text = 'model 7';
        }
    } else {
        $sir_model = $self->final_models->[0];
        $sir_model_text = 'model 4';
    }

    (my $error, $message) = check_covstep(output => $self->final_models->[0]->outputs->[0]);

    my $do_print_proposal = 0;
    if ($error and not $self->bipp) {
        $logger->warning('Covariance step of model 4 NOT successful. Falling back to using the BIPP method.');
        $do_print_proposal = 1;
        $self->bipp(1);
    }
    if (not $error and not $self->bipp) {
        $logger->info('Covariance step was successful!');
        if ($self->always_proposal_density) {
            $do_print_proposal = 1;
            $logger->info('Will create alternative proposal density for model 4 sir');
        }
    }

    my $output_model2;
    if (defined $output_model2_fullname) {
        $output_model2 = output->new(filename => $output_model2_fullname, parse_output => 0);
    } else {
        $output_model2 = $frem_model2->outputs->[0];
    }
    if ($do_print_proposal) {
        my $new_omega_order = [map { $self->etas_reorder_mapping->{$_} } sort keys %{$self->etas_reorder_mapping}];
        print_proposal_density(
            omega_orders => $new_omega_order,
            partial_outputs => [ $output_model1, $output_model2 ],
            full_model => $sir_model,     # not updated, but may have estimates of everything
            reordered_model1 => $frem_model1,
            rse => $self->rse,
            directory => $self->directory,
            filename => $proposal_filename,
            etas_mapping => $self->etas_reorder_mapping,
        );
        ui->print(category => 'frem',
            message => "printed $proposal_filename for sir -covmat_input option");
    }
    if ($error and $self->run_sir) {
        ui->print(category => 'frem', message => 'starting sir');
        ui->category('sir');
        my %options;
        $options{'problems_per_file'} = 25;
        $options{'covmat_input'} = $self->directory.$proposal_filename;
        input_checking::check_options(tool => 'sir', options => \%options, model => $sir_model);

        my $sir = tool::sir->new(
            %{common_options::restore_options(@common_options::tool_options)},
            %options,
            top_tool => 1,
            models => [ $sir_model ],
            template_file_rplots => 'sir_default.r',
            directory => $self->directory . 'sir_dir1',
            fast_posdef_checks => 1,
        );

        $sir->print_options(cmd_line => 'sir final_models/'.$sir_model->filename.' -covmat_input='.$proposal_filename,
            toolname => 'sir',
            local_options => ["samples:s", "resamples:s", "covmat_input:s", "problems_per_file:i"],
            common_options => \@common_options::tool_options) ;
        $sir->run();
        $sir->prepare_results();
        $sir->print_results();

        ui->category('frem');
        ui->print(category => 'frem', message => 'sir done');
    }
}

sub create_data2_model
{
    my %parm = validated_hash(\@_,
        model => { isa => 'Ref', optional => 0 },
        filename => { isa => 'Str', optional => 0 },
        use_pred => { isa => 'Bool', optional => 0 },
        filtered_datafile => { isa => 'Str', optional => 0 },
        bov_parameters => { isa => 'Int', default => 0 },
        dv  => { isa => 'Str', optional => 0 },
        time_varying  => { isa => 'ArrayRef', default => [] },
        covariates  => { isa => 'ArrayRef', optional => 0 },
        occasion  => { isa => 'Str', default => '' },
    );

    my $model = $parm{'model'};
    my $filename = $parm{'filename'};
    my $use_pred = $parm{'use_pred'};
    my $filtered_datafile = $parm{'filtered_datafile'};
    my $bov_parameters = $parm{'bov_parameters'};
    my $dv = $parm{'dv'};
    my $covariates = $parm{'covariates'};
    my $time_varying = $parm{'time_varying'};
    my $occasion = $parm{'occasion'};

    #in ref of model,
    #filename of new filter model
    #out name of data file $outdatafile with full path

    my $typeorder = [];
    my $extra_input_items = [];

    my $filtered_data_model = $model -> copy ( filename => $filename,
                                               output_same_directory => 1,
                                               write_copy => 0,
                                               copy_datafile          => 0,
                                               copy_output        => 0);

    die "no problems" unless defined $filtered_data_model->problems();
    die "more than one problem" unless (scalar(@{$filtered_data_model->problems()})==1);

    my @filter_table_header;

    if( defined $filtered_data_model->problems()->[0] -> inputs and
        defined $filtered_data_model->problems()->[0] -> inputs -> [0] -> options ) {
        my ($arr,$time_added) = $filtered_data_model->problems()->[0] -> inputs -> [0]->get_filter_table_names;
        croak ("found no undropped data column in \$INPUT ") unless (defined $arr);
        croak ("automatic filtering cannot yet handle \$INPUT with DATX but without TIME") if ($time_added);
        @filter_table_header = @{$arr};
    } else {
        croak("Trying to construct table for filtering data".
            " but no headers were found in \$INPUT" );
    }

    $typeorder = [$dv]; #index 0 is original obs column name
    if (scalar(@{$covariates})>0){
        push(@{$typeorder},@{$covariates}); #add list of covariate names to typeorder
    }
    my $first_timevar_type = scalar(@{$typeorder});
    if (scalar(@{$time_varying})>0){
        push(@{$typeorder},@{$time_varying}); #add list of time_varying covariate names to typeorder
    }
    my @cov_indices = (-1) x scalar(@{$typeorder}); #initiate with invalid indices

    my $evid_index;
    my $mdv_index;
    my $type_index;
    my $occ_index;
    for (my $i=0; $i< scalar(@filter_table_header); $i++){
        if ($filter_table_header[$i] eq 'EVID'){
            $evid_index = $i;
        }elsif($filter_table_header[$i] eq 'MDV'){
            $mdv_index = $i;
        }elsif($filter_table_header[$i] eq $fremtype){
            $type_index = $i;
        }elsif($filter_table_header[$i] eq $occasion){
            $occ_index = $i;
        }else{
            #typeorder 0 is dv
            for (my $j=0; $j< scalar(@cov_indices); $j++){
                if($filter_table_header[$i] eq $typeorder->[$j]){
                    $cov_indices[$j] = $i;
                    last;
                }
            }
        }
    }
    my $add_mdv=0;
    my @code;

    unless (defined $evid_index or defined $mdv_index){

        #if $PRED it means all rows are observations. Otherwise let nonmem add MDV
        if (not $use_pred){
            push(@filter_table_header,'MDV');
            $mdv_index = $#filter_table_header;
            push(@{$extra_input_items},'MDV');
            $add_mdv=1;
        }
    }
    if (defined $type_index){
        croak($fremtype." already defined in input model, not allowed.");
    }else{
        push(@filter_table_header,$fremtype);
        $type_index = $#filter_table_header;
        push(@{$extra_input_items},$fremtype);
    }
    unless (defined $occ_index or ($bov_parameters<1)){
        croak("occasion column ".$occasion." not found in input model.");
    }
    if ($cov_indices[0] < 0){
        croak("dependent value ".$dv." not found in input model.");
    }
    for (my $j=1; $j< scalar(@cov_indices); $j++){
        if ($cov_indices[$j] < 0){
            croak("covariate column ".$typeorder->[$j]." not found in input model.");
        }
    }

    my $message;
    if ($add_mdv){
        #cannot have dummy model, NONMEM cannot append MDV for dummy $PRED so must keep $PK
        foreach my $remove_rec ('simulation','covariance','table','scatter','estimation'){
            $filtered_data_model -> remove_records(type => $remove_rec);
        }

        if ($use_pred ) {
            croak("no add_mdv when PRED in model");
            $filtered_data_model->set_code(record => 'pred', code => \@code);
        } else {
            @code = @{$model->get_code(record => 'pk')};
            push(@code,$fremtype.'=0');
            $filtered_data_model->set_code(record => 'pk', code => \@code);
        }

        $message = "Running evaluation to filter data and add ".$fremtype." for FREM data set";
    }else{
        foreach my $remove_rec ('abbreviated','msfi','contr','subroutine','prior','model','tol','infn','omega','pk','aesinitial','aes','des','error','pred','mix','theta','sigma','simulation','estimation','covariance','nonparametric','table','scatter'){
            $filtered_data_model -> remove_records(type => $remove_rec);
        }

        my @predcode = ("$fremtype=0", 'Y=THETA(1)+ETA(1)+EPS(1)');
        if ($filtered_data_model->problems->[0]->find_data_column(column_name => 'L2') != -1) {     # Do we have L2?
            my $dummy_name = 'DMY6142';
            model_transformations::rename_column(model => $filtered_data_model, from => 'L2', to => $dummy_name);
            push @predcode, "L2=$dummy_name";
        }

        $filtered_data_model -> add_records(type => 'pred', record_strings => \@predcode);

        $filtered_data_model -> add_records(type => 'theta',
                                            record_strings => ['1']);
        $filtered_data_model -> add_records(type => 'omega',
                                            record_strings => ['1']);
        $filtered_data_model -> add_records(type => 'sigma',
                                            record_strings => ['1']);
        $message = "Running dummy model to filter data and add ".$fremtype." for FREM data set";
    }
    $filtered_data_model -> add_records(type => 'estimation',
                                        record_strings => ['MAXEVALS=0 METHOD=ZERO']);

    $filtered_data_model -> add_records( type           => 'table',
        record_strings => [ join( ' ', @filter_table_header ).
            ' NOAPPEND NOPRINT ONEHEADER FORMAT=sG15.9 FILE='.$filtered_datafile]);

    return ($filtered_data_model,\@filter_table_header,$extra_input_items,$message);
}

sub add_pred_error_code
{
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 },
        pred_error_code => { isa => 'ArrayRef', optional => 0 },
        pk_code => { isa => 'ArrayRef', optional => 0 },
        use_pred => { isa => 'Bool', optional => 0 },
        mu => { isa => 'Bool', optional => 0 },
    );
    my $model = $parm{'model'};
    my $pred_error_code = $parm{'pred_error_code'};
    my $pk_code = $parm{'pk_code'};
    my $use_pred = $parm{'use_pred'};
    my $mu = $parm{'mu'};

    if ($mu){
        if ($use_pred){
            if (scalar(@{$pk_code})>0){
                croak("pk code should be empty when use_pred is set. this is a bug");
            }
        }else{
            if (scalar(@{$pk_code})==0){
                croak("pk code should be defined when use_pred is false. this is a bug");
            }
        }
    }

    if (scalar(@{$pk_code}) > 0) {
        my @pk = @{$model->get_code(record => 'pk')};
        push(@pk,@{$pk_code});
        $model->set_code(record => 'pk', code => \@pk);
    }
    my @code;
    if ($use_pred){
        @code = @{$model->get_code(record => 'pred')};
        push(@code,@{$pred_error_code});
        $model->set_code(record => 'pred', code => \@code);
    }else{
        @code = @{$model->get_code(record => 'error')};
        push(@code,@{$pred_error_code});
        $model->set_code(record => 'error', code => \@code);
    }

}

sub check_model_features
{
    # Check and bail out if supplied models have features not supported by FREM
    my $self = shift;

    my $model = $self->models->[0];

    if (scalar(@{$model->problems}) > 1) {
        croak('Cannot have more than one $PROB in the input model.');
    }

    my $problem = $model->problems->[0];

    if (defined $problem->nwpri_ntheta()) {
        croak("frem does not support \$PRIOR NWPRI.");
    }

    if (defined $problem->priors) {
        croak("frem does not support \$PRIOR");
    }

    if (not defined $self->models->[0]->problems->[0]->estimations
            or scalar(@{$self->models->[0]->problems->[0]->estimations}) == 0) {
        croak("No \$EST in model");
    }
}

sub reorder_etas_mapping
{
    my $self = shift;
    my $model = shift;

    my $netas = $model->nomegas->[0];
    my %skipped_set;
    for my $skipped_omega (@{$self->skip_omegas}) {
        $skipped_set{$skipped_omega} = 1;
    }

    # Add FIXed to list of omegas to skip
    for my $record (@{$model->problems->[0]->omegas}) {
        if ($record->fix) {
            for (my $i = $record->n_previous_rows + 1; $i <= $record->n_previous_rows + $record->size; $i++) {
                $skipped_set{$i} = 1;
            }
        } elsif (not $record->is_block) {
            my $n = $record->n_previous_rows + 1;
            for my $option (@{$record->options}) {
                if ($option->fix) {
                    $skipped_set{$n} = 1;
                }
                $n++;
            }
        }
    }

    for my $iov_omega (@{model_transformations::find_etas(model => $model, type => 'iov')}) {
        $skipped_set{$iov_omega} = 1;
    }

    my @skipped = map { int($_) } sort { $a <=> $b } keys %skipped_set;
    $self->skip_omegas(\@skipped);
    my @non_skipped;
    for (my $n = 1; $n <= $netas; $n++) {
        if (not defined $skipped_set{$n}) {
            push @non_skipped, $n;
        }
    }
    my @new_order = (@skipped, @non_skipped);

    my %reorder;
    for (my $i = 0; $i < scalar(@new_order); $i++) {
        $reorder{$new_order[$i]} = $i + 1;
    }

    return \%reorder;
}

1;
