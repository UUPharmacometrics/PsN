package tool::frem;

use include_modules;
use tool::modelfit;
use Math::Random;
use Data::Dumper;
use Config;
use linear_algebra;
use ui;
use logging;
use File::Copy qw/cp mv/;
use File::Path 'rmtree';
use nmtablefile;
use utils::phitable;
use array;
use tool::sir;
use input_checking;
use POSIX ":sys_wait_h"; #for forking
use POSIX qw/floor/;

use Moose;
use MooseX::Params::Validate;

extends 'tool';

#FIXME dv synonym automatic handling

my $fremtype = 'FREMTYPE';
my $smallcorrelation = 0.01; #FIXME
my $bov_variance_init = 0.1; #FIXME
my $indentation = '     ';
my $smallnum = 0.0000001;
my $small_correlation = 0.01;
my $name_model_1 = 'model_1.mod';
my $name_model_1_updated = 'model_1_updated.mod';
my $name_model_2 = 'model_2.mod';
my $name_model_2_updated = 'model_2_updated.mod';
my $name_model_3 = 'model_3.mod';
my $name_model_3_updated = 'model_3_updated.mod';
my $name_model_4 = 'model_4.mod';
my $name_model_4b = 'model_4b.mod';
my $name_model_7 = 'model_7.mod';

has 'tool_child_id' => (is => 'rw', isa => 'Int', default => 0);
has 'deriv2_nocommon_maxeta'  => ( is => 'rw', isa => 'Int', default=> 60);
has 'skip_omegas' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'run_sir' => ( is => 'rw', isa => 'Bool', default=> 0);
has 'fork_runs' => ( is => 'rw', isa => 'Bool', default=> 0);
has 'poll_interval' => ( is => 'rw', isa => 'Int', default=> 30);
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
has 'vpc' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'dv' => ( is => 'rw', isa => 'Str', default => 'DV' );
has 'occasion' => ( is => 'rw', isa => 'Str');
has 'parameters_bov' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'time_varying' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'covariates' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'categorical' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'log' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'regular' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef', default => sub { ['frem.log'] } );
has 'use_pred' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'estimate_regular_final_model' => ( is => 'rw', isa => 'Bool', default => 1 ); #no commandline option currently
has 'estimate_means' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'estimate_covariates' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'has_missingness' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'cholesky' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'imp_covariance' => ( is => 'rw', isa => 'Bool', default => 1 );

has 'results_file' => ( is => 'rw', isa => 'Str', default => 'frem_results.csv' );
has 'reordered_model_1' => ( is => 'rw', isa => 'model' );
has 'model_2' => ( is => 'rw', isa => 'model' );
has 'final_numbers' => ( is => 'rw', isa => 'ArrayRef[Int]', default => sub { [] } );
has 'final_models' => ( is => 'rw', isa => 'ArrayRef[model]', default => sub { [] } );
has 'cov_summary' => ( is => 'rw', isa => 'Str' );

my $logger = logging::get_logger("frem");

sub BUILD
{
    my $self  = shift;
    my $model = $self->models->[0];
    my $problem = $model->problems->[0];
    my $datafiles = $model->datafiles(absolute_path => 1);
    my $data = data->new(
        filename => $datafiles->[0],
        ignoresign => defined $model->ignoresigns ? $model->ignoresigns->[0] : undef,
        missing_data_token => $self->missing_data_token,
        idcolumn => $model->idcolumns->[0],
    );

    # Check if any covariate column has all same value
    # In that case warn and remove column
    my @filtered_covariates;
    my @filtered_categorical;
    for my $column (@{$self->covariates}) {
        my $colno = $problem->find_data_column(column_name => $column, ignore_dropped => 0);
        my $column_data = $data->column_to_array(column => $colno);
        if (scalar @{array::unique($column_data)} == 1) {
            $logger->warning("Covariate $column excluded because it has only one value for all rows in the dataset.");
        } else {
            push @filtered_covariates, $column;
            if (grep { $_ eq $column } @{$self->categorical}) {
                push @filtered_categorical, $column;
            }
        }
    }
    $self->covariates(\@filtered_covariates);
    $self->categorical(\@filtered_categorical);

    for my $accessor ('logfile','raw_results_file','raw_nonp_file'){
        my @new_files=();
        my @old_files = @{$self->$accessor};
        for (my $i=0; $i < scalar(@old_files); $i++){
            my $name;
            my $ldir;
            ( $ldir, $name ) =
            OSspecific::absolute_path( $self ->directory(), $old_files[$i] );
            push(@new_files,$ldir.$name) ;
        }
        $self->$accessor(\@new_files);
    }

    foreach my $model ( @{$self -> models} ) {
        foreach my $problem (@{$model->problems()}){
            if (defined $problem->nwpri_ntheta()){
                $logger->warning("frem does not support \$PRIOR NWPRI.");
                last;
            }
        }
    }

    if ( scalar (@{$self -> models->[0]-> problems}) > 1 ){
        croak('Cannot have more than one $PROB in the input model.');
    }

    unless (scalar(@{$self->covariates})>0){
        croak("Must have at least one covariate");
    }

    if (scalar(@{$self->log})> 0){
        my $indices = array::get_array_positions(target => $self->covariates,
                                                 keys=> $self->log,
                                                 R_indexing => 0);
        unless (scalar(@{$indices}) == scalar(@{$self->log})){
            croak("-log list:".join(',',@{$self->log})." is not a subset of ".
                " -covariates:".join(',',@{$self->covariates}));
        }

    }
    if (scalar(@{$self->categorical})> 0){
        my $indices = array::get_array_positions(target => $self->covariates,
                                                 keys=> $self->categorical,
                                                 R_indexing => 0);
        unless (scalar(@{$indices}) == scalar(@{$self->categorical})){
            croak("-categorical list:".join(',',@{$self->categorical})." is not a subset of ".
                  " -covariates:".join(',',@{$self->covariates}));
        }
    }
    if (scalar(@{$self->log})> 0){
        my $indices = array::get_array_positions(target => $self->categorical,
                                                 keys=> $self->log,
                                                 R_indexing => 0);
        if (scalar(@{$indices})>0){
            croak("-log list:".join(',',@{$self->log})." must have no elements in common with ".
                  " -categorical:".join(',',@{$self->categorical}));
        }

    }

    my $regular = get_regular_covariates(covariates => $self->covariates,
                                         categorical => $self->categorical,
                                         log => $self->log);
    $self->regular($regular);

    my $dv_ok=0;

    my $prob = $self -> models->[0]-> problems -> [0];
    if (defined $prob->priors()){
        croak("frem does not support \$PRIOR");
    }

    if( defined $prob -> inputs and defined $prob -> inputs -> [0] -> options ) {
        foreach my $option ( @{$prob -> inputs -> [0] -> options} ) {
            unless (($option -> value eq 'DROP' or $option -> value eq 'SKIP'
                        or $option -> name eq 'DROP' or $option -> name eq 'SKIP')){
                $dv_ok = 1 if ($option -> name() eq $self->dv());
            }
        }
        croak("dependent column ".$self->dv()." not found in \$INPUT" ) unless $dv_ok;
    } else {
        croak("Trying to check parameters in input model".
            " but no headers were found in \$INPUT" );
    }

    my @code = @{$self -> models->[0]->get_code(record => 'pk')};
    my $use_pred = 0;
    unless ($#code > 0) {
        @code = @{$self -> models->[0]->get_code(record => 'pred')};
        $use_pred = 1;
    }
    if ( $#code <= 0 ) {
        croak("Neither PK or PRED defined in model");
    }
    $self->use_pred($use_pred);

    unless ( defined $self->models->[0]-> problems->[0]-> estimations
             and scalar (@{$self->models->[0]-> problems->[0]->estimations}) > 0 ){
        croak("No \$EST in model");
    }
    $self->input_model_fix_thetas(get_or_set_fix(model => $self->models->[0],
                                                 type => 'thetas'));
    $self->input_model_fix_omegas(get_or_set_fix(model => $self->models->[0],
                                                 type => 'omegas'));
    $self->input_model_fix_sigmas(get_or_set_fix(model => $self->models->[0],
                                                 type => 'sigmas'));


    # auto-skip fixed OMEGAs
    my %skip_omegas = map { $_ => 1 } @{$self->skip_omegas};
    my @new_skip_omegas;
    my $om_num = 1;
    for (my $i=0; $i<scalar(@{$self->input_model_fix_omegas}); $i++) { # loop over omegas
        for (my $j=0; $j<scalar(@{$self->input_model_fix_omegas->[$i]}); $j++) {
            my $fixed = $self->input_model_fix_omegas->[$i]->[$j];
            if ($fixed && !exists($skip_omegas{$om_num})) {
                push @{$self->skip_omegas}, $om_num;
                push @new_skip_omegas, $om_num;
            }
            $om_num++;
        }
    }
    if (scalar(@new_skip_omegas) > 0) {
        print "Skipping fixed OMEGA record(s) which were not already skipped (required): ", join( ", ", @new_skip_omegas ), "\n";
        %skip_omegas = map { $_ => 1 } @{$self->skip_omegas};
    }

    # check that skipped OMEGAs are all first or last
    if (keys %skip_omegas > 0) {
        my $nom = $om_num-1;
        my $first_skip = exists($skip_omegas{1}) ? 1 : 0;
        my $last_skip = exists($skip_omegas{$nom}) ? 1 : 0;
        unless ($first_skip || $last_skip) {
            print "Skipped OMEGA record(s) must all be positioned first or last (neither first nor last OMEGA record is skipped)";
            croak "Can't continue due to non-supported skipping pattern";
        }
        for (my $i=2; $i<=(keys %skip_omegas); $i++) {
            $om_num = $first_skip ? $i : $nom-$i+1; # count forward or backward
            unless (exists($skip_omegas{$om_num})) {
                print "Skipped OMEGA records must all be positioned first or last (expected OMEGA record $om_num skipped)";
            croak "Can't continue due to non-supported skipping pattern";
            }
        }
    }
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

    if ($is_classical){
        $diagonal = 'ETA';
        $offdiagonal = 'ETC';
    }else{
        $diagonal = 'PHI';
        $offdiagonal = 'PHC';
    }
    return ($diagonal,$offdiagonal);

}

sub read_covdata
{
    my %parm = validated_hash(\@_,
                              covnames => { isa => 'ArrayRef', optional => 0 },
                              filename => { isa => 'Str', optional => 0 },
                              dv => { isa => 'Str', default => 'DV' },
    );
    my $covnames = $parm{'covnames'};
    my $filename = $parm{'filename'};
    my $dv = $parm{'dv'};

    my %fremtype_to_cov=();
    my %cov_arrays=();
    my %id_arrays=();


    for (my $i=0; $i< scalar(@{$covnames}); $i++){
        my $fremtype = ($i+1)*100;
        $fremtype_to_cov{$fremtype}=$covnames->[$i];
        $cov_arrays{$covnames->[$i]} = [];
    }

    open my $fh, '<', $filename;
    my $frem_index=-1;
    my $dv_index=-1;
    my $id_index=-1;
    my $header_row = <$fh>;
    chomp($header_row);
    $header_row =~ s/\r//; # remove CR
    my @fields = split(',',$header_row);
    for (my $i=0; $i< scalar(@fields); $i++){
        if($fields[$i] eq $dv){
            $dv_index = $i;
        }elsif($fields[$i] eq 'FREMTYPE'){
            $frem_index = $i;
        }elsif($fields[$i] eq 'ID'){
            $id_index = $i;
        }
        last if (($frem_index >= 0) and ($dv_index >=0) and ($id_index >=0));
    }
    croak("could not find DV and FREMTYPE and ID") unless (($frem_index >= 0) and ($dv_index >=0) and ($id_index >=0));
    my %id_idx;
    my $idx = 0;
    my $row;
    while (1) {
        $row = <$fh>;
        last unless (defined $row);
        chomp ($row);
        @fields = split(',',$row);
        if ($fields[$frem_index] > 0){
            my $cov = $fremtype_to_cov{$fields[$frem_index]};
            push(@{$cov_arrays{$cov}},$fields[$dv_index]);
            my $id = $fields[$id_index];
            unless (exists $id_arrays{$id}){
                $id_arrays{$id}={};
                $idx++;
            }
            if (exists $id_arrays{$id}->{$cov}){
                croak("redefinition of $cov for id $id");
            }else{
                $id_arrays{$id}->{$cov} = $fields[$dv_index];
                # map id to index (in cov arrays)
                $id_idx{$id} = ($idx-1);
            }
        }
    }
    close $fh;
    for (my $i=0; $i< scalar(@{$covnames}); $i++){
        unless (scalar(@{$cov_arrays{$covnames->[$i]}}) > 0){
            croak("legth $i not larger than 0");
        }
    }

    # build covariate vectors and remove subjects with missing data
    my %id_has_missing;
    my %cov_has_missing;
    my @id_covariate_vectors = ();
    foreach my $idnum (sort {$a <=> $b} keys %id_arrays){ # for each id
        # check for missingness
        my @nonmissing_covs = ();
        for (my $i=0; $i< scalar(@{$covnames}); $i++){
            if (!defined $id_arrays{$idnum}->{$covnames->[$i]}){
                $id_has_missing{$idnum} = 1 unless (exists($id_has_missing{$idnum}));
                $cov_has_missing{$covnames->[$i]} = 1 unless (exists($cov_has_missing{$covnames->[$i]}));
            } else {
                push @nonmissing_covs, $covnames->[$i];
            }
        }

        # only consider non-missing subjects
        if (scalar(@nonmissing_covs) != scalar(@{$covnames})) {
            # delete all other read covariate values from this subject
            foreach my $cov (@nonmissing_covs) {
                splice @{$cov_arrays{$cov}}, $id_idx{$idnum}, 1;
            }
            # TODO: refactor this function in general (index counting is very patched)
            foreach my $id (keys %id_idx) {
                $id_idx{$id}-- if ($id_idx{$id} > $id_idx{$idnum});
            }
            delete $id_idx{$idnum};
            # delete corresponding value from raw id arrays
            delete $id_arrays{$idnum};
        } else {
            push(@id_covariate_vectors,[$idnum]);
            for (my $i=0; $i< scalar(@{$covnames}); $i++){
                push(@{$id_covariate_vectors[-1]},$id_arrays{$idnum}->{$covnames->[$i]});
            }
        }
    }
    if (scalar(keys %id_has_missing) > 0) {
        print "Warning: Covariate value(s) (of ", join(",", keys %cov_has_missing), ") are missing of some IDs (", join(",", keys %id_has_missing), ")\n";
        print "(Assumed missing completely at random, subjects dropped)\n";
        # TODO: support other modes of missingness handling
    }

    my %categoryinfo=();
    my @perc_5th=();
    my @perc_95th=();
    my @categorical=();
    for (my $i=0; $i< scalar(@{$covnames}); $i++){
        my @sorted = (sort {$a <=> $b} @{$cov_arrays{$covnames->[$i]}}); #sort ascending
        my $quantref = array::quantile(probs => [0.05,0.5,0.95], numbers=> \@sorted);
        push(@perc_5th,$quantref->[0]);
        push(@perc_95th,$quantref->[2]);
        my $unique = array::unique(\@sorted);
        if (scalar(@{$unique})==2){
            push(@categorical,1);
            my $ref = $unique->[0];
            my $other = $unique->[1];
            #median is either equal to a category, or mean of the two categories if exactly equal numbers
            if ($quantref->[1] == $unique->[1]){
                $ref = $unique->[1];
                $other = $unique->[0];
            }
            #diff is other minus reference
            $categoryinfo{$covnames->[$i]}={'reference' => $ref,'other' => $other, 'diff' => ($other-$ref)};
        }else{
            push(@categorical,0);
        }
    }


    for (my $i=1; $i< scalar(@{$covnames}); $i++){
        unless (scalar(@{$cov_arrays{$covnames->[$i]}}) == scalar(@{$cov_arrays{$covnames->[$i-1]}})){
            croak("unequal length $i and $i-1");
        }
    }

    return(\@perc_5th,\@perc_95th,\@id_covariate_vectors,\@categorical,\%categoryinfo,\%id_has_missing,\%cov_has_missing);
}

sub get_post_processing_data
{
    # Get post-processing data from final FREM model.
    # If cov_model (model 2) is defined, get cov means and variances
    # from inits here instead of (possibly estimated) final FREM.
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 },
        cov_model => { isa => 'model', optional => 1 },
    );

    # TODO: don't update inits just to get estimates, get them directly
    my $model = $parm{'model'};
    $model->update_inits(from_output => $model->outputs->[0]);
    my $cov_model = $parm{'cov_model'};
    if (defined $cov_model) {
        $cov_model->update_inits(from_output => $cov_model->outputs->[0]);
    }

    # figure out $OMEGA index of FREM block, size of it and ETAs before
    # (skipped and not a part of the final FREM block)
    my $omegaindex = scalar(@{$model->problems->[0]->omegas})-1;
    my $size = $model->problems->[0]->omegas->[$omegaindex]->size;
    my $etas_before_omega = 0;
    if ($omegaindex > 0) {
        my $etas_per_omega = model::problem::etas_per_omega(problem => $model->problems->[0]);
        foreach my $eta_arr (@{$etas_per_omega}[0..$omegaindex-1]) {
            $etas_before_omega = $etas_before_omega + scalar(@{$eta_arr});
        }
    }

    my $pred_code = $model->get_code(record => 'pred');
    my $pk_code = $model->get_code(record => 'pk');
    my $error_code = $model->get_code(record => 'error');
    my $code = $error_code;
    unless (scalar(@{$code}) > 0) {
        $code = $pred_code;
    }
    if ( scalar(@{$code}) <= 0 ) {
        croak("Neither ERROR or PRED defined in post-processing model");
    }

    #figure out if old frem code or new
    my $compact_frem=0;
    my $newtag=';;;FREM CODE END COMPACT';
    for (my $i=0; $i < scalar(@{$code}); $i++){
        if ($code->[$i] =~ /^$newtag/){
            $compact_frem=1;
            last;
        }
    }
    unless ($compact_frem){
        $code = $pk_code;
        unless (scalar(@{$code}) > 0) {
            $code = $pred_code;
        }
        if ( scalar(@{$code}) <= 0 ) {
            croak("Neither PK or PRED defined in post-processing model");
        }
    }
#FIXME handle mu-modeling, read rescale from cov-comment

    #get $ncov from code
    my $starttag = ';;;FREM CODE BEGIN';
    my $endtag=';;;FREM CODE END';
    my @covnames=();
    my @covetas=();
    my @rescaling=();
    my $foundstart=0;
    for (my $i=0; $i < scalar(@{$code}); $i++){
        if ($code->[$i] =~ /^$starttag/){
            $foundstart=1;
            next;
        }elsif($foundstart and ($code->[$i] =~ /^$endtag/)){
            last;
        }elsif($foundstart){
            if ($compact_frem){
                if ($code->[$i] =~ /^\s*IF \(FREMTYPE\.EQ\.(\d+)\) THEN/){
                    my $fremtype = $1;
                    my $cov;
                    my $etanum;
                    my $rescale='';
                    if ($code->[$i+1] =~ /^\s*;\s*(\w+)/){
                        $cov = $1;
                    }else{
                        croak("error parsing FREM code, was it modified?\n".$code->[$i+1]);
                    }
                    if ($code->[$i+1] =~ /^\s*;\s*$cov\s+(\d+\.?\d*)/ ){
                        #rescale printed on comment line -> might have mu modelling
                        $rescale = $1;
                        if ($code->[$i+2] =~ /^\s*Y = THETA\((\d+)\) \+ ETA\((\d+)\)/){
                            #no mu
                            $etanum = $2;
                        }elsif ($code->[$i+2] =~ /^\s*Y = COV(\d+) \+ EPS\(/){
                            #mu
                            $etanum = $1;
                        }else{
                            croak("error parsing FREM code, was it modified?\n".$code->[$i+2]);
                        }

                    }else{
                        #cannot have mu modelling, older format
                        if ($code->[$i+2] =~ /^\s*Y = THETA\((\d+)\) \+ ETA\((\d+)\)\*?(\d*\.?\d*)/){
                            $etanum = $2;
                            $rescale = $3;
                        }else{
                            croak("error parsing FREM code, was it modified?\n".$code->[$i+2]);
                        }
                    }
                    push(@covnames,$cov);
                    push(@covetas,$etanum);
                    if (length($rescale)>0){
                        push(@rescaling,$rescale);
                    }else{
                        push(@rescaling,1);
                    }
                }
            }else{
                if ($code->[$i] =~ /^\s*BSV_(.+) = ETA\((\d+)\)\*?(\d*\.?\d*)/){
                    my $cov = $1;
                    my $etanum = $2;
                    my $rescale = $3;
                    push(@covnames,$cov);
                    push(@covetas,$etanum);
                    if (length($rescale)>0){
                        push(@rescaling,$rescale);
                    }else{
                        push(@rescaling,1);
                    }
                }
            }
        }
    }
    unless ($foundstart){
        croak("Did not find FREM tags in model code");
    }

    # get means from inits of covariate thetas
    my $thetavalues;
    if (defined $cov_model) {
        $thetavalues = $cov_model->get_hash_values_to_labels(category => 'theta');
    } else {
        # fallback on final FREM model if no cov_model (model 2)
        $thetavalues = $model->get_hash_values_to_labels(category => 'theta');
    }
    my @cov_means = ();
    foreach my $cn (@covnames){
        unless (defined $thetavalues->[0]->{'theta'}->{'TV_'.$cn}){
            croak("could not find theta value for TV_".$cn);
        }
        push(@cov_means,$thetavalues->[0]->{'theta'}->{'TV_'.$cn});
    }

    # get variances from inits of covariate omegas (times rescale)
    my $omegavalues;
    if (defined $cov_model) {
        $omegavalues = $cov_model->get_hash_values_to_labels(category => 'omega');
    } else {
        # fallback on final FREM model if no cov_model (model 2)
        $omegavalues = $model->get_hash_values_to_labels(category => 'omega');
    }
    my @cov_var = ();
    for (my $i=0; $i<scalar(@covnames); $i++) {
        my $cn = $covnames[$i];
        my $resc = $rescaling[$i];
        unless (defined $omegavalues->[0]->{'omega'}->{'BSV_'.$cn}) {
            croak("could not find omega value for BSV_".$cn);
        }
        my $var = $resc * $resc * $omegavalues->[0]->{'omega'}->{'BSV_'.$cn};
        push(@cov_var, $var);
    }

    my $npar = $size - scalar(@covnames);

    # get parameter labels
    my $row=1;
    my $col=1;
    my @parnames=();
    my %parnames=();
    foreach my $opt (@{$model->problems->[0]->omegas->[$omegaindex]->options}){
        if ($row == $col){
            my $lab = $opt->label;
            my $etanum = $etas_before_omega + scalar(@parnames)+1;

            if (defined $lab) {
                # strip leading non-alpha chars and trailing whitespace
                $lab =~ s/^[^a-zA-z]+|\s+$//g;
                # strip quotation marks (") and replace whitespace with underscore (_)
                if ($lab =~ /[\s"]+/) {
                    $lab =~ s/\s+/_/g;
                    $lab =~ s/"+//g;
                    if (length($lab)>0) {
                        $logger->warning("Comment on OMEGA row contains whitespace or \" chars, using '$lab' for ETA($etanum)");
                    }
                }
            }
            unless (defined $lab and length($lab)>0){
                $lab = '';

                # try to get LHS of ETA(N) usage
                my @code;
                push @code, @{$pred_code}  if (defined $pred_code);
                push @code, @{$pk_code}    if (defined $pk_code);
                push @code, @{$error_code} if (defined $error_code);
                foreach my $line (@code) {
                    if ($line =~ /[^;]+=.*\bETA\($etanum\)/) {
                        chomp $line;
                        (my $lhs = $line) =~ s/\W*(\w+)\W*=.+/$1/;
                        if (length($lhs)>0) {
                            ($lab = $lhs) =~ s/^(TV|POP)_*//;
                            $logger->info("No comment on OMEGA row but found LHS '$lhs', considering '$lab' for ETA($etanum)");
                            last unless (exists $parnames{$lab});
                        }
                    }
                }

                if (length($lab) == 0) {
                    # generate "PAR2" style label if none now exists
                    $lab = 'PAR'.(scalar(@parnames)+1);
                    $logger->warning("No label for OMEGA, using '$lab' for ETA($etanum)");
                }
            }
            # iterate label if collision (e.g. "CL"->"CL_2")
            my $newparam = $lab;
            my $num = 2;
            while (exists $parnames{$newparam}) {
                $newparam = $lab."_$num";
            }
            if ($newparam ne $lab) {
                $logger->warning("OMEGA label (n=$num) collision, using '$newparam' for ETA($etanum)");
            }

            push(@parnames,$newparam);
            $parnames{$newparam} = 1;
            $col=1;
            $row++;
        }else{
            $col++;
        }
        last if (scalar(@parnames)==$npar);
    }

    return(\@covnames,\@rescaling,$omegaindex,\@parnames,$size,\@cov_means,\@cov_var);
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
                              start_omega_record  => { isa => 'Int', optional => 0 },
                              covariate_etanumbers  => { isa => 'ArrayRef', optional => 0 },
                              parameter_etanumbers => { isa => 'ArrayRef', optional => 0 },
    );
    my $model = $parm{'model'};
    my $skip_etas = $parm{'skip_etas'};
    my $start_omega_record = $parm{'start_omega_record'};
    my $covariate_etanumbers = $parm{'covariate_etanumbers'};
    my $parameter_etanumbers = $parm{'parameter_etanumbers'};

    #my $num = scalar(@{$covariate_etanumbers});
    #unless ($num == scalar(@{$parameter_etanumbers})){
    #    croak("num is $num but parameter num is ".scalar(@{$parameter_etanumbers}));
    #}
    #etas from evaluation do not have priority over inits

    my @omega_records = ();
    my $n_previous_rows = $skip_etas;

    my @start_etas = ();
    my @end_etas = ();
    foreach my $par (@{$parameter_etanumbers}){
        push(@start_etas,$par->[0]);
        push(@end_etas,$par->[-1]);
    }
    push(@start_etas,$covariate_etanumbers->[0]);
    push(@end_etas,$covariate_etanumbers->[-1]);

    my @labels = ();
    for (my $k=($start_omega_record-1); $k < scalar(@{$model->problems->[0]->omegas}); $k++){
        foreach my $opt (@{$model->problems->[0]->omegas->[$k]->options}){
            if ($opt->on_diagonal){
                if (defined $opt->label){
                    push(@labels,$opt->label);
                }else{
                    push(@labels,undef);
                }
            }
        }
    }
    my ($initblock,$message) = get_filled_omega_block(model => $model,
                                                      problem_index => 0,
                                                      start_etas => \@start_etas,
                                                      end_etas => \@end_etas,);

    my $size = scalar(@{$initblock});
    unless ($size > 0){
        croak("size of initblock is 0, message is $message\n");
    }
    my $omega_lines = get_omega_lines(new_omega => $initblock,
                                      labels => \@labels);
    push(@omega_records,model::problem::omega->new(record_arr => $omega_lines,
                                                   n_previous_rows => $n_previous_rows));

    return \@omega_records;
}

sub get_new_omega_order
{
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              problem_index  => { isa => 'Int', default => 0 },
                              skip_omegas  => { isa => 'ArrayRef', optional => 0 },
    );
    my $model = $parm{'model'};
    my $problem_index = $parm{'problem_index'};
    my $skip_omegas = $parm{'skip_omegas'};

    my $need_to_move = 0;
    #we assume skip_omegas already sorted ascending thanks to input_checking.pm
    for (my $i=0; $i<scalar(@{$skip_omegas}); $i++){
        unless ($skip_omegas->[$i] == ($i+1)){
            $need_to_move=1;
            last;
        }
    }

    my @old_omega_order = (1 .. scalar(@{$model->problems->[$problem_index]->omegas}));
    my @new_omega_order = ();
    if ($need_to_move){
        @new_omega_order = @{$skip_omegas};
        for (my $j=1; $j<= scalar(@{$model->problems->[$problem_index]->omegas}); $j++){
            my $this_is_skipped = 0;
            foreach my $s (@{$skip_omegas}){
                if ($s == $j){
                    $this_is_skipped =1;
                    last;
                }
            }
            push(@new_omega_order,$j) unless ($this_is_skipped);
        }
        unless (scalar(@new_omega_order) == scalar(@old_omega_order)){
            croak("coding error put skipped first");
        }
    }else{
        @new_omega_order = (1 .. scalar(@{$model->problems->[$problem_index]->omegas}));
    }
    return(\@new_omega_order,$need_to_move);
}

sub put_skipped_omegas_first
{

    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              problem_index  => { isa => 'Int', default => 0 },
                              start_omega_record  => { isa => 'Int', optional => 0 },
                              need_to_move  => { isa => 'Bool', optional => 0 },
                              new_omega_order  => { isa => 'ArrayRef', optional => 0 },
                              input_model_fix_omegas => { isa => 'ArrayRef', optional => 0 },
                              etas_file => { isa => 'Maybe[Str]', optional => 1 }
    );
    my $model = $parm{'model'};
    my $problem_index = $parm{'problem_index'};
    my $start_omega_record = $parm{'start_omega_record'};
    my $need_to_move = $parm{'need_to_move'};
    my $new_omega_order = $parm{'new_omega_order'};
    my $input_model_fix_omegas = $parm{'input_model_fix_omegas'};
    my $etas_file = $parm{'etas_file'};

    my @parameter_etanumbers = ();
    my $skip_etas = 0;
    my @fix_omegas;

    my $maxeta =  $model->problems()->[0]->nomegas(with_correlations => 0,
                                                   with_same => 1);
    #for each omega find old eta numbers and new eta numbers
    my $etas_per_omega = model::problem::etas_per_omega(problem => $model->problems->[0]);

    if ($need_to_move){
        if ($etas_file) {
            print "\$etas_file=$etas_file\n";
            # TODO: Support reordering via phitable->swap_etas
            carp("ETAS must be reordered but \$ETAS found, not yet fully supported: \$ETAS stripped from model 2 and later");
            $model->remove_records(type => "etas");
            undef $etas_file;
        }

        my @old_etas = (1 .. $maxeta);
        my @intermediate_etas =();
        foreach my $eta (@old_etas){
            push(@intermediate_etas,'o'.$eta);
        }

        foreach my $coderec ('error','des','pk','pred'){ #never any ETAs in $MIX
            my $acc = $coderec.'s';
            if (defined $model->problems->[0]->$acc and
                scalar(@{$model->problems->[0]->$acc})>0 ) {
                my @code = @{$model->problems->[0]->$acc->[0]->code};
                # rename all existing ETA\((\d+)\) to ETA(o\d+)
                model::problem::renumber_etas(code => \@code,
                                              eta_from => [\@old_etas],
                                              eta_to => [\@intermediate_etas]);
                #for each omega record
                #rename from ETA\(o(\d+)\) to ETA(newnum) , also if oldnum and newnum the same
                my $new_eta_count = 0;
                my @from = ();
                my @to = ();
                foreach my $pos (@{$new_omega_order}) {
                    my $j = $pos-1;
                    my $size = scalar(@{$etas_per_omega->[$j]});
                    foreach my $eta (@{$etas_per_omega->[$j]}){
                        push(@from,$intermediate_etas[($eta-1)]); #with o prefix
                    }
                    push(@to,(($new_eta_count+1) .. ($new_eta_count + $size)));
                    $new_eta_count += $size;
                }
                model::problem::renumber_etas(code => \@code,
                                              eta_from => [\@from],
                                              eta_to => [\@to]);

                $model->problems->[0]-> set_records( type => $coderec,
                                                     record_strings => \@code );
            }
        }

    }

    #reorder omega records and check non-skipped are not diagonal size > 1
    my @new_records = ();
    my $n_previous_rows = 0;
    for (my $k=0; $k<scalar(@{$new_omega_order}); $k++){
        if ($k==($start_omega_record-1)){ #    $start_omega_record = scalar(@{$skip_omegas})+1;
            $skip_etas = $n_previous_rows;
        }
        my $i = $new_omega_order->[$k]-1; #old index
        my $size = scalar(@{$etas_per_omega->[$i]});

        if ( $model->problems->[0]->omegas->[$i]->is_block or
            ($k < ($start_omega_record-1))){ #    $start_omega_record = scalar(@{$skip_omegas})+1;
            my $formatted = $model->problems->[0]->omegas->[$i]->_format_record();
            my @lines =();
            for (my $j=0; $j < scalar(@{$formatted}); $j++){
                push(@lines,split("\n",$formatted->[$j]));
            }
            #print "count ".scalar(@lines)."\n";
            push(@new_records,
                 model::problem::omega->new(record_arr => \@lines,
                                            n_previous_rows => $n_previous_rows));
            $n_previous_rows += $size;
        }else{ #DIAGONAL and non-skipped
            foreach my $opt (@{$model->problems->[0]->omegas->[$i]->options}){
                my ($formatted,$no_break) = $opt -> _format_option(is_block => 0); #is_blocks makes format add FIX if set
                push(@new_records,model::problem::omega->new(record_arr => ['BLOCK (1)',$formatted],
                                                             n_previous_rows => $n_previous_rows));
                $n_previous_rows++;
            }
        }
    }
    $model -> problems -> [0]-> omegas(\@new_records);

    @fix_omegas = @{get_or_set_fix(model => $model,
                                   type => 'omegas',
                                   stop_record => ($start_omega_record-1))};

    #reset etas per omega
    $etas_per_omega = model::problem::etas_per_omega(problem => $model->problems->[0]);
    for (my $j = ($start_omega_record-1); $j< scalar(@{$etas_per_omega}); $j++){
        push(@parameter_etanumbers,$etas_per_omega->[$j]);
    }

    return $skip_etas,\@fix_omegas,\@parameter_etanumbers,$etas_file;
}

sub get_reordered_coordinate_strings{
    my %parm = validated_hash(\@_,
                              problem => { isa => 'model::problem', optional => 0 },
                              omega_order => { isa => 'ArrayRef', optional => 0 },
        );
    my $problem = $parm{'problem'};
    my $omega_order = $parm{'omega_order'};

    unless (scalar(@{$omega_order})==scalar(@{$problem->omegas})){
        croak("omega order length is ".scalar(@{$omega_order}).
              " but number of old omega records is ".scalar(@{$problem->omegas}));
    }

    my @reordered_coordinate_strings = ();
    push(@reordered_coordinate_strings,
         @{$problem->get_estimated_attributes(parameter => 'theta',
                                              attribute=>'coordinate_strings')});
    foreach my $num (@{$omega_order}){
        my $oldindex = $num-1;
        push(@reordered_coordinate_strings,
             @{$problem->omegas->[$oldindex]->get_estimated_coordinate_strings});
    }
    push(@reordered_coordinate_strings,
         @{$problem->get_estimated_attributes(parameter => 'sigma',
                                              attribute=>'coordinate_strings')});

    return \@reordered_coordinate_strings;
}

sub get_eta_mapping{
    my %parm = validated_hash(\@_,
                              problem => { isa => 'model::problem', optional => 0 },
                              omega_order => { isa => 'ArrayRef', optional => 0 },
        );
    my $problem = $parm{'problem'};
    my $omega_order = $parm{'omega_order'};

    unless (scalar(@{$omega_order})==scalar(@{$problem->omegas})){
        croak("omega order length is ".scalar(@{$omega_order}).
              " but number of old omega records is ".scalar(@{$problem->omegas}));
    }

    my @reordered_etas =();
    foreach my $num (@{$omega_order}){
        my $oldindex = $num-1;
        push(@reordered_etas,
             @{$problem->omegas->[$oldindex]->get_estimated_coordinate_strings(only_eta_eps => 1)});
    }

    my %eta_mapping;
    for (my $i=0; $i<scalar(@reordered_etas); $i++){
        my $newnum = $i+1; #position in reordered array
        my $oldnum = $reordered_etas[$i];
        $eta_mapping{$oldnum} = $newnum;
    }
    return \%eta_mapping;
}

sub get_filled_omega_block
{
    #must have already done update inits on model so that get_matrix is estimated values, where available
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              problem_index  => { isa => 'Int', default => 0 },
                              table_index  => { isa => 'Int', default => -1 },
                              start_etas => { isa => 'ArrayRef', optional => 0 },
                              end_etas => { isa => 'ArrayRef', optional => 0 },
    );
    my $model = $parm{'model'};
    my $problem_index = $parm{'problem_index'};
    my $table_index = $parm{'table_index'};
    my $start_etas = $parm{'start_etas'};
    my $end_etas = $parm{'end_etas'};

    unless (scalar(@{$start_etas}) > 0){
        croak("start_etas array must be larger than 0");
    }
    unless (scalar(@{$start_etas}) == scalar(@{$end_etas})){
        croak("start_etas array must equal length to end_etas");
    }
    my $total_size = 0;
    my @sizes = ();
    for (my $i=0; $i <scalar(@{$start_etas}); $i++){
        unless (defined $start_etas->[$i]){
            croak("start_etas $i is undef");
        }
        unless (defined $end_etas->[$i]){
            croak("end_etas $i is undef");
        }
        if ($i > 0){
            unless ($start_etas->[$i] > $end_etas->[($i-1)]){
                croak("start_eta $i must be larger than end_eta ".($i-1));
            }
        }
        croak("start_eta $i cannot be larger than end_eta $i ") if ($start_etas->[$i] > $end_etas->[$i]);
        my $si = ($end_etas->[$i] - $start_etas->[$i] +1);
        $total_size += $si;
        push(@sizes,$si);
    }

    my $start_eta_1 = $start_etas->[0];
    my $end_eta_1 = $end_etas->[0];
    my $start_eta_2;
    my $end_eta_2;
    my $end_eta_top = $end_eta_1;

    if (scalar(@{$start_etas}) > 1){
        $start_eta_2 = $start_etas->[-1]; #last
        $end_eta_2  = $end_etas->[-1]; #last
        $end_eta_top = $end_etas->[-2]; #second to last, can be $end_eta_1. This usage assumes all consecutive
    }
    my $top_size = $end_eta_top - $start_eta_1 +1; #this assumes no gaps

    my $error = 0;
    my $message = '';
    my $corrmatrix;

    my @sd = ();
    my @mergematrix = ();

    #local coords
    my $have_corrmatrix=0;
    ($corrmatrix,$message) = get_correlation_matrix_from_phi(start_eta_1 => $start_eta_1,
                                                             end_eta_1 => $end_eta_top, #can be from multiple blocks here, or end_eta_1
                                                             start_eta_2 => $start_eta_2,
                                                             end_eta_2 => $end_eta_2,
                                                             problem_index => $problem_index,
                                                             table_index => $table_index,
                                                             model => $model);
    $have_corrmatrix=1 if (length($message) == 0);

    @sd = (0) x ($total_size);
    for (my $i=0; $i<($total_size); $i++){
        push(@mergematrix,[(0) x $total_size]);
    }

    #omega block. Do not assume all that are nonzero are estimated
    #get inits from model. local coords

    #print "\n sizes ".join(' ',@sizes)."\n";
    my $old_size=0;
    for (my $k=0; $k <scalar(@{$start_etas}); $k++){
        my $init_matrix = $model->problems->[$problem_index]->get_matrix(type => 'omega',
                                                                         start_row => $start_etas->[$k],
                                                                         end_row => $end_etas->[$k]);
        for (my $i=0; $i<$sizes[$k]; $i++){
            for (my $j=0; $j<$sizes[$k]; $j++){
                $mergematrix[$old_size+$i]->[$old_size+$j] = $init_matrix->[$i][$j];
            }
            $sd[$old_size+$i] = sqrt($init_matrix->[$i][$i]) if ($init_matrix->[$i][$i] > 0);
        }
        $old_size += $sizes[$k];
    }
    #foreach my $line (@mergematrix){
    #    print join("\t",@{$line})."\n";
    #}

    #now we have sd and valuematrix that are inits/estimates or 0.
    #for each value in mergematrix that is still 0, compute covar using correlation and sd,
    #or set very small

    for (my $i = 0; $i < $total_size; $i++){
        for (my $j = 0; $j < $i; $j++){
            #copy to make symmetric
            $mergematrix[$i]->[$j] = $mergematrix[$j]->[$i];
        }
        for (my $j = ($i+1); $j < $total_size; $j++){
            next unless ($mergematrix[$i]->[$j] == 0);

            unless (($j >= $sizes[0]) and ($j < $top_size)){
            #if (($j >= $sizes[0]) and ($j < $top_size)){
            #    $mergematrix[$i]->[$j] = $smallnum; #FIXME correlation 1%?
            #}else{
                #compute new
                if ((not $have_corrmatrix) or $corrmatrix->[$i][$j] == 0){
                    $mergematrix[$i]->[$j] = ($small_correlation)*($sd[$i])*($sd[$j]);
                }else{
                    $mergematrix[$i]->[$j] = ($corrmatrix->[$i][$j])*($sd[$i])*($sd[$j]);
                }
            }
        }
    }
    #print "\n";
    #foreach my $line (@mergematrix){
    #    print join("\t",@{$line})."\n";
    #}
    my $newmatrix = replace_0_correlation(old_matrix => \@mergematrix,
                                          is_covariance => 1,
                                          low_correlation => $small_correlation);
    my $rounded = round_off_omega(omega => $newmatrix);
    #get posdef is necessary, pheno will crash without it
    my ($posdefmatrix,$count)=linear_algebra::get_symmetric_posdef(matrix => $rounded);

    return($posdefmatrix,'');
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

    unless (length($filename)> 0){
        $error = 2;
        $message .= 'Empty phi file name';
    }
    unless (-e $filename){
        $error = 2;
        $message .= ' File '.$filename.' does not exist';
    }
    unless (($start_eta_1 > 0) and ($end_eta_1 >=$start_eta_1)){
        $error = 2;
        $message .= " Input error start, end eta 1: $start_eta_1, $end_eta_1";
    }
    if (defined $start_eta_2 and defined $end_eta_2){
        unless (($start_eta_2 > 0) and ($end_eta_2 >=$start_eta_2) and ($start_eta_2 > $end_eta_1)){
            $error = 2;
            $message .= " Input error end_eta_1, start 2, end eta 2: $end_eta_1,$start_eta_2, $end_eta_2";
        }
    }
    return([],$message) unless ($error == 0);

    my $nmtablefile = nmtablefile->new(filename => $filename);
    my @matrix = ();
    my $covariance = [];
    my $sdcorr = [];
    my ($diagonal,$offdiagonal) = get_phi_coltypes(model => $model);

    for (my $i = $start_eta_1; $i <= $end_eta_1; $i++){
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

    return ($sdcorr,'');
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
    my @record_lines=();
    push(@record_lines,'BLOCK('.$size.') ');
    my $form = '  %.12G';
    for (my $row=0; $row< $size; $row++){
        my $line = '';
        for (my $col=0; $col<=$row; $col++){
            my $str= sprintf("$form",$new_omega->[$row][$col]);
            $line = $line.' '.$str;
        }
        my $comment ='';
        $comment = '; '.$labels->[$row] if (defined $labels and scalar(@{$labels}) > $row and (defined $labels->[$row]));
        push(@record_lines,$line.$comment);
    }
    return \@record_lines;
}

sub set_model2_omega_blocks
{
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 },
        start_omega_record => {isa => 'Int', optional => 0},
        rescale => {isa => 'Bool', optional => 0},
        skip_etas => {isa => 'Int', optional => 0},
        covariate_covmatrix => {isa => 'ArrayRef', optional => 0},
        covariate_labels => {isa => 'ArrayRef', optional => 0},
    );
    my $model = $parm{'model'};
    my $start_omega_record = $parm{'start_omega_record'};
    my $rescale = $parm{'rescale'};
    my $skip_etas = $parm{'skip_etas'};
    my $covariate_covmatrix = $parm{'covariate_covmatrix'};
    my $covariate_labels = $parm{'covariate_labels'};

    my $covariate_size = scalar(@{$covariate_covmatrix});
    croak("too few labels") unless (scalar(@{$covariate_labels}) == $covariate_size);

    my @covariate_etanumbers = ();

    my @covariate_code = ();

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
                              omega_order => { isa => 'ArrayRef', optional => 0 },
    );

    my $output = $parm{'output'};
    my $omega_order = $parm{'omega_order'};

    my ($error,$message) = check_covstep(output => $output);
    return [] if $error;
    ui->print(category => 'frem',
              message => $message) if (length($message)>0);
    my $lower_covar  = $output-> get_single_value(attribute => 'covariance_matrix');

    unless (defined $lower_covar){
        croak("Trying get_covmatrix but the covariance matrix is undefined. Parsing error?\n");
    }

    my $covar = output::problem::subproblem::make_square($lower_covar);

    if (scalar(@{$omega_order})>0){
        #note that model itself may have been reordered, but output input problem has not
        my $original_strings = $output->problems->[0]->input_problem->
            get_estimated_attributes(attribute=>'coordinate_strings');

        #to be used for reordering covmatrix
        my $reordered_strings =    get_reordered_coordinate_strings(
            problem => $output->problems->[0]->input_problem,
            omega_order => $omega_order);

        return reorder_covmatrix(matrix => $covar,
                                 original_strings => $original_strings,
                                 reordered_strings => $reordered_strings);
    }else{
        return $covar;
    }

}

sub reorder_covmatrix
{
    my %parm = validated_hash(\@_,
                              original_strings => { isa => 'ArrayRef', optional => 0 },
                              reordered_strings => { isa => 'ArrayRef', optional => 0 },
                              matrix => { isa => 'ArrayRef', optional => 0 },
    );

    my $original_strings = $parm{'original_strings'};
    my $reordered_strings = $parm{'reordered_strings'};
    my $matrix = $parm{'matrix'};

    my $dimension = scalar(@{$matrix});
    unless ($dimension > 0){
        croak("matrix has size 0");
    }
    unless (scalar(@{$matrix->[0]}) == $dimension){
        croak("matrix is not square, dim $dimension but first row ".scalar(@{$matrix->[0]}));
    }
    unless (scalar(@{$original_strings}) == $dimension){
        croak("matrix has dimension $dimension but original strings is ".scalar(@{$original_strings}));
    }
    unless (scalar(@{$reordered_strings}) == $dimension){
        croak("matrix has dimension $dimension but reordered strings is ".scalar(@{$original_strings}));
    }

    my @mapping = ();
    for (my $i=0; $i< $dimension; $i++){
        for (my $j=0; $j< $dimension; $j++){
            if ($original_strings->[$j] eq $reordered_strings->[$i]){
                push(@mapping,$j);
                last;
            }
        }
    }
    unless (scalar(@mapping) == $dimension){
        croak("matrix has dimension $dimension but mappnig is ".scalar(@mapping));
    }

    my @newmatrix = ();
    for (my $i=0; $i< $dimension; $i++){
        push(@newmatrix,[(0) x $dimension]);
        for (my $j=0; $j< $dimension; $j++){
            $newmatrix[$i]->[$j] = $matrix->[($mapping[$i])]->[($mapping[$j])];
        }
    }

    return \@newmatrix;
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

sub    join_covmats
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
        );

    my $partial_outputs = $parm{'partial_outputs'};
    my $omega_orders = $parm{'omega_orders'};
    my $full_model = $parm{'full_model'};
    my $reordered_model1 = $parm{'reordered_model1'};
    my $directory = $parm{'directory'};
    my $filename = $parm{'filename'};
    my $rse = $parm{'rse'};

    $partial_outputs->[0]->load;
    $partial_outputs->[1]->load;

    my $full_strings=$full_model->problems->[0]->get_estimated_attributes(parameter => 'all',
                                                                          attribute => 'coordinate_strings');
    my $covmat1 = get_covmatrix(output => $partial_outputs->[0],
                                omega_order => $omega_orders->[0]);

    my $strings1 = $reordered_model1->problems->[0]->
        get_estimated_attributes(parameter => 'all',
                                 attribute => 'coordinate_strings'); #after possible reordering

    my $covmat2 = get_covmatrix(output => $partial_outputs->[1],
                                omega_order => []);
    my $strings2 = $partial_outputs->[1]->problems->[0]->input_problem->
        get_estimated_attributes(parameter => 'all',
                                 attribute => 'coordinate_strings');

    my $full_values = $full_model->outputs->[0]->get_filtered_values(category => 'estimate',
                                                                     parameter => 'all',
                                                                     problem_index => 0,
                                                                     subproblem_index => 0);
    unless (defined $full_values and scalar(@{$full_values})>0  and defined $full_values->[0]){
        $full_values = $full_model->problems->[0]->get_estimated_attributes(parameter => 'all',
                                                                            attribute => 'inits');
    }


    my $perfect_ids_hash = perfect_individuals(output1 => $partial_outputs->[0],
                                               omega_order1 => $omega_orders->[0],
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


    my ($posdefmatrix,$count)=linear_algebra::get_symmetric_posdef(matrix => $fullmat);

    my $formatted = tool::format_covmatrix(matrix => $posdefmatrix,
                                     header => $full_strings,
                                     comma => 0,
                                     print_labels => 1);
    open ( RES, ">" . $directory.$filename );
    foreach my $line (@{$formatted}){
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
                              omega_order1 => { isa => 'ArrayRef', optional => 0 },
                              output2 => { isa => 'output', optional => 0 },
    );

    my $output1 = $parm{'output1'};
    my $omega_order1 = $parm{'omega_order1'};
    my $output2 = $parm{'output2'};

    my ($error,$message) = check_covstep(output => $output1);

    my $mapping1 = get_eta_mapping( problem => $output1->problems->[0]->input_problem,
                                    omega_order => $omega_order1);

    ($error,$message) = check_covstep(output => $output2);
    my %hash=();

    my $is_output1=1;
    foreach my $output ($output1,$output2){
        my $hashref = $output->perfect_individual_count(); #can be empty
        foreach my $key (keys %{$hashref}){
            my $etanum = $key;
            if ($is_output1){
                unless (defined $mapping1->{$key}){
                    croak("mapping for ETA $key is undefined, only have for ".join(' ',(keys %{$mapping1})));
                }
                $etanum = $mapping1->{$key};
            }
            if (defined $hash{$etanum}){
                croak("perfect count for ETA $etanum already read from model 1, this is a coding error");
            }
            $hash{$etanum} = $hashref->{$key};
        }
        $is_output1=0;
    }
    return \%hash;
}

sub prepare_results
{
    my $self = shift;
    my $err = '';

    $logger->info("Preparing and printing results");
    my $directory = $self->directory;
    my $input_model = $self->models->[0];
    my $base_model = $self->reordered_model_1;
    my $model_2 = $self->model_2;
    my @final_models = @{$self->final_models};
    my @final_numbers = @{$self->final_numbers};
    my $cov_summary = $self->cov_summary;
    $logger->critical("no cov summary file known, this is a bug") unless (defined $cov_summary);

    # constants
    my $warn_perc_badest = 5;
    my $warn_perc_badest_missing = 10;

    # make sure old results file is not overwritten if re-run
    if (-e $self->directory.$self->results_file) {
        (my $filename = $self->results_file) =~ s/\.csv$//;
        my $num = 2;
        while (-e $self->directory."$filename.$num".'.csv'){
            $num++;
        }
        my $new_filename = "$filename.$num".'.csv';
        $self->results_file($new_filename);
        $logger->info("Old results file(s) exists, writing to '$new_filename' to avoid overwrite");
    }

    # check that final model is supported (4/4b only for now)
    if (scalar(@final_models) == 0 or scalar(@final_models) != scalar(@final_numbers)) {
        $logger->critical("No final models known (prepare_results), this is a bug"); die;
    }
    my ($full_model, $full_model_cov);
    for (my $i=0; $i<scalar(@final_models); $i++) {
        if ($final_numbers[$i] eq "4") {
            $full_model = $final_models[$i];
        } elsif ($final_numbers[$i] eq "4b") {
            $full_model_cov = $final_models[$i];
        }
    }
    unless (defined $full_model_cov) {
        $full_model_cov = $full_model;
    }

    # check that all models and outputs necessary exists
    if (!defined $base_model) {
        $logger->critical("Reordered base model does not exist (prepare_results), this is a bug"); die;
    } elsif (!defined $model_2) {
        $logger->critical("Model 2 does not exist (prepare_results), this is a bug"); die;
    } elsif (!defined $full_model) {
        $logger->critical("Full FREM (model 4) does not exist (prepare_results), can't continue"); die;
    }
    my ($base_model_output, $model_2_output, $full_model_output);
    if (defined $base_model->outputs and defined $base_model->outputs->[0]) {
        $base_model_output = $base_model->outputs->[0];
        $err = $base_model_output->load;
        return $err if ($err);
    } else {
        $logger->critical("No output base model (prepare_results), this is a bug"); die;
    }
    if (defined $model_2->outputs and defined $model_2->outputs->[0]) {
        $model_2_output = $model_2->outputs->[0];
        $err = $model_2_output->load;
        return $err if ($err);
    } else {
        $logger->critical("No output model 2 (prepare_results), this is a bug"); die;
    }
    if (defined $full_model->outputs and defined $full_model->outputs->[0]) {
        $full_model_output = $full_model->outputs->[0];
        $err = $full_model_output->load;
        return $err if ($err);
    } else {
        $logger->critical("No output model 4 (prepare_results), can't continue"); die;
    }

    # return section (input model name, date and versions)
	my %return_section;
	$return_section{'name'} = 'FREM run info';
	$return_section{'labels'} = [[],['Date','model','PsN version','NONMEM version']];
    my @datearr=localtime;
    my $the_date=($datearr[5]+1900).'-'.($datearr[4]+1).'-'.($datearr[3]);
    $return_section{'values'} = [[$the_date, $input_model->filename(), 'v'.$PsN::version, $self->nm_version]];
    push(@{$self->results->[0]{'own'}}, \%return_section);

    # space section (2 empty lines)
    my %space_section;
    $space_section{'name'}= '';
    $space_section{'labels'}= [];
    $space_section{'values'}= [[]];

    # model 1: base model (after possible reordering)
    my $base_coords = $base_model->problems->[0]->get_estimated_attributes(parameter => 'all', attribute => 'coordinate_strings');
    my $base_inits  = $base_model->problems->[0]->get_estimated_attributes(parameter => 'all', attribute => 'inits');
    my $base_values = $base_model->outputs->[0]->get_filtered_values(category => 'estimate',
                                                                      parameter => 'all',
                                                                      problem_index => 0,
                                                                      subproblem_index => 0);

    # model 2: covariate model
    my $m2_coords = $model_2->problems->[0]->get_estimated_attributes(parameter => 'all', attribute => 'coordinate_strings');
    my $m2_inits  = $model_2->problems->[0]->get_estimated_attributes(parameter => 'all', attribute => 'inits');
    my $m2_values = $model_2->outputs->[0]->get_filtered_values(category => 'estimate',
                                                                 parameter => 'all',
                                                                 problem_index => 0,
                                                                 subproblem_index => 0);
    my $m2_covmat  = get_covmatrix(output => $model_2_output, omega_order => []);

    # final FREM model
    my $full_coords = $full_model->problems->[0]->get_estimated_attributes(parameter => 'all', attribute => 'coordinate_strings');
    my $full_inits  = $full_model->problems->[0]->get_estimated_attributes(parameter => 'all', attribute => 'inits');
    my $full_labels = $full_model->problems->[0]->get_estimated_attributes(parameter => 'all', attribute => 'labels');
    my $full_values = $full_model->outputs->[0]->get_filtered_values(category => 'estimate',
                                                                     parameter => 'all',
                                                                     problem_index => 0,
                                                                     subproblem_index => 0);
    my $full_has_estimates = 0;
    if (defined $full_values and scalar(@{$full_values})>0 and defined $full_values->[0]) {
        $full_has_estimates = 1;
    }

    # raw estimates section
    my %est_section;
    $est_section{'name'}='Raw inits & estimates';
    my @est_header = ("type", @{$full_labels});
    $est_section{'labels'}=[
        [ ("M1 (base model)") x 2,
          ("M2 (covariates)") x 2,
          ("M4 (full FREM)") x 2,
        ], \@est_header,
    ];
    my $params = [
        ["init"], ["estimate"],
        ["init"], ["estimate"],
        ["init"], ["estimate"],
    ];
    for (my $i=0; $i<scalar(@{$full_coords}); $i++) {
        my $coord = $full_coords->[$i];
        # base model
        my ($base_init, $base_est);
        for (my $j=0; $j<scalar(@{$base_coords}); $j++) {
            if ($base_coords->[$j] eq $coord) {
                $base_init = $base_inits->[$j];
                $base_est = $base_values->[$j];
                last;
            }
        }
        push(@{$params->[0]}, $base_init);
        push(@{$params->[1]}, $base_est);
        # model 2
        my ($m2_init, $m2_est);
        for (my $j=0; $j<scalar(@{$m2_coords}); $j++) {
            if ($m2_coords->[$j] eq $coord) {
                $m2_init = $m2_inits->[$j];
                $m2_est = $m2_values->[$j];
                last;
            }
        }
        push(@{$params->[2]}, $m2_init);
        push(@{$params->[3]}, $m2_est);
        # final model
        my $full_init = $full_inits->[$i];
        my $full_est = $full_has_estimates ? $full_values->[$i] : '';
        push(@{$params->[4]}, $full_init);
        push(@{$params->[5]}, $full_est);
    }
    $est_section{'values'} = $params;
    push(@{$self->results->[0]{'own'}}, \%est_section);
    push(@{$self->results->[0]{'own'}}, \%space_section);

    # Base model compared to FREM model
    my %basechange_section;
    $basechange_section{'name'}='Base vs. FREM model';
    if ($full_has_estimates) {
        $basechange_section{'labels'}=[ ["base parameter change (%) by M4"], [] ];
        $basechange_section{'values'}=[ [] ];
        # check base parameter move (found new minima?)
        for (my $i=0; $i<scalar(@{$full_coords}); $i++) {
            my $base_est = $params->[1]->[1+$i];
            my $full_est = $params->[5]->[1+$i];
            if (defined $base_est) {
                my $label = $full_labels->[$i];
                my $perc_deviance = ($base_est/$full_est)*100-100;
                if (abs($perc_deviance) > $warn_perc_badest) {
                    $logger->warning("FREM est of parameter differ by ".neat_num(num=>$perc_deviance,sig=>3,plus=>1).
                                     "% from base model ($label)");
                }
                push @{$basechange_section{'labels'}->[1]}, $label;
                push @{$basechange_section{'values'}->[0]}, $perc_deviance;
            }
        }
    }
    push(@{$self->results->[0]{'own'}}, \%basechange_section);
    push(@{$self->results->[0]{'own'}}, \%space_section);

    # get FREM post-processing data
    my ($cov_names,$cov_rescale,$omegaindex,$par_names,$size,$emeans,$evars) = get_post_processing_data(model => $full_model);
    my @cov_rescale = @{$cov_rescale};
    my @cov_names = @{$cov_names};
    my @par_names = @{$par_names};
    my @estcov_means = @{$emeans};
    my @estcov_var = @{$evars};
    my @estcov_sd;
    foreach my $var (@estcov_var) {
        push @estcov_sd, sqrt($var);
    }
    my $npar = scalar(@par_names);
    my $ncov = scalar(@cov_names);
    my @rescale = (1) x $npar;
    push(@rescale,@cov_rescale);

    # get covariate post-processing data (and warn if off)
    my $covdata = read_covresults(covnames => $cov_names, filename => $cov_summary);
    unless (defined $covdata) {
        $logger->critical("covariate summary read error, can't continue"); die;
    }
    my @cov_means = @{ $covdata->{'invariant_mean'} };
    my @cov_var = @{ $covdata->{'invariant_variance'} };
    my @cov_sd = @{ $covdata->{'invariant_stdev'} };
    my @has_missingness = @{ $covdata->{'has_missingness'} };
    my (@perc_deviance_means, @perc_deviance_sd);
    for (my $i=0; $i<$ncov; $i++) {
        push @perc_deviance_means, ($estcov_means[$i]/$cov_means[$i])*100-100;
        push @perc_deviance_sd, ($estcov_sd[$i]/$cov_sd[$i])*100-100;
        my $perc_deviance_crit = $warn_perc_badest;
        my $missing_info = "";
        if ($has_missingness[$i]) {
            $perc_deviance_crit = $warn_perc_badest_missing;
            $missing_info = ", has missingness";
        }
        if (abs($perc_deviance_means[$i]) > $perc_deviance_crit) {
            $logger->warning("FREM est of covariate mean differ by ".neat_num(num=>$perc_deviance_means[$i],sig=>3,plus=>1).
                             "% from empirical value ($cov_names[$i]$missing_info)");
        }
        if (abs($perc_deviance_sd[$i]) > $perc_deviance_crit) {
            $logger->warning("FREM est of covariate SD differ by ".neat_num(num=>$perc_deviance_sd[$i],sig=>3,plus=>1).
                             "% from empirical value ($cov_names[$i]$missing_info)");
        }
    }

    # covariates section
    my %cov_section;
    $cov_section{'name'} = 'Covariates';
    my @cov_header = ("source", @cov_names);
    $cov_section{'labels'} = [
        [ "mean", "mean",
          "stdev", "stdev",
        ], \@cov_header,
    ];
    $cov_section{'values'} = [
        [ "data", @cov_means ], [ "estimate", @estcov_means ],
        [ "data", @cov_sd ], [ "estimate", @estcov_sd ],
    ];
    push(@{$self->results->[0]{'own'}}, \%cov_section);
    push(@{$self->results->[0]{'own'}}, \%space_section);

    # (cond) coefficients and covar section
    my (%coeff_section, %covar_section);
    $coeff_section{'name'}='FREM parameter-covariate coefficients';
    $covar_section{'name'}='FREM parameter (unexplained) variability';
    $coeff_section{'labels'}=[ [], ["cond on cov", "", @cov_names] ];
    $covar_section{'labels'}=[ [], ["cond on cov", "", @par_names] ];
    my $varcov;
    if ($full_has_estimates) {
        my ($error, $covar, $coeff);
        $varcov = $full_model->problems->[0]->omegas->[$omegaindex]->get_matrix;
        # get coeffs and cond variability, multiconditional (cond on all)
        ($error,$covar,$coeff) = linear_algebra::conditional_covariance_coefficients(varcov => $varcov,
                                                                                     rescaling => \@rescale,
                                                                                     cov_index_first => $npar,
                                                                                     cov_index_last => ($size-1),
                                                                                     par_index_first => 0,
                                                                                     par_index_last => ($npar-1));

        if ($error) {
            $logger->error("Numerical error: (cond all) coefficients/variability from M4 omega mat");
        } else {
            for (my $par=0; $par<scalar(@{$par_names}); $par++) {
                # fill multiconditional coefficients and variance for each par
                my $par_name = $par_names->[$par];
                push(@{$coeff_section{'labels'}->[0]}, "");
                push(@{$covar_section{'labels'}->[0]}, "");
                push(@{$coeff_section{'values'}}, ["all", $par_name, @{$coeff->[$par]}]);
                push(@{$covar_section{'values'}}, ["all", $par_name, @{$covar->[$par]}]);
            }
            foreach my $par_name (@par_names) {
                # prepare uniconditional coefficient fill by initializing $npar rows
                push(@{$coeff_section{'labels'}->[0]}, "");
                push(@{$coeff_section{'values'}}, ["each", $par_name]);
            }
            for (my $cov=0; $cov<scalar(@{$cov_names}); $cov++) {
                my $cov_name = $cov_names->[$cov];
                # get coeffs and cond variability, uniconditional (cond on each cov separetely)
                ($error,$covar,$coeff) = linear_algebra::conditional_covariance_coefficients(varcov => $varcov,
                                                                                             rescaling => \@rescale,
                                                                                             cov_index_first => $npar+$cov,
                                                                                             cov_index_last => $npar+$cov,
                                                                                             par_index_first => 0,
                                                                                             par_index_last => ($npar-1));
                if ($error) {
                    $logger->error("Numerical error: (cond $cov_name) coefficients/variability from M4 subset omega mat");
                } else {
                    for (my $par=0; $par<scalar(@{$par_names}); $par++) {
                        # fill uniconditional coefficients and variance for each par (note: only one coeff per cov)
                        my $par_name = $par_names->[$par];
                        push(@{$coeff_section{'values'}->[$par+$npar]}, $coeff->[$par]->[0]);
                        push(@{$covar_section{'labels'}->[0]}, "");
                        push(@{$covar_section{'values'}}, [$cov_name, $par_name, @{$covar->[$par]}]);
                    }
                }
            }
        }
        push(@{$self->results->[0]{'own'}}, \%coeff_section);
        push(@{$self->results->[0]{'own'}}, \%covar_section);

        # get final covariance matrix (if available)
        my ($full_has_covmat, $msg) = check_covstep(output => $full_model_cov->outputs->[0]);
        my $full_covmat = get_covmatrix(output => $full_model_cov->outputs->[0], omega_order => []) unless ($full_has_covmat);
        my $posdef_err = tool::sir::check_matrix_posdef(matrix => $full_covmat);
    }

    return $err;
}

sub neat_num
{
    # format number with 4 significant digits and sci notation outside [0.001,10000)
    my %parm = validated_hash(\@_,
          num => {isa => 'Num', optional => 0},
          sig => {isa => 'Int', optional => 1, default => 4},
          plus => {isa => 'Bool', optional => 1, default => 0}, # always show + sign
    );
    my $num = $parm{'num'};
    my $sig = $parm{'sig'};
    my $always_plus = $parm{'plus'};
    if ($sig <=0 ) {
        croak("sig must be non-zero & positive");
    }
    my $lownum   = 10**(1-$sig); # 0.001 for sig=4
    my $highnum  = 10**($sig); # 10000 for sig=4
    my $include_lownum = 1;
    my $include_highnum = 0;

    return 0 if ($num == 0);
    # determine if scientific notation shall be used
    my $sci_notation = 0;
    my $low_crit  = log(abs($num))/log(10) - log($lownum);
    my $high_crit = log($highnum) - log(abs($num))/log(10);
    $sci_notation = 1 if( $low_crit < 0 || ($include_lownum && $low_crit == 0) );
    $sci_notation = 1 if( $high_crit < 0 || ($include_highnum && $high_crit == 0) );

    if ($sci_notation) {
        my $decimals = $sig-1;
        $num = sprintf("%.${decimals}E", $num);
    } else {
        # determine number of decimal places to keep for sig figs
        my $decimals = -( floor( log(abs($num))/log(10) ) - ($sig-1) );
        $decimals = ($decimals < 0) ? 0 : $decimals;
        $num = sprintf("%.${decimals}f", $num);
    }
    if ($always_plus and $num >= 0) {
        $num = "+$num";
    }
    return $num
}

sub do_model1
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         model => { isa => 'Ref', optional => 0 }
    );
    my $model = $parm{'model'};

    my $name_model = $name_model_1;
    my $output;
    my $frem_model;
    my $need_update = 0;

    my $etas_file = $model->get_or_set_etas_file(problem_number => 1); # absolute path if present

    my $im_dir = $self->directory().'intermediate_models/';
    if (-e $self -> directory().'intermediate_models/'.$name_model) {
        $frem_model = model->new( %{common_options::restore_options(@common_options::model_options)},
                                  filename                    => $im_dir.$name_model,
                                  parse_output => 0,
                                  ignore_missing_output_files => 1 );
        (my $phi_filename = $name_model) =~ s/(.*)\..*/$1.phi/;
        $etas_file = $im_dir.$phi_filename;
    }else{
        $frem_model = $model ->  copy( filename    => $self -> directory().'intermediate_models/'.$name_model,
                                       output_same_directory => 1,
                                       write_copy => 1,
                                       copy_datafile   => 0,
                                       copy_output => 0);

        # copy base model lst, cov and ext to intermediate models (postfrem needs them)
        my $model_dir = $model->directory();
        my $model_filename = $model->filename();
        my @extra_extensions = (".lst", ".cov", ".ext", ".phi");
        foreach my $ext (@extra_extensions) {
            (my $extra_file_orig = $model_filename) =~ s/\..*$/$ext/;
            (my $extra_file_copy = $name_model) =~ s/\..*$/$ext/;
            if (-f $model_dir.$extra_file_orig) {
                cp($model_dir.$extra_file_orig, $im_dir.$extra_file_copy);
            }
        }

        # copy etas file and update model to new location
        if ($etas_file) {
            unless (-f $etas_file) {
                croak "\$ETAS FILE=$etas_file could not be read"
            }

            # copy file to intermediate_models
            cp($etas_file, $im_dir);

            # update etas file in model to new path (just filename since same directory)
            my (undef, undef, $etas_filename) = File::Spec->splitpath($etas_file);
            $frem_model->get_or_set_etas_file(problem_number => 1, new_file => $etas_filename);

            # update etas file to model 1 output (for downstream model 2 usage)
            (my $phi_filename = $name_model) =~ s/(.*)\..*/$1.phi/;
            $etas_file = $im_dir.$phi_filename;
        }
    }

    if ($frem_model -> is_run() and (defined $frem_model->outputs->[0] )
        ) {
        #no need to run again
        $output = $frem_model->outputs->[0];
    }elsif ($model -> is_run() and (defined $model->outputs->[0] )
            #and
            #(-e $model->outputs->[0]->problems->[0]->full_name_NM7_file(file_type => 'phi'))
        ) {
        #no need to run anything
        $output = $model->outputs->[0];
    }else{
        #run it
        my $rundir = $self -> directory().'/model1_modelfit_dir1';
        rmtree([ "$rundir" ]) if (-e $rundir);
        my $run = tool::modelfit ->new( %{common_options::restore_options(@common_options::tool_options)},
                                        base_directory     => $self -> directory(),
                                        directory         => $rundir,
                                        copy_data     => 1,
                                        models         => [$frem_model],
                                        top_tool              => 0);
        $run->add_to_nmoutput(extensions => ['phi','ext','cov']);
        ui -> print( category => 'all', message =>  'Estimating Model 1 (the input model)');
        $run-> run;
        if (defined $frem_model->outputs and (defined $frem_model->outputs->[0])){
            $output = $frem_model->outputs->[0] ;
        }
        $need_update = 1;
    }

    unless (defined $output){
        croak("No output from Model 1, cannot proceed with frem");
    }

    $frem_model->update_inits (from_output => $output);

    return ($frem_model,$output,$need_update,$etas_file);

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

    my ($filtered_data_model,$data_set_headers,$extra_input_items,$message) =
        create_data2_model(model=>$model,
                           filename => $self -> directory().'intermediate_models/filter_data_model.mod',
                           filtered_datafile => $filtered_datafile,
                           use_pred => $self->use_pred,
                           dv => $self->dv,
                           covariates => $self->covariates);

    $self->extra_input_items($extra_input_items);

    unless (-e $self -> directory().'intermediate_models/'.$filtered_datafile){
        $filtered_data_model -> _write();
        my $rundir = $self -> directory().'/create_fremdata_dir';
        rmtree([ "$rundir" ]) if (-e $rundir);
        my $filter_fit = tool::modelfit -> new
            ( %{common_options::restore_options(@common_options::tool_options)},
              base_directory => $self->directory,
              directory      => $rundir,
              models         => [$filtered_data_model],
              top_tool       => 0,
              copy_data      => 1,
              clean => 2  );
        ui -> print( category => 'all',
                     message  => $message,
                     newline => 1 );
        $filter_fit -> run;
    }

    my $filtered_data = data->new(filename => $filtered_data_model->directory.$filtered_datafile,
                                  ignoresign => '@',
                                  idcolumn => $model->idcolumns->[0],
                                  missing_data_token => $self->missing_data_token);

    my $indices = get_indices(target => $data_set_headers,
                              keys => ['EVID','MDV',$fremtype,$self->dv]);

    my @cov_indices = ();
    my @is_log = ();
    my @cov_names = ();

    if (scalar(@{$self->log}) > 0){
        #we assume all found already, error check in createdata2model
        my $log_indices = array::get_positions(target => $data_set_headers,
                                               keys=> $self->log);

        my @new_log =();
        foreach my $cov (@{$self->log}){
            push(@new_log,'LN'.$cov);
        }
        $self->log(\@new_log);
        push(@cov_indices,@{$log_indices});
        push(@cov_names,@new_log);
        push(@is_log,(1) x scalar(@new_log));
    }

    if (scalar(@{$self->regular}) > 0){
        my $regular_indices = array::get_positions(target => $data_set_headers,
                                                   keys=> $self->regular);
        push(@cov_indices,@{$regular_indices});
        push(@cov_names,@{$self->regular});
        push(@is_log,(0) x scalar(@{$self->regular}));
    }

    if (scalar(@{$self->categorical}) > 0){
        my $categorical_indices = array::get_positions(target => $data_set_headers,
                                                       keys=> $self->categorical);
        my @mdv_evid_indices =();
        push(@mdv_evid_indices,$indices->{'MDV'}) if (defined $indices->{'MDV'});
        push(@mdv_evid_indices,$indices->{'EVID'}) if (defined $indices->{'EVID'});
        my ($mapping,$new_indices,$new_categorical,$warn_multiple) =
            $filtered_data->append_binary_columns(indices => $categorical_indices,
                                                  baseline_only => 1,
                                                  mdv_evid_indices => \@mdv_evid_indices,
                                                  start_header => $data_set_headers);
        if (scalar(@{$warn_multiple})>0){
            ui -> print( category => 'all',
                         message => "\nWarning: Individuals were found to have multiple values in the ".join(' ',@{$warn_multiple}).
                         " column(s),".
                         " but the frem script will just use the first value for the individual.\n");
        }

        $categorical_indices = $new_indices;
        $self->categorical($new_categorical); #these are now binary
        push(@cov_indices,@{$categorical_indices});
        push(@cov_names,@{$new_categorical});
        push(@is_log,(0) x scalar(@{$new_categorical}));
    }

    $self->covariates(\@cov_names);

    $indices->{'cov_indices'} = \@cov_indices;
    $indices->{'is_log'} = \@is_log;

    return ($filtered_data,$indices);
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
    if (-e $self -> directory().$fremdataname){
        unlink($self -> directory().$fremdataname);
        $do_check = 0; #assume get same result second time
    }

    #this writes dataset to disk
    my $resultref = data::frem_compute_covariate_properties(filtered_data  => $filtered_data,
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
                                                            cov_indices => $indices->{'cov_indices'});

    if ($do_check){
        my $name_check_model = 'check_data.mod';
        my $data_check_model = $model ->  copy( filename    => $self -> directory().'intermediate_models/'.$name_check_model,
                                                output_same_directory => 1,
                                                write_copy => 0,
                                                copy_datafile   => 0,
                                                copy_output => 0);

        # have filtered data so can skip old accept/ignores. Need ignore=@ since have a header
        #have only one $PROB by input check
        $data_check_model->datafiles(problem_numbers => [1],
                                     new_names => [$self -> directory().$fremdataname]);
        $data_check_model->problems->[0]->datas->[0]->ignoresign('@');
        $data_check_model->remove_option( record_name => 'data',
                                          option_name => 'ACCEPT',
                                          fuzzy_match => 1);
        $data_check_model->set_option( record_name => 'data',
                                       option_name => 'IGNORE',
                                       option_value => '('.$fremtype.'.GT.0)',
                                       fuzzy_match => 1);

        foreach my $input (@{$data_check_model->problems->[0]->inputs}){
            $input->remove_drop_column_names;
        }

        foreach my $item (@{$self->extra_input_items()}){
            $data_check_model -> add_option(problem_numbers => [1],
                                            record_name => 'input',
                                            option_name => $item);
        }
        $data_check_model ->_write(overwrite => 1);

        my $rundir = $self -> directory().'/datacheck_modelfit_dir1';
        rmtree([ "$rundir" ]) if (-e $rundir);
        my $run = tool::modelfit ->new( %{common_options::restore_options(@common_options::tool_options)},
                                        base_directory     => $self -> directory(),
                                        directory         => $rundir,
                                        copy_data     => 0,
                                        models         => [$data_check_model],
                                        top_tool              => 0);

        $run->add_to_nmoutput(extensions => ['ext']);
        ui -> print( category => 'all', message => 'Running data check model');
        $run -> run;
        #compare ofv. print this to log file
        my $check_ofv = 'undefined';
        if ($data_check_model->is_run()){
            $check_ofv = $data_check_model->outputs -> [0]->get_single_value(attribute=> 'ofv');
        }
        print "\nModel 1 ofv is    $mod1_ofv\n";
        print   "Data check ofv is $check_ofv\n";
    }
    return $resultref;
}

sub get_covrecord
{
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
        );
    my $model = $parm{'model'};

    my $covrecordref=[];
    if (defined $model->problems->[0]->covariances and scalar(@{$model->problems->[0]->covariances})>0){
        $covrecordref = $model->problems->[0]->covariances->[0] -> _format_record() ;
        for (my $i=0; $i<scalar(@{$covrecordref}); $i++){
            $covrecordref->[$i] =~ s/^\s*\$CO[A-Z]*\s*//; #get rid of $COVARIANCE
            $covrecordref->[$i] =~ s/\s*$//; #get rid of newlines
        }
    }else{
        $covrecordref = ['PRINT=R,E UNCONDITIONAL'];
    }
    return $covrecordref;
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

    my @pkcode=();
    my @pred_error_code = (';;;FREM CODE BEGIN COMPACT',';;;DO NOT MODIFY');

    my @eta_labels=();
    my @eta_strings=();
    my @rescale_strings=();
    my @rescale_factors=();

    for (my $j=0; $j< scalar(@{$covariates}); $j++){
        my $label = 'BSV_'.$covariates->[$j];
        my $etanum = ($maxeta+1+$j);
        my $sd = '1';
        if ($rescale) {
            $sd = sprintf("%.12G",sqrt($invariant_covmatrix->[$j][$j]));
        }
        push(@rescale_factors,$sd);
        push(@rescale_strings,'SDC'.$etanum);
        push(@eta_strings, ['ETA('.$etanum.')']);
        push(@eta_labels, $label);
    }

    my $newtheta = 0;

    my @theta_record_strings =();
    my @theta_strings = ();
    for (my $i=0; $i< scalar(@{$covariates}); $i++){
        my $thetalabel = 'TV_'.$covariates->[$i];
        my $val=$invariant_mean->[$i];
        my $fixed = '';
        if ($estimate_mean->[$i]){
            $val=0.001 if ($val==0);
        }else{
            # #can be 0 since FIXed
            $fixed = ' FIX';
        }
        push(@theta_record_strings,' '.sprintf("%.12G",$val).$fixed.' ; '.$thetalabel);
        $newtheta++;
        my $num = ($ntheta+$newtheta);
        push(@theta_strings,'THETA('.$num.')');
    }

    my @code;
    if ($rescale) {
        my @rescalecode=();
        for (my $j=0; $j< scalar(@{$covariates}); $j++) {
            push(@rescalecode,$indent.$rescale_strings[$j].' = '.$rescale_factors[$j]);
        }
        push(@code,@rescalecode);
    }
    if ($mu) {
        #PK/PRED changes for mu modelling
        my @mucode=();
        for (my $j=0; $j< scalar(@{$covariates}); $j++){
            my $etanum = ($maxeta+1+$j);
            if ($rescale) {
                push(@mucode,$indent.'MU_'.$etanum.' = '.$theta_strings[$j].'/SDC'.$etanum);
                push(@mucode,$indent.'COV'.$etanum.' = (MU_'.$etanum.' + '.$eta_strings[$j]->[0].')*SDC'.$etanum);
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


    return (\@eta_labels,\@theta_record_strings, \@pred_error_code,\@pkcode);

}

sub prepare_model2
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              skip_etas => {isa => 'Int', optional => 0},
                              fremdataname => { isa => 'Str', optional => 0 },
                              start_omega_record => { isa => 'Int', optional => 0 },
                              invariant_mean => { isa => 'ArrayRef', optional => 0 },
                              invariant_covmatrix => { isa => 'ArrayRef', optional => 0 },
                              update_existing_model_files => { isa => 'Bool', optional => 0 },
                              etas_file => { isa => 'Maybe[Str]', optional => 0 }
    );
    my $model = $parm{'model'};
    my $fremdataname = $parm{'fremdataname'};
    my $start_omega_record = $parm{'start_omega_record'};
    my $skip_etas = $parm{'skip_etas'};
    my $invariant_mean = $parm{'invariant_mean'};
    my $invariant_covmatrix = $parm{'invariant_covmatrix'};
    my $update_existing_model_files = $parm{'update_existing_model_files'};
    my $etas_file = $parm{'etas_file'};

    my $name_model = $name_model_2;

    my $frem_model;
    my $maxeta =  $model->problems()->[0]->nomegas(with_correlations => 0,
                                                   with_same => 1);

    my $ntheta = $model ->nthetas(problem_number => 1);
    my $epsnum = 1 + $model->problems()->[0]->nsigmas(with_correlations => 0,
                                                      with_same => 1);

    my @estimate_mean = ();
    if ($self->estimate_means){
        if (scalar(@{$self->has_missingness}) == scalar(@{$self->covariates})){
            @estimate_mean = @{$self->has_missingness};
        }else{
            croak("No information about missing covariate values, this is a bug");
        }
    }else{
        @estimate_mean = (0) x scalar(@{$self->covariates});
    }
    my ($etalabels,$theta_strings,$pred_error_code,$pk_code) = get_pred_error_pk_code(covariates => $self->covariates,
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
                                                                                      indent => $indentation);

    cleanup_outdated_model(modelname => $self -> directory().'intermediate_models/'.$name_model,
                           need_update => $update_existing_model_files);

    # do estimation record changes even if this is a restart, to save records pre-set_maxeval_zero (for model 3 generation)
    $frem_model = $model->copy(filename    => $self -> directory().'intermediate_models/'.$name_model,
                               output_same_directory => 1,
                               write_copy => 0,
                               copy_datafile   => 0,
                               copy_output => 0);
    if ($frem_model->problems->[0]->estimations->[-1]->is_classical){
            if ((($PsN::nm_major_version == 7) and ($PsN::nm_minor_version > 2)) or ($PsN::nm_major_version > 7)){
                    $frem_model->problems->[0]->estimations->[-1]->remove_option(name => 'NONINFETA', fuzzy_match => 1);
                    $frem_model->problems->[0]->estimations->[-1]->_add_option(option_string => 'NONINFETA=1');
            }
    }
    if ($self->mceta > 0){
            #input checking that mceta ok NM version and est method
            $frem_model->problems->[0]->estimations->[-1]->remove_option(name => 'MCETA', fuzzy_match => 1);
            $frem_model->problems->[0]->estimations->[-1]->_add_option(option_string => 'MCETA='.$self->mceta);
    }
    my $est_records = $frem_model->problems->[0]->estimations;

    my $im_dir = $self->directory().'intermediate_models/';
    unless (-e $im_dir.$name_model) {
        # input model  inits have already been updated
        #omegas have been reordered

        # if $ETAS FILE= used, M2 needs modified file with new omegas (initialized to 0)
        if ($etas_file) {
            unless (-f $etas_file) {
                croak "\$ETAS file $etas_file could not be read for model 2, this is a bug";
            }
            # load etas file and get number of new ETAs
            my $phi = phitable->new(path => $etas_file);
            my $num_new_etas = scalar(@{$self->covariates});

            # add new ETAs and construct new filename and path
            $phi->add_zero_etas(num_etas => $num_new_etas);
            ($etas_file = $name_model) =~ s/(.*)\..*/$1_input.phi/;
            my (undef, $etas_filename) = OSspecific::absolute_path($im_dir, $etas_file);

            # update FILE in model to new path (just filename since same directory) and write file to disk
            $frem_model->get_or_set_etas_file(problem_number => 1, new_file => $etas_filename);
            $phi->write(path => $im_dir.$etas_filename);

            # update etas file to model 2 output (for downstream model 3 usage)
            (my $phi_filename = $name_model) =~ s/(.*)\..*/$1.phi/;
            $etas_file = $im_dir.$phi_filename;
        }

        #DATA changes
        #we want to clear all old options from DATA
        $frem_model->problems->[0]->datas->[0]->options([]);
        $frem_model->problems->[0]->datas->[0]->ignoresign('@');
        $frem_model->datafiles(problem_numbers => [1],
                               new_names => [$self -> directory().$fremdataname]);

        #INPUT changes
        #remove names of DROP items, in case have special meaning like DATE=DROP
        foreach my $input (@{$frem_model->problems->[0]->inputs}){
            $input->remove_drop_column_names;
        }

        foreach my $item (@{$self->extra_input_items}){
            #mdv and fremtype
            $frem_model -> add_option(problem_numbers => [1],
                                      record_name => 'input',
                                      option_name => $item);
            #we do not have to add for example binary-ized categoricals, they enter in DV col for special fremtype
        }


        #SIGMA changes
        if (defined $frem_model->problems->[0]->sigmas) {
            foreach my $record (@{$frem_model-> problems -> [0]->sigmas}){
                if ($record->is_block){
                    $record->fix(1) unless ($record->same);
                }else{
                    for (my $j=0; $j< scalar(@{$record->options}); $j++){
                        $record->options->[$j]->fix(1);
                    }
                }
            }
        }

        $frem_model->add_records(type => 'sigma',
                                 problem_numbers => [1],
                                 record_strings => [$smallnum.' FIX ; EPSCOV']);

        set_model2_omega_blocks(model => $frem_model,
                                start_omega_record => $start_omega_record,
                                skip_etas => $skip_etas,
                                rescale => $self->rescale,
                                covariate_covmatrix => $invariant_covmatrix,
                                covariate_labels => $etalabels);

        #THETA changes
        #FIX all existing
        if (defined $frem_model->problems->[0]->thetas){
            for (my $i=0; $i< scalar(@{$frem_model->problems->[0]->thetas}); $i++){
                for (my $j=0; $j< scalar(@{$frem_model->problems->[0]->thetas->[$i]->options}); $j++){
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
                scalar(@{$frem_model->problems->[0]->covariances})>0){
            $frem_model->problems->[0] -> add_records( record_strings => ['PRINT=R UNCONDITIONAL'],
                                                       type => 'covariance' );
        }

        my $totaletas = $frem_model->problems()->[0]->nomegas(with_correlations => 0,
                                                              with_same => 1);
        if ($totaletas > $self->deriv2_nocommon_maxeta){
            if (defined $frem_model->problems()->[0]->abbreviateds and scalar(@{$frem_model->problems()->[0]->abbreviateds})>0){
                unless ($frem_model->problems()->[0]->is_option_set(name => 'DERIV2',
                                                                    record => 'abbreviated',
                                                                    fuzzy_match => 1)){
                    $frem_model->set_option(option_name => 'DERIV2',
                                            record_name => 'abbreviated',
                                            option_value => 'NOCOMMON',
                                            problem_numbers => [1],
                                            fuzzy_match => 1);
                }
            }else{
                $frem_model->problems->[0] -> set_records( record_strings => ['DERIV2=NOCOMMON'],
                                                           type => 'abbreviated' );
            }
        }

        my $message = $frem_model->check_and_set_sizes('all' => 1);
        if (length($message)>0){
            ui -> print( category => 'all', message =>  $message.' However this NONMEM version does not support $SIZES. '.
                         'There may be NMtran errors when running the model');
        }

        unless ($self->estimate_covariates) {
            $frem_model->set_maxeval_zero(print_warning => 1,
                                          last_est_complete => $self->last_est_complete,
                                          niter_eonly => $self->niter_eonly,
                                          need_ofv => 0);
        }

        $frem_model->_write();
    } else {
        (my $phi_filename = $name_model) =~ s/(.*)\..*/$1.phi/;
        $etas_file = $im_dir.$phi_filename;
    }

    return ($est_records,$ntheta,$epsnum,$etas_file);
}

sub prepare_model3
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                                  model => { isa => 'model', optional => 0 },
                                  start_omega_record => { isa => 'Int', optional => 0 },
                                  parcov_blocks => { isa => 'ArrayRef', optional => 0 },
                                  update_existing_model_files => { isa => 'Bool', optional => 0 },
                                  est_records => { isa => 'ArrayRef', optional => 0 },
                                  etas_file => { isa => 'Maybe[Str]', optional => 0 });
    my $model = $parm{'model'};
    my $start_omega_record = $parm{'start_omega_record'};
    my $parcov_blocks = $parm{'parcov_blocks'};
    my $update_existing_model_files = $parm{'update_existing_model_files'};
    my $est_records = $parm{'est_records'};
    my $etas_file = $parm{'etas_file'};

    my $modnum=3;

    my $name_model = $name_model_3;
    my $frem_model;
    my $covrecordref=[];
    if (defined $model->problems->[0]->covariances and scalar(@{$model->problems->[0]->covariances})>0){
        $covrecordref = $model->problems->[0]->covariances->[0] -> _format_record() ;
        for (my $i=0; $i<scalar(@{$covrecordref}); $i++){
            $covrecordref->[$i] =~ s/^\s*\$CO[A-Z]*\s*//; #get rid of $COVARIANCE
            $covrecordref->[$i] =~ s/\s*$//; #get rid of newlines
        }
    }

    cleanup_outdated_model(modelname => $self -> directory().'intermediate_models/'.$name_model,
                           need_update => $update_existing_model_files);

    my $im_dir = $self->directory().'intermediate_models/';
    unless (-e $im_dir.$name_model) {
        # input model  inits have already been updated
        $frem_model = $model ->  copy( filename    => $self -> directory().'intermediate_models/'.$name_model,
                                       output_same_directory => 1,
                                       write_copy => 0,
                                       copy_datafile   => 0,
                                       copy_output => 0);

        # if $ETAS FILE= used, M3 needs M2 phi output
        if ($etas_file) {
            unless (-f $etas_file) {
                croak "\$ETAS file $etas_file could not be read for model 3, this is a bug";
            }

            # copy M2 output to M3 input phi file
            (my $etas_filename = $name_model) =~ s/(.*)\..*/$1_input.phi/;
            cp($etas_file, $im_dir.$etas_filename);
            (undef, $etas_filename) = OSspecific::absolute_path($im_dir, $etas_filename);

            # update FILE in model to new path (just filename since same directory)
            $frem_model->get_or_set_etas_file(problem_number => 1, new_file => $etas_filename);

            # update etas file to model 3 output (for downstream model 4 usage)
            (my $phi_filename = $name_model) =~ s/(.*)\..*/$1.phi/;
            $etas_file = $im_dir.$phi_filename;
        }

        my @omega_records = ();
        for (my $i=0; $i< ($start_omega_record-1);$i++){
            #if start_omega_record is 1 we will push nothing
            push(@omega_records,$frem_model-> problems -> [0]->omegas->[$i]);
        }

        for (my $i=0; $i< scalar(@{$parcov_blocks}); $i++){
            push(@omega_records,$parcov_blocks->[$i]);
        }

        $frem_model -> problems -> [0]-> omegas(\@omega_records);
        $frem_model -> set_maxeval_zero(print_warning => 1,
                                   last_est_complete => $self->last_est_complete,
                                   niter_eonly => $self->niter_eonly,
                                   need_ofv => 0);

        $frem_model->problems->[0] -> remove_records(type => 'covariance' );

        $frem_model->_write();
    } else {
        (my $phi_filename = $name_model) =~ s/(.*)\..*/$1.phi/;
        $etas_file = $im_dir.$phi_filename;
    }

    return ($est_records,$covrecordref,$etas_file);

}

sub prepare_model4
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              imp_covariance_eval => { isa => 'Bool', optional => 0 },
                              start_omega_record => { isa => 'Int', optional => 0 },
                              parcov_blocks => { isa => 'ArrayRef', optional => 0},
                              est_records => { isa => 'ArrayRef', optional => 0},
                              cov_records => { isa => 'ArrayRef', optional => 0},
                              update_existing_model_files => { isa => 'Bool', optional => 0 },
                              etas_file => { isa => 'Maybe[Str]', optional => 0 },
    );
    my $model = $parm{'model'};
    my $imp_covariance_eval = $parm{'imp_covariance_eval'};
    my $start_omega_record = $parm{'start_omega_record'};
    my $parcov_blocks = $parm{'parcov_blocks'};
    my $est_records = $parm{'est_records'};
    my $cov_records = $parm{'cov_records'};
    my $update_existing_model_files = $parm{'update_existing_model_files'};
    my $etas_file = $parm{'etas_file'};

    my $modnum = 4;
    my $name_model = $name_model_4;
    if ($imp_covariance_eval) {
        $modnum = "4b";
        $name_model = $name_model_4b;
    }

    my $frem_model;

    cleanup_outdated_model(modelname => $self -> directory().'final_models/'.$name_model,
                           need_update => $update_existing_model_files);

    my $fin_dir = $self->directory().'final_models/';
    unless (-e $fin_dir.$name_model) {
        # input model  inits have already been updated
        $frem_model = $model->copy( filename    => $self->directory().'final_models/'.$name_model,
                                    output_same_directory => 1,
                                    write_copy => 0,
                                    copy_datafile   => 0,
                                    copy_output => 0);

        # if $ETAS FILE= used, M4 needs M3 phi output
        if ($etas_file) {
            # TODO: breakout into function since M2, M3 and M4 does pretty much the same things
            unless (-f $etas_file) {
                croak "\$ETAS file $etas_file could not be read for model $modnum, this is a bug";
            }

            # copy M3/M4 output to M4/M4b input phi file
            (my $etas_filename = $name_model) =~ s/(.*)\..*/$1_input.phi/;
            cp($etas_file, $fin_dir.$etas_filename);
            (undef, $etas_filename) = OSspecific::absolute_path($fin_dir, $etas_filename);

            # update FILE in model to new path (just filename since same directory)
            $frem_model->get_or_set_etas_file(problem_number => 1, new_file => $etas_filename);

            # update etas file to model 4/4b output (no further downstream usage)
            (my $phi_filename = $name_model) =~ s/(.*)\..*/$1.phi/;
            $etas_file = $fin_dir.$phi_filename;
        }

        if ($imp_covariance_eval) {
            # model must come from M4, so we set M4b up (importance sampling, expectation only, to get covariance step)
            my $last_est = $est_records->[-1];
            push @{$est_records}, model::problem::estimation->new(
                record_arr => ['METHOD=IMPMAP MAPITER=0 ISAMPLE=3000 NITER=5 EONLY=1 PRINT=1']
            );
        } else {
            # model must come from M3, so we set M4 up
            get_or_set_fix(model => $frem_model,
                           type => 'thetas',
                           set_array => $self->input_model_fix_thetas);
            get_or_set_fix(model => $frem_model,
                           type => 'sigmas',
                           set_array => $self->input_model_fix_sigmas);

            get_or_set_fix(model => $frem_model,
                           type => 'omegas',
                           set_array => $self->input_model_fix_omegas);


            my @omega_records = ();
            for (my $i=0; $i< ($start_omega_record-1);$i++){
                #if start_omega_record is 1 we will push nothing
                push(@omega_records,$frem_model-> problems -> [0]->omegas->[$i]);
            }

            for (my $i=0; $i< scalar(@{$parcov_blocks}); $i++){
                push(@omega_records,$parcov_blocks->[$i]);
            }

            $frem_model -> problems -> [0]->omegas(\@omega_records);
        }

        my $new_cov_records = [];
        # if OMITTED was on $COV line, remove it for M4 covariance step (and add UNCONDITIONAL)
        my $uncond;
        foreach my $opt (@{$cov_records}) {
            if ($opt =~ /^OMIT/) {
                $logger->info("Removed OMIT option from \$COV in $name_model\n");
            } else {
                push @{$new_cov_records}, $opt;
            }
            $uncond = 1 if ($opt =~ /^UNC/);
        }
        unless ($uncond) {
            push @{$new_cov_records}, "UNCONDITIONAL";
            $logger->info("Added UNCONDITIONAL option to \$COV in $name_model");
        }
        $frem_model->problems->[0]->estimations($est_records);
        $frem_model->problems->[0]->add_records( record_strings => $new_cov_records,
                                                 type => 'covariance' );
        $frem_model->_write();
    } else {
        (my $phi_filename = $name_model) =~ s/(.*)\..*/$1.phi/;
        $etas_file = $fin_dir.$phi_filename;
    }

    return ($etas_file);
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

    my $modnum=5;

    my $name_model = 'model_'.$modnum.'.mod';
    my $frem_model;

    cleanup_outdated_model(modelname => $self -> directory().'intermediate_models/'.$name_model,
                           need_update => $update_existing_model_files);

    unless (-e $self -> directory().'intermediate_models/'.$name_model){
        #read model 4 from disk, then copy it
        my $model = model->new( %{common_options::restore_options(@common_options::model_options)},
                                parse_output => 0,
                                filename                    => 'final_models/model_4.mod',
                                ignore_missing_output_files => 1 );

        $frem_model = $model ->  copy( filename    => $self -> directory().'intermediate_models/'.$name_model,
                                       output_same_directory => 1,
                                       write_copy => 0,
                                       copy_datafile   => 0,
                                       copy_output => 0);

        #SIGMA fix all existing
        if (defined $frem_model->problems->[0]->sigmas) {
            foreach my $record (@{$frem_model-> problems -> [0]->sigmas}){
                if ($record->is_block){
                    $record->fix(1) unless ($record->same);
                }else{
                    for (my $j=0; $j< scalar(@{$record->options}); $j++){
                        $record->options->[$j]->fix(1);
                    }
                }
            }
        }

        #OMEGA fix all before $start_omega
        for (my $i=0; $i<($start_omega_record-1); $i++){
            my $record = $frem_model-> problems -> [0]->omegas->[$i];
            if ($record->is_block){
                $record->fix(1) unless ($record->same);
            }else{
                for (my $j=0; $j< scalar(@{$record->options}); $j++){
                    $record->options->[$j]->fix(1);
                }
            }
        }

        #THETA changes
        #FIX all existing
        if (defined $frem_model->problems->[0]->thetas){
            for (my $i=0; $i< scalar(@{$frem_model->problems->[0]->thetas}); $i++){
                for (my $j=0; $j< scalar(@{$frem_model->problems->[0]->thetas->[$i]->options}); $j++){
                    $frem_model->problems->[0]->thetas->[$i]->options->[$j]->fix(1);
                }
            }
        }
        my $dimension = $frem_model->problems->[0]->omegas->[$start_omega_record-1]->size;
        my $top_size = $dimension - scalar(@{$self->covariates});
        #do cholesky
        my $warnings =
            $frem_model->problems->[0]->cholesky_reparameterize(what => 'o'.$start_omega_record,
                                                                bounded_theta => 0,
                                                                correlation_cutoff => 0,
                                                                correlation_limit => 0.9, #if higher then warn
            );
        #correlation cutoff $smallnum would automatically gives 0 FIX for correlations not in input model, but
        #might give some extra.
        #Fix all parameter-parameter and covariate-covariate correlations, and all SD

        my @last_zero_col = ();
        my $cumulative = 0;
        for(my $i=0; $i<scalar(@{$parameter_etanumbers}); $i++) {
            my $new_size = scalar(@{$parameter_etanumbers->[$i]});
            push(@last_zero_col,( ($cumulative) x $new_size ));
            $cumulative += $new_size;
        }

        my $thetaindex = $first_cholesky_theta;
        my $row = 1;

        while ($row <= $dimension){
            #do the row
            #first comes SD, always fix
            $frem_model->problems->[0]->thetas->[$thetaindex]->options->[0]->fix(1);
            $thetaindex++;
            #then the correlations left to right
            for (my $col=1; $col< $row; $col++){
                #if an inserted parameter-parameter correlation
                if (($row <= $top_size) and ($col <= $last_zero_col[($row-1)])){
                    $frem_model->problems->[0]->thetas->[$thetaindex]->options->[0]->clear_upbnd;
                    $frem_model->problems->[0]->thetas->[$thetaindex]->options->[0]->clear_lobnd;
                    $frem_model->problems->[0]->thetas->[$thetaindex]->options->[0]->init('0');
                }
                if (($row <= $top_size) or ($col > $top_size)){ #not a paramater-covariate correlation
                    $frem_model->problems->[0]->thetas->[$thetaindex]->options->[0]->fix(1);
                }
                $thetaindex++;
            }
            $row++;
        }

        my $message = $frem_model-> check_and_set_sizes(LTH => 1); #set LTH if too many thetas.
        if (length($message)>0){
            ui -> print( category => 'all', message =>  $message.' However this NONMEM version does not support $SIZES. '.
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

    my $modnum=7;

    my $name_model = $name_model_7;
    my $frem_model;

    cleanup_outdated_model(modelname => $self -> directory().'final_models/'.$name_model,
                           need_update => $update_existing_model_files);

    unless (-e $self -> directory().'final_models/'.$name_model){
        $frem_model = $model ->  copy( filename    => $self -> directory().'final_models/'.$name_model,
                                       output_same_directory => 1,
                                       write_copy => 0,
                                       copy_datafile   => 0,
                                       copy_output => 0);

        $frem_model -> set_maxeval_zero(print_warning => 0,
                                        last_est_complete => $self->last_est_complete,
                                        niter_eonly => $self->niter_eonly,
                                        need_ofv => 1);

        $frem_model->problems->[0] -> remove_records(type => 'covariance' );

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
                              subdirectory => {isa => 'Str', default => 'intermediate_models'}
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
        my $run = tool::modelfit ->new( %{common_options::restore_options(@common_options::tool_options)},
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

sub save_covresults {
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
                    $logger->warning("Warning: abs(mean) for $self->covariates->[$i] is $amean,".
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

sub read_covresults {
    # read file for covariate stat summary (see save_covresults), needed by post-processing
    # TODO: Merge with read_covdata (ideally, all empirical covstats should be written out by save_covresults)
    my %parm = validated_hash(\@_,
        covnames => { isa => 'ArrayRef', optional => 0 },
        filename => { isa => 'Str', optional => 0 },
    );
    my @covnames = @{ $parm{'covnames'} }; # must match covariates in file
    my $ncov = scalar(@covnames);
    my $filename = $parm{'filename'};

    unless (open(FILE, '<', $filename)) {
        $logger->error("cannot open $filename: $!");
        return undef;
    }

    my (%res, $stat, $cmat);
    while (my $line = <FILE>) {
        chomp $line;
        my @fields = split ',', $line;
        if (scalar(@fields) == 0) { # ignore empty lines
            next;
        } elsif (scalar(@fields) == 1) { # single header => section
            if ($fields[0] eq "summary_statistics") {
                $stat = 1; $cmat = 0;
                next;
            } elsif ($fields[0] eq "invariant_covmatrix") {
                $stat = 0; $cmat = 1;
                next;
            } else {
                $logger->error("read error $filename: unknown section '$fields[0]', ignoring");
                $stat = 0; $cmat = 0;
                next;
            }
        } else { # multiple columns => data in section
            if ($stat) {
                if ($fields[0] eq "") { # covariate header
                    shift @fields;
                    my $nfields = scalar(@fields);
                    unless ($ncov == $nfields) {
                        $logger->error("read error $filename: $nfields covariates but $ncov expected");
                        return undef;
                    }
                    for (my $i=0; $i<$ncov; $i++) {
                        unless ($covnames[$i] eq $fields[$i]) {
                            $logger->error("read error $filename: cov $fields[$i] but $covnames[$i] expected");
                            return undef;
                        }
                    }
                } else { # per-covariate data
                    my $covdata = shift @fields;
                    $res{$covdata} = \@fields;
                }
            } elsif ($cmat) {
                if ($fields[0] eq "") { # covmat col header
                    $res{'invariant_covmatrix'} = [];
                } else {
                    shift @fields; # covmat row header
                    push @{ $res{'invariant_covmatrix'} }, \@fields;
                }
            }
        }
    }
    close(FILE);

    # check mandatory reads
    my @mandatory_keys = qw/has_missingness invariant_mean invariant_median invariant_variance invariant_stdev invariant_covmatrix/;
    foreach my $key (@mandatory_keys) {
        if (!exists $res{$key}) {
            $logger->error("read error $filename: could not read '$key'");
            return undef;
        }
    }
    return \%res;
}

sub get_recovery_string
{
    my $filename = shift;
    if (-e $filename){
        debugmessage(1,"Main process reading child results from $filename");
    }else{
        croak(' recovery file does not exist: '.$filename);
    }
    my $fh;
    open($fh, $filename) or croak("could not read recovery file $filename");
    my $string = join(' ',<$fh>);
    close($fh);
    return $string;
}

sub restore_fork
{
    my %parm = validated_hash(\@_,
                              outputname => { isa => 'Str', optional => 1 },
                              modelname => { isa => 'Str', optional => 0 },
        );
    my $outputname = $parm{'outputname'};
    my $modelname = $parm{'modelname'};

    my $outobj;
    my $model;
    if (defined $outputname){
        if (-e $outputname){
            $outobj = output -> new (filename =>$outputname, parse_output => 0);
        }else{
            croak('cannot restore output object after fork '.$outputname);
        }
    }
    if (defined $modelname and -e $modelname){
        $model = model->new ( %{common_options::restore_options(@common_options::model_options)},
                              filename                    => $modelname,
                              parse_output => 0,
                              ignore_missing_output_files => 1 );
    }else{
        croak('cannot restore model object after fork '.$modelname);
    }
    return ($outobj,$model);
}

sub modelfit_setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    my $model = $self -> models -> [$model_number-1];
    my ($frem_model2,$frem_model3,$frem_model4,$frem_model5,$frem_model7);
    my ($frem_model1,$output_model1,$mod1_ofv,$output_model1_fullname,$output_model2_fullname);
    my $frem_datasetname = 'frem_dataset.dta';
    my ($filtered_data,$indices);
    my ($new_omega_order,$need_to_move_omegas);
    my ($skip_etas,$fix_omegas,$parameter_etanumbers);
    my ($covariate_etanumbers,$ntheta,$epsnum,$maxeta);
    my ($mod3_parcov_block,$est_records,$cov_records);
    my (@tmp_covariates,@tmp_extra_input_items,@tmp_log,@tmp_categorical,%covresultref);
    my @tmp_parcov_block;
    my $covresultref;
    my ($mod4_parcov_block,$mod4ofv);
    my $finaldir= $self->directory.'final_models';
    my ($mes,$sir_model, $sir_model_text);
    my ($error,$message,$fh);
    my $do_print_proposal=0;
    my $proposal_filename = 'proposal_density.cov';
    my $recovery_filename = 'child_process_variables';
    my $update_existing_model_files = 0;
    my $need_update;
    my $etas_file; # $ETAS FILE= if present (continuously updated; new files created downstream)

    my $inter = $self -> directory().'intermediate_models';
    unless (-d $inter){
        mkdir($inter);
    }

    if ($self->fork_runs){
        $self->submit_child;
    }

    if ($self->tool_child_id == 0){ #this is true if no forking and main process, or with fork and child process
        #this runs input model, if necessary, and updates inits
        ($frem_model1,$output_model1,$need_update,$etas_file) = $self->do_model1(model => $model);
        $mod1_ofv = $output_model1->get_single_value(attribute=> 'ofv');
        $update_existing_model_files = 1 if ($need_update);
        if ($self->fork_runs){
            #we have done a fork and this is child process
            #store mod1_ofv and write updated frem_model1 to disk
            $frem_model1->filename($name_model_1_updated);
            $frem_model1->_write();
            my @dumper_names = qw(mod1_ofv output_model1_fullname update_existing_model_files);
            open($fh, '>'.$self->directory.$recovery_filename.'_mod1') or
                die "could not open file $recovery_filename mod1 for writing.\n";
            print $fh data::dumper->dump(
                [$mod1_ofv,$output_model1->full_name,$update_existing_model_files],
                \@dumper_names
                );
            close $fh;
            debugmessage(1,"exit child process for mod1");
            exit 0;
        }
    }else{
        #we have done a fork and this is main process
        $self->wait_until_child_finished;
        my $string = get_recovery_string($self->directory.$recovery_filename.'_mod1');
        eval $string;
        ($output_model1,$frem_model1) = restore_fork(outputname => $output_model1_fullname,
                                                     modelname => 'intermediate_models/'.$name_model_1_updated);
    }
    $self->reordered_model_1($frem_model1); # used by prepare_results

    if ($self->fork_runs){
        $self->submit_child;
    }

    if ($self->tool_child_id == 0){
        #either main process of not forked frem, or child of forked frem
        #this modifies $self->covariates,self->extra_input_items($extra_input_items),$self->log(\@new_log)
        #        $self->categorical($new_categorical);
        ($filtered_data,$indices) = $self->do_filter_dataset_and_append_binary(model => $frem_model1);
        #                                                                              rescale_data => $self->rescale_data);

        $covresultref = $self->do_frem_dataset(model => $model, #must be input model here, not updated with final ests
                                               N_parameter_blocks => 1,
                                               filtered_data => $filtered_data,
                                               indices => $indices,
                                               mod1_ofv => $mod1_ofv,
                                               fremdataname => $frem_datasetname);

        if ($self->fork_runs){
            my @dumper_names = qw(*tmp_covariates *tmp_extra_input_items *tmp_log *tmp_categorical *covresultref);
            open($fh, '>'.$self->directory.$recovery_filename.'_fremdata') or
                die "could not open file $recovery_filename fremdata for writing.\n";
            print $fh data::dumper->dump(
                [$self->covariates,$self->extra_input_items,$self->log,$self->categorical,$covresultref],
                \@dumper_names
                );
            close $fh;
            debugmessage(1,"exit child process for fremdata");
            exit 0;
        }else{
            $self->save_covresults($covresultref);
        }

    }else{
        #we have done a fork and this is main process
        $self->wait_until_child_finished;
        my $string = get_recovery_string($self->directory.$recovery_filename.'_fremdata');
        eval $string;
        $self->covariates(\@tmp_covariates);
        $self->extra_input_items(\@tmp_extra_input_items);
        $self->log(\@tmp_log);
        $self->categorical(\@tmp_categorical);
        $self->save_covresults(\%covresultref);
    }


    $self->start_omega_record(scalar(@{$self->skip_omegas})+1);

    ($new_omega_order,$need_to_move_omegas)=get_new_omega_order(model =>$frem_model1,
                                                                   skip_omegas => $self->skip_omegas);

    if ($need_to_move_omegas and $self->mu){
        ui->print(category => 'all',
                  message => "\n##########################################################################".
                  "\nwarning: -skip_omegas option (see userguide) will result in renumbering\n".
                  " of some etas, but mu variables will not be renumbered.\n".
                  "##########################################################################\n\n");
    }

    ($skip_etas,$fix_omegas,$parameter_etanumbers,$etas_file) =
        put_skipped_omegas_first(model => $frem_model1,
                                 start_omega_record =>$self->start_omega_record,
                                 new_omega_order =>$new_omega_order,
                                 need_to_move =>$need_to_move_omegas,
                                 input_model_fix_omegas => $self->input_model_fix_omegas,
                                 etas_file => $etas_file);

    #now model1 is reordered, and diagonal n -> n block 1

    $self->input_model_fix_omegas($fix_omegas);
    $self->skip_etas($skip_etas);

    $maxeta =  $frem_model1->problems()->[0]->nomegas(with_correlations => 0,
                                                         with_same => 1);
    $covariate_etanumbers = [(($maxeta+1) .. ($maxeta+scalar(@{$self->covariates})))] ;

    ($est_records,$ntheta,$epsnum,$etas_file) = $self->prepare_model2(model => $frem_model1,
                                                                      fremdataname => $frem_datasetname,
                                                                      skip_etas => $self->skip_etas,
                                                                      start_omega_record => $self->start_omega_record,
                                                                      invariant_mean => $self->invariant_mean,
                                                                      invariant_covmatrix => $self->invariant_covmatrix,
                                                                      update_existing_model_files => $update_existing_model_files,
                                                                      etas_file => $etas_file);

    if ($self->fork_runs){
        $self->submit_child;
    }

    $message = undef;
    if ($self->tool_child_id == 0){

        ($frem_model2,$message,$need_update) = $self->run_unless_run(numbers => [2]);
        if (defined $message and length($message)>0){
            ui->print(category => 'frem', message => $message) unless ($self->fork_runs); #let main process print message if fork
            die;
        }
        $update_existing_model_files = 1 if ($need_update);
        $mod3_parcov_block = get_parcov_blocks(model => $frem_model2,
                                               skip_etas => $self->skip_etas,
                                               covariate_etanumbers => $covariate_etanumbers,
                                               parameter_etanumbers => $parameter_etanumbers,
                                               start_omega_record => $self->start_omega_record);

        if ($self->fork_runs){
            #we have done a fork and this is child process
            #write updated model 2 and message
            $frem_model2->filename($name_model_2_updated);
            $frem_model2->_write();
            my @dumper_names = qw(message update_existing_model_files output_model2_fullname *tmp_parcov_block);
            open($fh, '>'.$self->directory.$recovery_filename.'_mod2') or
                die "could not open file $recovery_filename mod2 for writing.\n";
            print $fh data::dumper->dump([$message,$update_existing_model_files,$frem_model2->outputs->[0]->full_name,$mod3_parcov_block],
                                         \@dumper_names);
            close $fh;
            debugmessage(1,"exit child process for mod2");
            exit 0;
        }
    }else{
        #we have done a fork and this is main process
        $self->wait_until_child_finished;
        my $string = get_recovery_string($self->directory.$recovery_filename.'_mod2');
        eval $string;
        my $dirt;
        ($dirt,$frem_model2) = restore_fork(modelname => 'intermediate_models/'.$name_model_2_updated);
        if (defined $message and length($message)>0){
            ui->print(category => 'frem',
                      message => $message);
            die;
        }
        $mod3_parcov_block = \@tmp_parcov_block;
    }
    $self->model_2($frem_model2); # used by prepare_results

    ($est_records,$cov_records,$etas_file) = $self->prepare_model3(model => $frem_model2,
                                                                   start_omega_record => $self->start_omega_record,
                                                                   parcov_blocks => $mod3_parcov_block,
                                                                   update_existing_model_files => $update_existing_model_files,
                                                                   est_records => $est_records,
                                                                   etas_file => $etas_file);


    if ($self->fork_runs){
        $self->submit_child;
    }

    $message = undef;
    if ($self->tool_child_id == 0){

        ($frem_model3,$message,$need_update) = $self->run_unless_run(numbers => [3]);
        if (defined $message and length($message)>0){
            ui->print(category => 'frem', message => $message) unless ($self->fork_runs); #let main process print message if fork
            die;
        }
        $update_existing_model_files = 1 if ($need_update);
        $mod4_parcov_block = get_parcov_blocks(model => $frem_model3,
                                               skip_etas => $self->skip_etas,
                                               covariate_etanumbers => $covariate_etanumbers,
                                               parameter_etanumbers => $parameter_etanumbers,
                                               start_omega_record => $self->start_omega_record);
        if ($self->fork_runs){
            #we have done a fork and this is child process
            $frem_model3->filename($name_model_3_updated);
            $frem_model3->_write();
            my @dumper_names = qw(message update_existing_model_files *tmp_parcov_block);
            open($fh, '>'.$self->directory.$recovery_filename.'_mod3') or
                die "could not open file $recovery_filename mod3 for writing.\n";
            print $fh data::dumper->dump([$message,$update_existing_model_files,$mod4_parcov_block],\@dumper_names);
            close $fh;
            debugmessage(1,"exit child process for mod3");
            exit 0;
        }

    }else{
        #we have done a fork and this is main process
        $self->wait_until_child_finished;
        my $string = get_recovery_string($self->directory.$recovery_filename.'_mod3');
        eval $string;
        my $dirt;
        ($dirt,$frem_model3) = restore_fork(modelname => 'intermediate_models/'.$name_model_3_updated);
        if (defined $message and length($message)>0){
            ui->print(category => 'frem',
                      message => $message);
            die;
        }
        $mod4_parcov_block = \@tmp_parcov_block;
    }


    mkdir($finaldir) unless (-d $finaldir);

    ($etas_file) = $self->prepare_model4(model => $frem_model3,
                                         imp_covariance_eval => 0,
                                         start_omega_record => $self->start_omega_record,
                                         parcov_blocks => $mod4_parcov_block,
                                         est_records => $est_records,
                                         cov_records => $cov_records,
                                         update_existing_model_files => $update_existing_model_files,
                                         etas_file => $etas_file,
        );

    #fixme subtool instead?

    push(@{$self->final_numbers},4) if $self->estimate_regular_final_model;

    if ($self->cholesky){
        $self->prepare_model5(start_omega_record => $self->start_omega_record,
                              first_cholesky_theta => scalar(@{$frem_model3->problems->[0]->thetas}),
                              parameter_etanumbers => $parameter_etanumbers,
                              update_existing_model_files => $update_existing_model_files
            );

        ($frem_model5,$message,$need_update) = $self->run_unless_run(numbers => [5]);
        if (defined $message){
            ui->print(category => 'frem',
                      message => "estimation of model 5 failed, cannot prepare model 6 (final cholesky model)");
        }else{
            $self->prepare_model6(model => $frem_model5,
                                  first_cholesky_theta => scalar(@{$frem_model3->problems->[0]->thetas}),
                                  start_omega_record => $self->start_omega_record,
                                  parameter_etanumbers => $parameter_etanumbers,
                                  update_existing_model_files => ($need_update or $update_existing_model_files)
                );

            push(@{$self->final_numbers},6);
        }
    }

    (my $final_models, $mes) = $self->run_unless_run(numbers => $self->final_numbers,
                                                     subdirectory => 'final_models',
                                                     final => 1) if (scalar(@{$self->final_numbers})>0);
    push(@{$self->final_models}, @{$final_models});

    if ($self->estimate_regular_final_model){
        #model 4
        $mod4ofv = $self->final_models->[0]->outputs->[0]->get_single_value(attribute => 'ofv');
        if (not defined $mod4ofv){
            ui->print(category => 'frem',
                      message => 'estimation of model 4 failed to give ofv value. creating model 7.');
            $self->prepare_model7(model => $self->final_models->[0],update_existing_model_files => $update_existing_model_files);
            ($frem_model7,$message,$need_update) = $self->run_unless_run(numbers => [7],
                                                                         subdirectory => 'final_models');
            if (defined $message){
                ui->print(category => 'frem',
                          message => $message);
                die;
            }else{
                $sir_model = $frem_model7;
                $sir_model_text = 'model 7';
            }
        }else{
            $sir_model = $self->final_models->[0];
            $sir_model_text = 'model 4';
        }

        ($error,$message) = check_covstep(output => $self->final_models->[0]->outputs->[0]);

        if ($error){
            $logger->warning('Covariance step of model 4 NOT successful');
            $do_print_proposal=1;
            if ($self->imp_covariance and defined $mod4ofv) {
                $logger->info('Will use IMP sampling to get covariance step (model 4b)');
                unless ($self->mu) {
                    $logger->warning('MU referencing (not used) can speed up model 4b sampling');
                }
                $self->final_models->[0]->update_inits(from_output => $self->final_models->[0]->outputs->[0]);
                ($etas_file) = $self->prepare_model4(model => $self->final_models->[0],
                                                     imp_covariance_eval => 1,
                                                     start_omega_record => $self->start_omega_record,
                                                     parcov_blocks => $mod4_parcov_block,
                                                     est_records => $est_records,
                                                     cov_records => $cov_records,
                                                     update_existing_model_files => $update_existing_model_files,
                                                     etas_file => $etas_file,
                );
                push(@{$self->final_numbers},'4b');
                (my $model_4b,$mes) = $self->run_unless_run(numbers => ['4b'],
                                                            subdirectory => 'final_models',
                                                            final => 1);
                push(@{$self->final_models}, @{$model_4b});
                ($error,$message) = check_covstep(output => $model_4b->[0]->outputs->[0]);
                $logger->warning('Covariance step of model 4b NOT successful') if ($error);
            } elsif ($self->imp_covariance) {
                $logger->warning('Model 4 failed to give OFV value, IMP sampling not applicable');
            }
        }
        unless ($error) {
            $logger->info('Covariance step was successful!');
            if ($self->always_proposal_density){
                $do_print_proposal=1;
                $logger->info('Will create alternative proposal density for model 4 sir');
            }
        }


        my $output_model2;
        if (defined $output_model2_fullname) {
            $output_model2 = output->new(filename =>$output_model2_fullname, parse_output => 0);
        } else {
            $output_model2 = $frem_model2->outputs->[0];
        }
        if ($do_print_proposal) {
            print_proposal_density(omega_orders => [$new_omega_order,[]],
                       partial_outputs => [$output_model1,$output_model2],
                       full_model => $sir_model,# not updated, but may have estimates of everything
                       reordered_model1 => $frem_model1,
                       rse => $self->rse,
                       directory => $self->directory,
                       filename => $proposal_filename);
            ui->print(category => 'frem',
                  message => "printed $proposal_filename for sir -covmat_input option");
        }
        if ($error and $self->run_sir) {
            #                chdir($self->directory);
            ui->print(category => 'frem',
                      message => 'starting sir');
            ui->category('sir');
            my %options;
            #                $options{'samples'}=2000;
            #                $options{'resamples'}=1000;
            $options{'problems_per_file'}=25;
            $options{'covmat_input'} = $self->directory.$proposal_filename;
            input_checking::check_options(tool => 'sir', options => \%options, model => $sir_model);

            my $sir = tool::sir->new ( %{common_options::restore_options(@common_options::tool_options)},
                                       %options,
                                       top_tool => 1,
                                       models                     => [ $sir_model ],
                                       template_file_rplots => 'sir_default.r',
                                       directory => $self->directory.'sir_dir1',
                                       fast_posdef_checks => 1,
                );

            $sir-> print_options (cmd_line => 'sir final_models/'.$sir_model->filename.' -covmat_input='.$proposal_filename,
                                  toolname => 'sir',
                                  local_options => ["samples:s","resamples:s","covmat_input:s","problems_per_file:i"],
                                  common_options => \@common_options::tool_options) ;
            $sir -> run;
            $sir -> prepare_results();
            $sir -> print_results();

            ui->category('frem');
            ui->print(category => 'frem',
                      message => 'sir done');

        }
    }
}

sub modelfit_analyze
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
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

        $filtered_data_model -> add_records(type => 'pred',
                                            record_strings => [$fremtype.'=0','Y=THETA(1)+ETA(1)+EPS(1)']);

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

    # set $TABLE record

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

sub cleanup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
          arg1 => { isa => 'Int', optional => 1 }
    );

  #remove tablefiles in simulation NM_runs, they are
  #copied to m1 by modelfit and read from there anyway.
  for (my $samp=1;$samp<=$self->samples(); $samp++){
    unlink $self -> directory."/simulation_dir1/NM_run".$samp."/mc-sim-".$samp.".dat";
    unlink $self -> directory."/simulation_dir1/NM_run".$samp."/mc-sim-".$samp."-1.dat"; #retry
  }
}

sub submit_child
{
    my $self = shift;
    debugmessage(1,"try submit child process");

    my $errmess;
    my $pid = fork();
    unless (defined $pid){
        $errmess="$!";
        croak("Perl fork() failed: ".$errmess."\n".'cannot proceed');
    }
    $self->tool_child_id($pid);
}

sub wait_until_child_finished
{
    my $self = shift;
    if ($self->tool_child_id == 0){
        croak("sub wait_until_child_finished should not be run by child (tool_child_id 0) this is a bug");
    }
    debugmessage(1,"polling until child finished, id ".$self->tool_child_id.", poll interval ".$self->poll_interval."s");
    while (not $self->child_process_finished){
        sleep($self->poll_interval);
    }
}

sub child_process_finished
{
    my $self = shift;

    my $pid = waitpid($self->tool_child_id, WNOHANG);

    # Waitpid will return tool_child_id if that process has
    # finished and 0 if it is still running.

    if ($pid == -1) {
        # If waitpid return -1 the child has probably finished
        # and has been "Reaped" by someone else. We treat this
        # case as the child has finished.
        carp("waitpid returned -1");
        $pid = $self->tool_child_id; #child finished
    }

    return $pid;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
