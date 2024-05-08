package model;

use include_modules;
use Cwd;
BEGIN {
    require File::Copy;
}
use File::Basename;
use File::Spec qw(splitpath catfile);
use Config;
use OSspecific;
use Storable;
use model::shrinkage_module;
use random qw(random_multivariate_normal);
use model::iofv_module;
use model::nonparametric_module;
use model::annotation;
use output;
use model::problem;
use Mouse;
use MouseX::Params::Validate;
use PsN;
use pharmml;

=head1 Description

PsN::model is a Perl module for parsing and manipulating NONMEM model
files.

The model class is built around the NONMEM model file. This is an
ordinary ASCII text file that, except for the data, holds all
information needed for fitting a non-linear mixed effect model using
NONMEM. Typically, a model file contains specifications for a
pharmacokinetic and/or a pharmacodynamic model, initial estimates of
model parameters, boundaries for model parameters as well as details
about the data location and format.

=cut

has 'problems' => ( is => 'rw', isa => 'ArrayRef[model::problem]' );
has 'outputs' => ( is => 'rw', isa => 'Maybe[ArrayRef[output]]', clearer => 'clear_outputs' );
has 'nonparametric_modules' => ( is => 'rw', isa => 'ArrayRef[model::nonparametric_module]' );
has 'iofv_modules' => ( is => 'rw', isa => 'ArrayRef[model::iofv_module]' );
has 'active_problems' => ( is => 'rw', isa => 'Maybe[ArrayRef[Bool]]' );
has 'is_dummy' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'maxevals' => ( is => 'rw', isa => 'Maybe[Int]', default => 0 );
has 'cwres' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'iofv' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'mirror_plots' => ( is => 'rw', isa => 'Maybe[Int]', default => 0 );
has 'mirror_from_lst' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'directory' => ( is => 'rw', isa => 'Str' );
has 'extra_files' => ( is => 'rw', isa => 'Maybe[ArrayRef[Str]]' );
has 'extra_output' => ( is => 'rw', isa => 'Maybe[ArrayRef[Str]]' );
has 'filename' => ( is => 'rw', required => 1, isa => 'Str' );
has 'relative_data_path' => ( is => 'rw', isa => 'Bool', default => 1 ); #code relies on this default
has 'ignore_missing_data' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'ignore_missing_files' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'ignore_missing_output_files' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'outputfile' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'parse_output' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'sde' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'omega_before_pk' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'psn_record_order' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'tbs' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'dtbs' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'tbs_lambda' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'tbs_zeta' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'tbs_delta' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'tbs_thetanum' => ( is => 'rw', isa => 'Int' );
has 'missing_data_token' => ( is => 'rw', isa => 'Maybe[Int]', default => -99 );
has 'last_est_complete' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'niter_eonly' => ( is => 'rw', isa => 'Maybe[Int]' );
has 'annotation' => ( is => 'rw', isa => 'model::annotation' );
has 'phi_file' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'output_directory' => ( is => 'rw', isa => 'Maybe[Str]' );

sub BUILD
{
    my $self  = shift;
    my $parmref = shift;
    my %parm = %{$parmref};

    if (defined $parm{'problems'}) {
        $self->problems($parm{'problems'});
    } elsif (defined $parm{'model_lines'}) {
        $self->_read_problems(model_lines => $parm{'model_lines'});
    } else {
        my $dir;
        my $filename;
        ($dir, $filename) = OSspecific::absolute_path($self->directory, $self->filename);
        $self->filename($filename);
        $self->directory($dir);

        # Convert if in PharmML format
        my $file = $self->full_name;
        if (-e $file) {
            if (pharmml::is_pharmml($file)) {
                ui -> print( category => 'all',
                             message  => "*** Input file is in PharmML format. Starting conversion to NMTRAN. ***\n");
                if (not pharmml::is_java_installed) {
                    croak("Error: To be able use a PharmML file as input to PsN the java run time environment has to be installed");
                }
                my $return_code = pharmml::convert_file($file);
                if ($return_code != 0) {
                    croak("Error: Conversion of $file failed");
                }
                ui -> print( category => 'all',
                             message  => "*** Conversion done ***\n");
                my $filename = $self->filename;
                # Currently the only way to get the converted filename
                if ($filename =~ /\.xml$/) {
                    $filename =~ s/\.xml$/.ctl/;
                } else {
                    $filename .= '.ctl';
                }
                $self->filename($filename);
                ui -> print( category => 'all',
                             message  => "*** Running nmtran on converted model ***\n");
                if (not pharmml::check_converted_model($filename)) {
                    croak("Error when running nmtran on converted file.");
                }
                ui -> print( category => 'all',
                             message  => "*** Converted model checked successfully ***\n");
            }
        }

        $self->_read_problems;
    }

    #ensure unique labels per param
    if ( defined $self->problems ) {
        foreach my $prob (@{$self->problems}) {
            next unless (defined $prob);
            $prob->ensure_unique_labels();
        }
    }
    if (defined $self->maxevals and ($self->maxevals > 0)) {
        if ( defined $self->problems ) {
            my $n_prob = scalar(@{$self->problems});
            for (my $probnum = 1; $probnum <= $n_prob; $probnum++) {
                unless ($self->is_option_set(record => 'estimation',
                                             name => 'MSFO',
                                             problem_number => $probnum,
                                             fuzzy_match => 1)) {
                    $self->add_option(record_name=>'estimation',
                                      option_name=>'MSFO',
                                      problem_numbers=> [$probnum],
                                      option_value => ( $probnum == 1 ? 'psn_msf':('psn_msf_pr'.$probnum)),
                                      add_record=>0);
                }
            }
        }
    }

    if ( defined $parm{'active_problems'} ) {
        $self->active_problems($parm{'active_problems'});
    } elsif ( defined $self->problems ) {
        my @active = ();
        for ( @{$self->problems} ) {
                push( @active, 1 );
        }
        $self->active_problems(\@active);
    }

    if ( defined $self->extra_files ) {
        for( my $i=0; $i < scalar @{$self->extra_files}; $i++ ) {
            my ( $dir, $file ) = OSspecific::absolute_path( $self->directory, $self->extra_files->[$i]);
            $self->extra_files->[$i] = $dir . $file;
        }
    }

    # Read outputfile, if any.
    if (not defined $self->outputs) {
        if (not defined $self->outputfile) {
            my $filename = $self->create_output_filename();
            $self->outputfile($filename);
        }
        $self->outputs([]);
        my $directory;
        if (defined $self->output_directory) {      # If a specific output_directory is available use that.
            $directory = $self->output_directory;   # This would come from the model_subdir tool option
        } else {
            $directory = $self->directory;
        }
        push(@{$self->outputs}, output->new(
            filename => $self->outputfile,
            directory => $directory,
                 parse_output => $self->parse_output,
            ignore_missing_files => ($self->ignore_missing_files || $self->ignore_missing_output_files))
        );
    }

    # Adding mirror_plots module here, since it can add
    # $PROBLEMS. Also it needs to know wheter an lst file exists
    # or not.

    if( defined $self->mirror_plots and ($self->mirror_plots > 0 )){
        my $mirror_plot_module = model::mirror_plot_module -> new( base_model => $self,
                                                                   nr_of_mirrors => $self->mirror_plots,
                                                                   cwres => $self->cwres,
                                                                   mirror_from_lst => $self->mirror_from_lst,
                                                                   niter_eonly => $self->niter_eonly,
                                                                   last_est_complete => $self->last_est_complete);
        push( @{$self -> {'mirror_plot_modules'}}, $mirror_plot_module );    #FIXME: Should have had an accessor. Fix with Mouse
    }

    if (defined $self->iofv and ($self->iofv > 0)) {
        my $iofv_module = model::iofv_module -> new( base_model => $self);
        $self->iofv_modules([]) unless defined $self->iofv_modules;
        push( @{$self->iofv_modules}, $iofv_module );
    }

    unless ($self->is_dummy){
        #simple checks to detect garbage input, for example missing $DATA in first $PROB
        unless (defined $self -> problems and defined $self -> problems->[0] and defined $self -> problems->[0]->datas
                and scalar(@{$self -> problems->[0]->datas})>0){
            croak("\nCorrupt model ".$self->filename().": no \$DATA record in first \$PROBLEM\n");
        }
        unless (defined $self -> problems->[0]->inputs
                and scalar(@{$self -> problems->[0]->inputs})>0){
            croak("\nCorrupt model ".$self->filename().": no \$INPUT record in first \$PROBLEM\n");
        }

    }

    #check that data files exist
    unless ($self->ignore_missing_data or $self->ignore_missing_files){
        foreach my $file (@{$self->datafiles(absolute_path =>1)}){
            unless (-e $file){
                croak("datafile $file does not exist for model ".$self->full_name);
            }
        }
        for (my $i=0; $i< scalar(@{$self->problems}); $i++){
            if (defined $self->problems->[$i]->msfis){
                for (my $j=0; $j< scalar(@{$self->problems->[$i]->msfis}); $j++){
                    next if ($self->problems->[$i]->msfis->[$j]->get_msfo_from_problem_number >0);
                    my $file = $self->problems->[$i]->msfis->[$j]->get_absolute_filename;
                    unless (-e $file){
                        croak("msfi $file does not exist for model ".$self->full_name);
                    }
                }
            }
        }
    }

}

sub load_output
{
    my $self = shift;
    if (defined $self-> outputs){
        for (my $i=0; $i<scalar(@{$self->outputs}); $i++){
            $self->outputs->[$i]->load;
        }
    }
}

sub check_and_set_sizes
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              LVR => { isa => 'Bool', default => 0 },
                              LVR2 => { isa => 'Bool', default => 0 },
                              LTH => { isa => 'Bool', default => 0 },
                              PD => { isa => 'Bool', default => 0 },
                              all => { isa => 'Bool', default => 0 },
        );


    my %check=();
    $check{'LVR'} = ($parm{'LVR'} or $parm{'all'});
    $check{'LVR2'} = ($parm{'LVR2'} or $parm{'all'});
    $check{'LTH'} = ($parm{'LTH'} or $parm{'all'});
    $check{'PD'} = ($parm{'PD'} or $parm{'all'});

    my $can_set_sizes = 1;
    my $need_set_sizes = 0;
    my $error = '';

    if (($PsN::nm_major_version == 5) or ($PsN::nm_major_version == 6) or ($PsN::nm_major_version == 7 and ($PsN::nm_minor_version < 2))) {
        $can_set_sizes = 0;
    }

    my %max_allowed=(); #NONMEM defaults #FIXME set per NM version
    $max_allowed{'LVR'}=30;
    $max_allowed{'LVR2'}=20;
    $max_allowed{'LTH'}=100;
    $max_allowed{'PD'}=50;

    foreach my $option (keys %max_allowed){
        #check if other value than default set in $SIZES
        my $set_value = $self->get_option_value(record_name => 'sizes',
                                                option_name => $option,
                                                fuzzy_match => 0);
        if (defined $set_value and (length($set_value) > 0) ){
            $max_allowed{$option} = abs($set_value); #can set negative in SIZES
        }
    }

    my %count_in_model = ();
    foreach my $option (keys %max_allowed){
        $count_in_model{$option}=0;
    }

    foreach my $prob (@{$self->problems}){
        #LVR
        my $eta_plus_eps = ($prob->nomegas(with_correlations => 0,with_same => 1)+
                            $prob->nsigmas(with_correlations => 0,with_same => 1));
        $count_in_model{'LVR'} = $eta_plus_eps if ($eta_plus_eps > $count_in_model{'LVR'});
        #LTH
        my $thetacount = $prob->record_count(record_name=>'theta');
        $count_in_model{'LTH'} = $thetacount if ($thetacount > $count_in_model{'LTH'});
        #PD
        my $pdcount = 0;
        foreach my $input (@{$prob->inputs}){
            $pdcount += scalar(@{$input->get_nonskipped_columns});
        }
        $pdcount += 10; #seems that exact count was not enough. "cannot append items"
        $count_in_model{'PD'} = $pdcount if ($pdcount > $count_in_model{'PD'});
        #LVR2
        if ($prob->is_option_set ( name           => 'LAPLACE',
                                   record         => 'estimation',
                                   record_number => 0, #this means all records
                                   fuzzy_match    => 1 )){
            my $laplace_eta = $prob->nomegas(with_correlations => 0,with_same => 1);
            $count_in_model{'LVR2'} = $laplace_eta if ($laplace_eta > $count_in_model{'LVR2'});
        }
    }

    foreach my $option (keys %check){
        next unless ($check{$option});
        if ($count_in_model{$option} > $max_allowed{$option}){
            $need_set_sizes = 1;
            unless ($can_set_sizes){
                $error .= " Need $option set to at least ".$count_in_model{$option}.'.';
                next;
            }
            my $neg = '';
            $neg = '-' if ($option eq 'PD');
            if (defined $self->problems->[0]->sizess() and scalar(@{$self ->problems->[0]->sizess()})>0){
                $self -> set_option(record_name => 'sizes',
                                    record_number => 1,
                                    option_name => $option,
                                    option_value => $neg.$count_in_model{$option},
                                    fuzzy_match => 0);

            }else{
                $self -> add_records( type => 'sizes',
                                      record_strings => [ ' '.$option.'='.$neg.$count_in_model{$option}] );
            }
        }
    }
    return $error;
}

sub create_maxeval_zero_models_array
{
    my %parm = validated_hash(\@_,
        subdirectory => { isa => 'Str', optional => 0 },
        basedirectory => { isa => 'Str', optional => 0 },
        ignore_missing_parameters => { isa => 'Bool', default => 0, optional => 1 },
        sampled_params_arr => { isa => 'ArrayRef', optional => 0 },
        model => { isa => 'model', optional => 0 },
        mceta => { isa => 'Int', optional => 1 },
        purpose => { isa => 'Str', optional => 0 },
        match_labels => { isa => 'Bool', default => 1, optional => 1 },
        problems_per_file => { isa => 'Maybe[Int]', optional => 1, default => 100 },
    );
    my $model = $parm{'model'};
    my $subdirectory = $parm{'subdirectory'};
    my $basedirectory = $parm{'basedirectory'};
    my $sampled_params_arr = $parm{'sampled_params_arr'};
    my $mceta = $parm{'mceta'};
    my $purpose = $parm{'purpose'};
    my $ignore_missing_parameters = $parm{'ignore_missing_parameters'};
    my $match_labels = $parm{'match_labels'};
    my $problems_per_file = $parm{'problems_per_file'};

    $problems_per_file = 100 unless (defined $problems_per_file); #if given and undef

    croak("problems_per_file must be larger than 0") unless ($problems_per_file > 0);

    my $samples_done = 0;
    my $run_num = 1;
    my @modelsarr = ();
    my $run_model;
    my $dummymodel;
    my $dummyname = 'dummy.mod';
    my @problem_lines = ();
    while ($samples_done < scalar(@{$sampled_params_arr})) {
        #copy the model
        $run_model = $model ->  copy( filename    => $subdirectory.$purpose.'_'.$run_num.'.mod',
                                      output_same_directory => 1,
                                      copy_datafile =>0,
                                      copy_output => 0,
                                      write_copy => 0); #do not write until are done with modifications
        $run_num++;

        $run_model->set_maxeval_zero(need_ofv => 1);
        if ((not ($PsN::nm_major_version == 5 or $PsN::nm_major_version == 6)) and ($PsN::nm_minor_version >= 3)) {
            if ($mceta > 0) {
                $run_model->set_option(problem_numbers => [1],
                                       record_name => 'estimation',
                                       option_name => 'MCETA',
                                       option_value => $mceta);
            }
        }
        foreach my $record ('table','simulation','covariance','scatter'){
            $run_model -> remove_records (problem_numbers => [1],
                                          keep_last => 0,
                                          type => $record);
        }

        if (scalar(@problem_lines)<1){
            #first iteration
            $dummymodel = $run_model ->  copy( filename    => $subdirectory.$dummyname,
                                               output_same_directory => 1,
                                               copy_output => 0,
                                               write_copy =>0);

            #set $DATA REWIND
            $dummymodel->add_option(problem_numbers => [1],
                                    record_name => 'data',
                                    option_name => 'REWIND');

            #remove most records, keep only $PROB, $INPUT, $DATA, $THETA, $OMEGA, $SIGMA $EST $CONTR

            #TODO what about omegapd, thetap thetapv... for priors??
            foreach my $record ('table','simulation','pk','pred','error','covariance','scatter','msfi','subroutine',
                                'abbreviated','sizes','prior','model','tol','infn','aesinitial',
                                'bind',
                                'aes','des','mix','nonparametric'){
                $dummymodel -> remove_records (problem_numbers => [1],
                                               keep_last => 0,
                                               type => $record);
            }
            my $linesarray = $dummymodel->problems->[0]->_format_problem(relative_data_path => $run_model->relative_data_path,
                                                                         write_directory => $run_model->directory);
            #we cannot use this array directly, must make sure items do not contain line breaks
            foreach my $line (@{$linesarray}){
                my @arr = split(/\n/,$line);
                push(@problem_lines,@arr);
            }
            $dummymodel = undef;
        }


        #update ests for first $PROB in real model
        $run_model -> update_inits(from_hash => $sampled_params_arr->[$samples_done],
                                   ignore_missing_parameters => $ignore_missing_parameters,
                                   match_labels => $match_labels);

        $samples_done++;
        my $probnum=2;
        for (my $i=1; $i<$problems_per_file; $i++){
            last if ($samples_done == scalar(@{$sampled_params_arr}));
            #add one $PROB per $i, update inits from hash
            my $sh_mod = model::shrinkage_module -> new ( nomegas => $run_model -> nomegas -> [0],
                                                          directory => $run_model -> directory(),
                                                          problem_number => $probnum );

            push(@{$run_model->problems()},
                 model::problem ->
                 new ( directory                   => $run_model->directory,
                       ignore_missing_files        => 1,
                       ignore_missing_output_files => 1,
                       sde                         => $run_model->sde,
                       omega_before_pk             => $run_model->omega_before_pk,
                       psn_record_order            => $run_model->psn_record_order,
                       cwres                       => $run_model->cwres,
                       tbs                         => 0,
                       dtbs                         => 0,
                       prob_arr                    => \@problem_lines,
                       shrinkage_module            => $sh_mod )
                );
            push(@{$run_model->active_problems()},1);
            $run_model->update_inits(from_hash => $sampled_params_arr->[$samples_done],
                                     problem_number=> $probnum,
                                     match_labels => $match_labels,
                                     ignore_missing_parameters => $ignore_missing_parameters);
            $samples_done++;
            $probnum++;
        }
        $run_model -> _write();
        push(@modelsarr,$run_model);
    }

    return \@modelsarr;

}

sub shrinkage_modules
{
    my $self = shift;
    my $parm = shift;

    if( defined $parm ){
        if( ref $parm ne 'ARRAY' or not ( scalar @{$parm} == scalar @{$self->problems} ) ) {
            croak('New number of shrinkage modules must be equal to number of problems' );
        }
        my $probnum = 0;
        foreach my $prob ( @{$self->problems} ) {
            $probnum++;
            my $new_module = shift( @{$parm} );
            if (defined $new_module) {
                $new_module -> nomegas( $self -> nomegas() -> [$probnum-1] );
                $new_module -> directory( $self -> directory() );
                $new_module -> problem_number( $probnum );
                $prob -> shrinkage_module( $new_module );
            }
        }

    } else {
        my @return_array;
        foreach my $prob( @{$self->problems} ){
            push( @return_array, $prob -> shrinkage_module );
        }
        return \@return_array;
    }
}

sub set_outputfile
{
    my $self = shift;

      $self->outputs(
          [ output->new(filename => $self->outputfile, parse_output => $self->parse_output, ignore_missing_files => ($self->ignore_missing_files || $self->ignore_missing_output_files)) ]
      );
}

sub create_output_filename
{
    # Create the name of the output file given the model filename
    my $self = shift;
    my %parm = validated_hash(\@_,
         model_name => { isa => 'Str', default => $self->filename },
         MX_PARAMS_VALIDATE_NO_CACHE => 1,
    );
    my $model_name = $parm{'model_name'};

    my $filename = $model_name;

    if ($filename =~ /\.mod$/) {
        $filename =~ s/\.mod$/.lst/;
    } else {
        my $dotless_model_filename = $filename;
        #this regex must be the same as used in modelfit.pm, for consistency
        $dotless_model_filename =~ s/\.[^.]+$//; #last dot and extension
        $filename = $dotless_model_filename.'.lst';
    }

    return $filename;
}

sub add_records
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         type => { isa => 'Str', optional => 0 },
         record_strings => { isa => 'ArrayRef[Str]', optional => 0 },
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
    );
    my $type = $parm{'type'};
    my @record_strings = defined $parm{'record_strings'} ? @{$parm{'record_strings'}} : ();
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();

    unless(scalar(@problem_numbers)>0 ){
    $self->problems([]) unless defined $self->problems;
      @problem_numbers = (1 .. $#{$self->problems}+1);
    }

    my @problems = @{$self->problems};
    foreach my $i ( @problem_numbers ) {
      if ( defined $problems[ $i-1 ] ) {
        $problems[$i-1] -> add_records( 'type' => $type,
                        'record_strings' => \@record_strings );
      } else {
        croak("Problem number $i does not exist.");
      }
    }
}

sub copy
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              directory => { isa => 'Str', optional => 1 },
                              filename => { isa => 'Str', optional => 0 },
                              write_copy => { isa => 'Bool', default => 1, optional => 1 },
                              copy_output => { isa => 'Bool', default => 0, optional => 1 },
                              copy_etas => { isa => 'Bool', default => 0, optional => 1 },
                              copy_datafile => { isa => 'Bool', default => 0, optional => 1 },
                              copy_msfi => { isa => 'Bool', default => 0, optional => 1 },
                              output_same_directory => { isa => 'Bool', default => 0, optional => 1 },
                              update_shrinkage_tables => { isa => 'Bool', default => 1, optional => 1 },
                              MX_PARAMS_VALIDATE_NO_CACHE => 1,
    );
    my $directory = $parm{'directory'};
    my $filename = $parm{'filename'};
    my $write_copy = $parm{'write_copy'};
    my $copy_output = $parm{'copy_output'};
    my $copy_etas = $parm{'copy_etas'};
    my $copy_datafile = $parm{'copy_datafile'};
    my $copy_msfi = $parm{'copy_msfi'};
    my $output_same_directory = $parm{'output_same_directory'};
    my $update_shrinkage_tables = $parm{'update_shrinkage_tables'};

    my $new_model;

    ($directory, $filename) = OSspecific::absolute_path( $directory, $filename );
    #if copy datafile is set, the data file(s) of $self's $DATA is copied to 'directory'
    #and relative_data_path is set to true in the copy

    # New copy:

    # save references to own output objects
    my $outputs = $self->outputs;

    my @new_outputs;

    # remove ref to output object to speed up the
    # cloning
    $self->clear_outputs;

    # Clone self into new model object
    $new_model = Storable::dclone( $self );

    # Restore the output objects for self
    $self->outputs($outputs);

    # Set the new file name for the copy
    $new_model->directory( $directory );
    $new_model->filename( $filename );

    # update the shrinkage modules

    my @problems = @{$new_model -> problems};
    for (my $i = 1; $i <= scalar @problems; $i++) {
        if (defined $problems[$i - 1]->shrinkage_module) {
            $problems[$i - 1]->shrinkage_module->nomegas($new_model->nomegas()->[$i - 1]);
        }
    }

    # Copy the output object if so is requested (only one output
    # object defined per model object)
    if ($copy_output == 1) {
      if (defined $outputs) {
        foreach my $output (@{$outputs}) {
          push(@new_outputs, $output->copy);
        }
      }
      $new_model->outputs(\@new_outputs);
    } else {
        my $new_out = $self->create_output_filename(model_name => $filename);
        $new_model->ignore_missing_output_files(1);
        if ($output_same_directory) {
            $new_out = $new_model->directory().$new_out;
        }
        $new_model->outputfile($new_out);
        $new_model->set_outputfile();
    }
    my ($writedir,$modelfile) = OSspecific::absolute_path(undef,$new_model->full_name);
    if ($copy_datafile){
        my $datafiles = $new_model->datafiles(absolute_path => 1); #all problems
        my @new_names =();
        #for each problem get data file name without directory
        for ( my $i = 0; $i < scalar @{$new_model->problems}; $i++ ) {
            #if not exists $writedir.$datafilename then copy data full_name to that file
            #in any case set data file name to new name and relative_data_path
            my ($datadir,$datafile) = OSspecific::absolute_path(undef,$datafiles->[$i]);
            unless (-e $writedir.$datafile){
                if (-e $datadir.$datafile){
                    File::Copy::copy($datadir.$datafile,$writedir.$datafile);
                }else{
                    croak("data file $datadir$datafile does not exist in writing of ".$self->full_name.
                          " to file $filename") unless $new_model->ignore_missing_data;
                }
            }
            push(@new_names,$writedir.$datafile);
        }
        $new_model->datafiles(new_names => \@new_names);
        $new_model->relative_data_path(1);
    }

    if ($copy_etas) {
        my $phi_file = $self->get_or_set_etas_file();
        if (defined $phi_file) {
            File::Copy::copy($phi_file, $directory);
            $new_model->get_or_set_etas_file(new_file => basename($phi_file));
        }
    }

    $new_model->_write(copy_msfi => $copy_msfi) if ($write_copy);
    return $new_model;
}

sub copy_data_setting_ok
{
    #check if setting of copy_data will work
    #-no-copy_data means use absolute path for datafile, and that path must not
    #be longer than 80 characters because of NONMEM
    my $self = shift;
    my %parm = validated_hash(\@_,
         copy_data => { isa => 'Maybe[Bool]', optional => 1 }
    );
    my $copy_data = $parm{'copy_data'};
    #undef means copy_data is true
    my $ok = 1;
    if (defined $copy_data and $copy_data == 0){
        my $files = $self->datafiles(absolute_path => 1);
        foreach my $file (@{$files}){
            if (length($file)>80){
                $ok = 0;
                last;
            }
        }
    }
    return $ok;
}

sub datafiles
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         new_names => { isa => 'ArrayRef', optional => 1 },
         problem_numbers => { isa => 'ArrayRef', optional => 1 },
         absolute_path => { isa => 'Bool', default => 0, optional => 1 }
    );
    my @new_names = defined $parm{'new_names'} ? @{$parm{'new_names'}} : ();
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my $absolute_path = $parm{'absolute_path'};
    my @names;

    # The datafiles method retrieves or sets the names of the
    # datafiles specified in the $DATA record of each problem.
    # If filename is set then a name relative the current *working* directory is assumed unless
    # filename is given with absolute path
    # The problem_numbers argument can be used to control which
    # problem that is affected. If absolute_path is set to 1, the
    # returned file names are given with absolute paths.

    unless( scalar(@problem_numbers)>0 ){
        @problem_numbers = (1 .. $#{$self->problems}+1);
    }
    if ( scalar @new_names > 0 ) {
        my $i = 0;
        foreach my $new_name ( @new_names ) {
            #set filename also changes the directory
            if ((-e $new_name) or $self->ignore_missing_data){
                $self -> problems->[$i]->datas->[0]->set_filename(filename => $new_name);
            }else{
                croak("Setting data to $new_name in ".$self->full_name." but data file does not exist");
            }
            $i++;
        }
    } else {
        foreach my $prob_num ( @problem_numbers ) {
            my $file = $self -> problems->[$prob_num-1]->datas->[0]->get_filename;
            my $dir = $self -> problems->[$prob_num-1]->datas->[0]->get_directory;
            if ( $absolute_path ) {
                push( @names, $dir.$file );
            } else {
                push( @names, $file );
            }
        }
    }

    return \@names;
}

sub set_file
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         new_name => { isa => 'Str', optional => 1 },
         problem_number => { isa => 'Int', default => 0, optional => 1 },
         record => { isa => 'Str', optional => 1 }
    );
    my $new_name = $parm{'new_name'};
    my $problem_number = $parm{'problem_number'};
    my $record = $parm{'record'};
    if ($record eq 'data'){
        croak("illegal to use model->set_file on DATA record, this is a bug");
    }

    my @problem_numbers;
    if ( $problem_number == 0 ){
        $self->problems([]) unless defined $self->problems;
        @problem_numbers = (1 .. $#{$self->problems}+1);
    }else {
        push (@problem_numbers,$problem_number);
    }
    foreach my $num (@problem_numbers){
        $self -> _option_name( position      => 0,
                               record      => $record,
                               problem_number => $num,
                               new_name      => $new_name);

    }
}

sub covariance
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         enabled => { isa => 'ArrayRef[Bool]', optional => 1 },
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
    );
    my @enabled = defined $parm{'enabled'} ? @{$parm{'enabled'}} : ();
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @indicators;

    if ( scalar(@problem_numbers)>0 ){
      if ( $#problem_numbers != $#enabled ) {
        croak("The number of problem_numbers ".($#problem_numbers+1).
              "and enabled/disabled covariance records ".($#enabled+1).
              "do not match" );
      }
    }
    unless( $#problem_numbers > 0 ){
        $self->problems([]) unless defined $self->problems;
      @problem_numbers = (1 .. $#{$self->problems}+1);
    }
    my @problems = @{$self->problems};
    my $j = 0;
        foreach my $i ( @problem_numbers ) {
      if ( defined $problems[ $i-1 ] ) {
        if ( defined $enabled[ $j ] ) {
          $problems[ $i-1 ] -> covariance( enabled => $enabled[ $j ] );
        } else {
          push( @indicators, $problems[ $i-1 ] -> covariance );
        }
      } else {
        croak("Problem number $i does not exist!" );
      }
      $j++;
    }

    return \@indicators;
}

sub eigen
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         enabled => { isa => 'ArrayRef[Bool]', optional => 1 },
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
    );
    my @enabled = defined $parm{'enabled'} ? @{$parm{'enabled'}} : ();
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @indicators;

    $self->problems->[0]->eigen;

    return \@indicators;
}

sub fixed
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         parameter_type => { isa => 'Str', optional => 0 },
         parameter_numbers => { isa => 'ArrayRef', optional => 1 },
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
         new_values => { isa => 'ArrayRef', optional => 1 },
         with_priors => { isa => 'Bool', default => 0, optional => 1 }
    );
    my $parameter_type = $parm{'parameter_type'};
    my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
    my $with_priors = $parm{'with_priors'};
    my @fixed;

      # Sets or gets the 'fixed' status of a (number of)
      # parameter(s). 1 correspond to a parameter being fixed and
      # 0 not fixed. The returned parameter is a reference to a
      # two-dimensional array, indexed by problems and parameter
      # numbers.
      # Valid parameter types are 'theta', 'omega' and 'sigma'.

    @fixed = @{ $self -> _init_attr
              ( parameter_type    => $parameter_type,
            parameter_numbers => \@parameter_numbers,
            problem_numbers           => \@problem_numbers,
            new_values        => \@new_values,
            with_priors       => $with_priors,
            attribute         => 'fix')};

    return \@fixed;
}

sub fillblock_fixed
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        parameter_type => { isa => 'Str', optional => 0 },
        # parameter_numbers => { isa => 'ArrayRef', optional => 1 },
        problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
        # new_values => { isa => 'ArrayRef', optional => 1 },
        # with_priors => { isa => 'Bool', default => 0, optional => 1 }
    );
    my $parameter_type = $parm{'parameter_type'};
    # my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    # my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
    # my $with_priors = $parm{'with_priors'};
    # TODO: support parameter_numbers, new_values and with_priors

    # Sets or gets the same as fixed() above but includes all correlation parameters which
    # might not be in model (which might be considered "fixed" to 0). I.e. the size of the
    # returned vectors are only dependent on amount of variance parameters (or thetas) in model.

    unless ($parameter_type eq 'theta' || $parameter_type eq 'omega' || $parameter_type eq 'sigma') {
        croak "parameter type '$parameter_type' not known";
    }

    # thetas have no blocks, just return
    if ($parameter_type eq 'theta') {
        my $fixed = $self->fixed(
            parameter_type => 'theta',
            # parameter_numbers => \@parameter_numbers,
            problem_numbers => \@problem_numbers,
            # new_values => \@new_values,
            # with_priors => $with_priors,
        );
        return $fixed;
    }

    my $fixed = $self->fixed(
        parameter_type => $parameter_type,
        problem_numbers => \@problem_numbers,
    );
    my $indices = $self->indexes(
        parameter_type => $parameter_type,
        problem_numbers => \@problem_numbers,
    );

    my $fixed_arr = [];
    my $fixed_hash = [];
    for (my $problem=0; $problem<scalar(@{$indices}); $problem++) {
        $fixed_arr->[$problem] = [];
        $fixed_hash->[$problem] = {};

        # fill hash with coord string => fixed
        my $str;
        for (my $i=0; $i<scalar(@{$indices->[$problem]}); $i++) {
            $str = $indices->[$problem]->[$i];
            $fixed_hash->[$problem]->{$str} = $fixed->[$problem]->[$i];
        }
        my @last_coords = $str =~ /(\d+)/g;
        my $last_num = $last_coords[1];

        # fill missing coord strings with implicit fix and push to array
        for (my $i=1; $i<=$last_num; $i++) {
            for (my $j=1; $j<=$i; $j++) {
                $str = $parameter_type eq 'sigma' ? "SIGMA($i,$j)" : "OMEGA($i,$j)";
                unless (defined $fixed_hash->[$problem]->{$str}) {
                    $fixed_hash->[$problem]->{$str} = 1;
                }
                push @{$fixed_arr->[$problem]}, $fixed_hash->[$problem]->{$str};
            }
        }
    }

    return $fixed_arr;
}

sub fixed_or_same
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         parameter_type => { isa => 'Str', optional => 0 },
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
    );
    my $parameter_type = $parm{'parameter_type'};
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @fixed_or_same;


    @fixed_or_same = @{ $self -> _init_attr
                    ( parameter_type    => $parameter_type,
                      parameter_numbers => [],
                      problem_numbers   => \@problem_numbers,
                      new_values        => [],
                      with_priors       => 0,
                      get_same => 0,
                      attribute         => 'fix')};
    #when get_same is false then the values for SAME options will be undef
    if ($parameter_type eq 'sigma' or $parameter_type eq 'omega'){
        #check if same also, i.e. look for undef in @fixed and replace with 1
        #loop problems
        for (my $pi=0; $pi< scalar(@fixed_or_same); $pi++){
            for (my $i=0; $i< scalar(@{$fixed_or_same[$pi]}); $i++){
                $fixed_or_same[$pi]->[$i] = 1 unless (defined $fixed_or_same[$pi]->[$i]);
            }
        }
    }

    return \@fixed_or_same;
}

sub same
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         parameter_type => { isa => 'Str', optional => 0 },
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
    );
    my $parameter_type = $parm{'parameter_type'};
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @same;

    #use attribute fix to find which options are defined at all
    @same = @{ $self -> _init_attr
                   ( parameter_type    => $parameter_type,
                     parameter_numbers => [],
                     problem_numbers   => \@problem_numbers,
                     new_values        => [],
                     with_priors       => 0,
                     get_same => 0,
                     attribute         => 'fix')};
    #when option get_same is false above then the values for SAME options will be undef
    if ($parameter_type eq 'sigma' or $parameter_type eq 'omega'){
        #check if same, i.e. look for undef in fixed and replace with 1, set 0 for all that are defined
        #loop problems
        for (my $pi=0; $pi< scalar(@same); $pi++){
            for (my $i=0; $i< scalar(@{$same[$pi]}); $i++){
                if (defined $same[$pi]->[$i]){
                    $same[$pi]->[$i] = 0;
                }else{
                    $same[$pi]->[$i] = 1;
                }
            }
        }
    }else{
        #does not make sense to call same for theta, but if it happens...
        #loop problems
        for (my $pi=0; $pi< scalar(@same); $pi++){
            for (my $i=0; $i< scalar(@{$same[$pi]}); $i++){
                $same[$pi]->[$i] = 0;
            }
        }

    }

    return \@same;
}

sub idcolumn
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         problem_number => { isa => 'Num', default => 1, optional => 0 }
    );
    my $problem_number = $parm{'problem_number'};
    my $col;

    # Usage:
    #
    #   @idcolumns = @{$modelObject -> idcolumns( problem_numbers => [2,3] );
    #
    # idcolumn returns the idcolumn index in INPUT for the
    # specified problem.

    my $junk_ref;
    ( $junk_ref, $col ) = $self ->
      _get_option_val_pos( name => 'ID',
                   record_name => 'input',
                   problem_numbers => [$problem_number] );

    $col = $col->[0][0];
    croak("ID column was not defined in problem number $problem_number") unless (defined $col);
    return $col;
}

sub idcolumns
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
    );
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @column_numbers;

    # Usage:
    #
    #   @column_numbers = @{$modelObject -> idcolumns( problem_numbers => [2] )};
    #
    # idcolumns returns the idcolumn indexes in the datafile for the
    # specified problems.

    my ( $junk_ref, $col_ref ) = $self -> _get_option_val_pos(
        name            => 'ID',
        record_name     => 'input',
        problem_numbers => \@problem_numbers );

    # There should only be one instance of $INPUT and hence we collapse
    # the two-dim return from _get_option_pos_val to a one-dim array:

    foreach my $prob ( @{$col_ref} ) {
      foreach my $inst ( @{$prob} ) {
          croak("ID column was not defined in problem ".(scalar(@column_numbers)+1)) unless (defined $inst);
          push( @column_numbers, $inst );
      }
    }

    return \@column_numbers;
}

sub ignoresigns
{
    my $self = shift;
    my @ignore;

    # ignoresigns returns the ignore sign set in DATA
    # default is # in NONMEM, but here we have @ instead since includes # but covers more that can never be data lines

    foreach my $prob ( @{$self->problems} ) {
      my @datarecs = @{$prob -> datas};
      if ( defined $datarecs[0] ) {
          if (defined $datarecs[0] -> ignoresign and length($datarecs[0] -> ignoresign)>0){
              push( @ignore, $datarecs[0] -> ignoresign );
          }else{
              push( @ignore, '@' );
          }
      } else {
          push( @ignore, '@' );
      }
    }

    return \@ignore;
}

sub initial_values
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         parameter_type => { isa => 'Str', optional => 1 },
         parameter_numbers => { isa => 'ArrayRef', optional => 1 },
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
         new_values => { isa => 'ArrayRef', optional => 1 },
         add_if_absent => { isa => 'Bool', default => 0, optional => 1 },
         with_priors => { isa => 'Bool', default => 0, optional => 1 },
         get_same => { isa => 'Bool', default => 0, optional => 1 }
    );
    my $parameter_type = $parm{'parameter_type'};
    my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
    my $add_if_absent = $parm{'add_if_absent'};
    my $with_priors = $parm{'with_priors'};
    my $get_same = $parm{'get_same'};
    my @initial_values;

      # initial_values either sets or gets the initial values of
      # the parameter specified in "parameter_type" for each
      # problem specified in problem_numbers. For each element
      # in problem_numbers there must be a reference in
      # parameter_numbers to an array that specify the indices
      # of the parameters in the subproblem for which the initial
      # values are set, replaced or retrieved.
      #
      # The add_if_absent argument tells the method to add an init
      # (theta,omega,sigma) if the parameter number points to a
      # non-existing parameter with parameter number one higher
      # than the highest presently included. Only applicable if
      # new_values are set. Valid parameter types are 'theta',
      # 'omega' and 'sigma'.

    @initial_values = @{ $self -> _init_attr
                 ( parameter_type    => $parameter_type,
                   parameter_numbers => \@parameter_numbers,
                   problem_numbers   => \@problem_numbers,
                   new_values        => \@new_values,
                   attribute         => 'init',
                   get_same          => $get_same,
                   with_priors       => $with_priors,
                   add_if_absent     => $add_if_absent )};

    return \@initial_values;
}

sub labels
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         parameter_type => { isa => 'Str', optional => 1 },
         parameter_numbers => { isa => 'ArrayRef', optional => 1 },
         problem_numbers => { isa => 'ArrayRef[Num]', optional => 1 },
         new_values => { isa => 'ArrayRef', optional => 1 },
         generic => { isa => 'Bool', default => 0, optional => 1 }
    );
    my $parameter_type = $parm{'parameter_type'};
    my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
    my @labels;
    my $generic = $parm{'generic'};

    # Usage:
    #
    #   @labels = @{$modobj -> labels( parameter_type => 'theta' )};
    #
    # This basic usage takes one arguments and returns names
    # of the specified parameter. The parameter_type argument
    # is mandatory. It returns the labels of all parameters of type given by
    # $parameter_type.
    # @labels will be a two-dimensional array:
    # [[label1][label2][label3]...]
    #
    #   $labels -> labels( parameter_type  => 'theta',
    #                      problem_numbers => [2,4] );
    #
    # To get labels of specific problems, the problem_numbers argument can be used.
    # It should be a reference to an array containing the numbers
    # of all problems whos labels should be retrieved.
    #
    #   $modobj -> labels( parameter_type    => 'theta',
    #                      problem_numbers   => [2,4],
    #                      parameter_numbers => [[1,3][4,6]]);
    #
    # The retrieval can be even more specific by using the parameter_numbers
    # argument. It should be a reference to a two-dimensional array, where
    # the inner arrays holds the numbers of the parameters that should be
    # fetched. In the example above, parameters one and three from problem two
    # plus parameters four and six from problem four are retrieved.
    #
    #   $modobj -> labels( parameter_type    => 'theta',
    #                      problem_numbers   => [2,4],
    #                      parameter_numbers => [[1,3][4,6]],
    #                      generic           => 1 );
    #
    # To get generic labels for the parameters - e.g. OM1, OM2_1, OM2 etc -
    # set the generic argument to 1.
    #
    # $modobj -> labels( parameter_type     => 'theta',
    #                     problem_numbers   => [2],
    #                     parameter_numbers => [[1,3]],
    #                     new_values        => [['Volume','Clearance']] );
    #
    # The new_values argument can be used to give parameters new labels. In
    # the above example, parameters one and three in problem two are renamed
    # Volume and Clearance.
    #
    # if record is SAME then indexes will still be returned for those positions
    # if new values is set or parameter_numbers this will problably have bugs

    if ((scalar(@parameter_numbers)>0 or scalar(@new_values)>0)
        and ($parameter_type ne 'theta')){
      croak("only theta can be given new labels / be retrieved by param number");
    }

    #add attribute prior default 0 to init_record
    #label will be undef both if have no label and if record is SAME. Distinguish between them by
    #checking other property also, e.g. init or fix?

    my ( @index, $idx );
    @labels = @{ $self -> _init_attr
               ( parameter_type    => $parameter_type,
             parameter_numbers => \@parameter_numbers,
             problem_numbers           => \@problem_numbers,
             new_values        => \@new_values,
             attribute         => 'label',
             with_priors       => 0)};

    #indexes function is changed to return name plus index, e.g THETA2, OMEGA(2,1)
    @index = @{$self -> indexes( parameter_type => $parameter_type,
                     parameter_numbers => \@parameter_numbers,
                     problem_numbers => \@problem_numbers,
                     with_priors => 0)};


    for ( my $i = 0; $i <= $#labels; $i++ ) {
      #problems
      for ( my $j = 0; $j < scalar @{$labels[$i]}; $j++ ) {
        #parameters
        $idx = $index[$i][$j];
        $labels[$i][$j] = $idx unless ( defined $labels[$i][$j] and not $generic );

      }
      if (scalar @{$labels[$i]} == 0){
          #if $MSFI then no $THETA $OMEGA $SIGMA. if msf file comes from
          #previous problem then we copy labels from previous prob
          if ( (defined $self->problems()) and (defined $self->problems()->[$i])
               and (defined $self->problems()->[$i]->msfis()) and
               ($self->problems()->[$i]->msfis->[0]->get_msfo_from_problem_number > 0)){
              my $source = $self->problems()->[$i]->msfis->[0]->get_msfo_from_problem_number() -1;
              for ( my $j = 0; $j < scalar @{$labels[$source]}; $j++ ) {
                  $labels[$i][$j] = $labels[$source][$j];
              }

          }
      }
    }

    return \@labels;
}

sub get_hash_values_to_labels
{
    my $self = shift;


    my @allparams=();

    foreach my $prob (@{$self->problems}){
        my %allpar;
        foreach my $param ('theta','omega','sigma'){
            my $access = $param.'s';
            my %hash;

            if (defined $prob->$access){
                foreach my $rec (@{$prob->$access}){
                    next unless (defined $rec->options);
                    next if ($rec->same or $rec->prior);
                    foreach my $opt (@{$rec->options}){
                        my $label = $opt->label;
                        unless (defined $label and length($label)>0){
                            $label = $opt->coordinate_string;
                        }
                        my $val = $opt->init;
                        $hash{$label} = $val;
                    }
                }
            }
            $allpar{$param}=\%hash;
        }
        push(@allparams,\%allpar);
    }
    return \@allparams;

}

sub get_values_to_labels
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         category => { isa => 'Str', optional => 0 },
         label_model => { isa => 'Maybe[model]', optional => 1 },
         output_object => { isa => 'Maybe[output]', optional => 1 },
         onlywarn => { optional => 1, default => 0 },
    );
    my $category = $parm{'category'};
    my $label_model = $parm{'label_model'};
    my $output_object = $parm{'output_object'};
    my $onlywarn = $parm{'onlywarn'};
    my @out_values;

    # Usage:$modobj -> get_values_to_labels ( category => $categ);
    # return array of arrays of arrays of values that belong to the strings returned from labels
    # works for theta, omega, sigma, setheta, seomega, sesigma,
    # cvsetheta, cvseomega, cvsesigma, comega, csigma

    unless (defined $output_object){
        $output_object = $self -> outputs -> [0];
    }
    unless (defined $onlywarn){
        $onlywarn = 0;
    }
    unless (defined $output_object){
     croak("get_values_to_labels can only be called where output object exists");
    }
    my $error = $output_object ->load;
    if ( ($error) && ($onlywarn) ) {
        carp("model->get_values_to_labels cannot be run, output file error: \n".$error);
        my $empty = [];
        return $empty;
    } elsif ( $error ) {
        croak("model->get_values_to_labels cannot be run, output file error: \n".$error);
    }

    #zeros may be present, they are ambigous (really 0 or not estimated)
    #no padding ever
    my $param = $category;

    #prefix can be cvse, c, se or none
    $param =~ s/^cvse// ;
    $param =~ s/^c// ;
    $param =~ s/^se// ;

    #one element per problem
    my @coordinates;
    if (defined $label_model){
      @coordinates = @{$label_model -> labels( parameter_type => $param, generic => 1)}; #without se
    }else{
      @coordinates = @{$self -> labels( parameter_type => $param, generic => 1)}; #without se
    }
    my $access = $category.'coordval'; #with se, if it is there
    #array over problems of array over subproblems of arrays of values
    my $valref = $output_object -> $access ();
    croak("No accessor $access for output object") unless (defined $valref);
    my @from_coordval = @{$valref};

    for ( my $i = 0; $i <= $#coordinates; $i++ ) {
        #loop over problems
        if (defined $from_coordval[$i] and defined $coordinates[$i]) {
            my @prob_values = ();
            my @coords = @{$coordinates[$i]};
            foreach my $hashref (@{$from_coordval[$i]}) {
                #loop subprobs
                if (defined $hashref){
                    my @values =();
                    my %coordval = %{$hashref};
                    foreach my $coord (@coords){
                        if (defined $coordval{$coord}){
                            push (@values,$coordval{$coord});
                        }else {
                            # only not stored are undefs/NA
                            ui -> print( category => 'all',
                                         message  => "undefined value from labels(), bug!\n") unless (defined $coord);
                            push (@values,undef);
                        }
                    }
                    push (@prob_values,\@values);
                }else{
                    push (@prob_values,undef);
                }
            }
            push (@out_values,\@prob_values);
        }else {
            push (@out_values,undef);
        }
    }

    return \@out_values;
}

sub get_coordslabels
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         parameter_type => { isa => 'Str', optional => 1 },
         problem_numbers => { isa => 'ArrayRef[Num]', optional => 1 },
         with_priors => { isa => 'Bool', default => 0, optional => 1 }
    );
    my $parameter_type = $parm{'parameter_type'};
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my $with_priors = $parm{'with_priors'};
    my @coordslabels;

    # Usage:
    #
    #   @coordslabels = @{$modobj -> get_coordslabels( parameter_type => 'theta' )};
    #
    # This basic usage takes one argument and returns matched coords and labels
    # of the specified parameter. The parameter_type argument
    # is mandatory. It returns coords and labels of all parameters of type given by
    # $parameter_type. If label is undefined it will be set to coordinate string
    # (THETA1, OMEGA(1,1)....
    #
    # @coordslabels will be an array of hashes:
    # [hash1 hash2 ...]
    #
    #   $modobj -> get_coordslabels( parameter_type  => 'theta',
    #                      problem_numbers => [2,4] );
    #
    # To get_coordslabels of specific problems, the problem_numbers argument can be used.
    # It should be a reference to an array containing the numbers
    # of all problems whos coordslabels should be retrieved.
    #
    #   $modobj -> get_coordslabels( parameter_type    => 'theta',
    #                      problem_numbers   => [2,4]);
    #

    unless( scalar @problem_numbers > 0 ){
        $self->problems([]) unless defined $self->problems;
      @problem_numbers = (1 .. $#{$self->problems}+1);
    }
    my @problems = @{$self->problems};

    foreach my $prob (@problem_numbers){
      my $prob_index = $prob -1;
      my %hash;
      if ( defined $problems[ $prob_index ] ) {
        my $accessor = $parameter_type.'s';
        unless( $problems[ $prob_index ] -> can($accessor) ){
          croak("Error unknown parameter type: $parameter_type" );
        }
        my $ref =  eval( '$problems[ $prob_index ] -> '.$accessor.'()' );
        unless (defined $ref){
          push (@coordslabels,\%hash);
          next;
        }
        my  @records = @{$ref};
        foreach my $record ( @records ) {
          next if ($record->prior() and (not $with_priors));
          unless ( $record -> same() ) {
        if ( defined $record -> options ) {
          foreach my $option ( @{$record -> options} ) {
            next if ($option->prior() and (not $with_priors));
            my $coord = $option -> coordinate_string();
            my $label = $coord;
            if (defined $option -> label() and length($option -> label())>0){
                $label = $option -> label();
            }
            unless (defined $coord){
              croak("Error undefined coordinate string" );
            }
            $hash{$coord} = $label; #if not label defined then this will be coord-coord
          }
        }
          }
        }
      } else {
        croak("Problem number $prob does not exist!" );
      }
      push (@coordslabels,\%hash);
    }

    return \@coordslabels;
}

sub get_rawres_parameter_indices
{
    #this is a static method, no shift
    my %parm = validated_hash(\@_,
                              filename => { isa => 'Maybe[Str]', optional => 1 },
                              rawres_filename => { isa => 'Maybe[Str]', optional => 1 },
                              directory => { isa => 'Maybe[Str]', optional => 1 },
                              model_number => { isa => 'Int', optional => 1, default => 1 }
        );
    my $filename = $parm{'filename'};
    my $rawres_filename = $parm{'rawres_filename'};
    my $directory = $parm{'directory'};
    my $model_number = $parm{'model_number'};

    my $full_name;
    if (defined $filename){
        if (defined $directory){
            $full_name = File::Spec->catfile($directory,$filename);
        }else{
            $full_name = $filename;
        }
    }elsif (defined $directory){
        $full_name = $directory.'/raw_results_structure';
    }elsif (defined $rawres_filename){
        my ($volume,$dir,$file) = File::Spec->splitpath($rawres_filename );
        $full_name = File::Spec->catpath($volume,$dir,'raw_results_structure');
    }else{
        croak ("neither filename, rawres_filename or directory defined as input to model::get_rawres_structure");
    }
    unless (-e $full_name){
        return undef;
    }
    my $structure = ext::Config::Tiny -> read($full_name);
    my %indices;
    foreach my $param ('theta','omega','sigma'){
        my ($start,$len) = split(/,/,$structure -> {$model_number}->{$param});
        $indices{$param} = [];
        for (my $i=0; $i< $len; $i++){
            push(@{$indices{$param}},($i+$start));
        }
    }
    return \%indices;

}

sub get_rawres_params
{
    #static method, no shift
    my %parm = validated_hash(\@_,
                              model => { isa => 'Maybe[model]', optional => 1 },
                              filename => { isa => 'Str', optional => 0 },
                              filter => { isa => 'ArrayRef[Str]', optional => 1 },
                              string_filter => { isa => 'Maybe[ArrayRef[Str]]', optional => 1 },
                              extra_columns => { isa => 'Maybe[ArrayRef[Str]]', optional => 1 },
                              require_numeric_ofv => { isa => 'Bool', default => 0, optional => 1 },
                              only_first_match => { isa => 'Bool', default => 0, optional => 1 },
                              offset => { isa => 'Int', optional => 0 },
                              rawres_structure_filename => { isa => 'Maybe[Str]', optional => 1 },
                              rawres_model_number => { isa => 'Maybe[Int]', optional => 1, default => 1 }
        );
    my $model = $parm{'model'};
    my $filename = $parm{'filename'};
    my @filter = defined $parm{'filter'} ? @{$parm{'filter'}}: ();
    my @string_filter = defined $parm{'string_filter'} ? @{$parm{'string_filter'}} : ();
    my @extra_columns = defined $parm{'extra_columns'} ? @{$parm{'extra_columns'}} : ();
    my $require_numeric_ofv = $parm{'require_numeric_ofv'};
    my $offset = $parm{'offset'};
    my $rawres_structure_filename = $parm{'rawres_structure_filename'};
    my $rawres_model_number = $parm{'rawres_model_number'};
    my $only_first_match = $parm{'only_first_match'};

    my @allparams;
    my $extra_count = scalar(@extra_columns);

    #input is filename + offset and possibly array filter and possibly array string_filter
    #input require_numeric_ofv is special filter, default false, if true then check that usable_number(ofv)
    #input
    #output is hash of arrays of hashes allparams

    my %thetapos;
    my %omegapos;
    my %sigmapos;

    croak("file $filename does not exist") unless ( -e $filename );

    open( RRES, $filename) or die "could not open $filename";
    my @read_file = <RRES>;
    close( RRES );
    my @file;

    foreach (@read_file){
        chomp;
        #remove all windows line feed also if we are on unix but use a windows raw results
        s/\r//g;
        if (/\"/ ){
            #if any quotes at all
            #remove one column header at a time, check for each if enclosed in double quotes or not
            my $header = $_;
            my @tmp =();
            while (length($header)>0){
                $header =~ s/^\s*//; #remove leading whitespace
                my $col;
                if ($header =~ /^\"/){
                    #enclosed double quotes, handle more than one in a row
                    if ($header =~ /^\"+([^"]+)\"+\s*\,?/){
                        $header =~ s/^\"+([^"]+)\"+\s*\,?//; #"
                        $col = $1;
                    }else{
                        croak("Failed parsing the header of the rawres input file\n$header");
                    }
                }else{
                    #no quotes
                    $header =~ s/([^,]+)\,?// ; #"
                    $col = $1;
                }
                # we allow empty matches
                push(@tmp,$col);
            }
            push (@file,\@tmp);
        } else {
            my @tmp = split(',',$_);
            push (@file,\@tmp);
        }
    }

    my $ref = shift @file;
    my @header = @{$ref};

    my @thetalabels;
    my @omegalabels;
    my @sigmalabels;
    if (defined $model){
        @thetalabels = @{$model -> labels( parameter_type => 'theta', generic => 0)};
        @omegalabels = @{$model -> labels( parameter_type => 'omega', generic => 0)};
        @sigmalabels = @{$model -> labels( parameter_type => 'sigma', generic => 0)};
    }else{
        my $indices = get_rawres_parameter_indices(filename =>$rawres_structure_filename,
                                                   model_number => $rawres_model_number,
                                                   rawres_filename => $filename);
        $thetalabels[0]=[];
        $omegalabels[0]=[];
        $sigmalabels[0]=[];
        foreach my $j (@{$indices->{'theta'}}){
            push(@{$thetalabels[0]},$header[$j]);
        }
        foreach my $j (@{$indices->{'omega'}}){
            push(@{$omegalabels[0]},$header[$j]);
        }
        foreach my $j (@{$indices->{'sigma'}}){
            push(@{$sigmalabels[0]},$header[$j]);
        }
    }

    if (scalar(@thetalabels) != 1 or scalar(@omegalabels) != 1 or scalar(@sigmalabels) != 1){
        croak("get_rawres_params can only be done if exactly one \$PROB");
    }
    unless (defined $thetalabels[0] and defined $omegalabels[0] and defined $sigmalabels[0]){
        croak("all labels references are not defined in get_rawres_params");
    }

    my $sum = scalar(@{$thetalabels[0]})+scalar(@{$omegalabels[0]})+scalar(@{$sigmalabels[0]});
    $sum += scalar(@filter); #@filter is always defined, but may be empty - bug, may count some cols twice here
    $sum += 1;    #need model also. bug if filter is on model col
    unless (scalar(@header) >= $sum and (($header[0] eq 'model') or ($header[1] eq 'model') or ($header[2] eq 'model')) ){
        ui -> print( category => 'all',
                     message  => "\n\nThe found headers are\n".join("   ",@header)."\n\n");

        croak("The file $filename does not follow the format rules.\n".
              "Either first, second or third column should be model, you have ".$header[0].", ".$header[1]." and ".$header[2].
              ", need $sum cols and have ".scalar(@header)."\n");
    }
    if (($header[0] eq 'hypothesis') and ($offset == 1)){
        ui -> print( category => 'all',
                     message  => "\nWarning: Your rawres_input file looks like an sse raw results file,\n".
                     "but you use offset_rawres=1 which is the default suitable for bootstrap\n".
                     "raw results files. If you want to include also the first model\n".
                     "from the raw results file then rerun with offset_rawres=0.\n\n");
    }

    #parse filter
    my ($ref1,$ref2,$ref3);
    ($ref1,$ref2,$ref3) = setup_filter(filter => \@filter, header => \@header)
        if (scalar(@filter)>0);
    my @filter_column_index = @{$ref1} if (defined $ref1);
    my @filter_relation = @{$ref2} if (defined $ref2);
    my @filter_value = @{$ref3} if (defined $ref3);
    if (scalar(@string_filter)>0){
        my ($r1,$r2,$r3) = setup_filter(filter => \@string_filter, header => \@header, string_filter =>1);
        push(@filter_column_index,@{$r1}) if (defined $r1);
        push(@filter_relation,@{$r2}) if (defined $r3);
        push(@filter_value,@{$r3}) if (defined $r3);
    }

    my $pos=-1;
    my $ofvindex=-1;
    my $modelindex=-1;
    my @extra_indices = (-1) x $extra_count;
    #scan for ofv label and first theta label. Then following should be rest of theta,omega,sigma
    #sometimes first theta label is identical to other label, so only set $pos once
    for (my $i=0; $i<scalar(@header);$i++){
        if ($header[$i] eq 'ofv'){
            $ofvindex = $i;
        }elsif ($header[$i] eq 'model'){
            $modelindex = $i;
        }elsif ($header[$i] eq $thetalabels[0]->[0]){
            $pos = $i if ($pos == -1); #do not match multiple times
        }elsif($extra_count > 0){
            for (my $j=0; $j< $extra_count; $j++){
                if ($header[$i] eq $extra_columns[$j]){
                    $extra_indices[$j] = $i;
                    last;
                }
            }
        }
    }
    if ($pos == -1){
        croak("could not find header ".$thetalabels[0]->[0]." in rawres ".
              "header\n".join(' ',@header)."\n");
    }
    if (($ofvindex == -1) and ($require_numeric_ofv)){
        croak("could not find header ofv in rawres ".
              "header\n".join(' ',@header)."\n");
    }
    if($extra_count > 0){
        for (my $j=0; $j< $extra_count; $j++){
            if ($extra_indices[$j] == -1){
                croak("could not find header ".$extra_columns[$j]." in rawres ".
                      "header\n".join(' ',@header)."\n");
            }
        }
    }

    my %labels_hash;
    $labels_hash{'labels'}=[];
    $labels_hash{'param'}=[];;

    foreach my $lab (@{$thetalabels[0]}){
        if (defined $thetapos{$lab}){
            croak("More than one THETA has label $lab, this is not supported when when reading a raw results file");
        }
        $thetapos{$lab} = $pos;
        $pos++;
        push(@{$labels_hash{'labels'}},$lab);
        push(@{$labels_hash{'param'}},'theta');
    }
    foreach my $lab (@{$omegalabels[0]}){
        if (defined $omegapos{$lab}){
            croak("More than one OMEGA has label $lab, this is not supported when reading a raw results file");
        }
        $omegapos{$lab} = $pos;
        $pos++;
        push(@{$labels_hash{'labels'}},$lab);
        push(@{$labels_hash{'param'}},'omega');
    }
    foreach my $lab (@{$sigmalabels[0]}){
        if (defined $sigmapos{$lab}){
            croak("More than one SIGMA has label $lab, this is not supported when reading a raw results file");
        }
        $sigmapos{$lab} = $pos;
        $pos++;
        push(@{$labels_hash{'labels'}},$lab);
        push(@{$labels_hash{'param'}},'sigma');
    }

    #skip the offset first lines of @file
    for (my $i=0; $i< $offset; $i++){
        my $dirt = shift @file;
    }
    #loop through remaining lines, check if should be filtered out or saved to result hash
    foreach my $line (@file) {
        my $skip = 0;
        if (scalar(@$line) == 0) {
            $skip = 1;
        } elsif ($require_numeric_ofv and (not math::usable_number($line->[$ofvindex]))) {
            $skip = 1;
        } else {
            for (my $i = 0; $i < scalar(@filter_column_index); $i++) {
                my $val = $line->[$filter_column_index[$i]];
                if ($filter_relation[$i] =~ /(==|!=|>|<)/){
                    #numeric relation
                    if (($val eq 'NA') or ($val eq '')) {
                        $skip = 1;
                        last;
                    } elsif(not math::usable_number($val)) {
                        ui -> print( category => 'all',
                                     message  => "\nError: value $val in input filter column ".
                                     $header[$filter_column_index[$i]]." does not look numeric. All input ".
                                     "filter columns must be numeric, skipping this line\n");
                        $skip=1;
                        last;
                    }
                }
                #if we get here then $val was ok
                my $string;
                if ($filter_relation[$i] =~ /(==|!=|>|<)/) {
                    #numeric relation
                    $string = $val . $filter_relation[$i] . $filter_value[$i];
                } else {
                    $string = "\'" . $val . "\' $filter_relation[$i] \'" . $filter_value[$i] . "\'";
                }
                unless (eval($string)) {
                    $skip = 1;
                    last;
                }
            }
        }
        next if ($skip);
        my %theta;
        my %omega;
        my %sigma;
        foreach my $label (keys %thetapos){
            my $val = $line->[$thetapos{$label}];
            unless (math::usable_number($val) ){
                $skip =1;
                ui -> print( category => 'all',
                             message  => "\nWarning rawres input: $val in column $label does not look like a parameter value, skipping line\n");
            }
            $theta{$label} = $val;
        }
        foreach my $label (keys %omegapos){
            my $val = $line->[$omegapos{$label}];
            unless (math::usable_number($val) ){
                $skip =1;
                ui -> print( category => 'all',
                             message  => "\nWarning rawres input: $val in column $label does not look like a parameter value, skipping line\n");
            }
            $omega{$label} = $val;
        }
        foreach my $label (keys %sigmapos){
            my $val = $line->[$sigmapos{$label}];
            unless (math::usable_number($val) ){
                $skip =1;
                ui -> print( category => 'all',
                             message  => "\nWarning rawres input: $val in column $label does not look like a parameter value, skipping line\n");
            }
            $sigma{$label} = $val;
        }
        next if ($skip);

        my %allpar;
        $allpar{'theta'} = \%theta;
        $allpar{'omega'} = \%omega;
        $allpar{'sigma'} = \%sigma;
        if ($require_numeric_ofv){
            $allpar{'ofv'} = $line->[$ofvindex];
        }
        if ($modelindex >= 0){
            $allpar{'model'} = $line->[$modelindex];
        }
        if($extra_count > 0){
            for (my $j=0; $j< $extra_count; $j++){
                my $ind = $extra_indices[$j];
                my $lab = $extra_columns[$j];
                $allpar{$lab} = $line->[$ind];
            }
        }
        push (@allparams,\%allpar);
        last if $only_first_match;
    }

    return (\@allparams,\%labels_hash);
}

sub create_vectorsamples
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              sampled_params_arr => { isa => 'ArrayRef[HashRef]', optional => 0 }
        );
    my $sampled_params_arr = $parm{'sampled_params_arr'};
    my @vectorsamples=();

    my %labels;
    foreach my $param ('theta','omega','sigma'){
        $labels{$param} = $self -> labels(parameter_type => $param,problem_numbers => [1])->[0];
    }

    for (my $k=0; $k < scalar(@{$sampled_params_arr}); $k++){
        my @line =();
        foreach my $param ('theta','omega','sigma'){
            #loop labels in right order...
            foreach my $label (@{$labels{$param}}){
                push(@line,$sampled_params_arr->[$k]->{$param}->{$label});
            }
        }
        push(@vectorsamples,\@line);
    }

    return \@vectorsamples;
}

sub setup_filter
{
    #this is a static method, no shift
    my %parm = validated_hash(\@_,
         filter => { isa => 'ArrayRef[Str]', optional => 1 },
         string_filter => { isa => 'Bool', default => 0, optional => 1 },
         header => { isa => 'ArrayRef[Str]', optional => 1 }
    );
    my @filter = defined $parm{'filter'} ? @{$parm{'filter'}} : ();
    my $string_filter = $parm{'string_filter'};
    my @header = defined $parm{'header'} ? @{$parm{'header'}} : ();
    my @filter_column;
    my @filter_relation;
    my @filter_value;

    #input is array filter and possibly array header
      #boolean string_filter, default false
    #output is @filter_column,@filter_relation,@filter_value

    foreach my $filt (@filter){
      $filt =~ s/(\.gt\.|\.lt\.|\.eq\.|\.ne\.)(.*)$//;
      my $rel;
      if ($1 eq '.eq.'){
          if ($string_filter){
          $rel = 'eq'; #space important
          }else{
          $rel ='==';
          }
      }elsif ($1 eq '.ne.'){
          if ($string_filter){
          $rel = 'ne'; #spaces important
          }else{
          $rel ='!=';
          }
      }elsif ($1 eq '.gt.'){
          if ($string_filter){
          ui -> print( category => 'all',
                       message  => "error in string filter, relation .gt. not allowed for strings. Ignoring filter $filt\n");
          next;
          }
        $rel = '>';
      }elsif ($1 eq '.lt.'){
          if ($string_filter){
          ui -> print( category => 'all',
                       message  => "error in string filter, relation .lt. not allowed for strings. Ignoring filter $filt\n");
          next;
          }
        $rel = '<';
      }else{
        ui -> print( category => 'all',
                     message  => "Error identifying relation $1, ignoring filter $filt\n");
        next;
      }
      my $val = $2;
      if ((not math::usable_number($val)) and (not $string_filter)){
          ui -> print( category => 'all',
                       message  => "Error in filter analysis, value $val does not look like a number. Ignoring filter $filt\n");
          next;
      }
      my $col = $filt;
      my $pos=-1;
      if (scalar(@header)>0){
        #scan for filter column
        for (my $i=0; $i<scalar(@header);$i++){
          if ($header[$i] eq $col){
        $pos = $i;
        last;
          }
        }
        if ($pos == -1){
          croak("could not find filter header $col in ".
             "header\n".join(' ',@header)."\n");
        }
        #store index
      }else{
        $pos = $col;
        #store string
      }
      push(@filter_column,$pos);
      push(@filter_relation,$rel);
      push(@filter_value,$val);
    }

    return \@filter_column ,\@filter_relation ,\@filter_value;
}

sub get_covariance_params
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         filename => { isa => 'Str', optional => 0 },
         samples => { isa => 'Int', optional => 0 }
    );
    my $filename = $parm{'filename'};
    my $samples = $parm{'samples'};
    my @allparams;

    #input is filename, samples
    #output is hash of arrays of hashes allparams
    #will take parameter estimates from this model object as means,
    #assuming any update_inits from output_object or output_file
    #has already been performed

    my @thetalabels = @{$self -> labels( parameter_type => 'theta', generic => 0)};
    my @omegalabels = @{$self -> labels( parameter_type => 'omega', generic => 0)};
    my @sigmalabels = @{$self -> labels( parameter_type => 'sigma', generic => 0)};
    if (scalar(@thetalabels) != 1 or scalar(@omegalabels) != 1 or scalar(@sigmalabels) != 1){
      croak("get_covariance_params can only be done if exactly one \$PROB");
    }
    unless (defined $thetalabels[0] and defined $omegalabels[0] and defined $sigmalabels[0]){
      croak("all labels references are not defined in get_covariance_params");
    }

    #indexes function is changed to return name plus index, e.g THETA2, OMEGA(2,1)
    #returns array over problems
    my @thetacoords = @{$self -> indexes( parameter_type => 'theta',
                     problem_numbers => [1] )};
    my @omegacoords = @{$self -> indexes( parameter_type => 'omega',
                     problem_numbers => [1] )};
    my @sigmacoords = @{$self -> indexes( parameter_type => 'sigma',
                     problem_numbers => [1] )};

    unless (defined $thetacoords[0] and defined $omegacoords[0] and defined $sigmacoords[0]){
      croak("all coords references are not defined in get_covariance_params");
    }
    my @thetainits = @{$self-> initial_values(parameter_type    => 'theta',
                          problem_numbers   => [1])};
    my @omegainits = @{$self-> initial_values(parameter_type    => 'omega',
                          problem_numbers   => [1])};
    my @sigmainits = @{$self-> initial_values(parameter_type    => 'sigma',
                          problem_numbers   => [1])};

    my %coordsinits;
    my %coordslabels;
    my %coordstype;
    for (my $i=0;$i<scalar(@{$thetainits[0]});$i++){
      $coordsinits{$thetacoords[0]->[$i]} = $thetainits[0]->[$i];
      $coordslabels{$thetacoords[0]->[$i]} = $thetalabels[0]->[$i];
      $coordstype{$thetacoords[0]->[$i]} = 'theta';
    }
    for (my $i=0;$i<scalar(@{$omegainits[0]});$i++){
      $coordsinits{$omegacoords[0]->[$i]} = $omegainits[0]->[$i];
      $coordslabels{$omegacoords[0]->[$i]} = $omegalabels[0]->[$i];
      $coordstype{$omegacoords[0]->[$i]} = 'omega';
    }
    for (my $i=0;$i<scalar(@{$sigmainits[0]});$i++){
      $coordsinits{$sigmacoords[0]->[$i]} = $sigmainits[0]->[$i];
      $coordslabels{$sigmacoords[0]->[$i]} = $sigmalabels[0]->[$i];
      $coordstype{$sigmacoords[0]->[$i]} = 'sigma';
    }

    unless (defined $thetainits[0] and defined $omegainits[0] and defined $sigmainits[0]){
      croak("all inits references are not defined in get_covariance_params");
    }

    croak("file $filename does not exist") unless ( -e $filename );

    open( RRES, $filename) or die "could not open $filename";
    my @read_file = <RRES>;
    close( RRES );
    my @covar=();
    my @coordarr=();
    my @header=();
    my $found_table=0;
    my $found_name=0;
    my %remove_columns;
    foreach (@read_file){
      chomp;
      if (/^TABLE NO/ ){
        @covar=(); #reset when find new table, want the last one
        @coordarr=();
        %remove_columns=();
        $found_table=1;
        next;
      }
      if (/^ NAME/){
        $found_name=1;
        chomp;
        my @tmp = split;#(/\s*/,$_); #$_ on whitespace
        shift(@tmp);
        @header = @tmp;
        next;
      }
      chomp;
      my @tmp = split;#(/\s*/,$_); #$_ on whitespace
      croak("splitting row in $filename on whitespace gave only one item. ".
          "The delimiter in the file must be wrong") if (scalar(@tmp)==1);
      my $coord= shift(@tmp);
      unless ($coord =~ /^(THETA|OMEGA|SIGMA)/){
        croak("$filename does not look like a NM7 covariance file. ".
               "$coord does not look like a label for THETA/OMEGA/SIGMA");
      }
      #need to remove all all-zero rows and columns
      my $all_zeros=1;
      foreach my $val (@tmp){
        $all_zeros = 0 unless ($val == 0);
      }
      if ($all_zeros){
        $remove_columns{$coord}=1;
      }else{
        push(@coordarr,$coord);
        push (@covar,\@tmp);
      }
    }

    if (%remove_columns){
      my @new_matrix =();
      foreach my $row (@covar){
        my @newrow=();
        for (my $i=0;$i<scalar(@{$row});$i++){
          push(@newrow,$row->[$i]) unless
          (defined $remove_columns{$header[$i]});
        }
        push(@new_matrix,\@newrow);
      }
      @covar = @new_matrix;
    }

    unless ($found_table and $found_name){
        croak("$filename does not look like a NM7 covariance file. ".
               "Did not find TABLE NO and NAME lines.");
    }

    my @mean =();
    my @labels=();
    my @type=();
    foreach my $coord (@coordarr){
      if (defined $coordsinits{$coord} and defined $coordslabels{$coord}){
        push(@mean,$coordsinits{$coord});
        push(@type,$coordstype{$coord});
        if (defined $coordslabels{$coord}){
          push(@labels,$coordslabels{$coord});
        }else{
          push(@labels,$coord);
        }
      }else{
        unless ($coord =~ /(OMEGA|SIGMA)\(([\d]+)\,([\d]+)\)/){
          croak("$coord does not look like off-diagonal OMEGA/SIGMA");
        }
        if ($1 == $2){
          croak("$coord is diagonal OMEGA/SIGMA, but not found in model");
        }
        #if off-diagonal sigma or omega ok, set init 0 and label to coord
        push(@mean,0);
        push(@labels,$coord);
      }
    }

    my @newsets = random_multivariate_normal($samples, @mean, @covar);

    foreach my $set (@newsets){
      my %theta;
      my %omega;
      my %sigma;

      for (my $i=0; $i< scalar(@labels);$i++){
        $theta{$labels[$i]} = $set->[$i] if ($type[$i] eq 'theta');
        $omega{$labels[$i]} = $set->[$i] if ($type[$i] eq 'omega');
        $sigma{$labels[$i]} = $set->[$i] if ($type[$i] eq 'sigma');
      }
      my %allpar;
      $allpar{'theta'} = \%theta;
      $allpar{'omega'} = \%omega;
      $allpar{'sigma'} = \%sigma;
      push (@allparams,\%allpar);
    }

    return \@allparams;
}

sub lower_bounds
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         parameter_type => { isa => 'Str', optional => 1 },
         parameter_numbers => { isa => 'ArrayRef', optional => 1 },
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
         new_values => { isa => 'ArrayRef', optional => 1 },
         with_priors => { isa => 'Bool', default => 0, optional => 1 }
    );
    my $parameter_type = $parm{'parameter_type'};
    my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
    my $with_priors = $parm{'with_priors'};
    my @lower_bounds;

    # lower_bounds either sets or gets the initial values of the
    # parameter specified in the argument parameter_type for
    # each problem specified in problem_numbers. See L</fixed>.

    if ($parameter_type eq 'theta'){
        @lower_bounds = @{ $self -> _init_attr
                               ( parameter_type    => $parameter_type,
                                 parameter_numbers => \@parameter_numbers,
                                 problem_numbers           => \@problem_numbers,
                                 new_values        => \@new_values,
                                 with_priors       => $with_priors,
                                 attribute         => 'lobnd')};
    }else{
        #omega or sigma
        if (scalar (@new_values)> 0){
            croak("Trying to set lower bounds for $parameter_type, not allowed");
        }
        #pick up on diagonal first and then change elements to 0 or undef
        @lower_bounds = @{ $self -> _init_attr
                               ( parameter_type    => $parameter_type,
                                 parameter_numbers => \@parameter_numbers,
                                 problem_numbers           => \@problem_numbers,
                                 new_values        => \@new_values,
                                 with_priors       => $with_priors,
                                 attribute         => 'on_diagonal')};
        for (my $prob=0; $prob < scalar(@lower_bounds); $prob++){
            for (my $i=0; $i < scalar(@{$lower_bounds[$prob]}); $i++){
                if ($lower_bounds[$prob]->[$i] == 1){
                    #on diagonal
                    $lower_bounds[$prob]->[$i] = 0;
                }else{
                    $lower_bounds[$prob]->[$i] = undef;
                }
            }
        }
    }

    return \@lower_bounds;
}

sub on_diagonal
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         parameter_type => { isa => 'Str', optional => 1 },
         parameter_numbers => { isa => 'ArrayRef[Num]', optional => 1 },
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
         with_priors => { isa => 'Bool', default => 0, optional => 1 }
    );
    my $parameter_type = $parm{'parameter_type'};
    my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my $with_priors = $parm{'with_priors'};
    my @on_diagonal;

      # on_diagonal gets the values for
      #  argument parameter_type for
      # each problem specified in problem_numbers. See L</fixed>.

    @on_diagonal = @{ $self -> _init_attr
                  ( parameter_type    => $parameter_type,
                parameter_numbers => \@parameter_numbers,
                problem_numbers           => \@problem_numbers,
                with_priors       => $with_priors,
                attribute         => 'on_diagonal')};

    return \@on_diagonal;
}

sub maxeval
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         new_values => { isa => 'ArrayRef', optional => 1 },
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
         exact_match => { isa => 'Bool', default => 0, optional => 1 }
    );
    my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my $exact_match = $parm{'exact_match'};
    my @values;

#      Usage:
#
#        @maxev = @{$modobj -> maxeval};
#
#      This basic usage takes no arguments and returns the value of the
#      MAXEVAL option in the $ESTIMATION record of each problem.
#      @maxev will be a two dimensional array:
#      [[maxeval_prob1][maxeval_prob2][maxeval_prob3]...]
#
#        $modobj -> maxeval( new_values => [[0],[999]];
#
#      If the new_values argument of maxeval is given, the values of the
#      MAXEVAL options will be changed. In this example, MAXEVAL will be
#      set to 0 in the first problem and to 999 in the second.
#      The number of elements in new_values must match the number of problems
#      in the model object $modobj.
#
#        $modobj -> maxeval( new_values => [[0],[999]],
#                            problem_numbers    => [2,4] );
#
#      To set the MAXEVAL of specific problems, the problem_numbers argument can
#      be used. It should be a reference to an array containing the numbers
#      of all problems where the MAXEVAL should be changed or retrieved.
#      If specified, the size of new_values must be the same as the size
#      of problem_numbers.
#

     my ( $val_ref, $junk ) = $self ->
       _option_val_pos( name            => 'MAX',
                record_name     => 'estimation',
                problem_numbers => \@problem_numbers,
                new_values      => \@new_values,
                exact_match     => $exact_match );
     @values = @{$val_ref};

    return \@values;
}

sub set_maxeval_zero
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         problem_number => { isa => 'Int', default => 0, optional => 1 },
         print_warning => { isa => 'Bool', default => 0, optional => 1 },
         last_est_complete => { isa => 'Bool', default => 0, optional => 1 },
         need_ofv => { isa => 'Bool', default => 0, optional => 1 },
         niter_eonly => { isa => 'Maybe[Int]', optional => 1 }
    );
    my $problem_number = $parm{'problem_number'};
    my $print_warning = $parm{'print_warning'};
    my $last_est_complete = $parm{'last_est_complete'};
    my $need_ofv = $parm{'need_ofv'};
    my $niter_eonly = $parm{'niter_eonly'};
    my $success;

    #the intent here is to unset the estimation for a run where we only want
    #to get PRED defined values without changning the initial parameter estimates
    #or get filtered table output after IGNOREs and ACCEPTs.
    #For NONMEM5 and 6 it is very easy, just set MAXEVAL=0
    #for NONMEM7 and single $EST it is almost as easy
    #for NONMEM7 and multiple $EST it is more complicated, the problem being that
    #we need to remove all but the last $EST while options from previous $EST can carry over.

    #if NONMEM5 or 6 just set MAXEVAL=0, done.
    my $n_probs = 0;
    $n_probs = scalar(@{$self -> problems()}) if (defined $self->problems());
    my @problem_numbers;
    #default problem_number is 0 which means all problems
    if ($problem_number > 0){
      push(@problem_numbers,$problem_number);
    } elsif ($problem_number == 0){
      @problem_numbers = 1 .. $n_probs;
    }elsif ($problem_number == -1) {
      push(@problem_numbers,$n_probs);
    }else{
      croak("illegal input problem_number $problem_number");
    }

    $success = 1;
    if ($PsN::nm_major_version == 5 or $PsN::nm_major_version == 6){
      $self -> set_option(record_name => 'estimation',option_name => 'MAXEVALS',
                  fuzzy_match => 1, option_value => '0',
                  problem_numbers => \@problem_numbers);
    } else {
      if (not $last_est_complete) {
        $self -> set_union_estimation_record(problem_numbers => \@problem_numbers,
                         need_ofv => $need_ofv);
      } else {
        #If NONMEM7 and multiple $EST #remove all but last $EST. Continue to next step.
        $self -> remove_records( type => 'estimation', keep_last => 1,
                     problem_numbers => \@problem_numbers);
      }
      foreach my $i (@problem_numbers){
        my $meth = $self-> get_option_value (record_name => 'estimation',option_name=>'METHOD',
                         problem_index => ($i-1), record_index=>0,
                         option_index=>0);
        if ((not defined $meth) or ($meth =~ /^(0|1|ZER|CON|HYB)/ )){
          #undef is ok, default method is classical
          #will get undef also if no estimation at all...
          # if classical method (including no METH which means default) set MAXEVAL=0, done
          $self -> set_option(record_name => 'estimation',option_name => 'MAXEVALS',
                  fuzzy_match => 1, option_value => '0',
                  problem_numbers => [($i)]);
        }elsif ($meth =~ /^IMP/ ){
          # if IMP or IMPMAP set EONLY=1
          $self -> set_option(record_name => 'estimation',option_name => 'EONLY',
                  fuzzy_match => 1, option_value => '1',
                  problem_numbers => [($i)]);
          if (defined $niter_eonly){
        croak("illegal value niter_eonly $niter_eonly") unless ($niter_eonly >= 0);
        $self -> set_option(record_name => 'estimation',option_name => 'NITER',
                    fuzzy_match => 1, option_value => $niter_eonly,
                    problem_numbers => [($i)]);
          } else {
        unless ($need_ofv){
          #need to adjust NITER. set 1 or 0 unless need ofv
          $self -> set_option(record_name => 'estimation',option_name => 'NITER',
                      fuzzy_match => 1, option_value => '0',
                      problem_numbers => [($i)]);
        }
          }
        }else{
          if (defined $niter_eonly){
        croak("illegal value niter_eonly $niter_eonly") unless ($niter_eonly >= 0);
        $self -> set_option(record_name => 'estimation',option_name => 'NITER',
                    fuzzy_match => 1, option_value => $niter_eonly,
                    problem_numbers => [($i)]);
          }
          #if other method return error no success
          #should this be changed to replace last est with something classical?
          $success = 0;
        }
      }
      my $warning = "\n\nMETHOD in last \$EST was not classical nor IMP/IMPMAP. Cannot set MAXEVAL=0 or EONLY=1.\n";
      ui -> print( category => 'all',
                   message  => $warning) if ($print_warning and not $success);
    }

    return $success;
}

sub set_union_estimation_record
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 0 },
         need_ofv => { isa => 'Bool', optional => 0 }
    );
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my $need_ofv = $parm{'need_ofv'};

    #this will not work with CHAIN
    #Take first $EST. Loop over the following $EST.
    #Loop over options in following EST, set each in first EST.
    #set record EST (remove all existing) to union record.
    my @skiplist =('NOTITLE','NOLABEL','FILE','MSFO');
    push (@skiplist,'IACCEPT','PACCEPT','OACCEPT','NSIGDIGITS','SIGDIGITS');
    push (@skiplist,'NBURN');
    push (@skiplist,'THETABOUNDTEST','NOTHETABOUNDTEST','NOTBT');
    push (@skiplist,'OMEGABOUNDTEST','NOOMEGABOUNDTEST','NOOBT');
    push (@skiplist,'SIGMABOUNDTEST','NOSIGMABOUNDTEST','NOSBT');
    push (@skiplist,'MAXEVALS','EONLY'); #will be set anyway
    my @skip_exact =('ISAMPLE_M1','ISAMPLE_M2','ISAMPLE_M3');
    push (@skip_exact,'PSAMPLE_M1','PSAMPLE_M2','PSAMPLE_M3');
    push (@skip_exact,'OSAMPLE_M1','OSAMPLE_M2','OSAMPLE_M3');

    my @problems;
    @problems = @{$self -> problems()} if (defined $self->problems());
    foreach my $probnum (@problem_numbers){
      my $problem = $problems[$probnum-1];
      my @estimations;
      @estimations = @{$problem ->estimations()} if (defined $problem ->estimations());
      next unless (scalar(@estimations)>0);
      my $default_seed = '14455';
      my @namesarray=('LAPLACIAN','INTERACTION','FO','CENTERING');
      my @valuesarray=('NO','NO','NO','NO');
      push(@namesarray,'SLOW','NUMERICAL','POSTHOC','ABORT','REPEAT','REPEAT1','REPEAT2');
      push(@valuesarray,'NO','NO','NO','set','NO','NO','NO');
      push (@namesarray,,'ETABARCHECK','SORT','DF','SEED','MUM');
      push(@valuesarray, 'NO','NO','0',$default_seed,'unset');
      die unless (scalar(@namesarray) == scalar(@valuesarray));
      my @option_strings;
      my $method;
      my $niter;
      my $pred='unset';
      my $isample;
      #value is 'set', 'NO' or 'value' or 'unset'
      foreach my $est (@estimations){
        my @options;
        @options =@{$est->options()} if (defined $est->options());
        foreach my $opt (@options){
          next unless (defined $opt->name());
          my $name = $opt->name();
          my $skip=0;
          foreach my $string (@skiplist){
        if ($name eq $string or (index($string,$name) == 0)){
          $skip=1;
          last;
        }
          }
          next if ($skip);
          foreach my $string (@skip_exact){
        if ($name eq $string ){
          $skip=1;
          last;
        }
          }
          next if ($skip);
          if ($name eq 'METHOD' or (index('METHOD',$name) == 0)){
        croak("METHOD in \$EST requires a value")
            unless (defined $opt->value());
        $method = $opt->value();
        next;
          }
          if ($name eq 'NITER' or (index('NITER',$name) == 0)){
        croak("NITER in \$EST requires a value")
            unless (defined $opt->value());
        $niter = $opt->value();
        next;
          }
          if ($name eq 'ISAMPLE' or (index('ISAMPLE',$name) == 0)){
        croak("ISAMPLE in \$EST requires a value")
            unless (defined $opt->value());
        $isample = $opt->value();
        next;
          }
          if ($name eq 'PREDICTION' or (index('PREDICTION',$name) == 0)){
        $pred = 'unset';
        next;
          }
          if ($name eq 'LIKELIHOOD' or (index('LIKELIHOOD',$name) == 0)){
        $pred = $name;
        next;
          }
          if ($name eq '-2LOGLIKELIHOOD' or (index('-2LOGLIKELIHOOD',$name) == 0)
          or $name eq '-2LLIKELIHOOD' or (index('-2LLIKELIHOOD',$name) == 0)){
        $pred = $name;
        next;
          }

          my $val='set';
          if (defined $opt->value() and ($opt->value() =~ /[^\s]/)){
        #value has something that is not whitespace
        $val = $opt->value();
          }else {
        #we have skipped NOTITLE, and NOPRIOR has a value and will not end up here
        #get rid of leading NO, do matching on the rest
        $val = 'NO' if ($name =~ s/^NO//);
          }

          for (my $i=0; $i< scalar(@namesarray);$i++){
        if (index($namesarray[$i],$name) == 0 ){
          $valuesarray[$i]=$val;
          $skip=1;
          last;
        }
          }
          next if ($skip);

          #not thar many should end up here, but format should
          push(@namesarray,$name);
          push(@valuesarray,$val);
        }
      } #end loop estimations

      my $record_string;
      if (not defined $method){
        #undef is ok, default method is classical
      }elsif ($method =~ /^(0|1|ZER|CON|HYB)/ ){
        $record_string = 'METHOD='.$method; #maxeval handled outside this function
      }elsif ($method =~ /^IMP/ ){
        $niter = 10 unless (defined $niter);
        $record_string = 'METHOD='.$method.' NITER='.$niter; #EONLY handled outside
        $record_string .= ' ISAMPLE='.$isample if (defined $isample);
      }else {
        #set to IMP and hope for the best.
        # if not need ofv set isample to 1, otherwise leave isample to default
        $niter = 10 unless (defined $niter and $niter < 10);
        $record_string = 'METHOD=IMP NITER='.$niter; #EONLY handled outside
        $record_string .= ' ISAMPLE=1' unless ($need_ofv);
      }
      $record_string .= ' '.$pred unless ($pred eq 'unset');

      for (my $i=0;$i<scalar(@namesarray);$i++){
        next if ($valuesarray[$i] eq 'unset');
        if ($valuesarray[$i] eq 'NO'){
          $record_string .= ' NOABORT' if ($namesarray[$i] eq 'ABORT');
        }elsif  ($valuesarray[$i] eq 'set'){
          $record_string .= ' '.$namesarray[$i] unless ($namesarray[$i] eq 'ABORT');
        }elsif ($valuesarray[$i] eq '0'){
          $record_string .= ' '.$namesarray[$i].'=0' unless ($namesarray[$i] eq 'DF');
        }elsif ($valuesarray[$i] eq $default_seed){
          $record_string .= ' '.$namesarray[$i].'=0' unless ($namesarray[$i] eq 'SEED');
        }else{
          $record_string .= ' '.$namesarray[$i].'='.$valuesarray[$i];
        }
      }
      #replace all records
      if (defined $record_string) {
          $self->set_records(problem_numbers=>[$probnum],
              type => 'estimation',
              record_strings => [$record_string]);
      }
    } #end loop problems
}

sub nomegas
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
         with_correlations => { isa => 'Bool', default => 0, optional => 1 },
         with_same => { isa => 'Bool', default => 1, optional => 1 },
         with_priors => { isa => 'Bool', default => 0, optional => 1 }
    );
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @nomegas;
    my $with_correlations = $parm{'with_correlations'};
    my $with_same = $parm{'with_same'};
    my $with_priors = $parm{'with_priors'};

      # returns the number of omegas in the model for the given
      # problem number.
    unless( scalar(@problem_numbers)>0 ){
        $self->problems([]) unless defined $self->problems;
      @problem_numbers = (1 .. $#{$self->problems}+1);
    }

    my @problems = @{$self->problems};
    foreach my $i ( @problem_numbers ) {
      if ( defined $problems[ $i-1 ] ) {
        push( @nomegas, $problems[$i-1]->nomegas(
            with_correlations => $with_correlations,
            with_same => $with_same, with_priors => $with_priors));
      } else {
        croak("Problem number $i does not exist.");
      }
    }

    return \@nomegas;
}

sub nproblems
{
    # nproblems returns the number of problems in the modelobject.
    my $self = shift;

    return scalar @{$self->problems};
}

sub nsigmas
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
         with_correlations => { isa => 'Bool', default => 0, optional => 1 },
         with_same => { isa => 'Bool', default => 1, optional => 1 }
    );
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @nsigmas;
    my $with_correlations = $parm{'with_correlations'};
    my $with_same = $parm{'with_same'};

    # returns the number of sigmas in the model for the given problem number.

    unless( scalar(@problem_numbers)>0 ){
        $self->problems([]) unless defined $self->problems;
        @problem_numbers = (1 .. $#{$self->problems}+1);
    }

    my @problems = @{$self->problems};
    foreach my $i ( @problem_numbers ) {
        if ( defined $problems[ $i-1 ] ) {
            push( @nsigmas, $problems[ $i-1 ] -> nsigmas( with_correlations => $with_correlations,
                    with_same => $with_same));
        } else {
            croak("Problem number $i does not exist.");
        }
    }

    return \@nsigmas;
}

sub nthetas
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         problem_number => { isa => 'Num', default => 1, optional => 1 },
         with_priors => { isa => 'Bool', default => 0, optional => 1 }
    );
    my $problem_number = $parm{'problem_number'};
    my $with_priors = $parm{'with_priors'};
    my $nthetas;

    # returns the number of thetas in the model for the given
    # problem number.

    unless( defined $self->problems()->[$problem_number - 1] ){
      croak("problem number $problem_number does not exist" );
    }

    if((not $with_priors) and
       defined $self->problems()->[$problem_number - 1] -> nwpri_ntheta()){
        $nthetas = $self->problems()->[$problem_number - 1] -> nwpri_ntheta();
    }else{
      $nthetas =
          $self -> _parameter_count( 'record' => 'theta',
                     'problem_number' => $problem_number );
    }

    return $nthetas;
}

sub set_code
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         record => { isa => 'Str' },
         code => { isa => 'ArrayRef[Str]' },
         problem_number => { isa => 'Num', default => 1 }
    );
    my $record = $parm{'record'};
    my $code = $parm{'code'};
    my $problem_number = $parm{'problem_number'};

    # Sets the code for a given problem

    my @prob = @{$self->problems};

    if (not defined $prob[$problem_number - 1]) {
      croak("problem number $problem_number does not exist" );
    }

    my $precord = $record . 's';
    my $record_array = $prob[$problem_number - 1]->$precord;
    if (defined $record_array and scalar @{$record_array} > 0) {
        $record_array->[0]->code($code);
    } else {
        $record = uc($record);
        croak("The \$$record record does not exist");
    }
}

sub get_code
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         record => { isa => 'Str' },
         problem_number => { isa => 'Num', default => 1, optional => 1 }
    );
    my $record = $parm{'record'};
    my $problem_number = $parm{'problem_number'};

    # Gets the code for a given problem
    my @prob = @{$self->problems};

    if (not defined $prob[$problem_number - 1]) {
      croak("problem number $problem_number does not exist");
    }

    my @code;
    my $precord = $record . 's';
    my $record_array = $prob[$problem_number - 1]->$precord;
    if (defined $record_array and scalar @{$record_array} > 0) {
        @code = @{$record_array->[0]->code};
    }

    return \@code;
}

sub has_code
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         record => { isa => 'Str' },
         problem_number => { isa => 'Num', default => 1, optional => 1 }
    );
    my $record = $parm{'record'};
    my $problem_number = $parm{'problem_number'};

    my $code = $self->get_code(record => $record, problem_number => $problem_number);

    if (scalar(@$code) > 0) {
        return 1;
    } else {
        return 0;
    }
}

sub print
{
    my $self = shift;

    # Prints the formatted model to standard out.

    my ( @formatted );
    foreach my $problem ( @{$self->problems} ) {
        foreach my $line (@{$problem->_format_problem(relative_data_path => $self->relative_data_path,
                                                      write_directory => $self->directory)}) {
            print $line;
        }
    }
}

sub record
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         record_name => { isa => 'Str', optional => 1 },
         new_data => { isa => 'ArrayRef[Str]', optional => 1 },
         problem_number => { isa => 'Num', default => 1, optional => 1 }
    );
    my $record_name = $parm{'record_name'};
    my @new_data = defined $parm{'new_data'} ? @{$parm{'new_data'}} : ();
    my $problem_number = $parm{'problem_number'};
    my @data;

     # If the argument new_data is given, record sets new_data in
     # the model objects member specified with record_name. The
     # format of new_data is an array of strings, where each
     # element corresponds to a line of code as it would have
     # looked like in a valid NONMEM modelfile. If new_data is left
     # undefined, record returns lines of code belonging to the
     # record specified by record_name in a format that is valid in
     # a NONMEM modelfile.

     my @problems = @{$self->problems};
     my $records;

    if ( defined $problems[ $problem_number - 1 ] ) {
        if ( scalar(@new_data) > 0 ){
            my $rec_class = "model::problem::$record_name";
            my $record = $rec_class -> new('record_arr' => \@new_data );
        } else {
            $record_name .= 's';
            $records = $problems[ $problem_number - 1 ] -> {$record_name};
            foreach my $record( @{$records} ){
                if ($record_name eq 'datas'){
                    push(@data, $record -> _format_record(relative_data_path => $self->relative_data_path,
                                                          write_directory => $self->directory));
                }else{
                    push(@data, $record -> _format_record);
                }
            }
        }
    }

    return \@data;
}

sub get_option_value
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         record_name => { isa => 'Str', optional => 0 },
         option_name => { isa => 'Str', optional => 0 },
         problem_index => { isa => 'Int', default => 0, optional => 1 },
         record_index => { isa => 'Str', default => 0, optional => 1 },
         option_index => { isa => 'Str', default => 0, optional => 1 },
         fuzzy_match => { isa => 'Bool', default => 1, optional => 1 }
    );
    my $record_name = $parm{'record_name'};
    my $option_name = $parm{'option_name'};
    my $problem_index = $parm{'problem_index'};
    my $record_index = $parm{'record_index'};
    my $option_index = $parm{'option_index'};
    my $fuzzy_match = $parm{'fuzzy_match'};
    my $return_value;

    #$modelObject -> get_option_value(record_name => 'recordName', option_name => 'optionName',
    #                         problem_index => <index>, record_index => <index>/'all',
    #                         option_index => <index>/'all',
    #                         fuzzy_match => 1/0)
    # record_name and option_name are required. All other have default 0. Fuzzy match default 1.
    #record_index and option_index may either be scalar integer or string 'all'.
    # Depending on input parameters the return value can be
    # Case 1. a scalar for record_index => integer, option_index => integer
    # Case 2. a reference to an array of scalars for (record_index=>'all',option_index => integer)
    # Case 3. a reference to an array of scalars for (record_index=>integer,option_index => 'all')
    # Case 4. a reference to an array of references to arrays for (record_index=>'all',option_index => 'all')
    my ( @problems, @records, @options );
    my $accessor = $record_name.'s';
    my @rec_arr;
    my $fail;

    #Basic error checking. Error return type is undef for Case 1
    #and reference to empty array for Case 2 and 3 and 4.

    if (lc($record_index) eq 'all' || lc($option_index) eq 'all' ){
        $fail = [];
    } else {
        $fail =  undef;
    }

    if ( defined $self->problems ) {
        @problems = @{$self->problems};
    } else {
        warn "No problems defined in model";
        return $fail;
    }
    unless( defined $problems[$problem_index] ){
        return $fail;
    }

    if ( defined $problems[$problem_index] -> $accessor ) {
        @records = @{$problems[$problem_index] -> $accessor};
    } else {
        return $fail;
    }

    #go through all records, whole array is of correct type.
    #if current record is the single we want, investigare option values and break out of loop
    #if we want to look at all records, investigare option values and continue with loop
      REC: for (my $ri=0; $ri<scalar(@records); $ri++){
      if ((lc($record_index) eq 'all') || $record_index==$ri){
          my @val_arr = ();
          unless ((defined $records[$ri]) &&( defined $records[$ri] -> options )){
          if (lc($record_index) eq 'all'){
              if (lc($option_index) eq 'all'){
              push(@rec_arr,[]); #Case 4
              } else {
              push(@rec_arr,undef); #Case 2
              }
              next REC;
          } else {
              if (lc($option_index) eq 'all'){
              $return_value = []; #Case 3
              } else {
              $return_value = undef; #Case 1
              }
              last REC; #we are done
          }
          }
          @options = @{$records[$ri] -> options};
          my $oi=-1;
          my $val;
          #go through all options (array contains all options, regardless of name).
          # For each check if it the correct type, if so
          #increase counter $oi after possibly storing the option value
          #if current correct option is the single we want value for, then
          #store value and break out of loop. If want to store values for
          #all correct options, store value and then continue with loop
          foreach my $option ( @options ) {
          if (defined $option and
              (($option->name eq $option_name) || ($fuzzy_match and  index($option_name,$option ->name ) == 0))){
              $oi++; #first is 0
              if (lc($option_index) eq 'all' || $option_index == $oi){
              if ( (defined $option->value) and ($option->value ne '')){
                  $val = $option->value;
              } else {
                  $val = undef;
              }
              if (lc($option_index) eq 'all'){
                  push(@val_arr,$val); #Case 3 and 4
              } else {
                  last; #Case 1 and 2.  Take care of $val outside loop over options
              }
              }
          }
          }
          if (lc($record_index) eq 'all'){
          if (lc($option_index) eq 'all'){
              push(@rec_arr,\@val_arr); #Case 4
          } else {
              push(@rec_arr,$val); #Case 2
          }
          next REC;
          } else {
          if (lc($option_index) eq 'all'){
              $return_value = \@val_arr; #Case 3
          } else {
              $return_value = $val; #Case 1
          }
          last REC;
          }
      }
      }
    if (lc($record_index) eq 'all'){
        $return_value = \@rec_arr; #Case 2 and 4
    }

    return $return_value;
}

sub restore_inits
{
    my $self = shift;

    # restore_inits brings back initial values previously stored
    # using store_inits. This method pair allows a user to store
    # the currents initial values in a backup, replace them with
    # temporary values and later restore them.

    if ( defined $self->problems ) {
      foreach my $problem (@{$self->problems}) {
        $problem->restore_inits;
      }
    }
}

sub set_records
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         type => { isa => 'Str', optional => 0 },
         record_strings => { isa => 'ArrayRef', optional => 0 },
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
    );
    my $type = $parm{'type'};
    my @record_strings = defined $parm{'record_strings'} ? @{$parm{'record_strings'}} : ();
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();

    unless( scalar(@problem_numbers) > 0 ) {
        $self->problems([]) unless defined $self->problems;
      @problem_numbers = (1 .. $#{$self->problems}+1);
    }

    my @problems = @{$self->problems};
    foreach my $i ( @problem_numbers ) {
      if ( defined $problems[ $i-1 ] ) {
        $problems[$i-1] -> set_records( 'type' => $type,
                        'record_strings' => \@record_strings );
      } else {
        croak("Problem number $i does not exist." );
      }
    }
}

sub store_inits
{
    my $self = shift;

    # store_inits stores initial values that can later be
    # brought back using restore_inits. See L</restore_inits>.

    if ( defined $self->problems ) {
      foreach my $problem ( @{$self->problems} ){
        $problem -> store_inits;
      }
    }
}

sub msfi_names
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              absolute_path => { isa => 'Bool', default => 0 }
        );
    my $absolute_path = $parm{'absolute_path'};
    my @names = ();

    unless ( defined $self->problems ) {
        croak("No problems defined in model" );
    }

    for( my $i=0; $i< scalar(@{$self->problems}); $i++){
        if ( defined $self->problems->[$i] -> msfis and scalar(@{$self->problems->[$i] -> msfis})>0) {
            if ($absolute_path){
                push( @names, $self->problems->[$i]->msfis->[0]->get_absolute_filename );
            }else{
                push( @names, $self->problems->[$i]->msfis->[0]->get_filename );
            }
            if (scalar(@{$self->problems->[$i] -> msfis})>1){
                ui->print(category => 'all',message => "warning: more than one MSFI in problem ".($i+1));
            }
        }else{
            push( @names, undef );
        }
    }

    return \@names;
}

sub msfo_names
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
         absolute_path => { isa => 'Bool', optional => 1, default => 0 }
        );
    my $absolute_path = $parm{'absolute_path'};
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @names = ();

    unless (scalar(@problem_numbers)>0){
        @problem_numbers = (1 .. scalar(@{$self->problems}));
    }
    foreach my $probnum (@problem_numbers){
        my $arr = $self->problems->[$probnum-1]->get_msfo_filenames;
        if (scalar(@{$arr})==0){
            push(@names,undef);
        }else{
            if (scalar(@{$arr})>1){
                ui->print(category => 'all',message => "warning: different msfo filenames ".$arr->[0]." and ".
                          $arr->[1]." in problem $probnum");
            }
            if ($absolute_path){
                push(@names,$self->directory.$arr->[0]);
            }else{
                push(@names,$arr->[0]);
            }
        }
    }
    return \@names;
}

sub table_names
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         new_names => { isa => 'ArrayRef', optional => 1 },
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
         ignore_missing_files => { isa => 'Bool', default => 0, optional => 1 }
    );
    my @new_names = defined $parm{'new_names'} ? @{$parm{'new_names'}} : ();
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @names = ();
    my $ignore_missing_files = $parm{'ignore_missing_files'};

    # Usage:
    #
    #    @tableNames = @{$modobj -> table_names};
    #
    # This basic usage takes no arguments and returns the value of
    # the FILE option in the $TABLE NONMEM record of each
    # problem. @tableNames will be a two dimensional array:
    #
    #   [[tableName_prob1][tableName_prob2][tableName_prob3]...]
    #
    #
    # If the I<new_names> argument of table_names is given, the
    # values of the FILE options will be changed.
    #
    # To set the FILE of specific problems, the I<problem_numbers>
    # argument can be used. It should be a reference to an array
    # containing the numbers of all problems where the FILE should
    # be changed or retrieved.  If specified, the size of
    # I<new_names> must be the same as the size of
    # I<problem_numbers>.
    #
    # The I<ignore_missing_files> boolean argument can be used to
    # set names of table that does not exist yet (e.g. before a
    # run has been performed).

    my ( $name_ref, $junk ) = $self ->
      _option_val_pos( name           => 'FILE',
               record_name       => 'table',
               problem_numbers => \@problem_numbers,
               new_values         => \@new_names );
    if ( $#new_names >= 0 ) {
      my @problems = @{$self->problems};
      unless( $#problem_numbers > 0 ){
            $self->problems([]) unless defined $self->problems;
        @problem_numbers = (1 .. $#{$self->problems}+1);
      }
      #Kajsa 2014-04-25 skip this
#      foreach my $i ( @problem_numbers ) {
#        $problems[$i-1] -> _read_table_files( ignore_missing_files => $ignore_missing_files || $self->ignore_missing_output_files);
#      }
    }
    @names = @{$name_ref};

    return \@names;
}

sub flip_comments
{
    my %parm = validated_hash(\@_,
         from_model => { isa => 'model', optional => 0 },
         new_file_name => { isa => 'Str', optional => 0 },
         write => { isa => 'Bool', default => 0 },
    );

    my $from_model = $parm{'from_model'};
    my $new_file_name = $parm{'new_file_name'};
    my $write = $parm{'write'};

    #TODO create model object for new file, optional write to disk, use model_lines parameter
    #handle different dir than input model
    #after flipping resolve data path in copy, could be different dir
    #return model object for copy

    if (-e $new_file_name and $write){
        unlink($new_file_name);
    }
    #need to read fresh from disk, make sure no PsN rearrangements
    open(MOD, $from_model->full_name) || die("Couldn't open " . $from_model->full_name." : $!");

    my @simlines=();

    my $sim_tag = 0;
    while(<MOD>) {
        my $tag_line = 0;

        # find Sim_end
        if (/^\s*\;+\s*[Ss]im\_end/) {
            $sim_tag = 0;
            $tag_line = 1;
        }
        # find Sim_start
        if (/^\s*\;+\s*[Ss]im\_start/) {
            $sim_tag = 1;
            $tag_line=1;
        }

        if(($sim_tag==1)and (not $tag_line)) {
            if(/^\s*\;+/) {
                s/\;//;
            } else {
                $_ = ';'.$_
            }
        }
        push(@simlines,$_);

    }
    close(MOD);

    my $newdir;
    my $filename;
    ($newdir, $filename) = OSspecific::absolute_path(undef,$new_file_name);
    #first create new model object in dir of old model, to get relative paths to data correct
    my $newmodel = model->new(directory => $from_model->directory,
                              filename => $filename,
                              model_lines => \@simlines,
                              ignore_missing_data => 1,
                              ignore_missing_output_files => 1);
    #then set new directory, which will make sure data paths are correct for new dir
    $newmodel->directory($newdir);

    if ($write){
        $newmodel-> _write;
    }

    return $newmodel;
}

sub units
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         parameter_type => { isa => 'Str', optional => 1 },
         parameter_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
         new_values => { isa => 'ArrayRef[Str]', optional => 1 },
         with_priors => { isa => 'Bool', default => 0, optional => 1 }
    );
    my $parameter_type = $parm{'parameter_type'};
    my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
    my $with_priors = $parm{'with_priors'};
    my @units;

      # Sets or gets the units of a (number of) parameter(s). The
      # unit is not a proper NONMEM syntax but is recognized by
      # the PsN model class. A unit (and a label) can be specified
      # as a comments after a parameter definition. e.g.:
          #
          #    $THETA (0,13.2,100) ; MTT; h
          #
          # which will give this theta the label I<MTT> and unit I<h>.
    @units = @{ $self -> _init_attr( parameter_type    => $parameter_type,
                     parameter_numbers => \@parameter_numbers,
                     problem_numbers           => \@problem_numbers,
                     new_values        => \@new_values,
                     with_priors       => $with_priors,
                     attribute              => 'unit')};

    return \@units;
}

sub update_inits
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        from_output => { isa => 'output', optional => 1 },
        from_output_file => { isa => 'Str', optional => 1 },
        from_model => { isa => 'model', optional => 1 },
        from_hash => { isa => 'Ref', optional => 1 },
        problem_number => { isa => 'Maybe[Int]', optional => 1 },
        ignore_missing_parameters => { isa => 'Bool', default => 0, optional => 1 },
        update_fix => { isa => 'Bool', default => 0, optional => 1 },
        skip_output_zeros => { isa => 'Bool', default => 0, optional => 1 },
        ensure_posdef => { isa => 'Bool', default => 0, optional => 1 },
        update_omegas => { isa => 'Bool', default => 1, optional => 1 },
        update_sigmas => { isa => 'Bool', default => 1, optional => 1 },
        update_thetas => { isa => 'Bool', default => 1, optional => 1 },
        match_labels => { isa => 'Bool', default => 1, optional => 1 },
        start_record => { isa => 'Int', optional => 1 },
        end_record => { isa => 'Int', optional => 1 },
    );
    my $from_output = $parm{'from_output'};
    my $from_output_file = $parm{'from_output_file'};
    my $from_model = $parm{'from_model'};
    my $from_hash = $parm{'from_hash'};
    my $problem_number = $parm{'problem_number'};
    my $ignore_missing_parameters = $parm{'ignore_missing_parameters'};
    my $ensure_posdef = $parm{'ensure_posdef'};
    my $update_omegas = $parm{'update_omegas'};
    my $update_sigmas = $parm{'update_sigmas'};
    my $update_thetas = $parm{'update_thetas'};
    my $update_fix = $parm{'update_fix'};
    my $skip_output_zeros = $parm{'skip_output_zeros'};
    my $start_record = $parm{'start_record'};
    my $end_record = $parm{'end_record'};
    my $match_labels = $parm{'match_labels'};

    # Usage:
    #
    #   $modobj -> update_inits ( from_output => $outobj );
    #
    # alt
    #
    #   $modobj -> update_inits ( from_output_file => $outfile );
    #
    # This basic usage takes the parameter estimates from the
    # output object I<$outobj> or from the output file I<$outfile>
    # and updates the initial estimates in the model object
    # I<$modobj>. The number of problems and parameters must be
    # the same in the model and output objects unless parameter
    # problem_number has been defined. If there exists
    # more than one subproblem per problem in the output object,
    # only the estimates from the first subproblem will be
    # transferred.
    #
    #   $modobj -> update_inits ( from_output               => $outobj,
    #                             ignore_missing_parameters => 1 );
    #
    # If the ignore_missing_parameters argument is set to 1, the number of
    # parameters in the model and output objects do not need to match. The
    # parameters that exist in both objects are used for the update of the
    # model object.
    #
    #   $modobj -> update_inits ( from_model                => $from_modobj );
    #
    # If the from_model argument is given, update_inits tries to match the
    # parameter names (labels) given in $from_modobj and $modobj and
    # and thereafter updating the $modobj object. See L</units> and L</labels>.
    #

    croak("update_inits: No output object defined and no output object found through the model object specified.")
    unless ( ( defined $from_model and ( defined $from_model -> outputs and defined $from_model -> outputs->[0] ) ) or
        defined $from_output or defined $from_output_file or defined $from_hash);
    if ((defined $from_model) and ((defined $from_output) or (defined $from_output_file))) {
        croak("update_inits: Illegal usage, cannot specify both from_model and from_output(_file).");
    }
    if ((defined $from_hash) and ((defined $from_output) or (defined $from_output_file))) {
        croak("update_inits: Illegal usage, cannot specify both from_hash and from_output(_file).");
    }
    if ((defined $from_hash) and (defined $from_model )) {
        croak("update_inits: Illegal usage, cannot specify both from_hash and from_model.");
    }
    my %allparams;
    if ( defined $from_output ) {
        $from_output->load;
    } elsif ( defined $from_hash ) {
        $from_output = undef;
        %allparams = %{$from_hash};
    } elsif ( defined $from_output_file ) {
        $from_output = output->new(filename => $from_output_file,
                                   parse_output => 1);
    } else {
        $from_output = @{$from_model->outputs}[0]; #assume 1st $PROB
        $from_output->load;
    }

    my @params = ();
    if( $update_thetas ) {
        push( @params, 'theta' );
    }
    if( $update_omegas ) {
        push( @params, 'omega' );
    }
    if( $update_sigmas ) {
        push( @params, 'sigma' );
    }

    foreach my $param ( @params ) {
        my ( @intermediate_coordslabels, @from_coordval );
        my $access = $param.'coordval';
        my $from_string;
        my @problems = @{$self->problems};
        my %param_hash;

        if (defined $from_hash){
            unless (($#problems == 0) or (defined $problem_number)){
                croak("Updating initial estimates from hash ".
                    "can only be done with single PROB models unless parameter ".
                    "problem number has been defined");
            }
            #go directly to defining %namesvalues below
            $from_string = 'input hash';
        }else {
            # Since initial estimates are specified on the problem level and not on
            # the subproblem level we use the estimates from the outputs first subproblem
            @from_coordval = @{$from_output -> $access ( subproblems => [1] )};
            if ( defined $from_model and $match_labels) {
                @intermediate_coordslabels = @{$from_model -> get_coordslabels( parameter_type => $param )};
                $from_string = 'from-model '.$from_model -> full_name();
                croak("The number of problems are not the same in ".
                    "$from_string (".($#intermediate_coordslabels+1).")".
                    " and the model to be updated ".$self -> full_name." (".
                    ($#problems+1).")" )
                unless ($#problems == $#intermediate_coordslabels);
            } else {
                $from_string = 'from-output '.$from_output->full_name();
                if (defined $problem_number){
                    croak("The problem number to update ($problem_number) ".
                        "does not exist in $from_string (only ".($#from_coordval+1)." problems)")
                    unless ($problem_number<=($#from_coordval +1));
                    croak("The problem number to update ($problem_number) ".
                        "does not exist in the model to be updated ".
                        $self -> full_name." ( only ".($#problems+1)." problems)")
                    unless ($problem_number <=($#problems +1));
                }else{
                    croak("The number of problems are not the same in $from_string ".
                        " (".($#from_coordval+1).")".
                        " and the model to be updated ".
                        $self -> full_name." (".
                        ($#problems+1).")" ) unless ( $#problems == $#from_coordval );
                }
            }
        }

        # Loop over the problems:
        for ( my $i = 0; $i <= $#problems; $i++ ) {
            next if (defined $problem_number and (($problem_number-1) != $i));
            my $problem = $problems[$i];
            unless ( defined  $problem) {
                croak("Problem number ".($i+1)." does not exist" );
            }
            my $accessor = $param.'s';
            unless( $problem-> can($accessor) ){
                croak("Error unknown parameter type: $param" );
            }
            my @records;
            if (defined $problem -> $accessor()) {
                @records = @{$problem -> $accessor()};
                if (defined $start_record){
                    croak ("start_record $start_record cannot be smaller than 1 ") if ($start_record < 1);
                    unless (defined $end_record){
                        $end_record = scalar(@records);
                    }else{
                        croak ("end_record $end_record cannot be larger than number of records") if (scalar(@records) < $end_record);
                    }
                    croak ("end_record $end_record cannot be smaller than start_record $start_record") if ($end_record < $start_record);
                    @records = @records[($start_record-1) .. ($end_record-1)];
                }
            }
            next unless (scalar(@records) > 0); #no parameter in this problem

            my @diagnostics;
            my %namesvalues;

            #if we have intermediate then first match coordinates between intermediate and from,
            #and replace 'from' coordinates with the intermediate labels, if available

            if (defined $from_hash){
                %namesvalues = %{$allparams{$param}};
            }else{
                if (defined $from_coordval[$i]){
                    unless (defined $from_coordval[$i]->[0]){
                        croak("No $param values read from output for problem ".($i+1));
                    }
                    if ( defined $from_model and $match_labels and defined $intermediate_coordslabels[$i]) {
                        my %fromval = %{$from_coordval[$i]->[0]};
                        my %intermediate = %{$intermediate_coordslabels[$i]};
                        foreach my $coord (keys %fromval){
                            #if there is no label for the coord, get_coordslabels stores the coordinate string
                            #as the hash value. Always defined.
                            my $name = $intermediate{$coord}; #this is either coordinate string or label
                            if (defined $name){
                                $namesvalues{$name} = $fromval{$coord};
                            }#else{ #FIXME
                            # here we get omegas that are not estimated
                            #    print STDERR "coord is $coord fromval is ".$fromval{$coord}."\n";
                            #}
                        }
                    }else {
                        %namesvalues = %{$from_coordval[$i]->[0]};
                    }
                }
            }
            #loop through records and options
            #if record is same then skip
            #if fix but not ignore_missing we still match values to see if any missing
            #name of own param is label if from_model defined and label defined, otherwise coord
            #look up value in namesvalues hash, replace value with "matched"

            my $any_same=0;
            foreach my $record (@records){
                my $blockfix = 0;
                next if ($record->prior());
                my $store_rec = 1;
                if  ($record->same() ){
                    $any_same=1; # we can match nothing, cannot do error check
                    next;
                }
                if  ($record->fix() and (not $update_fix)){
                    $store_rec = 0;
                    next if ($ignore_missing_parameters or $any_same);
                }
                unless (defined $record -> options()){
                    croak("$param record has no values");
                }
                if ($record->is_block and $record->fix){
                    $blockfix = 1;
                }
                foreach my $option (@{$record -> options()}) {
                    next if ($option->prior());
                    my $store_val = $store_rec;
                    if ($option->fix() and (not $update_fix)){
                        $store_val = 0;
                        next if ($ignore_missing_parameters or $any_same);
                    }
                    my $name = $option -> coordinate_string();
                    if ($match_labels and (
                            ((defined $from_model) or (defined $from_hash)) and
                            (defined $option -> label()))){
                        $name = $option -> label();#do matching on label instead of coordinate
                    }
                    if (defined $namesvalues{$name}){
                        my $value = $namesvalues{$name};
                        croak("Multiple instances of label $name in problem to update, ".
                              "ambiguous parameter matching by label.")
                            if ($value eq 'matched');
                        $store_val = 0 if ($value == 0 and $skip_output_zeros);
                        if ($store_val){
                            #will change value even if fix
                            push( @diagnostics,
                                  $option -> check_and_set_init( new_value => $value ) );
                        }
                        $namesvalues{$name} = 'matched';
                    } else {
                        unless ($ignore_missing_parameters){
                            unless (($option->fix)
                                    or
                                     ($blockfix) or
                                    ($option->init() == 0 and (defined $option->on_diagonal) and (not $option->on_diagonal()))
                                ){
                                my $mes = "update_inits: No match for $param $name found in $from_string";
                                ui -> print( category => 'all',
                                             message  => $mes."\n");
                            }
                        }
                    }
                }
            }

            #check if any values not matched
            #we store same but they will not be matched even if all correct,
            #so this only catches errors if no same
            unless ($ignore_missing_parameters or $any_same){
                foreach my $name (keys %namesvalues){
                    croak("No match for $param ".
                        "$name found in problem to update")
                    if (($namesvalues{$name} ne 'matched') and ($namesvalues{$name} != 0));
                }
            }
            if (($param eq $params[$#params]) and ($update_omegas or $update_sigmas) and $ensure_posdef){
                $problem->ensure_posdef(verbose => 1); #ensure_posdef only set from update_inits program
            }
        } #each problem
    } #each param
}

sub upper_bounds
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         parameter_type => { isa => 'Str', optional => 1 },
         parameter_numbers => { isa => 'ArrayRef', optional => 1 },
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
         with_priors => { isa => 'Bool', default => 0, optional => 1 },
         new_values => { isa => 'ArrayRef', optional => 1 }
    );
    my $parameter_type = $parm{'parameter_type'};
    my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my $with_priors = $parm{'with_priors'};
    my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
    my @upper_bounds;

    # upper_bounds either sets or gets the initial values of the
    # parameter specified in I<parameter_type> for each
    # subproblem specified in I<problem_numbers>. For each
    # element in I<problem_numbers> there must be an array in
    # I<parameter_numbers> that specify the indices of the
    # parameters in the subproblem for which the upper bounds
    # are set, replaced or retrieved.

    if ($parameter_type eq 'theta'){
        @upper_bounds = @{ $self -> _init_attr
                               ( parameter_type    => $parameter_type,
                                 parameter_numbers => \@parameter_numbers,
                                 problem_numbers           => \@problem_numbers,
                                 new_values        => \@new_values,
                                 with_priors       => $with_priors,
                                 attribute         => 'upbnd')};

    }else{
        #omega or sigma
        if (scalar (@new_values)> 0){
            croak("Trying to set upper bounds for $parameter_type, not allowed");
        }
        #pick up on diagonal first to get correct output structure and then change elements to undef
        @upper_bounds = @{ $self -> _init_attr
                               ( parameter_type    => $parameter_type,
                                 parameter_numbers => \@parameter_numbers,
                                 problem_numbers           => \@problem_numbers,
                                 new_values        => \@new_values,
                                 with_priors       => $with_priors,
                                 attribute         => 'on_diagonal')};
        for (my $prob=0; $prob < scalar(@upper_bounds); $prob++){
            for (my $i=0; $i < scalar(@{$upper_bounds[$prob]}); $i++){
                $upper_bounds[$prob]->[$i] = undef;
            }
        }
    }

    return \@upper_bounds;
}

sub _write
{
    #Writes the content of the modelobject to disk. Either to the
    #filename given, or to the string returned by model::full_name.

    my $self = shift;
    my %parm = validated_hash(\@_,
        filename => { isa => 'Str', default => $self->full_name, optional => 1 },
        number_format => { isa => 'Maybe[Int]', optional => 1 },
        relative_data_path => { isa => 'Bool', default => $self->relative_data_path, optional => 1 },
        copy_msfi => { isa => 'Bool', default => 0, optional => 1 },
        overwrite => { isa => 'Bool', default => 0, optional => 1 },
        MX_PARAMS_VALIDATE_NO_CACHE => 1,
    );
    my $filename = $parm{'filename'};
    my $number_format = $parm{'number_format'};
    my $copy_msfi = $parm{'copy_msfi'};
    my $relative_data_path = $parm{'relative_data_path'};
    my $overwrite = $parm{'overwrite'};

    my ($writedir,$file) = OSspecific::absolute_path('',$filename);
    my $updatedmsfi = $self->update_internal_msfi;

    if ($copy_msfi){
        for ( my $i = 0; $i < scalar @{$self->problems}; $i++ ) {
            if (defined $self->problems->[$i]->msfis){
                for ( my $j = 0; $j < scalar @{$self->problems->[$i]->msfis}; $j++ ) {
                    $self->problems->[$i]->msfis->[$j]->copy_msfi_file(write_directory => $writedir,
                                                                       ignore_missing_file => $self->ignore_missing_data,
                                                                       overwrite => 1);
                }
            }
        }
    }

    my @formatted;

    # Insert annotation first
    if (defined $self->annotation) {
        push @formatted, @{$self->annotation->format()};
    }

    # An element in the active_problems array is a boolean that
    # corresponds to the element with the same index in the problems
    # array.  If the boolean is true, the problem will be run. All
    # other will be commented out.
    my @active = @{$self->active_problems};

    # loop over all problems.
    for ( my $i = 0; $i < scalar @{$self->problems}; $i++ ) {
        # Call on the problem object to format it as text. The
        # filename and problem numbers are needed to make some
        # autogenerated files (msfi, tabels etc...) unique to the
        # model and problem
        my @preformatted = @{$self->problems -> [$i] -> _format_problem(
                                 filename => $self -> filename,
                                 problem_number => ($i + 1),
                                 relative_data_path => $relative_data_path,
                                 write_directory => $writedir,
                                 number_format => $number_format)
            };
        # Check if the problem is NOT active, if so comment it out.
        unless ( $active[$i] ) {
            for ( my $j = 0; $j <= $#preformatted; $j++ ) {
                $preformatted[$j] = '; '.$preformatted[$j];
            }
        }
        # Add extra line to avoid problems with execution of NONMEM
        push(@preformatted, "\n");
        push(@formatted, @preformatted);
    }

    # Open a file and print the formatted problems.
    # TODO Add some errorchecking.
    no warnings qw(closed);
    open( FILE, '>'. $filename );
    for ( @formatted ) {
        chomp;
        print FILE;
        print FILE "\n";
    }
    close( FILE );


    if ($self->iofv_modules) {
        $self->iofv_modules->[0]->post_process;
    }
}

sub is_option_set
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str', optional => 0 },
        record => { isa => 'Str', optional => 0 },
        problem_number => { isa => 'Int', default => 1, optional => 1 },
        record_number => { isa => 'Int', default => 0, optional => 1 },
        fuzzy_match => { isa => 'Bool', default => 0, optional => 1 }
    );
    my $name = $parm{'name'};
    my $record = $parm{'record'};
    my $problem_number = $parm{'problem_number'};
    my $record_number = $parm{'record_number'};
    my $found = 0;
    my $fuzzy_match = $parm{'fuzzy_match'};

    # Usage:
    #
    # if( $modelObject -> is_option_set( record => 'recordName', name => 'optionName' ) ){
    #     print "problem_number 1 has option optionName set in record recordName";
    # }
    #
    # is_option_set checks if an option is set in a given record in given problem.
    #if record_number is 0 it means 'all', this is the default. -1 means last

    my @problems;
    if ( defined $self->problems ) {
        @problems = @{$self->problems};
    } else {
        croak("No problems defined in model" );
    }
    unless( defined $problems[$problem_number - 1] ){
        warn "model -> is_option_set: No problem number $problem_number defined in model";
        return 0; # No option can be set if no problem exists.
    }

    $found = $problems[$problem_number - 1]->is_option_set(name => $name,
                                                           record => $record,
                                                           record_number => $record_number,
                                                           fuzzy_match => $fuzzy_match);

    return $found;
}

sub is_run
{
    my $self = shift;
    my $return_value = 0;

    # Usage:
    #
    # is_run returns true if the outputobject owned by the
    # modelobject has valid outpudata either in memory or on disc.
    if (defined $self->outputs) {
      if (@{$self->outputs}[0]->have_output) {
        $return_value = 1;
      }
    }

    return $return_value;
}

sub indexes
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         parameter_type => { isa => 'Str', optional => 1 },
         parameter_numbers => { isa => 'ArrayRef', optional => 1 },
         with_priors => { isa => 'Bool', default => 0, optional => 1 },
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
    );
    my $parameter_type = $parm{'parameter_type'};
    my @parameter_numbers = defined $parm{'parameter_numbes'} ? @{$parm{'parameter_numbers'}} : ();
    my $with_priors = $parm{'with_priors'};
    my @problem_numbers =defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @indexes = ();

    # Usage:
    #
    #   @indexArray = @{$modelObject -> indexes( 'parameter_type' => 'omega' )};
    #
    # A call to I<indexes> returns the indexes of all parameters
    # specified in I<parameter_numbers> from the subproblems
    # specified in I<problem_numbers>. The method returns a reference to an array that has
    # the same structure as parameter_numbers but for each
    # array of numbers is instead an array of indices. The method
    # uses a method from the model::problem class to format the
    # indices, so here are a few lines from the code comments in
    # model/problem.pm that describes the returned value:
    #
    # New: returns name and index with new format
    # THETA1, THETA2, THETA3...
    # OMEGA(1,1) OMEGA (1,2)...
    # SIGMA (1,1) SIGMA (2,2)...
    # <snip> indexes_old
    # The Indexes method calculates the index for a
    # parameter. Off-diagonal elements will get a index 'i_j', where i
    # is the row number and j is the column number
    # </snip>

    unless( scalar(@problem_numbers) > 0 ){
        $self->problems([]) unless defined $self->problems;
        @problem_numbers = (1 .. $#{$self->problems}+1);
    }
    my @problems = @{$self->problems};
    foreach my $i ( @problem_numbers ) {
        if ( defined $problems[ $i-1 ] ) {
            push( @indexes,
                $problems[ $i-1 ] ->
                indexes( parameter_type => $parameter_type,
                    parameter_numbers => $parameter_numbers[ $i-1 ],
                    with_priors => $with_priors) );
        } else {
            croak("Problem number $i does not exist!" );
        }
    }

    return \@indexes;
}

sub _option_val_pos
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str', optional => 1 },
        record_name => { isa => 'Str', optional => 1 },
        problem_numbers => { isa => 'ArrayRef', optional => 1 },
        instance_numbers => { isa => 'ArrayRef', optional => 1 },
        exact_match => { isa => 'Bool', default => 1, optional => 1 },
        new_values => { isa => 'ArrayRef', optional => 1 }
    );
    my $name = $parm{'name'};
    my $record_name = $parm{'record_name'};
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @instance_numbers = defined $parm{'instance_numbers'} ? @{$parm{'instance_numbers'}} : ();
    my $exact_match = $parm{'exact_match'};
    my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
    my @values;
    my @positions;

    unless( scalar(@problem_numbers)>0 ){
        $self->problems([]) unless defined $self->problems;
        @problem_numbers = (1 .. $#{$self->problems}+1);
    }
    my @problems = @{$self->problems};
    if ( $#new_values >= 0 ) {
        croak("Trying to set option $name in record $record_name but the ".
            "number of new value sets (".
            ($#new_values+1).
            "), do not match the number of problems specified (".
            ($#problem_numbers+1).")" )
        unless(($#new_values == $#problem_numbers) );
        if ( $#instance_numbers > 0 ) {
            croak("The number of instance number sets (".
                ($#instance_numbers+1).
                "),do not match the number of problems specified (".
                ($#problem_numbers+1).")" )
            unless(($#instance_numbers == $#problem_numbers) );
        }
    }

    foreach my $i ( @problem_numbers ) {
        if ( defined $problems[ $i-1 ] ) {
            my $rn_ref = $#instance_numbers >= 0 ? \@{$instance_numbers[ $i-1 ]} : [];
            if ( scalar @new_values > 0) {
                # {{{ Update values

                if( not defined $new_values[ $i-1 ] ) {
                    croak(" The specified new_values was undefined for problem $i" );
                }

                if( not ref( $new_values[ $i-1 ] ) eq 'ARRAY' ) {
                    croak(" The specified new_values for problem $i is not an array as it should be but a ".
                        ( defined ref( $new_values[ $i-1 ] ) ?
                            ref( $new_values[ $i-1 ] ) : 'undef' ) );
                }

                $problems[ $i-1 ] ->
                _option_val_pos( record_name      => $record_name,
                    instance_numbers => $rn_ref,
                    new_values       => \@{$new_values[ $i-1 ]},
                    name             => $name,
                    exact_match      => $exact_match );

                # }}} Update values
            } else {
                # {{{ Retrieve values
                my ( $val_ref, $pos_ref ) =
                $problems[ $i-1 ] ->
                _option_val_pos( record_name      => $record_name,
                    instance_numbers => $rn_ref,
                    name             => $name,
                    exact_match      => $exact_match );
                push( @values, $val_ref );
                push( @positions, $pos_ref );
                # }}} Retrieve values
            }
        } else {
            croak("Problem number $i does not exist!" );
        }
    }

    return \@values ,\@positions;
}

sub input_files
{
    # List all input files needed by this model
    my $self = shift;
    my @file_names;

    # Copy file for $ETAS
    ( my $model_fsubs, undef ) = $self->_option_val_pos(record_name => 'etas', name => 'FILE');
    for my $problem_files (@$model_fsubs) {
        for my $etas_file (@$problem_files) {
            my ( $dir, $filename ) = OSspecific::absolute_path($self->directory, $etas_file);
            push(@file_names, [ $dir, $filename ]);
            $self->remove_option(
                record_name => 'etas',
                option_name => 'FILE',
            );
            $self->add_option(
                record_name => 'etas',
                option_name => 'FILE',
                option_value => ( $filename ),
                add_record => 0,
            );
        }
    }

    # Copy extra fortran files specified in "$SUBROUTINE"

    if( defined( $self -> subroutine_files ) ){
        foreach my $sub_file ( @{$self -> subroutine_files} ){
            my ( $dir, $filename ) = OSspecific::absolute_path( $self -> directory,
                $sub_file );
            push( @file_names, [$dir, $filename] );
        }
    }

    # Copy extra files the user specified.

    if( defined $self -> extra_files ){
        foreach my $x_file (@{$self -> extra_files}){
            my ( $dir, $filename ) = OSspecific::absolute_path( $self -> directory,
                $x_file );
            #add check that file exists
            croak("File $dir$filename listed as ".
                "extra input to NONMEM, but the file does not exist.")
            unless (-e $dir.$filename);
            push( @file_names, [$dir, $filename] );
        }
    }

    return \@file_names;
}

sub output_files
{
    my $self = shift;
    my @file_names;

    push( @file_names, $self -> outputs -> [0] -> filename );


    if (defined $self -> outputs -> [0] -> filename_root()){
        foreach my $ext (@PsN::nm7_extensions){
            push( @file_names, $self -> outputs -> [0] -> filename_root().$ext);
        }
    }

    if( defined $self -> table_names ){
        foreach my $table_files( @{$self -> table_names} ){
            foreach my $table_file( @{$table_files} ){
                my ($dir, $filename) = OSspecific::absolute_path( undef, $table_file );
                push( @file_names, $filename );
            }
        }
    }

    if( defined $self -> msfo_names() ){
        require model::problem::msfi;
        foreach my $msfo_file ( @{$self -> msfo_names()} ){
            next unless (defined $msfo_file);
            my ( $dir, $filename ) = OSspecific::absolute_path( undef, $msfo_file );
            push( @file_names, $filename );
            push( @file_names,@{model::problem::msfi::get_additional_msfo_files(msfname => $filename)});
        }
    }

    if ( defined $self->extra_output ) {
        foreach my $extra_out ( @{$self->extra_output} ){
            push( @file_names, $extra_out );
        }
    }

    my @problems = @{$self -> problems};
    for( my $i = 0; $i < scalar(@problems); $i++ ) {
        if (defined $problems[$i]->shrinkage_module and $problems[$i]->shrinkage_module->enabled) {
            my $dir;
            my $eta_filename;
            ( $dir, $eta_filename ) = OSspecific::absolute_path( undef,
                $problems[$i] -> shrinkage_module -> eta_tablename );

            push( @file_names, $eta_filename );

            my $wres_filename;
            ( $dir, $wres_filename ) = OSspecific::absolute_path( undef,
                $problems[$i] -> shrinkage_module -> wres_tablename );

            push( @file_names, $wres_filename );
        }
    }

    return \@file_names;
}

sub remove_inits
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         type => { isa => 'Str', optional => 1 },
         labels => { isa => 'ArrayRef[Str]', optional => 1 },
         indexes => { isa => 'ArrayRef[Int]', optional => 1 },
         problem_number => { isa => 'Int', default => 1, optional => 1 }
    );
    my $type = $parm{'type'};
    my @labels = defined $parm{'labels'} ? @{$parm{'labels'}} : ();
    my @indexes = defined $parm{'indexes'} ? @{$parm{'indexes'}} : ();
    my $problem_number = $parm{'problem_number'};

      # Usage
      #
      # $model -> remove_inits( type => 'theta',
      #                         indexes => [1,2,5,6] )
      #

      # In all cases the type must be set to theta. Removing Omegas and
      # Sigmas is not allowed, (If need that feature, send us a
      # mail). In the above example the thetas 1, 2, 5 and 6 will be
      # removed from the modelfile. Notice that this alters the theta
      # numbering, so if you later decide that theta number 7 must be
      # removed as well, you must calculate its new position in the
      # file. In this case the new number would be 3. Also notice that
      # numbering starts with 1.
      #
      # $model -> remove_inits( type => 'theta',
      #                         labels => ['V', 'CL'] )
      #

      # If you have specified labels in you modelfiles(a label is
      # string inside a comment on the same row as the theta) you can
      # specify an array with labels, and the corresponding theta, if
      # it exists, will be removed. This is a much better approach
      # since you don't need to know where in order the theta you wish
      # to remove appears. If you specify both labels and indexes, the
      # indexes will be ignored.

      croak('does not have the functionality for removing $OMEGA or $SIGMA options yet' )
      if ( $type eq 'omega' or $type eq 'sigma' );
      my $accessor = $type.'s';

      # First pick out a referens to the theta records array.
      my $records_ref = $self -> problems -> [$problem_number -1] -> $accessor;

      # If we have any thetas at all:
      if ( defined $records_ref ) {
    my @records_array = @{$records_ref};

    # If labels are specified, we translate the labels into
    # indexes.
    if ( scalar @labels > 0 ) {
      @indexes = ();
      my $i = 1;
      # Loop over theta records
      foreach my $init ( @records_array ) {
        # Loop over the individual thetas inside
        foreach my $option ( @{$init -> options} ) {
          # Loop over all given labels.
          foreach my $label ( @labels ) {
        # Push the index number if a given label match the
        # theta label
        push( @indexes, $i ) if ( $option -> label eq $label);
          }
          # $i is the count of thetas so far
          $i++;
        }
      }
    }

    # We don't really remove thetas, we do a loop over all thetas
    # and recording which we like to keep. We do that by selecting
    # an index, from @indexes, that shall be removed and loop over
    # the thetas, all thetas that doesn't match the index are
    # stored in @keep_options. When we find a theta that matches,
    # we pick a new index and continue the loop. So by makeing
    # sure that @indexes is sorted, we only need to loop over the
    # thetas once.

    @indexes = sort {$a <=> $b} @indexes; #sort input

    my $index = 0;
    my $nr_options = 1;
    my @keep_records;

    # Loop over all records
        RECORD_LOOP: foreach my $record ( @records_array ){
      my @keep_options = ();
      # Loop over all thetas
      foreach my $option ( @{$record -> options} ) {
        if( $indexes[ $index ] == $nr_options ){
          # If a theta matches an index, we take the next index
          # and forget the theta.
          unless( $index > $#indexes ){
        $index++;
          }
        } else {
          # Otherwise we rember it.
          push(@keep_options,$option);
        }
        $nr_options++;
      }
      if( scalar(@keep_options) > 0 ){
        # If we remember some thetas, we must also remember the
        # record which they are in.
        $record -> options( \@keep_options );
        push( @keep_records, $record );
      }
    }

    # Set the all kept thetas back into the modelobject.
    @{$records_ref} = @keep_records;

      } else {
    croak("No init of type $type defined" );
      }
}

sub remove_records
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         type => { isa => 'Str', optional => 0 },
         keep_last => { isa => 'Bool', default => 0, optional => 1 },
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
    );
    my $type = $parm{'type'};
    my $keep_last = $parm{'keep_last'};
    my @problem_numbers;

    if (not defined $self->problems) {
        croak("The model does not contain any problems");
    }

    if (defined $parm{'problem_numbers'}) {
        @problem_numbers = @{$parm{'problem_numbers'}};
    } else {
        @problem_numbers = (1 .. scalar(@{$self->problems}));
    }

    my @problems = @{$self->problems};

    foreach my $i (@problem_numbers) {
      if (defined $problems[$i-1]) {
        $problems[$i-1]->remove_records(type => $type, keep_last => $keep_last);
      } else {
        croak("Problem number $i, does not exist");
      }
    }
}

sub table_files
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
    );
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @table_files;

    # Usage:
    #
    #    @table_files = @{$modobj -> table_files};
    #
    # This basic usage takes no arguments and returns the table
    # files objects for all problems.  @table_files will be a
    # two dimensional array:
    #
    #   [[table_file_object_prob1][table_file_object_prob2]...]
    #
    #
    # To retrieve the table file objects from specific problems,
    # the I<problem_numbers> argument can be used. It should be
    # a reference to an array containing the numbers of all
    # problems from which the table file objects should be
    # retrieved.

    unless(scalar(@problem_numbers)>0 ){
        $self->problems([]) unless defined $self->problems;
      @problem_numbers = (1 .. $#{$self->problems}+1);
    }
    my @problems = @{$self->problems};
        foreach my $i ( @problem_numbers ) {
      if ( defined $problems[ $i-1 ] ) {
          $problems[$i-1] -> _read_table_files(ignore_missing_files => 1);
          push( @table_files, $problems[$i-1] -> table_files );
      } else {
        croak("Problem number $i does not exist!" );
      }
    }

    return \@table_files;
}

sub full_name
{
    my $self = shift;

    return $self->directory . $self->filename;
}

sub is_estimation
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        problem_number => { isa => 'Int', default => 1, optional => 1 }
    );
    my $problem_number = $parm{'problem_number'};
    my $is_est = 0;

    #this function is used to check whether we should care about minimization status
    #for possible retries. We only care of we are doing a real estimation
    #if multiple $EST in single PROB for NM7 we assume estimation

    $is_est = 1;
    my $verbose=0;
    my $problems = $self->problems;
    if( defined $problems -> [$problem_number - 1] ) {
        my $problem = $problems -> [$problem_number - 1];
        # If we don't have an ESTIMATION record we are simulating.
        unless( defined $problem->estimations and
            scalar( @{$problem->estimations} ) > 0 ){
            print "no estimation records\n" if $verbose;
            $is_est = 0;
        }
        # If we have a ONLYSIM option in the simulation record.
        if( $self -> is_option_set ( name           => 'ONLYSIM',
                record         => 'simulation',
                problem_number => $problem_number )){
            print "onlysim in simulation\n" if $verbose;
            $is_est = 0 ;
        }

        if (not ($PsN::nm_major_version == 5 or $PsN::nm_major_version == 6)){
            # If max evaluations is zero of last $EST we are not estimating
            # since output discards all but last step this is all we can check
            if ( defined $problem->estimations){
                my $record_index = scalar( @{$problem->estimations} )-1;
                if ($record_index >= 0) {
                    my $max = $self -> get_option_value(record_name => 'estimation',
                                                        option_name => 'MAXEVALS',
                                                        problem_index => ($problem_number - 1),
                                                        record_index => $record_index,
                                                        option_index => 0);

                    if (defined $max and $max == 0){
                        $is_est = 0 ;
                        print "last est step maxeval 0\n" if $verbose;
                    }
                    my $eonly = $self -> get_option_value(record_name => 'estimation',
                                                          option_name => 'EONLY',
                                                          problem_index => ($problem_number - 1),
                                                          record_index => $record_index,
                                                          option_index => 0);
                    if (defined $eonly and $eonly == 1){
                        $is_est = 0 ;
                        print "last est step eonly\n" if $verbose;
                    }
                }
            }

        } else {
            if( defined $self -> maxeval(problem_numbers => [$problem_number]) and
                defined $self -> maxeval(problem_numbers => [$problem_number])->[0][0] and
                $self -> maxeval(problem_numbers => [$problem_number])->[0][0] == 0 ){
                print "not nm7 and maxeval 00 is zero\n" if $verbose;
                $is_est = 0 ;
            }
        }
        # Anything else?

        # If none of the above is true, we are estimating.
    }

    return $is_est;
}

sub subroutine_files
{
    my $self = shift;
    my @fsubs;

    my %fsubs;
    foreach my $subr( 'PRED','CRIT', 'CONTR', 'CCONTR', 'MIX', 'CONPAR', 'OTHER', 'PRIOR', 'INFN' ){
        my ( $model_fsubs, $junk ) = $self -> _option_val_pos( record_name => 'subroutine',
            name => $subr );
        if( @{$model_fsubs} > 0 ){
            foreach my $prob_fsubs ( @{$model_fsubs} ){
                foreach my $fsub( @{$prob_fsubs} ){
                    $fsubs{$fsub} = 1;
                }
            }
        }
    }

    # BUG , nonmem6 might not require the file to be named .f And I've
    # seen examples of files named .txt

    @fsubs = keys %fsubs;
    if( @fsubs > 0  ){
        for( my $i = 0; $i <= $#fsubs; $i ++ ){
            unless( $fsubs[$i] =~ /\.f$/ ){
                $fsubs[$i] .= '.f';
            }
        }
    }

    return \@fsubs;
}

sub randomize_inits
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         degree => { isa => 'Num', optional => 1 }
    );
    my $degree = $parm{'degree'};

    foreach my $prob ( @{$self->problems} ) {
      $prob -> set_random_inits ( degree => $degree );
    }
}

sub remove_option
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
         record_name => { isa => 'Str', optional => 0 },
         record_number => { isa => 'Int', default => 0, optional => 1 },
         option_name => { isa => 'Str', optional => 0 },
         fuzzy_match => { isa => 'Bool', default => 0, optional => 1 }
    );
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my $record_name = $parm{'record_name'};
    my $record_number = $parm{'record_number'};
    my $option_name = $parm{'option_name'};
    my $fuzzy_match = $parm{'fuzzy_match'};

    unless( scalar(@problem_numbers)>0 ){
        $self->problems([]) unless defined $self->problems;
        @problem_numbers = (1 .. $#{$self->problems}+1);
    }

    my @problems = @{$self->problems};
    foreach my $i ( @problem_numbers ) {
        if ( defined $problems[ $i-1 ] ) {
            $problems[$i-1] -> remove_option( record_name => $record_name,
                record_number => $record_number,
                option_name => $option_name,
                fuzzy_match => $fuzzy_match);
        }
    }
}

sub add_option
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
         record_number => { isa => 'Int', default => 0, optional => 1 },
         record_name => { isa => 'Str', optional => 0 },
         option_name => { isa => 'Str', optional => 0 },
         option_value => { isa => 'Maybe[Str]', optional => 1 },
         add_record => { isa => 'Bool', default => 0, optional => 1 }
    );
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my $record_number = $parm{'record_number'};
    my $record_name = $parm{'record_name'};
    my $option_name = $parm{'option_name'};
    my $option_value = $parm{'option_value'};
    my $add_record = $parm{'add_record'};

    unless( scalar(@problem_numbers)>0 ){
        $self->problems([]) unless defined $self->problems;
        @problem_numbers = (1 .. $#{$self->problems}+1);
    }

    my @problems = @{$self->problems};
    foreach my $i ( @problem_numbers ) {
        if ( defined $problems[ $i-1 ] ) {
            $problems[$i-1] -> add_option( record_name  => $record_name,
                record_number => $record_number,
                option_name  => $option_name,
                option_value => $option_value,
                add_record   => $add_record );
        }
    }
}

sub set_option
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
        record_name => { isa => 'Str', optional => 0 },
        record_number => { isa => 'Int', default => 0, optional => 1 },
        option_name => { isa => 'Str', optional => 0 },
        option_value => { isa => 'Maybe[Str]', optional => 1 },
        fuzzy_match => { isa => 'Bool', default => 0, optional => 1 }
    );
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my $record_name = $parm{'record_name'};
    my $record_number = $parm{'record_number'};
    my $option_name = $parm{'option_name'};
    my $option_value = $parm{'option_value'};
    my $fuzzy_match = $parm{'fuzzy_match'};

    #record is not added if not already there. Could set option add_record in add_option
    unless( scalar(@problem_numbers)>0 ){
        $self->problems([]) unless defined $self->problems;
        @problem_numbers = (1 .. $#{$self->problems}+1);
    }

    my @problems = @{$self->problems};
    foreach my $i ( @problem_numbers ) {
        if ( defined $problems[ $i-1 ] ) {
            my $found = $self -> is_option_set( 'problem_number' => $i,
                'record'         => $record_name,
                'record_number'  => $record_number,
                'name'           => $option_name,
                'fuzzy_match'    => $fuzzy_match );
            $problems[$i-1] -> remove_option( record_name  => $record_name,
                record_number  => $record_number,
                option_name  => $option_name,
                fuzzy_match  => $fuzzy_match ) if ( $found );
            $problems[$i-1] -> add_option( record_name  => $record_name,
                record_number  => $record_number,
                option_name  => $option_name,
                option_value => $option_value );
        }
    }
}

sub add_marginals_code
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 },
        nomegas => { isa => 'ArrayRef[Int]', optional => 1 }
    );
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @nomegas = defined $parm{'nomegas'} ? @{$parm{'nomegas'}} : ();

    # add_marginals_code takes two arguments.
    #
    # - problem_numbers is an array holding the numbers of the problems in
    # which code should be added.
    #
    # - nomegas which is an array holding the number of (diagonal-element)
    # omegas of each problem given by problem_numbers.
    #
    # For each omega in each problem, verbatim code is added to make the
    # marginals available for printing (e.g. to a table file). COM(1) will
    # hold the nonparametric density, COM(2) the marginal cumulative value
    # for the first eta, COM(2) the marginal cumulative density for the
    # second eta and so on.

    unless( scalar(@problem_numbers)>0 ){
        $self->problems([]) unless defined $self->problems;
        @problem_numbers = (1 .. $#{$self->problems}+1);
    }

    my @problems = @{$self->problems};
    my $j = 0;
    foreach my $i ( @problem_numbers ) {
        if ( defined $problems[ $i-1 ] ) {
            $problems[$i-1] -> add_marginals_code( nomegas => $nomegas[ $j ]  );
        } else {
            croak("Problem number $i does not exist.");
        }
        $j++;
    }
}

sub problem_structure
{
    my $self = shift;
    my @subproblems;

    my ( $val, $pos ) = $self -> _option_val_pos( record_name => 'simulation',
        name        => 'SUBPROBLEMS' );
    if( defined $val ) {
        my @vals = @{$val};
        for( my $i = 0; $i <= $#vals; $i++ ) {
            if( defined $vals[$i] ) {
                if( scalar @{$vals[$i]} > 0 ) {
                    $subproblems[$i] = $vals[$i][0];
                } else {
                    $subproblems[$i] = 1;
                }
            } else {
                $subproblems[$i] = 1;
            }
        }
    }

    return \@subproblems;
}

sub add_nonparametric_code
{
    my $self = shift;

    $self -> set_records( type           => 'nonparametric',
        record_strings => [ 'MARGINALS UNCONDITIONAL' ] );
    $self -> set_option( record_name => 'estimation',
        option_name => 'POSTHOC' );
    my ( $msfo_ref, $junk ) = $self ->
    _get_option_val_pos( name            => 'MSFO',
        record_name     => 'estimation' );
    my @nomegas = @{$self -> nomegas};

    for( my $i = 0; $i <= $#nomegas; $i++ ) { # loop the problems
        my $marg_str = 'ID';
        for( my $j = 0; $j <= $nomegas[$i]; $j++ ) {
            $marg_str = $marg_str.' COM('.($j+1).')=MG'.($j+1);
        }
        $marg_str = $marg_str.' FILE='.$self->filename.'.marginals'.
        ' NOAPPEND ONEHEADER NOPRINT';
        $self -> add_records( problem_numbers => [($i+1)],
            type            => 'table',
            record_strings  => [ $marg_str ] );
        $self -> remove_option( record_name => 'abbreviated',
            option_name => 'COMRES' );
        $self -> add_option( record_name  => 'abbreviated',
            option_name  => 'COMRES',
            option_value => ($nomegas[$i]+1),
            add_record   => 1 );  #Add $ABB if not existing

        $self -> add_marginals_code( problem_numbers => [($i+1)],
            nomegas         => [ $nomegas[$i] ] );
    }

    if( not defined $msfo_ref ) {
        for( my $i = 0; $i < $self -> nproblems; $i++ ) {
            $self -> add_option( record_name =>  'estimation',
                option_name =>  'MSFO',
                option_value => $self -> filename.'.msfo'.($i+1) );
        }
    } else {
        for( my $i = 0; $i < scalar @{$msfo_ref}; $i++ ) {
            if( not defined $msfo_ref->[$i] or not defined $msfo_ref->[$i][0] ) {
                $self -> add_option( record_name =>  'estimation',
                    option_name =>  'MSFO',
                    option_value => $self -> filename.'.msfo'.($i+1) );
            }
        }
    }
}

sub nonparametric_code
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        enabled => { isa => 'ArrayRef[Bool]', optional => 1 },
        problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
    );
    my @enabled = defined $parm{'enabled'} ? @{$parm{'enabled'}} : ();
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @indicators;

    if ( $#problem_numbers > 0 and $#enabled > 0 ){
        if ( $#problem_numbers != $#enabled ) {
            croak("The number of problem_numbers ".($#problem_numbers+1).
                "and enabled/disabled nonparametric_code ".($#enabled+1).
                "do not match" );
        }
    }
    unless( scalar(@problem_numbers) > 0 ){
        $self->problems([]) unless defined $self->problems;
        @problem_numbers = (1 .. $#{$self->problems}+1);
    }
    my @problems = @{$self->problems};
    my $j = 0;
    foreach my $i ( @problem_numbers ) {
        if ( defined $problems[ $i-1 ] ) {
            if ( defined $enabled[ $j ] ) {
                $problems[ $i-1 ] -> nonparametric_code( $enabled[ $j ] );
            } else {
                push( @indicators, $problems[ $i-1 ] -> nonparametric_code );
            }
        } else {
            croak("Problem number $i does not exist!" );
        }
        $j++;
    }

    return \@indicators;
}

sub shrinkage_stats
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        enabled => { isa => 'Bool', optional => 1 },
        problem_numbers => { isa => 'ArrayRef[Int]', optional => 1 }
    );
    my $enabled = $parm{'enabled'};
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @indicators;

    if ( $#problem_numbers > 0 and ref $enabled eq 'ARRAY' ){
        if ( $#problem_numbers != ( scalar @{$enabled} - 1 ) ) {
            croak("The number of problem_numbers ".($#problem_numbers+1).
                "and enabled/disabled shrinkage_stats ".scalar @{$enabled}.
                " do not match" );
        }
    }
    unless( scalar(@problem_numbers) > 0 ) {
        $self->problems([]) unless defined $self->problems;
        @problem_numbers = (1 .. $#{$self->problems}+1);
    }
    my @en_arr;
    if( ref \$enabled eq 'SCALAR' ) {
        for ( @problem_numbers ) {
            push( @en_arr, $enabled );
        }
    } elsif ( not ref $enabled eq 'ARRAY' ) {
        croak('enabled must be a scalar or a reference to an array, '.
            'not a reference to a '.ref($enabled).'.' );
    }

    my @problems = @{$self->problems};
    my $j = 0;
    foreach my $i ( @problem_numbers ) {
        if ( defined $problems[ $i-1 ] ) {
            if ( defined $en_arr[ $j ] ) {
                if( $en_arr[ $j ] ) {
                    $problems[ $i-1 ] -> shrinkage_module -> enable;
                } else {
                    $problems[ $i-1 ] -> shrinkage_module -> disable;
                }
            } else {
                push( @indicators, $problems[ $i-1 ] -> shrinkage_module -> status );
            }
        } else {
            croak("Problem number $i does not exist!" );
        }
        $j++;
    }

    return \@indicators;
}

sub eta_shrinkage
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        eta_filename => { isa => 'Maybe[Str]', optional => 1 }
    );
    my @eta_shrinkage;
    my $eta_filename = $parm{'eta_filename'};

    my @problems = @{$self->problems};
    my $problem_number = 0;
    foreach my $problem ( @problems ) {
        $problem_number++;
        push( @eta_shrinkage, $problem -> eta_shrinkage( model => $self,
                probnum => $problem_number,
                directory => $self -> directory,
                eta_filename => $eta_filename) );
    }

    return \@eta_shrinkage;
}

sub iwres_shrinkage
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        iwres_filename => { isa => 'Maybe[Str]', optional => 1 }
    );
    my @iwres_shrinkage;
    my $iwres_filename = $parm{'iwres_filename'};

    my @problems = @{$self->problems};
    my $problem_number = 0;
    foreach my $problem ( @problems ) {
        $problem_number++;
        push( @iwres_shrinkage, $problem -> iwres_shrinkage( model => $self,
                probnum => $problem_number,
                directory => $self -> directory,
                iwres_filename => $iwres_filename) );
    }

    return \@iwres_shrinkage;
}

sub update_prior_information
{
    my $self = shift;

    foreach my $problem (@{$self->problems}) {
      $problem->update_prior_information;
    }
}

sub set_all_omegas_to_zero
{
    my $self = shift;
    my $fixed = 0;

    foreach my $omega (@{$self->problems->[0]->omegas}) {
        if ($omega->is_block and not $omega->same) {
            $omega->fix(1);
            $fixed = 1;
        } else {
            $fixed = 0;
        }

        foreach my $option (@{$omega->options}) {
            $option->init(0);
            if (not $fixed) {
                $option->fix(1);
            }
        }
    }
}

sub _read_problems
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_lines => { isa => 'ArrayRef', optional => 1 }
    );
    my $model_lines = $parm{'model_lines'};

    # To read problems from a modelfile we need its full name
    # (meaning filename and path). And we need an array for the
    # modelfile lines and an array with indexes telling where
    # problems start in the modelfile array.

    my $file = $self->full_name;
    my ( @modelfile, @problems );
    my ( @problem_start_index );

    # Check if the file is missing, and if that is ok.
    # TODO Check accessor what happens if the file is missing.

    return if( not (-e $file) && $self->ignore_missing_files && (not defined $model_lines));

    # Open the file, slurp it and close it

    if (defined $model_lines){
        @modelfile = @{$model_lines};
    }elsif( not (-e $file)) {
        croak("The model file " . $self->filename . " in ".
            $self->directory()." does not exist.");
    } else {
        open( FILE, "$file" ) ||
        croak("Model -> _read_problems: Could not open $file for reading" );

        @modelfile = <FILE>;
        close( FILE );
    }
    foreach (@modelfile){
        #remove any windows line feed if we are running a dos format file on unix
        s/\r//g;
    }

    # Parse the annotation block
    my $annotation = model::annotation->new();
    $annotation->parse_model(model_lines => \@modelfile);
    $self->annotation($annotation);

    my $start_index = 0;
    my $end_index;
    my $first = 1;
    my $prob_num = 0;
    my $warning_printed = 0;
    my $prev_was_not_sizes = 1;

    my @shrinkage_modules;
    my %internal_msfo_files=(); #hash of filename and problem number, numbering starts at 1

    # It may look like the loop takes one step to much, but its a
    # trick that helps parsing the last problem.
    for (my $i = 0; $i <= @modelfile; $i++) {
        if ($i <= $#modelfile) {
            $_ = $modelfile[$i];
        }

        if ($first and not (/^\s*(;|\$PROB|$|\$SIZ)/ )) {
            croak('Model -> _read_problems: '.
                "First non-comment line in modelfile $file \n".
                'is not a $PROB or $SIZES record. NONMEM syntax violation.');
        }

        # In this if statement we use the lazy evaluation of logical
        # or to make sure we only execute search pattern when we have
        # a line to search. Which is all cases but the very last loop
        # iteration.

        if (/^\s*\$SIZ/ and not $first and $prev_was_not_sizes) {
            croak("\$SIZES must be the first record in a model file");
        }

        if ($i > $#modelfile or ((/^\s*\$PROB/ and $prev_was_not_sizes) or (/^\s*\$SIZ/ and $prev_was_not_sizes))) {
            $end_index = $i;

            # The if statement here is only necessary in the first loop
            # iteration. When start_index == end_index == 0 we want to
            # skip to the next iteration looking for the actual end of
            # the first problem.

            if ( $end_index > $start_index and not $first ) {
                # extract lines of code:
                my @problem_lines = @modelfile[$start_index .. $end_index-1];
                # reset the search for problems by moving the problem start
                # forwards:
                $start_index = $i;
                my $problem_number = scalar @problems + 1;

                my $sh_mod = model::shrinkage_module -> new (
                    nomegas => $self->nomegas->[0],
                    directory => $self->directory,
                    problem_number => $problem_number );
                push @shrinkage_modules, $sh_mod;

                my $prob = model::problem -> new (
                    directory                   => $self->directory,
                    ignore_missing_files        => $self->ignore_missing_files,
                    ignore_missing_output_files => $self->ignore_missing_output_files,
                    sde                         => $self->sde,
                    omega_before_pk             => $self->omega_before_pk,
                    psn_record_order            => $self->psn_record_order,
                    cwres                       => $self->cwres,
                    tbs                         => $self->tbs,
                    dtbs                         => $self->dtbs,
                    tbs_lambda                   => $self->tbs_lambda,
                    tbs_delta                   => $self->tbs_delta,
                    tbs_zeta                   => $self->tbs_zeta,
                    mirror_plots                => $self->mirror_plots,
                    prob_arr                    => \@problem_lines,
                    shrinkage_module            => $sh_mod,
                    internal_msfo_files => \%internal_msfo_files);
                if (defined $prob->tbs_thetanum) {
                    $self->tbs_thetanum($prob->tbs_thetanum);
                }

                push( @problems, $prob );
                my $array = $prob->get_msfo_filenames;
                if (scalar(@{$array})>1){
                    ui->print(category=> 'all',
                              message => 'Warning: MSFO set to different file names in same $PROBLEM: '.join(' ',@{$array})."\n");
                }
                foreach my $fn (@{$array}){
                    $internal_msfo_files{$fn}=scalar(@problems); #probnum 1 or higher
                }

                if ( $self->cwres ) {
                    my @eo;
                    if ( defined $self->extra_output ) {
                        @eo = @{$self->extra_output};
                    }
                    if ( $prob->cwres_modules ) {
                        push( @eo, @{$prob->cwres_modules->[0]->cwtab_names()} );
                    }
                    $self->extra_output(\@eo);
                }

                $prob_num++;
            }
            $first = 0;
        }
        if (/^\s*\$/ ){
            if (/^\s*\$SIZ/ ){
                $prev_was_not_sizes = 0;
            }else{
                $prev_was_not_sizes = 1;
            }
        }
    }

    # Set the problems in the modelobject.
    if (scalar(@problems)<1){
        croak('Model -> _read_problems: '.
            "Could not find any problem in modelfile $file");
    }
    $self -> problems(\@problems);
    for my $module (@shrinkage_modules) {   # Need to set nomegas after parsing problems for shrinkage_modules
        $module->nomegas($self->nomegas->[0]);
    }
}

sub _get_option_val_pos
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str', optional => 1 },
        record_name => { isa => 'Str', optional => 1 },
        problem_numbers => { isa => 'ArrayRef[Maybe[Int]]', optional => 1 },
        instances => { isa => 'ArrayRef[Int]', optional => 1 },
        global_position => { isa => 'Bool', default => 0, optional => 1 }
    );
    my $name = $parm{'name'};
    my $record_name = $parm{'record_name'};
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my @instances = defined $parm{'instances'} ? @{$parm{'instances'}} : ();
    my $global_position = $parm{'global_position'};
    my @values;
    my @positions;

    # Usage:
    #
    #   ( $values_ref, $positions_ref ) ->
    #               _get_option_val_pos ( name        => 'ID',
    #                                     record_name => 'input' );
    #   my @values = @{$values_ref};
    #   my @positions = @{$positions_ref};
    #
    # This basic usage returns the name of the third option in the first
    # instance of the record specified by I<record_name> for all problems
    #
    # If global_position is set to 1, only one value and position
    # pair is returned per problem. If there are more than one
    # match in the model; the first will be returned for each
    # problem.
    #
    # Private method, should preferably not be used outside model.pm

    my $accessor = $record_name.'s';
    croak("model::_get_option_val_pos No problems defined") unless (defined $self->problems);
    my @problems = @{$self->problems};
    unless( scalar(@problem_numbers) > 0 ) {
        @problem_numbers = (1 .. $#{$self->problems}+1);
    }
    foreach my $i ( @problem_numbers ) {
        my $rec_ref = $problems[ $i-1 ] -> $accessor;
        if ( defined $problems[ $i-1 ] and defined $rec_ref ) {
            my @records = @{$rec_ref};
            unless( $#instances > 0 ){
                @instances = (1 .. $#records+1);
            }
            my @inst_values    = ();
            my @inst_positions = ();
            my $glob_pos = 1;
            my ( $glob_value, $glob_position );
            INSTANCES:  foreach my $j ( @instances ) {
                if ( defined $records[ $j-1 ] ) {
                    my $k = 1;
                    my ( $value, $position );
                    foreach my $option ( @{$records[$j-1]->options} ) {
                        if ( defined $option and $option -> name eq $name) {
                            if ( $global_position ) {
                                $glob_value = $option -> value;
                                $glob_position = $glob_pos;
                                last INSTANCES;
                            } else {
                                $value = $option -> value;
                                $position = $k;
                            }
                        }
                        $k++;
                        $glob_pos++;
                    }
                    push( @inst_values, $value );
                    push( @inst_positions, $position );
                } else {
                    croak("Instance $j in problem number $i does not exist!" )
                }
            }
            if ( $global_position ) {
                push( @values, $glob_value );
                push( @positions, $glob_position );
            } else {
                push( @values, \@inst_values );
                push( @positions, \@inst_positions );
            }
        } else {
            croak("Problem number $i does not exist!" );
        }
    }

    return \@values ,\@positions;
}

sub _option_name
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         position => { isa => 'Num', default => 1, optional => 1 },
         record => { isa => 'Str', optional => 1 },
         problem_number => { isa => 'Int', default => 1, optional => 1 },
         instance => { isa => 'Int', default => 1, optional => 1 },
         new_name => { isa => 'Str', optional => 1 }
    );
    my $position = $parm{'position'};
    my $record = $parm{'record'};
    my $problem_number = $parm{'problem_number'};
    my $instance = $parm{'instance'};
    my $new_name = $parm{'new_name'};
    my $name;

    if ($record eq 'data' and ($position == 0)){
        if (defined $new_name){
            croak("use setter in data record to change data file name");
        }else{
            croak("use getter in data record for data file name");
        }
    }
    my ( @problems, @records, @options, $i );
    my $accessor = $record.'s';
    if ( defined $self->problems ) {
      @problems = @{$self->problems};
    } else {
      croak("No problems defined in model" );
    }
    if ( defined $problems[$problem_number - 1] -> $accessor ) {
      @records = @{$problems[$problem_number - 1] -> $accessor};
    } else {
      croak("No record $record defined in ".
              "problem number $problem_number." );
    }
    if ( defined $records[$instance - 1] -> options ) {
      @options = @{$records[$instance - 1] -> options};
    } else {
      croak("model -> _option_name: No option defined in record ".
              "$record in problem number $problem_number." );
    }
    $i = 0;
    foreach my $option ( @options ) {
      if ( $i == $position ) {
        if ( defined $new_name ){
          $option -> name($new_name) if ( defined $option );
        }else{
          $name = $option -> name if ( defined $option );
        }
      }
      $i++;
    }

    return $name;
}

sub _parameter_count
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         record => { isa => 'Str', optional => 1 },
         problem_number => { isa => 'Int', default => 1, optional => 1 }
    );
    my $record = $parm{'record'};
    my $problem_number = $parm{'problem_number'};
    my $count = 0;

    if( defined $self->problems ){
      my $problems = $self->problems;
      if( defined $problems->[$problem_number - 1] ){
        $count = $problems->[$problem_number - 1] -> record_count( 'record_name' => $record );
      }
    }

    return $count;
}

sub _init_attr
{
    my $self = shift;
    my %parm = validated_hash(\@_,
         parameter_type => { isa => 'Str', optional => 0 },
         with_priors => { isa => 'Bool', default => 0, optional => 1 },
         get_same => { isa => 'Bool', default => 0, optional => 1 },
         parameter_numbers => { isa => 'ArrayRef', optional => 1 },
         attribute => { isa => 'Str', optional => 1 },
         new_values => { isa => 'ArrayRef', optional => 1 },
         problem_numbers => { isa => 'ArrayRef[Num]', optional => 1 },
         add_if_absent => { isa => 'Bool', default => 0, optional => 1 }
    );
    my $parameter_type = $parm{'parameter_type'};
    my $with_priors = $parm{'with_priors'};
    my $get_same = $parm{'get_same'};
    my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
    my $attribute = $parm{'attribute'};
    my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
    my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
    my $add_if_absent = $parm{'add_if_absent'};
    my @parameter_values;

    # The I<add_if_absent> argument tells the method to add an init (theta,omega,sigma)
    # if the parameter number points to a non-existing parameter with parameter number
    # one higher than the highest presently included. Only applicatble if
    # I<new_values> are set. Default value = 0;

    unless( scalar @problem_numbers > 0 ){
        $self->problems([]) unless defined $self->problems;
      @problem_numbers = (1 .. $#{$self->problems}+1);
    }
    my @problems = @{$self->problems};
    if ( $#new_values >= 0 ) {
      croak("The number of new value sets " .
              ($#new_values+1) . " do not" .
              " match the number of problems " . ($#problem_numbers+1) . " specified" )
          unless(($#new_values == $#problem_numbers) );
      if ( $#parameter_numbers > 0 ) {
        croak("The number of parameter number sets do not" .
                " match the number of problems specified" )
        unless(($#parameter_numbers == $#problem_numbers) );
      }
    }

    my $new_val_idx = 0;
        foreach my $i ( @problem_numbers ) {
      if ( defined $problems[ $i-1 ] ) {
        if ( scalar @new_values > 0) {
          # {{{ Update values
          # Use attribute parameter_values to collect diagnostic outputs
          push( @parameter_values,
            $problems[ $i-1 ] ->
            _init_attr( parameter_type    => $parameter_type,
                parameter_numbers => $parameter_numbers[ $new_val_idx ],
                new_values        => \@{$new_values[ $new_val_idx ]},
                attribute         => $attribute,
                    add_if_absent     => $add_if_absent ) );
          # }}} Update values
        } else {
          # {{{ Retrieve values
          push( @parameter_values,
            $problems[ $i-1 ] ->
            _init_attr( parameter_type    => $parameter_type,
                parameter_numbers => $parameter_numbers[ $i-1 ],
                with_priors       => $with_priors,
                get_same          => $get_same,
                attribute         => $attribute ) );
          # }}} Retrieve values
        }
      } else {
        croak("Problem number $i does not exist!" );
      }
      $new_val_idx++;
    }

    return \@parameter_values;
}

sub create_dummy_model
{
    my $dummy_prob = model::problem->new(ignore_missing_files=> 1,
                                         prob_arr       => ['$PROB','$INPUT ID','$DATA dummy.txt']);

    my $model = model->new(filename => 'dummy',
                           problems => [$dummy_prob],
                           is_dummy => 1,
                           ignore_missing_files => 1);

    return $model;
}

sub get_estimation_evaluation_problem_number
{
    #get the problem number to check status of in modelfit::restart_needed
    #always the first $PROB, if any, that has $EST that is not MAXEVAL=0
    #standard case is 1, if that prob is estimation
    # with two $PROB and $PRIOR TNPRI it is 2, if it is estimation
    # if do not find $PROB with est then return 0, i.e. turn off tweak inits
    my $self = shift;

    my $probnum=-1;

    if (defined $self->outputs and defined $self->outputs->[0] and $self->outputs->[0]->have_output()){
        $self->outputs->[0]->load;
        $probnum = $self->outputs->[0]->get_estimation_evaluation_problem_number();
    }else{
        for (my $i=1; $i <= scalar (@{$self->problems}); $i++ ){
            if ($self->is_estimation(problem_number=>$i)){
                $probnum = $i;
                last;
            }
        }
    }
    return ($probnum);
}

sub get_eta_names
{
    # Gets the names of the ETAs from a list in the code
    # ETA_CL = ETA(1)

    my $self = shift;

    my @names;
       my @code;
    if ($self->has_code(record => 'pk')) {
        @code = @{$self->get_code(record => 'pk')};
    } else {
        @code = @{$self->get_code(record => 'pred')};
    }

    foreach my $line (@code) {
        if ($line =~ /^\s*(\w+)\s*=\s*ETA\(\d+\)/) {
            push @names, $1;
        }
    }

    return \@names;
}

sub get_run_number_string
{
    #static method, no shift
    my %parm = validated_hash(\@_,
                              filename => { isa => 'Str', optional => 0 },
        );
    my $filename = $parm{'filename'};
    #input is model file name
    #output is number(s) plus possible anything up to first dot, which has to be there
    my $number = undef;
    if ($filename =~ /^(run|Run|RUN)([0-9]+[^0-9.]*)\./){
        $number = $2;
    }
    return $number;
}

sub get_xpose_runno_and_suffix
{
    #static method, no shift
    my %parm = validated_hash(\@_,
                              filename => { isa => 'Str', optional => 0 },
        );
    my $filename = $parm{'filename'};
    #input is table file name

    my $runno = undef;
    my $suffix = '';
    if ($filename =~ /^..tab([^.]+)(.*)/){
        $runno=$1;
        if (length($2)>0){
            $suffix = $2;
        }
    }
    return [$runno,$suffix];
}

sub need_data_filtering
{
    my $self = shift;

    my $ret = $self->problems->[0]->datas->[0]->have_ignore_accept();
    return $ret > 0 ? 1 : 0;    # Because moose doesn't accept 2 as true
}

sub update_internal_msfi
{
    my $self = shift;

    my @updated=();
    my $msfo_files = $self->msfo_names(); #array over problems
    for (my $j=0; $j<scalar(@{$self->problems}); $j++){
        if (defined $self->problems->[$j]->msfis and scalar(@{$self->problems->[$j]->msfis})>0){
            if (scalar(@{$self->problems->[$j]->msfis})>1){
                ui->print(category => 'all',message => 'warning: more than one MSFI in problem '.($j+1));
            }
            my $probnum = $self->problems->[$j]->msfis->[0]->get_msfo_from_problem_number;
            if ( $probnum>0){
                if (defined $msfo_files->[($probnum-1)]){
                    $self->problems->[$j]->msfis->[0]->set_filename(filename => $msfo_files->[($probnum-1)]);
                    push(@updated,($j+1));
                }else{
                    ui->print(category => 'all',message => "error update_internal_msfi problem ".($j+1).
                              " there is no msfo from problem $probnum but msfi depends on that problem");
                }
            }
        }
    }
    return \@updated;
}

sub set_first_problem_msfi
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              msfiname => { isa => 'Str', optional => 0 },
                              newmsfo => { isa => 'Str', optional => 1 },
                              extra_options => { isa => 'HashRef', optional => 1 },
                              set_new_msfo => { isa => 'Bool', optional => 1, default => 0 },
                              add_msfo_if_absent => { isa => 'Bool', optional => 1, default => 0 },
        );
    my $msfiname = $parm{'msfiname'};
    my $newmsfo = $parm{'newmsfo'};
    my $extra_options = $parm{'extra_options'};
    my $set_new_msfo = $parm{'set_new_msfo'};
    my $add_msfo_if_absent = $parm{'add_msfo_if_absent'};

    #Add rec in first prob if not there. If already there only change filename
    if (defined $self->problems->[0]->msfis and
        scalar(@{$self->problems->[0]->msfis})>0){
        $self->problems->[0]->msfis->[0]->set_filename(filename=>$msfiname);
    }else{
        $self->problems->[0]->set_records(type => 'msfi', record_strings => [$msfiname]);
    }

    if (defined $extra_options){
        foreach my $option (keys %{$extra_options}){
            my $value = $extra_options->{$option}; #can be undef
            my $found = $self->problems->[0]->is_option_set(record => 'msfi',
                                                            name           => $option,
                                                            fuzzy_match    => 1 );
            $self->problems->[0] -> remove_option( record_name  => 'msfi',
                                                   option_name  => $option,
                                                   fuzzy_match  => 1) if ( $found );
            $self->problems->[0] -> add_option( record_name  => 'msfi',
                                                option_name  => $option,
                                                option_value => $value );

        }
    }
    $self->remove_records(type => 'theta',problem_numbers =>[1]);
    $self->remove_records(type => 'omega',problem_numbers =>[1]);
    $self->remove_records(type => 'sigma',problem_numbers =>[1]);

    if ($set_new_msfo and (not defined $newmsfo)){
        require model::problem::msfi;
        my $modelname = $self->filename;
        my ($base,$msftype,$extension) = model::problem::msfi::get_basename_msftype_extension(filename => $modelname);
        $newmsfo = $base.'.msf';
    }

    if (defined $newmsfo){
        if ($msfiname eq $newmsfo){
            ui->print(category => 'all',message => "warning: set_first_problem_msfi same name msfo, msfi $newmsfo");
        }
        $self->rename_msfo(name => $newmsfo, add_if_absent => $add_msfo_if_absent);
    }
}

sub rename_msfo
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              name => { isa => 'Str', optional => 0 },
                              add_if_absent => { isa => 'Bool', optional => 1, default => 0 },
                              problem_index =>{ isa => 'Int', optional => 1, default => 0 },
        );
    my $name = $parm{'name'};
    my $add_if_absent = $parm{'add_if_absent'};
    my $problem_index = $parm{'problem_index'};

    if (defined $self->problems->[$problem_index]->estimations){
        foreach my $rec (@{$self->problems->[$problem_index]->estimations}){
            $rec->rename_msfo(name => $name, add_if_absent => $add_if_absent);
        }
    }
    if (defined $self->problems->[$problem_index]->nonparametrics){
        foreach my $rec (@{$self->problems->[$problem_index]->nonparametrics}){
            $rec->rename_msfo(name => $name, add_if_absent => $add_if_absent);
        }
    }
    my $updatedmsfi = $self->update_internal_msfi;

}

sub renumber_msfo_msfi
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              numberstring => { isa => 'Str', optional => 0 },
        );
    my $numberstring = $parm{'numberstring'};

    my @probmsfo=();
    my $updatedmsfi=[];
    my $msfo_files = $self->msfo_names(); #array over problems
    for (my $j=0; $j<scalar(@{$self->problems}); $j++){
        if (defined $msfo_files->[$j]){
            if (defined $self->problems->[$j]->estimations){
                foreach my $rec (@{$self->problems->[$j]->estimations}){
                    $rec->renumber_msfo (numberstring => $numberstring);
                }
            }
            if (defined $self->problems->[$j]->nonparametrics){
                foreach my $rec (@{$self->problems->[$j]->nonparametrics}){
                    $rec->renumber_msfo (numberstring => $numberstring);
                }
            }
            push(@probmsfo,($j+1));
        }
    }
    if (scalar(@probmsfo)>0){
        #anything updated
        $updatedmsfi = $self->update_internal_msfi;
    }
    return (\@probmsfo,$updatedmsfi);
}

sub msfo_to_msfi_mismatch
{
    my $self = shift;
    return 0 unless (scalar(@{$self->problems})>1);

    my $msfo = $self->msfo_names(); #array over probs
    my $msfi = $self->msfi_names(); #array over probs

    my $prev_msfo= undef;
    for (my $i=0; $i<scalar(@{$self->problems}); $i++){
        if (defined $prev_msfo){
            if (defined $msfi->[$i] ){
                unless ($prev_msfo eq $msfi->[$i]){
                    return ($i+1); #mismatch at problem number $i+1
                }
            }
        }
        if (defined $msfo->[$i] ){
            $prev_msfo = $msfo->[$i];
        }
    }
    return 0;

}

sub init_etas
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        phi_from_base => { isa => 'Bool', default => 0 },        # Should we use the phi-file from the base model? If not use the current phi-file
        phi_name => { isa => 'Str', optional => 1 },            # This will be used if specified
        full_path => { isa => 'Bool', default => 0 },
    );
    my $phi_from_base = $parm{'phi_from_base'};
    my $phi_name = $parm{'phi_name'};
    my $full_path = $parm{'full_path'};

    if (not defined $phi_name) {
        if ($phi_from_base) {
            my $based_on = $self->annotation->get_based_on();
            if (defined $based_on) {
                $phi_name = "run$based_on.phi";
            } else {
                print "Warning: the model " . $self->filename . " wasn't based on any other model. Option -etas skipped\n";
            }
        } else {
            $phi_name = utils::file::replace_extension($self->filename, 'phi');
        }
    }

    if ($full_path) {
        $phi_name = File::Spec->rel2abs($phi_name, $self->directory);
    }

    if (-e $phi_name) {
        $self->set_records(type => 'etas', record_strings => [ "FILE=$phi_name" ]);
        $self->remove_option(
            record_name => 'estimation',
            option_name => 'MCETA',
        );
        $self->add_option(
            record_name => 'estimation',
            option_name => 'MCETA',
            option_value => ( '1' ),
            add_record => 0,
        );
    } else {
        print "Warning: the phi file $phi_name does not exist. Option -etas skipped\n";
    }

    return $phi_name;
}

sub find_input_column
{
    # Find the number of the column given a name. If the name is numeric just return it.
    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str', optional => 0 },
    );
    my $name = $parm{'name'};

    if (not $name =~ /^\d/) {
        my ($junk, $column_position) = $self->_get_option_val_pos(
            name => $name,
            record_name => 'input',
            problem_numbers => [1],
        );
        # We assume that there are no duplicate column names
        return $column_position->[0][0];
    }

    return $name;
}

sub get_phi_file
{
    # Get the full path of phi file of this model
    # The attribute phi_file will override in case there is a user specified phi file
    my $self = shift;

    if (defined $self->phi_file) {
        return $self->phi_file;
    }

    my $name = $self->outputs->[0]->full_name;
    $name = utils::file::replace_extension($name, 'phi');

    if (not -e $name) {
        undef $name;
    }

    return $name;
}

sub get_or_set_etas_file
{
    # Get the (full) path of file specified on $ETAS record of this model
    # (undef if no record or no such option), or set it to new value
    # (record and option MUST then exist)
    my $self = shift;
    my %parm = validated_hash(\@_,
         problem_number => { isa => 'Num', default => 1, optional => 1 },
         new_file => { isa => 'Str', optional => 1 }
    );
    my $problem_number = $parm{'problem_number'};
    my $new_etas_file = $parm{'new_file'};

    my $filename = undef;
    if (scalar $self->record(record_name => 'etas', problem_number => $problem_number) > 0) {
        my $new_values = [];
        if ($new_etas_file) {
            push @{$new_values}, $new_etas_file;
        }

        my ($vals,$junk) = $self->problems->[$problem_number-1]->_option_val_pos(
            record_name => 'etas',
            name => 'FILE',
            exact_match => 0
        );
        if (scalar @{$vals} > 0) {
            my ($dir, $file) = OSspecific::absolute_path($self->directory, $vals->[0]);
            $filename = $dir.$file;

            if ($new_etas_file) {
                $self->problems->[$problem_number-1]->_option_val_pos(
                    record_name => 'etas',
                    name => 'FILE',
                    new_values => $new_values,
                    exact_match => 0
                );
            }
        } elsif ($new_etas_file) {
            croak "FILE option of ETAS record could not be set: FILE option not found";
        }
    } elsif ($new_etas_file) {
        croak "FILE option of ETAS record could not be set: ETAS record not found";
    }

    return $filename;
}

sub unfix_omega_0_fix
{
    # Unfix all omegas that are set to 0 FIX
    my $self = shift;

    my $did_fix = 0;
    for my $record (@{$self->problems->[0]->omegas}) {
        for my $option (@{$record->options}) {
            if ($option->init == 0 and $option->fix) {
                $did_fix = 1;
                $option->fix(0);
                $option->init(0.01);
            }
        }
    }

    return $did_fix;
}

sub get_pk_or_pred_code
{
    my $self = shift;

    my @code;
    my $code_record;
    if ($self->has_code(record => 'pk')) {
        @code = @{$self->get_code(record => 'pk')};
        $code_record = 'pk';
    } elsif ($self->has_code(record => 'pred')) {
        @code = @{$self->get_code(record => 'pred')};
        $code_record = 'pred';
    } else {
        croak("Neither PK nor PRED defined in " . $self->filename . "\n");
    }

    return ($code_record, \@code);
}

sub defined_variable
{
    # Check if a variable is defined in code or in $INPUT
    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str' },
    );
    my $name = $parm{'name'};

    if ($self->problems->[0]->find_data_column(column_name => $name) != -1) {
        return 1;
    }

    my @code_records = ( 'pred', 'pk', 'error' );
    for my $record (@code_records) {
        my $code = $self->get_code(record => $record);
        if (defined $code) {
            for my $line (@$code) {
                if ($line =~ /^[^;]*\b$name\s*=/) {
                    return 1;
                }
            }
        }
    }

    return 0;
}

sub find_input_synonyms
{
    # Find synonyms for listed input column names
    # Return a hash from all symbols that has synonyms to the synonyms
    my $self = shift;
    my %parm = validated_hash(\@_,
        columns => { isa => 'ArrayRef[Str]' },
    );
    my $columns = $parm{'columns'};

    my %synonyms;
    for my $record (@{$self->problems->[0]->inputs}) {
        for my $option (@{$record->options}) {
            if (defined $option->value and $option->value ne '') {
                next if ($option->is_drop());
                for my $col (@$columns) {
                    if ($option->name eq $col) {
                        $synonyms{$col} = $option->value;
                    } elsif ($option->value eq $col) {
                        $synonyms{$col} = $option->name;
                    }
                }
            }
        }
    }

    return \%synonyms;
}

sub have_output
{
    my $self = shift;
    return (defined $self->outputs and defined $self->outputs->[0] and $self->outputs->[0]->have_output());
}

1;
