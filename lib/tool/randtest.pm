package tool::randtest;

use include_modules;
use strict;
use File::Copy 'cp';
use data;
use OSspecific;
use tool::modelfit;
use Math::Random;
use Data::Dumper;
use Moose;
use MooseX::Params::Validate;

extends 'tool';

has 'randtest_raw_results' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'copy_data' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'samples' => ( is => 'rw', required => 1, isa => 'Int' );
has 'base_model' => ( is => 'rw', isa => 'model' );
has 'stratify_on' => ( is => 'rw', isa => 'Str' );
has 'strat_index' => ( is => 'rw', isa => 'Int' );
has 'rand_index' => ( is => 'rw', isa => 'Int' );
has 'randomization_column' => ( is => 'rw', required => 1, isa => 'Str' );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['randtestlog.csv'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'randtest_results.csv' );
has 'match_transitions' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'reference_column' => ( is => 'rw', isa => 'Str' );


sub BUILD
{
	my $self  = shift;

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

	croak("No \$PROBLEM in input model") unless 
	(defined $self ->models()->[0]->problems and scalar(@{$self ->models()->[0]->problems})>0);

	croak("No \$INPUT found") unless 
	(defined $self ->models()->[0]->problems->[0]->inputs and 
		scalar(@{$self ->models()->[0]->problems->[0]->inputs})>0);
	croak("No \$DATA found") unless 
	(defined $self ->models()->[0]->problems->[0]->datas and 
		scalar(@{$self ->models()->[0]->problems->[0]->datas})>0);

	#make sure IGNORE=C is not used

	croak("PsN randtest cannot handle IGNORE=C. Use IGNORE=@ instead\n")
		if ($self->models->[0]->problems->[0]->datas->[0]->ignoresign eq 'C');
	

	#Find column index of rand column
	#Find column index of strat column
	my $counter = 0;
	foreach my $opt (@{$self->models->[0]->problems->[0]->inputs->[0]->options()}){
		$self->rand_index($counter) if ($opt->name() eq $self->randomization_column());
		$self->strat_index($counter) if ((defined $self->stratify_on()) and ($opt->name() eq $self->stratify_on()));
		$counter++;
	}
	croak("Could not find randomization column ".$self->randomization_column()." in \$INPUT")
	unless (defined $self->rand_index);
	croak("Could not find stratification column ".$self->stratify_on()." in \$INPUT")
	unless ((not defined $self->stratify_on) or (defined $self->strat_index));

	croak("Number of samples must be larger than 0") unless ($self->samples()>0);
}


sub modelfit_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	my $model = $self ->models() -> [$model_number-1];

	# Check which models that hasn't been run and run them 

	# ------------------------  Run original run  -------------------------------

	unless ( $model -> is_run and ((not defined $self->base_model) or $self->base_model->is_run) ) {
		my %subargs = ();
		if ( defined $self -> subtool_arguments() ) {
			%subargs = %{$self -> subtool_arguments()};
		}

		if( $self -> nonparametric_etas() or
			$self -> nonparametric_marginals() ) {
			$model -> add_nonparametric_code unless ($model->is_run);
			$self->base_model -> add_nonparametric_code if (defined $self->base_model and not $self->base_model->is_run);
		}
		my @models=();
		my $message = "Executing ";
		unless ($model->is_run){
			push(@models,$model) ;
			$message .= "input model";
		}
		if (defined $self->base_model and not $self->base_model->is_run){
			push(@models,$self->base_model) ;
			if ($model->is_run){
				$message .= "base model";
			}else{
				$message .= "and base model";
			}
		}

		my $orig_fit = tool::modelfit ->
		new( %{common_options::restore_options(@common_options::tool_options)},
			base_directory	 => $self ->directory(),
			directory		 => $self ->directory().
			'/orig_modelfit_dir'.$model_number,
			models		 => \@models,
			threads               => $self->threads,
			logfile	         => undef,
			raw_results           => undef,
			 prepared_models       => undef,
			 copy_data            => $self->copy_data,
			 top_tool              => 0,
			%subargs );

		ui -> print( category => 'randtest',
			message => $message );

		$orig_fit -> run;

	}

	my $output = $model -> outputs -> [0];
	my $base_output;
	$base_output = $self->base_model -> outputs ->[0] if (defined $self->base_model);
	my $new_mod;
	my @problems   = @{$model -> problems};
	my @new_models;

	if (scalar(@{$model -> problems})>1){
		print "\nWarning: PsN randtest only randomizes data file of first \$PROB, seems like model has more than one \$PROB\n";
	}

	my $done = ( -e $self ->directory()."/m$model_number/done" ) ? 1 : 0;
	my $new_datas;
	if ( not $done ) {
		my $orig_data_file =  $model -> datafiles(absolute_path => 1, problem_numbers => [1])->[0];
		ui -> print( category => 'randtest',
			message  => "Randomizing column ".$self->randomization_column." in ".$orig_data_file );
		my $idcol= $model -> idcolumn();
		my $ignoresign=defined $model -> ignoresigns ? $model -> ignoresigns -> [0] : '@';

		$new_datas = data::create_randomized_data( output_directory   => $self ->directory().'/m'.$model_number,
												   input_filename => $orig_data_file,
												   idcolumn => $idcol,
												   ignoresign => $ignoresign,
												   missing_data_token => $self->missing_data_token,
												   name_stub   => 'rand',
												   samples     => $self->samples(),
												   stratify_index => $self->strat_index(), 
												   rand_index => $self->rand_index(), 
												   equal_obs => (not $self->match_transitions())
			);

		$self->stop_motion_call(tool=>'randtest',message => "Created randomized datasets in ".
			$self ->directory().'m'.$model_number)
		if ($self->stop_motion());

		for ( my $j = 0; $j < $self->samples(); $j++ ) {
			my @data_arr = ($new_datas->[$j]) x scalar(@{$model->problems});

			$new_mod = $model ->  copy( filename    => $self -> directory().'m'.$model_number.'/rand_'.($j+1).'.mod',
										output_same_directory => 1,
										copy_datafile   => 0,
										copy_output => 0,
										write_copy => 0);
			$new_mod->relative_data_path(1); #data is in m1
			$new_mod->datafiles(new_names => \@data_arr); #Number of $PROBS and length data_arr must match

			if( $self -> shrinkage() ) {
				$new_mod -> shrinkage_stats( enabled => 1 );
				$new_mod -> shrinkage_modules( $model -> shrinkage_modules );
			}

			if( $self -> nonparametric_etas() or
				$self -> nonparametric_marginals() ) {
				$new_mod -> add_nonparametric_code;
			}

			$new_mod -> update_inits( from_output => $output );
			$new_mod -> _write;

			push( @new_models, $new_mod );
		}
		$self->stop_motion_call(tool=>'randtest',message => "Created one modelfile per dataset in ".
			$self ->directory().'m'.$model_number)
		if ($self->stop_motion());

		# Create a checkpoint. Log the samples and individuals.
		open( DONE, ">".$self ->directory()."/m$model_number/done" ) ;
		print DONE "Randomization of ",$orig_data_file, " performed\n";
		print DONE $self->samples()." samples\n";
		close( DONE );
	} else {
		ui -> print( category => 'randtest',
			message  => "Recreating randtest from previous run." );

		# Recreate the datasets and models from a checkpoint
		my ($stored_filename, $stored_samples);
		my ($stored_filename_found, $stored_samples_found);
		open( DONE, $self ->directory()."/m$model_number/done" );
		while( <DONE> ){
			if( /^Randomization of (.+) performed$/ ){
				$stored_filename = $1;
				$stored_filename_found = 1;
				next;
			}
			if( /^(\d+) samples$/ ){
				ui -> print( category => 'randtest',
					message  => "Samples saved: $1" );
				$stored_samples = $1;
				$stored_samples_found = 1;
				next;
			}
		}
		close( DONE );
		unless( $stored_filename_found and $stored_samples_found ) {
			croak("The randtest/m1/done file could not be parsed.");
		}
		if ( $stored_samples < $self->samples() ) {
			croak("The number of samples saved in previous run ($stored_samples) ".
				"is smaller than the number of samples specified for this run (".
				$self->samples().")" );
		}

		# Reinitiate the model objects
		for ( my $j = 1; $j <= $self->samples(); $j++ ) {
			my ($model_dir, $filename) = OSspecific::absolute_path( $self ->directory().'/m'.$model_number,
				'rand_'.($j+1).'.mod' );

			$new_mod = model->new( directory   => $model_dir,
								   filename    => $filename,
								   extra_files => $model -> extra_files,
								   ignore_missing_files => 1);
			push( @new_models, $new_mod );
		}
		ui -> print( category => 'randtest',
			message  => "Using $stored_samples previously randomized ".
			"data sets sets from $stored_filename" )
	}

	$self -> prepared_models -> [$model_number-1]{'own'} = \@new_models;

	my @subtools = ();
	@subtools = @{$self -> subtools()} if (defined $self->subtools());
	shift( @subtools );
	my %subargs = ();
	if ( defined $self -> subtool_arguments() ) {
		%subargs = %{$self -> subtool_arguments()};
	}
	$self->tools([]) unless (defined $self->tools());

	push( @{$self -> tools()},
		tool::modelfit ->
		new( %{common_options::restore_options(@common_options::tool_options)},
			models		 => \@new_models,
			threads               => $self->threads,
			directory             => $self ->directory().'/modelfit_dir'.$model_number,
			_raw_results_callback => $self ->
			_modelfit_raw_results_callback( model_number => $model_number ),
			subtools              => \@subtools,
			nmtran_skip_model => 2,
			logfile		 => [$self -> logfile()->[$model_number-1]],
			raw_results           => undef,
			prepared_models       => undef,
			 copy_data            => 0,
			top_tool              => 0,
			%subargs ) );

	$self->stop_motion_call(tool=>'randtest',message => "Created a modelfit object to run all the models in ".
		$self ->directory().'m'.$model_number)
	if ($self->stop_motion());
}

sub cleanup
{
	my $self = shift;

	#remove datafiles in modelfit_dirX/NM_runX
	#leave in m1

	my $prob=1;
	while (1){
		my $dir = $self ->directory()."modelfit_dir$prob/";
		last unless (-e $dir);
		my $sample=1;
		while (1){
			my $file = $dir."NM_run".$sample."/rand_".$sample.".dta"; 
			last unless (-e $file);
			unlink $file;
			$sample++;
		}
		$prob++;
	}
}


sub calculate_delta_ofv
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};
}

sub general_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 },
		class => { isa => 'Str', optional => 1 }
	);
	my $model_number = $parm{'model_number'};
	my $class = $parm{'class'};
}

sub modelfit_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	1;
}

sub modelfit_post_fork_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};
}

sub _modelfit_raw_results_callback
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};
	my $subroutine;

	# Use the  raw_results file.
	my ($dir,$file) = 
	OSspecific::absolute_path( $self ->directory(),
		$self -> raw_results_file()->[$model_number-1] );
	my ($dir,$nonp_file) = 
	OSspecific::absolute_path( $self ->directory(),
		$self -> raw_nonp_file()->[$model_number-1] );
	my $orig_mod = $self ->models()->[$model_number-1];
	my $base_mod_ofv;
	my $base_mod;
	if (defined $self->base_model and $self->base_model->is_run){
		$base_mod= $self->base_model;
		$base_mod_ofv=$self->base_model->outputs->[0]->ofv(); #array over problems and subprobs
	}

	$subroutine = sub {
		my $modelfit = shift;
		my $mh_ref   = shift;
		my %max_hash = %{$mh_ref};
		$modelfit -> raw_results_file([$dir.$file] );
		$modelfit -> raw_nonp_file( [$dir.$nonp_file] );

		# The prepare_raw_results in the modelfit will fix the
		# raw_results for each rand sample model, we must add
		# the result for the original model.

		my %dummy;

		my ($raw_results_row, $nonp_rows) = $self -> create_raw_results_rows( max_hash => $mh_ref,
			model => $orig_mod,
			raw_line_structure => \%dummy );

		my ($base_raw_results_row, $base_nonp_rows);
		if (defined $base_mod){
			($base_raw_results_row, $base_nonp_rows) = $self -> create_raw_results_rows( max_hash => $mh_ref,
				model => $base_mod,
				raw_line_structure => \%dummy );
		}
		$orig_mod -> outputs -> [0] -> flush;
		$raw_results_row->[0]->[0] = 'input';

		unshift( @{$modelfit -> raw_results()}, @{$raw_results_row} );
		if (defined $base_raw_results_row){
			$base_raw_results_row->[0]->[0] = 'base';
			unshift( @{$modelfit -> raw_results()}, @{$base_raw_results_row} ) ;
		}
		$self->raw_line_structure($modelfit -> raw_line_structure());

		if ( defined $base_mod_ofv ) {
			my ($start,$len) = split(',',$self->raw_line_structure() -> {1}->{'problem'});
			my $probindex = $start;
			my ($start,$len) = split(',',$self->raw_line_structure() -> {1}->{'subproblem'});
			my $subindex = $start;
			my ($start,$len) = split(',',$self->raw_line_structure() -> {1}->{'ofv'});
			my $ofvindex=$start;
			croak("could not find ofv in raw results header") unless (defined $ofvindex);

			foreach my $row ( @{$modelfit -> raw_results()} ) {
				my $delta_ofv = $row->[$ofvindex] - $base_mod_ofv->[($row->[$probindex]-1)]->[($row->[$subindex]-1)];
				my @oldrow =@{$row};
				$row = [@oldrow[0 .. $ofvindex],$delta_ofv,@oldrow[$ofvindex+1 .. $#oldrow]]; 
			}

			my @old_header = @{$modelfit -> raw_results_header()};
			my $headerindex;
			for (my $k=0; $k<scalar(@old_header);$k++){
				$headerindex = $k if ($old_header[$k] eq 'ofv');
			}
			$modelfit -> raw_results_header(
				[@old_header[0 .. $headerindex],'deltaofv',@old_header[$headerindex+1 .. $#old_header]]);

			foreach my $mod (sort({$a <=> $b} keys %{$self->raw_line_structure()})){
				foreach my $category (keys %{$self->raw_line_structure() -> {$mod}}){
					next if ($category eq 'line_numbers');
					my ($start,$len) = split(',',$self->raw_line_structure() -> {$mod}->{$category});
					$self->raw_line_structure() -> {$mod}->{$category} = ($start+1).','.$len
					if ($start > $ofvindex); #+1 for deltaofv
				}
				$self->raw_line_structure() -> {$mod}->{'deltaofv'} = ($ofvindex+1).',1';
			}
		}
		$self->raw_line_structure() -> {'input'} = $self->raw_line_structure() -> {'1'}; #input model
		$self->raw_line_structure() -> {'base'} = $self->raw_line_structure() -> {'1'}; 
		$self->raw_line_structure() -> write( $dir.'raw_results_structure' );

		$self -> raw_results_header($modelfit -> raw_results_header());
		$self -> raw_results($modelfit -> raw_results());

	};
	return $subroutine;
}

sub _sse_raw_results_callback
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};
	my $subroutine;

	return \&subroutine;
}

sub sse_read_raw_results
{
	my $self = shift;
}

sub prepare_results
{
	my $self = shift;

	1;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
