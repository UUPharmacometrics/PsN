package data;

use include_modules;
use OSspecific;
use File::Copy "cp";
use Config;
use Math::Random;
use Storable;
use ui;
use status_bar;
use Data::Dumper;
use array qw(not_empty);
use Scalar::Util qw(looks_like_number);
use Moose;
use MooseX::Params::Validate;
use data::individual;
use linear_algebra;


has 'individuals' => ( is => 'rw', isa => 'ArrayRef[data::individual]' );
has 'column_head_indices' => ( is => 'rw', isa => 'HashRef[Str]', default => sub { {} } );
has 'found_missing_data' => ( is => 'rw', isa => 'HashRef[Str]', default => sub { {} } );
has 'comment' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'directory' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'filename' => ( is => 'rw', isa => 'Str' );
has 'cont_column' => ( is => 'rw', isa => 'Int' );
has 'header' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has 'idcolumn' => ( is => 'rw', isa => 'Int' );
has 'ignore_missing_files' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'ignoresign' => ( is => 'rw', isa => 'Maybe[Str]');
has 'missing_data_token' => ( is => 'rw', isa => 'Maybe[Num]', default => -99 );
has 'parse_header' => ( is => 'rw', isa => 'Bool', default => 0 );
has '_median' => ( is => 'rw', isa => 'ArrayRef[numbers]', default => sub { [] } );
has '_range' => ( is => 'rw', isa => 'ArrayRef[numbers]', default => sub { [] } );

# {{{ description

    # The structure of the data class is subject-centric, recognising that
    # the subjects included in a study often can be regarded as
    # independent. A class for the subject level exists within PsN and is
    # called the individual class. A data object consists of at least one
    # but probably many individual objects plus optional comments.

# }}} description

# {{{ synopsis

    #   use data;
    #
    #   my $data_obj = data -> new ( filename => 'test040314.dta' );
    #
    #   $data_obj -> renumber_ascending;
    #
    #   my $subsets_ref = $data_obj->case_deletion( bins => 10 );
    #
    #   my @subsets = @{$subsets_ref};

# }}} synopsis

# {{{ see_also

    # =begin html
    #
    # <a HREF="model.html">model</a>, <a HREF="output.html">output</a>,
    # <a HREF="tool/modelfit.html">tool::modelfit</a>,
    # <a HREF="tool.html">tool</a>
    #
    # =end html
    #
    # =begin man
    #
    # model, output, tool::modelfit, tool
    #
    # =end man

# }}} see_also

# FIXME: This is a workaround to not execute triggers at construction.
my $in_constructor = 0;

sub BUILDARGS
{
	my $self = shift;

	$in_constructor = 1;

	return $self->SUPER::BUILDARGS(@_);
}

sub BUILD
{
	my $self = shift;

	$in_constructor = 0;

	# If the column holding the subject identifier is not the
	# first, it can be specified using the I<idcolumn> attribute
	#
	# I<ignoresign> determines which rows that are regarded as
	# comments. Corresponds to the IGNORE= option in the $DATA
	# record in a NONMEM model file.

	(my $directory, my $filename) = OSspecific::absolute_path( $self->directory, $self->filename );
	$self->directory($directory);
	$self->filename($filename);

#	unless (defined $self->ignoresign and length($self->ignoresign)>0){
#		print("\nWarning: ignoresign undefined in data->new\nThis should be fixed\n");
#	}

	unless ( not_empty($self->header) or not_empty($self->individuals) ) { 
		#if empty
		if ( -e $self->full_name ) {
			$self->_read_header;
			$self->_read_individuals;
		} else {
			croak("No file " . $self->full_name . " on disk.")
				unless ($self->ignore_missing_files);
		}
	} else { #have header or individuals as input or stored in memory
		#we do NOT write to disk here
	}

	my $i = 1;
	foreach my $head ( @{$self->header} ) {
	    $self->column_head_indices->{$head} = $i;
	    $i++;
	}
	
}


sub add_randomized_input_data
{
	#static method no shift
	my %parm = validated_hash(\@_,
							  column_headers => { isa => 'ArrayRef[Str]', optional => 1 },
							  filename => { isa => 'Str', optional => 0 },
							  model => { isa => 'model', optional => 0 },
							  missing_data_token => { isa => 'Maybe[Num]', optional => 1 }
	);
	my @column_headers = defined $parm{'column_headers'} ? @{$parm{'column_headers'}} : ();
	my $filename = $parm{'filename'};
	my $model = $parm{'model'};
	my $missing_data_token = $parm{'missing_data_token'};
	my @xcolumn_names;

	#first prob only 
	#in array column_headers
	#in scalar datafilename, modelfilename
	#out array xcolumn_names

	my $dataname = $model->datafiles(problem_numbers => [1]);
	my $data_obj = data->new(filename => $dataname->[0],
							 idcolumn => $model->idcolumn(problem_number => 1),
							 ignoresign => $model->ignoresigns->[0],
							 missing_data_token => $missing_data_token);
	@xcolumn_names = @{$data_obj -> add_randomized_columns(
						   filename => $filename,
						   directory => $model->directory,
						   column_headers => \@column_headers)}; 
	#writes to own filename
	#after changing it to directory/filename
	
	foreach my $xcol (@xcolumn_names){
		$model -> add_option( record_name  => 'input',
							  problem_numbers => [1],
							  option_name  => $xcol);
	}
	$model -> datafiles(problem_numbers =>[1],
						new_names => [$model->directory.$filename]);
	$model->relative_data_path(1);
	$model->_write(); 
	
	return \@xcolumn_names;
}

sub _bootstrap
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 directory => { isa => 'Str', optional => 1 },
		 name_stub => { isa => 'Str', optional => 1 },
		 stratify_on => { isa => 'Maybe[Int]', optional => 1 },
		 resume => { isa => 'Bool', default => 0, optional => 1 },
		 samples => { isa => 'Int', default => 200, optional => 1 },
		 subjects => { isa => 'HashRef[Num]', default => $self->count_ind, optional => 1 },
		 model_ids => { isa => 'ArrayRef[Int]', optional => 1 },
		 MX_PARAMS_VALIDATE_NO_CACHE => 1
	);
	my $directory = $parm{'directory'};
	my $name_stub = $parm{'name_stub'};
	my $stratify_on = $parm{'stratify_on'};
	my $resume = $parm{'resume'};
	my $samples = $parm{'samples'};
	my %subjects = defined $parm{'subjects'} ? %{$parm{'subjects'}} : ();
	my @model_ids = defined $parm{'model_ids'} ? @{$parm{'model_ids'}} : ();
	my @boot_samples;
	my @incl_individuals;
	my @included_keys;

	my ($tmp1, $tmp2) = OSspecific::absolute_path($directory,'hej');
	$directory = $tmp1; #to get with /

	# The bootstrap method draws I<samples> number of boostrap
	# samples from the data set. The I<subjects> arguments
	# determines the size of each sample (default equals to the
	# number of individuals in the original data set). The method
	# returns references to three arrays: I<boot_samples_ref>,
	# which holds the name of bootstrap data files, I<incl_individuals_ref>
	# which holds arrays containing the subject identifiers (ID's)
	# for the included individuals of each bootstrap data set and
	# I<included_keys_ref> which holds the key or index of the
	# included individuals. The key or index is an integer
	# starting at 1 for the first individual in the original data
	# set and increasing by one for each following.

	my @header      = @{$self->header()};
	my $individuals = $self->individuals();
	my $key_ref;

	my $status_bar = status_bar->new( steps => $samples );
	ui->print( category => 'bootstrap',
		     message => $status_bar->print_step,
		     newline => 0);

	for ( my $i = 1; $i <= $samples; $i++ ) {
	  my $new_name = defined $name_stub ? $name_stub."_$i.dta" : "bs$i.dta";
	  $new_name = $directory.$new_name;
	  my ( $incl_ind_ref, $incl_key_ref ) =
		  $self->resample( subjects    => \%subjects,
						   resume      => $resume,
						   new_name    => $new_name,
						   stratify_on => $stratify_on);
	  push( @included_keys, $incl_key_ref );
	  push( @incl_individuals, $incl_ind_ref );
	  push( @boot_samples, $new_name ); 
	  if( $status_bar->tick() ){
		  ui->print( category => 'bootstrap',
					 message => $status_bar->print_step,
					 newline => 0,
					 wrap => 0);
	  }
	}
	ui->print( category => 'bootstrap',
		     message => ' ... done' );

	return \@boot_samples ,\@incl_individuals ,\@included_keys;
}

sub bootstrap_create_datasets_from_keys{
	#static method no shift
	my %parm = validated_hash(\@_,
							  input_filename => { isa => 'Str', optional => 0 },
							  input_directory => { isa => 'Maybe[Str]', optional => 1 },
							  name_stub   => { isa => 'Str', optional => 1 },
							  output_directory => { isa => 'Str', optional => 0 },
							  key_references => { isa => 'ArrayRef', optional => 0 },
							  ignoresign => { isa => 'Str', optional => 1 },
							  missing_data_token => { isa => 'Maybe[Num]', optional => 1 },
							  idcolumn => { isa => 'Int', optional => 0 }
	);
	my $input_filename = $parm{'input_filename'};
	my $input_directory = $parm{'input_directory'};
	my $name_stub = $parm{'name_stub'};
	my $output_directory = $parm{'output_directory'};
	my @key_references = defined $parm{'key_references'} ? @{$parm{'key_references'}} : ();
	my $ignoresign = $parm{'ignoresign'};
	my $missing_data_token = $parm{'missing_data_token'};
	my $idcolumn = $parm{'idcolumn'};

	unless (-d $output_directory){
		croak("output directory $output_directory is not a directory/does not exist");
	}
	my ($tmp1, $tmp2) = OSspecific::absolute_path($output_directory,'hej');
	$output_directory = $tmp1; #to get with /

	#data will be parsed here
	my $data = data->new(filename => $input_filename,
						 directory => $input_directory,
						 ignoresign => $ignoresign,
						 missing_data_token => $missing_data_token,
						 idcolumn => $idcolumn);

	my $new_datas = $data -> _bootstrap_from_keys( directory   => $output_directory,
												  name_stub   => $name_stub,
												  key_references => \@key_references);

	$data = undef;
	return $new_datas;
}

sub _bootstrap_from_keys
{
	#private method, only use from bootstrap_create_datasets_from_keys
	my $self = shift;
	my %parm = validated_hash(\@_,
		 directory => { isa => 'Str', optional => 0 },
		 name_stub => { isa => 'Str', optional => 1 },
		 key_references => { isa => 'ArrayRef', optional => 0 }
	);
	my $directory = $parm{'directory'};
	my $name_stub = $parm{'name_stub'};
	my @key_references = defined $parm{'key_references'} ? @{$parm{'key_references'}} : ();
	my @boot_samples;

	# The bootstrap_from_keys method draws I<samples> number of bootstrap
	# samples from the data set based on input keys reference generated 
	#by bootstrap method on same dataset (assumed).
	# returns references to one array: I<boot_samples>,
	# which holds the names of bootstrap data files

	for ( my $i = 1; $i <= scalar(@key_references); $i++ ) {
	  my $new_name = defined $name_stub ? $name_stub."_$i.dta" : "bs$i.dta";
	  $new_name = $directory.'/'.$new_name;
	  #resample_from_keys writes to disk and returns filename
	  my ( $boot ) =
	      $self->resample_from_keys( new_name    => $new_name,
									 key_arr      => $key_references[$i-1]);
	  push( @boot_samples, $boot );
	}

	return \@boot_samples;
}

sub frem_compute_covariate_properties
{
	#static method, no self
	#one unit test in data_extra.t
	my %parm = validated_hash(\@_,
							  directory => { isa => 'Maybe[Str]', optional => 1 },
							  filename => { isa => 'Str', optional => 0 },
							  idcolumn => { isa => 'Int', optional => 0 },
							  invariant_covariates => { isa => 'Maybe[ArrayRef]', optional => 1},
							  occ_index => { isa => 'Maybe[Int]', optional => 1 },
							  data2name => { isa => 'Str', optional => 0 },
							  evid_index => { isa => 'Maybe[Int]', optional => 1 },
							  mdv_index => { isa => 'Maybe[Int]', optional => 1 },
							  type_index => { isa => 'Int', optional => 0 },
							  cov_indices => { isa => 'Maybe[ArrayRef]', optional => 1 },
							  first_timevar_type => { isa => 'Int', optional => 0 },
							  missing_data_token => { isa => 'Maybe[Num]', optional => 1 }
		);
	#ref of hash of cov names to column numbers
	my $directory = $parm{'directory'};
	my $filename = $parm{'filename'};
	my $idcolumn = $parm{'idcolumn'};
	my @invariant_covariates = (defined $parm{'invariant_covariates'})? @{$parm{'invariant_covariates'}}: ();
	my $occ_index = $parm{'occ_index'};
	my $data2name = $parm{'data2name'};
	my $evid_index = $parm{'evid_index'};
	my $mdv_index = $parm{'mdv_index'};
	my $type_index = $parm{'type_index'};
	my @cov_indices = (defined $parm{'cov_indices'})? @{$parm{'cov_indices'}}: ();
	my $first_timevar_type = $parm{'first_timevar_type'};
	my $missing_data_token = $parm{'missing_data_token'};

	my $results={};

	my $filtered_data = data->new(filename => $directory.$filename,
								  ignoresign => '@', idcolumn => $idcolumn);
	
	foreach my $covariate (@invariant_covariates){
		my %strata = %{$filtered_data->factors(column_head => $covariate,
											   return_occurences => 1,
											   unique_in_individual => 1,
											   ignore_missing => 1)};

		if ( $strata{'Non-unique values found'} eq '1' ) {
			ui -> print( category => 'all',
						 message => "\nWarning: Individuals were found to have multiple values ".
						 "in the $covariate column, which will not be handled correctly by the frem script. ".
						 "Consider terminating this run and setting ".
						 "covariate $covariate as time-varying instead.\n" );
		}
	}

	if (defined $occ_index){
		my $factors = $filtered_data -> factors( column => ($occ_index+1),
												 ignore_missing =>1,
												 unique_in_individual => 0,
												 return_occurences => 1 );

		#key is the factor, e.g. occasion 1. Value is the number of occurences
		my @temp=();
		#sort occasions ascending 
		foreach my $key (sort {$a <=> $b} keys (%{$factors})){
			push(@temp,sprintf("%.12G",$key));
		}
		$results->{'occasionlist'}=\@temp; 
	}

	$filtered_data -> filename($data2name); #change name so that when writing to disk get new file
	my $invariant_matrix; #array of arrays
	my $timevar_matrix; #array of arrays of arrays

	#this writes new data to disk
	($invariant_matrix,$timevar_matrix) = $filtered_data->add_frem_lines( occ_index => $occ_index,
																		  evid_index => $evid_index,
																		  mdv_index => $mdv_index,
																		  type_index => $type_index,
																		  cov_indices => \@cov_indices,
																		  first_timevar_type => $first_timevar_type);

	$results->{'invariant_median'}= [];
	$results->{'invariant_covmatrix'}= [];
	$results->{'timevar_median'} = [];
	$results->{'timevar_covmatrix'} = [];

	my $err = linear_algebra::row_cov_median($invariant_matrix,
											 $results->{'invariant_covmatrix'},
											 $results->{'invariant_median'},
											 $missing_data_token);
	if ($err != 0){
		print "failed to compute invariant covariates covariance\n";
		$results->{'invariant_median'}= [];
		$results->{'invariant_covmatrix'}= [];
	}

	$err = linear_algebra::row_cov_median($timevar_matrix,
										  $results->{'timevar_covmatrix'},
										  $results->{'timevar_median'},
										  $missing_data_token);
	if ($err != 0){
		print "failed to compute time-varying covariates covariance\n";
		$results->{'timevar_median'} = [];
		$results->{'timevar_covmatrix'} = [];
	}
	$filtered_data = undef;
	return $results;

}



sub add_frem_lines
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		type_index => { isa => 'Int', optional => 0 },
		occ_index => { isa => 'Maybe[Int]', optional => 1 },
		evid_index => { isa => 'Maybe[Int]', optional => 1 },
		mdv_index => { isa => 'Maybe[Int]', optional => 1 },
		cov_indices => { isa =>'Ref', optional => 1 },
		first_timevar_type => { isa => 'Int', optional => 0 }
	);
	my $type_index = $parm{'type_index'};
	my $occ_index = $parm{'occ_index'};
	my $evid_index = $parm{'evid_index'};
	my $mdv_index = $parm{'mdv_index'};
	my $cov_indices = $parm{'cov_indices'};
	my $first_timevar_type = $parm{'first_timevar_type'};
	my @invariant_matrix;
	my @timevar_matrix;

	my $first_id = $self->individuals()->[0];

	croak("No individuals defined in data object based on ".
		$self->full_name ) unless ( defined $first_id );

	foreach my $individual ( @{$self->individuals()} ) {
		my ($invariants,$timevar) =  $individual->add_frem_lines( occ_index => $occ_index,
																  evid_index => $evid_index,
																  missing_data_token => $self->missing_data_token(),
																  mdv_index => $mdv_index,
																  type_index => $type_index,
																  cov_indices => $cov_indices,
																  first_timevar_type => $first_timevar_type);
		push(@invariant_matrix,$invariants);
		push(@timevar_matrix,$timevar);
	}
	$self->_write;

	return \@invariant_matrix ,\@timevar_matrix;
}

sub bootstrap_create_datasets{
	#static method no shift
	my %parm = validated_hash(\@_,
							  input_filename => { isa => 'Str', optional => 0 },
							  input_directory => { isa => 'Maybe[Str]', optional => 1 },
							  subjects => { isa => 'Maybe[HashRef]', optional => 1 },
							  name_stub   => { isa => 'Str', optional => 1 },
							  samples     => { isa => 'Int', optional => 0 },
							  stratify_on => { isa => 'Maybe[Int]', optional => 1 },
							  output_directory => { isa => 'Str', optional => 0 },
							  ignoresign => { isa => 'Str', optional => 1 },
							  missing_data_token => { isa => 'Maybe[Num]', optional => 1 },
							  idcolumn => { isa => 'Int', optional => 0 }
	);
	my $input_filename = $parm{'input_filename'};
	my $input_directory = $parm{'input_directory'};
	my %subjects = (defined $parm{'subjects'})? %{$parm{'subjects'}}: ();
	my $name_stub = $parm{'name_stub'};
	my $samples = $parm{'samples'};
	my $stratify_on = $parm{'stratify_on'};
	my $output_directory = $parm{'output_directory'};
	my $ignoresign = $parm{'ignoresign'};
	my $missing_data_token = $parm{'missing_data_token'};
	my $idcolumn = $parm{'idcolumn'};

	unless (-d $output_directory){
		croak("output directory $output_directory is not a directory/does not exist");
	}
	my ($tmp1, $tmp2) = OSspecific::absolute_path($output_directory,'hej');
	$output_directory = $tmp1; #to get with /

	#data will be parsed here
	my $data = data->new(filename => $input_filename,
						 directory => $input_directory,
						 ignoresign => $ignoresign,
						 missing_data_token => $missing_data_token,
						 idcolumn => $idcolumn);

	my $count = $data->count_ind;
	unless (scalar(keys %subjects)>0){
		$subjects{'default'} = $count;
	}

	my ($new_datas, $incl_ids, $incl_keys) = $data->_bootstrap( directory   => $output_directory,
															   name_stub   => $name_stub,
															   samples     => $samples,
															   subjects    => \%subjects,
															   stratify_on => $stratify_on);
	$data = undef;
	return ($new_datas, $incl_ids, $incl_keys,\%subjects, $count);
}

sub cdd_create_datasets{
	#static method no shift
	my %parm = validated_hash(\@_,
		input_filename => { isa => 'Str', optional => 0 },
		input_directory => { isa => 'Maybe[Str]', optional => 1 },
		bins => { isa => 'Maybe[Int]', optional => 1 },
		case_column => { isa => 'Int', optional => 0 },
		selection_method => { isa => 'Maybe[Str]', default => 'consecutive', optional => 1 },
		output_directory => { isa => 'Str', optional => 0 },
		ignoresign => { isa => 'Str', optional => 1 },
		missing_data_token => { isa => 'Maybe[Num]', optional => 1 },
		idcolumn => { isa => 'Int', optional => 0 }
	);
	my $input_filename = $parm{'input_filename'};
	my $input_directory = $parm{'input_directory'};
	my $bins = $parm{'bins'};
	my $case_column = $parm{'case_column'}; #starting at 1
	my $selection_method = $parm{'selection_method'};
	my $output_directory = $parm{'output_directory'};
	my $ignoresign = $parm{'ignoresign'};
	my $missing_data_token = $parm{'missing_data_token'};
	my $idcolumn = $parm{'idcolumn'};

	unless (-d $output_directory){
		croak("output directory $output_directory is not a directory/does not exist");
	}
	my ($tmp1, $tmp2) = OSspecific::absolute_path($output_directory,'hej');
	$output_directory = $tmp1; #to get with /

	#data will be parsed here
	my $data = data->new(filename => $input_filename,
						 directory => $input_directory,
						 ignoresign => $ignoresign,
						 missing_data_token => $missing_data_token,
						 idcolumn => $idcolumn);

	
	my ($new_datas, $skip_ids, $skip_keys, $skip_values, $remainders, $pr_bins ) =
		$data -> _case_deletion( case_column => $case_column,
								selection   => $selection_method,
								bins        => $bins,
								directory   => $output_directory );
	$data =undef;
	return $new_datas, $skip_ids, $skip_keys, $skip_values, $remainders, $pr_bins;

}

sub _case_deletion
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		bins => { isa => 'Maybe[Num]', optional => 1 },
		case_column => { isa => 'Int', optional => 0 },
		selection => { isa => 'Maybe[Str]', default => 'consecutive', optional => 1 },
		directory => { isa => 'Str', optional => 0 }
	);
	my $bins = $parm{'bins'};
	my $case_column = $parm{'case_column'};
	my $selection = $parm{'selection'};
	my $directory = $parm{'directory'};

	my @subsets;
	my @skipped_ids;
	my @skipped_keys;
	my @skipped_values;
	my @remainders;

	# case_deletion creates subsets of the data. The number of
	# subsets is specified by the bins argument. The individuals
	# of each subset is selected randomly or in ascending
	# numerical order depending on the selection argument that can
	# be either 'consecutive' or 'random'. case_column must be
	# specified to give the method something to base the selection
	# on. Valid case_column values are either the column number
	# (pure digits) or the name of the column in the (optional)
	# header row.

	my @header    = @{$self->header()};

	my %factors = %{$self -> factors( column => $case_column )};
	if ( $factors{'Non-unique values found'} eq '1' ) {
		croak("Individuals were found to have multiple values in column number $case_column. ".
			  "Column $case_column cannot be used for case deletion." );
	}
	my $maxbins      = scalar (keys %factors);
	my $pr_bins      = ( defined $bins and $bins <= $maxbins ) ? $bins : $maxbins;
	$bins = $pr_bins;
	my @ftrs      = sort { $a <=> $b } keys %factors;

	my $individuals = $self->individuals();
	my $maxkey    = scalar @{$individuals} - 1;

	my ( @tmp_ftrs, @binsize ) = ((), ());
	my ( $k, $j, $i ) = ( 0, 0, 0 );
	# Create the binsizes
	for ( $j = 0; $j < $maxbins; $j++ ) {
		$binsize[ $k++ ]++;
		$k = 0 if( $k >= $bins );
	}
	$self->_fisher_yates_shuffle( array => \@ftrs ) if( $selection eq 'random' );
	for ( $k = 0; $k < $bins; $k++ ) {
		for ( $j = 0; $j < $binsize[ $k ]; $j++ ) {
			push( @{$skipped_keys[ $k ]}, @{$factors{ $ftrs[ $i ] }} );
			push( @{$skipped_values[ $k ]}, $ftrs[ $i++ ] );
		}
	}

	for ( $k = 0; $k < $bins; $k++ ) {
		my @cd_inds = ();
		my @del_inds = ();
		SELKEYS: foreach my $key ( 0..$maxkey ) {
			foreach my $skipped ( @{$skipped_keys[ $k ]} ) {
				if ( $key == $skipped ) {
					push( @{$skipped_ids[ $k ]}, $individuals -> [ $skipped ]->idnumber );
					push( @del_inds, $individuals->[ $key ]->copy );
					next SELKEYS;
				}
			}
			push( @cd_inds, $individuals->[ $key ]->copy );
		}
		#here we simply write to file and then delete objects again
		#we only return file names, not data objects
		my $newname = $directory . 'cdd_' . ($k + 1) . '.dta';
		my $newdata = data -> new ( header      => \@header,
									ignoresign  => $self->ignoresign,
									missing_data_token => $self->missing_data_token,
									idcolumn    => $self->idcolumn,
									individuals => \@cd_inds,
									filename    => $newname,
									ignore_missing_files => 1 );
		$newdata->_write;
		
		my $delname = $directory . 'rem_' . ($k + 1) . '.dta';
		my $deldata = data -> new ( header      => \@header,
									ignoresign  => $self->ignoresign,
									missing_data_token => $self->missing_data_token,
									idcolumn    => $self->idcolumn,
									individuals => \@del_inds,
									filename    => $delname,
									ignore_missing_files => 1 );
		$deldata->_write;
		push( @subsets,  $newname);
		push( @remainders, $delname );
	}

	return \@subsets ,\@skipped_ids ,\@skipped_keys ,\@skipped_values ,\@remainders, $bins;
}

sub copy
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 directory => { isa => 'Str', optional => 1 },
		 filename => { isa => 'Str', default => 'copy.dta', optional => 1 },
		 ignore_missing_files => { isa => 'Bool', default => 0, optional => 1 },
		 write_copy => { isa => 'Bool', default => 1, optional => 1 }
	);
	my $new_data;
	my $directory = $parm{'directory'};
	my $filename = $parm{'filename'};
	my $ignore_missing_files = $parm{'ignore_missing_files'};
	my $write_copy = $parm{'write_copy'};

	# filename: new data file name.

	($directory, $filename) = OSspecific::absolute_path($directory, $filename);

	$new_data = Storable::dclone( $self );

	#TODO check that regular attributes, like idcolumn and ignoresign, are copied
	#and that individuals etc also are copied

	# Set the new file name for the copy
	$new_data->directory($directory);
	$new_data->filename($filename);

	if ($write_copy){
		if (-e $new_data->full_name){
			croak("attempting to overwrite data file ".$new_data->full_name." when copying ".$self->full_name);
		}
		$new_data->_write;
	}
	return $new_data;
}

sub count_ind
{
	my $self = shift;
	my $num = 0;

	# Returns the number of individuals in the data set.
	if (defined $self->individuals) {
	  $num = scalar @{$self->individuals};
	} else {
	  croak("No individuals found in file " . $self->filename);
	}

	return $num;
}

sub add_randomized_columns
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		column_headers => { isa => 'ArrayRef[Str]', optional => 1 },
		filename => { isa => 'Str', optional => 0 },
		directory => { isa => 'Str', optional => 0 }
	);
	my @column_headers = defined $parm{'column_headers'} ? @{$parm{'column_headers'}} : ();
	my $filename = $parm{'filename'};
	my $directory = $parm{'directory'};
	my @xcolumn_names;

	#in array of column headers
	#in desired filename
	#out xcolumn_names
	$self->filename($filename);
	$self->directory($directory);

	$self->individuals([]) unless defined $self->individuals;	# FIXME: Need default value!
	my $n_individuals = scalar(@{$self->individuals()});
	my @xcovvalues;

	foreach my $cov (@column_headers){
		push(@xcolumn_names,'X'.$cov);
		my %strata = %{$self->factors( column_head => $cov, return_occurences => 1 )};
		my @values;
		if ( $strata{'Non-unique values found'} eq '1' ) {
			ui->print( category => 'all',
				message => "Warning: Individuals were found to have multiple values ".
				"in the $cov column. When randomizing this column to create X$cov ".
				"the arithmetic mean for each individual is used, ".
				"which may lead to an incorrect assessment of type I error." );

			my $first_id = $self->individuals()->[0];
			die "data->add_randomized columns: No individuals defined in data object based on ",
			$self->full_name,"\n" unless defined $first_id;
			my @data_row = split( /,/ , $first_id->subject_data ->[0] );
			my $column;
			unless (defined($self->column_head_indices->{$cov})) {
				die "Error in data->mean: unknown column: \"$cov\" \n";
			} else {
				$column = $self->column_head_indices->{$cov};
			}
			foreach my $individual ( @{$self ->individuals()} ) {
				my $ifactors = $individual->subject_data;
				my $individual_sum = 0;
				my $data_rows = 0;
				for(my $i = 0; $i <= $#{$ifactors}; $i++ ) {
					# data is stored in strings. We need to split them into an array.
					my @data_row = split( /,/, $ifactors->[$i] );
					if ( $data_row[$column - 1] == $self->missing_data_token ) {
						next;
					}
					$individual_sum += $data_row[$column - 1];
					$data_rows++;
				}
				if( $data_rows != 0 ) {
					push(@values, $individual_sum / $data_rows);
				} else {
					push(@values, undef);
				}
			}
		} else {
			foreach my $key (keys %strata) {
				push(@values, (($key) x $strata{$key}));
			}
		}
		$self->_fisher_yates_shuffle( array => \@values );
		croak("number of values for $cov is " . scalar(@values) . " but individuals is $n_individuals" )
		unless (scalar(@values) == $n_individuals);

		push(@xcovvalues,\@values);
	}

	my @new_header = @{$self->header()};
	push(@new_header,@xcolumn_names);
	$self->header(\@new_header);

	my $counter = 0;
	$self->individuals([]) unless defined $self->individuals; # FIXME
	foreach my $individual (@{$self->individuals()}) {
		my @rowvalues;
		foreach my $arr (@xcovvalues) {
			push(@rowvalues,$arr->[$counter]);
		}
		my $newstring = ',' . join(',',@rowvalues);
		#create arr of extra values, append to each row of subject data
		$individual->subject_data([]) unless defined $individual->subject_data; # FIXME
		for( my $i = 0 ; $i < scalar(@{$individual->subject_data}); $i++ ) {
			$individual->subject_data->[$i] .= $newstring;
		}
		$counter++;
	}
	$self->_write();

	return \@xcolumn_names;
}


sub format_data
{
	my $self = shift;
	my @form_data;

	my $header = $self->header();
	my $have_header=0;
	if (defined $header) {
		$have_header = 1 if (scalar(@{$header}) > 0);
	}

	# format the data for NONMEM (simple comma-separated layout)
	if (defined $self->comment()) {
		my @comment   = @{$self->comment()};
		for (@comment) {
			1;
		}
	}

	if ( $have_header) {
		push( @form_data, join(',',@{$self->header()})."\n" );
	}
	foreach my $individual ( @{$self->individuals()} ) {
		foreach my $row ( @{$individual->subject_data} ) {
			push(@form_data, $row ."\n");
		}
	}

	return \@form_data;
}

sub factors
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 column => { isa => 'Int', optional => 1 },
		 column_head => { isa => 'Str', optional => 1 },
		 unique_in_individual => { isa => 'Bool', default => 1, optional => 1 },
		 ignore_missing => { isa => 'Bool', default => 0, optional => 1 },
		 return_occurences => { isa => 'Bool', default => 0, optional => 1 }
	);
	my $column = $parm{'column'};
	my $column_head = $parm{'column_head'};
	my $unique_in_individual = $parm{'unique_in_individual'};
	my $ignore_missing = $parm{'ignore_missing'};
	my $return_occurences = $parm{'return_occurences'};
	my %factors;

	# Either column (number, starting at 1) or column_head must be specified.
	#
	# The default behaviour is to return a hash with the factors as keys
	# and as values references to arrays with the order numbers (not the ID numbers)
	# of the individuals that contain this factor
	#
	# If unique_in_individual is true (1), the returned hash will contain
	# an element with key 'Non-unique values found' and value 1 if any
	# individual contain more than one value in the specified column.
	#
	# Return occurences will calculate the occurence of each
	# factor value. Several occurences in one individual counts as
	# one occurence. The elements of the returned hash will have the factors
	# as keys and the number of occurences as values.
	#

	# Check if $column(-index) is defined and valid, else try to find index
	# using column_head
	croak("No individuals stored from ".$self->full_name ) unless ( defined $self->individuals() );
	my $first_id = $self->individuals()->[0];

	croak("No individuals defined in data object based on ".
		      $self->full_name ) unless ( defined $first_id );

	my @data_row = split( /,/, $first_id->subject_data->[0] );
	unless ( defined $column && defined( $data_row[$column-1] ) ) {
	  unless (defined($column_head) && defined($self->column_head_indices->{$column_head})) {
	    croak("Error in data->factors: unknown column: \"$column_head\" ".
			  "or invalid column number: \"$column\".\n".
			  "Valid column numbers are 1 to ".scalar @data_row ."\n".
			  "Valid column headers are (in no particular order):\n".
			  join(', ', keys(%{$self->column_head_indices})) );
	  } else {
	    $column = $self->column_head_indices->{$column_head};
	    carp("$column_head is in column number $column" );
	  }
	}

	my $key = 0;
	foreach my $individual ( @{$self->individuals()} ) {
		#get a hash: key:data value value: array of order numbers in individual
		my @ifactors = keys %{$individual->factors( column => $column )};
		if ( scalar @ifactors > 1 and $unique_in_individual ) {
			#do not set non-unique if only two and one of them is missing data
			unless (scalar @ifactors == 2 and $ignore_missing and 
					($ifactors[0] == $self->missing_data_token ||
					 $ifactors[1] == $self->missing_data_token)){ 
				%factors = ( 'Non-unique values found' => 1 );
			}
		}
		croak("No value found in column $column in individual ".
			  $individual->idnumber ) if ( scalar @ifactors == 0 );

	  # Return occurences will calculate the occurence of each
	  # factor value. Several occurences in one individual counts as
	  # one occurence.

		if ( $return_occurences ) {
			#how many individuals have this factor
			#one count per individual, not counting how many per individual
			foreach my $ifactor ( @ifactors ) {
				$factors{$ifactor}++;
			}
		} else {
			foreach my $ifactor ( @ifactors ) {
				#push order number of individual with this factor. Once per individual
				#even if many per individual
				push( @{$factors{$ifactor}}, $key );
			}
		}
	  $key++;
	}

	return \%factors;
}


sub fractions
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 column => { isa => 'Int', optional => 1 },
		 ignore_missing => { isa => 'Bool', default => 0, optional => 1 },
		 column_head => { isa => 'Str', optional => 1 },
		 unique_in_individual => { isa => 'Bool', default => 0, optional => 1 }
	);
	my $column = $parm{'column'};
	my $ignore_missing = $parm{'ignore_missing'};
	my $column_head = $parm{'column_head'};
	my $unique_in_individual = $parm{'unique_in_individual'};
	my %fractions;

	my %factors = $self->factors( 'return_occurences' => 1, 
			'unique_in_individual' => $unique_in_individual,
			'column_head' => $column_head,
			'column' => $column);
	
	my $sum = 0;
	while (my ($factor, $amount) = each %factors) {
		if ( $factor == $self->missing_data_token && $ignore_missing ) {
			next;
		} else {
			$sum += $amount;
		}
	}
	while (my ($factor, $amount) = each %factors) {
		if ( $factor == $self->missing_data_token && $ignore_missing ) {
			next;
		} else {
			$fractions{$factor} = $amount / $sum;
		}
	}

	return \%fractions;
}

sub have_missing_data
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 column => { isa => 'Int', optional => 1 },
		 column_head => { isa => 'Str', optional => 1 }
	);
	my $column = $parm{'column'};
	my $column_head = $parm{'column_head'};
	my $return_value;

	# Either I<column> or I<column_head> must be specified.
	#
	# This method looks through the data column with index I<column> or
	# (optional) header name I<column_head> and returns O if no missing
	# data indicator was found or 1 otherwise.

	my $first_id = $self->individuals->[0];
	croak("No individuals defined in data object based on " . $self->full_name ) unless ( defined $first_id );
	my @data_row = split( /,/ , $first_id->subject_data->[0] );
	unless ( defined $column  && defined( $data_row[$column-1] ) ) {
	  unless(defined($column_head) && defined($self->column_head_indices->{$column_head})){
	    die "Error in data->have_missing_data: unknown column: \"$column_head\" or invalid column number: \"$column\"\n";
	  } else {
	    $column = $self->column_head_indices->{$column_head};
	  }
	}

	$return_value = (defined $self->found_missing_data and (defined $self->found_missing_data->{$column}))? $self->found_missing_data->{$column} : 0;

	return $return_value;
}

sub max
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 column => { isa => 'Maybe[Int]', optional => 1 },
		 column_head => { isa => 'Str', optional => 1 }
	);
	my $column = $parm{'column'};
	my $column_head = $parm{'column_head'};
	my $return_value;

	# Either column or column_head must be specified. Column_head must be a string that
	# identifies a column in the (optional ) data file header.

# The if-statement below used to be a cache of allready calculated
# means. But since individuals can be accessed in so many ways, we
# don't know when this cache should be updated. Its easier to
# recalculate the max. Maybe we can include this optimization in the
# future, if it turns out to be a bottleneck

	  my $first_id = $self->individuals()->[0];
	  croak("data->max: No individuals defined in data object based on " . 
			$self->full_name ) unless defined $first_id;

	  my @data_row = split( /,/ , $first_id->subject_data ->[0] );

	  unless ( defined $column  && defined( $data_row[$column-1] ) ) {
	    unless (defined($column_head) && defined($self->column_head_indices->{$column_head})) {
	      die "Error in data->max: unknown column: \"$column_head\" or invalid column number: \"$column\"\n";
	    } else {
	      $column = $self->column_head_indices->{$column_head};
	    }
	  }
	  foreach my $individual ( @{$self->individuals()} ) {
	    my $ifactors = $individual->factors( 'column' => $column );
	    foreach ( keys %{$ifactors} ) {
	      next if ( $_ == $self->missing_data_token );
	      if ( defined ($return_value) ) {
					$return_value = $_ > $return_value ? $_ : $return_value;
	      } else {
					$return_value = $_;
	      }
	    }
	  }

	return $return_value;
}

sub median
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 column => { isa => 'Maybe[Int]', optional => 1 },
		 column_head => { isa => 'Str', optional => 1 },
		 unique_in_individual => { isa => 'Bool', default => 0, optional => 1 }
	);
	my $column = $parm{'column'};
	my $column_head = $parm{'column_head'};
	my $unique_in_individual = $parm{'unique_in_individual'};
	my $return_value;

	my $first_id = $self->individuals()->[0];
	die "data->median: No individuals defined in data object based on ",
	  $self->full_name,"\n" unless defined $first_id;

	my @data_row = split( /,/ , $first_id->subject_data ->[0] );

	unless ( defined $column  && defined( $data_row[$column-1] ) ) {
	  unless(defined($column_head) && defined($self->column_head_indices->{$column_head})){
	    die "Error in data->median: unknown column: \"$column_head\" or invalid column number: \"$column\"\n";
	  } else {
	    $column = $self->column_head_indices->{$column_head};
	  }
	}

	my @median_array;

	foreach my $individual ( @{$self->individuals()} ) {
	  if( $unique_in_individual ) {
	    my $ifactors = $individual->factors( 'column' => $column );
	    
	    foreach ( keys %{$ifactors} ) {
	      next if ( $_ == $self->missing_data_token );
	      push( @median_array, $_ );
	    }
	  } else {
	    my $ifactors = $individual->subject_data;
	    
	    for (my $i = 0; $i <= $#{$ifactors}; $i++ ) {
	      my @data_row = split( /,/ , $ifactors->[$i] );
	      next if ( $data_row[$column-1] == $self->missing_data_token );
	      push(@median_array, $data_row[$column-1]);
	    }
	  }
	}
	@median_array = sort {$a <=> $b} @median_array ;
	if( @median_array % 2 ){
	  $return_value = $median_array[$#median_array / 2];
	} else {
	  $return_value = ( $median_array[@median_array / 2] + 
			    $median_array[(@median_array - 2) / 2] ) / 2;
	}    
	
	$self->_median->[$column] = $return_value;

	return $return_value;
}

sub mean
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		column => { isa => 'Maybe[Int]', optional => 1 },
		column_head => { isa => 'Str', optional => 1 },
		hi_cutoff => { isa => 'Num', optional => 1 },
		low_cutoff => { isa => 'Num', optional => 1 },
		subset_column => { isa => 'Maybe[Int]', optional => 1 },
		subset_syntax => { isa => 'Str', default => ' != undef', optional => 1 },
		global_mean => { isa => 'Bool', default => 0, optional => 1 }
	);
	my $column = $parm{'column'};
	my $column_head = $parm{'column_head'};
	my $hi_cutoff = $parm{'hi_cutoff'};
	my $low_cutoff = $parm{'low_cutoff'};
	my $subset_column = $parm{'subset_column'};
	my $subset_syntax = $parm{'subset_syntax'};
	my $global_mean = $parm{'global_mean'};
	my $return_value;

	# Returns mean value of a column
	# If a individual has more than one observation, a mean
	# value for each individual is calculated first, then the mean
	# value over the individuals. If hi_cutoff is defined the mean function
	# will cut all value below the cutoff, and set their value to
	# 0. It's used to calculate the HI-mean/LOW-mean of a column for
	# e.g. Hockey-stick covariates. If both hi_cutoff and low_cutoff
	# are defined only the hi_cutoff will be used.  See L</max>.

	my $first_id = $self->individuals->[0];
	die "data->median: No individuals defined in data object based on ",
	$self->full_name,"\n" unless defined $first_id;

	my @data_row = split(/,/, $first_id->subject_data ->[0]);

	unless ( defined $column  && defined( $data_row[$column-1] ) ) {
		unless (defined($column_head) && defined($self->column_head_indices->{$column_head})) {
			die "Error in data->mean: unknown column: \"$column_head\" or invalid column number: \"$column\"\n";
		} else {
			$column = $self->column_head_indices->{$column_head};
		}
	}

	## Here the calculation starts
	my $num_individuals = 0;
	my $sum = 0;

	my $all_data_rows = 0;
	foreach my $individual (@{$self->individuals}) {

		my $ifactors = $individual->subject_data;
		my $individual_sum = 0;
		my $data_rows = 0;
		for (my $i = 0; $i <= $#{$ifactors}; $i++) {

			# data is stored in strings. We need to split them into an
			# array.

			my @data_row = split( /,/, $ifactors->[$i] );
			if ( $data_row[$column - 1] == $self->missing_data_token ) {
				next;
			}

			if( defined $subset_column and not eval ( $data_row[$subset_column - 1].$subset_syntax ) ) {
				next;
			}
			if (defined $hi_cutoff) {
				if ($data_row[$column - 1] > $hi_cutoff) {
					$individual_sum += $data_row[$column - 1] - $hi_cutoff;
				}
			} elsif (defined $low_cutoff) {
				if ($data_row[$column - 1]<$low_cutoff) {
					$individual_sum += $low_cutoff - $data_row[$column - 1];
				}
			} else {
				$individual_sum += $data_row[$column-1];
			}
			$data_rows++;
		}
		if ($global_mean) {
			$sum += $individual_sum;
			$num_individuals += $data_rows;
		} else {
			if ($data_rows != 0) {
				$sum += $individual_sum / $data_rows;
				$num_individuals++
			}
			#in denominator. Put individual count inside if statement to check there were any observations
		}
		$all_data_rows += $data_rows;
	}
	if ($num_individuals != 0) {
		$return_value = $sum / $num_individuals;
	}

	return $return_value;
}

sub sd
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		column => { isa => 'Maybe[Int]', optional => 1 },
		column_head => { isa => 'Str', optional => 1 },
		hi_cutoff => { isa => 'Num', optional => 1 },
		low_cutoff => { isa => 'Num', optional => 1 },
		subset_column => { isa => 'Int', optional => 1 },
		subset_syntax => { isa => 'Str', default => ' != undef', optional => 1 },
		global_sd => { isa => 'Bool', default => 0, optional => 1 }
	);
	my $column = $parm{'column'};
	my $column_head = $parm{'column_head'};
	my $hi_cutoff = $parm{'hi_cutoff'};
	my $low_cutoff = $parm{'low_cutoff'};
	my $subset_column = $parm{'subset_column'};
	my $subset_syntax = $parm{'subset_syntax'};
	my $global_sd = $parm{'global_sd'};
	my $return_value;

	# This sub returns standard deviation for a specific column
	# If there are more than one sample/individual the value used for that specific
	# individual is the mean value of its samples.
	# The cut-offs are for hockey stick variables. I.e. If one individual value is
	# lower than the hi-cutoff the individual value will be zero.
	# HI_cutoff is used to calculate the HI-mean of a column.
	# If cut_off is undef it won't be used
	# See L</max>.
	# If skip_zeros is 1 (default is 0) values that are exactly 0 are skipped,
	# needed when computing shrinkage
	my $first_id = $self->individuals()->[0];
	croak("No individuals defined in data object based on ".
		$self->full_name ) unless defined $first_id;

	my @data_row = split( /,/ , $first_id->subject_data ->[0] );

	unless ( defined $column  && defined( $data_row[$column-1] ) ) {
		unless (defined($column_head) && defined($self->column_head_indices->{$column_head})) {
			croak("Unknown column: \"$column_head\" or "
				."invalid column number: \"$column\"" );
		} else {
			$column = $self->column_head_indices->{$column_head};
		}
	}

	## Here the calculation starts
	my $num_individuals = 0;
	my $sum = 0;
	my $mean;
	if (defined $hi_cutoff) {
		$mean = $self->mean(column   => $column,
			hi_cutoff => $hi_cutoff,
			global_mean => $global_sd );
	} elsif (defined $low_cutoff) {	  
		$mean = $self->mean(column   => $column,
			low_cutoff => $low_cutoff,
			global_mean => $global_sd );
	} else {
		$mean = $self->mean( column        => $column,
			subset_column => $subset_column,
			subset_syntax => $subset_syntax,
			global_mean => $global_sd );
	}

	foreach my $individual ( @{$self->individuals()} ) {
		my $ifactors = $individual->subject_data;
		my $individual_sum = 0;
		my $data_rows = 0;
		for (my $i = 0; $i <= $#{$ifactors}; $i++ ) {

			# data is stored in strings. We need to split them into an
			# array.

			my @data_row = split( /,/, $ifactors->[$i] );

			if ( $data_row[$column - 1] == $self->missing_data_token ) {
				next;
			}

			if( defined $subset_column and not eval ( $data_row[$subset_column - 1] . $subset_syntax ) ) {
				next;
			}

			if (defined $hi_cutoff) {
				if ($data_row[$column - 1]>$hi_cutoff) {
					if( $global_sd ) {
						$individual_sum += ($data_row[$column - 1] - $hi_cutoff - $mean) ** 2;
					} else {
						$individual_sum += $data_row[$column - 1] - $hi_cutoff;
					}
				}
			} else {
				if (defined $low_cutoff) {
					if ($data_row[$column - 1] < $low_cutoff) {
						if( $global_sd ) {
							$individual_sum += ($low_cutoff - $data_row[$column-1] - $mean) ** 2;
						} else {
							$individual_sum += $low_cutoff - $data_row[$column-1];
						}
					}
				} else {
					if( $global_sd ) {
						$individual_sum += ($data_row[$column-1] - $mean) ** 2;
					} else {
						$individual_sum += $data_row[$column-1];
					}
				}
			}
			$data_rows++;
		}
		if( $global_sd ) {
			$sum += $individual_sum;
			$num_individuals += $data_rows;
		} else {
			if( $data_rows != 0 ) {
				$sum += ($individual_sum/$data_rows - $mean) ** 2;
			}
			$num_individuals++;
		}
	}
	if( $num_individuals < 2 ) {
		$return_value = 0;
	} else {
		if( $num_individuals != 0 ) {
			$return_value = (1/($num_individuals-1)*$sum) ** 0.5;
		}
	}

	return $return_value;
}

sub min
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		column => { isa => 'Maybe[Int]', optional => 1 },
		column_head => { isa => 'Str', optional => 1 }
	);
	my $column = $parm{'column'};
	my $column_head = $parm{'column_head'};
	my $return_value;

	my $tmp_column = $self->column_head_indices->{$column_head};

	# The if-statement below used to be a cache of allready calculated
	# means. But since individuals can be accessed in so many ways, we
	# don't know when this cache should be updated. Its easier to
	# recalculate the min. Maybe we can include this optimization in the
	# future, if it turns out to be a bottleneck
	my $first_id = $self->individuals()->[0];
	die "data->min: No individuals defined in data object based on ", $self->full_name, "\n"
		unless defined $first_id;

	my @data_row = split(/,/, $first_id->subject_data->[0]);

	unless (defined $column && defined($data_row[$column-1])) {
		unless (defined($column_head) && defined($self->column_head_indices->{$column_head})) {
			die "Error in data->min: unknown column: \"$column_head\" or invalid column number: \"$column\"\n";
		} else {
			$column = $self->column_head_indices->{$column_head};
		}
	}
	foreach my $individual (@{$self->individuals}) {
		my $ifactors = $individual->factors('column' => $column);
		foreach (keys %{$ifactors}) {
			next if ($_ == $self->missing_data_token);
			if (defined ($return_value)) {
				$return_value = $_ < $return_value ? $_ : $return_value;
			} else {
				$return_value = $_;
			}
		}
	}

	return $return_value;
}

sub merge
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		mergeobj => { isa => 'data', optional => 1 }
	);
	my $mergeobj = $parm{'mergeobj'};

	$self->individuals([]) if (not defined $self->individuals);

	if (defined $mergeobj->individuals) {
		push(@{$self->individuals}, @{$mergeobj->individuals});
	}
}

sub range
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		column => { isa => 'Int', optional => 1 },
		column_head => { isa => 'Str', optional => 1 }
	);
	my $column = $parm{'column'};
	my $column_head = $parm{'column_head'};
	my $return_value;

	my $tmp_column;
	if (defined $self->column_head_indices and defined $self->column_head_indices->{$column_head}) {
		$tmp_column = $self->column_head_indices->{$column_head};
	}
	if (defined $tmp_column and defined $self->_range->[$tmp_column]) {
		$return_value = $self->_range->[$tmp_column];
	} else {
		if (defined $column) {
			$return_value = $self->max(column => $column) - $self->min(column => $column);
		} else {
			$return_value = $self->max(column_head => $column_head) - $self->min(column_head => $column_head);
		}
		$self->_range->[$column] = $return_value;

	}

	return $return_value;
}

sub _renumber_ascending
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		start_at => { isa => 'Int', default => 1, optional => 1 }
	);
	my $start_at = $parm{'start_at'};

	#private method, do not use outside class since changes object in memory

	# Renumbers the individuals (changes the subject identifiers) so that
	# all have unique integer numbers starting with start_at and
	# ascending. The primary use of this
	# method is not to order the individuals after their identifiers but to
	# ensure that all individuals have unique identifiers.

	foreach my $individual ( @{$self->individuals()} ) {
		$individual->idnumber ( $start_at++ );
	}

}

sub resample
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  new_name => { isa => 'Str', optional => 0 },
							  stratify_on => { isa => 'Maybe[Int]', optional => 1 },
							  resume => { isa => 'Bool', default => 0, optional => 1 },
							  subjects => { isa => 'HashRef[Int]', default => $self->count_ind, optional => 1 },
							  model_id => { isa => 'Int', optional => 1 },
							  MX_PARAMS_VALIDATE_NO_CACHE => 1
		);
	my $new_name = $parm{'new_name'};
	my $stratify_on = $parm{'stratify_on'};
	my $resume = $parm{'resume'};
	my %subjects = defined $parm{'subjects'} ? %{$parm{'subjects'}} : ();
	my $boot;
	my @incl_individuals;
	my @included_keys;
	my $model_id = $parm{'model_id'};

	my ( @header, $individuals, @bs_inds, $key_ref, @id_ids, @bs_id_ids );

	my @subj_keys = keys( %subjects );
	if ( $#subj_keys < 0 ) {
	  croak("sample_size must be defined" );
	}
	if ( defined $stratify_on ) {
	  my %strata;
	  if( $stratify_on =~ /\D/ ) {
	    %strata = %{$self->factors( column_head => $stratify_on )};
	    if ( $strata{'Non-unique values found'} eq '1' ) {
	      croak("At least one individual was found to have multiple values in the $stratify_on column. ".
			    "The column $stratify_on cannot be used for stratification of the resampling." );
	    }
	  } else {
	    %strata = %{$self->factors( column => $stratify_on )};
	    if ( $strata{'Non-unique values found'} eq '1' ) {
	      croak("At least one individual was found to have multiple values in column number $stratify_on. ".
			    "Column $stratify_on cannot be used for stratification of the resampling." );
	    }
	  }
	  if ( scalar keys( %subjects) != scalar keys( %strata ) and
	       not ( $#subj_keys == 0 and defined $subjects{'default'} ) ) {
	    croak("sample_size must be defined using one default value ".
			  "or exactly one value per strata:\n".
			  "resampling per STUD=1001,1002,1003\n".
			  "use -sample_size='1001=>10,1002=>25,1003=>12' or ".
			  "-sample_size='default=>10'");
	  }
 	  unless ( $resume and -e $new_name ) {
		  @header = @{$self->header()};
		  $individuals = $self->individuals();
		  my @factorlist = sort { $a <=> $b } keys %strata;
		  
		  foreach my $factor (@factorlist) {
			  my $key_list = $strata{$factor};	
			  my $keys;
			  if ( defined $subjects{$factor} ) {
				  $keys = $subjects{$factor};
			  } elsif( defined $subjects{'default'} ) {
				  $keys = sprintf( "%.0f",($subjects{'default'}*
										   (scalar(@{$key_list})) / ($self->count_ind())) );
			  } else {
				  croak("A sample size for strata $factor could not be found ".
						"and no default sample size was set" );
			  }
			  for ( my $i = 0; $i < $keys; $i++ ) {
				  my $list_ref = random_uniform_integer(1,0,(scalar(@{$key_list}) - 1));
				  push( @bs_inds, $individuals -> [ $key_list->[$list_ref] ]->copy );
				  push( @included_keys, $key_list->[$list_ref] );
				  push( @incl_individuals, $individuals -> [ $key_list->[$list_ref] ]->idnumber );
				  push( @bs_id_ids, $id_ids[ $key_list->[$list_ref] ] );
			  }
		  }

		  $boot = data->new( header      => \@header,
							 idcolumn    => $self->idcolumn,
							 ignoresign  => $self->ignoresign,
							 missing_data_token => $self->missing_data_token,
							 individuals => \@bs_inds,
							 filename    => $new_name,
							 ignore_missing_files => 1);
		  $boot->_renumber_ascending;
		  $boot->_write;
 	  } else {
		  # If we are resuming, we still need to generate the
		  # pseudo-random sequence and initiate a data object
		  while( my ( $factor, $key_list ) = each %strata ) {
			  my $keys;
			  if ( defined $subjects{$factor} ) {
				  $keys = $subjects{$factor};
			  } elsif( defined $subjects{'default'} ) {
				  $keys = sprintf( "%.0f",($subjects{'default'}*
										   (scalar(@{$key_list})) / ($self->count_ind())) );
			  } else {
				  croak("A sample size for strata $factor could not be found ".
						"and no default sample size was set" );
			  }
			  for ( my $i = 0; $i < $keys; $i++ ) {
				  my $list_ref = random_uniform_integer(1,0,(scalar(@{$key_list}) - 1));
			  }
		  }
	  }
	} else {
		my $size;
		if( defined $subjects{'default'} ) {
			$size = $subjects{'default'};
		} else {
			croak("No default sample size was set" );
		}
		unless ( $resume and -e $new_name ) {
			@header = @{$self->header()};
			$self->individuals([]) unless defined $self->individuals; # FIXME
			$individuals = $self->individuals;
			for ( my $i = 1; $i <= $size; $i++ ) {
				$key_ref = random_uniform_integer(1, 0, scalar @{$individuals} - 1);
				push( @bs_inds, $individuals->[$key_ref]->copy );
				push( @included_keys, $key_ref );
				push( @incl_individuals, $individuals->[ $key_ref ]->idnumber );
				push( @bs_id_ids, $id_ids[ $key_ref ] );
			}
			
			$boot = data->new( header      => \@header,
							   idcolumn    => $self->idcolumn,
							   ignoresign  => $self->ignoresign,
							   missing_data_token => $self->missing_data_token,
							   individuals => \@bs_inds,
							   filename    => $new_name,
							   ignore_missing_files => 1);
			
			$boot->_renumber_ascending;
			$boot->_write;

		} else {
			# If we are resuming, we still need to generate the
			# pseudo-random sequence
			for ( my $i = 1; $i <= $size; $i++ ) {
				random_uniform_integer(1,0,scalar @{$individuals}-1)
			}
		}	
	}

	return \@incl_individuals, \@included_keys;
}

sub resample_from_keys
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  new_name => { isa => 'Str', default => 'resampled.dta', optional => 1 },
							  key_arr => { isa => 'ArrayRef[Int]', optional => 0 }
	);
	my $new_name = $parm{'new_name'};
	my @key_arr = defined $parm{'key_arr'} ? @{$parm{'key_arr'}} : ();
	my $boot;

	my (@header, $individuals, @bs_inds);
	@header = @{$self->header};
	$individuals = $self->individuals;
	for (my $i = 0; $i < scalar(@key_arr); $i++) {
	  push(@bs_inds, $individuals->[$key_arr[$i]]->copy);
	}

	$boot = data->new(
		header      => \@header,
		idcolumn    => $self->idcolumn,
		ignoresign  => $self->ignoresign,
		missing_data_token => $self->missing_data_token,
		individuals => \@bs_inds,
		filename    => $new_name,
		ignore_missing_files => 1
	);
	$boot->_renumber_ascending;
	$boot->_write;
	$boot = undef;
	return $new_name;
}

sub subsets
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		bins => { isa => 'Int', optional => 1 },
		stratify_on => { isa => 'Maybe[Int]', optional => 1 },
		MX_PARAMS_VALIDATE_NO_CACHE => 1
	);
	my $bins = $parm{'bins'};
	my $stratify_on = $parm{'stratify_on'};
	my @subsets;
	my @incl_ids;

	#this is used in xv_step_subs.pm and nowhere else
	#returns data objects which are not written to disk
	#input is integer bins integer stratify_on
	#add possibility to have stratify_on $column_head which is then translated to column number
	#or make it only on column head instead of column number

	my @header  = @{$self->header()};
	my @comment = defined $self->comment() ? @{$self->comment()} : ();
	my @subset_ids= ();
	my %rnd_ids;
	my %rnd_ids_hash;
	my $key = 0;
	my @ids = @{$self->individuals()};
	if ( defined $stratify_on ) {
		my $work_data = $self->copy( filename => 'work_data.dta',
									 write_copy   => 0 );
		my %strata;
		if( $stratify_on =~ /^[0-9]+$/ ){
			%strata = %{$work_data->factors( column => $stratify_on )};
			if ( $strata{'Non-unique values found'} eq '1' ) {
				croak("Individuals were found to have multiple values in column".
					" number $stratify_on. ".
					"Column $stratify_on cannot be used for stratification of the subsets." );
			}
		} else {
			%strata = %{$work_data->factors( column_head => $stratify_on )};
			if ( $strata{'Non-unique values found'} eq '1' ) {
				croak("Individuals were found to have multiple values in".
					" the $stratify_on column. ".
					"The column $stratify_on cannot be used for stratification of the subsets." );
			}
		}

		# The default behaviour of factors is to return a hash with the factors as keys
		# and as values references to arrays with the order numbers (not the ID numbers)
		# of the individuals that contain this factor

		#Give each individual in each factor group a random number
		while ( my ( $factor, $keys ) = each %strata ) {
			if (scalar(@{$keys})< $bins){
				my $num = scalar(@{$keys});
				print "Warning: Only $num ids have stratification value $factor. Stratification variable inappropriate.\n";
			}
			foreach my $key ( @{$keys} ) {
				my $rnd_num = random_uniform(1,0,10);
				while ( defined $rnd_ids{$factor}{$rnd_num} ) {
					$rnd_num = random_uniform(1,0,10);
				}
				$rnd_ids_hash{$factor}{$rnd_num} = $ids[$key];
			}
		}
		my $first = 1;
		my $first_j=0;
		while ( my ($factor, $rnd_ids ) = each %rnd_ids_hash ) {
			#Sort individuals in each factor group according to assigned random number.
			#Deal, like a deck of cards,  individuals from each factor groups to bins.
			#If there are not enough 'cards' to give each bin equal number then 
			#whichever bin was next in turn will get the first individual from the next factor.
			#Individuals that should be ignored based on ignoresign are not filtered here,
			#so sorting may be more uneven after ignoring.

			my @keys = sort { $a <=> $b } keys %{$rnd_ids};
			for ( my $i = 0; $i <= $#keys; ) {
				my $j_index;
				for ( my $j = $first_j; $j < ($first_j+$bins); $j++ ) {
					$j_index = $j % $bins;
					if ( $first ) {
						push( @subset_ids, [$rnd_ids_hash{$factor}{$keys[$i]}->copy] );
						push( @incl_ids, [$rnd_ids_hash{$factor}{$keys[$i]}->idnumber] );
					} else {
						push( @{$subset_ids[$j_index]}, $rnd_ids_hash{$factor}{$keys[$i]}->copy );
						push( @{$incl_ids[$j_index]}, $rnd_ids_hash{$factor}{$keys[$i]}->idnumber );
					}
					$i++;
					last if $i > $#keys;
				}
				$first_j = $j_index+1;
				$first = 0;
			}
		}
		for ( my $j = 0; $j < $bins; $j++ ) {
			my $sdata = data->new ( header               => \@header,
									comment              => \@comment,
									ignoresign           => $self->ignoresign,
									individuals          => $subset_ids[$j],
									missing_data_token => $self->missing_data_token,
									ignore_missing_files => 1,
									idcolumn             => $self->idcolumn,
									filename             => "subset_$j.dta" );
			push( @subsets, $sdata );
		}
	} else {
		#no stratification
		for ( my $i = 0; $i <= $#ids; $i++ ) {
			my $rnd_num = random_uniform(1,0,10);
			while ( defined $rnd_ids{$rnd_num} ) {
				$rnd_num = random_uniform(1,0,10);
			}
			$rnd_ids{$rnd_num} = $ids[$i];
		}
		my @keys = sort { $a <=> $b } keys %rnd_ids;
		my $first = 1;
		for ( my $i = 0; $i <= $#keys; ) {
			for ( my $j = 0; $j < $bins; $j++ ) {
				if ( $first ) {
					push( @subset_ids, [$rnd_ids{$keys[$i]}->copy] );
					push( @incl_ids, [$rnd_ids{$keys[$i]}->idnumber] );
				} else {
					push( @{$subset_ids[$j]}, $rnd_ids{$keys[$i]}->copy );
					push( @{$incl_ids[$j]}, $rnd_ids{$keys[$i]}->idnumber );
				}
				$i++;
				last if $i > $#keys;
			}
			$first = 0;
		}
		for ( my $j = 0; $j < $bins; $j++ ) {
			my $sdata = data->new ( header               => \@header,
									comment              => \@comment,
									ignoresign           => $self->ignoresign,
									individuals          => $subset_ids[$j],
									missing_data_token => $self->missing_data_token,
									ignore_missing_files => 1,
									idcolumn             => $self->idcolumn,
									filename             => "subset_$j.dta" );
			push( @subsets, $sdata );
		}
	}

	return \@subsets ,\@incl_ids;
}


sub full_name
{
	my $self = shift;
	my $path = File::Spec->catfile(($self->directory), $self->filename);
	return $path;
}

sub split_vertically
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		split_index => { isa => 'Int', optional => 0 },
		stratify_index => { isa => 'Maybe[Int]', optional => 1 }
	);
	my $split_index = $parm{'split_index'};
	my $stratify_index = $parm{'stratify_index'};
	my @left_side_individuals;
	my @right_side_individuals;
	my @split_values;
	my @stratify_values;

	#split data set on column with index $split_index and extract stratification col
	#and return left_side_individuals and right_side_individuals as two arrays of individual objects, 
	#and split_values as ref of array of refs of arrays
	# and stratification values as array
	#without changing $self object. split values returned as array of array over individuals
	#used in randomization data generation

	unless (defined $self->individuals and scalar(@{$self->individuals}) > 0) {
		croak("cannot do split_vertically on empty data object");
	}
	my @individuals = @{$self->individuals};
	unless (defined $individuals[0] ) {
		croak("first individual not defined in split_vertically");
	}

	my @ind_data = @{$individuals[0]->subject_data};
	my $ncol = scalar(split(/,/, $ind_data[0]));
	if (($split_index < 0) || ($split_index >= $ncol)) {
		croak("illegal split_index $split_index in data->split_vertically, have $ncol columns");
	}
	if (defined $stratify_index and (($stratify_index < 0) || ($stratify_index >= $ncol) || ($stratify_index == $split_index))) {
		croak("illegal stratify_index $stratify_index in data->split_vertically, have $ncol columns and slit index $split_index");
	}
	my $left_start = 0;
	my $left_end = $split_index - 1;
	my $right_start = $split_index + 1;
	my $right_end = $ncol - 1;
	my $warned_stratify_error = 0;

	for (my $id = 0; $id <= $#individuals; $id++) {
		my $idnumber = $individuals[$id]->idnumber;
		my $idcolumn = $individuals[$id]->idcolumn;
		my @data = @{$individuals[$id]->subject_data};
		my @left_data = ();
		my @right_data = ();
		my @values = ();
		for (my $i = 0; $i < scalar(@data); $i++) {
			my @data_row = split(/,/, $data[$i]);
			push(@values,$data_row[$split_index]);
			if (defined $stratify_index) {
				if ($i == 0) {
					push(@stratify_values, $data_row[$stratify_index]);
				} else {
					if (not $warned_stratify_error and ($data_row[$stratify_index] != $stratify_values[-1])) {
						print "ERROR in randomization test preparation: non-unique values for stratification variable\n".
						"found for individual index $i, using first value and ignoring the rest.\n";
						$warned_stratify_error = 1;
					}
				}
			}
			if ($left_end >= $left_start) {
				push(@left_data, join(',', @data_row[$left_start .. $left_end]));
			}else{
				push(@left_data, '');
			}
			if ($right_start <= $right_end) {
				push(@right_data, join( ',', @data_row[$right_start .. $right_end]));
			}else{
				push(@right_data, '');
			}
		}

		push(@split_values, \@values);
		push(@left_side_individuals, data::individual->new(idnumber     => $idnumber,
				idcolumn     => $idcolumn,
				subject_data => \@left_data));
		push(@right_side_individuals, data::individual->new(idnumber     => $idnumber,
				idcolumn     => $idcolumn,
				subject_data => \@right_data));
	}

	return \@left_side_individuals, \@right_side_individuals, \@split_values, \@stratify_values;
}

sub create_randomized_data
{
	#static, no shift
	my %parm = validated_hash(\@_,
							  rand_index => { isa => 'Int', optional => 0 },
							  stratify_index => { isa => 'Maybe[Int]', optional => 1 },
							  name_stub => { isa => 'Str', optional => 1 },
							  samples => { isa => 'Int', optional => 0 },
							  equal_obs => { isa => 'Bool', optional => 0 },
							  input_filename => { isa => 'Str', optional => 0 },
							  input_directory => { isa => 'Maybe[Str]', optional => 1 },
							  output_directory => { isa => 'Str', optional => 0 },
							  ignoresign => { isa => 'Str', optional => 1 },
							  missing_data_token => { isa => 'Maybe[Num]', optional => 1 },
							  idcolumn => { isa => 'Int', optional => 0 }
		);
	my $rand_index = $parm{'rand_index'};
	my $stratify_index = $parm{'stratify_index'};
	my $name_stub = $parm{'name_stub'};
	my $samples = $parm{'samples'};
	my $equal_obs = $parm{'equal_obs'};
	my $input_filename = $parm{'input_filename'};
	my $input_directory = $parm{'input_directory'};
	my $output_directory = $parm{'output_directory'};
	my $ignoresign = $parm{'ignoresign'};
	my $missing_data_token = $parm{'missing_data_token'};
	my $idcolumn = $parm{'idcolumn'};

	unless (-d $output_directory){
		croak("output directory $output_directory is not a directory/does not exist");
	}
	my ($tmp1, $tmp2) = OSspecific::absolute_path($output_directory,'hej');
	$output_directory = $tmp1; #to get with /

	#data will be parsed here
	my $data = data->new(filename => $input_filename,
						 directory => $input_directory,
						 ignoresign => $ignoresign,
						 missing_data_token => $missing_data_token,
						 idcolumn => $idcolumn);

	#files are written in _randomize_data
	my $filenames = $data->_randomize_data(name_stub   => $name_stub,
										   samples     => $samples,
										   stratify_index => $stratify_index, 
										   rand_index => $rand_index, 
										   equal_obs => $equal_obs,
										   directory => $output_directory);
	$data = undef;
	return $filenames;
}
sub _randomize_data
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		rand_index => { isa => 'Int', optional => 0 },
		stratify_index => { isa => 'Maybe[Int]', optional => 1 },
		name_stub => { isa => 'Str', optional => 1 },
		samples => { isa => 'Int', optional => 0 },
		equal_obs => { isa => 'Bool', optional => 0 },
		directory => { isa => 'Str', optional => 1 }
	);
	my $rand_index = $parm{'rand_index'};
	my $stratify_index = $parm{'stratify_index'};
	my $name_stub = $parm{'name_stub'};
	my $samples = $parm{'samples'};
	my $equal_obs = $parm{'equal_obs'};
	my $directory = $parm{'directory'};
	my @data_file_names;

	#in is mandatory integer samples
	#mandatory integer rand_index index of randomization column
	#optional integer stratify_index
	#mandatory boolean equal_obs
	#optional string name_stub
	#optional directory where to write results
	#return array of data file names including dir

	#setup
	my ($left_side_individuals,$right_side_individuals,$rand_values,$stratify_values) = 
	$self->split_vertically(split_index => $rand_index,
		stratify_index => $stratify_index);

	my $n_individuals = scalar(@{$rand_values});
	my @header = @{$self->header()};
	my @stratified_data=();
	if (defined $stratify_index){
		@stratified_data = @{$self->stratify_indices(stratify_values => $stratify_values)};
	}else{
		push(@stratified_data,[0 .. ($n_individuals-1)]);
	}

	for (my $i=0; $i<$samples; $i++){
		my $new_name = defined $name_stub ? $name_stub."_".($i+1).".dta" : "rand_".($i+1).".dta";
		$new_name = $directory.'/'.$new_name if (defined $directory);
		my @new_individuals=();
		foreach my $individual (@{$left_side_individuals}){
			push(@new_individuals,$individual->copy());
		}
		for (my $j=0; $j < scalar(@stratified_data); $j++){
			my @shuffled_indices = @{$stratified_data[$j]};
			$self-> _fisher_yates_shuffle(array => \@shuffled_indices);
			for (my $k=0; $k< scalar(@shuffled_indices); $k++){
				my $base_index = $stratified_data[$j]->[$k];
				my $rand_index = $shuffled_indices[$k];
				my $new_values = $self->reconcile_column(old_values => $rand_values->[$base_index],
					template_values => $rand_values->[$rand_index],
					equal_obs => $equal_obs);
				$new_individuals[$base_index]->append_column(new_values => $new_values);
				$new_individuals[$base_index]->append_individual(new_individual => $right_side_individuals->[$base_index]);
			}
		}
		my $newdata = data->new( header      => \@header,
								 idcolumn    => $self->idcolumn,
								 missing_data_token => $self->missing_data_token,			 
								 ignoresign  => $self->ignoresign,
								 individuals => \@new_individuals,
								 filename    => $new_name,
								 ignore_missing_files => 1);
		$newdata->_write;
		push(@data_file_names,$new_name);
		$newdata = undef;
	}

	return \@data_file_names;
}

sub reconcile_column
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		old_values => { isa => 'Ref', optional => 0 },
		template_values => { isa => 'Ref', optional => 0 },
		equal_obs => { isa => 'Bool', optional => 0 }
	);
	my $old_values = $parm{'old_values'};
	my $template_values = $parm{'template_values'};
	my @new_values;
	my $equal_obs = $parm{'equal_obs'};

	#To be used in randomization test, determine and return array of values to replace old_values with
	#based on reconciliation between @{$old_values} and @{$template_values}
	# either $equal_obs is true or not
	#make array new_values as equal length to old_values
	#if $equal obs is true then copy directly from template_values to new_values.
	#if new_values is longer then do last observation carry forward. If new_values is shorter
	#then just skip last values in template_values
	#if equal_obs is not true (equal_switch is true) then create array value_sequence from template_values
	#copy first value from template_values to new_values. Then for each new 
	#position compare old_value at this pos with previous pos
	#if equal then set new_values at this pos to same as previous pos
	#if different then set new_values at this pos to next value in value_sequence.
	#If no more values in unique_values then last observation carry forward

	# FIXME: Undef is in Moose an allowed boolean.
	if (!defined($equal_obs)) {
		croak("Equal_obs must be defined\n");
	}

	if (scalar(@{$old_values})<1){
		return []; #nothing to do
	}elsif(scalar(@{$template_values})<1){
		croak("Empty template_values as input to individuals->update_column\n");
	}
	@new_values = @{$old_values};
	my $template_length=scalar(@{$template_values});
	if ($equal_obs){
		$new_values[0] = $template_values->[0];
		for (my $i=1; $i< scalar(@new_values); $i++){
			if ($i<$template_length){
				$new_values[$i] = $template_values->[$i];
			}else{
				$new_values[$i] = $new_values[$i-1]; #last obs carry forward, also if missing
			}
		}
	}else{
		#equal switch
		#find first non-missing in template and old
		my $first_non_miss_template = $self->missing_data_token();
		foreach my $val (@{$template_values}){
			if (looks_like_number($val)){
				if ($val != $self->missing_data_token()){
					$first_non_miss_template = $val;
					last;
				}
			}else{
				#dot ok for transition
				$first_non_miss_template = $val;
				last;
			}
		}
		my $first_non_miss_index=0;
		foreach my $val (@{$old_values}){
			if (looks_like_number($val)){
				if ($val != $self->missing_data_token()){
					last;
				}
			}else{
				#dot ok for transition
				last;
			}
			$first_non_miss_index++;
		}
		$first_non_miss_index=0 if($first_non_miss_index>=scalar(@{$old_values})); #found no non-missing
		#here we assume only non-numeric is . (dot)
		my @value_sequence=($first_non_miss_template);
		my $index=0;
		for (my $i=1; $i< scalar(@$template_values); $i++){
			if (looks_like_number($template_values->[$i]) and looks_like_number($value_sequence[$index])){
				#both numeric
				if (($template_values->[$i] != $value_sequence[$index]) and
					($template_values->[$i] != $self->missing_data_token())){
					push(@value_sequence,$template_values->[$i]);
					$index++;
				} 
			}elsif(looks_like_number($template_values->[$i]) or looks_like_number($value_sequence[$index])){
				#one is numeric, then must be different
				push(@value_sequence,$template_values->[$i]);
				$index++;
			}
		}
		$index=0;
		$new_values[0] = $value_sequence[$index];

		for (my $i=1; $i< scalar(@new_values); $i++){
			next unless ($i> $first_non_miss_index);
			if (looks_like_number($old_values->[$i]) and looks_like_number($old_values->[$i-1])){
				if (($old_values->[$i] !=$old_values->[$i-1]) and
					($old_values->[$i] != $self->missing_data_token())){
					#switch in old values
					$index++ unless ($index == $#value_sequence); #do not change index if no values left
				}
			}elsif(looks_like_number($old_values->[$i]) or looks_like_number($old_values->[$i-1])){
				#only one is numeric, then must be different
				#switch in old values
				$index++ unless ($index == $#value_sequence); #do not change index if no values left
			}
			$new_values[$i] = $value_sequence[$index];
		}
	}

	return \@new_values;
}

sub stratify_indices
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		stratify_values => { isa => 'Ref', optional => 0 }
	);
	my @stratified_indices;
	my $stratify_values = $parm{'stratify_values'};

	#used when creating data for randomization test
	#input is ref of array of stratification values
	#output is stratified_indices, array of refs of arrays of stratification indices
	#stratification is done on unique values of stratification values

	my %values_hash;
	my $next_index = 0;

	for (my $i = 0; $i < scalar (@{$stratify_values}); $i++) {
		my $value = $stratify_values->[$i];
		unless (defined $values_hash{$value}) {
			$values_hash{$value} = $next_index;
			push(@stratified_indices, []);
			$next_index++;
		}
		push(@{$stratified_indices[$values_hash{$value}]}, $i);
	}

	return \@stratified_indices;
}


sub get_eta_matrix
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		n_eta => { isa => 'Int', optional => 0 },
		start_eta => { isa => 'Int', optional => 0 }
	);
	my $n_eta = $parm{'n_eta'};
	my $start_eta = $parm{'start_eta'};
	my @eta_matrix = ();

	#used in frem

	my @columns = ();
	for (my $eta=$start_eta; $eta < ($n_eta+$start_eta); $eta++) {
		my $col = 'ETA('.$eta.')';
		my $index = $self->column_head_indices->{$col} - 1; #need to verify -1 here
		if ( $index < 0 or $index > $#{$self->header()} ) {
			print "Warning: column index out of bounds in get_eta_matrix\n";
			return [];
		}
		push(@columns,$index);
	}

	# to minimize risk of errors.

	foreach my $individual ( @{$self->individuals} ){
		foreach my $individual_row( @{$individual->subject_data} ){
			my @row = split(/,/ , $individual_row);
			my @new_row=();
			foreach my $index (@columns){
				push( @new_row, $row[$index] );
			}
			push( @eta_matrix, \@new_row );
		}
	}  

	return \@eta_matrix;
}

sub column_to_array
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		column => { isa => 'Str', optional => 0 },				# Counting from zero
		filter => { isa => 'ArrayRef', optional => 1 }		# Warning: Do not use ArrayRef[Int] here. Causes Perl to crash. Bug report filed
	);
	my $column = $parm{'column'};
	my @array;
	my @filter = defined $parm{'filter'} ? @{$parm{'filter'}} : ();

	if (not $column =~ /^\d/) {
		$column = $self->column_head_indices->{$column} - 1;
	}

	if ($column < 0) {  # FIXME: had upper bounds check here or $column > $#{$self->header}) {
		return [];
	}

	#have separate loops for case without filter and with filter,
	#to minimize risk of errors.

	if (scalar(@filter) == 0) {
		foreach my $individual (@{$self->individuals}) {
			foreach my $individual_row(@{$individual->subject_data}) {
				my @row = split(/,/, $individual_row);
				push(@array, $row[$column]);
			}
		}  
	} else {
		my $index = 0;
		foreach my $individual (@{$self->individuals}) {
			foreach my $individual_row(@{$individual->subject_data}) {
				if ($filter[$index] > 0) {
					my @row = split(/,/, $individual_row);
					push(@array, $row[$column]);
				}
				$index++;
				if ($index > $#filter) {
					$index = 0;
				}
			}
		}
		unless ($index == 0) {
			croak("Number of rows in dataset was not a multiple of filter length.");
		}
	}

	return \@array;
}

sub _write
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		filename => { isa => 'Str', default => $self->full_name, optional => 1 },
		MX_PARAMS_VALIDATE_NO_CACHE => 1
	);
	my $filename = $parm{'filename'};

	die "ERROR: data->_write: No filename set in data object.\n" if( $filename eq '' );

	unless( defined $self->individuals()  and (scalar(@{$self->individuals()})>0)){
		# If we don't have any individuals this is a bug
		croak("Trying to write to $filename, but no individuals in memory");
	}

	if (-e $filename){
		croak("Trying to write to $filename, but file already exists");
	}

	open(FILE,">$filename") || 
	die "Could not create $filename\n";
	my $data_ref = $self->format_data;
	my @data = @{$data_ref};
	for ( @data ) {
		print ( FILE );
	}
	close(FILE);
}

sub _fisher_yates_shuffle
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		array => { isa => 'ArrayRef[Str]', optional => 1 }
	);
	my @array = defined $parm{'array'} ? @{$parm{'array'}} : ();

	my $arr_ref = $parm{'array'};
	carp("Array of zero length received" )
	if ( scalar @{$arr_ref} < 1 );
	my $i;
	for ($i = @$arr_ref; --$i;) {
		my $j = random_uniform_integer(1, 0, $i);
		@$arr_ref[$i, $j] = @$arr_ref[$j, $i];
	}
}

sub _read_header
{
	my $self = shift;

	my $filename   = $self->full_name;
	my $ignoresign = $self->ignoresign;
	my ( @data, @new_record, $row, $tmp_row, @header, $hdrstring );
	$row=0;
	open(DATAFILE,"$filename") || 
	die "Could not open $filename for reading";
	my $found_data=0;
	while (<DATAFILE>) {
		s/^\s*//; #skip leading spaces
		#skip spaces after commas
		s/\,\s*/\,/g;
		$tmp_row    = $_;
		
		my $is_header=0;

		if (defined $ignoresign and length($ignoresign)>0){
			if ($ignoresign eq '@'){
				#A-Z a-z and #
				if (/^[A-Za-z#]/){
					$is_header=1;
				}
			}else{
				#literal ignoresign
				if (/^$ignoresign/){
					$is_header=1;
				}
			}
		}else{
			if (/^[A-Za-z#;]/){
				$is_header=1;
			}
		}
		if ( $is_header){
			$data[$row] = $tmp_row;
			$row++;
		} else {
			# We have reached the first data-row, break
			$found_data=1;
			last;
		}
	}
	close(DATAFILE);
	print "\nWarning: Found no data lines in ".$self->filename().
		". This can happen e.g.\nif you have a data file in old MacOSX format and run on unix/linux,\n".
		"in which case the workaround is to run mac2unix on " . $self->filename . "\n" unless $found_data;
	
	chomp($hdrstring = pop(@data)); #last value of array
#	print "headerstring $hdrstring\n";
	@header = split(/\,\s*|\s+/, $hdrstring);
	if ($self->parse_header or not defined $self->idcolumn) {
		if (not $self->parse_header and not defined $self->idcolumn) {
			print "No id column specified for data->_read_header: trying to parse the header anyway\n";
		}

		my @new_header;
		for (my $i = 1; $i <= scalar @header; $i++) {
			if ($header[$i - 1] eq 'CONT') {
				if (defined $self->cont_column and not $i == $self->cont_column) {
					carp("The supplied columns for the CONT data item (".
						$self->cont_column . ") does not match the column where the CONT ".
						"header was found ($i), using $i");
				}
				$self->cont_column($i);
			} else {
				push(@new_header, $header[$i - 1]);
			}
		}
		@header = @new_header;
		for (my $i = 1; $i <= scalar @header; $i++) {
			if ($header[$i - 1] eq 'ID') {
				if (defined $self->idcolumn and not $i == $self->idcolumn) {
					carp("The supplied columns for the ID data item (" .
						$self->idcolumn . ") does not match the column where the ID ".
						"header was found ($i), using $i");
				}
				$self->idcolumn($i);
			}
		}

		if (not defined $self->idcolumn) {
			croak("Cannot find an id column in the data set header\n".join(' ',@header));
		}
	}

	$self->header(\@header);
	$self->comment(\@data);
}

sub _read_individuals
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		skip_tables => { isa => 'Int', default => 0, optional => 1 },
		max_individuals => { isa => 'Int', default => 0, optional => 1 }
	);
	my $skip_tables = $parm{'skip_tables'};
	my $max_individuals = $parm{'max_individuals'};

	my $idcol	= $self->idcolumn;
	my $filename = $self->full_name;
	my $ignoresign = $self->ignoresign;
	#print "idcolumn in read_individuals is $idcol\n";
	open(DATAFILE,"$filename") || die "Could not open $filename for reading";

	my ( @new_row, $new_ID, $old_ID, @init_data );

	my ( $sth, $dbh, $first_row_id, $first_value_id );
	my $insert = 1;

	my $insert_rows;
	my $insert_values;
	my $row_counter = 0;
	my $table_counter = 0;
	my $full_row;
	ROW: while ( <DATAFILE> ) {
		#scan to first table to read. Will do next ROW until table header $skip_tables+1
		if (($skip_tables>0) && ($table_counter<= $skip_tables)) {
			#if want to read only part of file and have not found start place yet
			if ( /^TABLE/ ) {
				$table_counter++;
			}
			next ROW;
		}

		s/^\s*//; #skip leading spaces
		s/\,\s*/\,/g; #spaces after commas

		my @new_row	= split(/\,\s*|\s+/);
		my $is_data=1;

		if (defined $ignoresign and length($ignoresign)>0){
			if ($ignoresign eq '@'){
				#A-Z a-z and #
				if (/^[A-Za-z#]/){
					$is_data=0;
				}
			}else{
				#literal ignoresign
				if (/^$ignoresign/){
					$is_data=0;
				}
			}
			if ($is_data and (/^[A-Za-c#;]/)){
				croak("Error: We have IGNORE=".$ignoresign." so the data set line\n".$_.
					  "is not filtered out, but it looks like a header/comment.\n");
			}
		}else{
			if (/^[A-Za-z#;]/){
				$is_data=0;
			}
		}

		if ( $is_data ) {

#			print "data is ".join(';',@new_row)."\n";
			if ( defined $self->cont_column ) {
				if ( $new_row[$self->cont_column - 1] == 1 ) {
					if ( not $self->table_file ) { # Skip the CONT=1 rows if this is a table file
						for ( my $i = $#new_row; $i > 0; $i-- ) {
							if ( $i != ($self->cont_column - 1) ) {
								unshift( @{$full_row}, $new_row[$i] );
							}
						}
					}
					next ROW;
				} else {
					for ( my $i = $#new_row; $i >= 0; $i-- ) {
						if ( $i != ($self->cont_column - 1) ) {
							unshift( @{$full_row}, $new_row[$i] );
						}
					}
				}
			} else {
				@{$full_row} = @new_row;
			}
			$new_ID = $full_row->[$idcol-1]; # index starts at 0
			$old_ID = $new_ID if ( not defined $old_ID );

			#If we have not yet found first individual to read, then
			#count each time new individual found. If new individual is 
			#first individual to read, then reset old_ID so that 
			#the individual's lines will be read before new individual is pushed.
			#If we have not found first individual to read then skip to next line in file

			# Check if column miss data at some row (This adds about 30% of init time)
			my $mdt = $self->missing_data_token;
			for ( my $i = 0; $i <= $#{$full_row}; $i++ ) {			# FIXME: Gives a warning. Should change to eq below
				$self->found_missing_data->{$i+1} = 1
				if ( $full_row->[$i] == $mdt ); # == is slower but safer than eq
			}
			if ( $new_ID != $old_ID ) {
				my @subject_data = @init_data;
#				print "individual new, idcol is $idcol\n";
				my $id = data::individual->new ( idcolumn     => $idcol,
					subject_data => \@subject_data );
				unless (defined $self->individuals) {
					$self->individuals([]);
				}
				push( @{$self->individuals}, $id );

				#check if have stored max number individuals
				if ($max_individuals > 0) {
					if (scalar(@{$self->individuals}) == $max_individuals) {
						@init_data = (); #prevent any more rows from being stored after loop
						last ROW;
					}
				}

				@init_data =(join( ",", @{$full_row}));
			} else {
				push( @init_data, join( ",", @{$full_row}) );
			}
			$old_ID = $new_ID;
			$full_row = undef;
		}
	}

	# if we have ended loop because of max number individuals, init_data will be empty.
	if ( $#init_data >= 0 ) {
		$self->individuals([]) unless defined $self->individuals;
#		print "individual new, idcol is $idcol\n";
		push( @{$self->individuals()},
			data::individual->new ( idcolumn     => $idcol,
				subject_data => \@init_data ) );
	}
	close(DATAFILE);
}

sub create_row_filter
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		no_individuals => { isa => 'Int', optional => 0 }
	);
	my @filter = ();
	my $no_individuals = $parm{'no_individuals'};

	my %index_hash = ();
	my $index = 0;
	my $keep;
	my $ind_counter = 0;

	my $count=scalar(@{$self->individuals()});
	if ($no_individuals < 1){
		croak("Requested filter length must be at least 1 individual.");
	}
	if ($no_individuals > $count){
		croak("Requested filter length $no_individuals individuals is larger ".
			"than number of individuals in loaded dataset $count.");
	}

	foreach my $column_name (@{$self->header()}){
		#could add if-statement here to allow different filter types.
		if ($column_name =~ /^(MDV)$/){
			if (exists $index_hash{$1}){
				croak("Found column header $1 twice, bailing out.");
			}
			$index_hash{$1}=$index;
		}
		$index++;
	}

	#if no columns to filter on were found, return empty filter array 
	#which means "keep everything". Otherwise enter loop below.

	unless (scalar(keys %index_hash) < 1){
		foreach my $individual ( @{$self->individuals()} ) {
			foreach my $datarow ( @{$individual->subject_data}){
				$keep = 1;
				my @row = split( /,/ , $datarow );
				foreach my $key (keys %index_hash){
					if ($row[$index_hash{$key}] != 0){
						$keep = 0;
						last;
					}
				}
				push (@filter,$keep);
			}
			$ind_counter++;
			if ($ind_counter == $no_individuals){
				last;
			}
		}
	}

	return \@filter;
}

sub lasso_calculate_covariate_statistics
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  column_number => { isa => 'Int', optional => 1 },
							  breakpoint => { isa => 'Maybe[Num]', optional => 1 },
							  function => { isa => 'Int', optional => 1 },
							  missing_data_token => { isa => 'Maybe[Num]', optional => 0 }
	);

	#here parse data file unless already parsed

	my $column_number = $parm{'column_number'};
	my $breakpoint = $parm{'breakpoint'};
	my $function = $parm{'function'};
	my $missing_data_token = $parm{'missing_data_token'};
	my %statistics;

	my $have_missing_data = $self->have_missing_data(column => $column_number);

#check 'Non-unique values found' => 1
	my $n_individuals = $self->count_ind();
	if ($function == 1){
		#linear categorical
		# Sort by frequency
		my %temp_factors = %{$self->lasso_get_categories(column_number => $column_number)};
		#here we may have floating point categories. Redefine to integer
		my $all_integer=1;
		foreach my $fact (keys %temp_factors){
			my $tmp = sprintf("%.0f",$fact);
			$all_integer = 0 unless ($tmp == $fact);
		}
		my %factors;
		if ($all_integer){
			foreach my $fact (keys %temp_factors){
				my $tmp = sprintf("%.0f",$fact);
				$factors{$tmp}=$temp_factors{$fact};
			}
		}else{
			croak("the lasso can currently not handle non-integer categorical values for covariates. ".
				"You need to change your dataset so that all categorical covariates only have integer values.");
		}
		$statistics{'cat_hash'} = \%factors;
		#sort most first
		my @sorted = sort {$factors{$b}<=>$factors{$a}} keys (%factors);
		if ($sorted[0] ne $missing_data_token or (scalar (@sorted)==1 )){
			$statistics{'most_common'} = $sorted[0]; # First element of the sorted array
			# (the factor that most subjects have)
		}else{
			$statistics{'most_common'} = $sorted[1];
		} 
		foreach my $fact (@sorted){
			next if ($fact eq $statistics{'most_common'});
			#mean is number of subjects with this factor divided by N ind
			$statistics{'mean'}{$fact} = $factors{$fact}/$n_individuals;
			my $sd_sum = 0;
			foreach my $individual ( @{$self -> individuals} ){
				my $ifactors = $individual -> subject_data;
				my $value = 0;
				for(my $i=0; $i<=$#{$ifactors}; $i++ ){
					my @recor = split(',', $ifactors -> [$i], $column_number+1);
					my $type = $recor[$column_number-1];
					if ($type eq $fact){
						$value+=1/($#{$ifactors}+1);
					}
				}
				$sd_sum+=($value-$statistics{'mean'}{$fact})**2;
			}
			$statistics{'sd'}{$fact}=sqrt($sd_sum/($n_individuals-1));
		}

	}elsif ($function == 3){
		#max and min ignores missing data
		if (defined $breakpoint){
			$statistics{'breakpoint'} = $breakpoint;
		}else{
			$statistics{'breakpoint'} = $self -> median( column => $column_number,
				unique_in_individual => 1);
		}
		$statistics{'min'} = $self -> min(column => $column_number);
		$statistics{'max'} = $self -> max(column => $column_number);
		$statistics{'sd'} = $self -> sd( column => $column_number);

		$statistics{'mean'} = $self -> mean( column => $column_number);

		$statistics{'H-sd'} = $self -> sd( column => $column_number,
			hi_cutoff => $statistics{'breakpoint'});

		$statistics{'H-mean'} = $self -> mean( column => $column_number,
			hi_cutoff => $statistics{'breakpoint'});
	}else {
		$statistics{'mean'} = $self -> mean( column => $column_number);
		$statistics{'sd'} = $self -> sd(column => $column_number);
		$statistics{'min'} = $self -> min(column => $column_number);
		$statistics{'max'} = $self -> max(column => $column_number);
	}

	return \%statistics;
}

sub lasso_get_categories
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		column_number => { isa => 'Int', optional => 0 }
	);
	my $column_number = $parm{'column_number'};
	my %categories;

	my @individuals = @{$self->individuals};
	my $first_id = @individuals[0];
	die "data -> factor: No individuals defined in data object based on ",
	$self->filename,"\n" unless defined $first_id;

	my $type;

	foreach my $individual ( @individuals ) {
		my $ifactors = $individual -> subject_data;

		for(my $i=0; $i<=$#{$ifactors}; $i++ ) {
			my @recor = split(',', $ifactors -> [$i], $column_number+1);
			$type = $recor[$column_number-1];
			#Weight the individual data
			my $value = 1/($#{$ifactors}+1);
			if (exists $categories{$type}){
				$categories{$type}+=$value;
			} else {
				$categories{$type}=$value;
			}
		}
	}

	return \%categories;
}

sub scm_calculate_covariate_statistics
{
	#unit test indirectly via scm->new in unit/scm.t
	my $self = shift;
	my %parm = validated_hash(\@_,
							  categorical_covariates => { isa => 'Maybe[ArrayRef]', optional => 1},
							  continuous_covariates => { isa => 'Maybe[ArrayRef]', optional => 1},
							  model_column_numbers => { isa => 'HashRef', optional => 0},
							  time_varying => { isa => 'Maybe[ArrayRef]', optional => 1},
							  linearize => { isa => 'Bool', optional => 0 },
							  return_after_derivatives_done => { isa => 'Bool', optional => 0 },
							  gof => { isa => 'Str', optional => 0 },
							  missing_data_token => { isa => 'Maybe[Num]', optional => 0 }
		);
	#ref of hash of cov names to column numbers
	my %model_column_numbers = (defined $parm{'model_column_numbers'})? %{$parm{'model_column_numbers'}}: ();
	my @categorical_covariates = (defined $parm{'categorical_covariates'})? @{$parm{'categorical_covariates'}}: ();
	my @continuous_covariates = (defined $parm{'continuous_covariates'})? @{$parm{'continuous_covariates'}}: ();
	my $missing_data_token = $parm{'missing_data_token'};
	my $gof = $parm{'gof'};
	my $linearize = $parm{'linearize'};
	my @time_varying = (defined $parm{'time_varying'})? @{$parm{'time_varying'}}: ();;
	my $return_after_derivatives_done = $parm{'return_after_derivatives_done'};
	my $results={};

	my $category='scm';

	unless( defined $self->individuals()  and (scalar(@{$self->individuals}) > 0)) {
		croak("empty data in scm_calculate_covariate_statistics");
	}

	if (scalar(@continuous_covariates)>0) {
		ui -> print( category => $category,
					 message  => "Calculating continuous covariate statistics",
					 newline => 1);

		my $ncov = scalar(@continuous_covariates);
		my $status_bar = status_bar -> new( steps => $ncov );
		ui -> print( category => $category,
					 message  => $status_bar -> print_step(),
					 newline  => 0);

		foreach my $cov (@continuous_covariates){
			# Factors
			unless (defined $model_column_numbers{$cov}){
				croak("Could not find continuous covariate $cov in \$INPUT of model:\n".
					  join(' ',(keys %model_column_numbers)));
			}
			$results->{$cov}{'factors'} = $self -> factors( column => $model_column_numbers{$cov},
													   unique_in_individual => 0,
													   return_occurences => 1 );
			# Statistics
			$results->{$cov}{'have_missing_data'} = $self -> have_missing_data( column => $model_column_numbers{$cov} );

			($results->{$cov}{'median'},$results->{$cov}{'min'},$results->{$cov}{'max'},$results->{$cov}{'mean'}) =
				$self -> scm_calculate_continuous_statistics(covariate => $cov,
															 column_number => $model_column_numbers{$cov},
															 time_varying => \@time_varying,
															 linearize => $linearize,
															 return_after_derivatives_done => $return_after_derivatives_done);
			if( $status_bar -> tick () ){
				ui -> print( category => $category,
							 message  => $status_bar -> print_step(),
							 wrap     => 0,
							 newline  => 0 );
			}
		}
		ui -> print( category => $category,
					 message  => " ... done",newline => 1 );
		
		
	}
	if (scalar(@categorical_covariates)>0) {
		ui -> print( category => $category,
					 message  => "Calculating categorical covariate statistics",
					 newline => 1);
		my $ncov = scalar(@categorical_covariates);

		my $status_bar = status_bar -> new( steps => $ncov );
		ui -> print( category => $category,
					 message  => $status_bar -> print_step(),
					 newline  => 0);

		foreach my $cov (@categorical_covariates){
			unless (defined $model_column_numbers{$cov}){
				croak("Could not find categorical covariate $cov in \$INPUT of model:\n".
					  join(' ',(keys %model_column_numbers)));
			}
			# Factors
			$results->{$cov}{'factors'} = $self -> factors( column => $model_column_numbers{$cov},
															unique_in_individual => 0,
															return_occurences => 1 );
			# Statistics
			$results->{$cov}{'have_missing_data'} = $self -> have_missing_data( column => $model_column_numbers{$cov} );
			( $results->{$cov}{'median'},$results->{$cov}{'min'},	$results->{$cov}{'max'} ) =
				$self -> scm_calculate_categorical_statistics(covariate => $cov,
															  column_number => $model_column_numbers{$cov},
															  missing_data_token => $missing_data_token,
															  factors => $results->{$cov}{'factors'},
															  gof => $gof,
															  linearize => $linearize);
			
			if( $status_bar -> tick () ){
				ui -> print( category => $category,
							 message  => $status_bar -> print_step(),
							 wrap     => 0,
							 newline  => 0 );
			}
		}
		ui -> print( category => $category,
					 message  => " ... done",
					 newline => 1);
	}
	return $results;
}

sub scm_calculate_categorical_statistics
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  covariate => { isa => 'Str', optional => 0 },
							  column_number => { isa => 'Int', optional => 0 },
							  missing_data_token => { isa => 'Maybe[Num]', optional => 0 },
							  factors => { isa => 'HashRef', optional => 0 },
							  gof => { isa => 'Str', optional => 0 },
							  linearize => { isa => 'Bool', optional => 0 }
	);
	my $covariate = $parm{'covariate'};
	my $column_number = $parm{'column_number'};
	my $linearize = $parm{'linearize'};
	my $missing_data_token = $parm{'missing_data_token'};
	my $gof = $parm{'gof'};
	my %factors = defined $parm{'factors'} ? %{$parm{'factors'}} : ();

	my $median;
	my $min;
	my $max;

	my %strata = %{$self-> factors( column => $column_number,
									return_occurences =>1,
									unique_in_individual => 1,
									ignore_missing => 1)};
	
	if ( $strata{'Non-unique values found'} eq '1' ) {
		if ($linearize){
			ui -> print( category => 'all',
				message => "\nWarning: Individuals were found to have multiple values ".
				"in the $covariate column, this renders the linearization inappropriate for this covariate. ".
				"Consider terminating this run and setting ".
				"covariate $covariate as continuous and time-varying in the configuration file.\n" );
		}
	}

	# Sort by frequency
	my @sorted = sort {$factors{$b}<=>$factors{$a}} keys (%factors); #switched a b Kajsa bugfix
	if (scalar(@sorted) > 11){
		ui-> print (category => 'scm',
			"\n\n***Warning:***\nMore than 11 categories found for a categorical ".
			"covariate. The program can only handle changes by 10 degrees of freedom.".
			"\n",newline => 1) unless ( lc($gof) eq 'p_value' );

	}

	# These lines will set the most common value in $medians{$cov}
	if ($sorted[0] ne $missing_data_token or (scalar (@sorted)==1 )){
		$median = $sorted[0]; # First element of the sorted array
		# (the factor that most subjects have)
	}else{
		$median = $sorted[1];
	} 
	#max and min ignores missing data
	$max = $self -> max( column => $column_number );
	$min = $self -> min( column => $column_number );

	return $median ,$min ,$max;
}

sub scm_calculate_continuous_statistics
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  covariate => { isa => 'Str', optional => 0 },
							  column_number => { isa => 'Int', optional => 0 },
							  time_varying => { isa => 'Maybe[ArrayRef]', optional => 1},
							  linearize => { isa => 'Bool', optional => 0 },
							  return_after_derivatives_done => { isa => 'Bool', optional => 0 }
	);
	my $covariate = $parm{'covariate'};
	my $column_number = $parm{'column_number'};
	my $linearize = $parm{'linearize'};
	my @time_varying = (defined $parm{'time_varying'})? @{$parm{'time_varying'}}: ();;
	my $return_after_derivatives_done = $parm{'return_after_derivatives_done'};

	my $median;
	my $min;
	my $max;
	my $mean;

	my %strata = %{$self-> factors( column => $column_number,
									return_occurences =>1,
									unique_in_individual => 1,
									ignore_missing => 1)};
	
	if ( $strata{'Non-unique values found'} eq '1' ) {
		my $found=0;
		foreach my $tv (@time_varying){
			$found =1 if ($tv eq $covariate);
		}
		unless ($found){
			if ($linearize){
				ui -> print( category => 'all',
					message => "\nWarning: Individuals were found to have multiple ".
					"values in the $covariate column, this renders the linearization ".
					"inappropriate for this covariate. Consider terminating this run and ".
					"setting covariate $covariate as time-varying in the configuration ".
					"file.\n" ) unless $return_after_derivatives_done;
			}else{
				ui -> print( category => 'all',
					message => "\nWarning: Individuals were found to have multiple values ".
					"in the $covariate column, but $covariate was not set as time_varying in the ".
					"configuration file. Mean and median may not be computed correctly for $covariate. ") unless $return_after_derivatives_done;
			}	
		}
	}

	#must be unique in individual here, to do median over individuals rather than observations
	$median = $self-> median( unique_in_individual => 1,
							   column => $column_number);

	$max = $self -> max(column => $column_number );
	$min = $self -> min(column => $column_number );
	$mean = $self -> mean(column => $column_number );


	$median = sprintf("%6.2f", $median );
	$mean = sprintf("%6.2f", $mean );

	return $median ,$min ,$max ,$mean;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
