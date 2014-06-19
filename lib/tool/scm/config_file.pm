package tool::scm::config_file;

use include_modules;
use ext::Config::Tiny;
use Moose;
use MooseX::Params::Validate;
use file;

has 'file' => ( is => 'rw', isa => 'file' );
has 'abort_on_fail' => ( is => 'rw', isa => 'Bool|Ref', default => sub { \'1,0' });
has 'base_criteria_values' => ( is => 'rw', isa => 'Any', default => sub { } );
has 'categorical_covariates' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has 'error_code' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'code' => ( is => 'rw', isa => 'HashRef', default => sub { { 'ARRAY' => undef } } );
has 'compress' => ( is => 'rw', isa => 'Bool|Ref', default => sub { \'1,0' } );
has 'continuous_covariates' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has 'cpu_time' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'debug' => ( is => 'rw', isa => 'Int|Ref', default => sub { \'1,2,3' } );
has 'directory' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'do_not_drop' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'extra_files' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'global_init' => ( is => 'rw', isa => 'Num|Ref', default => sub { \'' } );
has 'gof' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'ofv,p_value' } );
has 'grid_batch_size' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'included_relations' => ( is => 'rw', isa => 'HashRef', default => sub { { 'ARRAY' => undef } } );
has 'logfile' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'lower_bounds' => ( is => 'rw', isa => 'HashRef', default => sub { { 'ARRAY' => undef } } );
has 'missing_data_token' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'model' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'nm_version' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'nmfe_options' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'ofv_backward' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'ofv_change' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'ofv_forward' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'p_backward' => ( is => 'rw', isa => 'Num|Ref', default => sub { \'' } );
has 'p_forward' => ( is => 'rw', isa => 'Num|Ref', default => sub { \'' } );
has 'p_value' => ( is => 'rw', isa => 'Num|Ref', default => sub { \'' } );
has 'picky' => ( is => 'rw', isa => 'Bool|Ref', default => sub { \'1,0' });
has 'relations' => ( is => 'rw', isa => 'HashRef' );
has 'retries' => ( is => 'rw', isa => 'Int|Ref', default => sub { \'' } );
has 'linearize' => ( is => 'rw', isa => 'Bool|Ref', default => sub { \'1,0' } );
has 'noabort' => ( is => 'rw', isa => 'Bool|Ref', default => sub { \'1,0' } );
has 'max_steps' => ( is => 'rw', isa => 'Int|Ref', default => sub { \'' } );
has 'epsilon' => ( is => 'rw', isa => 'Bool|Ref', default => sub { \'1,0' } );
has 'only_successful' => ( is => 'rw', isa => 'Bool|Ref', default => sub { \'1,0' } );
has 'parallel_states' => ( is => 'rw', isa => 'Bool|Ref', default => sub { \'1,0' } );
has 'update_derivatives' => ( is => 'rw', isa => 'Bool|Ref', default => sub { \'1,0' } );
has 'error' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'add,prop,propadd,exp,user' } );
has 'logit' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has 'time_varying' => ( is => 'rw', isa => 'Maybe[ArrayRef[Str]]', default => sub { [] } );
has 'second_order' => ( is => 'rw', isa => 'Bool|Ref', default => sub { \'1,0' } );
has 'foce' => ( is => 'rw', isa => 'Bool|Ref', default => sub { \'1,0' } );
has 'lst_file' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'derivatives_data' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'search_direction' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'forward,backward,both' } );
has 'seed' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'subtools' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has 'subtool_arguments' => ( is => 'rw', isa => 'Any', default => sub { } );
has 'test_relations' => ( is => 'rw', isa => 'HashRef', default => sub { { 'ARRAY' => undef } } );
has 'threads' => ( is => 'rw', isa => 'Int|Ref', default => sub { \'' } );
has 'tweak_inits' => ( is => 'rw', isa => 'Bool|Ref', default => sub { \'1,0' } );
has 'upper_bounds' => ( is => 'rw', isa => 'HashRef', default => sub { { 'ARRAY' => undef } } );
has 'valid_array_options' => ( is => 'rw', isa => 'HashRef' );
has 'valid_code_options' => ( is => 'rw', isa => 'HashRef' );
has 'valid_hash_options' => ( is => 'rw', isa => 'HashRef' );
has 'valid_scalar_options' => ( is => 'rw', isa => 'HashRef' );
has 'valid_states' => ( is => 'rw', isa => 'HashRef', default => sub { { 'ARRAY' => undef } } );
has 'inits' => ( is => 'rw', isa => 'HashRef', default => sub { { 'ARRAY' => undef } } );
has 'grid_poll_interval' => ( is => 'rw', isa => 'Int|Ref', default => sub { \'' } );
has 'run_on_lsf' => ( is => 'rw', isa => 'Bool|Ref', default => sub { \'' } );
has 'lsf_resources' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'lsf_queue' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'lsf_project_name' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'lsf_job_name' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'lsf_ttl' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'lsf_options' => ( is => 'rw', isa => 'Str|Ref', default => sub { \'' } );
has 'run_on_torque' => ( is => 'rw', isa => 'Bool|Ref', default => sub { \'1,0' } );

sub BUILD
{
	my $self = shift;

	unless( defined $self -> file ){
		croak('You must give a "file" argument to config_file -> new' );
	}
	my %valid_scalar_options;
	my %valid_array_options;
	my %valid_hash_options;
	my %valid_code_options;
	# Get the types of the possible options.
	foreach my $key ( keys %{$self} ){
		if( ref( $self -> $key ) eq 'SCALAR' ) {
			if( ${$self -> $key} ne '' ){
				$valid_scalar_options{$key} = $self -> $key;
			} else {
				$valid_scalar_options{$key} = 1;
			}
			$self -> {$key} = undef; #FIXME
		} elsif( ref( $self -> $key ) eq 'ARRAY' ) {
			$valid_array_options{$key} = 1;
			$self -> {$key} = undef; #FIXME
		} elsif( ref( $self -> $key ) eq 'HASH' ) {
			if( keys %{ $self -> $key } > 0 ){
				my @list = keys %{ $self -> $key };
				if( $list[0] eq 'ARRAY' ){
					$valid_hash_options{$key} = 'ARRAY';
				} else {
					carp("Type specification of $key is weird\n" );
				}
			} else {
				$valid_hash_options{$key} = 'SCALAR';
			}
			$self -> {$key} = undef; #FIXME
		} elsif( ref( $self -> $key ) eq 'CODE' ){
			$valid_code_options{$key} = 1;
			$self -> {$key} = undef; #FIXME
		}
	}
	$self -> valid_scalar_options(\%valid_scalar_options);
	$self -> valid_array_options(\%valid_array_options);
	$self -> valid_hash_options(\%valid_hash_options);
	$self -> valid_code_options(\%valid_code_options);

	my $string;
	open( FILE, $self -> file -> full_name );
	while( <FILE> ){
		s/\s*\\\s*$/\\/;
		s/[\t\r\f]*//g;
		s/^\s*//;
		$string .= $_ ;
	}
	close( FILE );

	my $config_tiny = ext::Config::Tiny -> read_string( $string );

	unless( defined $config_tiny ){
		croak("In configuration file [ " . $self -> file -> name . " ]: " . $ext::Config::Tiny::errstr );
	}

	# Force config_tiny to lowercase
	foreach my $section ( keys %{$config_tiny} ){
		my %new_section = %{$config_tiny -> {$section}};
		if( $section eq '_' ){
			foreach my $option ( keys %{$config_tiny -> {$section}} ) {
				my $value = $config_tiny -> { $section } -> { $option };
				$new_section{ lc($option) } = $value;
			}
		}
		delete $config_tiny -> { $section };
		$config_tiny -> { lc( $section ) } = \%new_section ;
	}

	# Check for the three main section.
	foreach my $section( 'test_relations' ){
		unless( defined $config_tiny -> {$section} ){
			croak("scm::config_file -> new: No [$section] section found." );
		} else {
			unless( scalar( keys( %{$config_tiny -> {$section}} ) ) > 0 ){
				croak("scm::config_file -> new: Section [$section] found empty" );
			}
		}
	}

	$self -> parse_config( config_tiny => $config_tiny );
}

sub add_file
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	push( @{$self->files}, file->new(%{$parm{'init_data'}}) );
}

sub _check_various
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		slave => { isa => 'Str', optional => 1 },
		master => { isa => 'Str', optional => 0 },
		header => { isa => 'Str', optional => 0 }
	);
	my $slave = $parm{'slave'};
	my $master = $parm{'master'};
	my $header = $parm{'header'};

	$self->relations({}) unless (defined $self->relations);
	if( defined $self -> $header ){
		# If header is specified.
		foreach my $parmcov ( keys %{$self -> $header } ){
			# Loop over parmcov settings.
			if( $parmcov =~ /^\s*(\*|\w+):(\*|\w+)-(\d+)\s*$/ ){
				# If left side has correct form. With state spec.
				my @bounds = @{$self -> $header -> {$parmcov}};
				my $parm = $1;
				my $cov = $2;
				my $state = $3;
				my %parmcov;

				if( $parm eq '*' ){
					if( $cov eq '*' ){
						if( defined $self -> test_relations ){
							foreach my $parameter (keys %{$self -> test_relations}){
								$parmcov{$parameter}=[];
							}
						}
					} else {

						foreach my $parameter( keys %{$self -> test_relations} ){
							if( defined $self -> test_relations -> {$parameter} ){
								foreach my $covariate( @{$self -> test_relations -> {$parameter}} ){
									if( $cov eq $covariate ){
										$parmcov{$parameter}=[];
									}
								}
							}
						}

					}
				} else {
					$parmcov{$parm}=[];
				}

				if( $cov eq '*' ){
					foreach my $parameter( keys %parmcov ){
						if( defined $self -> test_relations -> {$parameter} ){
							my @covs = @{$self -> test_relations -> {$parameter}};
							$parmcov{$parameter} = \@covs;
						}
					}
				} else {
					my @covs = ($cov);
					foreach my $parameter (keys %parmcov){
						$parmcov{$parameter} = \@covs;
					}
				}
				foreach my $parameter ( keys %parmcov ){
					next unless (defined $parmcov{$parameter});
					foreach my $covariate( @{$parmcov{$parameter}} ){
						if( ($parm eq '*' or $cov eq '*') ) {

							if( length( $slave ) > 0 ){
								unless( exists $self -> relations -> {$parameter} and 
									exists $self -> relations -> {$parameter} -> {$covariate} and 
									exists $self -> relations -> {$parameter} -> {$covariate} -> {$master} and 
									exists $self -> relations -> {$parameter} -> {$covariate} -> {$master} -> {$state} and 
									exists $self -> relations -> {$parameter} -> {$covariate} -> {$master} -> {$state} -> {$slave} ){
									@{$self -> relations -> {$parameter} -> {$covariate} -> {$master} -> {$state} -> {$slave}} = @bounds;
								}
							} else {
								unless( exists $self -> relations -> {$parameter} and 
									exists $self -> relations -> {$parameter} -> {$covariate} and 
									exists $self -> relations -> {$parameter} -> {$covariate} -> {$master} and 
									exists $self -> relations -> {$parameter} -> {$covariate} -> {$master} -> {$state} ){
									@{$self -> relations -> {$parameter} -> {$covariate} -> {$master} -> {$state}} = @bounds;
								}
							}
						} else {
							if( length( $slave ) > 0 ){
								@{$self -> relations -> {$parameter} -> {$covariate} -> {$master} -> {$state} -> {$slave}} = @bounds;
							} else {
								@{$self -> relations -> {$parameter} -> {$covariate} -> {$master} -> {$state}} = @bounds;
							}
						}

						# This is a hack to smack a linefeed at the end of code.
						if( $master eq 'code' ){
							if( defined $self -> relations -> {$parameter} -> {$covariate} -> {$master} -> {$state} ){
								my $code = @{$self -> relations -> {$parameter} -> {$covariate} -> {$master} -> {$state}}[0];
								@{$self -> relations -> {$parameter} -> {$covariate} -> {$master} -> {$state}} = split( /\n/, $code );
							}
						}
					}
				}

			} else {
				# If left side has wrong form. Die with help full message
				croak("Invalid left side: $parmcov . Format is PARAMETER:COV-STATE\n" );
			}
		}
	}
}

sub _check_included_relations
{
	my $self = shift;

	if( defined $self -> included_relations ){
		foreach my $parameter ( keys %{$self -> included_relations} ){
			my $new_parameter_hash;
			foreach(my $i; $i < scalar @{$self -> included_relations -> {$parameter}}; $i++ ){
				my $cov_state = @{$self -> included_relations -> {$parameter}}[$i];
				if( $cov_state =~ /^\s*(\w+)-(\d+)\s*$/ ){
					$new_parameter_hash -> {$1} -> {'state'} = $2;
				} else {
					# Default state value is 2. ( Linearly included )
					$new_parameter_hash -> {$cov_state} -> {'state'} = 2;
				}
			}
			delete $self -> included_relations -> {$parameter};
			%{$self -> included_relations -> {$parameter}} = %{$new_parameter_hash};
		}
	}      
}

sub write
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		filename => { isa => 'Str', optional => 0 }
	);
	my $filename = $parm{'filename'};

	open( CFG, '>', $filename ) 
		or croak("Failed to open file $filename for writing: $!" );

	my $contents = '';
	$contents .= "model=".$self->model."\n" if (defined $self->model);
	$contents .= "lst_file=".$self->lst_file."\n" if (defined $self->lst_file);

	foreach my $opt (keys %{$self -> valid_scalar_options}){
		next if ($opt eq 'model');
		next if ($opt eq 'lst_file');
		$contents .= "$opt=".$self->$opt."\n" if (defined $self->$opt);
	}
	$contents .= "\n";
	foreach my $opt (keys %{$self -> valid_array_options}){
		$contents .= "$opt=".join(',',@{$self->$opt})."\n" if (defined $self->$opt);
	}
	$contents .= "\n";

	foreach my $opt (keys %{$self -> valid_code_options}){
		if (defined $self->$opt){
			my $first=1;
			$contents .= "$opt={";
			foreach my $key (keys %{$self->$opt}){
				#separate with commas here
				$contents .= $key.'=>'.$self->$opt->{$key};
				$contents .= ',' unless ($first);
				$first=0;
			}
			$contents .= "}\n";
		}
	}

	foreach my $section (keys %{$self -> valid_hash_options}){
		if( defined $self -> $section) {
			if (ref( $self -> $section ) eq 'HASH') {
				$contents .= "\n[$section]\n";
				foreach my $val ( keys %{$self -> $section} ){
					if (defined $self -> $section->{$val}){
						if (scalar(@{$self->$section->{$val}}) ==1){
							$contents .= "$val=".$self->$section->{$val}->[0]."\n";
						}elsif ($section eq 'code'){
							$contents .= "$val=".$self->$section->{$val}->[0];
							for (my $i=1; $i< scalar(@{$self->$section->{$val}});$i++){
								$contents .= "\\\n".$self->$section->{$val}->[$i];
							}
							$contents .= "\n";
						}else{
							$contents .= "$val=".join(',',@{$self->$section->{$val}})."\n";
						}
					}
				}
			}
		}

	}
	print CFG $contents;
	close CFG;
}

sub parse_config
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		config_tiny => { isa => 'ext::Config::Tiny', optional => 1 }
	);
	my $config_tiny = $parm{'config_tiny'};

	foreach my $section ( keys %{$config_tiny} ) {
		if( $self -> valid_hash_options->{$section} eq 'ARRAY' ){
			foreach my $left_side( keys %{$config_tiny -> {$section}} ){
				my $right_side = $config_tiny -> {$section} -> {$left_side};

				if( $section eq 'code' ){
					$right_side =~ s/\\/\n/g;
				} else {
					$right_side =~ s/\\//g;
				}

				my @right_side_list;
				@right_side_list = split( /,/ , $right_side );

				my @left_side_list;
				@left_side_list = split( /,/ , $left_side );

				foreach my $left ( @left_side_list ){
					$self -> $section({}) unless (defined $self->$section());
					$self -> $section -> {$left} = [] unless (defined $self -> $section -> {$left});
					push(@{$self -> $section -> {$left}},@right_side_list);
				}
			}
		} elsif ( $self -> valid_hash_options->{$section} eq 'SCALAR' ) { 

			foreach my $left_side( keys %{$config_tiny -> {$section}} ){
				my $right_side = $config_tiny -> {$section} -> {$left_side};

				$right_side =~ s/\\//g;

				my @left_side_list;
				@left_side_list = split( /,/ , $left_side );

				foreach my $left ( @left_side_list ){
					$self -> $section -> {$left} = $right_side;
				}
			}

		} elsif( $section eq '_' ){
			foreach my $option ( keys %{$config_tiny -> { $section }} ){
				if( $self -> valid_scalar_options->{$option} ){

					my $value = $config_tiny -> { $section } -> {$option};

					if($option eq 'error_code'){
						#split on \ and convert to array
						my @arr = split(/\\/,$value);
						$config_tiny -> { $section } -> {$option} = \@arr;
					}elsif( $self -> valid_scalar_options->{$option} != 1 ){
						my $success = 0;
						foreach my $valid_values( split( /,/, ${$self -> valid_scalar_options->{$option}} ) ){
							if( $valid_values eq $value ){
								$success = 1;
								last;
							}
						}
						unless( $success ){
							croak("Invalid value for option $option: \"$value\". Valid values of $option is one of: " . ${$self -> valid_scalar_options->{$option}} );
						}
					}

					$self -> $option($config_tiny -> {$section} -> {$option});

				} elsif( $self -> valid_array_options-{$option} ) {
					my $value = $config_tiny -> { $section } -> {$option};
					$value =~ s/\s*//g;
					my @arr = split( /,/ , $value );
					$self -> $option(\@arr); 
				} elsif( $self -> valid_code_options->{$option} ){
					$self -> $option(eval $config_tiny -> {$section} -> {$option});
				} else {
					croak("Found invalid option: $option\n" );
				}
			}

		} else {
			croak("Found invalid section: $section" );
		}
	}

	#check no duplicates in covariate lists
	if(defined $self->categorical_covariates and defined $self->continuous_covariates){
		foreach my $cat (@{$self -> categorical_covariates}){
			foreach my $con (@{$self -> continuous_covariates}){
				croak("It is not allowed to specify $cat as both a ".
					"continuous and categorical covariate") if ($cat eq $con);
			}
		}
	}

	$self -> _check_included_relations;
	$self -> _check_various( header => 'upper_bounds', master => 'bounds', slave => 'upper' );
	$self -> _check_various( header => 'lower_bounds', master => 'bounds', slave => 'lower' );
	$self -> _check_various( header => 'code', master => 'code' );
	$self -> _check_various( header => 'inits', master => 'inits' );
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
