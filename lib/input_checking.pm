package input_checking;

use include_modules;
use Cwd;
use OSspecific;
use PsN;
use ui;
use model;
use MooseX::Params::Validate;

sub check_options
{
	my %parm = validated_hash(\@_,
							  options => {isa => 'HashRef', optional => 0},
							  rawres_input => {isa => 'Bool', default => 0},
							  copy_data => {isa => 'Bool', default => 0},
							  msfi => {isa => 'Bool', default => 0},
							  single_prob_or_tnpri => {isa => 'Bool', default => 0},
							  tool => {isa => 'Str', optional => 0},
							  model => {isa=> 'model',optional => 1},
		);
	my $options = $parm{'options'};
	my $rawres_input = $parm{'rawres_input'};
	my $copy_data = $parm{'copy_data'};
	my $tool = $parm{'tool'};
	my $model = $parm{'model'};
	my $msfi = $parm{'msfi'};
	my $single_prob_or_tnpri = $parm{'single_prob_or_tnpri'};

	my $require_est = 0;
	my $error = '';

	if ($tool eq 'frem'){
		$msfi = 1;
		$error .= check_frem(options => $options, model => $model);
	}

	if ($tool eq 'sir'){
		$rawres_input = 1;
		$copy_data = 1;
		$error .= check_sir(options => $options, model => $model);
	}

	if ($tool eq 'simeval'){
		$msfi = 1;
		$single_prob_or_tnpri = 1;
		$require_est = 1;
		$error .= check_simeval(options => $options, model => $model);
	}
	
	if ($rawres_input){
		$error .= check_rawres_input(options => $options);
	}
	if ($copy_data and (defined $model)){
		$error .= check_copy_data(model => $model, options => $options);
	}
	if ($msfi and (defined $model)){
		$error .= check_msfi(model => $model, options => $options);
	}
	if ($single_prob_or_tnpri and (defined $model)){
		#check_msfi must be run before check tnpri
		$error .= check_single_prob_or_tnpri(model => $model, options => $options, require_est => $require_est);
	}

	if (length($error)> 0){
		ui->print(category=> 'all',message => "\n"."Input error:\n".$error);
		die;
	}
}

sub check_single_prob_or_tnpri
{
	my %parm = validated_hash(\@_,
							  options => {isa => 'HashRef', optional => 0},
							  model =>  {isa => 'model', optional => 0},
							  require_est => {isa => 'Bool', optional => 0},
		);
	my $options = $parm{'options'};
	my $model = $parm{'model'};
	my $require_est = $parm{'require_est'};
	
	my $error = '';
	my $tnpri=0;
	if ( scalar (@{$model-> problems}) > 2 ){
		$error .= "Cannot have more than two $PROB in the input model.n";
	}elsif  (scalar (@{$model-> problems}) == 2 ){
		if ((defined $model-> problems->[0]->priors()) and 
			scalar(@{$model-> problems->[0] -> priors()})>0 ){
			foreach my $rec (@{$model-> problems->[0] -> priors()}){
				foreach my $option ( @{$rec -> options} ) {
					if ((defined $option) and 
						(($option->name eq 'TNPRI') || (index('TNPRI',$option ->name ) == 0))){
						$tnpri=1;
					}
				}
			}
		}
		if ($tnpri){
			unless( defined $model-> extra_files ){
				$error .= 'When using $PRIOR TNPRI you must set option -extra_files to '.
					'the msf-file, otherwise the msf-file will not be copied to the NONMEM '.
					'run directory.'."\n";
			}
		}else{
			$error .= 'The input model must contain exactly one problem, unless'.
				' first $PROB has $PRIOR TNPRI'."\n";
		}
	}
	if ($require_est){
		my $est_record = $model -> record( problem_number => (1+$tnpri),
										   record_name => 'estimation' );
		unless( scalar(@{$est_record}) > 0 ){
			$error .=  "The input model must have a \$EST record\n";
		}
	}
	return $error;
}

sub check_msfi
{
	my %parm = validated_hash(\@_,
							  options => {isa => 'HashRef', optional => 0},
							  model =>  {isa => 'model', optional => 0},
		);
	my $options = $parm{'options'};
	my $model = $parm{'model'};

	my $error = '';
	#this just adds msfi to extra input files, does not check msfi file exists
	if( defined $model -> msfi_names() ){
		my @needed_files;
		foreach my $msfi_files( @{$model -> msfi_names()} ){
			#loop $PROB
			if (defined $msfi_files){
				foreach my $msfi_file( @{$msfi_files} ){
					#loop instances
					if ( defined $msfi_file ){
						my ( $dir, $file ) = OSspecific::absolute_path(cwd(),$msfi_file);
						push (@needed_files,$dir.$file) ;
					}
				}
			}
		}
		if (scalar(@needed_files)>0){
			if( defined $model -> extra_files ){
				push(@{$model -> extra_files},@needed_files);
			}else{
				$model -> extra_files(\@needed_files);
			}
		}
	}
	return $error;
}

sub check_copy_data
{
	my %parm = validated_hash(\@_,
							  options => {isa => 'HashRef', optional => 0},
							  model =>  {isa => 'model', optional => 0},
		);
	my $options = $parm{'options'};
	my $model = $parm{'model'};

	my $error = '';

	unless (defined $options->{'copy_data'} and (not $options->{'copy_data'})) {
		$options->{'copy_data'} = 1;
	}
	
	unless ($model->is_dummy){
		unless ($model->copy_data_setting_ok(copy_data => $options->{'copy_data'})){
			$error .= "Cannot set -no-copy_data, absolute data file path is too long.\n";
		} 
		if (defined $options->{'copy_data'} and (not $options->{'copy_data'})){
			$model->relative_data_path(0);
		}
	}
	return $error;
}

sub check_rawres_input
{
	my %parm = validated_hash(\@_,
							  options => {isa => 'HashRef', optional => 0},
		);
	my $options = $parm{'options'};

	my $error = '';

	if ( defined $options->{'rawres_input'} ){
		if (-e $options->{'rawres_input'}){
			my ( $dir, $file ) = OSspecific::absolute_path(cwd(), $options->{'rawres_input'});
			$options->{'rawres_input'} = $dir . $file;
		}else{
			$error .=  "The rawres_input file ".$options->{'rawres_input'}." does not exist.\n"; 
		}
	}
	my @in_filter=();
	if ( defined $options->{'in_filter'} ){
		unless (defined $options->{'rawres_input'}){
			$error .=  "Cannot use option -in_filter without option -rawres_input.\n";
		}
		#split string, assume comma separated
		foreach my $filt (split(/,/,$options->{'in_filter'})){
			if ($filt =~ /.\.(gt|lt|eq)\.\d+\.?\d*/){
				push(@in_filter,$filt);
			}else{
				$error .= "Input filter $filt does not fulfill the format rules.\n";
			}
		}
		if (scalar(@in_filter)<1){
			$error .=  "Option in_filter used, but list of conditions could not be parsed.\n";
		}	
	}
	$options->{'in_filter'} = \@in_filter;

	return $error;
}

sub check_sir
{
	my %parm = validated_hash(\@_,
							  options => {isa => 'HashRef', optional => 0},
							  model =>  {isa => 'model', optional => 0},
		);
	my $options = $parm{'options'};
	my $model = $parm{'model'};

	my $error = '';

	if ( defined $options->{'add_iterations'} and ($options->{'add_iterations'}) ){
		unless ((defined $options->{'directory'}) && (-d $options->{'directory'})) {
			$error .= "Cannot set option -add_iterations unless -directory is set to existing sir run directory\n";
		}
		unless ((defined $options->{'samples'}) && (defined $options->{'resamples'})) {
			$error .=  "Options -samples and -resamples are required in combination with add_iterations\n";
		}
	}

	if ( defined $options->{'covmat_input'} ){
		foreach my $opt ('rawres_input','cv_theta','auto_rawres'){
			if (defined $options->{$opt}){
				$error .=  "Cannot use option $opt together with option -covmat_input.\n";
			}
		}
		#set to global filepath here
		if ($options->{'covmat_input'} eq 'identity'){
			#special
		}else{
			unless (-e $options->{'covmat_input'}){
				$error .=  "The covmat_input file ".$options->{'covmat_input'}." does not exist.\n"; 
			}
			my ( $dir, $file ) = OSspecific::absolute_path(cwd(), $options->{'covmat_input'});
			$options->{'covmat_input'} = $dir . $file;
		}
	}

	if (defined $options->{'cv_theta'}){
		foreach my $opt ('covmat_input','rawres_input','auto_rawres','theta_inflation','omega_inflation','sigma_inflation'){
			if (defined $options->{$opt}){
				$error .=  "Cannot use option $opt together with option -cv_theta.\n";
			}
		}
		foreach my $param ('omega','sigma'){
			my $opt = 'cv_'.$param;
			if (not defined $options->{$opt}){
				my $coords = $model->problems->[0]->get_estimated_attributes(parameter => $param,
																			 attribute => 'coords');
				if ($options->{'cv_theta'} =~ /,/){
					#not a scalar
					if (scalar(@{$coords})> 0){
						#any $param estimated
						$error .= "Must also set option $opt when -cv_theta is set to a list of values\n";
					}else{
						#no param estimated. Set to empty string
						$options->{$opt}='';
					}
				}else{
					#scalar (no comma)
					$options->{$opt}=$options->{'cv_theta'};
				}
			}
		}
	}else{
		foreach my $opt ('cv_omega','cv_sigma'){
			if (defined $options->{$opt}){
				my $coords = $model->problems->[0]->get_estimated_attributes(parameter => 'theta',
																			 attribute => 'coords');
				if (scalar(@{$coords})> 0){
					$error .=  "Cannot use option $opt without -cv_theta.\n";
				}else{
					#no theta estimated. Set to empty string
					$options->{'cv_theta'}='';
				}
			}
		}
	}

	my @samples =();
	if (defined $options->{'samples'}){
		@samples = split(/,/,$options->{'samples'}) ;
	}else{
		@samples=(1000,1000,1000,2000,2000);
	}
	$options->{'samples'} = \@samples;

	my @resamples = ();
	if (defined $options->{'resamples'}){
		@resamples = split(/,/,$options->{'resamples'});
	}else{
		@resamples = (200,400,500,1000,1000);
	}
	$options->{'resamples'} = \@resamples;

	return $error;
}

sub check_simeval
{
	my %parm = validated_hash(\@_,
							  options => {isa => 'HashRef', optional => 0},
							  model =>  {isa => 'model', optional => 0},
		);
	my $options = $parm{'options'};
	my $model = $parm{'model'};
	
	my $error = '';
	if (defined $options->{'samples'}) {
		unless ($options->{'samples'} > 0){
			$error .= "samples must be larger than 0\n";
		}
	}else{
		$options->{'samples'}=300;
	}

	if (defined $options->{'n_simulation_models'}){
		unless ($options->{'n_simulation_models'} > 0){
			$error .= "n_simulation_models must be larger than 0\n";
		}
		if ($options->{'n_simulation_models'} > $options->{'samples'}){
			$options->{'n_simulation_models'} = $options->{'samples'};
		}
	}else{
		if (defined $options->{'threads'} and ($options->{'threads'}>0) ){
			$options->{'n_simulation_models'} = $options->{'threads'};
		}else{
			$options->{'n_simulation_models'} = 1;
		}
	}

	#here we have not yet checked that single prob or msfi. just check last $PROB
	my $prob = scalar(@{$model->problems})-1;
	my $meth = $model->get_option_value( record_name  => 'estimation',
										 problem_index => $prob,
										 option_name  => 'METHOD',
										 option_index => 0);
	if (not (defined $meth) or ($meth eq '0') or ($meth =~ /^ZE/)){
		$error .= 'Cannot run simeval if METHOD=0, all ETAs will be 0'."\n";
	}

	return $error;
}


sub check_frem
{
	my %parm = validated_hash(\@_,
							  options => {isa => 'HashRef', optional => 0},
							  model =>  {isa => 'model', optional => 0},
		);
	my $options = $parm{'options'};
	my $model = $parm{'model'};

	my $error = '';


	my @covariates =();
	if (defined $options->{'covariates'}){
		@covariates = split(/,/,$options->{'covariates'}) ;
	}else{
		$error .= 'Option -covariates is required'."\n";
	}
	$options->{'covariates'} = \@covariates;

	my @categorical = ();
	if (defined $options->{'categorical'}){
		@categorical = split(/,/,$options->{'categorical'});
	}
	$options->{'categorical'} = \@categorical;

	my @log = ();
	if (defined $options->{'log'}){
		@log = split(/,/,$options->{'log'});
	}
	$options->{'log'} = \@log;

	if ($model->tbs){
		$error.= "frem is incompatible with option -tbs.\n";
	}
	
	if ( scalar (@{$model-> problems}) > 1 ){
		$error .= "Cannot have more than one \$PROB in the input model.\n";
	}

	my $est_record = $model -> record( problem_number => 1,
									   record_name => 'estimation' );
	unless( scalar(@{$est_record}) > 0 ){
		$error .=  "The input model must have a \$EST record.\n";
	}
	
	return $error;
}


1;
