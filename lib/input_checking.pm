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
							  tool => {isa => 'Str', optional => 0},
							  model => {isa=> 'model',optional => 1},
		);
	my $options = $parm{'options'};
	my $rawres_input = $parm{'rawres_input'};
	my $copy_data = $parm{'copy_data'};
	my $tool = $parm{'tool'};
	my $model = $parm{'model'};

	my $error = '';

	if ($tool eq 'sir'){
		$rawres_input = 1;
		$copy_data = 1;
		$error .= check_sir(options => $options, model => $model);
	}

	if ($rawres_input){
		$error .= check_rawres_input(options => $options);
	}
	if ($copy_data and (defined $model)){
		$error .= check_copy_data(model => $model, options => \%options);
	}

	if (length($error)> 0){
		ui->print(category=> 'all',message => "\n"."Input error:\n".$error);
		die;
	}
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




1;
