package tool::frem;

use include_modules;
use tool::modelfit;
use Math::Random;
use Data::Dumper;
use Config;
use linear_algebra;
use ui;
use File::Copy qw/cp mv/;
use File::Path 'rmtree';
use nmtablefile;


use Moose;
use MooseX::Params::Validate;

extends 'tool';


my $fremtype = 'FREMTYPE'; 
my $smallcorrelation = 0.01; #FIXME
my $bov_variance_init = 0.1; #FIXME

has 'skip_etas' => ( is => 'rw', isa => 'Int', default=> 0);
has 'done_file' => ( is => 'rw', isa => 'Str', default => 'template_models_done.pl');
has 'estimate' => ( is => 'rw', isa => 'Int', default => 3 );
has 'occasionlist' => ( is => 'rw', isa => 'ArrayRef[Int]', default => sub { [] } );
has 'extra_input_items' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has 'invariant_median' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'timevar_median' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'invariant_covmatrix' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'timevar_covmatrix' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'check' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'vpc' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'dv' => ( is => 'rw', isa => 'Str', default => 'DV' );
has 'occasion' => ( is => 'rw', isa => 'Str', default => 'OCC' );
has 'parameters_bov' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'time_varying' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'invariant' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef', default => sub { ['frem.log'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'frem_results.csv' );
has 'use_pred' => ( is => 'rw', isa => 'Bool', default => 1 );


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

	foreach my $model ( @{$self -> models} ) {
		foreach my $problem (@{$model->problems()}){
			if (defined $problem->nwpri_ntheta()){
				ui -> print( category => 'all',
					message => "Warning: frem does not support \$PRIOR NWPRI.",
					newline => 1);
				last;
			}
		}
	}

	if ( scalar (@{$self -> models->[0]-> problems}) > 1 ){
		croak('Cannot have more than one $PROB in the input model.');
	}

	#checks left for frem->new:
#checks left for frem->new: occ and dv exist in $INPUT if needed. type exists in $INPUT if given. 
	#if bov_parameters is > 0 then must have occasion in $input
	#must have dv in $input

	my $occ_ok=1;
	my $dv_ok=0;

	if (scalar(@{$self->parameters_bov()})>0){
		$occ_ok=0;
	}

	my $prob = $self -> models->[0]-> problems -> [0];
	if (defined $prob->priors()){
		croak("frem does not support \$PRIOR");
	}

	if( defined $prob -> inputs and defined $prob -> inputs -> [0] -> options ) {
		foreach my $option ( @{$prob -> inputs -> [0] -> options} ) {
			unless (($option -> value eq 'DROP' or $option -> value eq 'SKIP'
						or $option -> name eq 'DROP' or $option -> name eq 'SKIP')){
				$dv_ok = 1 if ($option -> name() eq $self->dv()); 
#				$type_ok = 1 if ($option -> name() eq $self->type()); 
				$occ_ok = 1 if ($option -> name() eq $self->occasion()); 
			}
		}
#		croak("type column ".$self->type()." not found in \$INPUT" ) unless $type_ok;
		croak("dependent column ".$self->dv()." not found in \$INPUT" ) unless $dv_ok;
		croak("occasion column ".$self->occasion()." not found in \$INPUT" ) unless $occ_ok;
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
	
}

sub get_filled_omega_block
{
	#must have already done update inits on model so that get_matrix is estimated values, where available
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  phi_file  => { isa => 'Maybe[Str]', optional => 1 },
							  problem_index  => { isa => 'Int', default => 0 },
							  table_index  => { isa => 'Int', default => 0 },
							  start_eta_1 => { isa => 'Int', optional => 0 },
							  end_eta_1 => { isa => 'Int', optional => 0 },
							  start_eta_2 => { isa => 'Int', optional => 1 },
							  end_eta_2 => { isa => 'Int', optional => 1 },
	);
	my $model = $parm{'model'};
	my $phi_file = $parm{'phi_file'};
	my $problem_index = $parm{'problem_index'};
	my $table_index = $parm{'table_index'};
	my $start_eta_1 = $parm{'start_eta_1'};
	my $end_eta_1 = $parm{'end_eta_1'};
	my $start_eta_2 = $parm{'start_eta_2'};
	my $end_eta_2 = $parm{'end_eta_2'};

	my $error = 0;
	my $message = '';
	my $corrmatrix;
	my $size1 = $end_eta_1-$start_eta_1+1;
	my $size2 = 0;
	$size2 = $end_eta_2-$start_eta_2+1 if (defined $start_eta_2 and defined $end_eta_2);
	my @sd = ();
	my @mergematrix = ();

	unless (defined $phi_file){
		$phi_file = $model->outputs->[0]->problems->[$problem_index]->full_name_NM7_file(file_type => 'phi');
	}
	
	#local coords
	($corrmatrix,$message) = get_correlation_matrix_from_phi(start_eta_1 => $start_eta_1,
															 end_eta_1 => $end_eta_1,
															 start_eta_2 => $start_eta_2,
															 end_eta_2 => $end_eta_2,
															 table_index => $table_index,
															 phi_file => $phi_file);
	return([],$message) unless (length($message) == 0);

	@sd = (0) x ($size1+$size2);
	for (my $i=0; $i<($size1+$size2); $i++){
		push(@mergematrix,[(0) x ($size1+$size2)]);
	}
	
	#omega block. Do not assume all that are nonzero are estimated
	#get inits from model. local coords
	my $init_matrix_1 = $model->problems->[$problem_index]->get_matrix(type => 'omega',
																	   start_row => $start_eta_1,
																	   end_row => $end_eta_1);

	for (my $i=0; $i<$size1; $i++){
		for (my $j=0; $j<$size1; $j++){
			$mergematrix[$i]->[$j] = $init_matrix_1->[$i][$j];
		}
		$sd[$i] = sqrt($init_matrix_1->[$i][$i]) if ($init_matrix_1->[$i][$i] > 0);
	}

	if (defined $start_eta_2 and defined $end_eta_2){
		my $init_matrix_2 = $model->problems->[$problem_index]->get_matrix(type => 'omega',
																		   start_row => $start_eta_2,
																		   end_row => $end_eta_2);
		for (my $i=0; $i<$size2; $i++){
			for (my $j=0; $j<$size2; $j++){
				$mergematrix[$size1+$i]->[$size1+$j] = $init_matrix_2->[$i][$j];
			}
			$sd[$size1+$i] = sqrt($init_matrix_2->[$i][$i]) if ($init_matrix_2->[$i][$i] > 0);
		}
	}

	#now we have sd and valuematrix that are inits/estimates or 0.
	#for each value in mergematrix that is still 0, compute covar using correlation and sd

	for (my $i = 0; $i < ($size1+$size2); $i++){
		for (my $j = 0; $j < $i; $j++){
			#copy
			$mergematrix[$i]->[$j] = $mergematrix[$j]->[$i]; 
		}
		for (my $j = ($i+1); $j < ($size1+$size2); $j++){
			next unless ($mergematrix[$i]->[$j] == 0);
			#compute new
			$mergematrix[$i]->[$j] = ($corrmatrix->[$i][$j])*($sd[$i])*($sd[$j]);
		}
	}

	my ($posdefmatrix,$diff)=linear_algebra::get_symmetric_posdef(\@mergematrix);
	
	return($posdefmatrix,'');	
}

sub get_correlation_matrix_from_phi
{
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 1 },
							  phi_file  => { isa => 'Maybe[Str]', optional => 1 },
							  problem_index  => { isa => 'Int', default => 0 },
							  table_index  => { isa => 'Int', default => 0 },
							  start_eta_1 => { isa => 'Int', optional => 0 },
							  end_eta_1 => { isa => 'Int', optional => 0 },
							  start_eta_2 => { isa => 'Maybe[Int]', optional => 1 },
							  end_eta_2 => { isa => 'Maybe[Int]', optional => 1 },
	);
	my $model = $parm{'model'};
	my $phi_file = $parm{'phi_file'};
	my $problem_index = $parm{'problem_index'};
	my $table_index = $parm{'table_index'};
	my $start_eta_1 = $parm{'start_eta_1'};
	my $end_eta_1 = $parm{'end_eta_1'};
	my $start_eta_2 = $parm{'start_eta_2'};
	my $end_eta_2 = $parm{'end_eta_2'};

	my $error = 0;
	my $message = '';

	unless (defined $phi_file){
		if (defined $model){
			$phi_file = $model->outputs->[0]->problems->[$problem_index]->full_name_NM7_file(file_type => 'phi');
		}else{
			$error = 2;
			$message .= 'Either model or phi_file name is required ';
		}
	}

	unless (length($phi_file)> 0){
		$error = 2;
		$message .= 'Empty phi file name';
	}
	unless (-e $phi_file){
		$error = 2;
		$message .= ' File '.$phi_file.' does not exist';
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
	
	my $nmtablefile = nmtablefile->new(filename => $phi_file);
	my @matrix = ();
	my $covariance = [];
	my $sdcorr = [];
	
	for (my $i = $start_eta_1; $i <= $end_eta_1; $i++){
		push(@matrix,$nmtablefile->tables->[$table_index]->get_column(name=> 'ETA('.$i.')'));
	}
	if (defined $start_eta_2 and defined $end_eta_2){
		for (my $i = $start_eta_2; $i <= $end_eta_2; $i++){
			push(@matrix,$nmtablefile->tables->[$table_index]->get_column(name=> 'ETA('.$i.')'));
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
		if ($error == 1){
			$message = 'Numerical error covar2sdcorr';
		}else{
			$message = 'Input error covar2sdcorr';
		}
		return([],$message);
	}

	return ($sdcorr,'');
}

sub get_CTV_parameters
{
	#find union of input bov_parameters and additional TVpars that have ETA on them in input model
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  bov_parameters => { isa => 'ArrayRef', optional => 0 },
	);
	my $model = $parm{'model'};
	my $bov_parameters = $parm{'bov_parameters'};

	my %etanum_to_parameter=();

	#find bov_parameters that have ETAs on them already
	#search in Model 0 because there we have not added BOV ETAS
	my @code = @{$model->get_code(record => 'pk')};
	my $use_pred = 0;
	unless ($#code > 0) {
		@code = @{$model->get_code(record => 'pred')};
		$use_pred = 1;
	}
	if ( $#code <= 0 ) {
		croak("Neither PK or PRED defined in model 0");
	}
	
	my %CTV_par;
	foreach my $p (@{$bov_parameters}){
		$CTV_par{$p}=1;
	}
	
	my @TVPAR=();
	#find all TV par on left hand side. Allow IF lines
	for (my $i=0; $i<scalar(@code); $i++) {
		next if ( $code[$i] =~ /^\s*\;/); #comment line
		if ( $code[$i] =~ /^[^;]*\bTV(\w+)\s*=/  ){
			push(@TVPAR,$1);
		}
	}
	#find all par = lines and check if have ETA, then add to CTV hash. Allow IF lines
	foreach my $par (@TVPAR){
#		next if (defined $CTV_par{$par}); #we already know this has BOV ETA but we want to get etanum mapping
		for (my $i=0; $i<scalar(@code); $i++) {
			next if ( $code[$i] =~ /^\s*\;/); #comment line
			if ( $code[$i] =~ /^[^;]*\b$par\s*=.*\bETA\((\d+)\)/  ){
				my $num = $1;
				$etanum_to_parameter{$1}=$par;
				$CTV_par{$par} =1;
				last;
			}
		}
	}
	my @CTV_parameters = sort (keys %CTV_par);


	return (\@CTV_parameters,\%etanum_to_parameter);
}


sub create_labels{
	my %parm = validated_hash(\@_,
							  invariant => { isa => 'ArrayRef', optional => 0 },
							  bov_parameters => { isa => 'ArrayRef', optional => 0 },
							  time_varying => { isa => 'ArrayRef', optional => 0 },
							  etanum_to_parameter => { isa => 'HashRef', optional => 0 },
							  occasionlist => { isa => 'ArrayRef', optional => 0 },
							  occasion => { isa => 'Str', optional => 0 },
							  start_eta => { isa => 'Int', optional => 0 },
							  bsv_parameters => { isa => 'Int', optional => 0 },
		);
	my $invariant = $parm{'invariant'};
	my $bov_parameters = $parm{'bov_parameters'};
	my $time_varying = $parm{'time_varying'};
	my $etanum_to_parameter = $parm{'etanum_to_parameter'};
	my $occasionlist = $parm{'occasionlist'};
	my $occasion = $parm{'occasion'};
	my $start_eta = $parm{'start_eta'};
	my $bsv_parameters = $parm{'bsv_parameters'};


	my @occasion_labels;
	my @bsv_par_labels;
	my @bsv_cov_labels;
	my @bov_par_labels;
	my @bov_cov_labels;

	for (my $i=0; $i< scalar(@{$occasionlist}); $i++){
		push(@occasion_labels,$occasion.'='.$occasionlist->[$i]);
	}

	if (scalar(@{$invariant}) > 0){
		for (my $i=$start_eta;$i< ($start_eta + $bsv_parameters);$i++){
			if (defined $etanum_to_parameter->{$i}){
				push(@bsv_par_labels,'BSV par '.$etanum_to_parameter->{$i});
			}else{
				push(@bsv_par_labels,'BSV par ');
			}
		}
	}

	for (my $i=0; $i< scalar(@{$invariant}); $i++){
		push(@bsv_cov_labels,'BSV cov '.$invariant->[$i]);
	}

	for (my $i=0; $i< scalar(@{$bov_parameters}); $i++){
		push(@bov_par_labels,'BOV par '.$bov_parameters->[$i]);
	}

	for (my $i=0; $i< scalar(@{$time_varying}); $i++){
		push(@bov_cov_labels,'BOV cov '.$time_varying->[$i]);
	}

	my %hash;
	$hash{'occasion_labels'}=\@occasion_labels;
	$hash{'bsv_par_labels'}=\@bsv_par_labels;
	$hash{'bsv_cov_labels'}=\@bsv_cov_labels;
	$hash{'bov_par_labels'}=\@bov_par_labels;
	$hash{'bov_cov_labels'}=\@bov_cov_labels;

	return \%hash;

}

sub replace_tvpar_with_ctvpar
{
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  ctvpar => { isa => 'ArrayRef', optional => 0 },
		);
	my $model = $parm{'model'};
	my $ctvpar = $parm{'ctvpar'};


	#replace TVpar with CTVpar
	my @code;
	@code = @{$model->get_code(record => 'pk')};
	my $use_pred = 0;
	unless ( $#code > 0 ) {
		@code = @{$model->get_code(record => 'pred')};
		$use_pred = 1;
	}
	if ( $#code <= 0 ) {
		croak("Neither PK or PRED defined in vpc model 1");
	}

	foreach my $par (@{$ctvpar}){
		my $ctv = 'CTV'.$par;
		my $tv = 'TV'.$par;
		my $found = 0;
		for (my $i=0; $i<scalar(@code); $i++) {
			next if ( $code[$i] =~ /^\s*\;/); #comment line
			if ( $code[$i] =~ /^([^;]*)\b$tv\s*=/ ){
				$code[$i] = $1.$tv.'='.$ctv;
				$found = 1;
				#do not break here, may be multiple definitions, replace all
			}
		}
		croak("could not find where to set\n".$tv.'='.$ctv) unless $found;
	}
	if ($use_pred) {
		$model->set_code(record => 'pred', code => \@code);
	} else {
		$model->set_code(record => 'pk', code => \@code);
	}
	
}

sub create_full_block
{
	my %parm = validated_hash(\@_,
							  top_block => { isa => 'ArrayRef', optional => 0 },
							  bottom_block => { isa => 'ArrayRef', optional => 0 },
							  correlation => { isa => 'Num', optional => 0 },
		);
	my $top_block = $parm{'top_block'};
	my $bottom_block = $parm{'bottom_block'};
	my $correlation = $parm{'correlation'};

	my $dim1 = scalar(@{$top_block});
	my $new_size = $dim1+scalar(@{$bottom_block});
	my $full_block=[];
	for (my $i=0 ; $i< $new_size; $i++){
		push(@{$full_block},[(0) x $new_size]);
	}
	for (my $i=0 ; $i< $dim1; $i++){
		for (my $j=0 ; $j<=$i; $j++){
			$full_block->[$i][$j] = $top_block->[$i][$j];
		}
	}
	for (my $i=$dim1; $i< $new_size; $i++){
		for (my $j=$dim1 ; $j<=$i; $j++){
			$full_block->[$i][$j] = $bottom_block->[$i-$dim1][$j-$dim1];
		}
		for (my $j=0 ; $j<$dim1; $j++){
			$full_block->[$i][$j] = $correlation*sqrt($full_block->[$i][$i])*sqrt($full_block->[$j][$j]);
		}
	}
	for (my $i=0 ; $i< $new_size; $i++){
		for (my $j=($i+1) ; $j<$new_size; $j++){
			$full_block->[$i][$j] = $full_block->[$j][$i];
		}
	}

	return $full_block;
}

sub get_start_numbers
{
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  n_invariant => {isa => 'Int', optional => 0},
							  skip_etas => {isa => 'Int', optional => 0},
	);
	my $model = $parm{'model'};
	my $n_invariant = $parm{'n_invariant'};
	my $skip_etas = $parm{'skip_etas'};

	my $ref = $model->problems->[0]->get_eta_sets(header_strings => 0,
												  skip_etas =>$skip_etas);
#	$self->iiv_eta($ref->{'iiv'});
	#	$self->iov_eta($ref->{'iov'});

	my @bsv_eta = @{$ref->{'iiv'}};
	
	#FIXME check iov etas, if any, are larger than all bsv
	my $occasions = scalar(@{$ref->{'iov'}});
	if (($occasions > 0) and (scalar(@bsv_eta)>0)){
		foreach my $bov (@{$ref->{'iov'}->[0]}){
			croak("all pre-included BOV ETA must come after all BSV ETA") 
				if ($bov < $bsv_eta[-1]); 
		}
	}
	my $bsv_parameter_count=scalar(@bsv_eta);
	my $start_omega_record = $model-> problems -> [0]->check_skip_etas(skip_etas => $skip_etas);
	my $total_bsv_etas = $bsv_parameter_count+$n_invariant;

	return($total_bsv_etas,$bsv_parameter_count,$start_omega_record,$ref->{'iov'});
}

sub check_input_bov{
	my %parm = validated_hash(\@_,
							  input_bov_parameters => { isa => 'ArrayRef', optional => 0 },
							  parameters_bov => { isa => 'ArrayRef', optional => 0 },
	);
	my $input_bov_parameters = $parm{'input_bov_parameters'};
	my $parameters_bov = $parm{'parameters_bov'};

	foreach my $input (@{$input_bov_parameters}){
		my $found = 0;
		foreach my $par (@{$parameters_bov}){
			if ($par eq $input){
				$found = 1;
				last;
			}
		}
		croak("input model has BOV on $input, but this parameter is not listed in -parameters_bov") 
			unless ($found);
	}
	
}
sub get_parameters_to_etas{
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  use_pred => { isa => 'Bool', optional => 0 },
							  etas => { isa => 'ArrayRef', optional => 0 },
	);
	my $model = $parm{'model'};
	my $use_pred = $parm{'use_pred'};
	my $etas = $parm{'etas'};

	my @parameters = ();
	my $code;
	if ($use_pred){
		$code = $model->get_code(record => 'pred');
	}else{
		$code = $model->get_code(record => 'pk');
	}
	croak ("no code ") unless (scalar(@{$code})>0);
	foreach my $eta (@{$etas}){
		my $found = 0;
		foreach my $line (@{$code}){
			if ($line =~ /[^;]*\bETA\($eta\)/){
				if ($line =~ /^\s*(\S+)\s*=/){
					push(@parameters,$1);
					$found = 1;
				}
			}
			last if ($found);
		}
		unless ($found){
			croak("Could not find parameter coupled to ETA($eta)");
		}
	}
	return \@parameters;
}



sub do_model0
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model => { isa => 'Ref', optional => 0 }
	);
	my $model = $parm{'model'};

	my $name_model = 'model_0.mod';
	my $output;
	my $frem_model;

	if (-e $self -> directory().'m1/'.$name_model){
		$frem_model = model->new( %{common_options::restore_options(@common_options::model_options)},
								  filename                    => 'm1/'.$name_model,
								  ignore_missing_output_files => 1 );
	}else{
		$frem_model = $model ->  copy( filename    => $self -> directory().'m1/'.$name_model,
									   output_same_directory => 1,
									   write_copy => 1,
									   copy_datafile   => 1,
									   copy_output => 0);
	} 

	if ($frem_model -> is_run() and (defined $frem_model->outputs->[0] ) and 
		(-e $frem_model->outputs->[0]->problems->[0]->full_name_NM7_file(file_type => 'phi'))
		) {
		#no need to run again
		$output = $frem_model->outputs->[0];
	}elsif ($model -> is_run() and (defined $model->outputs->[0] ) and 
			(-e $model->outputs->[0]->problems->[0]->full_name_NM7_file(file_type => 'phi'))
		) {
		#no need to run anything
		$output = $model->outputs->[0];
	}else{
		#run it
		my $rundir = $self -> directory().'/model0_modelfit_dir1';
		rmtree([ "$rundir" ]) if (-e $rundir);
		my $run = tool::modelfit ->new( %{common_options::restore_options(@common_options::tool_options)},
										base_directory	 => $self -> directory(),
										directory		 => $rundir,
										copy_data     => 0,
										models		 => [$frem_model],
										top_tool              => 0);
		tool::add_to_nmoutput(run => $run, extensions => ['phi','ext']);		
		ui -> print( category => 'all', message =>  'Estimating Model 0 (the input model)');
		$run-> run;
		if (defined $frem_model->outputs and (defined $frem_model->outputs->[0])){
			$output = $frem_model->outputs->[0] ;
		}
	}

	unless (defined $output){
		croak("No output from Model 0, cannot proceed with frem");
	}
	
	$frem_model->update_inits (from_output => $output);
	my $mod0_ofv = $output->get_single_value(attribute=> 'ofv');
	my $phi_file = $output->problems->[0]->full_name_NM7_file(file_type => 'phi');
	
	return ($mod0_ofv, $phi_file, $frem_model); 

}

sub do_frem_dataset
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model => { isa => 'Ref', optional => 0 },
							  mod0_ofv => { isa => 'Num', optional => 0 },
							  fremdataname => { isa => 'Str', optional => 0 },
	);
	my $model = $parm{'model'};
	my $mod0_ofv = $parm{'mod0_ofv'};
	my $fremdataname = $parm{'fremdataname'};
	
	my $filtered_datafile = 'filtered_plus_type0.dta';


	my $do_check = $self->check;
	if (-e $self -> directory().'m1/'.$fremdataname){
		unlink($self -> directory().'m1/'.$fremdataname);
		$do_check = 0; #assume get same result second time
	}
	
	my ($filtered_data_model,$indices,$first_timevar_type,$extra_input_items,$message) = 
		create_data2_model(model=>$model,
						   filename => $self -> directory().'m1/filter_data_model.mod',
						   filtered_datafile => $filtered_datafile,
						   bov_parameters => scalar(@{$self->parameters_bov()}),
						   dv => $self->dv,
						   invariant => $self->invariant,
						   time_varying => $self->time_varying,
						   occasion => $self->occasion);
	
	$self->extra_input_items($extra_input_items);
	
	unless (-e $self -> directory().'m1/'.$filtered_datafile){
		$filtered_data_model -> _write();
		my $rundir = $self -> directory().'/create_fremdata_dir';
		rmtree([ "$rundir" ]) if (-e $rundir);
		my $filter_fit = tool::modelfit -> new
			( %{common_options::restore_options(@common_options::tool_options)},
			  base_directory => $self->directory,
			  directory      => $rundir,
			  models         => [$filtered_data_model],
			  top_tool       => 0,
			  copy_data      => 0,
			  clean => 2  );
		ui -> print( category => 'all',
					 message  => $message,
					 newline => 1 );
		$filter_fit -> run;
	}
	
	#this writes dataset to disk
	my $resultref = data::frem_compute_covariate_properties(directory  => $filtered_data_model->directory,
															filename => $filtered_datafile,
															idcolumn => $model->idcolumns->[0],
															invariant_covariates => $self->invariant,
															occ_index => $indices->{'occ_index'},
															data2name => $fremdataname,
															evid_index => $indices->{'evid_index'},
															mdv_index => $indices->{'mdv_index'},
															type_index => $indices->{'type_index'},
															cov_indices => $indices->{'cov_indices'},
															first_timevar_type => $first_timevar_type,
															missing_data_token => $self->missing_data_token);

	if (defined $resultref){
		$self->occasionlist($resultref->{'occasionlist'}) if (defined $resultref->{'occasionlist'}); 
		$self->invariant_median($resultref->{'invariant_median'}) if (defined $resultref->{'invariant_median'});
		$self->invariant_covmatrix($resultref->{'invariant_covmatrix'}) if (defined $resultref->{'invariant_covmatrix'});
		$self->timevar_median($resultref->{'timevar_median'}) if (defined $resultref->{'timevar_median'});
		$self->timevar_covmatrix($resultref->{'timevar_covmatrix'}) if (defined $resultref->{'timevar_covmatrix'});
	}

	if ($do_check){
		my $name_check_model = 'check_data.mod';
		my $data_check_model = $model ->  copy( filename    => $self -> directory().'m1/'.$name_check_model,
												output_same_directory => 1,
												write_copy => 0,
												copy_datafile   => 0,
												copy_output => 0);
		
		# have filtered data so can skip old accept/ignores. Need ignore=@ since have a header
		#have only one $PROB by input check
		$data_check_model->datafiles(problem_numbers => [1],
									 new_names => [$self -> directory().'m1/'.$fremdataname]);
		$data_check_model->problems->[0]->datas->[0]->ignoresign('@');
		$data_check_model->remove_option( record_name => 'data',
										  option_name => 'ACCEPT',
										  fuzzy_match => 1);
		$data_check_model->set_option( record_name => 'data',
									   option_name => 'IGNORE',
									   option_value => '('.$fremtype.'.GT.0)',
									   fuzzy_match => 1);
	
		foreach my $item (@{$self->extra_input_items()}){
			$data_check_model -> add_option(problem_numbers => [1],
											record_name => 'input',
											option_name => $item);
		}
		$data_check_model ->_write(overwrite => 1);

		my $rundir = $self -> directory().'/datacheck_modelfit_dir1';
		rmtree([ "$rundir" ]) if (-e $rundir);
		my $run = tool::modelfit ->new( %{common_options::restore_options(@common_options::tool_options)},
										base_directory	 => $self -> directory(),
										directory		 => $rundir,
										copy_data     => 0,
										models		 => [$data_check_model],
										top_tool              => 0);

		tool::add_to_nmoutput(run => $run, extensions => ['ext']);		
		ui -> print( category => 'all', message => 'Running data check model');
		$run -> run;
		#compare ofv. print this to log file
		my $check_ofv = 'undefined';
		if ($data_check_model->is_run()){
			$check_ofv = $data_check_model->outputs -> [0]->get_single_value(attribute=> 'ofv');
		}
		print "\nModel 0 ofv is    $mod0_ofv\n";
		print   "Data check ofv is $check_ofv\n";
	}
	
}

sub do_model1
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  BSV_par_block => { isa => 'ArrayRef', optional => 0 },
							  labelshash => { isa => 'HashRef', optional => 0 },
							  start_omega_record => { isa => 'Int', optional => 0 },
	);
	my $model = $parm{'model'};
	my $BSV_par_block = $parm{'BSV_par_block'};
	my $labelshash = $parm{'labelshash'};
	my $start_omega_record = $parm{'start_omega_record'};
	
	my $name_model = 'model_1.mod';
	my @leading_omega_records = ();
	my $frem_model;

	if (-e $self -> directory().'m1/'.$name_model){
		$frem_model = model->new( %{common_options::restore_options(@common_options::model_options)},
								  filename                    => 'm1/'.$name_model,
								  ignore_missing_output_files => 1 );

		if (scalar(@{$self->invariant}) > 0){
			croak('BSV_par undefined') unless (defined $BSV_par_block);
			for (my $i=0; $i< ($start_omega_record-1);$i++){
				#if start_omega_record is 1 we will push nothing
				#if no invariant stuff we will push all
				push(@leading_omega_records,$frem_model-> problems -> [0]->omegas->[$i]);
			}
		}

	}else{	
		#here we use original data file. It has been copied before to m1
		# input model 0 inits have already been updated
		$frem_model = $model ->  copy( filename    => $self -> directory().'m1/'.$name_model,
									   output_same_directory => 1,
									   write_copy => 0,
									   copy_datafile   => 0,
									   copy_output => 0);
		

		if (scalar(@{$self->invariant}) > 0){
			croak('BSV_par undefined') unless (defined $BSV_par_block);
			for (my $i=0; $i< ($start_omega_record-1);$i++){
				#if start_omega_record is 1 we will push nothing
				#if no invariant stuff we will push all
				push(@leading_omega_records,$frem_model-> problems -> [0]->omegas->[$i]);
			}

			#reset $start_omega_record and on, do not kill all
			$frem_model -> problems -> [0]-> omegas(\@leading_omega_records);
			$frem_model-> problems -> [0]->add_omega_block(new_omega => $BSV_par_block,
														   labels => $labelshash->{'bsv_par_labels'});
		}

		#FIXME if pre-existing BOV then replace with 0 in code
		
		$frem_model ->_write();
	}

	unless ($frem_model->is_run){
		my $rundir = $self -> directory().'/model1_modelfit_dir1';
		rmtree([ "$rundir" ]) if (-e $rundir);
		my $run = tool::modelfit ->new( %{common_options::restore_options(@common_options::tool_options)},
										base_directory	 => $self -> directory(),
										directory		 => $rundir,
										copy_data     => 0,
										models		 => [$frem_model],
										top_tool              => 0);
		tool::add_to_nmoutput(run => $run, extensions => ['phi','ext']);		
		ui -> print( category => 'all', message =>  'Estimating Model 1');
		$run-> run;
	}
	if (defined $frem_model->outputs and (defined $frem_model->outputs->[0])){
		$frem_model->update_inits(from_output=> $frem_model->outputs->[0]) ;
	}else{
		croak("No output from Model 1, cannot proceed with frem");
	}

	return ($frem_model,\@leading_omega_records);
}


sub do_model2
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  fremdataname => { isa => 'Str', optional => 0 },
							  bsv_parameter_count => { isa => 'Int', optional => 0 },
	);
	my $model = $parm{'model'};
	my $fremdataname = $parm{'fremdataname'};
	my $bsv_parameter_count = $parm{'bsv_parameter_count'};

	my $name_model = 'model_2.mod';
	
	my $frem_model;

	my $ntheta = $model ->nthetas(problem_number => 1);
	my $epsnum = 1 + $model->problems()->[0]->nsigmas(with_correlations => 0,
															   with_same => 1);

	if (-e $self -> directory().'m1/'.$name_model){
		$frem_model = model->new( %{common_options::restore_options(@common_options::model_options)},
								  filename                    => 'm1/'.$name_model,
								  ignore_missing_output_files => 1 );

	}else{	
		# input model  inits have already been updated
		$frem_model = $model ->  copy( filename    => $self -> directory().'m1/'.$name_model,
									   output_same_directory => 1,
									   write_copy => 0,
									   copy_datafile   => 0,
									   copy_output => 0);
		
		
	
		#DATA changes
		#we want to clear all old options from DATA
		$frem_model->problems->[0]->datas->[0]->options([]);
		$frem_model->problems->[0]->datas->[0]->ignoresign('@');
		$frem_model->datafiles(problem_numbers => [1],
							   new_names => [$self -> directory().'m1/'.$fremdataname]);

		#set theta omega sigma code input
		#FIXME input Mod 0 BOV if available, use model0 updated + phifile
		set_frem_records(model => $frem_model,
						 start_eta => ($self->skip_etas+1),
						 epsnum => $epsnum,
						 ntheta => $ntheta,
						 vpc => 0,
						 bsv_parameters => $bsv_parameter_count,
						 model_type =>2,
						 occasionlist =>  $self->occasionlist,
						 occasion => $self->occasion,
						 extra_input_items => $self->extra_input_items,
						 invariant_median => $self->invariant_median,
						 timevar_median => $self->timevar_median,
						 invariant_covmatrix => $self->invariant_covmatrix,
						 timevar_covmatrix => $self->timevar_covmatrix,
						 invariant => $self->invariant,
						 time_varying => $self->time_varying,
						 parameters_bov => $self->parameters_bov,
			);

		#FIXME use spdarise
		$frem_model-> problems -> [0]->ensure_posdef();
		$frem_model->_write();
	}

	unless ($frem_model->is_run){
		my $rundir = $self -> directory().'/model2_modelfit_dir1';
		rmtree([ "$rundir" ]) if (-e $rundir);
		my $run = tool::modelfit ->new( %{common_options::restore_options(@common_options::tool_options)},
										base_directory	 => $self -> directory(),
										directory		 => $rundir,
										copy_data     => 0,
										models		 => [$frem_model],
										top_tool              => 0);
		tool::add_to_nmoutput(run => $run, extensions => ['phi','ext']);		
		ui -> print( category => 'all', message =>  'Estimating Model 2');
		$run-> run;
	}
	if (defined $frem_model->outputs and (defined $frem_model->outputs->[0])){
		$frem_model->update_inits(from_output=> $frem_model->outputs->[0]) ;
	}else{
		croak("No output from Model 2, cannot proceed with frem");
	}

	return ($frem_model,$ntheta,$epsnum);
		
}


sub do_model3
{
	#FIXME input eta numbers
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  fremdataname => { isa => 'Str', optional => 0 },
							  ntheta => { isa => 'Int', optional => 0 },
							  epsnum => { isa => 'Int', optional => 0 },
							  bsv_parameter_count => { isa => 'Int', optional => 0 },
							  start_omega_record => { isa => 'Int', optional => 0 },
							  labelshash => { isa => 'HashRef', optional => 0 },
	);
	my $model = $parm{'model'};
	my $fremdataname = $parm{'fremdataname'};
	my $ntheta = $parm{'ntheta'};
	my $epsnum = $parm{'epsnum'};
	my $bsv_parameter_count = $parm{'bsv_parameter_count'};
	my $start_omega_record = $parm{'start_omega_record'};
	my $labelshash = $parm{'labelshash'};
	
	my $name_model = 'frem_model.mod';
	my $frem_model2; #FIXME undef
	my $mod3_bov_record; #FIXME undef
	
	my $frem_model;
	#FIXME original code uses copy of model 1, here we input model 2 for better initial estimates
	#need to replace pk/pred code for BOV
	if (-e $self -> directory().'m1/'.$name_model){
		$frem_model = model->new( %{common_options::restore_options(@common_options::model_options)},
								  filename                    => 'm1/'.$name_model,
								  ignore_missing_output_files => 1 );

	}else{	
		# input model  inits have already been updated
		$frem_model = $model ->  copy( filename    => $self -> directory().'m1/'.$name_model,
									   output_same_directory => 1,
									   write_copy => 0,
									   copy_datafile   => 0,
									   copy_output => 0);

		#continue FIXME
		#use model 2 phi for full blocks, BSV_all and BOV all, need model 2 eta numbers
		$frem_model->problems->[0]->datas->[0]->options([]);
		$frem_model->datafiles(problem_numbers => [1],
							   new_names => [$self -> directory().'m1/'.$fremdataname]);
		$frem_model->problems->[0]->datas->[0]->ignoresign('@');

		#FIXME input BSV_par_block.
		#FIXME change create omega block so that can take covariate data as input for BSV_all_block
		#set theta omega sigma code input
		set_frem_records(model => $frem_model,
						 start_eta => $self->skip_etas+1,
						 epsnum => $epsnum, #same as going into model 2
						 ntheta => $ntheta, #same as into model 2
						 bsv_parameters => $bsv_parameter_count,
						 vpc => 0,
						 model_type =>3,
						 occasionlist =>  $self->occasionlist,
						 occasion => $self->occasion,
						 extra_input_items => $self->extra_input_items,
						 invariant_median => $self->invariant_median,
						 timevar_median => $self->timevar_median,
						 invariant_covmatrix => $self->invariant_covmatrix,
						 timevar_covmatrix => $self->timevar_covmatrix,
						 invariant => $self->invariant,
						 time_varying => $self->time_varying,
						 parameters_bov => $self->parameters_bov,
			);
		#FIXME spdarise	
		$frem_model-> problems -> [0]->ensure_posdef();
		
		$frem_model->_write();

		my @leading_omega_records=();


		#create new BOV_all_occ1 to add
		if (scalar(@{$self->time_varying}) > 0){
			for (my $i=0; $i< ($mod3_bov_record-1);$i++){
				push(@leading_omega_records,$frem_model-> problems -> [0]->omegas->[$i]);
			}
			#reset omega
			$frem_model -> problems -> [0]-> omegas(\@leading_omega_records);

			my $mod2_bov_record=$start_omega_record;
			$mod2_bov_record=$start_omega_record+2 if (scalar(@{$self->invariant})>0);
			my $BOV_par_occ1 = $frem_model2->problems->[0]->get_record_matrix(type => 'omega',
																			  record_number => $mod2_bov_record);
			my $BOV_cov_occ1 = $frem_model2->problems->[0]->get_record_matrix(type => 'omega',
																			  record_number => ($mod2_bov_record+scalar(@{$self->occasionlist()})));

			#create full block to add to mod3
			#FIXME do better
			my $BOV_all_occ1 = create_full_block(top_block => $BOV_par_occ1,
												 bottom_block => $BOV_cov_occ1,
												 correlation => $smallcorrelation);

			my @all_labels = @{$labelshash->{'bov_par_labels'}};
			push(@all_labels,@{$labelshash->{'bov_cov_labels'}});
			$frem_model -> problems -> [0]-> add_omega_block(new_omega => $BOV_all_occ1,
															  labels=>\@all_labels);

			#BOV_all_occ2-end
			for (my $i=1; $i< scalar(@{$self->occasionlist()}); $i++){
				$frem_model -> add_records (type => 'omega',
											 record_strings => ['BLOCK SAME ;'.$labelshash->{'occasion_labels'}->[$i]]);
				
			}

		}
		
	}
	unless ($frem_model->is_run){
		my $rundir = $self -> directory().'/frem_modelfit_dir1';
		rmtree([ "$rundir" ]) if (-e $rundir);
		my $run = tool::modelfit ->new( %{common_options::restore_options(@common_options::tool_options)},
										base_directory	 => $self -> directory(),
										directory		 => $rundir,
										copy_data     => 0,
										models		 => [$frem_model],
										top_tool              => 0);
		tool::add_to_nmoutput(run => $run, extensions => ['phi','ext']);		
		ui -> print( category => 'all', message =>  'Estimating FREM model');
		$run-> run;
	}
	if (defined $frem_model->outputs and (defined $frem_model->outputs->[0])){
		$frem_model->update_inits(from_output=> $frem_model->outputs->[0]) ;
	}else{
		croak("No output from frem model, cannot proceed");
	}

	return $frem_model;
}	

sub do_model_vpc1
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  joindata => { isa => 'Str', optional => 0 },
							  CTV_parameters => { isa => 'ArrayRef', optional => 0 },
	);
	my $model = $parm{'model'};
	my $joindata = $parm{'joindata'};
	my $CTV_parameters = $parm{'CTV_parameters'};

	my $name_model = 'vpc_model_1.mod';
	my $frem_vpc_model;
	my @vpc2_input_params =();
	my @vpc1_table_params =();

	my $done = 0;
	if (-e $self -> directory().'m1/'.$name_model){
		$frem_vpc_model = model->new( %{common_options::restore_options(@common_options::model_options)},
									  filename                    => 'm1/'.$name_model,
									  ignore_missing_output_files => 1 );
		$done = 1;
	}else{	
		#input Model 3 is updated
		$frem_vpc_model = $model ->  copy( filename    => $self -> directory().'m1/'.$name_model,
										   output_same_directory => 1,
										   copy_datafile   => 0,
										   write_copy => 0,
										   copy_output => 0);      

	}

	#To create combined data simply print table with both filtered input data and new conditional data
	#The conditional headers will have wrong headers in table file to be used as data, but that is fine
	#as long as $INPUT in vpc2 is correct
	if( defined $frem_vpc_model->problems()->[0] -> inputs and 
		defined $frem_vpc_model->problems()->[0] -> inputs -> [0] -> options ) {
		foreach my $option ( @{$frem_vpc_model->problems()->[0] -> inputs -> [0] -> options} ) {
			unless ( $option -> name eq 'DROP' or $option -> name eq 'SKIP' or
					 $option -> value eq 'DROP' or $option -> value eq 'SKIP' or
					 $option -> name eq $fremtype){
				push(@vpc1_table_params,$option -> name);
				push( @vpc2_input_params, $option -> name );
			}
		}
	} else {
		croak("Trying to construct table for filtering data".
			  " but no headers were found in \$INPUT" );
	}
	
	foreach my $par (@{$CTV_parameters}){
		push(@vpc1_table_params,$par);
		push( @vpc2_input_params, 'CTV'.$par);
	}
	
	unless ($done){
		$frem_vpc_model->add_option( record_name => 'data',
									 option_name => 'IGNORE',
									 option_value => '('.$fremtype.'.GT.0)');
		
		#fix theta 
		foreach my $rec (@{$frem_vpc_model->problems()->[0]->thetas()}){
			foreach my $opt (@{$rec->options()}){
				$opt->fix(1);
			}
		}
		#unfix all sigma
		foreach my $rec (@{$frem_vpc_model->problems()->[0]->sigmas()}){
			$rec->fix(0) unless $rec->same();
			foreach my $opt (@{$rec->options()}){
				$opt->fix(0);
			}
		}
		#fix omega
		foreach my $rec (@{$frem_vpc_model->problems()->[0]->omegas()}){
			$rec->fix(1) unless $rec->same();
			foreach my $opt (@{$rec->options()}){
				$opt->fix(1);
			}
		}
		
		$frem_vpc_model -> remove_records(type => 'covariance');
		
	
		$frem_vpc_model -> add_records( type           => 'table',
										record_strings => [ join( ' ', @vpc1_table_params ).
															' NOAPPEND NOPRINT ONEHEADER FORMAT=sG15.7 FILE='.$joindata]);
		$frem_vpc_model-> problems -> [0]->ensure_posdef(); #FIXME
		
		$frem_vpc_model->_write();
	}
	
	unless ($frem_vpc_model->is_run){
		my $rundir = $self -> directory().'/vpc1_modelfit_dir1';
		rmtree([ "$rundir" ]) if (-e $rundir);

		my $run = tool::modelfit ->new( %{common_options::restore_options(@common_options::tool_options)},
										base_directory	 => $frem_vpc_model -> directory(),
										directory	 => $rundir,
										copy_data	 => 0,
										models		 => [$frem_vpc_model],
										top_tool              => 0);
		
		tool::add_to_nmoutput(run => $run, extensions => ['phi','ext']);		
		ui -> print( category => 'all', message => "\nExecuting FREM vpc model 1" );
		$run -> run;
		unless (-e $frem_vpc_model->directory().$joindata){
			die ($frem_vpc_model->directory().$joindata." does not exist\n");
		}

	}

	if (defined $frem_vpc_model->outputs and (defined $frem_vpc_model->outputs->[0])){
		$frem_vpc_model->update_inits(from_output=> $frem_vpc_model->outputs->[0]) ;
	}else{
		croak("No output from vpc 1 model, cannot proceed");
	}
	
	return ( $frem_vpc_model, \@vpc2_input_params);
}

sub do_model_vpc2
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  joindata => { isa => 'Str', optional => 0 },
							  ntheta => { isa => 'Int', optional => 0 },
							  epsnum => { isa => 'Int', optional => 0 },
							  CTV_parameters => { isa => 'ArrayRef', optional => 0 },
							  vpc2_input_params => { isa => 'ArrayRef', optional => 0 },
							  labelshash => { isa => 'HashRef', optional => 0 },
							  bsv_parameter_count => { isa => 'Int', optional => 0 },
							  start_omega_record => { isa => 'Int', optional => 0 },
	);
	my $model = $parm{'model'};
	my $joindata = $parm{'joindata'};
	my $epsnum = $parm{'epsnum'};
	my $ntheta = $parm{'ntheta'};
	my $CTV_parameters = $parm{'CTV_parameters'};
	my $vpc2_input_params = $parm{'vpc2_input_params'};
	my $labelshash = $parm{'labelshash'};
	my $bsv_parameter_count = $parm{'bsv_parameter_count'};
	my $start_omega_record = $parm{'start_omega_record'};
	
	my $frem_model1; #FIXME input this
	my $vpc_model1 ; #FIXME
	unless ($vpc_model1->is_run){
		croak("No output from frem_vpc1 run, cannot create final vpc model");
	}

	
	my $frem_vpc_model;
	my $name_model = 'frem_vpc.mod';

	if (-e $self -> directory().$name_model){
		$frem_vpc_model = model->new( %{common_options::restore_options(@common_options::model_options)},
									  filename                    => 'm1/'.$name_model,
									  ignore_missing_output_files => 1 );
	}else{	
		$frem_vpc_model = $frem_model1 ->  copy( filename    => $self -> directory().$name_model,
												 output_same_directory => 1,
												 write_copy => 0,
												 copy_datafile   => 0,
												 copy_output => 0);

		#DATA changes
		$frem_vpc_model->problems->[0]->datas->[0]->options([]);
		$frem_vpc_model->datafiles(problem_numbers => [1],
								   new_names => [$self -> directory().'m1/'.$joindata]);
		$frem_vpc_model->relative_data_path(1);
		$frem_vpc_model->problems->[0]->datas->[0]->ignoresign('@');

		set_frem_records(model => $frem_vpc_model,
						 start_eta => $self->skip_etas+1,
						 epsnum => $epsnum,
						 ntheta => $ntheta,
						 bsv_parameters => $bsv_parameter_count,
						 vpc => 1,
						 model_type =>2,
						 occasionlist =>  $self->occasionlist,
						 occasion => $self->occasion,
						 extra_input_items => $self->extra_input_items,
						 invariant_median => $self->invariant_median,
						 timevar_median => $self->timevar_median,
						 invariant_covmatrix => $self->invariant_covmatrix,
						 timevar_covmatrix => $self->timevar_covmatrix,
						 invariant => $self->invariant,
						 time_varying => $self->time_varying,
						 parameters_bov => $self->parameters_bov,
			);

		my $new_input_string = join(' ',@{$vpc2_input_params});
		$frem_vpc_model->problems()->[0]->set_records(type => 'input',
													  record_strings => [$new_input_string]);



		#fix theta  ??
		#must at least fix the ones that are replace by CTVPAR, since NM error otherwise?
		foreach my $rec (@{$frem_vpc_model->problems()->[0]->thetas()}){
			foreach my $opt (@{$rec->options()}){
				$opt->fix(1);
			}
		}
		#fix all sigma ??
		#foreach my $rec (@{$frem_vpc_model->problems()->[0]->sigmas()}){
		#	$rec->fix(1) unless $rec->same();
		#}
		
		$frem_vpc_model -> remove_records(type => 'covariance');
	

		replace_tvpar_with_ctvpar(model => $frem_vpc_model,
								  ctvpar =>$CTV_parameters);
	

		my @leading_omega_records=();
		$frem_vpc_model -> update_inits ( from_output => $vpc_model1->outputs->[0],
										  ignore_missing_parameters => 1,
										  update_fix => 1,
										  skip_output_zeros => 0,
										  update_omegas => 0,
										  update_sigmas => 1,
										  update_thetas => 1,
										  problem_number => 1);
		if ($start_omega_record > 1){
			#update all *before* start_omega_record  
			$frem_vpc_model -> update_inits ( from_output => $vpc_model1->outputs->[0],
											  ignore_missing_parameters => 1,
											  update_fix => 1,
											  skip_output_zeros => 0,
											  update_omegas => 1,
											  update_sigmas => 0,
											  update_thetas => 0,
											  start_record => 1,
											  end_record => ($start_omega_record-1),
											  problem_number => 1);
			for (my $i=0; $i< ($start_omega_record-1);$i++){
				push(@leading_omega_records,$frem_vpc_model-> problems -> [0]->omegas->[$i]);
			}

		}
		#reset $start_omega_record and on, not not kill all
		$frem_vpc_model -> problems -> [0]-> omegas(\@leading_omega_records);
		
		#compute conditional omega blocks
	
		my $bov_record;
		#TODO can get something not pos def from here.... round up diagonals in get_record_matrix???
		if (scalar(@{$self->invariant}) > 0){
			$bov_record = $start_omega_record+1;
			my $BSV_all = $vpc_model1->problems->[0]->get_record_matrix(type => 'omega',
																		record_number => $start_omega_record);
			my $new_BSV_par = [];
			my $res = linear_algebra::frem_conditional_omega_block($BSV_all,$bsv_parameter_count,$new_BSV_par);
			if ($res == 1){
				print "\nError when calling frem_conditional_omega_block for BSV, probably BSV_all part of omega from Model 3 ".
					"was not positive definite. Take care of this manually and restart frem.\n";
				exit;
			}
			if ($res == 2){
				croak("\nInput error when calling frem_conditional_omega_block for BSV, this is a bug.\n");
			}
			
			$frem_vpc_model -> problems -> [0]-> add_omega_block(new_omega => $new_BSV_par,
																 labels => $labelshash->{'bsv_par_labels'});
		}else{
			$bov_record = $start_omega_record;
		}
		if (scalar(@{$self->time_varying}) > 0){
			my $BOV_all_occ1 = $vpc_model1->problems->[0]->get_record_matrix(type => 'omega',
																			 record_number => $bov_record);
			
			my $new_BOV_par_occ1 = [];
			my $res = linear_algebra::frem_conditional_omega_block($BOV_all_occ1,scalar(@{$self->parameters_bov()}),$new_BOV_par_occ1);
			if ($res == 1){
				print "\nError when calling frem_conditional_omega_block for BOV, ".
					"probably BOV_all_occ1 part of omega from Model 3 ".
					"was not positive definite. Take care of this manually and restart frem.\n";
				exit;
			}
			if ($res == 2){
				croak("\nInput error when calling frem_conditional_omega_block for BOV, this is a bug.\n");
			}

			$frem_vpc_model -> problems -> [0]-> add_omega_block(new_omega => $new_BOV_par_occ1,
																 labels => $labelshash->{'bov_par_labels'});
			#add block same
			#BOV_par_occ2-end
			for (my $i=1; $i< scalar(@{$self->occasionlist()}); $i++){
				$frem_vpc_model -> add_records (type => 'omega',
												record_strings => ['BLOCK SAME ;'.$labelshash->{'occasion_labels'}->[$i]]);
				
			}
		}


		$frem_vpc_model->_write();

	}


}

sub create_template_models
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model => { isa => 'Ref', optional => 0 }
		);
	my $model = $parm{'model'};

	my $name_model2_timevar = 'model_2_timevar.mod';
	my $name_model2_invar = 'model_2_invariant.mod';

	my $frem_model2_invar;
	my $frem_model3;
	my $frem_model1;
	my $frem_model2_timevar;
	my $frem_model0;
	my $data2name;
	my $epsnum;
	my $ntheta;
	my $bsv_parameter_count;
	##########################################################################################
	#Create Model 2 only invariant
	##########################################################################################

	if (0 and (scalar(@{$self->invariant}) > 0)){
		$frem_model2_invar = $frem_model1 ->  copy( filename    => $self -> directory().'m1/'.$name_model2_invar,
													output_same_directory => 1,
													write_copy => 0,
													copy_datafile   => 0,
													copy_output => 0);

		#DATA changes
		#we want to clear all old options from DATA
		$frem_model2_invar->problems->[0]->datas->[0]->options([]);
		$frem_model2_invar->datafiles(problem_numbers => [1],
									  new_names => [$self -> directory().'m1/'.$data2name]);
		$frem_model2_invar->problems->[0]->datas->[0]->ignoresign('@');
		$frem_model2_invar->add_option( record_name => 'data',
										option_name => 'IGNORE',
										option_value => '('.$fremtype.'.GT.'.scalar(@{$self->invariant}).')');
		
		#set theta omega sigma code input
		set_frem_records(model => $frem_model2_invar,
						 start_eta => $self->skip_etas+1,
						 skip_time_varying => 1,
						 bsv_parameters => $bsv_parameter_count,
						 epsnum => $epsnum,
						 ntheta => $ntheta,
						 vpc => 0,
						 model_type =>2,
						 occasionlist =>  $self->occasionlist,
						 occasion => $self->occasion,
						 extra_input_items => $self->extra_input_items,
						 invariant_median => $self->invariant_median,
						 timevar_median => $self->timevar_median,
						 invariant_covmatrix => $self->invariant_covmatrix,
						 timevar_covmatrix => $self->timevar_covmatrix,
						 invariant => $self->invariant,
						 time_varying => $self->time_varying,
						 parameters_bov => $self->parameters_bov,
			);
		#FIXME spdarise
		$frem_model2_invar-> problems -> [0]->ensure_posdef();
		$frem_model2_invar->_write();
	}
	##########################################################################################
	#Create Model 2 only time-varying
	##########################################################################################
	
	if (0 and (scalar(@{$self->time_varying}) > 0)){
		#base on model 0
		$frem_model2_timevar = $frem_model0 ->  copy( filename    => $self -> directory().'m1/'.$name_model2_timevar,
													  output_same_directory => 1,
													  write_copy => 0,
													  copy_datafile   => 0,
													  copy_output => 0);
		
		#DATA changes
		#we want to clear all old options from DATA
		$frem_model2_timevar->problems->[0]->datas->[0]->options([]);
		$frem_model2_timevar->datafiles(problem_numbers => [1],
										new_names => [$self -> directory().'m1/'.$data2name]);
		$frem_model2_timevar->problems->[0]->datas->[0]->ignoresign('@');
		$frem_model2_timevar->add_option( record_name => 'data',
										  option_name => 'ACCEPT',
										  option_value => '('.$fremtype.'.LT.1,'.$fremtype.'.GT.'.scalar(@{$self->invariant}).')');

		#set theta omega sigma code input
		set_frem_records(model => $frem_model2_timevar,
						 start_eta => $self->skip_etas+1,
						 skip_invariant => 1,
						 epsnum => $epsnum,
						 bsv_parameters => $bsv_parameter_count,
						 vpc => 0,
						 ntheta => $ntheta,
						 model_type =>2,
						 occasionlist =>  $self->occasionlist,
						 occasion => $self->occasion,
						 extra_input_items => $self->extra_input_items,
						 invariant_median => $self->invariant_median,
						 timevar_median => $self->timevar_median,
						 invariant_covmatrix => $self->invariant_covmatrix,
						 timevar_covmatrix => $self->timevar_covmatrix,
						 invariant => $self->invariant,
						 time_varying => $self->time_varying,
						 parameters_bov => $self->parameters_bov,
			);
		#FIXME spdarise
		$frem_model2_timevar-> problems -> [0]->ensure_posdef();
		
		$frem_model2_timevar->_write();
	}


}


sub modelfit_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	my $model = $self -> models -> [$model_number-1];


	my ($total_bsv_etas,$bsv_parameter_count,$start_omega_record,$input_bov_etas)=
		get_start_numbers(model=>$model,
						  n_invariant=>scalar(@{$self->invariant}),
						  skip_etas => $self->skip_etas());

	#if pre-existing bov etas, then check which parameter they are on, array same order as input_bov_etas
	my $input_bov_parameters = [];
	if (scalar(@{$input_bov_etas})>0){
		croak("support for pre-existing BOV etas is not yet implemented");
		$input_bov_parameters = get_parameters_to_etas(model => $model,
													   use_pred => $self->use_pred,
													   etas => $input_bov_etas->[0]);
		check_input_bov(input_bov_parameters => $input_bov_parameters,
 						parameters_bov => $self-> parameters_bov);
		#FIXME these must be input to model2
	}

	##########################################################################################
	#Mod 0, always create, run if needed
	##########################################################################################
	my ($mod0_ofv, $phi_file_0, $frem_model0) = $self-> do_model0(model => $model);
	my $BSV_par_block;
	
	if ($bsv_parameter_count < 1){
		$BSV_par_block = [];
	}else{
		my $errormessage;
		($BSV_par_block,$errormessage) = get_filled_omega_block(model => $frem_model0,
													   phi_file => $phi_file_0,
													   start_eta_1 => ($self->skip_etas()+1),
													   end_eta_1 => ($self->skip_etas()+$bsv_parameter_count));
		unless (length($errormessage) == 0){
			croak("creating full omega block from model 0 failed:\n $errormessage");
		}
	}

	my ($CTV_parameters,$etanum_to_parameter) = get_CTV_parameters(model => $frem_model0,
																   bov_parameters => $self->parameters_bov);
	#FIXME leading_omega_records


	my $frem_dataset = 'frem_dataset.dta';
	$self->do_frem_dataset(model => $frem_model0,
						   mod0_ofv => $mod0_ofv,
						   fremdataname => $frem_dataset);


	my $labelshash = create_labels(occasionlist => $self->occasionlist,
									 occasion => $self->occasion,
									 invariant => $self->invariant,
									 bov_parameters => $self->parameters_bov,
									 time_varying => $self->time_varying,
									 etanum_to_parameter => $etanum_to_parameter,
									 start_eta => ($self->skip_etas +1),
									 bsv_parameters => $bsv_parameter_count);

	my ($frem_model1,$leading_omega_records) = $self->do_model1(model => $frem_model0,
																BSV_par_block => $BSV_par_block,
																start_omega_record=> $start_omega_record,
																labelshash => $labelshash);
	

	my ($frem_model2,$ntheta,$epsnum) = $self->do_model2(model => $frem_model1,
														 fremdataname => $frem_dataset,
														 bsv_parameter_count => $bsv_parameter_count);


	my $frem_model3 = $self->do_model3(model => $frem_model2,
									   fremdataname => $frem_dataset,
									   bsv_parameter_count => $bsv_parameter_count,
									   start_omega_record=> $start_omega_record,
									   labelshash => $labelshash,									   
									   ntheta =>$ntheta,
									   epsnum => $epsnum);

	if ($self->vpc()){
		my $joindata = 'frem_vpc.dta';
		my ($vpc_model1,$vpc2_input_par) = $self->do_model_vpc1(model => $frem_model3,
																joindata => $joindata,
																CTV_parameters => $CTV_parameters);


		$self->do_model_vpc2(model => $vpc_model1,
							 joindata => $joindata,
							 ntheta => $ntheta,
							 epsnum => $epsnum,
							 CTV_parameters => $CTV_parameters,
							 vpc2_input_params => $vpc2_input_par,
							 labelshash => $labelshash,
							 bsv_parameter_count => $bsv_parameter_count,
							 start_omega_record => $start_omega_record	);

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


    #this is just a placeholder
	my ($dir,$file) = 
		OSspecific::absolute_path( $self -> directory,
								   $self -> raw_results_file->[$model_number-1] );
	my ($npdir,$npfile) = 
		OSspecific::absolute_path( $self -> directory,
								   $self -> raw_nonp_file->[$model_number -1]);


	#my $orig_mod = $self -> models[$model_number-1];
	$subroutine = sub {

		my $modelfit = shift;
		my $mh_ref   = shift;
		my %max_hash = %{$mh_ref};
		
	};
	return $subroutine;


	return \&subroutine;
}

sub modelfit_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};


}

sub prepare_results
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		arg1  => { isa => 'Int', optional => 1 }
	);
}

sub create_data2_model
{
	my %parm = validated_hash(\@_,
							  model => { isa => 'Ref', optional => 0 },
							  filename => { isa => 'Str', optional => 0 },
							  filtered_datafile => { isa => 'Str', optional => 0 },
							  bov_parameters => { isa => 'Int', optional => 0 },
							  dv  => { isa => 'Str', optional => 0 },
							  time_varying  => { isa => 'ArrayRef', optional => 0 },
							  invariant  => { isa => 'ArrayRef', optional => 0 },
							  occasion  => { isa => 'Str', optional => 0 },
		);

	my $model = $parm{'model'};
	my $filename = $parm{'filename'};
	my $filtered_datafile = $parm{'filtered_datafile'};
	my $bov_parameters = $parm{'bov_parameters'};
	my $dv = $parm{'dv'};
	my $invariant = $parm{'invariant'};
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
	if (scalar(@{$invariant})>0){
		push(@{$typeorder},@{$invariant}); #add list of invariant covariate names to typeorder
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
	unless (defined $evid_index or defined $mdv_index){
		push(@filter_table_header,'MDV');
		$mdv_index = $#filter_table_header;
		push(@{$extra_input_items},'MDV');
		$add_mdv=1;
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
		#cannot have dummy model, NONMEM cannot append MDV
		foreach my $remove_rec ('simulation','covariance','table','scatter','estimation'){
			$filtered_data_model -> remove_records(type => $remove_rec);
		}
		my @code;
		@code = @{$filtered_data_model->get_code(record => 'pk')};
		my $use_pred = 0;
		unless ( $#code > 0 ) {
			@code = @{$filtered_data_model->get_code(record => 'pred')};
			$use_pred = 1;
		}
		if ( $#code <= 0 ) {
			croak("Neither PK or PRED defined in model 0");
		}
		push(@code,$fremtype.'=0');
		if ($use_pred ) {
			$filtered_data_model->set_code(record => 'pred', code => \@code);
		} else {
			$filtered_data_model->set_code(record => 'pk', code => \@code);
		}
		
		$message = "Running with MAXEVAL=0 to filter data and add MDV ".$fremtype." for Data set 2";
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
		$message = "Running dummy model to filter data and add ".$fremtype." for Data set 2";
	}
	$filtered_data_model -> add_records(type => 'estimation',
										record_strings => ['MAXEVALS=0 METHOD=ZERO']);

	# set $TABLE record

	$filtered_data_model -> add_records( type           => 'table',
		record_strings => [ join( ' ', @filter_table_header ).
			' NOAPPEND NOPRINT ONEHEADER FORMAT=sG15.7 FILE='.$filtered_datafile]);

	my %indices;
	$indices{'occ_index'}=$occ_index;
	$indices{'evid_index'}=$evid_index;
	$indices{'mdv_index'}=$mdv_index;
	$indices{'type_index'}=$type_index;
	$indices{'cov_indices'}=\@cov_indices;

	return ($filtered_data_model,\%indices,$first_timevar_type,$extra_input_items,$message);

}

sub set_frem_records
{
	my %parm = validated_hash(\@_,
							  model => { isa => 'Ref', optional => 0 },
							  skip_invariant => { isa => 'Bool', default => 0 },
							  skip_time_varying => { isa => 'Bool', default => 0 },
							  model_type => { isa => 'Int', optional => 0 },
							  vpc => { isa => 'Bool', optional => 0 },
							  start_eta => { isa => 'Int', optional => 0 },
							  epsnum => { isa => 'Int', optional => 0 },
							  ntheta => { isa => 'Int', optional => 0 },
							  bsv_parameters => { isa => 'Int', optional => 0 },
							  occasionlist =>  { isa => 'ArrayRef', optional => 0 },
							  occasion =>  { isa => 'Str', optional => 0 },
							  extra_input_items =>  { isa => 'ArrayRef', optional => 0 },
							  invariant_median =>  { isa => 'ArrayRef', optional => 0 },
							  timevar_median =>  { isa => 'ArrayRef', optional => 0 },
							  invariant_covmatrix =>  { isa => 'ArrayRef', optional => 0 },
							  timevar_covmatrix =>  { isa => 'ArrayRef', optional => 0 },
							  invariant =>  { isa => 'ArrayRef', optional => 0 },
							  time_varying =>  { isa => 'ArrayRef', optional => 0 },
							  parameters_bov =>  { isa => 'ArrayRef', optional => 0 },
		);
	my $model = $parm{'model'}; #this will always be model1
	my $skip_invariant = $parm{'skip_invariant'};
	my $skip_time_varying = $parm{'skip_time_varying'};
	my $model_type = $parm{'model_type'};
	my $epsnum = $parm{'epsnum'};
	my $ntheta = $parm{'ntheta'};
	my $bsv_parameters = $parm{'bsv_parameters'};
	my $start_eta = $parm{'start_eta'};
	my $vpc = $parm{'vpc'};
	my $occasionlist = $parm{'occasionlist'};
	my $occasion = $parm{'occasion'};
	my $extra_input_items = $parm{'extra_input_items'};
	my $invariant_median = $parm{'invariant_median'};
	my $timevar_median = $parm{'timevar_median'};
	my $invariant_covmatrix = $parm{'invariant_covmatrix'};
	my $timevar_covmatrix = $parm{'timevar_covmatrix'};
	my $invariant = $parm{'invariant'};
	my $time_varying = $parm{'time_varying'};
	my $parameters_bov = $parm{'parameters_bov'};

    #in is ref of model
    #model_type 2 or 3
    #epsnum
    #ntheta
    #this sets theta omega sigma input

    unless ($model_type == 2 or $model_type ==3){
		croak("invalid model_type $model_type input to set_frem_records");
    }

	my $n_invariant = scalar(@{$invariant});
	my $n_time_varying = scalar(@{$time_varying});

	my %labelshash = %{create_labels(occasionlist => $occasionlist,
									 occasion => $occasion,
									 invariant => $invariant,
									 bov_parameters => $parameters_bov,
									 time_varying => $time_varying,
									 etanum_to_parameter => {},
									 start_eta => $start_eta,
									 bsv_parameters => $bsv_parameters)};


    my $n_occasions = scalar(@{$occasionlist});

    #INPUT changes. This would be undone for vpc2, but does not hurt to do it here
	if (not $vpc){
		foreach my $item (@{$extra_input_items}){
			#mdv and fremtype
			$model -> add_option(problem_numbers => [1],
								 record_name => 'input',
								 option_name => $item);
		}
	}
    
    #SIGMA changes
    $model->add_records(type => 'sigma',
						problem_numbers => [1],
						record_strings => ['0.0000001 FIX ; EPSCOV']);


    #THETA changes from Mod 1
	
	if (not $vpc){
		my @theta_strings =();
		unless ($skip_invariant){
			for (my $i=0; $i< $n_invariant; $i++){
				my $val=$invariant_median->[$i];
				$val=0.001 if ($val==0);
				push(@theta_strings,' '.sprintf("%.12G",$val).'; TV'.$invariant->[$i]);
			}
		}
		unless ($skip_time_varying){
			for (my $i=0; $i< $n_time_varying; $i++){
				my $val=$timevar_median->[$i];
				$val=0.001 if ($val==0);
				push(@theta_strings,' '.sprintf("%.12G",$val).'; TV'.$time_varying->[$i]);
			}
		}
		$model->add_records(type => 'theta',
							problem_numbers => [1],
							record_strings => \@theta_strings);
	}

    #OMEGA changes starting from Mod1
	#BSV part
    if ((not $skip_invariant) and ($n_invariant > 0) and (not $vpc)){
		#this is BSV_cov
		$model-> problems -> [0]->add_omega_block(new_omega => $invariant_covmatrix,
												  labels => $labelshash{'bsv_cov_labels'});
		if($model_type == 3){
			#replace with BSV_all (full block from BSV_par + BSV_cov
			#FIXME use new code here omega_block, make sure update inits done and have input from mod 2 and not 1!
			my $BSV_all_block = $model-> problems -> [0]->get_filled_omega_matrix(start_eta => $start_eta);

			my $start_omega_record = $model-> problems -> [0]->check_skip_etas(start_eta => $start_eta);
			my @leading_omega_records=();
			for (my $i=0; $i< ($start_omega_record-1);$i++){
				#if start_omega_record is 1 we will push nothing
				push(@leading_omega_records,$model-> problems -> [0]->omegas->[$i]);
			}
			#reset $start_omega_record and on, do not kill all
			$model -> problems -> [0]-> omegas(\@leading_omega_records);
			
			$model -> problems -> [0]->add_omega_block(new_omega => $BSV_all_block);
		}
    }


    #OMEGA changes starting from Mod1
	#BOV part
    if ((not $skip_time_varying) and ($n_time_varying > 0)){

		if ($model_type == 2){ #2 or vpc2
			my $BOV_par_block;
			for (my $i=0 ; $i< scalar(@{$parameters_bov}); $i++){
				push(@{$BOV_par_block},[($bov_variance_init*$smallcorrelation) x scalar(@{$parameters_bov})]);
				$BOV_par_block->[$i][$i] = $bov_variance_init;
			}
			#BOV_par_occ1
			$model-> problems -> [0]->add_omega_block(new_omega => $BOV_par_block,
													  labels => $labelshash{'bov_par_labels'});
			#BOV_par_occ2-end
			for (my $i=1; $i< $n_occasions; $i++){
				$model -> add_records (type => 'omega',
									   record_strings => ['BLOCK SAME ; '.$occasion.'='.$occasionlist->[$i]]);
				
			}
			if (not $vpc){
				#BOV_cov, do not add this for vpc2
				#BOV_cov_occ1
				$model-> problems -> [0]->add_omega_block(new_omega => $timevar_covmatrix,
														  labels => $labelshash{'bov_cov_labels'});
				#BOV_cov_occ2-end
				for (my $i=1; $i< $n_occasions; $i++){
					$model -> add_records (type => 'omega',
										   record_strings => ['BLOCK SAME ; '.$occasion.'='.$occasionlist->[$i]]);
					
				}
			}
		}elsif ($model_type == 3){
			my $BOV_all_block;
			my @bovlabels=@{$labelshash{'bov_par_labels'}};
			push(@bovlabels,@{$labelshash{'bov_cov_labels'}});

			for (my $i=0 ; $i< (scalar(@{$parameters_bov})+$n_time_varying); $i++){
				push(@{$BOV_all_block},[($bov_variance_init*$smallcorrelation) x (scalar(@{$parameters_bov})+$n_time_varying)]);
				$BOV_all_block->[$i][$i] = $bov_variance_init;
			}
			#replace part with ->timevar_covmatrix 
			for (my $i=0 ; $i<$n_time_varying; $i++){
				for (my $j=0 ; $j<= $i ; $j++){
					$BOV_all_block->[scalar(@{$parameters_bov})+$i][scalar(@{$parameters_bov})+$j] = 
						$timevar_covmatrix->[$i][$j];
				}
			}

			$model-> problems -> [0]->add_omega_block(new_omega => $BOV_all_block,
													  labels => \@bovlabels);
			for (my $i=1; $i< $n_occasions; $i++){
				$model -> add_records (type => 'omega',
									   record_strings => ['BLOCK SAME ; '.$occasion.'='.$occasionlist->[$i]]);
				
			}
		}else{
			croak("bug in loop set_theta_omega_code");
			
		}

    }

	set_frem_code( model => $model,
				   skip_invariant => $skip_invariant,
				   skip_time_varying => $skip_time_varying,
				   model_type => $model_type,
				   vpc => $vpc,
				   start_eta => $start_eta,
				   epsnum => $epsnum,
				   ntheta => $ntheta,
				   bsv_parameters => $bsv_parameters,
				   occasionlist => $occasionlist,
				   occasion => $occasion,
				   invariant => $invariant,
				   time_varying => $time_varying,
				   parameters_bov => $parameters_bov);
	
}



sub set_frem_code
{
	my %parm = validated_hash(\@_,
							  model => { isa => 'Ref', optional => 0 },
							  skip_invariant => { isa => 'Bool', optional => 0 },
							  skip_time_varying => { isa => 'Bool', optional => 0 },
							  model_type => { isa => 'Int', optional => 0 },
							  vpc => { isa => 'Bool', optional => 0 },
							  start_eta => { isa => 'Int', optional => 0 },
							  epsnum => { isa => 'Int', optional => 0 },
							  ntheta => { isa => 'Int', optional => 0 },
							  bsv_parameters => { isa => 'Int', optional => 0 },
							  occasionlist =>  { isa => 'ArrayRef', optional => 0 },
							  occasion =>  { isa => 'Str', optional => 0 },
							  invariant =>  { isa => 'ArrayRef', optional => 0 },
							  time_varying =>  { isa => 'ArrayRef', optional => 0 },
							  parameters_bov =>  { isa => 'ArrayRef', optional => 0 },
		);
	my $model = $parm{'model'}; #this will always be model1
	my $skip_invariant = $parm{'skip_invariant'};
	my $skip_time_varying = $parm{'skip_time_varying'};
	my $model_type = $parm{'model_type'};
	my $epsnum = $parm{'epsnum'};
	my $ntheta = $parm{'ntheta'};
	my $bsv_parameters = $parm{'bsv_parameters'};
	my $start_eta = $parm{'start_eta'};
	my $vpc = $parm{'vpc'};
	my $occasionlist = $parm{'occasionlist'};
	my $occasion = $parm{'occasion'};
	my $invariant = $parm{'invariant'};
	my $time_varying = $parm{'time_varying'};
	my $parameters_bov = $parm{'parameters_bov'};

    #in is ref of model
    #model_type 2 or 3
    #epsnum
    #ntheta
    #this sets code

	my $n_invariant = scalar(@{$invariant});
	my $n_time_varying = scalar(@{$time_varying});

    unless ($model_type == 2 or $model_type ==3){
		croak("invalid model_type $model_type input to set_frem_code");
    }
    my $n_occasions = scalar(@{$occasionlist});

    my @code;
    @code = @{$model->get_code(record => 'pk')};
    my $use_pred = 0;
    unless ($#code > 0) {
			@code = @{$model->get_code(record => 'pred')};
			$use_pred = 1;
    }
    if ($#code <= 0) {
			croak("Neither PK or PRED defined in input model");
    }


	
    #PK/PRED changes at beginning A
	my @begin_code =(';;;FREM CODE BEGIN A');
	
	if ((not $skip_invariant) and (not $vpc) and ($n_invariant > 0)){
		#this is for BSV_cov
		for (my $i=0; $i< $n_invariant; $i++){
			push(@begin_code,'BSV'.$invariant->[$i].' = ETA('.($start_eta+$bsv_parameters+$i).')' );
		}
	}
	if ((not $skip_time_varying) and ($n_time_varying > 0)){
		for (my $i=0 ; $i< scalar(@{$parameters_bov}); $i++){
			push(@begin_code,'BOV'.$parameters_bov->[$i].' = 0' );
		}

		if (not $vpc){
			for (my $i=0 ; $i< $n_time_varying; $i++){
				push(@begin_code,'BOV'.$time_varying->[$i].' = 0' );
			}
		}
		if ($model_type == 2){
			#for BOV_par
			for (my $i=0; $i< $n_occasions; $i++){
				push(@begin_code,'IF ('.$occasion.'.EQ.'.$occasionlist->[$i].') THEN' );
				my $offset = ($start_eta-1)+$bsv_parameters+$n_invariant+$i*(scalar(@{$parameters_bov}));
				if ($vpc){
					#smaller offset if vpc
					$offset = ($start_eta-1)+$bsv_parameters+$i*(scalar(@{$parameters_bov}));
				}
				for (my $j=0 ; $j< scalar(@{$parameters_bov}); $j++){
					push(@begin_code,'   BOV'.$parameters_bov->[$j].' = ETA('.($offset+$j+1).')');
				}
				push(@begin_code,'END IF' );
			}
			
			if (not $vpc){
				#BOV_cov
				for (my $i=0; $i< $n_occasions; $i++){
					push(@begin_code,'IF ('.$occasion.'.EQ.'.$occasionlist->[$i].') THEN' );
					my $offset = ($start_eta-1)+$bsv_parameters+$n_invariant;
					$offset = $offset+(scalar(@{$parameters_bov}))*$n_occasions+$i*$n_time_varying;
					for (my $j=0 ; $j< $n_time_varying; $j++){
						push(@begin_code,'   BOV'.$time_varying->[$j].' = ETA('.($offset+$j+1).')');
					}
					push(@begin_code,'END IF' );
				}
			}
			
		}elsif ($model_type == 3){
			for (my $i=0; $i< $n_occasions; $i++){
				push(@begin_code,'IF ('.$occasion.'.EQ.'.$occasionlist->[$i].') THEN' );
				my $offset = ($start_eta-1)+$bsv_parameters+$n_invariant+$i*((scalar(@{$parameters_bov}))+$n_time_varying);
				for (my $j=0 ; $j< scalar(@{$parameters_bov}); $j++){
					push(@begin_code,'   BOV'.$parameters_bov->[$j].' = ETA('.($offset+$j+1).')');
				}
				$offset = $offset+(scalar(@{$parameters_bov}));
				for (my $j=0 ; $j< $n_time_varying; $j++){
					push(@begin_code,'   BOV'.$time_varying->[$j].' = ETA('.($offset+$j+1).')');
				}
				push(@begin_code,'END IF' );
			}
		}else{
			croak("bug in loop set_theta_omega_code");
		}
	}
	push(@begin_code,';;;FREM CODE END A' );


    my @end_code =();
    #ERROR/PRED changes at end
	if (not $vpc){
		@end_code = (';;;FREM CODE BEGIN C');
		for (my $i=0; $i< $n_invariant; $i++){
			push(@end_code,'Y'.($i+1).' = THETA('.($ntheta+$i+1).') + BSV'.$invariant->[$i]);
		}
		for (my $i=0; $i< $n_time_varying; $i++){
			push(@end_code,'Y'.($n_invariant+$i+1).' = THETA('.($ntheta+$n_invariant+$i+1).') + BOV'.$time_varying->[$i]);
		}
		for (my $i=1; $i<=($n_invariant+$n_time_varying); $i++){
			push(@end_code,'IF ('.$fremtype.'.EQ.'.$i.') THEN' );
			push(@end_code,'   Y = Y'.$i.'+EPS('.$epsnum.')' );
			push(@end_code,'   IPRED = Y'.$i );
			push(@end_code,'END IF' );
		}
		
		push(@end_code,';;;FREM CODE END C' );
	}


    #PK/PRED changes at beginning B: Add BOV on parameters
    if ((not $skip_time_varying) and  ($n_time_varying > 0)){
		foreach my $parameter (@{$parameters_bov}){
			my $success = 0;
			my $etanum = 0;
			my $bov= 'BOV'.$parameter;
			#	for (reverse  @code ) {
			for (my $i=$#code; $i>=0 ; $i--) {
				next if ( $code[$i] =~ /^\s*;/); #comment line
				$_ = $code[$i];
				if ( /^\s*(\w+)\s*=\s*/ and ($1 eq $parameter) ){
					s/^(\s*\w+\s*=\s*)//;
					my $left = $1;
					my ($right,$comment) = split( ';', $_, 2 );

					if ($right =~ /\bETA\(([0-9]+)\)/){
						#add BOV
						$etanum = $1;
						$right =~ s/ETA\($etanum\)/(ETA($etanum)+$bov)/;
					}elsif ($right =~ /\(0\)/){
						#replace 0 with BOV
						$right =~ s/\(0\)/($bov)/;
					}else{
						croak("Could not find an appropriate place to add $bov on the line ".
							  $parameter."= ...");
					}
					$success=1;
					$code[$i] = $left.$right;
					$code[$i] = $code[$i].';'.$comment if (length($comment)>0);
					last;
				}
				
			}
			unless ( $success ) {
				my $mes = "Could not find $parameter=... line to add $bov\n";
				croak($mes );
			}
		}
    }

    my $found_anchor = -1;
    my $i = 0;
    for ( @code ) {
		if ( /^;;;FREM-ANCHOR/) {
			$found_anchor = $i;
			last;
		}
		$i++
    }
    if ($found_anchor >= 0){
		my @block1 =  (@code[0..$found_anchor]);
		my @block2 =  (@code[($found_anchor+1)..$#code]);
		@code = (@block1,@begin_code,@block2);
    }else{
		unshift(@code,@begin_code);
    }
    
	if ( $use_pred ) {
		push(@code,@end_code);
		$model->set_code(record => 'pred', code => \@code);
	} else {
		$model->set_code(record => 'pk', code => \@code);
		my @error = @{$model->get_code(record => 'error')};
		push(@error,@end_code);
		$model->set_code(record => 'error', code => \@error);
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

no Moose;
__PACKAGE__->meta->make_immutable;
1;
