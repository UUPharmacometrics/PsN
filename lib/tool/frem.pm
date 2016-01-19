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
use array;

use Moose;
use MooseX::Params::Validate;

extends 'tool';

#FIXME dv synonym automatic handling

my $fremtype = 'FREMTYPE'; 
my $smallcorrelation = 0.01; #FIXME
my $bov_variance_init = 0.1; #FIXME
my $indentation = '     ';
my $smallnum = 0.0000001;

has 'skip_omegas' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'skip_etas' => ( is => 'rw', isa => 'Int', default=> 0);
has 'start_omega_record' => ( is => 'rw', isa => 'Int', default=> 1);
has 'estimate' => ( is => 'rw', isa => 'Int', default => 3 );
has 'occasionlist' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'extra_input_items' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'invariant_median' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'timevar_median' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'invariant_covmatrix' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'timevar_covmatrix' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'input_model_fix_thetas' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'input_model_fix_omegas' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'input_model_fix_sigmas' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'check' => ( is => 'rw', isa => 'Bool', default => 1 );
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
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'frem_results.csv' );
has 'use_pred' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'estimate_regular_final_model' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'estimate_cholesky_final_model' => ( is => 'rw', isa => 'Bool', default => 0 );


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
#				$type_ok = 1 if ($option -> name() eq $self->type()); 
#				$occ_ok = 1 if ($option -> name() eq $self->occasion()); 
			}
		}
#		croak("type column ".$self->type()." not found in \$INPUT" ) unless $type_ok;
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
	
	$self->input_model_fix_thetas(get_or_set_fix(model => $self->models->[0],
												 type => 'thetas'));
	$self->input_model_fix_omegas(get_or_set_fix(model => $self->models->[0],
												 type => 'omegas'));
	$self->input_model_fix_sigmas(get_or_set_fix(model => $self->models->[0],
												 type => 'sigmas'));

	
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
	$stop_record = scalar(@{$model->problems->[0]->$type}) unless (defined $stop_record);
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
			if ($model->problems->[0]->$type->[$i]->type eq 'BLOCK'){
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
	#	croak("num is $num but parameter num is ".scalar(@{$parameter_etanumbers}));
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


sub put_skipped_omegas_first
{

	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  problem_index  => { isa => 'Int', default => 0 },
							  skip_omegas  => { isa => 'ArrayRef', optional => 0 },
							  input_model_fix_omegas => { isa => 'ArrayRef', optional => 0 },
	);
	my $model = $parm{'model'};
	my $problem_index = $parm{'problem_index'};
	my $skip_omegas = $parm{'skip_omegas'};
	my $input_model_fix_omegas = $parm{'input_model_fix_omegas'};

	my $start_omega_record = scalar(@{$skip_omegas})+1;
	my @parameter_etanumbers = ();
	my $skip_etas = 0;
	my @fix_omegas;
	my $need_to_move = 0;

	my $maxeta =  $model->problems()->[0]->nomegas(with_correlations => 0,
												   with_same => 1);


	#we assume skip_omegas already sorted ascending thanks to input_checking.pm
	for (my $i=0; $i<scalar(@{$skip_omegas}); $i++){
		unless ($skip_omegas->[$i] == ($i+1)){
			$need_to_move=1;
			last;
		}
	}

	if ($need_to_move){
		my @old_omega_order = (1 .. scalar(@{$model->problems->[$problem_index]->omegas}));
		my @new_omega_order = @{$skip_omegas};
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
		my @old_etas = (1 .. $maxeta);
		my @intermediate_etas =();
		foreach my $eta (@old_etas){
			push(@intermediate_etas,'o'.$eta);
		}
		
		#for each omega find old eta numbers and new eta numbers
		my $etas_per_omega = model::problem::etas_per_omega(problem => $model->problems->[0]);
		
		foreach my $coderec ('error','des','pk','pred'){ #never any ETAs in $MIX
			my $acc = $coderec.'s';
			if (defined $model->problems->[0]->$acc and 
				scalar(@{$model->problems->[0]->$acc})>0 ) {
				my @code = @{$model->problems->[0]->$acc->[0]->code};
#				print "\n old ".join(' ',@old_etas)."\n";
#				print "\n intermediate ".join(' ',@intermediate_etas)."\n";
#				print "before\n".join(' ',@code)."\n";
				# rename all existing ETA\((\d+)\) to ETA(o\d+)
				renumber_etas(code => \@code,
							  eta_from => [\@old_etas],
							  eta_to => [\@intermediate_etas]);
#				print "\n intermediate ".join(' ',@code)."\n";
				#for each omega record
				#rename from ETA\(o(\d+)\) to ETA(newnum) , also if oldnum and newnum the same
				my $new_eta_count = 0;
				my @from = ();
				my @to = ();
				foreach my $pos (@new_omega_order) {
					my $j = $pos-1;
					my $size = scalar(@{$etas_per_omega->[$j]});
					foreach my $eta (@{$etas_per_omega->[$j]}){
						push(@from,$intermediate_etas[($eta-1)]); #with o prefix
					}
					push(@to,(($new_eta_count+1) .. ($new_eta_count + $size)));
					$new_eta_count += $size;
				}
#				print "\n from ".join(' ',@from)."\n";
#				print "\n to ".join(' ',@to)."\n";
				renumber_etas(code => \@code,
							  eta_from => [\@from],
							  eta_to => [\@to]);
				
#				print "after\n".join(' ',@code)."\n";
				$model->problems->[0]-> set_records( type => $coderec,	
													 record_strings => \@code );
			}
		}

		#reorder omega records
		my @new_records = ();
		my $n_previous_rows = 0;
		for (my $k=0; $k<scalar(@new_omega_order); $k++){
			if ($k==scalar(@{$skip_omegas})){
				$skip_etas = $n_previous_rows;
			}
			my $i = $new_omega_order[$k]-1; #old index
			my $size = scalar(@{$etas_per_omega->[$i]});
			my @lines =();
			my $formatted = $model->problems->[0]->omegas->[$i]->_format_record();
			for (my $j=0; $j < scalar(@{$formatted}); $j++){
				push(@lines,split("\n",$formatted->[$j]));
			}
			push(@new_records,
				 model::problem::omega->new(record_arr => \@lines, 
											n_previous_rows => $n_previous_rows));
			$n_previous_rows += $size;
		}
		$model -> problems -> [0]-> omegas(\@new_records);
		
		@fix_omegas = @{get_or_set_fix(model => $model,
									   type => 'omegas')};
		
	}
	@fix_omegas = @{get_or_set_fix(model => $model,
								   type => 'omegas',
								   stop_record => ($start_omega_record-1))};

	my $etas_per_omega = model::problem::etas_per_omega(problem => $model->problems->[0]);
	for (my $j=0; $j< scalar(@{$etas_per_omega}); $j++){
		if ($j >= scalar(@{$skip_omegas})){
			push(@parameter_etanumbers,$etas_per_omega->[$j]);
		}
	}
	return $skip_etas,\@fix_omegas,$start_omega_record,\@parameter_etanumbers;
}

sub get_filled_omega_block
{
	#must have already done update inits on model so that get_matrix is estimated values, where available
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  problem_index  => { isa => 'Int', default => 0 },
							  table_index  => { isa => 'Int', default => 0 },
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
	($corrmatrix,$message) = get_correlation_matrix_from_phi(start_eta_1 => $start_eta_1,
															 end_eta_1 => $end_eta_top, #can be from multiple blocks here, or end_eta_1
															 start_eta_2 => $start_eta_2,
															 end_eta_2 => $end_eta_2,
															 problem_index => $problem_index,
															 table_index => $table_index,
															 model => $model);
	return([],$message) unless (length($message) == 0);

	@sd = (0) x ($total_size);
	for (my $i=0; $i<($total_size); $i++){
		push(@mergematrix,[(0) x $total_size]);
	}
	
	#omega block. Do not assume all that are nonzero are estimated
	#get inits from model. local coords

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
			if (($j >= $sizes[0]) and ($j < $top_size)){
				$mergematrix[$i]->[$j] = $smallnum;
			}else{
				#compute new
				$mergematrix[$i]->[$j] = ($corrmatrix->[$i][$j])*($sd[$i])*($sd[$j]);
			}
		}
	}

	my ($posdefmatrix,$diff)=linear_algebra::get_symmetric_posdef(\@mergematrix);
	
	return($posdefmatrix,'');	
}

sub get_correlation_matrix_from_phi
{
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  problem_index  => { isa => 'Int', default => 0 },
							  table_index  => { isa => 'Int', default => 0 },
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

sub get_CTV_parameters
{
	#find union of bov_parameters and additional TVpars that have ETA on them in input model
	#in frem now bov_parameters not used
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  bov_parameters => { isa => 'ArrayRef', optional => 1 },
	);
	my $model = $parm{'model'};
	my $bov_parameters = $parm{'bov_parameters'};
	$bov_parameters = [] unless (defined $bov_parameters);
	
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
							  covariates => { isa => 'ArrayRef', optional => 0 },
							  bov_parameters => { isa => 'ArrayRef', default => [] },
							  time_varying => { isa => 'ArrayRef', default => [] },
							  etanum_to_parameter => { isa => 'HashRef', optional => 0 },
							  occasionlist => { isa => 'ArrayRef', default => [] },
							  occasion => { isa => 'Str', default => '' },
							  start_eta => { isa => 'Int', optional => 0 },
							  bsv_parameters => { isa => 'Int', optional => 0 },
		);
	my $covariates = $parm{'covariates'};
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

	if (scalar(@{$covariates}) > 0){
		for (my $i=$start_eta;$i< ($start_eta + $bsv_parameters);$i++){
			if (defined $etanum_to_parameter->{$i}){
				push(@bsv_par_labels,'BSV par '.$etanum_to_parameter->{$i});
			}else{
				push(@bsv_par_labels,'BSV par ');
			}
		}
	}

	for (my $i=0; $i< scalar(@{$covariates}); $i++){
		push(@bsv_cov_labels,'BSV cov '.$covariates->[$i]);
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
							  skip_etas => {isa => 'Int', optional => 0},
	);
	my $model = $parm{'model'};
	my $skip_etas = $parm{'skip_etas'};

	my $start_omega_record = $model-> problems -> [0]->check_skip_etas(skip_etas => $skip_etas);
	my $ref = $model->problems->[0]->get_eta_sets(header_strings => 0,
												  skip_etas =>$skip_etas);
	if (scalar(@{$ref->{'iov'}})>0){
		croak("Cannot have BOV ETAs with numbers > skip_etas, not supported in frem");
	}
	unless (scalar(@{$ref->{'iiv'}})>0){
		croak("No ETAs left after skip_etas, nothing to do in frem");
	}
	
	return $start_omega_record;
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
	my $form = '  %.6G';
	for (my $row=0; $row< $size; $row++){
		my $line;
		for (my $col=0; $col<=$row; $col++){
			my $str= sprintf("$form",$new_omega->[$row][$col]); 
			$line = $line.' '.$str;
		}
		my $comment ='';
		$comment = '; '.$labels->[$row] if (defined $labels and scalar(@{$labels}) > $row);
		push(@record_lines,$line.$comment);
	}
	return \@record_lines;
}

sub set_model2_omega_blocks
{
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  start_omega_record => {isa => 'Int', optional => 0},
							  skip_etas => {isa => 'Int', optional => 0},
							  covariate_covmatrix => {isa => 'ArrayRef', optional => 0},
							  covariate_labels => {isa => 'ArrayRef', optional => 0},
	);

	my $model = $parm{'model'};
	my $start_omega_record = $parm{'start_omega_record'};
	my $skip_etas = $parm{'skip_etas'};
	my $covariate_covmatrix = $parm{'covariate_covmatrix'};
	my $covariate_labels = $parm{'covariate_labels'};

	my $covariate_size = scalar(@{$covariate_covmatrix});
	croak("too few labels") unless (scalar(@{$covariate_labels}) == $covariate_size);
	
	my @covariate_etanumbers = ();
	
	my @covariate_code = ();
	
	my $n_previous_rows =  $model->problems()->[0]->nomegas(with_correlations => 0,
															with_same => 1);
	my $omega_lines = get_omega_lines(new_omega => $covariate_covmatrix,
									  labels => $covariate_labels);
	
	push(@{$model -> problems -> [0]-> omegas},model::problem::omega->new(record_arr => $omega_lines, 
																		  n_previous_rows => $n_previous_rows));
	for (my $i=0; $i < scalar(@{$model -> problems -> [0]-> omegas}); $i++){
		if ($model -> problems -> [0]-> omegas->[$i]->type eq 'BLOCK'){
			$model -> problems -> [0]-> omegas->[$i]->fix(1) unless ($model -> problems -> [0]-> omegas->[$i]->same);
		}else{
			for (my $j=0; $j< scalar(@{$model -> problems -> [0]-> omegas->[$i]->options}); $j++){
				$model -> problems -> [0]-> omegas->[$i]->options->[$j]->fix(1);
			}
		}
	}
	
}

sub old_set_model2_omega_blocks
{ #not used, kept as backup
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  start_omega_record => {isa => 'Int', optional => 0},
							  skip_etas => {isa => 'Int', optional => 0},
							  parameter_blocks => {isa => 'ArrayRef', optional => 0},
							  covariate_covmatrix => {isa => 'ArrayRef', optional => 0},
							  covariate_labels => {isa => 'ArrayRef', optional => 0},
	);

	my $model = $parm{'model'};
	my $start_omega_record = $parm{'start_omega_record'};
	my $skip_etas = $parm{'skip_etas'};
	my $parameter_blocks = $parm{'parameter_blocks'};
	my $covariate_covmatrix = $parm{'covariate_covmatrix'};
	my $covariate_labels = $parm{'covariate_labels'};

	my $covariate_size = scalar(@{$covariate_covmatrix});
	croak("too few labels") unless (scalar(@{$covariate_labels}) == $covariate_size);
	
	my @omega_records = ();
	my @covariate_etanumbers = ();
	my @parameter_etanumbers = ();
	my @covariate_code = ();
	for (my $i=0; $i< ($start_omega_record-1);$i++){
		#if start_omega_record is 1 we will push nothing
		push(@omega_records,$model-> problems -> [0]->omegas->[$i]);
		if ($omega_records[$i]->type eq 'BLOCK'){
			$omega_records[$i]->fix(1) unless ($omega_records[$i]->same);
		}else{
			for (my $j=0; $j< scalar(@{$omega_records[$i]->options}); $j++){
				$omega_records[$i]->options->[$j]->fix(1);
			}
		}
	}
	
	my $n_previous_rows = $skip_etas;
	for (my $i=0; $i< scalar(@{$parameter_blocks}); $i++){
		push(@omega_records,$parameter_blocks->[$i]);
		$omega_records[-1]->fix(1);
		push(@parameter_etanumbers,[(($n_previous_rows+1) .. ($n_previous_rows+$parameter_blocks->[$i]->size) )]);
		$n_previous_rows += $parameter_blocks->[$i]->size;
		my @labels = ();
		foreach my $lab (@{$covariate_labels}){
			push(@labels,'BSV_'.$lab.'_'.($i+1));
		}
		my $omega_lines = get_omega_lines(new_omega => $covariate_covmatrix,
										  labels => \@labels);
		push(@omega_records,model::problem::omega->new(record_arr => $omega_lines, 
													   n_previous_rows => $n_previous_rows));
		push(@covariate_etanumbers,[(($n_previous_rows+1) .. ($n_previous_rows+$covariate_size) )]);
		for (my $j=0; $j< $covariate_size; $j++){
			push(@covariate_code,$indentation.$labels[$j].' = ETA('.($n_previous_rows+1+$j).')');
		}
		$n_previous_rows += $covariate_size;
	}
	$model -> problems -> [0]-> omegas(\@omega_records);
	return (\@covariate_etanumbers,\@parameter_etanumbers,\@covariate_code);
}

sub get_parameter_blocks
{
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  start_omega_record => {isa => 'Int', optional => 0},
							  skip_etas => {isa => 'Int', optional => 0},
							  n_covariates => {isa => 'Int', optional => 0},
	);
 	my $model = $parm{'model'};
	my $start_omega_record = $parm{'start_omega_record'};
	my $skip_etas = $parm{'skip_etas'};
	my $n_covariates = $parm{'n_covariates'};

	croak("must have n_covariates > 0") unless ($n_covariates > 0);
	
	my $n_previous_new_rows = $skip_etas;
	my $n_previous_old_rows = $skip_etas;
	my @parameter_blocks = ();
	my @eta_from =();
	my @eta_to = ();
	
	for (my $i=($start_omega_record-1); $i < scalar(@{$model-> problems -> [0]->omegas()}); $i++){
		if ($model->problems->[0]->omegas->[$i]->type eq 'BLOCK'){
			my $formatted = $model->problems->[0]->omegas->[$i]->_format_record();
			my @lines =();
			for (my $j=0; $j < scalar(@{$formatted}); $j++){
				push(@lines,split("\n",$formatted->[$j]));
			}
			#print "count ".scalar(@lines)."\n";
			push(@parameter_blocks,
				 model::problem::omega->new(record_arr => \@lines, 
											n_previous_rows => $n_previous_new_rows));
			my $size = $model->problems->[0]->omegas->[$i]->size; #cannot have BLOCK SAME w/o size, then error message earlier;
			push(@eta_from,[($n_previous_old_rows+1) .. ($n_previous_old_rows+$size)]);
			push(@eta_to,[($n_previous_new_rows+1) .. ($n_previous_new_rows+$size)]);
			$n_previous_new_rows += $size;
			$n_previous_old_rows += $size;
			$n_previous_new_rows += $n_covariates; #BSV_cov block to be inserted later
		}else{
			foreach my $opt (@{$model->problems->[0]->omegas->[$i]->options}){
				my ($formatted,$no_break) = $opt -> _format_option(is_block => 0); #is_blocks makes formatting add FIX if set
				push(@parameter_blocks,model::problem::omega->new(record_arr => ['BLOCK (1)',$formatted], 
																  n_previous_rows => $n_previous_new_rows));
				push(@eta_from,[($n_previous_old_rows+1)]);
				push(@eta_to,[($n_previous_new_rows+1)]);
				$n_previous_new_rows++;
				$n_previous_old_rows++;
				$n_previous_new_rows += $n_covariates; #BSV_cov block to be inserted later
			}
		}
	}
	return (\@parameter_blocks,{'eta_from' => \@eta_from, 'eta_to'=> \@eta_to});
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



sub do_model1
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model => { isa => 'Ref', optional => 0 }
	);
	my $model = $parm{'model'};

	my $name_model = 'model_1.mod';
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
									   copy_datafile   => 0,
									   copy_output => 0);
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
										base_directory	 => $self -> directory(),
										directory		 => $rundir,
										copy_data     => 0,
										models		 => [$frem_model],
										top_tool              => 0);
		tool::add_to_nmoutput(run => $run, extensions => ['phi','ext']);		
		ui -> print( category => 'all', message =>  'Estimating Model 1 (the input model)');
		$run-> run;
		if (defined $frem_model->outputs and (defined $frem_model->outputs->[0])){
			$output = $frem_model->outputs->[0] ;
		}
	}

	unless (defined $output){
		croak("No output from Model 1, cannot proceed with frem");
	}
	
	$frem_model->update_inits (from_output => $output);
	my $mod_ofv = $output->get_single_value(attribute=> 'ofv');
	
	return ($frem_model,$mod_ofv); 

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
						   filename => $self -> directory().'m1/filter_data_model.mod',
						   filtered_datafile => $filtered_datafile,
						   dv => $self->dv,
						   covariates => $self->covariates);
	
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
		my ($mapping,$new_indices,$new_categorical) = $filtered_data->append_binary_columns(indices => $categorical_indices,
																							start_header => $data_set_headers);
		$categorical_indices = $new_indices;
		$self->categorical($new_categorical); #these are now binary
		push(@cov_indices,@{$categorical_indices});
		push(@cov_names,@{$new_categorical});
		push(@is_log,(0) x scalar(@{$new_categorical}));
	}

#	print "\n".join("\t",@cov_names)."\n";
#	print join("\t",@cov_indices)."\n";
#	print join("\t",@{$data_set_headers})."\n";
	
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
							  mod0_ofv => { isa => 'Num', optional => 0 },
							  N_parameter_blocks => {isa => 'Int', optional => 0},
							  fremdataname => { isa => 'Str', optional => 0 },
	);
	my $model = $parm{'model'};
	my $mod0_ofv = $parm{'mod0_ofv'};
	my $indices = $parm{'indices'};
	my $filtered_data = $parm{'filtered_data'};
	my $N_parameter_blocks = $parm{'N_parameter_blocks'};
	my $fremdataname = $parm{'fremdataname'};
	
	my $do_check = $self->check;
	if (-e $self -> directory().'m1/'.$fremdataname){
		unlink($self -> directory().'m1/'.$fremdataname);
		$do_check = 0; #assume get same result second time
	}
	
	#this writes dataset to disk
	my $resultref = data::frem_compute_covariate_properties(filtered_data  => $filtered_data,
															invariant_covariates => $self->covariates,
															N_parameter_blocks => $N_parameter_blocks,
															is_log => $indices->{'is_log'},
															occ_index => undef,
															data2name => $fremdataname,
															evid_index => $indices->{'EVID'},
															mdv_index => $indices->{'MDV'},
															dv_index => $indices->{$self->dv},
															type_index => $indices->{$fremtype},
															cov_indices => $indices->{'cov_indices'});
#															first_timevar_type => scalar(@cov_indices));

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
		print "\nModel 1 ofv is    $mod0_ofv\n";
		print   "Data check ofv is $check_ofv\n";
	}
	
}

sub renumber_etas
{
    my %parm = validated_hash(\@_,
							  code => { isa => 'ArrayRef', optional => 0 },
							  eta_from => { isa => 'ArrayRef', optional => 0 },
							  eta_to => { isa => 'ArrayRef', optional => 0 },
		);
	my $code = $parm{'code'};
	my $eta_from = $parm{'eta_from'};
	my $eta_to = $parm{'eta_to'};

	croak('from and to lists must have equal length') 
		unless (scalar(@{$eta_from}) == scalar(@{$eta_to}));

	for (my $i=0; $i<scalar(@{$eta_from}); $i++){
		croak('from and to lists must have equal length') 
			unless (scalar(@{$eta_from->[$i]}) == scalar(@{$eta_to->[$i]}));
		for (my $j=0; $j<scalar(@{$eta_from->[$i]}); $j++){
			my $from = $eta_from->[$i]->[$j];
			my $to = $eta_to->[$i]->[$j];
			foreach (@{$code}){
				s/\bETA\($from\)/ETA\($to\)/g;
			}
		}
	}
}

sub prepare_model2
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
#							  parameter_blocks => {isa => 'ArrayRef', optional => 0},
#							  eta_mapping => {isa => 'HashRef', optional => 0},
							  fremdataname => { isa => 'Str', optional => 0 },
#							  start_omega_record => { isa => 'Int', optional => 0 },
#							  skip_etas => {isa => 'Int', optional => 0},
							  skip_omegas => {isa => 'ArrayRef', optional => 0},
	);
	my $model = $parm{'model'};
	my $fremdataname = $parm{'fremdataname'};
#	my $parameter_blocks = $parm{'parameter_blocks'}; #this is omega records w/ block form for parameters, ready for model2
#	my $eta_mapping = $parm{'eta_mapping'};
#	my $start_omega_record = $parm{'start_omega_record'};
	my $skip_omegas = $parm{'skip_omegas'};
	
#	my $N_parameter_blocks = scalar(@{$parameter_blocks});
	my $name_model = 'model_2.mod';
	
	my $frem_model;
	my $maxeta =  $model->problems()->[0]->nomegas(with_correlations => 0,
												   with_same => 1);
	
	my ($skip_etas,$fix_omegas,$start_omega_record,$parameter_etanumbers) = 
		put_skipped_omegas_first(model => $model,
								 skip_omegas => $skip_omegas,
								 input_model_fix_omegas => $self->input_model_fix_omegas);

	$self->input_model_fix_omegas($fix_omegas);
	$self->skip_etas($skip_etas);
	$self->start_omega_record($start_omega_record);
	
	my $ntheta = $model ->nthetas(problem_number => 1);
	my $newtheta = 0;
	my $epsnum = 1 + $model->problems()->[0]->nsigmas(with_correlations => 0,
													  with_same => 1);
	my $Ncov = scalar(@{$self->covariates});
	my $covariate_etanumbers = [(($maxeta+1) .. ($maxeta+$Ncov))] ;

	my @pk_pred_code =();
	my @labels = ();
	for (my $j=0; $j< scalar(@{$self->covariates}); $j++){
		my $label = 'BSV_'.$self->covariates->[$j];
		push(@pk_pred_code,$indentation.$label.' = ETA('.($maxeta+1+$j).')');
		push (@labels, $label);
	}
	
	unless (-e $self -> directory().'m1/'.$name_model){
		# input model  inits have already been updated
		#omegas have been reordered
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


#		foreach my $coderec ('error','des','pk','pred'){ #never any ETAs in $MIX
#			my $acc = $coderec.'s';
#			if (defined $frem_model->problems->[0]->$acc and 
#				scalar(@{$frem_model->problems->[0]->$acc})>0 ) {
#				my @extra_code = @{$frem_model->problems->[0]->$acc->[0]->code};
#				renumber_etas(code => \@extra_code,
#							  eta_from => $eta_mapping->{'eta_from'},
#							  eta_to => $eta_mapping->{'eta_to'});
#				$frem_model->problems->[0]-> set_records( type => $coderec,	
#														  record_strings => \@extra_code );
#			}
#		}
		
		#INPUT changes
		foreach my $item (@{$self->extra_input_items}){
			#mdv and fremtype
			$frem_model -> add_option(problem_numbers => [1],
									  record_name => 'input',
									  option_name => $item);
			#we do not have to add for example binary-ized categoricals, they enter in DV col for special fremtype
		}

		#SIGMA changes
		foreach my $record (@{$frem_model-> problems -> [0]->sigmas}){
			if ($record->type eq 'BLOCK'){
				$record->fix(1) unless ($record->same);
			}else{
				for (my $j=0; $j< scalar(@{$record->options}); $j++){
					$record->options->[$j]->fix(1);
				}
			}
		}

		$frem_model->add_records(type => 'sigma',
								 problem_numbers => [1],
								 record_strings => [$smallnum.' FIX ; EPSCOV']);

		set_model2_omega_blocks(model => $frem_model,
								start_omega_record => $start_omega_record,
								skip_etas => $skip_etas,
								covariate_covmatrix => $self->invariant_covmatrix,
								covariate_labels => \@labels);


		#THETA changes
		#FIX all existing
		for (my $i=0; $i< scalar(@{$frem_model->problems->[0]->thetas}); $i++){
			for (my $j=0; $j< scalar(@{$frem_model->problems->[0]->thetas->[$i]->options}); $j++){
				$frem_model->problems->[0]->thetas->[$i]->options->[$j]->fix(1);
			}
		}

		
		my @theta_strings =();
		my @covariate_thetanumbers = ();
		for (my $i=0; $i< scalar(@{$self->covariates}); $i++){
			my $val=$self->invariant_median->[$i];
			$val=0.001 if ($val==0);
			my $label = 'TV_'.$self->covariates->[$i];
			push(@theta_strings,' '.sprintf("%.12G",$val).'; '.$label);
			$newtheta++;
			my $num = ($ntheta+$newtheta);
			push(@covariate_thetanumbers,$num);
			push(@pk_pred_code,$indentation.$label.'=THETA('.$num.')');
		}
		
		$frem_model->add_records(type => 'theta',
								 problem_numbers => [1],
								 record_strings => \@theta_strings);
		
		add_pk_pred_error_code(model=>$frem_model,
							   pk_pred_code => \@pk_pred_code,
#							   N_parameter_blocks => $N_parameter_blocks,
							   N_parameter_blocks => 1,
							   covariates => $self->covariates,
							   epsnum => $epsnum,
							   use_pred => $self->use_pred);

		#FIXME use spdarise
		$frem_model-> problems -> [0]->ensure_posdef();
		$frem_model->_write();

	}

	return ($ntheta,$epsnum,$covariate_etanumbers,$parameter_etanumbers);
		
}

sub prepare_model3
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  start_omega_record => { isa => 'Int', optional => 0 },
							  parcov_blocks => { isa => 'ArrayRef', optional => 0}
	);
	my $model = $parm{'model'};
	my $start_omega_record = $parm{'start_omega_record'};
	my $parcov_blocks = $parm{'parcov_blocks'};

	my $modnum=3;

	my $name_model = 'model_'.$modnum.'.mod';
	my $frem_model;
	my $est_records = $model->problems->[0]->estimations;
	
	unless (-e $self -> directory().'m1/'.$name_model){
		# input model  inits have already been updated
		$frem_model = $model ->  copy( filename    => $self -> directory().'m1/'.$name_model,
									   output_same_directory => 1,
									   write_copy => 0,
									   copy_datafile   => 0,
									   copy_output => 0);

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
		$frem_model->_write();

	}

	return ($est_records);

}

sub prepare_model4
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  start_omega_record => { isa => 'Int', optional => 0 },
							  parcov_blocks => { isa => 'ArrayRef', optional => 0},
							  est_records => { isa => 'ArrayRef', optional => 0}
	);
	my $model = $parm{'model'};
	my $start_omega_record = $parm{'start_omega_record'};
	my $parcov_blocks = $parm{'parcov_blocks'};
	my $est_records = $parm{'est_records'};
	
	my $modnum=4;

	my $name_model = 'model_'.$modnum.'.mod';
	my $frem_model;
	
	unless (-e $self -> directory().'m1/'.$name_model){
		# input model  inits have already been updated
		$frem_model = $model ->  copy( filename    => $self -> directory().'m1/'.$name_model,
									   output_same_directory => 1,
									   write_copy => 0,
									   copy_datafile   => 0,
									   copy_output => 0);

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

		$frem_model -> problems -> [0]-> omegas(\@omega_records);
		$frem_model -> problems -> [0]->estimations($est_records);
		$frem_model->_write();

	}


}

sub prepare_model5
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  start_omega_record => { isa => 'Int', optional => 0 },
							  first_cholesky_theta => { isa => 'Int', optional => 0 },
							  parameter_etanumbers => { isa => 'ArrayRef', optional => 0 },
	);
	my $start_omega_record = $parm{'start_omega_record'};
	my $first_cholesky_theta = $parm{'first_cholesky_theta'};
	my $parameter_etanumbers = $parm{'parameter_etanumbers'};
	
	my $modnum=5;

	my $name_model = 'model_'.$modnum.'.mod';
	my $frem_model;
	
	unless (-e $self -> directory().'m1/'.$name_model){
		#read model 4 from disk, then copy it
		my $model = model->new( %{common_options::restore_options(@common_options::model_options)},
								filename                    => 'm1/model_4.mod',
								ignore_missing_output_files => 1 );

		$frem_model = $model ->  copy( filename    => $self -> directory().'m1/'.$name_model,
									   output_same_directory => 1,
									   write_copy => 0,
									   copy_datafile   => 0,
									   copy_output => 0);

		#SIGMA fix all existing
		foreach my $record (@{$frem_model-> problems -> [0]->sigmas}){
			if ($record->type eq 'BLOCK'){
				$record->fix(1) unless ($record->same);
			}else{
				for (my $j=0; $j< scalar(@{$record->options}); $j++){
					$record->options->[$j]->fix(1);
				}
			}
		}
		#OMEGA fix all before $start_omega
		for (my $i=0; $i<($start_omega_record-1); $i++){
			my $record = $frem_model-> problems -> [0]->omegas->[$i];
			if ($record->type eq 'BLOCK'){
				$record->fix(1) unless ($record->same);
			}else{
				for (my $j=0; $j< scalar(@{$record->options}); $j++){
					$record->options->[$j]->fix(1);
				}
			}
		}

		#THETA changes
		#FIX all existing
		for (my $i=0; $i< scalar(@{$frem_model->problems->[0]->thetas}); $i++){
			for (my $j=0; $j< scalar(@{$frem_model->problems->[0]->thetas->[$i]->options}); $j++){
				$frem_model->problems->[0]->thetas->[$i]->options->[$j]->fix(1);
			}
		}
		my $dimension = $frem_model->problems->[0]->omegas->[$start_omega_record-1]->size;
		my $top_size = $dimension - scalar(@{$self->covariates});
		#do cholesky
		my $warnings = 
			$frem_model->problems->[0]->cholesky_reparameterize(what => 'o'.$start_omega_record,
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
		
		$frem_model-> check_and_set_sizes(LTH => 1); #set LTH if too many thetas. TODO make dependent on nm_version
	
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
		);
	my $model = $parm{'model'};
	my $start_omega_record = $parm{'start_omega_record'};
	my $first_cholesky_theta = $parm{'first_cholesky_theta'};
	my $parameter_etanumbers = $parm{'parameter_etanumbers'};
	
	my $modnum=6;
	my $name_model = 'model_'.$modnum.'.mod';
	my $frem_model;
	
	unless (-e $self -> directory().'m1/'.$name_model){
		$frem_model = $model ->  copy( filename    => $self -> directory().'m1/'.$name_model,
									   output_same_directory => 1,
									   write_copy => 0,
									   copy_datafile   => 0,
									   copy_output => 0);

		get_or_set_fix(model => $frem_model,
					   type => 'thetas',
					   set_array => $self->input_model_fix_thetas);
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




sub run_unless_run
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 numbers => { isa => 'ArrayRef', optional => 0 }
	);
	my $numbers = $parm{'numbers'};

	croak("no numbers to run") unless (scalar(@{$numbers})>0 and defined $numbers->[0]);

	my @models = ();	
	my $do_run = 0;
	
	for (my $i=0; $i<scalar(@{$numbers}); $i++){
		#reread from disk so that omegas are properly stored
		my $name_model = 'model_'.$numbers->[$i].'.mod';
		push(@models,model->new( %{common_options::restore_options(@common_options::model_options)},
								 filename                    => 'm1/'.$name_model,
								 ignore_missing_output_files => 1 ));
		unless ($models[$i]->is_run){
			$do_run = 1;
		}
	}
	if ($do_run){
		my $rundir = $self -> directory().'/model'.join('_',@{$numbers}).'_modelfit_dir1';
		rmtree([ "$rundir" ]) if (-e $rundir);

		my $run = tool::modelfit ->new( %{common_options::restore_options(@common_options::tool_options)},
										base_directory	 => $self -> directory(),
										directory		 => $rundir,
										copy_data     => 0,
										models		 => \@models,
										top_tool              => 0);
		tool::add_to_nmoutput(run => $run, extensions => ['phi','ext']);		
		my $text = 'Estimating ';
		$text = 'Evaluating ' if ($numbers->[0] == 3);
		$text .= 'Model '.join(' and ',@{$numbers});
		ui -> print( category => 'all', message =>  $text);
		$run-> run;
	}

	
	if (scalar(@{$numbers}) > 1){
		return \@models; #final estimation
	}else{
		if (defined $models[0]->outputs and (defined $models[0]->outputs->[0])){
			$models[0]->update_inits(from_output=> $models[0]->outputs->[0]) ;
		}else{
			croak("No output from Model ".$numbers->[0].", cannot proceed with frem");
		}
		return $models[0];
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

	#this runs input model, if necessary
	my ($frem_model1,$mod1_ofv)=  $self-> do_model1(model => $model);
	
	#this modifies $self->covariates
	my ($filtered_data,$indices) = $self->do_filter_dataset_and_append_binary(model => $frem_model1);

	my $frem_dataset = 'frem_dataset.dta';
	$self->do_frem_dataset(model => $frem_model1,
						   N_parameter_blocks => 1,
						   filtered_data => $filtered_data,
						   indices => $indices,
						   mod0_ofv => $mod1_ofv,
						   fremdataname => $frem_dataset);
	
	my ($ntheta,$epsnum,$covariate_etanumbers,$parameter_etanumbers) = 
		$self->prepare_model2(model => $frem_model1,
							  fremdataname => $frem_dataset,
							  skip_omegas => $self->skip_omegas
		);
	
	my $frem_model2 = $self->run_unless_run(numbers => [2]);

	my $mod3_parcov_block = get_parcov_blocks(model => $frem_model2,
											  skip_etas => $self->skip_etas,
											  covariate_etanumbers => $covariate_etanumbers,
											  parameter_etanumbers => $parameter_etanumbers,
											  start_omega_record => $self->start_omega_record);
	
	my $est_records = $self->prepare_model3(model => $frem_model2,
											start_omega_record => $self->start_omega_record,
											parcov_blocks => $mod3_parcov_block);

	my $frem_model3 = $self->run_unless_run(numbers => [3]);

	my $mod4_parcov_block = get_parcov_blocks(model => $frem_model3,
											  skip_etas => $self->skip_etas,
											  covariate_etanumbers => $covariate_etanumbers,
											  parameter_etanumbers => $parameter_etanumbers,
											  start_omega_record => $self->start_omega_record);
	
	$self->prepare_model4(model => $frem_model3,
						  start_omega_record => $self->start_omega_record,
						  parcov_blocks => $mod4_parcov_block,
						  est_records => $est_records);

	$self->prepare_model5(start_omega_record => $self->start_omega_record,
						  first_cholesky_theta => scalar(@{$frem_model3->problems->[0]->thetas}),
						  parameter_etanumbers => $parameter_etanumbers);
						  
	my $frem_model5 = $self->run_unless_run(numbers => [5]);

	$self->prepare_model6(model => $frem_model5,
						  first_cholesky_theta => scalar(@{$frem_model3->problems->[0]->thetas}),
						  start_omega_record => $self->start_omega_record,
						  parameter_etanumbers => $parameter_etanumbers);

	#FIXME subtool instead?
	my @final_numbers = ();

	push(@final_numbers,4) if $self->estimate_regular_final_model;
	push(@final_numbers,6) if $self->estimate_cholesky_final_model;

	my $final_models = $self->run_unless_run(numbers => \@final_numbers) if (scalar(@final_numbers)>0);
	
	if ($self->vpc()){
		#FIXME we renumber according to eta_mapping, should get_CTV be done after or before that?
		my $bsv_parameter_count; #FIXME
		my ($CTV_parameters,$etanum_to_parameter) = get_CTV_parameters(model => $frem_model1);

		my $labelshash = create_labels(	 covariates => $self->covariates,
										 etanum_to_parameter => $etanum_to_parameter,
										 start_eta => ($self->skip_etas +1),
										 bsv_parameters => $bsv_parameter_count);
		my $BSV_par_block; #FIXME
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
							 start_omega_record => $self->start_omega_record	);

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
							  bov_parameters => { isa => 'Int', default => 0 },
							  dv  => { isa => 'Str', optional => 0 },
							  time_varying  => { isa => 'ArrayRef', default => [] },
							  covariates  => { isa => 'ArrayRef', optional => 0 },
							  occasion  => { isa => 'Str', default => '' },
		);

	my $model = $parm{'model'};
	my $filename = $parm{'filename'};
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
		
		$message = "Running with MAXEVAL=0 to filter data and add MDV ".$fremtype." for FREM data set";
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
			' NOAPPEND NOPRINT ONEHEADER FORMAT=sG15.7 FILE='.$filtered_datafile]);

	return ($filtered_data_model,\@filter_table_header,$extra_input_items,$message);

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
						 covariates => $self->covariates,
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
		if (scalar(@{$self->covariates}) > 0){
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

sub set_frem_records
{ #not used
	my %parm = validated_hash(\@_,
							  model => { isa => 'Ref', optional => 0 },
							  skip_covariates => { isa => 'Bool', default => 0 },
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
							  covariates =>  { isa => 'ArrayRef', optional => 0 },
							  time_varying =>  { isa => 'ArrayRef', optional => 0 },
							  parameters_bov =>  { isa => 'ArrayRef', optional => 0 },
		);
	my $model = $parm{'model'}; #this will always be model1
	my $skip_covariates = $parm{'skip_covariates'};
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
	my $covariates = $parm{'covariates'};
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

	my $n_covariates = scalar(@{$covariates});
	my $n_time_varying = scalar(@{$time_varying});

	my %labelshash = %{create_labels(covariates => $covariates,
									 etanum_to_parameter => {},
									 start_eta => $start_eta,
									 bsv_parameters => $bsv_parameters)};


    my $n_occasions = scalar(@{$occasionlist});


    #OMEGA changes starting from Mod1
	#BSV part
    if ((not $skip_covariates) and ($n_covariates > 0) and (not $vpc)){
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
				   skip_covariates => $skip_covariates,
				   skip_time_varying => $skip_time_varying,
				   model_type => $model_type,
				   vpc => $vpc,
				   start_eta => $start_eta,
				   epsnum => $epsnum,
				   ntheta => $ntheta,
				   bsv_parameters => $bsv_parameters,
				   occasionlist => $occasionlist,
				   occasion => $occasion,
				   covariates => $covariates,
				   time_varying => $time_varying,
				   parameters_bov => $parameters_bov);
	
}

sub	add_pk_pred_error_code
{
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 },
							  pk_pred_code => { isa => 'ArrayRef', optional => 0 },
							  N_parameter_blocks => { isa => 'Int', optional => 0 },
							  covariates => { isa => 'ArrayRef', optional => 0 },
							  epsnum => { isa => 'Int', optional => 0 },
							  use_pred => { isa => 'Bool', optional => 0 },
		);
	my $model = $parm{'model'}; 
	my $pk_pred_code = $parm{'pk_pred_code'};
	my $N_parameter_blocks = $parm{'N_parameter_blocks'};
	my $covariates = $parm{'covariates'};
	my $epsnum = $parm{'epsnum'};
	my $use_pred = $parm{'use_pred'};

    my @code;
	if ($use_pred){
		@code = @{$model->get_code(record => 'pred')};
	}else{
		@code = @{$model->get_code(record => 'pk')};
	}
	
    #PK/PRED changes at beginning A
	my @begin_code =(';;;FREM CODE BEGIN');
	push(@begin_code,@{$pk_pred_code});
	push(@begin_code,';;;FREM CODE END' );


    my @end_code =();

	@end_code = (';;;FREM CODE BEGIN');
	for (my $i=0; $i< scalar(@{$covariates}); $i++){
		for (my $j=0; $j< $N_parameter_blocks; $j++){
			my $label = '_'.$covariates->[$i];
			if ($N_parameter_blocks > 1){
				$label .= '_'.($j+1);
			}
			push(@end_code, $indentation.'Y'.$label.' = TV'.$label.' + BSV'.$label);
		}
	}
	for (my $i=0; $i< scalar(@{$covariates}); $i++){
		for (my $j=0; $j< $N_parameter_blocks; $j++){
			my $label = '_'.$covariates->[$i];
			if ($N_parameter_blocks > 1){
				$label .= '_'.($j+1);
			}
			my $num = 100*($i+1)+$j;
			push(@end_code,$indentation.'IF ('.$fremtype.'.EQ.'.$num.') THEN' );
			push(@end_code,$indentation.'   Y = Y'.$label.'+EPS('.$epsnum.')' );
			push(@end_code,$indentation.'   IPRED = Y'.$label );
			push(@end_code,$indentation.'END IF' );
		}
	}
	push(@end_code,';;;FREM CODE END' );
	
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

sub set_frem_code
{
	my %parm = validated_hash(\@_,
							  model => { isa => 'Ref', optional => 0 },
							  skip_covariates => { isa => 'Bool', optional => 0 },
							  skip_time_varying => { isa => 'Bool', optional => 0 },
							  model_type => { isa => 'Int', optional => 0 },
							  vpc => { isa => 'Bool', optional => 0 },
							  start_eta => { isa => 'Int', optional => 0 },
							  epsnum => { isa => 'Int', optional => 0 },
							  ntheta => { isa => 'Int', optional => 0 },
							  bsv_parameters => { isa => 'Int', optional => 0 },
							  occasionlist =>  { isa => 'ArrayRef', optional => 0 },
							  occasion =>  { isa => 'Str', optional => 0 },
							  covariates =>  { isa => 'ArrayRef', optional => 0 },
							  time_varying =>  { isa => 'ArrayRef', optional => 0 },
							  parameters_bov =>  { isa => 'ArrayRef', optional => 0 },
		);
	my $model = $parm{'model'}; #this will always be model1
	my $skip_covariates = $parm{'skip_covariates'};
	my $skip_time_varying = $parm{'skip_time_varying'};
	my $model_type = $parm{'model_type'};
	my $epsnum = $parm{'epsnum'};
	my $ntheta = $parm{'ntheta'};
	my $bsv_parameters = $parm{'bsv_parameters'};
	my $start_eta = $parm{'start_eta'};
	my $vpc = $parm{'vpc'};
	my $occasionlist = $parm{'occasionlist'};
	my $occasion = $parm{'occasion'};
	my $covariates = $parm{'covariates'};
	my $time_varying = $parm{'time_varying'};
	my $parameters_bov = $parm{'parameters_bov'};

    #in is ref of model
    #model_type 2 or 3
    #epsnum
    #ntheta
    #this sets code

	my $n_covariates = scalar(@{$covariates});
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
	
	if ((not $skip_covariates) and (not $vpc) and ($n_covariates > 0)){
		#this is for BSV_cov
		for (my $i=0; $i< $n_covariates; $i++){
			push(@begin_code,'BSV'.$covariates->[$i].' = ETA('.($start_eta+$bsv_parameters+$i).')' );
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
				my $offset = ($start_eta-1)+$bsv_parameters+$n_covariates+$i*(scalar(@{$parameters_bov}));
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
					my $offset = ($start_eta-1)+$bsv_parameters+$n_covariates;
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
				my $offset = ($start_eta-1)+$bsv_parameters+$n_covariates+$i*((scalar(@{$parameters_bov}))+$n_time_varying);
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
		for (my $i=0; $i< $n_covariates; $i++){
			push(@end_code,'Y'.($i+1).' = THETA('.($ntheta+$i+1).') + BSV'.$covariates->[$i]);
		}
		for (my $i=0; $i< $n_time_varying; $i++){
			push(@end_code,'Y'.($n_covariates+$i+1).' = THETA('.($ntheta+$n_covariates+$i+1).') + BOV'.$time_varying->[$i]);
		}
		for (my $i=1; $i<=($n_covariates+$n_time_varying); $i++){
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
sub create_template_models
{ #not used
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

sub olddo_model1
{#not used
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

		if (scalar(@{$self->covariates}) > 0){
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
		

		if (scalar(@{$self->covariates}) > 0){
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


no Moose;
__PACKAGE__->meta->make_immutable;
1;
