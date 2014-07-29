package tool::lasso;

use include_modules;
use tool::xv;
use tool::modelfit;
use Math::Random;
use Data::Dumper;
use Cwd;
use OSspecific;
use Moose;
use MooseX::Params::Validate;

extends 'tool';

has 'relations' => ( is => 'rw', required => 1, isa => 'Str' );
has 'covariate_statistics_file' => ( is => 'rw', isa => 'Str', default => 'covariate_statistics.txt' );
has 'lasso_model_file' => ( is => 'rw', isa => 'Str', default => 'lasso_start_model.mod' );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['lasso.log'] } );
has 'blank' => ( is => 'rw', isa => 'Str' );
has 'row_length' => ( is => 'rw', isa => 'Int', default => 80 );
has 'dec_str' => ( is => 'rw', isa => 'Str' );
has 'sign_dec_str' => ( is => 'rw', isa => 'Str' );
has 'model_optimal' => ( is => 'rw', isa => 'model' );
has 'NOABORT_added' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'use_pred' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'lst_file' => ( is => 'rw', isa => 'Str' );
has 'convergence' => ( is => 'rw', isa => 'Str', default => 'FIRSTMIN' );
has 'statistics' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'cutoff' => ( is => 'rw', isa => 'Num', default => 0.005 );
has 'stratify_on' => ( is => 'rw', isa => 'Str' );
has 'warnings' => ( is => 'rw', isa => 'Int', default => 0 );
has 'step_t' => ( is => 'rw', isa => 'Num', default => 0.05 );
has 'start_t' => ( is => 'rw', isa => 'Num', default => 0 );
has 'stop_t' => ( is => 'rw', isa => 'Num', default => 1 );
has 'pred_ofv_start_t' => ( is => 'rw', isa => 'Num' );
has 'groups' => ( is => 'rw', isa => 'Int', default => 5 );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'lasso_results.csv' );


sub BUILD
{
	my $self  = shift;

	if ( scalar (@{$self -> models->[0]-> problems}) != 1 ){
		croak('The input model must contain exactly one problem.');
	}

	if (defined $self->lst_file()){
		#create output object to check that can be parsed correctly, and to 
		#extract data for error checking
		my $outputObject= output -> new(filename => '../'.$self -> lst_file);
		unless ($outputObject->parsed_successfully()){
			croak("lst file ".$self->lst_file." could not be parsed.");
		}
	}elsif (defined $self-> models->[0] ->outputs() and 
		defined $self-> models->[0] ->outputs()->[0] and
		$self-> models->[0] ->outputs()->[0]-> have_output()){
		1;
	}else{
		croak("No output found for model. Set option -lst_file.");
	}

	if ($self->groups()<2){
		croak("groups must be at least 2");
	}
	if ($self->step_t()==0) {
		croak("step_t cannot be 0");
	}
	unless ($self->start_t()>=0) {
		croak("start_t must not be smaller than 0");
	}
	unless ($self->stop_t()>=0) {
		croak("stop_t must not be smaller than 0");
	}

	if ($self->step_t()>0) {
		if ($self->stop_t()< $self->start_t()) {
			croak("stop_t cannot be smaller than start_t when step_t is positive");
		}
	}else{
		if ($self->stop_t()> $self->start_t()) {
			croak("stop_t cannot be larger than start_t when step_t is negative");
		}
	}

	unless ($self->convergence() =~ /^(REACHMAX|FIRSTMIN|HALT)$/){
		croak("convergence criterion must be either REACHMAX,".
			" FIRSTMIN or HALT.");
	}

	foreach my $model ( @{$self -> models} ) {
		foreach my $problem (@{$model->problems()}){
			if (defined $problem->nwpri_ntheta()){
				ui -> print( category => 'all',
					message => "Warning: lasso does not support \$PRIOR NWPRI.",
					newline => 1);
				last;
			}
		}
	}

	{
		my $ldir;
		my $name;

		($ldir, $name) = OSspecific::absolute_path($self->directory, $self->logfile->[0]);
		$self->logfile->[0] = $ldir.$name;
		($ldir, $name) = OSspecific::absolute_path($self->directory, $self->raw_results_file->[0]);
		$self->raw_results_file->[0] = $ldir.$name;
	}

	foreach my $attribute ( 'covariate_statistics_file', 'lasso_model_file') {
		my $name = $self -> {$attribute};
		my $ldir;
		( $ldir, $name ) = OSspecific::absolute_path( $self -> directory, $name );
		$self -> {$attribute} = $ldir.$name;
	}
}


sub parse_row
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		parse_str => { isa => 'Str', optional => 1 },
		parse_operator => { isa => 'Str', optional => 1 },
		max_length => { isa => 'Int', optional => 1 }
	);
	my $parse_str = $parm{'parse_str'};
	my $parse_operator = $parm{'parse_operator'};
	my $max_length = $parm{'max_length'};
	my @rows;

	my $variable_name;
	my $len;
	my $tmp_str;
	my $done = 0;
	if (length($parse_str)<$max_length) {
		push(@rows,$parse_str . "\n");
	}elsif (index($parse_str,"=")==-1){
		push(@rows,$parse_str . "\n");
	}else{
		$variable_name = substr($parse_str,0,index($parse_str,"="));
		$variable_name  =~ s/^\s+//;
		$variable_name   =~ s/\s+$//;

		my $op_index = index($parse_str,$parse_operator);
		if ($op_index==-1) {
			push(@rows,$parse_str . "\n");
		}else{
			while (length($parse_str)>$max_length)
			{
				my $i = $op_index+1;
				$len=length($parse_str)-1;
				while (index($parse_str,$parse_operator,$i)!=-1
					&& $i<=$max_length && $i<=$len)	{
					if (index($parse_str,$parse_operator,$i)<=$max_length) {
						$op_index = index($parse_str,$parse_operator,$i);
					} else{
						last;
					}
					$i = $op_index+1;
				}
				$tmp_str = substr($parse_str,0,$op_index);
				push @rows, $tmp_str ."\n";
				$parse_str = substr($parse_str,0,index($parse_str,"=")+1) . " " .
				$variable_name .$parse_operator .
				substr($parse_str,$op_index+length($parse_operator));
				$op_index = index($parse_str,$parse_operator);
				if ($op_index==-1){
					$done = 1;
				}

			}
			push (@rows, $parse_str ."\n") unless ($done);

		}
	}

	return \@rows;
}

sub setup_lasso_model
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		lasso_model => { isa => 'model', optional => 1 },
		parameter_covariate_form => { isa => 'HashRef', optional => 1 }
	);
	my $lasso_model = $parm{'lasso_model'};
	my %parameter_covariate_form = defined $parm{'parameter_covariate_form'} ? %{$parm{'parameter_covariate_form'}} : ();

	my $t_value = $self->start_t();
	$self->row_length(80);
	my $initial_value = 0.0001;  ##Initial value for thetas
	my $decimal_points =5;
	$self->blank("    ");
	$self->dec_str("%.$decimal_points". "f");
	$self->sign_dec_str("%+.$decimal_points". "f");
	my @old_code;
	@old_code = @{$lasso_model -> pk( problem_number => 1 )};
	$self->use_pred(0);
	unless ( $#old_code > 0 ) {
		@old_code = @{$lasso_model -> pred( problem_number => 1 )};
		$self->use_pred(1);
	}
	if ( $#old_code <= 0 ) {
		croak("Neither PK or PRED defined in " .
			$lasso_model -> filename . "\n" );
	}

	my @new_code;
	my @if_statements; #only once for each covariate, not for each param.
	my @zero_statements; #only once for each covariate, not for each param.
	my %if_printed;
	my @factor_code;
	my $nthetas =  $lasso_model->nthetas();
	my $old_thetas = $nthetas;
	unshift @new_code, ";;; LASSO-END\n";

	my $tmpstr;
	my $tmpfactor;

	## Create boundaries and inital values and labels for the THETAS
	my $lower_val;
	my $upper_val;

	foreach my $par (keys %parameter_covariate_form){
		$tmpstr = $par . "COV = ";
		my $first = 1;
		foreach my $covariate (keys %{$parameter_covariate_form{$par}}){
			if ($first == 1){
				$first=0;
			} else {
				$tmpstr = $tmpstr . "*";
			}
			if ($parameter_covariate_form{$par}{$covariate} == 2){
				if ($PsN::nm_major_version < 7 and length($par.$covariate)>6){
					my $shorten = (length($covariate)>= length($par))? $covariate : $par;
					croak("Lasso parameter name ".$par.$covariate.' is '.
						"longer than 6 characters, not allowed by NONMEM".$PsN::nm_major_version.
						". Shorten the name for $shorten or use NONMEM7.");
				}
				## Create COV-code for the linear continuous
				$tmpstr = $tmpstr . "($par" ."$covariate+1)";
				## Create FACTOR-code for all the linear continuous covariates
				$tmpfactor = $par.$covariate. " = THETA(" .++$nthetas .
				")*($covariate" . 
				sprintf($self->sign_dec_str,-$self->statistics->{$covariate}{2}{'mean'}) .
				")/" . sprintf($self->dec_str,$self->statistics->{$covariate}{2}{'sd'}) . 
				"*FACTOR";
				push @factor_code, @{$self->parse_row(parse_str => $self->blank . $tmpfactor,
				parse_operator =>"*",
				max_length => $self->row_length)};

				#boundaries and initial values and labels for linear continuous
				$lasso_model->initial_values(parameter_type => 'theta',
					parameter_numbers => [[$nthetas]],
					new_values =>[[$initial_value]],
					add_if_absent => 1);


				$upper_val=sprintf($self->dec_str,-1/(($self->statistics->{$covariate}{2}{'min'}
							- $self->statistics->{$covariate}{2}{'mean'}) / 
						$self->statistics->{$covariate}{2}{'sd'}));
				$lower_val=sprintf($self->dec_str,-1/(($self->statistics->{$covariate}{2}{'max'}
							-  $self->statistics->{$covariate}{2}{'mean'}) / 
						$self->statistics->{$covariate}{2}{'sd'}));

				$lasso_model -> lower_bounds(parameter_type =>  'theta',
					parameter_numbers =>[[$nthetas]],
					new_values => [[$lower_val]] );
				$lasso_model -> upper_bounds(parameter_type =>  'theta',
					parameter_numbers =>[[$nthetas]],
					new_values => [[$upper_val]] );
				$lasso_model -> labels( parameter_type     => 'theta',
					parameter_numbers => [[$nthetas]],
					new_values        => [['TH'.($nthetas)." $par$covariate"]] );

			}elsif ($parameter_covariate_form{$par}{$covariate} == 3){
				##Create COV-code for all the hockey sticks
				if ($PsN::nm_major_version < 7 and length($par.'H'.$covariate)>6){
					my $shorten = (length($covariate)>= length($par))? $covariate : $par;
					croak("Lasso parameter name ".$par.'H'.$covariate.' is '.
						"longer than 6 characters, not allowed by NONMEM".$PsN::nm_major_version.
						". Shorten the name for $shorten or use NONMEM7.");
				}
				$tmpstr = $tmpstr . "($par" ."$covariate+1)*("."$par"."H"."$covariate"."+1)";
				#FACTOR for hockey-stick
				$tmpfactor = $par.$covariate. " = THETA(" .++$nthetas .
				")*($covariate" . 
				sprintf($self->sign_dec_str,-$self->statistics->{$covariate}{3}{'mean'}) .
				")/" . sprintf($self->dec_str,$self->statistics->{$covariate}{3}{'sd'}) . 
				"*FACTOR";
				push @factor_code, @{$self->parse_row(parse_str => $self->blank . $tmpfactor,
				parse_operator =>"*",
				max_length => $self->row_length)};
				$tmpfactor = $par."H$covariate". " = THETA(" .++$nthetas .
				")*(H$covariate" . 
				sprintf($self->sign_dec_str,-$self->statistics->{$covariate}{3}{'H-mean'}) .
				")/" . sprintf($self->dec_str,$self->statistics->{$covariate}{3}{'H-sd'}) 
				. "*FACTOR";
				push @factor_code, @{$self->parse_row(parse_str => $self->blank . $tmpfactor,
				parse_operator =>"*",
				max_length => $self->row_length)};

				##IF hockey-stick covariates
				unless ($if_printed{$covariate}{3}){
					my $cut_off =  $self->statistics->{$covariate}{3}{'breakpoint'};
					push @zero_statements, $self->blank . "H".$covariate . " = 0\n";
					push @if_statements, $self->blank . "IF ($covariate .GT. " . sprintf($self->dec_str,$cut_off) .
					") H$covariate = $covariate" .sprintf($self->sign_dec_str,-$cut_off) ."\n";
					$if_printed{$covariate}{3}=1;
				}	

				#boundaries and initial values and labels for hockey-stick
				$lasso_model->initial_values(parameter_type => 'theta',
					parameter_numbers => [[$nthetas-1]],
					new_values =>[[$initial_value]],
					add_if_absent => 1);
				$lasso_model->initial_values(parameter_type => 'theta',
					parameter_numbers => [[$nthetas]],
					new_values =>[[$initial_value]],
					add_if_absent => 1);

				$upper_val=sprintf($self->dec_str,-1/(($self->statistics->{$covariate}{3}{'min'}
							- $self->statistics->{$covariate}{3}{'mean'}) / 
						$self->statistics->{$covariate}{3}{'sd'}));
				$lower_val=sprintf($self->dec_str,-1/(($self->statistics->{$covariate}{3}{'max'}
							-  $self->statistics->{$covariate}{3}{'mean'}) / 
						$self->statistics->{$covariate}{3}{'sd'}));

				$lasso_model -> lower_bounds(parameter_type =>  'theta',
					parameter_numbers =>[[$nthetas-1]],
					new_values => [[$lower_val]] );
				$lasso_model -> upper_bounds(parameter_type =>  'theta',
					parameter_numbers =>[[$nthetas-1]],
					new_values => [[$upper_val]] );

				$upper_val=sprintf($self->dec_str,-1/((-$self->statistics->{$covariate}{3}{'H-mean'}) /
						$self->statistics->{$covariate}{3}{'H-sd'}));
				$lower_val=sprintf($self->dec_str,-1/((($self->statistics->{$covariate}{3}{'max'}
								-  $self->statistics->{$covariate}{3}{'breakpoint'})
							-  $self->statistics->{$covariate}{3}{'H-mean'}) /
						$self->statistics->{$covariate}{3}{'H-sd'}));

				$lasso_model -> lower_bounds(parameter_type =>  'theta',
					parameter_numbers =>[[$nthetas]],
					new_values => [[$lower_val]] );
				$lasso_model -> upper_bounds(parameter_type =>  'theta',
					parameter_numbers =>[[$nthetas]],
					new_values => [[$upper_val]] );

				$lasso_model -> labels( parameter_type     => 'theta',
					parameter_numbers => [[$nthetas-1]],
					new_values        => [['TH'.($nthetas-1)." $par$covariate"]] );

				$lasso_model -> labels( parameter_type     => 'theta',
					parameter_numbers => [[$nthetas]],
					new_values     => [['TH'.($nthetas)." $par"."H$covariate"]] );


			}elsif ($parameter_covariate_form{$par}{$covariate} == 1){
				my $most_common_key = $self -> statistics->{$covariate}{1}{'most_common'};
				my $first_cat = 1;
				foreach my $fact (sort {$a<=>$b} keys %{$self -> statistics->{$covariate}{1}{'cat_hash'}}) {
					my %mean = %{$self -> statistics->{$covariate}{1}{'mean'}};
					my %sd = %{$self -> statistics->{$covariate}{1}{'sd'}};
					if (($fact ne $most_common_key) and 
						($fact ne $self->missing_data_token)){

						if ($PsN::nm_major_version < 7 and length($par.$covariate.$fact)>6){
							my $shorten = (length($covariate)>= length($par))? $covariate : $par;
							croak("Lasso parameter name ".$par.$covariate.$fact.' is '.
								"longer than 6 characters, not allowed by NONMEM".$PsN::nm_major_version.
								". Shorten the name for $shorten or use NONMEM7.");
						}
						#COV code for the categorical
						$tmpstr .= '*' unless $first_cat;
						$first_cat = 0;
						$tmpstr = $tmpstr . "(".$par.$covariate.$fact . "+1)";
						#FACTOR code for the categorical
						$tmpfactor = $par.$covariate.$fact. " = THETA(" .++$nthetas .
						")*($covariate$fact" . 	sprintf($self->sign_dec_str,-$mean{$fact}) .
						")/" . sprintf($self->dec_str,$sd{$fact}) . "*FACTOR";
						push @factor_code, @{$self->parse_row(parse_str => $self->blank . $tmpfactor,
						parse_operator =>"*",
						max_length => $self->row_length)};
						# IF statements
						unless ($if_printed{$covariate}{1}){
							push @zero_statements, $self->blank . $covariate.$fact ." = 0\n";
							push @if_statements, $self->blank . "IF ($covariate .EQ. $fact) $covariate$fact=1\n";
						}

						#boundaries and initial values and labels for categorical
						$lasso_model->initial_values(parameter_type => 'theta',
							parameter_numbers => [[$nthetas]],
							new_values =>[[$initial_value]],
							add_if_absent => 1);

						$upper_val=sprintf($self->dec_str,-1/((0-$mean{$fact})/$sd{$fact}));
						$lower_val=sprintf($self->dec_str,-1/((1-$mean{$fact})/$sd{$fact}));

						$lasso_model -> lower_bounds(parameter_type =>  'theta',
							parameter_numbers =>[[$nthetas]],
							new_values => [[$lower_val]] );
						$lasso_model -> upper_bounds(parameter_type =>  'theta',
							parameter_numbers =>[[$nthetas]],
							new_values => [[$upper_val]] );
						$lasso_model -> labels( parameter_type     => 'theta',
							parameter_numbers => [[$nthetas]],
							new_values        => [['TH'.($nthetas)." $par$covariate$fact"]] );
					}
				}
				$if_printed{$covariate}{1}=1;
			}
		}
		unshift @new_code, @{$self->parse_row(parse_str => $self->blank . $tmpstr,
		parse_operator =>"*",
		max_length => $self->row_length)};
	}
	unshift @new_code, "\n";
	unshift @new_code, @factor_code;
	unshift @new_code, "\n";
	unshift @new_code,@if_statements;
	unshift @new_code,@zero_statements;
	unshift @new_code, "\n";

	#add FACTOR
	unshift @new_code, $self->blank . "FACTOR = EXP(1-RATIO)\n";
	unshift @new_code, $self->blank . "IF (RATIO .GT. 5) EXIT 1 1\n";
	unshift @new_code, $self->blank . "RATIO = ABSSUM/TVALUE\n";
	unshift @new_code, "\n";

	$tmpstr = "ABSSUM = ";
	my $first = 1;
	my $i;
	for ($i=$old_thetas+1; $i<=$nthetas; $i++){
		if ($first) {
			$tmpstr = $tmpstr . "ABS(THETA($i))";
			$first=0;
		} else {
			$tmpstr = $tmpstr . "+ABS(THETA($i))";
		}
	}
	unshift @new_code, @{$self->parse_row(parse_str => $self->blank . $tmpstr,
	parse_operator => "+",
	max_length => $self->row_length)};
	## Set the initial t-value and make it a fixed variable
	unshift @new_code, $self->blank . "TVALUE  = THETA(".++$nthetas.")\n";
	$lasso_model->initial_values(parameter_type => 'theta',
		parameter_numbers => [[$nthetas]],
		new_values =>[[$t_value]],
		add_if_absent => 1);
	$lasso_model -> fixed(parameter_type => 'theta',
		parameter_numbers => [[$nthetas]],
		new_values => [[1]] );
	$lasso_model -> labels( parameter_type     => 'theta',
		parameter_numbers => [[$nthetas]],
		new_values        => [['TH'.($nthetas)." T-VALUE"]] );

	unshift @new_code, $self->blank . "\n;;; LASSO-BEGIN\n";

	## Add the multiplication of the Typical Values with the 
	#covariate for all params.
	#handle if/else clauses here, like in scm
	my $success;
	foreach my $parameter (keys %parameter_covariate_form){
		$success = 0;
		for ( reverse @old_code ) {
			#want to find last occurrence of TVpar set
			if ( /[^A-Z0-9_]*TV(\w+)\s*=\s*/ and $1 eq $parameter){
				#add new definition line after last occurence
				$_ = $_."\n".$self->blank.
				"TV$parameter = TV$parameter"."*$parameter"."COV\n";
				$success = 1;
				last; #only change the last line where appears
			}
		}
		unless ( $success ) {
			croak("Could not determine a good place to add the covariate relation.\n".
				" i.e. No TV$parameter was found\n" );
		}
	}

	## Merge the old_code and new_code
	@new_code = (@new_code, @old_code);

	## Set the new code to the new model

	if ($self->use_pred){
		$lasso_model->pred(new_pred => \@new_code);
	}else{
		$lasso_model->pk(new_pk => \@new_code);
	}
}

sub xv_step_init
{
	my $self = shift;

	print "\nlasso: xv_step_init\n" if ($self->stop_motion());
	#self here will be xv_step object, shift gets single parameter given in xv_step_subs
	$self->stop_motion_call(tool=>'lasso',message => "starting xv_step_init")
	if ($self->stop_motion());
	sub print_log {
		my $filename = shift;
		my $message  = shift;
		open(LOG, ">>$filename") or die "Can not create logfile: $filename\n$!";
		print LOG $message;
		close LOG;
	}
	my @estimation_models = @{$self->estimation_models()};
	my @prediction_models = @{$self->prediction_models()};
	my $own_parameters = $self->own_parameters();
	my $counter=0;

	print_log ($own_parameters->{'logfile'}->[0],  "Last t-value: " .$own_parameters->{'last_t_value'} ."\n");

	my $new_t_value = $own_parameters->{'last_t_value'} + $own_parameters->{'steplength'};
	foreach my $model ( @prediction_models){
		$model->initial_values(parameter_type => 'theta',
							   parameter_numbers => [[$model->nthetas()]],
							   new_values =>[[$new_t_value]]);
		$model->_write(overwrite => 1);
	}


	foreach my $model (@estimation_models) {
		#set new t-value
		$model->initial_values(parameter_type => 'theta',
			parameter_numbers => [[$model->nthetas()]],
			new_values =>[[$new_t_value]]);

		#Set the new MSFO file
		my $MSFO_file = "lasso_t_". $new_t_value ."_" .($counter++).".msfo1";

		# -1 means last record
		$model -> set_option( record_name => 'estimation',
			record_number => -1,
			option_name => 'MSFO',
			option_value=> $MSFO_file );

		$model->_write(overwrite => 1);
	}
	$self->stop_motion_call(tool=>'lasso',message => "written ".scalar(@estimation_models).
		" new estimation models and ".scalar(@prediction_models).
		" new prediction models")
	if ($self->stop_motion());

	print_log ($own_parameters->{'logfile'}->[0],  "Last OFV sum: " . $own_parameters->{'last_ofv_sum'} ."\n");
	$own_parameters -> {'last_t_value'}+=$own_parameters->{'steplength'};
}

sub xv_step_analyze
{
	my $self = shift;
	my $retur;

	print "\nlasso: xv_step_analyze\n" if ($self->stop_motion());
	#self here will be xv_step object
	sub print_log {
		my $filename = shift;
		my $message  = shift;
		open(LOG, ">>$filename") or die "Can not create logfile: $filename\n$!";
		print LOG $message;
		close LOG;
	}

	$retur = 1;

	my @estimation_models = @{$self->estimation_models()};
	my @prediction_models = @{$self->prediction_models()};
	my $own_parameters = $self->own_parameters();

	my @est_strings;

	my $sum_ofv = 0;
	my $fh1;
	my $j=1;
	open($fh1,  ">>", $own_parameters->{'pred_filename'});
	foreach my $pred_model (@prediction_models) {
		#use Data::Dumper;
		#print Dumper $pred_model -> outputs -> [0];
		if (defined $pred_model -> outputs -> [0] and $pred_model->outputs->[0]->have_output) {
			my $ofv =  $pred_model -> outputs -> [0] -> get_single_value(attribute => 'ofv');
			if (defined $ofv){
				$sum_ofv += $ofv;
			} else {
				$retur = 0;
				die "No defined ofv from the pred model!\n";
			}
			printf $fh1 "%-12.4f %-6.3f %-5d %-10d\n", $ofv, $own_parameters -> {'last_t_value'}, $j, $own_parameters->{'seed'};

			$j++;
		} else {
			$retur = 0;
			die "No defined output from the pred model!\n";
		}
	}
	$self->stop_motion_call(tool=>'lasso',message => "computed sum ofv from ".scalar(@prediction_models).
		" prediction models")
	if ($self->stop_motion());
	printf $fh1 "%-12.4f %-6.3f %-5s %-10d\n", $sum_ofv, $own_parameters -> {'last_t_value'}, "All", $own_parameters->{'seed'};

	close($fh1);

	my @est_ofv;
	my $i=1;
	my $fh;
	print "\n" unless (ui -> silent());
	foreach my $est_model (@estimation_models){
		if (defined $est_model -> outputs -> [0] and $est_model->outputs->[0]->have_output){
			my $ofv =  $est_model -> outputs -> [0] -> get_single_value(attribute => 'ofv');
			if (defined $ofv){
				my $warning = '';
				unless (defined $est_model->outputs->[0]->get_single_value (attribute => 'minimization_successful')
						and $est_model->outputs->[0]->get_single_value (attribute => 'minimization_successful')){
					$warning = ' *minimization unsuccessful';
					if (defined $est_model->outputs->[0]->get_single_value (attribute => 'rounding_errors')
							and $est_model->outputs->[0]->get_single_value (attribute => 'rounding_errors')){
						if ($self->significant_digits_accept() and 
							(defined $est_model->outputs->[0] -> get_single_value(attribute =>'significant_digits',
									problem_index => 0)) and
							$est_model->outputs->[0]-> get_single_value(attribute =>'significant_digits',
								problem_index => 0)>= $self->significant_digits_accept()){
							my $dig = $est_model->outputs->[0]-> get_single_value(attribute =>'significant_digits',
								problem_index => 0);
							$warning = ' *minimization with rounding errors accepted, SIGDIG_'.$dig;
						}else{
							$warning .= ', rounding errors';
							$self->warnings($self->warnings()+1);
						}
					}else{
						$self->warnings($self->warnings()+1);
					}
				}

				print_log ($own_parameters->{'logfile'}->[0],  "Estimation OFV in validation group $i: ". $ofv ."$warning\n");
				ui -> print( category => 'lasso',
					message  => "Estimation OFV group $i: ". $ofv ."$warning");

				push @est_ofv, $ofv;
				push @est_strings,sprintf("%-12.4f %-6.3f %-5d %-10d", $ofv, $own_parameters -> {'last_t_value'}, $i, $own_parameters->{'seed'});

				$i++;

			}else { 
				$self->stop_motion_call(tool=>'lasso',message => "estimation model did not have defined ofv, return 0")
				if ($self->stop_motion());
				$retur = 0; 
			}
		} else { 
			$self->stop_motion_call(tool=>'lasso',message => "estimation model did not have defined output, return 0")
			if ($self->stop_motion());
			$retur = 0;
		}
	}

	if (defined $own_parameters->{'est_ofv'}) {
		for (my $i = 0; $i <= $#est_ofv; $i++){
			if ($own_parameters->{'steplength'}>0){
				if ( $own_parameters->{'est_ofv'}->[$i] <= $est_ofv[$i]){ 
					print_log ($own_parameters->{'logfile'}->[0],  "Warning: estimation OFV in group ".($i+1).
						" did not decrease when increasing t compared to previous step\n");
					$self->warnings($self->warnings()+1);
				}
			}else{
				if ( $own_parameters->{'est_ofv'}->[$i] >= $est_ofv[$i]){
					print_log ($own_parameters->{'logfile'}->[0],  "Warning: estimation OFV in group ".($i+1).
						" did not increase when decreasing t compared to previous step\n");
					$self->warnings($self->warnings()+1);
				}
			}

		}
	}
	$own_parameters->{'est_ofv'} = \@est_ofv;

	my $mess = "Prediction OFV at t=".sprintf("%-12.2f",$own_parameters -> {'last_t_value'})." : $sum_ofv";
	print_log ($own_parameters->{'logfile'}->[0],  "$mess\n");
	ui -> print( category => 'lasso', message  => $mess);

	open($fh, ">>", $own_parameters->{'coeff_table'});
	$i=1;
	foreach my $pred_model (@prediction_models) {
		if (defined $pred_model -> outputs -> [0] and $pred_model->outputs->[0]->have_output) {
			my $cutoff_thetas = $self->subtool_arguments()->{'modelfit'}->{'cutoff_thetas'};
			my $init_val = $pred_model -> initial_values( parameter_type    => 'theta',
				parameter_numbers => [$cutoff_thetas])->[0];
			my $abssum = 0;
			foreach my $value (@{$init_val})  {
				$abssum+=abs($value);
			}

			my $factor = exp(1-($abssum/$own_parameters -> {'last_t_value'}));
			$est_strings[$i-1] = $est_strings[$i-1] . sprintf(" %-6.4f",$factor);
			my $labels = $pred_model->labels( parameter_type  => 'theta', problem_numbers => [1],
				parameter_numbers => [$cutoff_thetas] )->[0];
			my $j=0;
			foreach my $label1 (@{$labels})  {
				my @tmp = split(' ',$label1);
				my $value = @{$init_val}[$j]*$factor;
				printf $fh "%-6.3f %-5d %-6s %-10.7f %-10d\n",$own_parameters -> {'last_t_value'}, $i, $tmp[1], $value, $own_parameters->{'seed'};
				$j++;
			}
			$i++;
		} else {
			$retur = 0;
		}
	}
	close($fh);

	open($fh,  ">>", $own_parameters->{'est_filename'});
	$i=1;
	foreach my $est_model (@estimation_models){
		if (defined $est_model -> outputs -> [0] and $est_model->outputs->[0]->have_output
				and defined $est_model->outputs->[0]->get_single_value (attribute => 'minimization_successful')
				and $est_model->outputs->[0]->get_single_value (attribute => 'minimization_successful')) {
			if (defined $est_strings[$i-1])  {
				printf $fh $est_strings[$i-1] .sprintf(" %-3s\n",'OK');
			}
		}elsif (defined $est_model -> outputs -> [0] and $est_model->outputs->[0]->have_output
				and $self->significant_digits_accept() and 
			(defined $est_model->outputs->[0] -> get_single_value(attribute =>'significant_digits',
					problem_index => 0)) and
			$est_model->outputs->[0]-> get_single_value(attribute =>'significant_digits',
				problem_index => 0)>= $self->significant_digits_accept()){
			my $dig = $est_model->outputs->[0]-> get_single_value(attribute =>'significant_digits',
				problem_index => 0);
			if (defined $est_strings[$i-1])  {
				printf $fh $est_strings[$i-1] .sprintf(" %-3s\n",'SIGDIG_'.$dig);
			}
		}elsif (defined $est_model -> outputs -> [0] and $est_model->outputs->[0]->have_output
				and defined $est_model->outputs->[0]->get_single_value (attribute => 'rounding_errors')
				and $est_model->outputs->[0]->get_single_value (attribute => 'rounding_errors')){
			if (defined $est_strings[$i-1]) {
				print $fh $est_strings[$i-1] . sprintf(" %-3s\n",'ROUND_ERR');
			}
		} else {
			if (defined $est_strings[$i-1]) {
				print $fh $est_strings[$i-1] . sprintf(" %-3s\n",'ERROR');
			}
		}
		$i++;
	}
	close($fh);

	if ($own_parameters->{'last_ofv_sum'}<=$sum_ofv) {
		print_log ($own_parameters->{'logfile'}->[0], "OFV-sum is larger than the previous step\n");
		#do not change t_optimal
		$retur = 0 if ($own_parameters->{'converge'} eq "FIRSTMIN");
	} else {
		print_log ($own_parameters->{'logfile'}->[0],  "Found a better ofv $sum_ofv\n");
		if ($retur!=0) {#If all pred and est did terminate  
			$own_parameters->{'t_optimal'} = $own_parameters->{'last_t_value'};
			ui -> print( category => 'lasso',
				message  => "T-optimal is now " .$own_parameters->{'t_optimal'});
			print_log ($own_parameters->{'logfile'}->[0], "T-optimal is now " .$own_parameters->{'t_optimal'} ."\n");
		} else {
			print_log ($own_parameters->{'logfile'}->[0], "Not all pred and est did terminate\n");
		}
		$own_parameters->{'last_ofv_sum'}=$sum_ofv;
	}

	if (defined $own_parameters->{'stop_t'} && $own_parameters->{'steplength'}>0
		&& $own_parameters->{'last_t_value'}>=$own_parameters->{'stop_t'}) {
		print_log ($own_parameters->{'logfile'}->[0], "t-value greater or equal to " .$own_parameters->{'stop_t'} . "\n");
		$retur = 0 if ($own_parameters->{'converge'} ne "HALT");
	}

	if (defined $own_parameters->{'stop_t'} && $own_parameters->{'steplength'}<0
		&& $own_parameters->{'last_t_value'}<=$own_parameters->{'stop_t'}) {
		print_log ($own_parameters->{'logfile'}->[0],"t-value smaller or equal to " .$own_parameters->{'stop_t'} . "\n");
		$retur = 0 if ($own_parameters->{'converge'} ne "HALT");
	}

	return $retur;
}

sub write_log
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		message => { isa => 'Str', optional => 1 }
	);
	my $message = $parm{'message'};

	open(LOG, ">>".$self->logfile->[0]) or 
	die "Cannot open logfile: " . $self->logfile->[0] . "\n";
	print LOG $message."\n";
	close LOG;
}

sub general_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 },
		class => { isa => 'Str', optional => 1 },
		subm_threads => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};
	my $class = $parm{'class'};
	my $subm_threads = $parm{'subm_threads'};
}

sub modelfit_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	ui -> print( category => 'lasso',
		message  => "Parsing relations and calculating covariate statistics" );
	#parse and store relations
	my @sets = split( ',,' , $self->relations() );
	#a hash {parameter}{covariate}{form}
	my %parameter_covariate_form;

	# Assume one $PROBLEM one model
	my $model = $self -> models -> [0];
	my $dataobj = data->new(filename => $model->datafiles(absolute_path=>1)->[0],
							idcolumn => $model->idcolumn,
							missing_data_token => $self->missing_data_token,
							ignoresign => $model->ignoresigns->[0]);

	foreach my $set (@sets){
		my @parlist = split (':',$set);
		croak("Error parsing relations: expected exactly one : in $set but found ".
			  (scalar(@parlist)-1)) unless (scalar(@parlist)==2);
		my $parameter=$parlist[0];
		if (defined $parameter_covariate_form{$parameter}){
			croak("Error parsing relations: $parameter found twice ");
		}
		my @list = split (',',$parlist[1]);
		foreach my $covform (@list){
			my @pair = split ('-',$covform);
			my $breakpoint;
			if (scalar(@pair)==3){
				croak("Can only specify breakpoint (number after second dash) if ".
					  "parameterization is 3, but found $covform ".
					  "where parameterization is ".$pair[1])	unless ($pair[1] eq '3');
				$breakpoint = $pair[2];
			}else{
				croak("Error parsing relations: expected exactly one - in $covform but found ".
					  (scalar(@pair)-1))	unless (scalar(@pair)==2);
			}
			my $covariate=$pair[0];
			my $function=$pair[1];

			my $column_number;
			# Check normal data object first
			my ( $values_ref, $positions_ref ) = $model ->
				_get_option_val_pos ( problem_numbers => [1], 
									  name        => $covariate,
									  record_name => 'input',
									  global_position => 1  );
			$column_number = $positions_ref -> [0];

			croak("Cannot find $covariate in \$INPUT" )	unless ( defined $column_number );
			#check if $covariate in data is done when computing statistics
			#check if $function 1-5
			croak("Error parsing relations: ".
				  "parameterization in $covform must be in the set [1,2,3]")
				unless ($function =~ /^(1|2|3)$/);

			unless (defined $self -> statistics->{$covariate}{$function}){
				$self -> statistics->{$covariate}{$function} =
					$dataobj -> lasso_calculate_covariate_statistics
					( 	missing_data_token =>$self->missing_data_token,
						column_number => $column_number,
						function => $function,
						breakpoint => $breakpoint);
			}
			$parameter_covariate_form{$parameter}{$covariate}=$function;

		}

	}

	$dataobj = undef;
	ui -> print( category => 'lasso',
		message  => " ... done\n" );


	if(defined $self -> covariate_statistics_file){  
		open( STAT, '>'.$self-> covariate_statistics_file );
		$Data::Dumper::Purity = 1;
		print STAT Dumper %{$self -> statistics}; #or give ref here??
		$Data::Dumper::Purity = 0;
		close( STAT );
	}

	$self->NOABORT_added(0);

	unless ($model->is_option_set(record => 'estimation',
			name   => 'NOABORT')) {
		$model->set_option(record_name => 'estimation',
			option_name => 'NOABORT');
		$self->NOABORT_added(1);
	}
	## Kill the covariance record
	$model -> remove_records( type => 'covariance');

	my $basic_model= $model->copy(filename => $self -> directory().'m'.$model_number.'/basic.mod',
								  copy_datafile => 0,
								  copy_output => 0);
	## Create the new model object
	#must do this in setup, not new
	my $lasso_model= $model->copy(filename => $self -> directory().'m'.$model_number.'/lasso_initial.mod',
								  copy_datafile => 0,
								  copy_output => 0,
								  write_copy => 0);
	
	$self->setup_lasso_model(lasso_model => $lasso_model,
		parameter_covariate_form => \%parameter_covariate_form);
	$lasso_model->_write();
	my $added_thetas = ($lasso_model->nthetas()-$basic_model->nthetas());

	#create xv data, use relative path to this directory when running nonmem
	if (defined $self->stratify_on()){
		my $column_number;
		my ( $values_ref, $positions_ref ) = $model ->
		_get_option_val_pos ( problem_numbers => [1], 
			name        => $self->stratify_on(),
			record_name => 'input',
			global_position => 1  );
		$column_number = $positions_ref -> [0];
		croak("Cannot find ".$self->stratify_on()." in \$INPUT" )
		unless ( defined $column_number );
		$self->stratify_on($column_number);
	}

	my $data_xv_step = tool::xv_step -> new( 
		%{common_options::restore_options(@common_options::tool_options)},
		models => [$lasso_model],
		nr_validation_groups => $self->groups(),
		stratify_on      => $self -> stratify_on(),
		base_directory => $self->directory(),
		directory => $self->directory().'xv_data');
	$data_xv_step -> create_data_sets();

	#prepare log files
	my $est_filename =  $self->directory() .  "/est_ofv.log";
	my $pred_filename = $self->directory() .  "/pred_ofv.log";
	my $coeff_table =   $self->directory() .  "/coeff_table.log";
	my ($fh1,$fh2,$fh3);
	open($fh1,  ">", $est_filename);
	open($fh2 ,">", $pred_filename);
	open($fh3, ">", $coeff_table);

	printf $fh1 "%-12s %-6s %-5s %-10s %-6s %-3s\n", "Est_OFV", "t", "Group", "Seed", "Factor", "Termination";
	printf $fh2 "%-12s %-6s %-5s %-10s\n", "Pred_OFV", "t", "Group", "Seed";
	printf $fh3 "%-6s %-5s %-6s %-10s %-10s\n", "t", "Group", "COV", "Coeff", "Seed";

	close $fh1;
	close $fh2;
	close $fh3;

	#run the basic step unless defined pred_ofv_start_t (last_ofv_sum)

	my $common_seed = $self->seed();
	if (not defined $self->pred_ofv_start_t()){
		ui -> print( category => 'lasso',
			message  => "Running the basic xv_step (option pred_ofv_start_t undefined).\n" );
		my $basic_step =  tool::xv_step -> new(
			directory => $self->directory().'basic_xv_step',
			cutoff => $self->cutoff(),
			n_model_thetas => $basic_model->nthetas(),
			nr_validation_groups => $self->groups(),
			stratify_on      => $self -> stratify_on(), 
			models => [$basic_model],
			prediction_data => $data_xv_step->prediction_data(),
			estimation_data => $data_xv_step->estimation_data(),
			subtool_arguments => { modelfit => {
					%{common_options::restore_options(@common_options::tool_options)},
					directory=> undef,
					seed => $common_seed,
					copy_data => 0,
					top_tool => 0,
					prepend_model_file_name => 1
				}
			});
		my $return_val = $basic_step->run();
		my @basic_pred_models = @{$basic_step->prediction_models()};
		my @basic_est_models = @{$basic_step->estimation_models()};
		$self->stop_motion_call(tool=>'lasso',message => "have run ".scalar(@basic_est_models).
			" basic est models and ".scalar(@basic_pred_models).
			" basic pred models")
		if ($self->stop_motion());

		my $sum_ofv = 0;
		my $j=1;
		my $fh;

		$j=1;
		open($fh,  ">>", $est_filename);
		foreach my $basic_mod (@basic_est_models)  {
			if (defined $basic_mod -> outputs -> [0] and $basic_mod->outputs->[0]->have_output) {
				my $ofv =  $basic_mod -> outputs -> [0] -> get_single_value(attribute => 'ofv');
				printf $fh "%-12.4f %-6.3f %-5d %-10d %-6s %-3s\n", $ofv, $self->start_t(), $j, $common_seed,'-', 'OK';
			} else {
				die("The basic estimation model nr $j, did not terminate\n");
			}
			$j++;
		}
		close($fh);


		$j=1;
		open($fh,  ">>", $pred_filename);
		foreach my $basic_mod (@basic_pred_models) {
			if (defined $basic_mod -> outputs -> [0] and $basic_mod->outputs->[0]->have_output) {
				my $ofv =  $basic_mod -> outputs -> [0] -> get_single_value(attribute => 'ofv');
				$sum_ofv += $ofv;

				printf $fh "%-12.4f %-6.3f %-5d %-10d\n", $ofv,$self->start_t(), $j, $common_seed;
			} else {
				die("The basic prediction model nr $j, did not terminate\n");
			}
			$j++;
		}

		$self->pred_ofv_start_t($sum_ofv);

		printf $fh "%-12.4f %-6.3f %-5s %-10d\n", $sum_ofv, $self->start_t(), "All", $common_seed;
		close($fh);
		my $mess = "Prediction OFV at t=".sprintf("%-12.2f",$self->start_t())." : $sum_ofv\n";
		print_log ($self->logfile->[0],  $mess);
		ui -> print( category => 'lasso',
			message  => "\n$mess");
	}

	my $t_optimal;

	if ($self->start_t() == $self->stop_t()){
		$t_optimal = $self->start_t()
	}else{
		ui -> print( category => 'lasso',
			message  => "Running the cross-validation to find optimal t-value.\n" );

		#run the xv

		my $cvobject = tool::xv->new(%{common_options::restore_options(@common_options::tool_options)},
									 models => [$lasso_model],
									 base_directory   => $self -> directory,
									 directory        => $self -> directory.'xv_dir'.$model_number, 
									 subtool_arguments => 
									 {xv_step => {%{common_options::restore_options(@common_options::tool_options)},
												  seed => $common_seed,
												  directory => undef,
												  cutoff => $self->cutoff(),
												  n_model_thetas => $basic_model->nthetas(),
												  nr_validation_groups => $self->groups(),
												  stratify_on      => $self -> stratify_on(), 
												  estimation_data => $data_xv_step->estimation_data(),
												  prediction_data => $data_xv_step->prediction_data(),
												  own_parameters => {logfile => $self->logfile,
																	 last_t_value => $self->start_t(),
																	 steplength => $self->step_t(),
																	 basic_model => $basic_model,
																	 last_ofv_sum => $self->pred_ofv_start_t(),
																	 seed => $self->seed(),
																	 stop_t =>  $self->stop_t(),
																	 t_optimal => $self->start_t(),
																	 converge => $self->convergence(),
																	 est_filename => $est_filename,
																	 pred_filename => $pred_filename,
																	 coeff_table => $coeff_table },
												  init => \&xv_step_init,
												  post_analyze =>  \&xv_step_analyze},
									  modelfit => {%{common_options::restore_options(@common_options::tool_options)},
												   seed => $common_seed,
												   directory => undef,
												   prepend_model_file_name => 1,
												   cut_thetas_rounding_errors => 1,
												   handle_hessian_npd => 0,
												   copy_data => 0,
												   cutoff => $self->cutoff(),
												   cutoff_thetas => [($basic_model->nthetas()+1)..($lasso_model->nthetas()-1)], #Last theta is t-value
									  }}
			);
		
		$cvobject->run();
		$self->warnings($self->warnings() + $cvobject->warnings());
		$t_optimal = $cvobject->subtool_arguments->{'xv_step'}->{'own_parameters'}->{'t_optimal'} if (defined $cvobject->subtool_arguments);
}


	$self->write_log(message=>"The final t-value is: ". $t_optimal);
	
	if ($t_optimal<=0){
		ui -> print( category => 'lasso',
			message =>"Of the *tested* t_values, none gave a better model than the model".
			" without covariates (t_value=0). No use running lasso model.\n");
		return;
	}
	ui -> print( category => 'lasso',
		message  => "The optimal t-value is $t_optimal. Running lasso model with t=$t_optimal\n" );

	my $lasso_optimal = $lasso_model -> copy(filename => $self->directory()."m1/lasso_optimal.mod",
											 copy_datafile => 0,
											 output_same_directory => 1,
											 write_copy => 0,
											 copy_output => 0);

	$lasso_optimal->initial_values(parameter_type => 'theta',
								   parameter_numbers => [[$lasso_optimal->nthetas()]],
								   new_values =>[[$t_optimal]]);
	$lasso_optimal->_write();

	my @cutoff_thetas = ($basic_model->nthetas()+1)..($lasso_model->nthetas()-1);
	# Run a modelfit on the the whole data set, with the 'best' t-value

	my $mfitobj = tool::modelfit -> new (%{common_options::restore_options(@common_options::tool_options)},
		models    => [$lasso_optimal],
		seed => $common_seed,
		cut_thetas_rounding_errors => 1,
		handle_hessian_npd => 0,
		cutoff => $self->cutoff(),
		cutoff_thetas => [\@cutoff_thetas],
		base_directory => $self->directory(),
		directory => $self->directory().'optimal_lasso_modelfit_dir',
		top_tool =>1,
		parent_threads=>1);

	$mfitobj->run();

	if (not defined $lasso_optimal -> outputs -> [0] 
			or not defined $lasso_optimal -> outputs ->[0]->get_single_value(attribute => 'ofv')){
		die("Couldn't execute the LASSO - optimal model\n");
	}
	#print minimization status to log file

	#Create the optimal model file (not LASSO)

	$self->model_optimal($lasso_optimal -> copy(filename =>$self->directory(). "m1/optimal_model.mod",
												copy_datafile => 0, 
												copy_output => 0,
												write_copy => 0,
												output_same_directory => 1));
	
	$self->model_optimal -> update_inits( from_output => $lasso_optimal->outputs->[0],
										  update_omegas => 1,
										  update_sigmas => 1,
										  update_thetas => 1);
	
	my @remove_theta_num;
	my %remove_parameters;
	my %keep_parameters;

	my $rem_nthetas = $self->model_optimal->nthetas() - $added_thetas;

	my $labels = $self->model_optimal->labels( parameter_type  => 'theta', 
											   problem_numbers => [1],
											   parameter_numbers => [\@cutoff_thetas] )->[0];
	
	#have %parameter_covariate_form here
	my $index=0;
	my @old_code;
	if ($self->use_pred){
		@old_code= @{$self->model_optimal->pred()};
	}else{
		@old_code= @{$self->model_optimal->pk()};
	}
	my $abssum = 0;
	foreach my $th_num (@cutoff_thetas) {
		my $init_val = $self->model_optimal ->initial_values( parameter_type    => 'theta',
			parameter_numbers => [[$th_num]])->[0][0];
		$abssum+=abs($init_val);
	}
	my $factor = exp(1-($abssum/$t_optimal));
	if ($factor<0.9 or $factor>1.1) {
		$self->write_log(message =>"WARNING: Factor for the final lasso model: $factor");
		$self->warnings($self->warnings()+1);
	}

	#thetas that are below cutoff are set to 0 and marked for removal,
	#if not below cutoff then pick up sd and init and compute new initial value and
	#unset boundaries
	foreach my $th_num ( @cutoff_thetas ) {
		my $init_val = $self->model_optimal ->initial_values( parameter_type    => 'theta',
			parameter_numbers => [[$th_num]])->[0][0];
		if (abs($init_val)<=$self->cutoff()) {
			$self->model_optimal->initial_values(parameter_type => 'theta',
				parameter_numbers => [[$th_num]],
				new_values =>[[0]]);
			push @remove_theta_num, $th_num;
			my @tmp = split(' ',$labels->[$index]);
			$remove_parameters{$tmp[1]}=1; #this is the name of the variable: $par$cov
		}else{
			my @tmp = split(' ',$labels->[$index]);
			my $sd=0;
			foreach my $code_line (@old_code) {
				my $tmp_var=$tmp[1];
				$_=$code_line;
				if (/^\s*$tmp_var\s*=\s*THETA\($th_num\)\s*/){
					my $tmp_line = $code_line;
					$tmp_line =~ /\/(\d+\.*\d*)*/;
					$sd = $1;
				}
			}
			$self->model_optimal->initial_values(parameter_type => 'theta',
				parameter_numbers => [[$th_num]],
				new_values =>[[$factor*$init_val/($sd)]]);
			my $new_bound = undef;
			$self->model_optimal -> lower_bounds(parameter_type =>  'theta',
				parameter_numbers =>[[$th_num]],
				new_values => [[$new_bound]] );
			$self->model_optimal -> upper_bounds(parameter_type =>  'theta',
				parameter_numbers =>[[$th_num]],
				new_values => [[$new_bound]] );

			#tmp[1] is name of param: $par$cov$fact $parH$cov $par$cov
			$keep_parameters{$tmp[1]}= 
			$labels->[$index]." $th_num $sd ". ($rem_nthetas+scalar(keys %keep_parameters)+1);
			$self->model_optimal -> labels( parameter_type => 'theta',
				parameter_numbers =>[[$th_num]],
				new_values => [["TH".($rem_nthetas+scalar(keys %keep_parameters))." ". $tmp[1]]]);

		}
		$index++;
	}

	my $init_val = $self->model_optimal ->
	initial_values( parameter_type    => 'theta',
		parameter_numbers => [[1..$self->model_optimal->nthetas()]])->[0];

	#fix the thetas that were set to 0 above
	for(my $j = 0; $j<scalar(@{$init_val}); $j++){
		my $value = $init_val -> [$j];
		if ($value == 0)  {
			$self->model_optimal->fixed(parameter_type => 'theta',
				parameter_numbers => [[$j+1]],
				new_values => [[1]] );
		}
	}

	#????? ar inte detta alla nya thetas?
	foreach my $th_num ( @cutoff_thetas ) {
		$self->model_optimal->fixed(parameter_type => 'theta',
			parameter_numbers => [[$th_num]],
			new_values => [[1]] );
	}

	# Remove the t-value as well
	#can this handle prior? assumes estimated param is last...
	push @remove_theta_num,$self->model_optimal->nthetas();
	$self->model_optimal->remove_inits(type => 'theta',
		indexes => \@remove_theta_num);

	my @code;

	my $do_copy = 1;
	foreach my $code_line (@old_code) {
		if ($code_line =~ /\s*;;; LASSO-BEGIN\s*/){
			$do_copy = 0 ;
		}elsif ($code_line =~ /\s*;;; LASSO-END\s*/){
			$do_copy = 1;
		}elsif ($do_copy){
			push @code, $code_line;
		}
	}

	my @new_code;
	push @new_code, "\n";
	foreach my $par (keys %parameter_covariate_form){
		push @new_code, $self->blank . $par . "COV=1\n";
	}

	my $nthetas = $basic_model ->nthetas();
	my %taken_values_cat;
	my %if_printed;
	my %selected_cont;

	foreach my $par (keys %parameter_covariate_form){
		foreach my $covariate (keys %{$parameter_covariate_form{$par}}){
			if ($parameter_covariate_form{$par}{$covariate} == 2){

				my $name = $par."$covariate";
				if (defined $keep_parameters{$name}){
					my $mean = $self -> statistics->{$covariate}{2}{'mean'};
					my @tmp = split(' ',$keep_parameters{$name});
					push @new_code, $self->blank . "$name = THETA(". $tmp[4] .")*(".$covariate .
					sprintf($self->sign_dec_str,-$mean). ")\n";
					if (defined $selected_cont{$par}){ 
						$selected_cont{$par} = $selected_cont{$par}. " ". $name;
					}else { 
						$selected_cont{$par} = $name;
					}
				}

			}elsif ($parameter_covariate_form{$par}{$covariate} == 3){
				my $hname = $par."H$covariate";
				my $name = $par.$covariate;
				my $break =  $self->statistics->{$covariate}{3}{'breakpoint'};
				if (defined $keep_parameters{$hname}){
					my @tmp = split(' ',$keep_parameters{$hname});
					#only if statement if parHcov is in keep_parameters
					unless ($if_printed{'H'.$covariate}){
						push @new_code, $self->blank . "H".$covariate . " = 0\n";
						push @new_code, $self->blank . "IF ($covariate .GT. " . sprintf($self->dec_str,$break) .
						") H$covariate = $covariate" .sprintf($self->sign_dec_str,-$break) ."\n";
						$if_printed{'H'.$covariate}=1;
					}
					push @new_code, $self->blank . "$hname = THETA(". $tmp[4] .")*(H$covariate)\n";
					if (defined $selected_cont{$par}){ 
						$selected_cont{$par} = $selected_cont{$par}. " ". $hname;
					}else { 
						$selected_cont{$par} = $hname;
					}

				}
				if (defined $keep_parameters{$name}){
					my @tmp = split(' ',$keep_parameters{$name});
					push @new_code, $self->blank . "$name = THETA(". $tmp[4] .
					")*($covariate".sprintf($self->sign_dec_str,-$break).")\n";
					if (defined $selected_cont{$par}){ 
						$selected_cont{$par} = $selected_cont{$par}. " ". $name;
					}else { 
						$selected_cont{$par} = $name;
					}
				}

			}elsif ($parameter_covariate_form{$par}{$covariate} == 1){
				my $first_cat = 1;
				my $add = 0;
				foreach my $fact (sort {$a<=>$b} keys %{$self -> statistics->{$covariate}{1}{'cat_hash'}}) {

					my $name = $par.$covariate.$fact;
					if (defined $keep_parameters{$name}){
						my @tmp = split(' ',$keep_parameters{$name});
						push @new_code,$self->blank ."IF (".$covariate." .EQ. $fact) ".$par."COV=".$par."COV*(1+THETA(".$tmp[4]."))\n";
					}
				}
			}
		}
	}
	push @new_code, "\n";
	foreach my $par (keys %selected_cont){
		my $str = $par ."COV = $par" ."COV";
		my @tmp = split(' ',$selected_cont{$par});
		foreach my $t (@tmp) {
			$str = $str . "*($t+1)";
		}
		push @new_code,@{$self->parse_row(parse_str => $self->blank . $str,
		parse_operator =>"*",
		max_length => $self->row_length)};


	}
	push @new_code,@code;
	if ($self->use_pred){
		$self->model_optimal->pred(new_pred => \@new_code);
	}else{
		$self->model_optimal->pk(new_pk => \@new_code);
	}

	if ($self->NOABORT_added){
		$self->model_optimal->remove_option(record_name => 'estimation',
			option_name => 'NOABORT');
	}
	ui -> print( category => 'lasso',
		message  => "Running normal model with covariate relations added.\n" );
	$self->model_optimal->_write();
	$self->tools([]) unless (defined $self->tools);
	push( @{$self -> tools},
		tool::modelfit -> new (%{common_options::restore_options(@common_options::tool_options)},
			seed => $common_seed,
			models    => [$self->model_optimal],
			threads    => 1,
			base_directory => $self->directory(),
			directory => $self->directory().'final_model_modelfit_dir'));
}

sub _modelfit_raw_results_callback
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};
	my $subroutine;

	return \&subroutine;
}

sub modelfit_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	return unless (defined $self -> model_optimal);

	if (not defined $self -> model_optimal-> outputs -> [0] or 
		not $self->model_optimal->outputs->[0]->get_single_value(attribute =>'minimization_successful')){
		my $round;
		if (defined $self -> model_optimal->outputs->[0]->get_single_value (attribute => 'rounding_errors')
				and $self -> model_optimal->outputs->[0]->get_single_value (attribute => 'rounding_errors')){
			$round = ', rounding errors';
		}

		ui -> print( category => 'lasso',  message  => "No successful minimization of optimal model$round" );
	}
	ui -> print( category => 'lasso',  message  => "\nLasso done." );
	if ($self->warnings == 1){
		ui -> print( category => 'lasso',  message  => "There was 1 warning, please check ".
			$self->logfile->[0]." for details."); 

	}elsif ($self->warnings > 0){
		ui -> print( category => 'lasso', 
			message  => "There were ".$self->warnings." warnings, please check ".
			$self->logfile->[0]." for details."); 
	}
}

sub prepare_results
{
	my $self = shift;
}

sub cleanup
{
	my $self = shift;

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
