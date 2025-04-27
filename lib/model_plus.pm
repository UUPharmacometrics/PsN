package model_plus;

use strict;
use MouseX::Params::Validate;
use File::Spec;
require model;
require model::problem;


sub set_specific_and_common_maxevals
{
	my %parm = validated_hash(\@_,
							  options => {isa => 'HashRef'},
		);
	my $options = $parm{'options'};

	my $specific_maxevals = undef;
	if (defined $options->{'maxevals'} and (length($options->{'maxevals'})>0)){
		unless (($options->{'maxevals'} =~ /^\d+$/) and ($options->{'maxevals'} >= 10000)){
			#unless integer suitable for common option maxevals
			$specific_maxevals = $options->{'maxevals'};
			$options->{'maxevals'} = undef; #remove common option
		}
	}
	return $specific_maxevals;
}

sub get_new_maxeval
{
	my %parm = validated_hash(\@_,
							  specific_maxevals => {isa => 'Maybe[Str]'},
							  reference_model_evaluations => {isa => 'Maybe[Int]'},
		);
	my $specific_maxevals = $parm{'specific_maxevals'};
	my $reference_model_evaluations = $parm{'reference_model_evaluations'};

	my $new_maxeval = undef;
	if (defined $specific_maxevals and (length($specific_maxevals)>0)){
		if ($specific_maxevals =~ /^\d+\.\d+$/){
			if (defined $reference_model_evaluations){
				$new_maxeval = int($specific_maxevals * $reference_model_evaluations);
				$new_maxeval = 9999 if ($new_maxeval > 9999);
				$new_maxeval = 1 if ($new_maxeval < 1);
			}
		}elsif($specific_maxevals =~ /^\d+$/){
			$new_maxeval=$specific_maxevals;
		}else{
			die("option -maxevals is not a valid number: $specific_maxevals");
		}
	}
	return $new_maxeval;
}

sub get_number_of_evaluations
{
	my %parm = validated_hash(\@_,
							  model => {isa => 'model'},
							  problem_number => {isa => 'Int', optional=>1},
		);
	my $model = $parm{'model'};
	my $problem_number = $parm{'problem_number'};

	my $index = $model->nproblems()-1;
	$index = ($problem_number-1) if (defined $problem_number);
	if ($model->outputs->[0]->load()){
		#something went wrong with load
		return undef;
	}
	return $model->outputs->[0]->get_single_value(attribute => 'feval',problem_index => $index);	
}

sub check_sigl
{
	my %parm = validated_hash(\@_,
							  model => {isa => 'model', optional => 0},
							  ignore_no_sigl => {isa => 'Bool', optional => 0},
		);
	my $model = $parm{'model'};
	my $ignore_no_sigl = $parm{'ignore_no_sigl'};

	my $error = [];
	my $warning=[];
	my $info=[];

	if (not defined $model->problems->[-1]->estimations() or
		scalar(@{$model->problems->[-1]->estimations()})<1){
		push(@{$warning},"The last \$PROBLEM of the input model has no \$ESTIMATION");
	}else{
		my $recno = scalar(@{$model->problems->[-1]->estimations()});
		my $has_sigl = $model->is_option_set(name => 'SIGL',
											 record => 'estimation',
											 fuzzy_match => 0,
											 record_number => $recno,
											 problem_number => scalar(@{$model->problems()}));

		my $advan = get_advan_number(model => $model);
		if ($has_sigl){
			push(@{$info},"SIGL is set in \$EST");
		}else{
			#check which advan
			if (defined $advan){
				if ($advan == 6 or $advan == 8 or $advan == 9 or $advan == 13){
					my $text = "ADVAN$advan is used but SIGL is not set in \$EST";
					if ($ignore_no_sigl){
						push(@{$warning},$text);
					}else{
						push(@{$error},$text.". To allow this use option -ignore_no_sigl");
					}
				}else{
					push(@{$info},"ADVAN$advan is used and SIGL is not set in \$EST");
				}
			}
		}
	}
	return ($error,$warning,$info);

}

sub get_advan_number
{
	my %parm = validated_hash(\@_,
							  model => {isa => 'model', optional => 0},
		);
	my $model = $parm{'model'};

	# Figure out wheter we have an 'ADVAN' option. By not using
	# "exact_match" we can search for a prefix of the different ADVAN
	# options.

	my $advan=undef;
	if (defined $model->problems->[-1]->subroutines){
		foreach my $subr (@{$model->problems->[-1]->subroutines}){
			if (defined $subr->options()){
				foreach my $opt (@{$subr->options()}){
					if ($opt->name =~ /^ADVAN(\d+)/){
						$advan = $1;
						last;
					}
				}
				last if (defined $advan);
			}
		}
		
	}
	return $advan;

}

sub tune_ctype4
{
	my %parm = validated_hash(\@_,
							  model => {isa => 'model', optional => 0},
		);
	my $model = $parm{'model'};

	my $error = [];
	my $warning=[];
	my $info=[];
	
	if (not defined $model->problems->[-1]->estimations() or
		scalar(@{$model->problems->[-1]->estimations()})<1){
		push(@{$error},"The last \$PROBLEM of the input model has no \$ESTIMATION");
	}elsif($model->problems->[-1]->estimations->[-1]->is_classical){
		$model -> set_option(record_name => 'estimation',option_name => 'CTYPE',
							 fuzzy_match => 1, option_value => '4', 
							 problem_numbers => [(scalar(@{$model->problems}))]);
		push(@{$info},"Set CTYPE=4");
	}else{
		push(@{$info},"Did not set CTYPE=4 because estimation method not classical");
	}
	
	return ($error,$warning,$info);

}

sub update_etas_filename
{
	my %parm = validated_hash(\@_,
							  model => {isa => 'model', optional => 0},
							  source_model => { isa => 'Maybe[model]',optional => 1},
							  initiate_extra_files => {isa => 'Bool',default => 0},
							  etas_fullname => {isa => 'Maybe[Str]',optional => 1},
		);
	my $model = $parm{'model'};
	my $source_model = $parm{'source_model'};
	my $initiate_extra_files = $parm{'initiate_extra_files'};
	my $etas_fullname = $parm{'etas_fullname'};
	my $error = [];
	my $warning=[];
	my $info=[];

	unless (defined $etas_fullname){
		if (defined $source_model){
			my $phi_name = utils::file::replace_extension($source_model->filename, 'phi');
			$etas_fullname = $source_model->directory.$phi_name;
		}else{
			push(@{$error},"Trying to update \$ETAS but neither eta filename or source model defined");
			return ($error,$warning,$info);
		}
	}
	unless (-e $etas_fullname){
		push(@{$warning},"cannot find phi-file for ETAS: $etas_fullname");
	}
	
	my $probnum = scalar(@{$model->problems()});
	if (defined $model->problems->[-1]->estimations() and
		scalar(@{$model->problems->[-1]->estimations()})>0 and
		$model->problems->[-1]->estimations->[-1]->is_classical){
		if(	defined $model->problems->[-1]->etass() and
			scalar(@{$model->problems->[-1]->etass()})>0){
			my ($volume,$directories,$filename) = File::Spec->splitpath( $etas_fullname );
			if (length($filename)>0){
				$model -> set_option(record_name => 'etas',
									 option_name => 'FILE',
									 fuzzy_match => 1, 
									 option_value => $filename, 
									 problem_numbers => [($probnum)]);
				push(@{$info},"Set FILE=$filename in \$ETAS");

				my @extra_files =();
				if (defined $model->extra_files()){
					push(@extra_files,@{$model->extra_files});
				}
				unless ($initiate_extra_files){
					if (scalar(@extra_files)>0){
						pop(@extra_files);
					}else{
						push(@{$warning},
							 "Trying to update \$ETAS but it seems model has not had extra_files initiated");
					}
				}
				push(@extra_files,$etas_fullname);
				$model->extra_files(\@extra_files);
				push(@{$info},"set $etas_fullname in extra_files");
			}else{
				push(@{$error},"Could not get filename from $etas_fullname");
			}
		}else{
			push(@{$error},"Trying to update \$ETAS but it seems model has not had the record initiated");
		}
	}
	return ($error,$warning,$info);

}

sub tune_etas
{
	my %parm = validated_hash(\@_,
							  model => {isa => 'model', optional => 0},
							  etas_fullname => {isa => 'Maybe[Str]'},
		);
	my $model = $parm{'model'};
	my $etas_fullname = $parm{'etas_fullname'};
	my $error = [];
	my $warning=[];
	my $info=[];

	my $probnum = scalar(@{$model->problems()});
	if (not defined $model->problems->[-1]->estimations() or
		scalar(@{$model->problems->[-1]->estimations()})<1){
		push(@{$error},"The last \$PROBLEM of the input model has no \$ESTIMATION");
	}elsif($model->problems->[-1]->estimations->[-1]->is_classical){
		if (not defined $model->problems->[-1]->etass() or
			scalar(@{$model->problems->[-1]->etass()})<1){
			$model->add_records(type => 'etas',
								record_strings => [''],
								problem_numbers =>[$probnum]);
			push(@{$info},"Add record \$ETAS");
		}
		my $infos=[];
		($error,$warning,$infos) =  update_etas_filename(model => $model,
														 initiate_extra_files => 1,
														 etas_fullname => ((defined $etas_fullname)? $etas_fullname : undef ),
														 source_model => ((defined $etas_fullname)? undef : $model));
		push(@{$info},@{$infos});
		my $recno = scalar(@{$model->problems->[-1]->estimations()});
		my $current_mceta = $model->get_option_value (record_name => 'estimation',
													  option_name=>'MCETA',
													  problem_index => ($probnum-1), 
													  record_index=>($recno-1),
													  fuzzy_match =>1,
													  option_index=>0);
		if (defined $current_mceta and ($current_mceta > 0)){
			push(@{$info},"MCETA is set in \$EST");
		}else{
			$model -> set_option(record_name => 'estimation',option_name => 'MCETA',
								 fuzzy_match => 1, option_value => '1', 
								 problem_numbers => [($probnum)]);
			push(@{$info},"Set MCETA=1 in \$EST so that \$ETAS will have effect");
		}
	}else{
		push(@{$info},"Did not set \$ETAS because estimation method not classical");
	}
	return ($error,$warning,$info);

}

sub tune_maxevals
{
	my %parm = validated_hash(\@_,
							  model => {isa => 'model', optional => 0},
							  maxevals => {isa => 'Str'},
							  reference_evaluations => {isa=> 'Maybe[Int]', optional => 1}
		);
	my $model = $parm{'model'};
	my $maxevals = $parm{'maxevals'};
	my $reference_evaluations = $parm{'reference_evaluations'};

	my $error = [];
	my $warning=[];
	my $info=[];
	my $new_maxeval = get_new_maxeval(specific_maxevals => $maxevals,
									  reference_model_evaluations => $reference_evaluations);

	if (defined $new_maxeval){
		if (not defined $model->problems->[-1]->estimations() or
			scalar(@{$model->problems->[-1]->estimations()})<1){
			push(@{$error},"The last \$PROBLEM of the input model has no \$ESTIMATION");
		}elsif($model->problems->[-1]->estimations->[-1]->is_classical){
			$model -> set_option(record_name => 'estimation',option_name => 'MAXEVALS',
								 fuzzy_match => 1, option_value => $new_maxeval, 
								 problem_numbers => [(scalar(@{$model->problems}))]);
			push(@{$info},"Set MAXEVALS=$new_maxeval");
		}else{
			push(@{$info},"Did not set MAXEVALS because estimation method not classical");
		}
	}else{
		push(@{$warning},"new maxeval undefined based on maxevals $maxevals and".
			" reference $reference_evaluations: will not change MAXEVAL");
	}
	return ($error,$warning,$info);
}

sub check_model
{
	my %parm = validated_hash(\@_,
							  model => {isa => 'model', optional => 0},
							  single_problem => {isa => 'Bool'},
							  single_estimation => {isa => 'Bool'},
							  classical_estimation => {isa => 'Bool'},
		);
	my $model = $parm{'model'};
	my $single_problem = $parm{'single_problem'};
	my $single_estimation = $parm{'single_estimation'};
	my $classical_estimation = $parm{'classical_estimation'};

	my @warnings=();
	
	if ($single_problem and scalar(@{$model->problems})>1){
		push(@warnings,"The input model has ".scalar(@{$model->problems})." \$PROBLEM ".
			 "but only the last one will be tuned");
	}
	if (not defined $model->problems->[-1]->estimations() or
		scalar(@{$model->problems->[-1]->estimations()})<1){
		push(@warnings,"The last \$PROBLEM of the input model has no \$ESTIMATION");
	}else{
		if ($single_estimation and scalar(@{$model->problems->[-1]->estimations()})>1){
			push(@warnings,"The last \$PROBLEM of the input model has ".scalar(@{$model->problems->[-1]->estimations()}).
				 " \$ESTIM but only the last one will be tuned");
		}
		unless ($model->problems->[-1]->estimations->[-1]->is_classical){
			push(@warnings,"The last \$ESTIM of the last \$PROBLEM does not use a classical estimation method");
		}
	}

	return \@warnings;
}

sub report
{
	my %parm = validated_hash(\@_,
							  errors => {isa => 'ArrayRef', optional => 0},
							  warnings => {isa => 'ArrayRef', optional => 0},
							  information => {isa => 'ArrayRef', optional => 0},
							  category => {isa => 'Str',optional => 0},
							  scriptname => {isa => 'Str',optional => 0});
	
	my $errors = $parm{'errors'};
	my $warnings = $parm{'warnings'};
	my $information = $parm{'information'};
	my $category = $parm{'category'};
	my $scriptname = $parm{'scriptname'};
	
	if (scalar(@{$errors})>0){
		die(join("\n","Terminating $scriptname due to model errors: ",@{$errors}));
	}
	if (scalar(@{$warnings})>0){
		ui -> print( category => $category,
					 message  => join("\n","$scriptname model warnings: ",@{$warnings} )); 
	}
	if (scalar(@{$information})>0){
		ui -> print( category => $category,
					 message  => join("\n","$scriptname model information: ",@{$information})); 
	}
}

sub tune
{
	my %parm = validated_hash(\@_,
							  model => {isa => 'model', optional => 0},
							  keep_covariance => {isa => 'Bool'},
							  etas => {isa => 'Bool'},
							  etas_fullname => {isa => 'Maybe[Str]',optional=>1},
							  keep_tables => {isa => 'Bool'},
							  ctype4 => {isa => 'Bool'},
							  ignore_no_sigl => {isa => 'Bool'},
							  maxevals => {isa => 'Maybe[Str]'},
							  reference_evaluations => {isa=> 'Maybe[Int]', optional => 1}
		);
	my $model = $parm{'model'};
	my $keep_covariance = $parm{'keep_covariance'};
	my $etas_fullname = $parm{'etas_fullname'};
	my $etas = $parm{'etas'};
	my $keep_tables = $parm{'keep_tables'};
	my $ctype4 = $parm{'ctype4'};
	my $ignore_no_sigl = $parm{'ignore_no_sigl'};
	my $maxevals = $parm{'maxevals'};
	my $reference_evaluations = $parm{'reference_evaluations'};

	my @errors=();
	my @warnings=();
	my @information=();
	
	my ($error,$warning,$info);
	
	if ($ctype4 or (not $ignore_no_sigl) or (defined $maxevals)){
		push(@warnings,@{check_model(model => $model, 
									 single_problem => 1, 
									 single_estimation => 1, 
									 classical_estimation => 1)});
	}

	if ($etas){
		($error,$warning,$info) = tune_etas(model => $model,
											etas_fullname => $etas_fullname);
		push(@errors,@{$error});
		push(@warnings,@{$warning});
		push(@information,@{$info});
	}
	
	if (defined $maxevals){
		($error,$warning,$info) = tune_maxevals(model => $model,
												maxevals => $maxevals,
												reference_evaluations => $reference_evaluations);
		push(@errors,@{$error});
		push(@warnings,@{$warning});
		push(@information,@{$info});
	}
	if ($ctype4){
		($error,$warning,$info) = tune_ctype4(model => $model);
		push(@errors,@{$error});
		push(@warnings,@{$warning});
		push(@information,@{$info});
	}
	if (not $keep_covariance){
		push(@information,"Removing all \$COVARIANCE from the model");
		$model->remove_records(type => 'covariance');
	}
	if (not $keep_tables){
		push(@information,"Removing all \$TABLE from the model");
		$model->remove_records(type => 'table');
	}

	if (defined $ignore_no_sigl){
		($error,$warning,$info) = check_sigl(model => $model,
											 ignore_no_sigl => $ignore_no_sigl);
		push(@errors,@{$error});
		push(@warnings,@{$warning});
		push(@information,@{$info});
	}

	return (\@errors,\@warnings,\@information);
}

1;
