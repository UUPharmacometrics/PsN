package tool::npc;

use include_modules;
use Data::Dumper;
use Math::Random;
use strict;
use tool::modelfit;
use model;
use ui;
use Config;
use OSspecific;
use File::Copy qw/cp mv/;
use File::Spec;
use Cwd;
use binning;
use Moose;
use MooseX::Params::Validate;

extends 'tool';

has 'is_vpc' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'searchdir' => ( is => 'rw', isa => 'Str' );
has 'refstrat' => ( is => 'rw', isa => 'Num', clearer => 'clear_refstrat' );
has 'fine_pi' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'confidence_interval' => ( is => 'rw', isa => 'Int', default => 95 );
has 'covariance_file' => ( is => 'rw', isa => 'Str' );
has 'rawres_input' => ( is => 'rw', isa => 'Str' );
has 'offset_rawres' => ( is => 'rw', isa => 'Int', default => 1 );
has 'have_nwpri' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'have_tnpri' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'copy_data' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'simprobnum' => ( is => 'rw', isa => 'Int', default => 1 );
has 'origprobnum' => ( is => 'rw', isa => 'Int', default => 1 );
has 'boxcox_lambda' => ( is => 'rw', isa => 'Num', default => 0 );
has 'auto_bin_mode' => ( is => 'rw', isa => 'Str' );
has 'min_no_bins' => ( is => 'rw', isa => 'Num' );
has 'max_no_bins' => ( is => 'rw', isa => 'Num' );
has 'min_points_in_bin' => ( is => 'rw', isa => 'Num' );
has 'predcorr' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'lnDV' => ( is => 'rw', isa => 'Int', default => 0 );
has 'lower_bound' => ( is => 'rw', isa => 'Str' );
has 'bound_variable' => ( is => 'rw', isa => 'Str' );
has 'npc_alert_written' => ( is => 'rw', isa => 'Bool' );
has 'detection_censored' => ( is => 'rw', isa => 'Bool' );
has 'run_the_original' => ( is => 'rw', isa => 'Bool' );
has 'run_the_sim' => ( is => 'rw', isa => 'Bool' );
has 'data_matrix' => ( is => 'rw', isa => 'ArrayRef', clearer => 'clear_data_matrix' );
has 'censor_data_matrix' => ( is => 'rw', isa => 'ArrayRef', clearer => 'clear_censor_data_matrix' );
has 'n_simulations' => ( is => 'rw', isa => 'Int' );
has 'n_observations' => ( is => 'rw', isa => 'Int' );
has 'strata_matrix' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'strata_labels' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'strata_variable_vector' => ( is => 'rw', isa => 'Maybe[ArrayRef]', default => sub { [] }, clearer => 'clear_strata_variable_vector' );
has 'stratified_data' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'idv_array' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] }, clearer => 'clear_idv_array' );
has 'pred_array' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'bound_array' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'id_array' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] }, clearer => 'clear_id_array' );
has 'binned_data' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'censor_binned_data' => ( is => 'rw', isa => 'ArrayRef' );
has 'bin_ceilings' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'bin_floors' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'binned_id' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'binned_idv' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'binned_strt' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'vpctab_filename' => ( is => 'rw', isa => 'Str' );
has 'mirror_labels' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'mirror_set' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'vpctab_header' => ( is => 'rw', isa => 'Str' );
has 'censor_stratified_data' => ( is => 'rw', isa => 'ArrayRef' );
has 'varcorr' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'noprediction' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'samples' => ( is => 'rw', isa => 'Int' );
has 'extra_table_parameters' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'dv' => ( is => 'rw', isa => 'Str', default => 'DV' );
has 'orig_table' => ( is => 'rw', isa => 'Str' );
has 'sim_table' => ( is => 'rw', isa => 'Str' );
has 'keep_estimation' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'dv_table_name' => ( is => 'rw', isa => 'Str' );
has 'n_simulation_models' => ( is => 'rw', isa => 'Int', default => 1 );
has 'idv' => ( is => 'rw', isa => 'Str' );
has 'bin_by_count' => ( is => 'rw', isa => 'Bool' );
has 'no_of_bins' => ( is => 'rw', isa => 'Int' );
has 'single_bin_size' => ( is => 'rw', isa => 'Int' );
has 'overlap_percent' => ( is => 'rw', isa => 'Num' );
has 'bin_array' => ( is => 'rw', isa => 'ArrayRef[Num]', default => sub { [] } );
has 'categorized' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'levels' => ( is => 'rw', isa => 'ArrayRef[Num]' );
has 'lloq' => ( is => 'rw', isa => 'Num' );
has 'uloq' => ( is => 'rw', isa => 'Num' );
has 'stratify_on' => ( is => 'rw', isa => 'Str' );
has 'censor' => ( is => 'rw', isa => 'Str' );
has 'tte' => ( is => 'rw', isa => 'Str' );
has 'sim_model' => ( is => 'rw', isa => 'Str' );
has 'flip_comments' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'no_of_strata' => ( is => 'rw', isa => 'Int' );
has 'lst_file' => ( is => 'rw', isa => 'Str' );
has 'msfo_file' => ( is => 'rw', isa => 'Str' );
has 'mirrors' => ( is => 'rw', isa => 'Int' );
has 'simulation_models' => ( is => 'rw', isa => 'ArrayRef' );
has 'original_model' => ( is => 'rw', isa => 'model' );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['npc.lop'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'npc_results.csv' );
has 'nca' => ( is => 'rw', isa => 'Bool', default => 0 );

sub BUILD
{
	my $self  = shift;

	if (defined $self->auto_bin_mode) {
		unless ($self->auto_bin_mode eq 'auto' or $self->auto_bin_mode eq 'minmax') {
			croak("There are only two possible auto_bin modes: auto and minmax\n");
		}

		if ($self->min_no_bins > $self->max_no_bins) {
			croak("min_no_bins cannot be bigger than max_no_bins\n");
		}
	}

	if ((defined $self->sim_table) || (defined $self->orig_table)) {
		unless ((defined $self->sim_table) && (defined $self->orig_table)) {
			croak("Options -sim_table and -orig_table can only " .
				  "be used together, not individually\n");
		}
		unless ( -e $self->sim_table ) {
			croak("Simulation data table file " . $self->sim_table . " could not be found.");
		}
		unless ( -e $self->orig_table ) {
			croak("Original data table file " . $self->orig_table . " could not be found.");
		}
		if ($self->n_simulation_models > 1) {
			croak("Options n_simulation_models and -sim_table cannot be used together.");
		}
		croak("Options -sim_model or -flip_comments cannot be used with -sim_table") 
			if (defined $self->sim_model or $self->flip_comments);
	}
	if (defined $self->sim_model or $self->flip_comments) {
		croak("Options -sim_model and -flip_comments cannot be used together.") 
			if (defined $self->sim_model and $self->flip_comments);
		if ($self->keep_estimation or $self->noprediction) {
			croak('Options -sim_model or -flip_comments cannot be used with '.
				  '-keep_estimation or -noprediction.');
		}
		if ($self->orig_table) {
			croak('Options -sim_model or -flip_comments cannot be used with -orig_table');
		}
	}
	if (defined $self->tte) {
		croak("Options -tte and -mirrors cannot be used together.") 
			if (defined $self->mirrors);
		croak("Options -tte and -censor cannot be used together.") 
			if (defined $self->censor);
		croak("Options -tte and -predcorr cannot be used together.") 
			if ($self->predcorr);
		croak("Options -tte and -varcorr cannot be used together.") 
			if ($self->varcorr);
		croak("Options -tte and -lnDV cannot be used together.") 
			if ($self->lnDV);
		croak("Options -tte and -boxcox_lambda cannot be used together.") 
			if ($self->boxcox_lambda);
		croak("Options -tte and -uloq cannot be used together.") 
			if (defined $self->uloq);
		croak("Options -tte and -lloq cannot be used together.") 
			if (defined $self->lloq);
	}

	if (( $self->confidence_interval() < 1) or ( $self->confidence_interval() > 99)) {
		croak("Confidence interval must be between 1 and 99 percent.");
	}
	if (( $self->boxcox_lambda() < 0) or ( $self->boxcox_lambda() >= 1)) {
		croak("boxcox-lambda must be > 0 and < 1.");
	}
	if ( $self->boxcox_lambda() > 0) {
		if (defined $self->lnDV and ($self->lnDV > 0)) {
			croak("Option -lnDV is not allowed when -boxcox_labmda is set.");
		}
		ui -> print (category=>'all', 
					 message=>"Warning: option -boxcox_lambda has no effect unless option -predcorr is set.")	unless ($self->predcorr);
	}


	if (defined $self->samples) {
		if ( $self->samples < 20 ) {
			croak("Program does not allow less than 20 samples.");
		}
	} else {
		croak("Option -samples is required.");
	}

	if ($self->dv =~ /^(PRED|MDV|TIME|DATE|EVID|AMT|RATE|ID)$/) {
		croak("It is not allowed to use ${\$self->dv} as the dependent".
			  " variable,\nbecause it".
			  " is the same for all simulations of a single observation.");
	}
	
	if ($PsN::nm_major_version < 7) {
		unless ($self->dv =~ /^(IPRED|IWRES|CWRES)$/ ) {
			if (length($self->dv) > 4) {
				croak("The dependent variable name (option -dv) can have at most".
					  " 4 letters, unless it is IPRED/IWRES/CWRES, due to a limitation in NONMEM.");
			}
		}
		if ($self->dv =~ /^CWRES$/ ) {
			if ($self->n_simulation_models > 1) {
				croak(
					"Cannot use option -n_simulation_models if dv=CWRES and NONMEM version < 7");
			}
		}
	}

	if (defined $self->mirrors) {
		if ( $self->mirrors > 20) {
			croak("Program does not allow more than 20 mirror plots.");
		}
		if (defined $self->censor || (defined $self->lloq ) || (defined $self->uloq)) {
			ui -> print (category => 'all', 
						 message => "\nWarning: The mirror plot output in vpctab ".
						 "will not be correctly filtered when censor/lloq/uloq is used.\n");
		}
	}

	unless ( defined $self->orig_table) {

		if (defined $self->sim_model) {
			if (scalar (@{$self->models->[0]-> problems}) > 1 ) {
				croak('Cannot have more than one $PROB in the input model for the MAXEVAL=0 run.');
			}
		} else {
			#must check sim_model separately if sim_model. Assume ok to check flip here
			if (scalar (@{$self->models->[0]->problems}) > 2 ) {
				croak('Cannot have more than two $PROB in the simulation model.');
			} elsif (scalar (@{$self->models->[0]->problems}) == 2 ) {
				if ((defined $self->models->[0]->problems->[0]->priors()) and 
					scalar(@{$self->models->[0]->problems->[0]->priors()}) > 0 ) {
					my $tnpri = 0;
					foreach my $rec (@{$self->models->[0]->problems->[0]->priors()}) {
						unless ((defined $rec) &&( defined $rec->options )) {
							carp("No options for rec \$PRIOR" );
						}
						foreach my $option ( @{$rec->options} ) {
							if ((defined $option) and 
								(($option->name eq 'TNPRI') || (index('TNPRI', $option->name ) == 0))) {
								$tnpri = 1;
							}
						}
					}
					
					$self->have_tnpri(1) if ($tnpri);
				}
				if ($self->have_tnpri()) {
					if (defined $self->rawres_input) {
						croak('Cannot use option rawres_input if the simulation model has $PRIOR.');
					}
					unless( defined $self->models->[0]->extra_files ) {
						croak('When using $PRIOR TNPRI you must set option -extra_files to '.
							  'the msf-file, otherwise the msf-file will not be copied to the NONMEM '.
							  'run directory.');
					}

				} else {
					croak('The simulation model must contain exactly one problem, unless'.
						  ' first $PROB has $PRIOR TNPRI');
				}
			}
			if ((not $self->have_tnpri()) and
				(defined $self->models->[0]->problems->[0]->priors()) and 
				scalar(@{$self->models->[0]->problems->[0]->priors()}) > 0 ) {
				$self->have_nwpri(1);
				if (defined $self->rawres_input) {
					croak('Cannot use option rawres_input if the simulation model has $PRIOR.');
				}
			}

			$self->simprobnum(2) if ($self->have_tnpri());
			$self->origprobnum(2) if ($self->have_tnpri());
		}
	}

	if (defined $self->refstrat() and (defined $self->no_of_strata())){
	    croak("It is not allowed to set refstrat together with no_of_strata");
	}

	if (defined $self->stratify_on) {
		my @list = split(',', $self->stratify_on);
		my $tmp = shift @list;
		$self->stratify_on($tmp);
		$self->extra_table_parameters(\@list) if (scalar (@list)>0);
	} else {
		if (defined $self->refstrat()) {
			croak("It is not allowed to set refstrat unless stratify_on is set");
		}
	}
	

	my @check_cols;
	push (@check_cols, $self->idv) if (defined $self->idv);
	push (@check_cols, $self->stratify_on) 
		if ((defined $self->stratify_on) && ('STRT' ne $self->stratify_on));
	foreach my $col (@check_cols) {
		my $var_text = ($col eq $self->stratify_on) ? 'stratification' : 'independent';
		if ($col =~ /^(IPRED|IWRES|IRES|RES|WRES|DV|CWRES)$/) {
			croak("It is not allowed to use $col as the $var_text ".
				  "variable, since it it is dependent and hence differs between ".
				  "simulations of the same observation.");
		}

		unless ( defined $self->orig_table) {
			unless ($col =~ /^(PRED)$/) {
				my $input_record = $self->models->[0]->record(record_name => 'input');
				my $found = 0;
				if ( scalar(@{$input_record}) > 0 ) { #always true
					foreach my $line ( @{$input_record->[0]} ) {
						next if ( $line =~ /^\s*;/); #skip comments
						if ( $line =~ /[\s]+$col=(SKIP|DROP)[\s]+/ ) {
							$found = 1;
							croak("Cannot SKIP/DROP the $var_text variable ".
								  "in the \$INPUT record.");
							last;
						} elsif ( $line =~ /[\s]+(SKIP|DROP)=$col[\s]+/ ) {
							$found = 1;
							croak("Cannot SKIP/DROP the $var_text variable ".
								  "in the \$INPUT record.");
							last;
						} elsif ($line =~ /[=\s]+$col[=\s]+/) {
							$found = 1;
							last;
						}
					}
				}
				ui -> print (category=>'vpc', 
							 message=>"\n\nWarning: The $var_text variable $col was ".
							 "not found in the \$INPUT record. ".
							 "If $col is not defined in the model, NONMEM will exit with error. ".
							 "If $col varies between the original data and/or simulated datasets, the  ".
							 "vpc output will be incorrect.\n") if !($found);
				ui -> print (category=>'npc', 
							 message=>"\n\nWarning: The $var_text variable $col was ".
							 "not found in the \$INPUT record. ".
							 "If $col is not defined in the model, NONMEM will exit with error. ".
							 "If $col varies between the original data and/or simulated datasets, the  ".
							 "npc output will be incorrect.\n") if !($found);
			}
		}
	}

	if ($self->categorized) {
		for (my $i = 1; $i < scalar(@{$self->levels}); $i++) {
			unless ($self->levels->[$i] > $self->levels->[$i - 1]) {
				croak("List of category levels must be sorted in ".
					  "increasing order.");
			}
		}
	}
	if ((defined $self->lloq ) && (defined $self->uloq)) {
		unless ($self->lloq < $self->uloq) {
			croak("lloq must be smaller than uloq.");
		}
	}

	if (defined $self->censor || (defined $self->lloq ) || (defined $self->uloq)) {
		if ($self->predcorr) {
			ui -> print (category => 'vpc',
						 message => "Warning: When -censor/-lloq/-uloq is used in combination with -predcorr/-varcorr, ".
						 "the prediction/variability correction is performed based on all PRED and all simulated ".
						 "dependent variables, including those that are censored in later analyses. Therefore it ".
						 "is important to simulate reasonable values for the dependent variable ".
						 "even, for example, after drop-out.");
		}
	}

	if (($self->have_tnpri or $self->have_nwpri or (defined $self->rawres_input)) and
		$self->predcorr){
		print "\n\nWarning: Prediction correction in combination with simulation with uncertainty is ".
			"not supported. PsN will use the same PRED values (from original data) for all ".
			"simulated dataset, even though PRED differs between simulations.\n\n";
	}
	##Checks for VPC
	if ($self->is_vpc) {

		if (defined $self->lower_bound) {
			croak("Option -lower_bound is only allowed together with -predcorr.") 
				unless ($self->predcorr);
		}

		if ((defined $self->lloq ) || (defined $self->uloq) ) {
			if ($self->predcorr) {
				ui -> print (category => 'vpc', 
							 message => "Warning: When -lloq/-uloq is used in combination with -predcorr, ".
							 "the censoring is performed before prediction correction. ".
							 "Therefore it could happen that some values which are originally within the boundaries of ".
							 "-lloq and -uloq appear to be outside these boundaries after the correction.");
			}
		}

		if (defined $self->lnDV and ($self->lnDV > 0)) {
			if (defined $self->lower_bound) {
				croak("Option -lower_bound is not allowed when -lnDV=" . $self->lnDV);
			}
		}
		if ($self->lnDV == 1) {
			if ($self->stratify_on eq 'PRED') {
				ui -> print (category => 'vpc', 
							 message => "Warning: Exponentiation of PRED and DV will be performed,\n".
							 "but strata boundaries from stratification on PRED will\n".
							 "be presented in vpc_results.csv as non-exponentiated values.");
			}
			if ( ($self->categorized) || (defined $self->lower_bound) ||
				 (defined $self->lloq ) || (defined $self->uloq)) {
				ui -> print (category => 'vpc', 
							 message => "Warning: Exponentiation of PRED and DV will be performed,\n".
							 "but levels/boundaries given via options -lloq/-uloq/-levels/-lower_bound\n".
							 "will not be exponentiated (they must be given on the real scale).");
			}
		} elsif ($self->lnDV == 2) {
			croak("-lnDV=2 is only allowed together with -predcorr.") 
				unless ($self->predcorr);
		} elsif ($self->lnDV == 3) {
			if ($self->stratify_on eq 'PRED') {
				ui -> print (category => 'vpc', 
							 message=>"Warning: Log-transformation of PRED and DV will be performed,\n".
							 "but strata boundaries from stratification on PRED will\n".
							 "be presented in vpc_results.csv as non-transformed values.");
			}
			if (($self->categorized) || (defined $self->lower_bound)
				|| (defined $self->lloq ) || (defined $self->uloq)) {
				ui -> print (category => 'vpc', 
							 message => "Warning: Log-transformation of PRED and DV will be performed,\n".
							 "but levels/boundaries given via options -lloq/-uloq/-levels/-lower_bound\n".
							 "will not be transformed (they must be given on the log scale).");
			}
		} elsif ($self->lnDV != 0) {
			croak("Option -lnDV must be either 0, 1, 2 or 3.");
		}

		if ($self->varcorr) {
			croak("option -varcorr is only allowed together with option -predcorr.")
				unless ($self->predcorr);
		}

		unless (defined $self->idv) {
			croak("VPC requires an independent variable column.");
		}
		
		my $option_count = 0;
		if (defined $self->no_of_bins) {
			$option_count++;
			unless (defined $self->bin_by_count) {
				croak("Option bin_by_count must be defined when option no_of_bins is used.");
			}
		}
		if (scalar(@{$self->bin_array} > 0)) {
			if ($option_count > 0) {
				croak("Incompatible set of binning options. Use vpc -h for help.");
			}
			$option_count++;
			unless (defined $self->bin_by_count) {
				croak("Option bin_by_count must be defined when option bin_array is used.");
			}
			if ($self->bin_by_count == 1) {
				#check at least two values and all counts larger than 0
				unless (scalar(@{$self->bin_array}) > 1) {
					croak("Must define at least two counts in bin_array when binning by count.");
				}
				foreach my $c ($self->bin_array) {
					if ($c < 1) {
						croak("Number of observations in each bin must be at least 1.");
					}
				}
			} else {
				#check at least one value and sorted in ascending order and increasing
				unless (scalar(@{$self->bin_array}) > 0) {
					croak("Must define at least one boundary in bin_array when binning by width.");
				}
				for (my $i = 1; $i < scalar(@{$self->bin_array}); $i++) {
					unless ($self->bin_array->[$i] > $self->bin_array->[$i - 1]) {
						croak("List of bin boundaries must be sorted and increasing.");
					}
				}
			}
			
		}
		
		if (defined $self->single_bin_size) {
			if ($option_count > 0) {
				croak("Incompatible set of binning options. Use vpc -h for help.");
			}
			$option_count++;
			unless ($self->single_bin_size > 0) {
				croak("Option single_bin_size must be larger than 0.");
			}
			unless (defined $self->bin_by_count) {
				croak("Option bin_by_count must be defined when ".
					  "option single_bin_size is used.");
			}
			if (defined $self->overlap_percent) {
				unless ($self->overlap_percent > 0) {
					croak("Option overlap must be larger than 0\%.");
				}
				unless ($self->overlap_percent < 100) {
					croak("Option overlap must be smaller than 100\%.");
				}
			}
			#else handle by translating to no_of_bins-alternative after know width/no of observations
		} else {
			if (defined $self->overlap_percent) {
				croak("Option single_bin_size must be defined when ".
					  "option overlap is used.");
			}
		}

		if (defined $self->bin_by_count) {
			unless ($option_count > 0) {
				croak("Option bin_by_count is forbidden without ".
					  "additional binning options.");
			}
			unless ($self->bin_by_count eq '0' || $self->bin_by_count eq '1') {
				croak("Option bin_by_count must be either 1 or 0.");
			}
		}

	} 
	#endof check vpc 

	unless (defined $self->orig_table or defined $self->sim_model or $self->flip_comments) {
		#no more checks when tablefiles given as input

		if ($self->keep_estimation && $self->noprediction) {
			croak('Using both -keep_estimation and -noprediction is not allowed.');
		}

		my $require_icall = 0;
		
		foreach my $opt_name ('LIKELIHOOD', '-2LOGLIKELIHOOD', '-2LLIKELIHOOD') {
			if ( $self->models->[0]->is_option_set( record => 'estimation',
													name => $opt_name, fuzzy_match => 1 )) {
				$require_icall = 1;
				if ($self->categorized) {
					ui -> print (category => 'all', 
								 message=> "****** Warning:\nOption $opt_name found in \$ESTIMATION.\n".
								 "Please note that the model file needs to be adapted for ".
								 "simulation of count/categorical data and a \$SIMULATION record needs to be\n".
								 "present in the model file.\n\n");
				} else {
					ui -> print (category => 'all', 
								 message => "****** Warning:\nWhen \$ESTIMATION contains $opt_name ".
								 "the option -levels is generally recommended.\n") 
						if ($self->is_vpc and (not defined $self->tte));	  
				}
				unless ($self->noprediction) {
					ui -> print (category => 'all', 
								 message => "****** Warning:\nWhen \$ESTIMATION contains $opt_name ".
								 "the option -noprediction is generally recommended.\n")	  
						if ($self->is_vpc and (not defined $self->tte));
				}
			}
		}  
		my $opt_name = 'LAPLACIAN';
		if ( $self->models->[0]->is_option_set( record => 'estimation', name => $opt_name, fuzzy_match => 1 )) {
			ui -> print (category => 'all', 
						 message => "****** Warning:\nOption $opt_name found in \$ESTIMATION \n".
						 "\$ESTIMATION will be removed for simulations, this may produce erroneous results.\n".
						 "Use option -keep_estimation to avoid deletion of \$ESTIMATION.\n")
				unless ($self->keep_estimation);
			#possible change, see to_do 61. Is current handling of NONP wrong?????
		}

		##look for F_FLAG in pred, error
		my @flag_array;
		push (@flag_array, @{$self->models->[0]->problems->[0]->errors->[0]->code})
			if (defined ($self->models->[0]->problems->[0]->errors));
		
		push (@flag_array, @{$self->models->[0]->problems->[0]->preds->[0]->code})	
			if (defined $self->models->[0]->problems->[0]->preds);
		
		foreach my $line (@flag_array) {
			next if ($line =~ /^\s*;/);
			if ($line =~ /F_FLAG/) {
				$require_icall = 1;
				ui -> print (category => 'all', 
							 message=> "****** Warning:\nThe option -levels is recommended when \$ERROR contains F_FLAG.\n") if (!$self->categorized && $self->is_vpc);
				last;
			}
		}

		#done looking for fflag

		my @needed_variables = ();
		push (@needed_variables, 'STRT') if ($self->stratify_on eq 'STRT');

		push (@needed_variables, 'IPRED') if (($self->dv =~ /^(CWRES)$/) and ($PsN::nm_major_version < 7));
		
		if (scalar(@needed_variables) > 0) {
			my @line_array;
			push (@line_array, @{$self->models->[0]->problems->[0]->errors->[0]->code})
				if (defined ($self->models->[0]->problems->[0]->errors));
			
			push (@line_array, @{$self->models->[0]->problems->[0]->preds->[0]->code})	
				if (defined $self->models->[0]->problems->[0]->preds);
			
			push (@line_array,@{$self->models->[0]->problems->[0]->pks->[0]->code})
				if (defined $self->models->[0]->problems->[0]->pks);
			
			my $missing_variables = '';
			foreach my $var (@needed_variables) {
				my $found = 0;
				foreach my $line (@line_array) {
					#variable must be left hand side, i.e. first set of non-blank characters
					#bug for STRT!!!
					next if ( $line =~ /^\s*;/); #skip comments
					if ($line =~ /^ *$var[ =]/) {
						$found = 1;
						last;
					} 
				}
				$missing_variables = "$missing_variables $var" unless $found;
			}
			if (length($missing_variables) > 0) {
				ui -> print (category=>'all', 
							 message=> "****** Warning:\nThe selected input options require the user to define".
							 "$missing_variables in the modelfile,\nbut $missing_variables ".
							 "could not be found in \$ERROR, \$PRED or \$PK. ");
			}
		}
#check for ICALL .EQ. 4

		my @line_array;
		push (@line_array, @{$self->models->[0]->problems->[0]->errors->[0]->code})
			if (defined ($self->models->[0]->problems->[0]->errors));
		
		push (@line_array, @{$self->models->[0]->problems->[0]->preds->[0]->code})	
			if (defined $self->models->[0]->problems->[0]->preds);
		
		push (@line_array, @{$self->models->[0]->problems->[0]->pks->[0]->code})
			if (defined $self->models->[0]->problems->[0]->pks);
		
		my $found = 0;
		foreach my $line (@line_array) {
			next if ( $line =~ /^\s*;/); #skip comments
			if ($line =~ /ICALL\s?\.EQ\.\s?4/) {
				$found = 1;
				last;
			} 
		}

		if ($require_icall) {
			croak("There must be an ICALL.EQ.4 block in the modelfile ".
				  "when \$ESTIMATION contains -2LOGLIKELIHOOD/LIKELIHOOD ".
				  "or \$ERROR/\$PRED contains F_FLAG. ".
				  "See userguide section Handling BQL data.") unless $found;
		} else {
			if ($found) {
				ui -> print (category => 'all', 
							 message => "******\nWarning: String ICALL.EQ.4 found in modelfile. Simulation statements\n".
							 "will influence NPC/VPC simulations and might produce unwanted results.\n");
			}
		}
		##endof ICALL check

		if ( scalar (@{$self->models->[0]->record(record_name => 'simulation' )}) > 1 ) {
			croak("Tool does not allow more than one \$SIMULATION".
				  " record in the modelfile.");
		}

		if ($self->keep_estimation) {
			unless ( scalar (@{$self->models->[0]->record( record_name => 'estimation' )}) > 0 ) {
				croak('Tool requires an $ESTIMATION record in '.
					  'the modelfile when option -keep_estimation is set.');
			}
		}
		
	} #end of checks for input model when user has not given separate simulation model  
	
	unless (defined $self->orig_table) {
		my $np_record = $self->models->[0]->record(record_name => 'nonparametric');

		if ( scalar (@{$self->models->[0]->record(record_name => 'table')}) > 0 ) {
			carp('Tool will delete existing $TABLE records in the modelfile.');
		}
		if (defined $self->msfo_file) {
			if (defined $self->lst_file) {
				croak('Tool does not allow using both -lst_file '.
					  'and -msfo_file options simultaneously.');
			}
		} else {
			if( scalar(@{$np_record}) > 0 ) {
				croak("Tool requires an msfo file when there is a \$NONPARAMETRIC record".
					  ' in the modelfile.');
			}
		}
		
		#Synonym forbidden for MDV,ID
		
		foreach my $variable ('MDV','ID') {
			my $input_record = $self->models->[0]->record(record_name => 'input');
			if( scalar(@{$input_record}) > 0 ) { #always true
				foreach my $line ( @{$input_record -> [0]} ) {
					next if ( $line =~ /^\s*;/); #skip comments
					if ( $line =~ /([\w]+)=(MDV|ID)[^\w]/ ) {
						if (($variable eq $2) && !($1 =~ /(SKIP|DROP)/ )) {
							croak("It is forbidden to use a synonym ".
								  "for $variable in the \$INPUT record.");
						}
						last;
					}
					if ( $line =~ /(MDV|ID)=([\w]+)[^\w]/ ) {
						if (($variable eq $1) && !($2 =~ /(SKIP|DROP)/ )) {
							croak("It is forbidden to use a synonym ".
								  "for $variable in the \$INPUT record.");
						}
						last;
					}
				}
			}

		}
		
		#check if synonyms used for DV If so, warn
		my @reserved_labels = ('ID','L1','L2','MDV','RAW_','MRG_','RPT_','TIME','DATE');
		push (@reserved_labels, ('DAT1','DAT2','DAT3','EVID','AMT','RATE','SS','II','ADDL'));
		push (@reserved_labels, ('CMT','PCMT','CALL','CONT','SKIP','DROP'));
		
		my $check_it = 0;
		if ($self->dv eq 'DV') {
			$check_it = 1;
		} else {
			foreach my $lab (@reserved_labels) {
				if ($self->dv eq $lab) {
					$check_it = 1;
					last;
				}
			}
		}
		my $found_synonym = 0;
		if ($self->models->[0]->is_option_set(record => 'input', name => $self->dv)) {
			my $value = $self->models->[0]->get_option_value(record_name => 'input', option_name => $self->dv);
			if (defined $value) {
				unless ($value =~ /(SKIP|DROP)/ ) {
					$found_synonym = 1;
				}
			} 
		} else {
			my $input_record = $self->models->[0]->record(record_name => 'input');
			if( scalar(@{$input_record}) > 0 ) { #always true
				foreach my $line ( @{$input_record->[0]} ) {
					next if ( $line =~ /^\s*;/); #skip comments
					if ( $line =~ /([\w]+)=([\w]+)[^\w]/ ) {
						unless ($1 =~ /(SKIP|DROP)/ ) { #synonym
							$found_synonym = 1 if ($2 eq $self->dv);
							last;
						}
					}
				}
			}
		}
		ui -> print (category => 'all', 
					 message=> "****** Warning:\n".
					 "It seems like a synonym is used for the dependent variable ".
					 $self->dv . ". PsN will look for a column with header ".
					 $self->dv . " in the table output, ".
					 "and if it is not found, for example because NONMEM prints a synonym for ".
					 $self->dv . " as the header instead, then there will be no output from PsN.") 
			if ($found_synonym);
		ui -> print (category=>'all', 
					 message=>"Consider setting option -dv on the commandline.") 
			if ($found_synonym and ($self->dv eq 'DV'));


		my $command_file = $self->directory . "/original_command.txt";

		if ( -e $command_file ) {
			#check if calls are compatible
			open( CMD, $command_file ) or 
				croak("Could not open ".$command_file);
			my $row = <CMD>;
			close(CMD);
			my $str = $self->models->[0]->filename();
			unless ($row =~ /($str)/) {
				ui -> print (category => 'all', 
							 message => "****** Warning:\nName of modelfile $str on command line does not match name of ".
							 "modelfile \nin restart directory ". $self->directory . "\n");
			}
			if (defined $self->lst_file) {
				unless ($row =~ /($self->lst_file)/) {
					ui -> print (category=>'all', 
								 message=> "****** Warning:\nlst-file on command line was not used in call\n$row in".
								 " restart directory " . $self->directory . "\n");
				}
			} elsif ($row =~ / \-lst/) {
				ui -> print (category => 'all', 
							 message=> "****** Warning:\nNo lst-file was specified on command line, but an lst-file was used ".
							 "in call\n$row in restart directory " . $self->directory . "\n");
			}
			if (defined $self->msfo_file) {
				unless ($row =~ /($self->msfo_file)/) {
					ui -> print (category=>'all', 
								 message=> "****** Warning:\nmsfo-file on command line was not used in call\n$row".
								 "in restart directory " . $self->directory . "\n");
				}
			} elsif ($row =~ / \-msf/){
				ui -> print (category => 'all', 
							 message => "****** Warning:\nNo msfo-file was specified on command line, but an msfo-file was used ".
							 "in call\n$row in restart directory " . $self->directory . "\n");
			}
			if (defined $self->samples) {
				if ($row =~ / \-samp[^=]*=(\d+)/ ) {
					unless ($1 == $self->samples) {
						croak("-samples on command line is different from call\n$row".
							  "in restart directory ".$self->directory);
					}
				}
			}
		}
	}
}

sub modelfit_setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model_number => { isa => 'Int', optional => 1 }
		);
	my $model_number = $parm{'model_number'};

	#m1 directory exists here

	my $type = 'npc';
	if ($self->is_vpc) {
		$type = 'vpc';
		$self->logfile(['vpc.log']);
		$self->results_file('vpc_results.csv');

		if (defined $self->lower_bound) {
			if ($self->lower_bound =~ /^[A-Z]/) {
				#must start with a letter if variable name
				$self->bound_variable($self->lower_bound);
			}
		}
	} elsif ($self->nca) {
		$type = 'nca';
	}

	$self->npc_alert_written(1); #initiate to 1 so that won't be written.

	if ((defined $self->lloq) || (defined $self->uloq)) {
		$self->detection_censored(1);
	}else{
		$self->detection_censored(0);
	}

	return if (defined $self->orig_table);

	my $user_sim_model = 0;
	if (-e 'm1/'.$type.'_original.mod'){
		unlink('m1/'.$type.'_original.mod');
	}
	my $model_orig = $self->models->[0]->copy(filename => $type . '_original.mod', 
											  directory => 'm1',
											  copy_output => 0,
											  write_copy => 0,
											  copy_datafile => 0);
	my $model_simulation;
	my $model_simulation_output;
	if (defined $self->sim_model()) {
		$user_sim_model = 1;
		my $temp_model = model -> new ( 
			%{common_options::restore_options(@common_options::model_options)},
			filename                    => $self->sim_model(),
			ignore_missing_output_files => 1,
			cwres                       => (($self->dv eq 'CWRES') and ($PsN::nm_major_version < 7))  );


		if (defined $temp_model->outputs() and 
			defined $temp_model->outputs()->[0] and $temp_model->outputs()->[0]->have_output()) {
			$model_simulation_output = $temp_model->outputs()->[0];
		}
		if (-e 'm1/'.$type.'_user_sim_model.mod'){
			unlink('m1/'.$type.'_user_sim_model.mod');
		}
		$model_simulation = $temp_model->copy(filename=>$type.'_user_sim_model.mod',
											  directory => 'm1',
											  copy_datafile => 0,
											  write_copy => 0,
											  copy_output => 0);

	} elsif ($self->flip_comments()) {
		$user_sim_model = 1;

		my $simname = "m1/$type"."_user_sim_model.mod";
		if (-e $simname){
			unlink($simname);
		}
		open(MOD, $self->models->[0]->full_name()) || 
			die("Couldn't open " . $self->models->[0]->full_name()." : $!");
		open(SIM, ">$simname") || die("Couldn't open $simname : $!");
		my $sim_tag = 0;
		while(<MOD>) {
			my $remove_line = 0;
			
			# find Sim_end
			if (/^\s*\;+\s*[Ss]im\_end/) {
				$sim_tag = 0;
				$remove_line = 1;
			}
			# find Sim_start
			if (/^\s*\;+\s*[Ss]im\_start/) {
				$sim_tag = 1;
				$remove_line=1;
			}
			if($remove_line==1) {
				next;
			} elsif($sim_tag==1) {
				if(/^\s*\;+/) {
					s/\;//;
				} else {
					$_ = ';'.$_
				}
			}

			#change data path unless absolute. do this after flip, might change dataset even
			if (s/^\s*\$DAT[A]?\s*//) {
				#remove $DATA, put it back afterwards
				#now first string is filename
				my $head = '$DATA ';
				#Check OS
				if ($Config{osname} eq 'MSWin32' or $Config{osname} eq 'MSWin64') {
					unless (/^[A-Za-z]:/) {
						#if start with volume, absolute path, do not do this
						$head .= '..\\..\\';
					}
				} else {
					unless (/^\//) {
						#if start with /, absolute path, do not do this
						$head .= '../../';
					}
				}
				print SIM $head.$_;
			} else {
				print SIM $_;
			}

		}
		
		close(SIM);
		close(MOD);

		$model_simulation = model -> new ( 
			%{common_options::restore_options(@common_options::model_options)},
			filename                    => $simname,
			ignore_missing_output_files => 1,
			cwres                       => (($self->dv eq 'CWRES') and ($PsN::nm_major_version < 7))  );
	}

	if (defined $model_simulation) {
		unless ($model_simulation->copy_data_setting_ok(copy_data => $self->copy_data)){
			croak("Cannot set -no-copy_data, absolute data file path in simulation model is too long.");
		} 
		unless ($self->copy_data){
			$model_simulation->relative_data_path(0);
		}


		my $tnpri = 0;
		if (scalar (@{$model_simulation->problems}) > 2 ) {
			croak('Cannot have more than two $PROB in the simulation model.');
		} elsif (scalar (@{$model_simulation->problems}) == 2 ) {
			if ((defined $model_simulation->problems->[0]->priors()) and 
				scalar(@{$model_simulation->problems->[0]->priors()})>0 ){
				my $tnpri = 0;
				foreach my $rec (@{$model_simulation->problems->[0]->priors()}){
					unless ((defined $rec) &&( defined $rec -> options )) {
						carp("No options for rec \$PRIOR" );
					}
					foreach my $option ( @{$rec -> options} ) {
						if ((defined $option) and 
							(($option->name eq 'TNPRI') || (index('TNPRI',$option ->name ) == 0))){
							$tnpri = 1;
						}
					}
				}
				
				$self->have_tnpri(1) if ($tnpri);
			}
			if ($self->have_tnpri()) {
				if (defined $self->rawres_input) {
					croak('Cannot use option rawres_input if the simulation model has $PRIOR.');
				}
				unless( defined $model_simulation->extra_files ) {
					croak('When using $PRIOR TNPRI you must set option -extra_files to '.
						  'the msf-file, otherwise the msf-file will not be copied to the NONMEM '.
						  'run directory.');
				}
				
			} else {
				croak('The simulation model must contain exactly one problem, unless'.
					  ' first $PROB has $PRIOR TNPRI');
			}
		}
		if ((not $self->have_tnpri()) and
			(defined $model_simulation->problems->[0]->priors()) and 
			scalar(@{$model_simulation->problems->[0]->priors()}) > 0 ) {
			$self->have_nwpri(1);
			if (defined $self->rawres_input) {
				croak('Cannot use option rawres_input if the simulation model has $PRIOR.');
			}
		}

		$self->simprobnum(2) if ($self->have_tnpri());
	}

	my $vpctab = $self->directory . "m1/vpctab";

	#force rerun of original model if stratifying on something new by 
	#removing lst file of original model

	$self->run_the_original(1);
	$self->run_the_sim(1);
	if ( -d $self->directory . "m1" ) {
		my $oldlst = $self->directory . "m1/" . $type . "_original.lst";
		my $file = $self->directory . "m1/" . $type . "_original.npctab.dta";
		my $file2 = $self->directory . "m1/" . $type . "_simulation.1.npctab.dta";
		if (-e $file) {
			#this is a rerun.
			if (-e $file2) {
				$self->run_the_sim(0);
			} 
			print "Trying to reuse existing $type table output.\n";
			open( TAB,$file  ) or croak("Could not open ".$file);
			my $row = <TAB>;
			$row = <TAB>; #get second line
			my $remove = 0;
			close(TAB);
			#check have stratify
			if (defined $self->stratify_on) {
				my $str = $self->stratify_on;
				unless ($row =~ /($str)/) {
					print "Did not find $str in\n$file,\nwill rerun original model to create new table.\n";
					$remove = 1;
				}
			}
			#check have censor
			if (defined $self->censor()) {
				my $str = $self->censor();
				unless ($row =~ /($str)/) {
					print "Did not find $str in\n$file,\nwill rerun original model to create new table.\n";
					$remove = 1;
				}
			}
			if ($self->is_vpc) {
				my $str = $self->idv;
				unless ($row =~ /($str)/) {
					print "Did not find $str in\n$file,\nwill rerun original model to create new table.\n";
					$remove = 1;
				}
			}
			my $str = $self->dv;
			unless ($row =~ /($str)/) {
				#check that at least present in simfile
				my $row2 = '';
				if (-e $file2) {
					open( TAB, $file2 ) or croak("Could not open " . $file2);
					$row2 = <TAB>;
					$row2 = <TAB>; #get second line
					close(TAB);
				}
				unless ($row2 =~ /($str)/) {
					croak("The dependent variable $str was not found in ".
						  "the existing table\n$file2\nIt is necessary to start a new $type from scratch.");
				}
				print "Did not find $str in $file, will rerun original model to create new table.\n";
				$remove = 1;
			}
			if (defined $self->censor()) {
				my $str = $self->censor();
				unless ($row =~ /($str)/) {
					#check that at least present in simfile
					my $row2 = '';
					if (-e $file2) {
						open( TAB,$file2 ) or croak("Could not open " . $file2);
						$row2 = <TAB>;
						$row2 = <TAB>; #get second line
						close(TAB);
					}
					unless ($row2 =~ /($str)/){
						croak("The censor variable $str was not found in ".
							  "the existing table\n$file2\nIt is necessary to start a new $type from scratch.");
					}
					print "Did not find $str in $file, will rerun original model to create new table.\n";
					$remove = 1;
				}
			}
			if ($remove) {
				unlink $oldlst;
				$self->run_the_original(1);
			} else {
				if ($self->run_the_sim) {
					print "Did not find $file2,\nwill rerun simulations.\n";
				} else {
					print "Creating output $type"."_results.csv based on existing tablefiles in\n".
						$self->directory . "m1/\n";
				}
				$self->run_the_original(0);
			}
		}
	}

	#get rid of $SCAT records, if any
	$model_orig -> remove_records(type => 'scatter');

	#logic for use of MDV column as basis for finding which rows are observations
	#request MDV in $TABLE if no $PRED record or if there is a $PRED record and 
	#MDV is in the input
	#if there is a $PRED but no MDV in input then all rows will be observations

	my $MDV = '';
	if (defined $self->tte()) {
		if ($self->models->[0]->is_option_set(record => 'input', name => 'EVID')) {
			$MDV = 'EVID';
		} else {
			ui->print(category => 'all',
					  message => "\nWarning: No EVID column found in \$INPUT. To use kaplan.plot in Xpose\n".
					  "an Xpose table containing EVID is needed for the original data, but PsN cannot ".
					  "create this table.\n");
			$MDV = 'MDV';
		}
	} elsif (scalar @{$model_orig->record( record_name => 'pred' )} < 1) {
		$MDV = 'MDV';
	} else {
		if ($model_orig->is_option_set(record => 'input', name => 'MDV')) {
			my $mdv_val = $model_orig -> get_option_value(record_name => 'input',
														  option_name => 'MDV',
														  problem_index => 0);
			#have already checked no synonym. If value defined then SKIP/DROP ->don't include
			unless (defined $mdv_val) {
				$MDV='MDV'; #MDV in input, no synynom
			}
		}
		#have already checked no synonym. If value defined then SKIP/DROP ->don't include
	}

	######## fix $TABLE record: remove any existing. create a new correct one.

	#store $TABLE if FILE=cwtab<>.deriv, i.e. dv is CWRES, add it back to models later
	my @extra_table_record;
	if (($self->dv eq 'CWRES') and ($PsN::nm_major_version < 7)){
		my $table_record = $model_orig -> record(record_name => 'table',
												 problem_number => $self->origprobnum());
		
		my $rec_count = scalar(@{$table_record});
		if( $rec_count > 0 ){
			for (my $i=0; $i<$rec_count; $i++){
				@extra_table_record=();
				my $keep=0;
				foreach my $table_line ( @{$table_record -> [$i]} ){
					push(@extra_table_record,$table_line);
					if ( $table_line =~ /(cwtab[0-9]*)\.deriv/){
						$self->dv_table_name($1); 
						$keep=1;
					}
				}
				last if ($keep==1);
			}
		}
	}

	$model_orig -> remove_records(type => 'table');
	$model_simulation -> remove_records(type => 'table') if (defined $model_simulation);

	my @rec_strings = ('ID',$MDV);
	if (defined $self->stratify_on) {
		push (@rec_strings, $self->stratify_on) unless ($self->stratify_on eq 'PRED');
	}
	push (@rec_strings,@{$self->extra_table_parameters()}) if (defined $self->extra_table_parameters());

	if (defined $self->censor()){
		push (@rec_strings,$self->censor());
	}
	if ($self->is_vpc or $self->nca) {
		push (@rec_strings, $self->idv) unless ($self->idv eq 'PRED');
		push (@rec_strings, $self->bound_variable) if (defined $self->bound_variable);
	}

	if ($PsN::nm_major_version >=  7){
		#in NM7 CWRES can be requested in $TABLE
		push (@rec_strings, $self->dv) unless ($self->dv =~ /^(RES|WRES)$/ ); 
	}else {
		push (@rec_strings, $self->dv) unless ($self->dv =~ /^(RES|WRES|CWRES)$/ ); #PRED forbidden dv
	}


	my @tte_strings;
	push(@tte_strings, @rec_strings) if (defined $self->tte());
	if (defined $model_simulation){
		push (@tte_strings, $self->tte()) 
			if (defined $self->tte() and ($self->tte() ne $self->dv));
	} else {
		#if we do not have model_simulation then it is ok to request 
		#tte variable in $TAB of both sim and orig, since
		#must be available in both when models are so similar
		push (@rec_strings, $self->tte()) 
			if (defined $self->tte() and ($self->tte() ne $self->dv));
	}
	
	# Remove duplicate columns
	my @rec_strings2;
	my %column_seen;
	foreach my $column (@rec_strings) {
		if (not $column_seen{$column}) {
			push @rec_strings2, $column;
			$column_seen{$column} = 1;
		}
	}
	@rec_strings = @rec_strings2;

	push (@rec_strings,('ONEHEADER','NOPRINT'));
	push (@tte_strings,('ONEHEADER','NOPRINT'));

	push (@rec_strings,('FILE=npctab.dta'));
	push (@tte_strings,('FILE=npctab.dta'));
	$model_orig -> set_records(type => 'table',
							   record_strings => \@rec_strings,
							   problem_numbers => [($self->origprobnum())]);

	$model_orig -> add_records(type => 'table',
							   record_strings => \@extra_table_record,
							   problem_numbers => [($self->origprobnum())]) 
		if (scalar(@extra_table_record)>0);

	if (defined $model_simulation){
		if (defined $self->tte()){
			$model_simulation -> set_records(type => 'table',
											 record_strings => \@tte_strings,
											 problem_numbers => [($self->simprobnum())]) ;
		}else{
			$model_simulation -> set_records(type => 'table',
											 record_strings => \@rec_strings,
											 problem_numbers => [($self->simprobnum())]) ;
		}
		$model_simulation -> add_records(type => 'table',
										 record_strings => \@extra_table_record,
										 problem_numbers => [($self->simprobnum())]) 
			if (scalar(@extra_table_record)>0);
	}

	#check if synonyms used for DV/strat/bin. If so, replace reserved variable with synonym.
	#why not DV here? because set on commandline?
	my @reserved_labels=('ID','L1','L2','MDV','RAW_','MRG_','RPT_','TIME','DATE');
	push (@reserved_labels,('DAT1','DAT2','DAT3','EVID','AMT','RATE','SS','II','ADDL'));
	push (@reserved_labels,('CMT','PCMT','CALL','CONT','SKIP','DROP'));

	my @check_var;

	#collect variables to check
	my @cvar;
	push (@cvar, $self->stratify_on) if ((defined $self->stratify_on)
										 && !($self->stratify_on =~ /(STRT|PRED)/));
	push (@cvar,$self->idv) if ((defined $self->idv) 
								&& ($self->idv ne 'PRED'));
	#check if strat/bin reserved
	foreach my $var (@cvar){
		foreach my $lab (@reserved_labels){
			if ($var eq $lab){
				push (@check_var,$var) ;
				last;
			}
		}
	}

	foreach my $reserved_name (@check_var){
		my $synonym=$reserved_name;
		if ($model_orig-> is_option_set(record=>'input',name=>$reserved_name)){
			my $value = $model_orig -> get_option_value(record_name => 'input',
														option_name => $reserved_name);
			if (defined $value){
				unless ($value =~ /(SKIP|DROP)/ ){
					$synonym=$value;
				}
			} 
		} else {
			my $input_record = $model_orig -> record(record_name => 'input' ); #default prob is 1
			if( scalar(@{$input_record}) > 0 ){ #always true
				foreach my $line ( @{$input_record -> [0]} ){
					if ( $line =~ /([\w]+)=([\w]+)[^\w]/ ){
						unless ($1 =~ /(SKIP|DROP)/ ){ #synonym
							$synonym=$1 if ($2 eq $reserved_name);
						}
						last;
					}
				}
			}
		}
		if ($synonym ne $reserved_name){
			#found synonym
			if ('DV' eq $reserved_name){
				$self->dv($synonym);
			} elsif ($reserved_name eq $self->idv){
				$self->idv($synonym);
			} elsif ($reserved_name eq $self->stratify_on){
				$self->stratify_on($synonym);
			}
		}
	}

##### fix $ESTIMATION for original model
	
	
	#if multiple $EST with NM7, the following will remove all but the last

	#default is set 0  all probs
	print "Run may take extremely long time. Consider interrupting and changing last \$EST.\n" 
		unless ($model_orig -> set_maxeval_zero(print_warning => 1,
												last_est_complete => $self->last_est_complete,
												niter_eonly => $self->niter_eonly,
												need_ofv => 0));
	

	if (defined $model_simulation){
		$model_simulation -> set_maxeval_zero(print_warning => 1,
											  last_est_complete => $self->last_est_complete,
											  niter_eonly => $self->niter_eonly,
											  need_ofv => 0);
	}

	$model_orig -> remove_option(record_name => 'estimation',
								 option_name => 'MSFO',
								 fuzzy_match => 1);
	$model_orig -> remove_option(record_name => 'nonparametric',
								 option_name => 'MSFO',
								 fuzzy_match => 1);

	if ($self->keep_estimation){
		$model_orig -> set_option(record_name => 'estimation',
								  option_name => 'POSTHOC',
								  fuzzy_match => 1,
								  problem_numbers => [($self->origprobnum())]);
	}


##### remove $COVARIANCE
	$model_orig -> remove_records(type => 'covariance');
	$model_simulation -> remove_records(type => 'covariance') if (defined $model_simulation);

###update initial estimates/input data, if given###


	if (defined $self->lst_file){
		#create output object to check that can be parsed correctly, and to 
		#extract data for error checking
		my $outputObject= output -> new(filename => '../' . $self->lst_file);
		unless ($outputObject->parsed_successfully()){
			croak("lst file ${\$self->lst_file} could not be parsed.");
		}
		#update initial values in model
		# sim_model defined then only update this one
		$model_orig -> update_inits ( from_output => $outputObject,
									  problem_number => $self->origprobnum())
			unless (defined $self->sim_model());
		if (defined $model_simulation){
			$model_simulation -> update_inits ( from_output => $outputObject,
												problem_number => $self->simprobnum());
		}

	} elsif (defined $self->msfo_file) {
		#add msfi record, and remove $THETA, $OMEGA, $SIGMA
		#must make sure there is no path here first
		my ($dirt, $fname) =
			OSspecific::absolute_path('', $self->msfo_file );
		unless (defined $self->sim_model()) {
			$model_orig -> set_records(type => 'msfi',
									   record_strings => [$fname],
									   problem_numbers => [1]);
			$model_orig -> remove_records(type => 'theta',problem_numbers => [1]);
			$model_orig -> remove_records(type => 'omega',problem_numbers => [1]);
			$model_orig -> remove_records(type => 'sigma',problem_numbers => [1]);
		}
		if (defined $model_simulation){
			$model_simulation -> set_records(type => 'msfi',
											 record_strings => [$fname],
											 problem_numbers => [1]);
			$model_simulation -> remove_records(type => 'theta',problem_numbers => [1]);
			$model_simulation -> remove_records(type => 'omega',problem_numbers => [1]);
			$model_simulation -> remove_records(type => 'sigma',problem_numbers => [1]);
		}
	}elsif ( (not defined $self->sim_model()) and
			 defined $self->models->[0]->outputs() and 
			 defined $self->models->[0]->outputs()->[0] and
			 $self->models->[0]->outputs->[0]->have_output() and
			 $self->models->[0]->outputs->[0]->get_single_value(attribute => 'estimation_step_initiated',
																problem_index => ($self->origprobnum()-1))) {
		$model_orig -> update_inits ( from_output => $self->models->[0]->outputs->[0],
									  problem_number => $self->origprobnum());
		if (defined $model_simulation){
			$model_simulation -> update_inits ( from_output => $self->models->[0]->outputs->[0],
												problem_number => $self->simprobnum());
		}
	}elsif ( defined $self->sim_model() and
			 defined $model_simulation_output){
		$model_simulation -> update_inits ( from_output => $model_simulation_output,
											problem_number => $self->simprobnum());
		
	}

####end update initial estimates############


#####Copy to simulation model

	#if user has $TABLE with certain FILE names in model, then shrinkage will be turned on
	# automatically. Make sure this does not happen.
	$model_orig->shrinkage_stats(enabled => 0);
	$model_simulation->shrinkage_stats(enabled => 0) if (defined $model_simulation);

	my $rem = $self -> samples % $self -> n_simulation_models();
	my $base = ($self -> samples - $rem)/$self -> n_simulation_models();
	my @model_sims = ();
	my $ignore_char=undef;
	my $nopred_is_set=0;
	unless (defined $model_simulation){
		$nopred_is_set = $model_orig -> is_option_set( record => 'simulation',
													   name => 'noprediction',
													   fuzzy_match => 1,
													   problem_number => $self->origprobnum());
		
	}
	if ($self->keep_estimation && $nopred_is_set){
		ui->print(category=>'all',
				  message=>"\nWarning: NOPRED found in \$SIMULATION and option -keep_estimation is set.\n".
				  "This is likely to cause a NONMEM error. Consider either removing NOPRED or not using ".
				  "option -keep_estimation\n");
	}

	my $sampled_params_arr;
	if (defined $self->rawres_input()){
		if (defined $model_simulation){
			$sampled_params_arr = 
				$model_simulation -> get_rawres_params(filename => $self->rawres_input(),
													   offset => $self->offset_rawres());
		}else{
			$sampled_params_arr = 
				$model_orig -> get_rawres_params(filename => $self->rawres_input(),
												 offset => $self->offset_rawres());
		}
		if (defined $sampled_params_arr){
			unless (scalar(@{$sampled_params_arr}) >= ($self->samples())){
				croak("Too few sets (lines) of parameter values in\n".
					  $self->rawres_input()."\nNeed at least ".
					  ($self->samples()+$self->offset_rawres())."\n");
			}
		}else{
			croak("get_rawres_params returned undef");
		}
		
	}

	for( my $i = 0; $i < $self -> n_simulation_models(); $i++ ) {
		my $samples = $base;
		if( $rem > 0 ) {
			$samples += 1;
			$rem--;
		}
		if (-e 'm1/'.$type.'_simulation.'.($i+1).'.mod'){
			unlink('m1/'.$type.'_simulation.'.($i+1).'.mod');
		}
		if (defined $model_simulation){
			push( @model_sims, $model_simulation -> copy(filename  => $type.'_simulation.'.($i+1).'.mod',
														 output_same_directory => 1,
														 copy_datafile => 0,
														 write_copy => 0,
														 copy_output => 0,
														 directory => 'm1'));
		}else{
			push( @model_sims, $model_orig -> copy(filename  => $type.'_simulation.'.($i+1).'.mod',
												   copy_datafile => 0,
												   write_copy => 0,
												   copy_output => 0,
												   output_same_directory => 1,
												   directory => 'm1'));
			unless ($self->keep_estimation) {
				$model_sims[$i] -> remove_records(type => 'estimation');
			}

			if (defined $self->tte()){
				if ($i == 0){
					#Keep IGNORE=char but remove all other IGNORE plus ACCEPT 
					my $sim_ignorelist = $model_sims[$i] -> get_option_value( record_name  => 'data',
																			  problem_index => 0,
																			  option_name  => 'IGNORE',
																			  option_index => 'all');
					if ((defined $sim_ignorelist) and scalar (@{$sim_ignorelist})>0){
						my $printed = 0;
						foreach my $val (@{$sim_ignorelist}){
							if ((defined $val) and length($val)==1){
								$ignore_char = $val;
							}else{
								ui->print(category=>'all',
										  message=>"\nInformation: Since option -tte is set ".
										  "all IGNORE/ACCEPT statements,\n".
										  "except IGNORE=@ or similar, will be removed ".
										  "from \$DATA in the simulation model\n") unless ($printed);
								$printed = 1;
							}
						}
					}
				}
				#all probs
				$model_sims[$i] -> remove_option( record_name  => 'data',
												  option_name  => 'IGNORE',
												  fuzzy_match => 1);
				$model_sims[$i] -> remove_option( record_name  => 'data',
												  option_name  => 'ACCEPT',
												  fuzzy_match => 1);
			}
		}

		if (defined $sampled_params_arr){
			$model_sims[$i] -> update_inits(from_hash => $sampled_params_arr->[$i]); 
		}


###Create simulation record######

		
		#Check if existing simulation record
		my $sim_record = $model_sims[$i]-> record(record_name => 'simulation',
												  problem_number => $self->simprobnum());

		my @simrec_strings;
		if (scalar (@{$sim_record}) > 0){
			#this option will be set below
			foreach my $altopt ('SUBPROBLEMS','SUBPROBS','NSUBPROBLEMS','NSUBPROBS','NSUBS'){
				#NONMEM accepts a heck of a lot of alternatives...
				$model_sims[$i] -> remove_option(record_name => 'simulation',
												 option_name => $altopt,
												 fuzzy_match => 1,
												 problem_numbers => [$self->simprobnum()]);
			}

			#this option will be set below if noprediction
			$model_sims[$i] -> remove_option(record_name => 'simulation',
											 option_name => 'NOPREDICTION',
											 fuzzy_match => 1,
											 problem_numbers => [$self->simprobnum()]) 
				unless ($user_sim_model);
			
			#unless $keep_estimation this option will be set below
			$model_sims[$i] -> remove_option(record_name => 'simulation',
											 option_name => 'ONLYSIMULATION',
											 problem_numbers => [$self->simprobnum()],
											 fuzzy_match => 1) unless ($user_sim_model);
			
			if ($self->have_nwpri() or $self->have_tnpri()){
				my $val= $model_sims[$i] -> get_option_value(record_name => 'simulation',
															 option_name => 'TRUE',
															 problem_index => ($self->simprobnum()-1));
				unless ((defined $val)&& ($val eq 'PRIOR')){
					croak("Error in \$SIMULATION record in modelfile: when using \$PRIOR\n".
						  "the option TRUE=PRIOR must be set.");
					
				}
			}elsif (defined $self->msfo_file){ #always if $nonp, but even if not $nonp
				my $val= $model_sims[$i] -> get_option_value(record_name => 'simulation',
															 option_name => 'TRUE',
															 problem_index => ($self->simprobnum()-1));
				unless ((defined $val)&& ($val eq 'FINAL')){
					croak("Error in \$SIMULATION record in modelfile: when using an msfo-file\n".
						  "the option TRUE=FINAL must be set.");
					
				}
			}

			#find and replace seeds
			my $short_record = $model_sims[$i]-> record(record_name => 'simulation',
														problem_number => $self->simprobnum());
			my $set_seeds=0;
			#Simply look for numbers. Since got rid of NSUBPROBS this is ok.
			foreach my $sim_line ( @{$short_record -> [0]} ){
				my $new_line;
				while ( $sim_line =~ /(\D*)(\d+)(\D.*)/g ){
					my $seed = random_uniform_integer(1,0,2147483560 );
					$new_line .= "$1$seed";
					$sim_line = $3;
					$set_seeds += 1;
				}
				push (@simrec_strings,$new_line.$sim_line);
			}
			if ($set_seeds < 1){
				ui->print(category=>'all',
						  message=>"***\nError in \$SIMULATION record in modelfile: did not find any seed.\n***\n");
			}
			

		} elsif (not defined $model_simulation) {
			#no $sim, never if own sim-file
			my $seed1 = random_uniform_integer(1,0,2147483560 );
			my $seed2 = random_uniform_integer(1,0,2147483560 );
			@simrec_strings = ('('.$seed1.')');
			my $np_record = $model_sims[$i] -> record(record_name => 'nonparametric' ,
													  problem_number => $self-> simprobnum());
			if( scalar(@{$np_record}) > 0 ){
				push (@simrec_strings ,'('.$seed2.' NONPARAMETRIC)');
				$model_sims[$i] -> remove_records(type => 'nonparametric');
			}

			if ($self->have_nwpri() or $self->have_tnpri()){
				push (@simrec_strings ,'TRUE=PRIOR');
			}elsif (defined $self->msfo_file){ #always if $nonp, but even if not $nonp
				push (@simrec_strings ,'TRUE=FINAL');
			}
		}else{
			croak("When -sim_model or -flip_comments is used, \$SIM must be defined ".
				  "by the user, PsN will not add \$SIM.");
			
		}
		unless ($self->keep_estimation or $user_sim_model){
			push (@simrec_strings,('ONLYSIMULATION'));
		}
		if (($self->noprediction or $nopred_is_set )and (not $user_sim_model)){
			push (@simrec_strings,('NOPREDICTION'));
		}
		push (@simrec_strings,('NSUBPROBLEMS='.$samples));
		$model_sims[$i] -> set_records(type => 'simulation',
									   record_strings => \@simrec_strings,
									   problem_numbers => [$self->simprobnum()]);
		$model_sims[$i] -> _write(relative_data_path => $self->copy_data); 
	}
	$model_orig -> remove_records(type => 'simulation');

	if ($self-have_tnpri() or $self->have_nwpri()){
		$model_orig -> remove_option( record_name  => 'prior',
									  problem_numbers => [(1)],
									  option_name  => 'PLEV',
									  fuzzy_match => 1);
	}

	$model_orig -> _write(relative_data_path => $self->copy_data); 



###end simulation record######

#det som koer modellerna aer ett modelfit-objekt. Detta maoste skapas explicit.

	$self->original_model($model_orig);
	$self->simulation_models(\@model_sims);

	$self->stop_motion_call(tool=>'npc/vpc',
							message =>"Preparing to create modelfit object to run models.")
		if ($self->stop_motion());

	my @runmodels=();
	push (@runmodels,$model_orig) if ($self->run_the_original);
	push (@runmodels,@model_sims) if ($self->run_the_sim);

	if ($self->run_the_sim or $self->run_the_original){
		my %subargs = ();
		#Kajsa 2013-10-04 changed top tool to 0, to get better raw_result_file name
		my $modfit = tool::modelfit ->
			new( %{common_options::restore_options(@common_options::tool_options)},
				 models		 => \@runmodels,
				 base_directory      => $self->directory, 
				 directory => undef,
				 nmtran_skip_model => 3,
				 parent_threads        => 1,
				 raw_results           => undef,
				 prepared_models       => undef,
				 top_tool              => 0,
				 copy_data             => $self->copy_data,
				 prepend_model_file_name => 1,
				 %subargs );
		$self->searchdir($modfit->directory);
		$self->tools([]) unless defined $self->tools;
		push( @{$self->tools}, $modfit);
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

	return \&subroutine;
}

sub modelfit_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model_number => { isa => 'Num', optional => 1 }
		);
	my $model_number = $parm{'model_number'};

	# If we ran an nca move data files and return
	if ($self->nca) {
		chdir 'm1';
		foreach my $filename (glob '*.dta') {
			rename $filename, "../$filename";
		}
		return;
	}

	$self->stop_motion_call(tool=>'npc/vpc',message => "done running the models. Do analysis.")
		if ($self->stop_motion);

	if (defined $self->tte) {
		$self->get_tte_data;
		return;
	}

	$self->get_data_matrix; #creates global @data_matrix and global censor_data_matrix

	my $no_sim= (split(/,/,$self->data_matrix->[0])) - 1;
	unless ($no_sim == $self->samples) {
		croak("Number of simulated datasets in matrix file $no_sim is\n".
			  "different from number ${\$self->samples} in input (option -samples).");
	}
	print "$no_sim simulations \n" if ($self->verbose);

	$self->n_simulations($no_sim);
	$self->n_observations(scalar(@{$self->data_matrix}));

	$self->create_stratified_data(); #identical stratification for censoring variable

	if ($self->is_vpc) {
		$self->vpc_analyze();
	} else {
		$self->npc_analyze();
	}

	$self->cleanup;

	#compress m1
	if ($self->compress) {
		chdir( $self->directory."/m1" );
		system('tar cz --remove-files -f nonmem_files.tgz *')
			if ( $Config{osname} ne 'MSWin32' );
		system('compact /c /s /q > NUL')
			if ( $Config{osname} eq 'MSWin32' );
		chdir( $self->directory );
	}
}

sub create_unique_values_hash
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  data_column => { isa => 'ArrayRef[Num]', optional => 0 },
							  reference => { isa => 'Maybe[Num]', optional => 1 }
		);
	my %value_hash;
	my @data_column = defined $parm{'data_column'} ? @{$parm{'data_column'}} : ();
	my $reference = $parm{'reference'};

	#in @data_column and possibly reference
	#if reference is set then make sure that value exists, but keep hash 'sorted'
	#out %value_hash
	my $value_index = 0;

	foreach my $val (sort {$a <=> $b} @data_column){
		if ($value_index == 0){
			$value_hash{$value_index}=$val;
			$value_index++;
			next;
		}
		unless ($val == $value_hash{($value_index-1)}){
			$value_hash{$value_index}=$val;
			$value_index++;
		}
	}
	if (defined $reference){
		my $found_reference = 0;
		foreach my $val (values %value_hash){
			if ($val == $reference){
				$found_reference = 1;
				last;
			}
		}
		unless ($found_reference){
			print "\n\nERROR: The reference stratum value refstrat was set to ".$self->stratify_on()."=".$reference." but no observations were found where ".
				$self->stratify_on()."=".$reference.". Continuing analysis with refstrat undefined.\n\n";
			$self->clear_refstrat;
		}
	}

	return \%value_hash;
}

sub get_bin_boundaries_overlap_count
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  value_hash => { isa => 'HashRef', optional => 1 },
							  overlap_percent => { isa => 'Num', optional => 0 },
							  count => { isa => 'Int', optional => 0 },
							  data_column => { isa => 'ArrayRef[Num]', optional => 0 },
							  data_indices => { isa => 'ArrayRef[Num]', optional => 0 }
		);
	my %value_hash = defined $parm{'value_hash'} ? %{$parm{'value_hash'}} : ();
	my $overlap_percent = $parm{'overlap_percent'};
	my $count = $parm{'count'};
	my @data_column = defined $parm{'data_column'} ? @{$parm{'data_column'}} : ();
	my @data_indices = defined $parm{'data_indices'} ? @{$parm{'data_indices'}} : ();
	my @bin_floors = ();
	my @bin_ceilings = ();

	# input value_hash
	# overlap_percent and scalar count
	# data_column, and data_indices
	# return bin_floors, lower boundary not part of upper bin
	# and bin_ceilings, list of upperlimits, upperlimit is part of lower bin!!!

	my $value_index;
	my $n_values = scalar(keys %value_hash);
	my @obs_count=(0) x $n_values;
	my $check_count=0;

	#Count how many values we have in input for each unique value
	for ($value_index=0; $value_index < $n_values;$value_index++){
		foreach my $index (@data_indices){
			if ($data_column[$index] == $value_hash{$value_index}){
				$obs_count[$value_index] +=1;
				$check_count+=1;
			}
		}
	}

	unless ($check_count == scalar(@data_indices)){
		croak("Did not find all data values in hash.");
	}

	my $local_error=-$count;
	my $overlap=$count*$overlap_percent/100; 
	my $overlap_error;
	push(@bin_floors,($value_hash{0}));
	$value_index = 0;
	my $prev_start_index=0;

	while ($value_index < $n_values ) {
		if ($obs_count[$value_index] == 0){
			$value_index++; #no observations for this value, skip to next	    
		} elsif ($local_error == -$count){
			#bin empty since error equals initial error, must add observations
			$local_error += $obs_count[$value_index];
			$prev_start_index=$value_index; 
			$value_index++;
		}elsif (abs($local_error)>abs($local_error+$obs_count[$value_index])){
			#bin not empty and adding observations will reduce error
			$local_error += $obs_count[$value_index];
			$value_index++;
		} else {
			#bin not empty and best to switch to new bin
			push(@bin_ceilings,$value_hash{$value_index-1}); #last index was previous index
			$local_error=-$count;
			$overlap_error=-$overlap;
			#compute overlap error if starting at $prev_start_index+1
			for (my $k=($value_index-1);$k>$prev_start_index;$k--){
				$overlap_error += $obs_count[$k]; 
			}	    
			$value_index= $prev_start_index+1; #must have nonzero overlap
			#check if increasing value_index, i.e. moving start of next bin, will reduce overlap errror
			while (abs($overlap_error)>abs($overlap_error-$obs_count[$value_index])){
				$overlap_error -= $obs_count[$value_index];
				$value_index++;
			}
			push(@bin_floors,$value_hash{$value_index-1});
		}
	}
	push(@bin_ceilings,$value_hash{$n_values-1});

	return \@bin_floors ,\@bin_ceilings;
}

sub get_bin_ceilings_from_count
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  value_hash => { isa => 'HashRef', optional => 1 },
							  n_bins => { isa => 'Maybe[Int]', optional => 1 },
							  single_bin_size => { isa => 'Maybe[Int]', optional => 1 },
							  list_counts => { isa => 'ArrayRef[Num]', optional => 1 },
							  data_column => { isa => 'ArrayRef[Num]', optional => 0 },
							  data_indices => { isa => 'ArrayRef[Num]', optional => 0 }
		);
	my %value_hash = defined $parm{'value_hash'} ? %{$parm{'value_hash'}} : ();
	my $n_bins = $parm{'n_bins'};
	my $single_bin_size = $parm{'single_bin_size'};
	my @list_counts = defined $parm{'list_counts'} ? @{$parm{'list_counts'}} : ();
	my @data_column = defined $parm{'data_column'} ? @{$parm{'data_column'}} : ();
	my @data_indices = defined $parm{'data_indices'} ? @{$parm{'data_indices'}} : ();
	my @bin_ceilings = ();

	# input value_hash
	# n_bins or list_counts or single_bin_size
	# data_column, and data_indices
	# return bin_ceilings, list of upperlimits, upperlimit is part of lower bin!!!

	my @ideal_count=();
	my $value_index;
	my $n_values = scalar(keys %value_hash);
	my @obs_count=(0) x $n_values;
	my $nonzero_counts=0;

	#Count how many values we have in input for each unique value
	#and how many of the unique values have nonzero count
	for ($value_index=0; $value_index < $n_values;$value_index++){
		foreach my $index (@data_indices){
			if ($data_column[$index] == $value_hash{$value_index}){
				$obs_count[$value_index] +=1;
			}
		}
	}
	foreach my $c (@obs_count){
		if ($c > 0){
			$nonzero_counts++;
		}
	}

	if (defined $n_bins){
		if ($n_bins < 1){
			croak("Number of bins must be at least 1.");
		}
		if (scalar(@list_counts)>0){
			croak("Cannot input both number of bins and set of bin counts.");
		}

		if ($n_bins >= $nonzero_counts){#one bin per unique value with nonzero obs => no binning
			return;
		}
		my $count = scalar(@data_indices)/$n_bins;
		@ideal_count = ($count) x $n_bins;
		
	} elsif (defined $single_bin_size){
		#translate to $n_bins
		my $nb = int (scalar(@data_indices)/$single_bin_size); #round down
		$nb++ if ((scalar(@data_indices)/$single_bin_size)-$nb >= 0.5);
		if ($nb >= $nonzero_counts){#one bin per unique value with nonzero obs => no binning
			return;
		}
		my $count = scalar(@data_indices)/$nb;
		@ideal_count = ($count) x $nb;	
	} elsif (scalar(@list_counts)>0 && scalar(@list_counts)<$nonzero_counts ){
		#check that requested counts matches total number of observations
		my $checksum=0;
		foreach my $c (@list_counts){
			$checksum += $c;
		}
		my $data_index_count=scalar(@data_indices);
		unless ($checksum == $data_index_count){
			croak("Sum of observations requested in each bin $checksum must equal ".
				  "total number of observations (in strata) $data_index_count.");
		}

		push (@ideal_count,@list_counts);
	}  else {
		return;
	}

	my $global_error=0;
	my $bin_index=0;
	my $local_error=-$ideal_count[$bin_index];

	for ($value_index = 0; $value_index < $n_values; $value_index++) {
		if ($bin_index == $#ideal_count){
			#have reached last bin
			#last upper limit is largest unique value, include all remaining observations
			push(@bin_ceilings,$value_hash{($n_values-1)});
			last;
		}elsif ($local_error == -$ideal_count[$bin_index]){
			#bin empty since error equals initial error
			$local_error += $obs_count[$value_index];
		}elsif ($obs_count[$value_index] == 0){
			next;
		}elsif (abs($global_error+$local_error)>
				abs($global_error+$local_error+$obs_count[$value_index])
				&& (($n_values-$value_index-1)> ($#ideal_count - $bin_index))){
			#adding observations will reduce global error
			#and more than one unique value remaining per remaining bin.
			$local_error += $obs_count[$value_index];

		} else {
			#bin not empty and best add observations to new bin
			push(@bin_ceilings,$value_hash{$value_index-1});
			$global_error += $local_error;
			$bin_index++;
			$local_error=-$ideal_count[$bin_index]+$obs_count[$value_index];
		}


	}

	return \@bin_ceilings;
}

sub get_bin_boundaries_overlap_value
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  data_column => { isa => 'ArrayRef[Num]', optional => 0 },
							  data_indices => { isa => 'ArrayRef[Num]', optional => 0 },
							  width => { isa => 'Num', optional => 0 },
							  overlap_percent => { isa => 'Num', optional => 0 }
		);
	my @data_column = defined $parm{'data_column'} ? @{$parm{'data_column'}} : ();
	my @data_indices = defined $parm{'data_indices'} ? @{$parm{'data_indices'}} : ();
	my $width = $parm{'width'};
	my $overlap_percent = $parm{'overlap_percent'};
	my @bin_floors = ();
	my @bin_ceilings = ();

	# @data_column, @data_indices
	# width, overlap_percent
	#return bin_floors, bin_ceilings

	my @local_values=();
	for (my $i=0; $i< scalar(@data_indices); $i++){
		push(@local_values,$data_column[$data_indices[$i]]);
	}
	
	my @temp = sort {$a <=> $b} @local_values;
	my $minval = $temp[0];
	my $maxval = $temp[$#temp];
	my $non_overlap_width=$width*(100-$overlap_percent)/100;

	my $next_ceiling=$minval+$width;
	my $next_floor=$minval;
	while ($next_ceiling < $maxval){
		push(@bin_floors,$next_floor);
		push(@bin_ceilings,$next_ceiling);
		$next_ceiling += $non_overlap_width;
		$next_floor = $next_ceiling - $width;
	}
	push(@bin_floors,$next_floor);
	push(@bin_ceilings,$maxval);

	return \@bin_floors ,\@bin_ceilings;
}

sub get_bin_ceilings_from_value
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  data_column => { isa => 'ArrayRef[Num]', optional => 0 },
							  data_indices => { isa => 'ArrayRef[Num]', optional => 0 },
							  n_bins => { isa => 'Maybe[Int]', optional => 1 },
							  single_bin_size => { isa => 'Maybe[Int]', optional => 1 },
							  list_boundaries => { isa => 'ArrayRef[Num]', optional => 1 }
		);
	my @data_column = defined $parm{'data_column'} ? @{$parm{'data_column'}} : ();
	my @data_indices = defined $parm{'data_indices'} ? @{$parm{'data_indices'}} : ();
	my $n_bins = $parm{'n_bins'};
	my $single_bin_size = $parm{'single_bin_size'};
	my @list_boundaries = defined $parm{'list_boundaries'} ? @{$parm{'list_boundaries'}} : ();
	my @bin_ceilings = ();

	# @data_column, @data_indices
	# n_bins or list_boundaries or single_bin_size
	#return bin_ceilings, upper limits of intervals, upperlimit part of lower bin

	my @local_values=();
	for (my $i=0; $i< scalar(@data_indices); $i++){
		push(@local_values,$data_column[$data_indices[$i]]);
	}
	
	my @temp = sort {$a <=> $b} @local_values;
	my $minval = $temp[0];
	my $maxval = $temp[$#temp];
	
	if (defined $n_bins){
		if (scalar(@list_boundaries)>0){
			croak("Cannot input both number of intervals and set of boundaries");
		}
		my $width = ($maxval-$minval)/$n_bins;
		for (my $i=1; $i<=$n_bins; $i++){
			push (@bin_ceilings,($width*$i+$minval));
		}
	} elsif (defined $single_bin_size){
		#translate to $n_bins
		my $nb = int(($maxval-$minval)/$single_bin_size); #round down
		$nb++ if ((($maxval-$minval)/$single_bin_size)-$nb >= 0.5);
		$nb++ if ($nb <= 0);
		my $width = ($maxval-$minval)/$nb;
		for (my $i=1; $i<=$nb; $i++){
			push (@bin_ceilings,($width*$i+$minval));
		}

	} elsif (scalar(@list_boundaries)>0){
		push (@bin_ceilings,@list_boundaries);
		if ($maxval > $list_boundaries[$#list_boundaries]){
			push (@bin_ceilings,$maxval);
		}
	}

	return \@bin_ceilings;
}

sub index_matrix_binned_values
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  value_hash => { isa => 'HashRef[Num]', optional => 0 },
							  reference_index => { isa => 'Maybe[Int]', optional => 1 },
							  data_column => { isa => 'ArrayRef[Num]', optional => 0 },
							  data_indices => { isa => 'ArrayRef[Num]', optional => 0 },
							  bin_ceilings => { isa => 'Maybe[ArrayRef[Num]]', optional => 1 },			# default => ()?
							  bin_floors => { isa => 'Maybe[ArrayRef[Num]]', optional => 1 }					# default => ()?
		);
	my %value_hash = defined $parm{'value_hash'} ? %{$parm{'value_hash'}} : ();
	my $reference_index = $parm{'reference_index'};
	my @data_column = defined $parm{'data_column'} ? @{$parm{'data_column'}} : ();
	my @data_indices = defined $parm{'data_indices'} ? @{$parm{'data_indices'}} : ();
	my @bin_ceilings = defined $parm{'bin_ceilings'} ? @{$parm{'bin_ceilings'}} : ();
	my @bin_floors = defined $parm{'bin_floors'} ? @{$parm{'bin_floors'}} : ();
	my @index_matrix;

	#based on either unique values hash or bin_ceilings, 
	#create array of arrays of data matrix row indices
	#one index array for each unique value
	#input %value_hash, @data_column, @data_indices, 
	#optional input @bin_ceilings, if empty then unique values
	#optional input @bin_floors, only allowed if ceilings given. 
	#If floors empty then ceilings are floors , if given then first value is inclusive, the rest exclusive
	#output index_matrix
	


    #optional input reference_index put reference stratum first *after created strata*
    #reference index will come with empty array strata_ceilings, which 
    #index_matrix_binned_values interprets as stratify on unique values.



	my @ceilings=();
	my @floors=();
	if (scalar(@bin_ceilings)>0){
		push(@ceilings,@bin_ceilings);
		if (scalar(@bin_floors)>0){
			push(@floors,@bin_floors);
		}else {
			push(@floors,$value_hash{0}); #no floors, only ceilings given
			for (my $i=0; $i<$#ceilings; $i++){
				push(@floors,$ceilings[$i]);
			}
		}
	}else {
		#check that floors not given as input
		if (scalar(@bin_floors)>0){
			croak("Cannot give bin floor input to index_matrix_binned_values ".
				  "unless ceilings given.");
		}
		#no_binning, base on unique values
		my $no_of_values = scalar (keys %value_hash);
		for (my $value_index=0; $value_index<$no_of_values; $value_index++){
			push(@ceilings,$value_hash{$value_index});
		}
		push(@floors,$value_hash{0} - 0.2);
		for (my $i=0; $i<$#ceilings; $i++){
			push(@floors,$ceilings[$i]);
		}
	}

	@index_matrix = (undef) x scalar(@ceilings); #create empty matrix to sort stuff into
	my $found_ref=0;
	for (my $bin_index=0; $bin_index<scalar(@ceilings); $bin_index++){
		my @index_row=();
		my $this_floor=$floors[$bin_index];
		$this_floor -= 0.2 if ($bin_index==0); #first floor is inclusive
		foreach my $row_index (@data_indices){
			if ($data_column[$row_index]>$this_floor && 
				$data_column[$row_index]<=$ceilings[$bin_index] ){
				push (@index_row,$row_index);
			}
		}
		if (defined $reference_index){
			if ($bin_index > $reference_index){
				$index_matrix[$bin_index] = \@index_row; #all with higher index have the same place
			}elsif ($bin_index < $reference_index){
				$index_matrix[$bin_index+1] = \@index_row; #all with a lower index is moved one step to the right
			}else{ #actual reference is put first
				# == 
				$index_matrix[0] = \@index_row;
				$found_ref=1;
			}
		}else{
			$index_matrix[$bin_index] = \@index_row;
		}
	}
	if (defined $reference_index and (not $found_ref)){
		croak("reference index defined: $reference_index but not found in list 0 to ".$#ceilings);
	}

	return \@index_matrix;
}

sub round
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  number => { isa => 'Num', optional => 0 }
		);
	my $number = $parm{'number'};
	my $integer_out;

	my $floor=int($number);
	my $rem=$number-$floor;
	if ($rem >= 0){
		$integer_out = ($rem >= 0.5)? $floor+1 : $floor;
	} else {
		$integer_out = (abs($rem) >= 0.5)? $floor-1 : $floor;
	}

	return $integer_out;
}

sub ceil
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  number => { isa => 'Num', optional => 0 }
		);
	my $number = $parm{'number'};
	my $integer_out;

	my $floor=int($number);
	my $rem=$number-$floor;
	if ($rem > 0){
		$integer_out = $floor+1;
	} else {
		#equal or  neg
		$integer_out = $floor;
	} 

	return $integer_out;
}

sub median
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  sorted_array => { isa => 'Ref', optional => 1 }
		);
	my $sorted_array = $parm{'sorted_array'};
	my $result;

	my $len = scalar( @{$sorted_array} );
	
	if( $len  % 2 ){
		$result = $sorted_array->[($len-1)/2];
	} else {
		$result = ($sorted_array->[$len/2]+$sorted_array->[($len-2)/2])/ 2;
	}

	return $result;
}

sub mean
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  array => { isa => 'Ref', optional => 1 }
		);
	my $array = $parm{'array'};
	my $mean;

	my $val_count= scalar( @{$array} );
	my $sum_values=0;
	foreach my $val (@{$array}){
		$sum_values += $val;
	}
	$mean=$sum_values/$val_count;

	return $mean;
}

sub standard_deviation
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  array => { isa => 'Ref', optional => 1 }
		);
	my $array = $parm{'array'};
	my $result;

	$result = 0;
	my $val_count = scalar(@{$array});
	return if (($val_count == 0) or ($val_count == 1));
	my @sorted = (sort {$a <=> $b} @{$array}); #sort ascending
	my $sum_values=0;
	foreach my $val (@sorted){
		$sum_values += $val;
	}
	
	my $mean = $sum_values / $val_count;
	my @squared_errors;
	foreach my $val (@sorted) {
		push(@squared_errors, ($val - $mean) ** 2);
	}

	@sorted = (sort {$a <=> $b} @squared_errors); #sort ascending
	my $sum_errors_pow2=0;
	foreach my $val (@sorted){
		$sum_errors_pow2 += $val;
	}
	
	$result= sqrt ($sum_errors_pow2/($val_count-1));
	return $result;
}

sub get_data_matrix
{
	my $self = shift;

	my $signal_file = $self->directory . "/m1/" . $self->dv . "_matrix_saved";
	my $censor_signal_file = $self->directory . "/m1/" . $self->censor() . "_matrix_saved";
	my $matrix_saved = ( -e $signal_file ) ? 1 : 0;
	if ((defined $self->censor()) and $matrix_saved){
		croak("inconsistent saved data, " . $self->dv . " matrix saved but not ".
			  $self->censor()." matrix\n") 
			unless (-e $censor_signal_file);
	}
	my $no_sim = $self->samples;
	my $type = ($self->is_vpc) ? 'vpc' : 'npc';
	my @matrix;
	my @censor_matrix;

	#new: lloq/uloq censoring on untransformed dv, must transform uloq/lloq
	#lnDV=1 
	#lnDV=3
	my $lloq = undef;
	my $uloq = undef;
	if (defined $self->lloq()){
		if ($self->lnDV() == 1){
			# DV is on log scale, lloq on normal scale
			$lloq=log($self->lloq());
		}elsif ($self->lnDV() == 3){
			# DV on normal scale, lloq on log scale
			$lloq=exp($self->lloq());
		}else{
			$lloq=$self->lloq();
		}
	}
	if (defined $self->uloq()){
		if ($self->lnDV() == 1){
			# DV is on log scale, uloq on normal scale
			$uloq=log($self->uloq());
		}elsif ($self->lnDV() == 3){
			# DV on normal scale, uloq on log scale
			$uloq=exp($self->uloq());
		}else{
			$uloq=$self->uloq();
		}
	}


	if ($matrix_saved){
		open( MATRIX, $self->directory . "/m1/" . $self->dv . "_matrix.csv" ) or 
			croak("Could not find saved matrix.");
		my $nsim=0;
		while (my $row = <MATRIX>){
			chomp $row;
			push (@matrix,$row);
			$nsim= (split(/,/,$row)) -1 if ($nsim==0);
		}
		close(MATRIX);
		#check that found correct number of columns in matrix
		unless ($nsim == $no_sim){
			croak("Number of simulated data sets $nsim in saved matrix\n".
				  "is different from number $no_sim in input (-samples option).");
		}
		$self->data_matrix(\@matrix);

		if (defined $self->censor()){
			open( MATRIX, $self->directory . "/m1/" . $self->censor() . "_matrix.csv" ) or 
				croak("Could not find saved censor matrix.");
			my $nsim=0;
			while (my $row = <MATRIX>){
				chomp $row;
				push (@censor_matrix,$row);
				$nsim= (split(/,/,$row)) -1 if ($nsim==0);
			}
			close(MATRIX);
			#check that found correct number of columns in matrix
			unless ($nsim == $no_sim){
				croak("Number of simulated data sets $nsim in saved censor matrix\n".
					  "is different from number $no_sim in input (-samples option).");
			}
			#check that found correct number of rows in censor matrix
			unless (scalar(@matrix) == scalar(@censor_matrix)){
				croak("Number of observation rows ".scalar(@censor_matrix).
					  " in saved censor matrix\n".
					  "is different from number ".scalar(@matrix)." in saved dv matrix.");
			}
			if ($self->detection_censored){

				my @temp_matrix;
				for (my $i=0; $i<scalar(@matrix); $i++){
					my @dvrow = (split(/,/,$matrix[$i]));
					my @censrow = (split(/,/,$censor_matrix[$i]));
					my @newrow;
					for (my $j=0; $j<scalar(@dvrow) ; $j++){
						if ($censrow[$j] == 1){
							#dropout
							push(@newrow,1);
						}elsif (defined $lloq and ($dvrow[$j] < $lloq)){
							#below lloq
							push(@newrow,2);
						}elsif (defined $uloq and ($dvrow[$j] > $uloq)){
							#above uloq
							push(@newrow,3);
						}else{
							#not censored
							push(@newrow,0);
						}
					}
					push(@temp_matrix,(join ',',@newrow));
				}
				$self->censor_data_matrix(\@temp_matrix);
				@censor_matrix = ();
			}else{
				$self->censor_data_matrix(\@censor_matrix);
			}
		}elsif ($self->detection_censored){
			#create row by row of censor_matrix based only on uloq/lloq
			my @temp_matrix;
			for (my $i=0; $i<scalar(@matrix); $i++){
				my @dvrow = (split(/,/,$matrix[$i]));
				my @newrow;
				for (my $j=0; $j<scalar(@dvrow) ; $j++){
					if (defined $lloq and ($dvrow[$j] < $lloq)){
						#below lloq
						push(@newrow,2);
					}elsif (defined $uloq and ($dvrow[$j] > $uloq)){
						#above uloq
						push(@newrow,3);
					}else{
						#not censored
						push(@newrow,0);
					}
				}
				push(@temp_matrix,(join ',',@newrow));
			}
			$self->censor_data_matrix(\@temp_matrix);
		}
		#what is saved should be pure 0 - 1 table values for censoring variable
		#then, if lloq/uloq defined, edit the recently read matrix ( 0 -> 2 if lloq, 0 -> 3 if uloq)
		#of create new censor matrix if uloq/lloq but no censor variable

		return;
	}


	my @datearr=localtime;
	my $the_time=sprintf "%2.2d:%2.2d:%2.2d",$datearr[2],$datearr[1],$datearr[0];
	my $this_sec;
	my $message = "\nReading and formatting ${\$self->dv} data. This can take a while...";
	if (defined $self->censor()){
		$message = "\nReading and formatting ${\$self->dv} and ".$self->censor().
			" data. This can take a while...";
	}
	ui -> print (category=>'npc', 
				 message=>$message);
	ui -> print (category=>'vpc', 
				 message=>$message);

	my $orig_file;
	if (defined $self->orig_table) {
		$orig_file = $self->orig_table;
	} else {
		#dv_table_name is for cwres as indpendent variable NM version < 7
		croak("not implemented censoring with cwres as independent variable NM6")
			if (defined $self->censor() and (defined $self->dv_table_name));
		$orig_file = (defined $self->dv_table_name) ? $self->dv_table_name:
		$self->original_model->get_option_value(record_name => 'table',
												option_name => 'FILE',
												problem_index => ($self->origprobnum()-1));
		$orig_file = $self->original_model->directory().$type.'_original.'.$orig_file;
	}

	unless (-e $orig_file) {
		my $file_to_check = $self->searchdir."/NM_run1/compute_cwres.Rout";
		print "\nCheck installation of R and XPose, check file $file_to_check\n" 
			if (($self->dv eq 'CWRES') and ($PaN::nm_major_version < 7));
		$file_to_check = $self->searchdir. "/NM_run1/psn-1.lst";
		croak("File $orig_file \nwith table output for original data does not exist. ".
			  "It is recommended to check lst-file $file_to_check for NONMEM error messages.");
	}
	my $d = data -> new(filename => $orig_file, ignoresign => '@', idcolumn => 1); #we made this table file, ID is 1
	
	unless (defined $d->individuals()){
		croak("File $orig_file \nexists but PsN failed to read any individuals from it.");
	}

	my $no_individuals= scalar(@{$d->individuals()});

	my $filt = $d -> create_row_filter(no_individuals=>$no_individuals);
	my $all_rows_observations = (scalar(@{$filt}) > 0)? 0 : 1;
	my $dv_header = $self->dv;
	my $censor_header = $self->censor();
	if ($PsN::nm_major_version < 7){
		#NONMEM only prints first 4 letters of variable name as header
		#exception for CWRES
		$dv_header = ($self->dv eq 'CWRES') ? $self->dv : substr($self->dv,0,4);
		$censor_header = substr($self->censor(),0,4) if (defined $self->censor());
	}

	@matrix = @{$d -> column_to_array('column'=>$dv_header,'filter'=>$filt)};
	my $no_observations = scalar(@matrix);
	
	unless ($no_observations >0){
		croak("No ${\$self->dv} values found after filtering original data.");
	}
	if (defined $self->censor()){
		@censor_matrix = @{$d -> column_to_array('column'=>$censor_header,'filter'=>$filt)};
		my $cens_observations = scalar(@censor_matrix);
		
		unless ($cens_observations == $no_observations){
			croak("Number of censoring data $cens_observations different from number of ".
				  " dv observations $no_observations.");
		}
	}
	

	$filt = undef;
	$d = undef;

	@datearr=localtime;
	my $start_sec=$datearr[2]*3600+$datearr[1]*60+$datearr[0];

	#### New coding of simulation_model(s) will break the handling of DV in cwtab

	my @model_sims;
	my $n_model_sims=0;
	if (defined $self->sim_table){
		$n_model_sims=1;
	}else{
		@model_sims = @{$self-> simulation_models()};
		$n_model_sims=scalar(@model_sims);
	}
	my $tables_read=0;
	my $mdv_index;
	my $dv_index;
	my $censor_index;
	my $obscount=0;

	for( my $sim_i = 0; $sim_i < $n_model_sims; $sim_i++ ) {
		my $sim_file;
		
		#error if sim_table and n_simulation_models > 1
		if (defined $self->sim_table){
			croak("cannot use option n_simulation_models with sim_table") 
				unless ($self->n_simulation_models() == 1);
			$sim_file = $self->sim_table;
		} else {
			#dv_table_name is for cwres as indpendent variable NM version < 7
			#will break if multiple simulation models
			croak("not implemented censoring with cwres as independent variable NM6")
				if (defined $self->censor() and (defined $self->dv_table_name));
			$sim_file = (defined $self->dv_table_name) ? $self->dv_table_name:
			$model_sims[$sim_i]->get_option_value(record_name => 'table',
												  option_name => 'FILE',
												  problem_index => ($self->simprobnum()-1));
			$sim_file = $model_sims[$sim_i]->directory().$type.'_simulation.'.($sim_i+1).'.'.$sim_file;
		}
		
		unless ( -e $sim_file ){
			my $file_to_check = $self->searchdir."/NM_run".(1+$self->run_the_original)."/compute_cwres.Rout";
			print "\nCheck installation of R and XPose, check file $file_to_check\n" 
				if (($self->dv eq 'CWRES') and ($PsN::nm_major_version < 7));
			$file_to_check = $self->searchdir."/NM_run" . ($sim_i + 1+$self->run_the_original) . "/psn-1.lst";
			croak("File $sim_file \nwith table output for simulated data does not exist. ".
				  "It is recommended to check lst-file\n$file_to_check \nfor NONMEM error messages.");
		}

		open (FILE, "$sim_file") or croak("Could not open $sim_file for reading");
		
		while (1){
			my $line = readline(FILE);
			last unless (defined $line); #reached EOF
			chomp $line;
			next if ($line =~ /^TABLE NO/);
			if ($line =~ /^\s*ID\s/){
				if ($tables_read==0){
					#first table, parse header
					$line =~ s/^\s*//;
					my @header = split (/\s+/,$line);
					for (my $i=0; $i<scalar(@header);$i++){
						if ($header[$i] eq $self->dv){
							$dv_index = $i;
							last;
						}
					}
					unless (defined $dv_index){
						croak("Could not find column with header for dependent variable ".$self->dv." in $sim_file\n");
					}
					if (defined $self->censor){
						for (my $i=0; $i<scalar(@header);$i++){
							if ($header[$i] eq $self->censor){
								$censor_index = $i;
								last;
							}
						}
						unless (defined $censor_index){
							croak("Could not find column with header for censor variable ".$self->censor." in $sim_file\n");
						}
					}
					unless ($all_rows_observations){
						for (my $i=0; $i<scalar(@header);$i++){
							if ($header[$i] eq 'MDV'){
								$mdv_index = $i;
								last;
							}
						}
						unless (defined $mdv_index){
							croak("Could not find column with header MDV in $sim_file\n");
						}
					}
					ui -> print (category=>'npc', 
								 message=>'Reading sample ',
								 newline => 0);
					ui -> print (category=>'vpc', 
								 message=>'Reading sample ',
								 newline => 0);
				}else{
					#found new table, check if previous had right number obs
					unless ($obscount == $no_observations){
						croak ("Expected $no_observations observation rows after MDV filtering ".
							   "but found $obscount. File is $sim_file, table count (starting from first sim file) is $tables_read");
					}
					$obscount=0;
				}
				$tables_read++;

				my $modulus = (($no_sim+1) <= 50) ? 1 : (($no_sim+1) / 50);
				
				if ( $tables_read % $modulus == 0 or $tables_read == 1 or $tables_read == $no_sim ) {
					ui -> print (category=>'npc', 
								 message=>$tables_read.' ',
								 newline => 0);
					ui -> print (category=>'vpc', 
								 message=>$tables_read.' ',
								 newline => 0);
				}


				
			}elsif($line =~ /[0-9]/){
				#assume this is data line, at least it is not empty
				$line =~ s/^\s*//;
				my @items = split (/\s+/,$line);
				if ($all_rows_observations or $items[$mdv_index]<1){
					$matrix[$obscount] .= ','.$items[$dv_index];
					if (defined $self->censor()){
						$censor_matrix[$obscount] .= ','.$items[$censor_index];
					}
					$obscount++;
				}
			}
		} #end loop over sim file lines
		close FILE;
		unless ($obscount == $no_observations){
			croak ("Expected $no_observations observation rows after MDV filtering ".
				   "but found $obscount. File is $sim_file, table count (starting from first sim file) is $tables_read");
		}

	} #endof loop over read simdata


	#check simulations read
	my $no_read_sim= (split(/,/,$matrix[0])) -1;
	unless ($no_sim == $no_read_sim){
		croak("Number of read simulated datasets $no_read_sim is\n".
			  "different from expected number $no_sim.");
	}
	if (defined $self->censor()){
		my $no_read_cens= (split(/,/,$censor_matrix[0])) -1;
		unless ($no_sim == $no_read_cens){
			croak("Number of read simulated censor datasets $no_read_cens is\n".
				  "different from expected number $no_sim.");
		}
	}
	
	#print matrix
	open( MATRIX, ">".$self->directory."/m1/".$self->dv."_matrix.csv" ) ;
	foreach my $row (@matrix){
		print MATRIX "$row"."\n";
	}
	close (MATRIX);
	#create signal file
	open( DONE, ">".$signal_file ) ;
	my $tmp = $no_sim+1;
	print DONE "Created $no_observations rows by $tmp columns data matrix ".
		"and wrote to file ${\$self->dv}_matrix.csv\n";
	print DONE "First column is original data, columns 2 to $tmp are simulated data.\n";
	close( DONE );

	$self->data_matrix(\@matrix);

	if (defined $self->censor()){
		open( MATRIX, ">" . $self->directory . "/m1/" . $self->censor() . "_matrix.csv" ) ;
		foreach my $row (@censor_matrix) {
			print MATRIX "$row"."\n";
		}
		close (MATRIX);
		#create signal file
		open( DONE, ">".$censor_signal_file ) ;
		my $tmp = $no_sim+1;
		print DONE "Created $no_observations rows by $tmp columns censor matrix ".
			"and wrote to file ${\$self->dv}_matrix.csv\n";
		print DONE "First column is original data, columns 2 to $tmp are simulated data.\n";
		close( DONE );
		#edit censor matrix if combined with lloq/uloq
		if ($self->detection_censored){
			my @temp_matrix;
			for (my $i=0; $i<scalar(@matrix); $i++){
				my @dvrow = (split(/,/,$matrix[$i]));
				my @censrow = (split(/,/,$censor_matrix[$i]));
				my @newrow;
				for (my $j=0; $j<scalar(@dvrow) ; $j++){
					if ($censrow[$j] == 1){
						#dropout
						push(@newrow,1);
					}elsif (defined $lloq and ($dvrow[$j] < $lloq)){
						#below lloq
						push(@newrow,2);
					}elsif (defined $uloq and ($dvrow[$j] > $uloq)){
						#above uloq
						push(@newrow,3);
					}else{
						#not censored
						push(@newrow,0);
					}
				}
				push(@temp_matrix,(join ',',@newrow));
			}
			$self->censor_data_matrix(\@temp_matrix);
			@censor_matrix = ();
		}else{
			$self->censor_data_matrix(\@censor_matrix);
		}
	}elsif ($self->detection_censored){
		#create row by row of censor_matrix based only on uloq/lloq
		my @temp_matrix;
		for (my $i=0; $i<scalar(@matrix); $i++){
			my @dvrow = (split(/,/,$matrix[$i]));
			my @newrow;
			for (my $j=0; $j<scalar(@dvrow) ; $j++){
				if (defined $lloq and ($dvrow[$j] < $lloq)){
					#below lloq
					push(@newrow,2);
				}elsif (defined $uloq and ($dvrow[$j] > $uloq)){
					#above uloq
					push(@newrow,3);
				}else{
					#not censored
					push(@newrow,0);
				}
			}
			push(@temp_matrix,(join ',',@newrow));
		}
		$self->censor_data_matrix(\@temp_matrix);
	}


	ui -> print (category=>'npc', message=> "\nDone reading and formatting data, finishing run.");
	ui -> print (category=>'vpc', message=> "\nDone reading and formatting data, finishing run.");
}

sub get_tte_data
{
	my $self = shift;

	#make sure we are in $self->directory(), zipping cannot handle absolute paths

	my $return_directory = getcwd();
	chdir( $self->directory );

	my $type = ($self->is_vpc) ? 'vpc' : 'npc';
	my $orig_file;
	$orig_file = (defined $self->dv_table_name) ? $self->dv_table_name:
	$self->original_model->get_option_value(record_name => 'table',
											option_name => 'FILE',
											problem_index => ($self->origprobnum()-1));
	$orig_file = $self->original_model->directory().$type.'_original.'.$orig_file;
	my @model_sims = @{$self->simulation_models()};
	my $n_model_sims=scalar(@model_sims);

	unless ( -e $orig_file ){
		my $file_to_check = $self->searchdir."/NM_run1/psn-1.lst";
		croak("File $orig_file \nwith table output for original data does not exist. ".
			  "It is recommended to check lst-file $file_to_check for NONMEM error messages.");
	}
	my $tabno='';
	if (($self->models->[0]->filename() =~ /^run/) and
		($self->models->[0]->filename() =~ /\.mod$/)){
		$tabno=$self->models->[0]->filename();
		$tabno =~ s/^run// ;
		$tabno =~ s/\.mod$// ;
	}
	if (length($tabno)>0){
		mv ($orig_file,'mytab'.$tabno);
		#check that mytab does not already exists in above directory before copying
		#if it exists then probably kaplan.plot will work anyway
		if (-e $self->models->[0]->directory().'mytab'.$tabno and
			not -e $self->models->[0]->directory().'mytab'.$tabno.'_original'){
			mv ($self->models->[0]->directory().'mytab'.$tabno,
				$self->models->[0]->directory().'mytab'.$tabno.'_original');
			ui -> print (category=>'vpc', message=>"\nRenamed original mytab$tabno, new name ".
						 "mytab$tabno"."_original. New mytab$tabno can be used for kaplan.plot in Xpose.");
		}
		cp('mytab'.$tabno, $self->models->[0]->directory().'mytab'.$tabno);
	}else{
		mv ($orig_file,'mytab1');
	}
	print "\nReading and formatting ".$self->tte()." data. This can take a while...\n";

	my $sim_data_full='fullsimtab'.$tabno; #must have local names
	my $sim_data='simtab'.$tabno; #local names
	unless ($self->clean() > 2){
		open(FULL, ">$sim_data_full") || die("Couldn't open $sim_data_full : $!");
	}
	open(DATA, ">$sim_data") || die("Couldn't open $sim_data : $!");
	my $header_seen = 0;
	my @names;
	my $counter = 0;
	my %values;
	my $sim_num = 0;
	my $did_print = 0;
	for( my $sim_i = 0; $sim_i < $n_model_sims; $sim_i++ ) {
		my $sim_file = (defined $self->dv_table_name) ? $self->dv_table_name:
		$model_sims[$sim_i]-> get_option_value(record_name => 'table',
											   option_name => 'FILE',
											   problem_index => ($self->simprobnum()-1));
		$sim_file = $model_sims[$sim_i]->directory().$type.'_simulation.'.($sim_i+1).'.'.$sim_file;

		unless ( -e $sim_file ){
			my $file_to_check = $self->searchdir."/NM_run".($sim_i+1+$self->run_the_original)."/psn-1.lst";
			croak("File $sim_file \nwith table output for simulated data does not exist. ".
				  "It is recommended to check lst-file\n$file_to_check \nfor NONMEM error messages.");
		}
		
		my $old_id =  0;
		my $sim_id = 0;
		
		open(RAW, "$sim_file") || die("Couldn't open $sim_file : $!");
		while(<RAW>) {
			print FULL $_ unless ($self->clean() > 2);
			chomp;
			next if /^\s*TABLE NO/i; # remove "TABLE NO" header lines
			my @row = split(" ",$_);
			# read the names of the rows if it is the first header line
			if (/ID/) {
				$sim_num++;
				if($header_seen==0){ # keep first header
					@names=@row;
					$header_seen = 1;
					# add a count of events per individual
					@names = (@names,"counter","simID","simNumber");
					
					my $out_line=join(" ",@names);
					print DATA "$out_line\n";
				} 
				next;
			} else {
				@values{@names}=@row;
				if(@values{"ID"} != $old_id){
					$counter = 0;
					$sim_id++;
				}
				$old_id = @values{"ID"};
				if(@values{$self->tte()}==0){ # non-events
					next;
					#print "zero\n"
				} else { # events
					$counter++;
					@values{"counter"} = $counter;
					@values{"simID"} = $sim_id;
					@values{"simNumber"} = $sim_num;
				}
			}

			my $out_line=join(" ",@values{@names});
			print DATA "$out_line\n";
			$did_print=1;

		}
		close(RAW);

	}
	close(FULL) unless ($self->clean() > 2);
	close(DATA);

	unless ($did_print){
		ui -> print (category=>'vpc', message=>"\nError: Did not find any events. Something went ".
					 "wrong with the simulations. Expect errors if trying to plot results.\n");
	}

	my $zip_files=1;
	my $remove_after_zip=1;
	my $done_zip=0;
	if($zip_files == 1 and eval("require Archive::Zip")){

		unless ($self->clean() > 2){
			my $zip = Archive::Zip->new();
			my $file_member = $zip->addFile($sim_data_full);
			if ( $zip->writeToFileNamed($sim_data_full.'.zip') == 'AZ_OK' ) {
				unlink($sim_data_full) if ($remove_after_zip==1);
			}
		}
		my $zip = Archive::Zip->new();
		my $file_member = $zip->addFile($sim_data);
		if ( $zip->writeToFileNamed($sim_data.'.zip') == 'AZ_OK' ) {
			unlink($sim_data) if ($remove_after_zip==1);
			$done_zip=1;
			if (length($tabno)>0){
				mv($sim_data.'.zip',$self->models->[0]->directory().$sim_data.'.zip');
			}else{
				mv($sim_data.'.zip',$sim_data.'1.zip');
			}
		}
	}
	unless ($done_zip){
		ui -> print (category=>'vpc', message=>"\nCould not zip file $sim_data. ".
					 "It must be zipped manually before creating Kaplan-Meier plots with Xpose.\n");
		if (length($tabno)>0){
			mv($sim_data, $self->models->[0]->directory().$sim_data);
		}else{
			mv($sim_data,$sim_data.'1');
		}
	}
	chdir( $return_directory );
	$self -> cleanup();
}

sub cleanup
{
	my $self = shift;

	#remove tablefiles in NM_run1 and NM_run2, they are 
	#copied to m1 by modelfit and read from there anyway.

	#npctab.dta npctab-1.dta
	#globbing does not work on windows
	my $modf = $self->directory; 

	my $index = 1;
	while (-d $self->searchdir."/NM_run$index"){
		unlink $self->searchdir."/NM_run$index"."/npctab-1.dta";
		unlink $self->searchdir."/NM_run$index"."/npctab.dta";
		$index++;
	}
	if (defined $self->tte()){
		my $index = 1;
		while (-e "$modf/m1/vpc_simulation.$index.npctab.dta"){
			unlink "$modf/m1/vpc_simulation.$index.npctab.dta";
			$index++;
		}
	}
}

sub create_binned_data
{
	my $self = shift;

	#get rid of stratified_data, replace with array or arrays of arrays row strings
	# binned_data
	#do the same with censor variable. Use censor variable in pred/varcorr

	#make hash for binning

	open my $bin_file, ">", "vpc_bins.txt";    # For binning results

	my $no_of_strata = scalar(@{$self->strata_matrix});
	for (my $strat_ind=0; $strat_ind<$no_of_strata; $strat_ind++) {
		my @bin_array;
		my @pred_array;
		my @bound_array;
		my @id_array;
		my @strt_array;
		foreach my $index (@{$self->strata_matrix->[$strat_ind]}) {
			push (@bin_array, $self->idv_array->[$index]);
			push (@pred_array, $self->pred_array->[$index]) if ($self->predcorr || $self->varcorr);
			push (@bound_array, $self->bound_array->[$index]) if ($self->predcorr and 
																  (defined $self->bound_variable) );
			push (@id_array,$self->id_array->[$index]);
			push (@strt_array,$self->strata_variable_vector->[$index]) if (defined $self->stratify_on);
		}
		my @data_indices = (0..$#bin_array);

		my $bin_hash = $self->create_unique_values_hash('data_column' => \@bin_array);

		my ($bin_floors, $bin_ceilings);

		if ($self->bin_by_count eq '1') {
			if (defined $self->overlap_percent) {
				($bin_floors, $bin_ceilings) = $self -> get_bin_boundaries_overlap_count(
					'data_column'		 => \@bin_array,
					'value_hash'		 => $bin_hash,
					'data_indices'	 => \@data_indices,
					'count'				 => $self->single_bin_size,
					'overlap_percent' => $self->overlap_percent);
			} else {
				# add input param single_bin_size
				$bin_ceilings = $self -> get_bin_ceilings_from_count(
					'data_column'			=> \@bin_array,
					'value_hash'			=> $bin_hash,
					'data_indices'		=> \@data_indices,
					'n_bins'				=> $self->no_of_bins,
					'single_bin_size'	=> $self->single_bin_size,
					'list_counts' 		=> $self->bin_array);
			}
		} elsif ($self->bin_by_count eq '0') {
			if (defined $self->overlap_percent) {
				($bin_floors, $bin_ceilings) = $self -> get_bin_boundaries_overlap_value(
					'data_column'		 => \@bin_array,
					'data_indices'	 => \@data_indices,
					'width'				 => $self->single_bin_size,
					'overlap_percent' => $self->overlap_percent);
			} else {
				# add input param single_bin_size
				$bin_ceilings = $self -> get_bin_ceilings_from_value(
					'data_column'			=> \@bin_array,
					'data_indices'		=> \@data_indices,
					'n_bins'				=> $self->no_of_bins,
					'single_bin_size'	=> $self->single_bin_size,
					'list_boundaries'	=> $self->bin_array);
			}
		} elsif ($self->auto_bin_mode eq 'auto') {
			$bin_ceilings = binning::bin_auto(\@bin_array, $self->min_points_in_bin, $self->strata_labels->[$strat_ind]);
		} elsif ($self->auto_bin_mode eq 'minmax') {
			$bin_ceilings = binning::bin_range(\@bin_array, $self->min_no_bins, $self->max_no_bins, $self->min_points_in_bin, $self->strata_labels->[$strat_ind]);
		}

		my $bin_matrix = $self -> index_matrix_binned_values(
			'data_column'		=> \@bin_array,
			'value_hash'		=> $bin_hash,
			'data_indices'	=> \@data_indices,
			'bin_floors'		=> $bin_floors,
			'bin_ceilings'	=> $bin_ceilings);
		my $no_bins = scalar(@{$bin_matrix});
		if (defined $bin_ceilings) {
			unless (defined $bin_floors) {
				$bin_floors->[0] = ${$bin_hash}{0};
				for (my $bi = 1; $bi < $no_bins; $bi++) {
					$bin_floors->[$bi] = $bin_ceilings->[$bi - 1];
				}
			}
		} else { # unique value binning
			for (my $bi = 0; $bi < $no_bins; $bi++) {
				$bin_ceilings->[$bi] = ${$bin_hash}{$bi};
			}
		}

		my @all_bins;
		my @all_bins_censor;
		my @binned_id;
		my @binned_idv;
		my @binned_strt;
		my $lambda = $self->boxcox_lambda();

		for (my $bin_ind=0; $bin_ind<$no_bins; $bin_ind++){
			my @bin_data;
			my @bin_data_censor;
			my @pred_values;
			my @bound_values;
			my @id_values;
			my @idv_values;
			my @strt_values;
			foreach my $index (@{$bin_matrix->[$bin_ind]}){
				my $valuestring = $self->stratified_data->[$strat_ind]->[$index];
				if ($self->lnDV == 3){ #do log transform
					chomp $valuestring;
					my @tmp = split(/,/,$valuestring); 
					croak("cannot log non-positive observation value ".$tmp[0]) unless ($tmp[0]>0);
					$valuestring = log($tmp[0]);
					for (my $i=1; $i<scalar(@tmp); $i++){
						croak("cannot log non-positive simulated value ".$tmp[$i]) unless ($tmp[$i]>0);
						$valuestring .= ','.log($tmp[$i]);
					}
				}elsif (( $lambda > 0) and ($self->predcorr)){
					#transform from Box-Cox to normal scale
					chomp $valuestring;
					my @tmp = split(/,/,$valuestring); 
					$valuestring= ($tmp[0]*$lambda+1)**(1/$lambda);
					for (my $i=1; $i<scalar(@tmp); $i++){
						$valuestring .= ','.(($tmp[$i]*$lambda+1)**(1/$lambda));
					}
				}
				push (@bin_data,$valuestring);
				if (defined $self->censor_stratified_data){
					my $censorstring = $self->censor_stratified_data->[$strat_ind]->[$index];
					push (@bin_data_censor,$censorstring);
				}
				if ($self->predcorr || $self->varcorr){
					my $val = $pred_array[$index];
					if ($self->lnDV == 3){
						croak("cannot log non-positive PRED value ".$val) unless ($val>0);
						$val = log($val) ;
					}elsif ( $lambda > 0){
						#transform from Box-Cox to normal scale
						$val = ($val*$lambda+1)**(1/$lambda);
					}
					push (@pred_values,$val);
					if (defined $self->bound_variable){
						push (@bound_values,$bound_array[$index]) ;
					}elsif (defined $self->lower_bound){
						push (@bound_values,$self->lower_bound) ;
					}else {
						push (@bound_values,0) ;
					}
				}
				push (@id_values,$id_array[$index]);
				push (@idv_values,$bin_array[$index]);
				push (@strt_values,$strt_array[$index]) if (defined $self->stratify_on);
			}

			if ($self->predcorr and (scalar(@pred_values) > 0)){
				my $new_data = $self->do_predcorr_and_varcorr(pred_array=>\@pred_values, 
															  data_array => \@bin_data,
															  bound_array=>\@bound_values) ;
				@bin_data = @{$new_data};
			}

			if ($self->lnDV == 1){ #do exponentiation
				my @old_data = @bin_data;
				@bin_data=();
				foreach my $valuestring (@old_data){
					chomp $valuestring;
					my @tmp = split(/,/,$valuestring); 
					$valuestring = exp($tmp[0]);
					for (my $i=1; $i<scalar(@tmp); $i++){
						$valuestring .= ','.exp($tmp[$i]);
					}
					push(@bin_data,$valuestring);
				}
			}elsif (( $lambda > 0) and ($self->predcorr)){
				#transform from normal to Box-Cox
				my @old_data = @bin_data;
				@bin_data=();
				foreach my $valuestring (@old_data){
					chomp $valuestring;
					my @tmp = split(/,/,$valuestring); 
					$valuestring = (($tmp[0]**$lambda)-1)/$lambda;  
					for (my $i=1; $i<scalar(@tmp); $i++){
						$valuestring .= ','.(($tmp[$i]**$lambda)-1)/$lambda;
					}
					push(@bin_data,$valuestring);
				}

			}

			push(@all_bins,\@bin_data);
			push(@all_bins_censor,\@bin_data_censor);
			push(@binned_id,\@id_values);
			push(@binned_idv,\@idv_values);
			push(@binned_strt,\@strt_values);
		}
		$self->stratified_data->[$strat_ind] = undef;
		$self->binned_data->[$strat_ind]=\@all_bins;
		if (defined $self->censor_stratified_data){
			$self->censor_stratified_data->[$strat_ind] = undef;
			$self->censor_binned_data([]) unless defined $self->censor_binned_data;
			$self->censor_binned_data->[$strat_ind]=\@all_bins_censor;
		}
		$self->bin_ceilings->[$strat_ind] = $bin_ceilings;
		$self->bin_floors->[$strat_ind]=$bin_floors; #may be undef
		$self->binned_id->[$strat_ind] = \@binned_id;
		$self->binned_idv->[$strat_ind] = \@binned_idv;
		$self->binned_strt->[$strat_ind] = \@binned_strt;

		# Write bin edges to disk
		print $bin_file '-bin_array=', join(',', @$bin_ceilings), "\n";

	} #endof loop over strata

	close $bin_file;

	$self->clear_idv_array;
	$self->clear_id_array;
	$self->clear_strata_variable_vector;

	$self->reprint_mirror_and_plot_data() if ($self->varcorr 
											  or $self->predcorr
											  or $self->lnDV == 1
											  or $self->lnDV == 3);
}

sub do_predcorr_and_varcorr
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  pred_array => { isa => 'Ref', optional => 1 },
							  bound_array => { isa => 'Ref', optional => 1 },
							  data_array => { isa => 'Ref', optional => 1 }
		);
	my $pred_array = $parm{'pred_array'};
	my $bound_array = $parm{'bound_array'};
	my $data_array = $parm{'data_array'};
	my @corrected_data_array;

	#this is for one bin
	my $n_pred = scalar(@{$pred_array}); #number of observations in bin = number of PRED values
	my @pc_data_array; #will hold pred corrected values after pc step

	#data_array has one element for each observation, each element is
	# a comma-separated string of values, first the real value and then the simulations 

	unless ( $n_pred == scalar(@{$data_array})){
		croak("number of pred values and observations not equal in do_predcorr");
	}

	unless ( $n_pred == scalar(@{$bound_array})){
		croak("number of pred values and bound values not equal in do_predcorr");
	}


	#After discussion with Martin: use all pred even if some observations
	#have all datasets censored

	my $median_pred = $self->median(sorted_array => [(sort {$a <=> $b} @{$pred_array})]); # PREDnm
	for (my $i=0; $i< $n_pred; $i++){
		my $pcorr;
		my @newrow;

		if (($self->lnDV == 2) or ($self->lnDV == 1) or ($self->lnDV == 3)){
			#log-transformed data
			$pcorr=$median_pred-($pred_array->[$i]);
			my $row = $data_array->[$i]; 
			chomp $row;
			my @tmp = split(/,/,$row); 
			foreach my $val (@tmp){ #first $val is real observation, the rest are simulated
				push(@newrow,($val+$pcorr));
			}
		}else {

			my $diff = $pred_array->[$i] - $bound_array->[$i]; #PREDi-LBi
			if ($diff > 0){
				$pcorr=($median_pred-$bound_array->[$i])/$diff; #(PREDbin-LBi)/PREDi-LBi
				#can be numerical problems here, cancellation, if median_pred and bound very close. 
				#ignore for now, get worse problems if multiply denominator with median+bound which could be 0
			}else {
				my $message="Found PRED value (".$pred_array->[$i].") lower than the lower bound (".$bound_array->[$i]."). ".
					"Solve this by setting option -lower_bound and rerunning in the same directory, ".
					"reusing the already formatted ${\$self->dv} data.";
				croak($message);
			}
			my $row = $data_array->[$i]; 
			chomp $row;
			my @tmp = split(/,/,$row); 
			foreach my $val (@tmp){
				push(@newrow,($bound_array->[$i]*(1-$pcorr)+$val*$pcorr));
			}
		}
		push (@pc_data_array,\@newrow);
	}

	if ($self->varcorr){
		my $warn=0;
		my @stdevs;
		my $obs_index = 0;
		foreach my $row (@pc_data_array){ #for each observation=row in pc_data_array
			my @values = @{$row}; #make copy of values so we do not destroy anything
			my $orig_value = shift(@values); #remove first value from this row
			push(@stdevs,($self->standard_deviation(array => \@values))); #store stdev from simulated values only
			#standard_deviation handles zero lenght array
		}
		my $median_stdev = $self->median(sorted_array => [(sort {$a <=> $b} @stdevs)]);
		
		for (my $i=0; $i < $n_pred; $i++){ #for each observation
			my $vcorr;
			if ($stdevs[$i] != 0){
				$vcorr=$median_stdev/$stdevs[$i];
			}else {
				$warn =1;
				$vcorr =100000;
			}
			my @newrow;
			foreach my $val (@{$pc_data_array[$i]}){ #for each value (real and sim) for observation i
				push(@newrow,($vcorr*$val+$median_pred*(1-$vcorr)));

			}
			push (@corrected_data_array,(join ',',@newrow));
		}

		if ($warn == 1){
			ui -> print (category=>'vpc', 
						 message=>"\nWARNING\nFor one or more observations, the standard deviation for ".
						 "simulated data values was equal to 0. Setting VCORR ".
						 "to 100000 for those observations.\n\n");
		}

	}else {
		#no varcorr, just arrange pc data in correct output form
		foreach my $arr (@pc_data_array){
			push (@corrected_data_array,(join ',',@{$arr}));
		}
	}

	return \@corrected_data_array;
}

sub create_mirror_and_plot_data
{
	my $self = shift;

	#after stratification, but before removing data_matrix
	#before binning
	#this also collects PRED column for predcorr

	#print header to file
	#if npc or vpc without predcorr or varcorr also print the rest
	#else store array (over strata) of arrays (of values) for
	#identity_data (id)
	#idv_data (eg TIME)
	#strt_data (eg DOSE)
	#and print in separate function after varcorr

	my $no_observations = $self->n_observations;
	my $bin_header = $self->idv;
	if ($PsN::nm_major_version < 7){
		#NONMEM only prints first 4 letters of variable name as header
		#no exception for CWRES since not allowed to bin on CWRES
		$bin_header = substr($self->idv,0,4);
	}
	#add new: If ((defined $self->{dv_table_name }) && (not $self->{'dv} eq 'CWRES'))
	#use dv_table_name instead
	#OR require that stratification and binning variables are in vpc-generated table

	my $orig_file;
	if (defined $self->orig_table){
		$orig_file = $self->orig_table;
	} else {
		$orig_file =  $self->original_model->directory().'vpc_original.'.
			$self->original_model->get_option_value(record_name => 'table',
													option_name => 'FILE',
													problem_index => ($self->origprobnum()-1));
	}
	unless (-e $orig_file) {
		croak("Could not find file $orig_file.");
	}

	my $d = data->new(filename => $orig_file, ignoresign => '@', idcolumn => 1); #table that we made, ID is 1

	my $no_individuals= scalar(@{$d->individuals});
	my $filter = $d -> create_row_filter('no_individuals'=>$no_individuals);

	my $idv_array = $d -> column_to_array('column'=>$bin_header,'filter'=>$filter);
	unless (scalar @{$idv_array} > 0){
		croak("Could not find independent variable column $bin_header in original data file.");
	}
	unless (scalar @{$idv_array} == $no_observations){
		croak("Number of observations in binning column after filtering ".
			  "does not match number of observations in matrix file.");
	}
	$self->idv_array($idv_array);


	my $id_array = $d -> column_to_array('column'=>'ID','filter'=>$filter);
	unless (scalar @{$id_array} > 0){
		croak("Could not find ID column in original data file.");
	}
	unless (scalar @{$id_array} == $no_observations){
		croak("Number of observations in ID column after filtering ".
			  "does not match number of observations in matrix file.");
	}
	$self->id_array($id_array);

	if ($self->predcorr || $self->varcorr){
		my $pred_array = $d -> column_to_array('column'=>'PRED','filter'=>$filter);
		unless (scalar @{$pred_array} > 0){
			croak("Could not find PRED column in original data file.");
		}
		unless (scalar @{$pred_array} == $no_observations){
			croak("Number of observations in PRED column after filtering ".
				  "does not match number of observations in matrix file.");
		}
		$self->pred_array($pred_array);
		if (defined $self->bound_variable){
			#NONMEM only prints first 4 letters of variable name as header
			my $col = ($PsN::nm_major_version < 7) ? substr($self->bound_variable,0,4) : $self->bound_variable;
			my $bound_array = $d -> column_to_array('column'=>$col,'filter'=>$filter);
			unless (scalar @{$bound_array} > 0){
				croak("Could not find $col column in original data file.");
			}
			unless (scalar @{$bound_array} == $no_observations){
				croak("Number of observations in $col column after filtering ".
					  "does not match number of observations in matrix file.");
			}
			$self->bound_array($bound_array);
		}
	}

	#add translated stratacol
	my $no_of_strata = scalar(@{$self->strata_matrix});
	my @translated_strata = (0) x $no_observations;
	for (my $strat_ind=0; $strat_ind<$no_of_strata; $strat_ind++){
		foreach my $j (@{$self->strata_matrix->[$strat_ind]}){
			$translated_strata[$j] = ($strat_ind +1);
		}
	}

	#filename vpctabN - find number N
	my $tabno='';
	if (($self->models->[0]->filename() =~ /^run/) and
		($self->models->[0]->filename() =~ /\.mod$/)){
		$tabno=$self->models->[0]->filename();
		$tabno =~ s/^run// ;
		$tabno =~ s/\.mod$// ;
	}
	my $vpctabname = "vpctab".$tabno;
	if (-e $self->directory."/".$self->results_file()){
		my $fname = $self->results_file();
		$fname =~ s/\.csv$// ;

		my $addnum=1;
		while (-e $self->directory."/$fname"."-old$addnum".'.csv'){
			$addnum++;
		}
		my $newnametab = $vpctabname."-old$addnum";
		while (-e $self->directory."$newnametab"){
			$addnum++;
			$newnametab = $vpctabname."-old$addnum";
		}

		mv( $self->directory.$vpctabname, $self->directory.$newnametab);

		my $newname = "$fname"."-old$addnum".'.csv';
		mv( $self->directory.$fname.'.csv', $self->directory.$newname);

		ui -> print (category=>'vpc', 
					 message=>"Renamed old $vpctabname to $newnametab, and old $fname".
					 ".csv to $newname to protect old output. New output is $vpctabname and $fname".".csv.");
	}
	$self->vpctab_filename($self->directory.$vpctabname);


	#mirrors
	my @mirror_set=();
	my @mirror_labels=();
	if (defined $self->mirrors){
		my @rand_indices=random_permuted_index($self->n_simulations);
		@mirror_set = @rand_indices[0 .. ($self->mirrors-1)];
		for my $j (1 .. $self->mirrors){
			push (@mirror_labels,"mirror_$j");
		}
	}

	$self->mirror_labels(\@mirror_labels);
	$self->mirror_set(\@mirror_set);

	$self->vpctab_header("ID,".$self->dv.",".$self->idv);
	$self->vpctab_header($self->vpctab_header . ",strata_no,".$self->stratify_on) if (defined $self->stratify_on);
	foreach my $j (@mirror_labels){
		$self->vpctab_header($self->vpctab_header . ",$j");
	}
	$self->vpctab_header($self->vpctab_header . "\n");

	open( ST, ">".$self->vpctab_filename);
	print ST $self->vpctab_header;


	for (my $i=0;$i<$no_observations;$i++){ 
		my $idno = sprintf "%d",$self->id_array->[$i] ;
		my $row = $self->data_matrix->[$i];
		my $idvar=$self->idv_array->[$i];
		my ($orig_value,@tmp) = split(/,/,$row); 
		if (defined $self->censor_data_matrix){
			my ($orig_cens,@censor) = split(/,/,$self->censor_data_matrix->[$i]); 
			if ($orig_cens == 1){
				next;
			}elsif ($orig_cens == 2){
				next;
			}elsif ($orig_cens == 3){
				next;
			}
			foreach my $j (@mirror_set){
				if ($censor[$j] == 1){
					$tmp[$j] = -99;
				}elsif ($censor[$j] == 2){
					$tmp[$j] = -99;
				}elsif ($censor[$j] == 3){
					$tmp[$j] = -99;
				}
			}
		}
		print ST "$idno,$orig_value,$idvar";
		print ST ",$translated_strata[$i],".$self->strata_variable_vector->[$i] if (defined $self->stratify_on);
		foreach my $j (@mirror_set){
			print ST ",$tmp[$j]";
		}
		print ST "\n";
	}
	close (ST);
}

sub reprint_mirror_and_plot_data
{
	my $self = shift;

	#if we have done predcorr and/or varcorr and/or transformation
	#must recreate vpctab with corrected DV

	open( ST, ">".$self->vpctab_filename);
	print ST $self->vpctab_header;
	my $no_of_strata = scalar(@{$self->strata_labels});
	for (my $strat_ind=0;$strat_ind<$no_of_strata;$strat_ind++){ 
		my $stratanum = $strat_ind+1; 
		my $no_bins = scalar(@{$self->binned_data->[$strat_ind]});
		for (my $bin=0;$bin<$no_bins;$bin++){ 
			for (my $i=0;$i< scalar(@{$self->binned_data->[$strat_ind]->[$bin]});$i++){ 
				my $row = $self->binned_data->[$strat_ind]->[$bin]->[$i];
				my $idno = sprintf "%d",$self->binned_id->[$strat_ind]->[$bin]->[$i];
				my $idvar=$self->binned_idv->[$strat_ind]->[$bin]->[$i];
				my $strt=$self->binned_strt->[$strat_ind]->[$bin]->[$i] 
					if (defined $self->stratify_on);
				my ($orig_value,@tmp) = split(/,/,$row); 
				if (defined $self->censor_binned_data){
					my ($orig_cens,@censor) = 
						split(/,/,$self->censor_binned_data->[$strat_ind]->[$bin]->[$i]); 
					if ($orig_cens == 1){
						next;
					}elsif ($orig_cens == 2){
						next;
					}elsif ($orig_cens == 3){
						next;
					}
					foreach my $j (@{$self->mirror_set}){
						if ($censor[$j] == 1){
							$tmp[$j] = -99;
						}elsif ($censor[$j] == 2){
							$tmp[$j] = -99;
						}elsif ($censor[$j] == 3){
							$tmp[$j] = -99;
						}
					}
				}
				print ST "$idno,$orig_value,$idvar";
				print ST ",$stratanum,".$strt if (defined $self->stratify_on);
				foreach my $j (@{$self->mirror_set}){
					print ST ",$tmp[$j]";
				}
				print ST "\n";
			}
		}
	}
	close (ST);
}

sub create_stratified_data
{
	my $self = shift;

	#get rid of data_matrix, replace with array or arrays of row strings
	#stratified_data_matrix, stratified_censor_data_matrix
	my $type='npc';
	if ($self->is_vpc){
		$type='vpc';
	}

	my $no_of_strata = 1;
	my $strat_hash;
	my $strat_array;
	my $strata_matrix;
	my @strata_labels=();
	my $no_observations = $self->n_observations;
	my @all_indices= 0 .. ($no_observations-1);

	if (defined $self->stratify_on){

		my $col = $self->stratify_on;

		my $strat_header = $self->stratify_on;
		if ($PsN::nm_major_version < 7){
			#NONMEM only prints first 4 letters of variable name as header
			#no CWRES exception since not allowed stratify on CWRES
			$strat_header = substr($self->stratify_on, 0, 4);
		}
		#add new: If ((defined $self->{dv_table_name }) && (not $self->{'dv} eq 'CWRES'))
		#use dv_table_name instead
		#OR require that stratification and binning variables are in vpc-generated table

		my $orig_file;

		if (defined $self->orig_table){
			$orig_file = $self->orig_table;
		} else {
			$orig_file = $self->original_model->directory().$type.'_original.'.
				$self->original_model-> get_option_value(record_name => 'table',
														 option_name => 'FILE',
														 problem_index => ($self->origprobnum()-1));
		}
		unless (-e $orig_file) {
			croak("Could not find file $orig_file.");
		}
		
		my $d = data->new(filename => $orig_file, ignoresign => '@', idcolumn => 1); #table we made, idcol is 1
		
		my $no_individuals = scalar(@{$d->individuals});
		my $filt = $d -> create_row_filter('no_individuals'=>$no_individuals);
		$strat_array = $d -> column_to_array('column'=>$strat_header,'filter'=>$filt);
		unless (scalar @{$strat_array} > 0){
			croak("Could not find column $strat_header to stratify on in file $orig_file.");
		}
		unless (scalar @{$strat_array} == $no_observations){
			croak("Number of observations in stratification column $strat_header after filtering ".
				  "does not match number of observations in matrix file.");
		}

		$strat_hash = $self -> create_unique_values_hash(data_column=>$strat_array,
														 reference => $self->refstrat());

		my $reference_index=undef;
		if (defined $self->refstrat()){
			for (my $index=0;$index< scalar(keys %{$strat_hash});$index++){
				if ($self->refstrat() == $strat_hash->{$index}){
					$reference_index = $index;
					last;
				}
			}
		}
		#compute strata limits. Currently only no_of_strata based on counts allowed.
		#ok with undefined no_of_strata, will return empty array strata_ceilings, which 
		#index_matrix_binned_values interprets as stratify on unique values.
		#input check ensures that if refstrat is defined then no_of_strata is undefined

		my $strata_ceilings = $self -> get_bin_ceilings_from_count('data_column'=>$strat_array,
																   'value_hash'=>$strat_hash,
																   'data_indices'=>\@all_indices,
																   'n_bins'=>$self->no_of_strata);
		$strata_matrix = $self -> index_matrix_binned_values(data_column=>$strat_array,
															 value_hash=>$strat_hash,
															 reference_index => $reference_index,
															 data_indices=>\@all_indices,
															 bin_ceilings=>$strata_ceilings);

		#create label array for printed report.
		if (defined $self->no_of_strata && $self->no_of_strata < scalar(keys %{$strat_hash})) {
			$no_of_strata = $self->no_of_strata;
			my $low= sprintf "[%g;",${$strat_hash}{0};
			foreach my $high (@{$strata_ceilings}){
				my $lab = sprintf "strata $col %s %g]",$low,$high;
				push (@strata_labels,$lab);
				$low=sprintf "(%g;",$high;
			}
		} else{
			$no_of_strata = scalar (keys %{$strat_hash});
			@strata_labels = ('') x $no_of_strata;
			for (my $strat_ind=0; $strat_ind<$no_of_strata; $strat_ind++){
				my $lab = sprintf "strata $col = %g",${$strat_hash}{$strat_ind};
				if (defined $reference_index){
					if ($strat_ind > $reference_index){
						$strata_labels[$strat_ind]=$lab; #if higher index than ref then keep same place 
					}elsif ($strat_ind < $reference_index){
						$strata_labels[$strat_ind+1]=$lab; #if lower index than ref then move one step to the right
					}else{
						# == ref
						$strata_labels[0]=$lab; #if reference index then put first
					}
				}else{
					$strata_labels[$strat_ind]=$lab;
				}
			}
		}



		if ($self->verbose){
			print "\n strata $no_of_strata\n";
			foreach my $k (keys %{$strat_hash}){
				print "key $k and value ${$strat_hash}{$k}\n";
			}
		}
		$d = undef; #get rid of data object
	} else {
		$strata_matrix = [\@all_indices];
		push (@strata_labels," ");

	}

	
	if ($no_of_strata > 10){
		ui -> print (category=>'npc', message=>"Warning: The number of stratification levels is $no_of_strata");
		ui -> print (category=>'vpc', message=>"Warning: The number of stratification levels is $no_of_strata");
	}

	$self->strata_matrix($strata_matrix);
	$self->strata_labels(\@strata_labels);
	$self->strata_variable_vector($strat_array);

	$self->create_mirror_and_plot_data() if $self->is_vpc; #still have data_matrix

	my @all_strata;
	for (my $strat_ind=0; $strat_ind<$no_of_strata; $strat_ind++){
		my @strata_data;
		foreach my $index (@{$strata_matrix->[$strat_ind]}){
			push (@strata_data,$self->data_matrix->[$index]);
		}
		push(@all_strata,\@strata_data);
	}
	$self->clear_data_matrix;
	$self->stratified_data(\@all_strata);

	if (defined $self->censor_data_matrix){
		my @cens_all_strata;
		for (my $strat_ind=0; $strat_ind<$no_of_strata; $strat_ind++){
			my @strata_data;
			foreach my $index (@{$strata_matrix->[$strat_ind]}){
				push (@strata_data,$self->censor_data_matrix->[$index]);
			}
			push(@cens_all_strata,\@strata_data);
		}
		$self->clear_censor_data_matrix;
		$self->censor_stratified_data(\@cens_all_strata);
	}

}

sub vpc_analyze
{
	my $self = shift;

	#input refs strata_matrix, strata_labels(fix no_of_strata from labels)
	#matrix (fix no_observations and no_sim from matrix)
	
	my $no_sim= $self->n_simulations;
	my $no_observations = $self->n_observations;
	my $no_of_strata = scalar(@{$self->strata_labels});
	my $col = $self -> idv;
	my $NA = -99;
	my $c_i=$self->confidence_interval();
	my $warn_about_missing_data=0;

	$self->create_binned_data();
	#check here that same n-bins for all strata
	my $ref_n_bins = 0;
	if (defined $self->refstrat()){
		$ref_n_bins = scalar(@{$self->binned_data->[0]});
		for (my $strat_ind=1; $strat_ind<$no_of_strata; $strat_ind++){
			unless ($ref_n_bins == scalar(@{$self->binned_data->[$strat_ind]})){
				print "\n ERROR: cannot use refstrat option unless equal number of bins in all strata. Continuing analysis with refstrat undefined\n\n";
				$self->clear_refstrat;
				last;
			}
		}  
	}
	my @pred_int = sort {$a <=> $b} 0,40,80,90,95;
	if ($self->fine_pi()){
		@pred_int = sort {$a <=> $b} 0,10,20,30,40,50,60,70,80,90,95;
	}
	my $meantext = 'mean';
	my $deltameantext = 'delta-mean';

	my @perc_limit;
	foreach my $pi (@pred_int){
		if ($pi == 0){
			push (@perc_limit,$meantext);
			push (@perc_limit,$deltameantext) if (defined $self->refstrat());
			push (@perc_limit,50); #need to have median last of these three for order in diagnostics output
		}else {
			push (@perc_limit,(100-$pi)/2);
			push (@perc_limit,(100-(100-$pi)/2));
		}
	}

	#ok to create reference arrays even if not using refstrat
	my @reference_mean_limit = (0) x  $ref_n_bins; 
	my @reference_mean_limit_real = (0) x  $ref_n_bins;
	my @reference_mean_limit_singlesim;
	for (my $i=0; $i<$ref_n_bins; $i++){
		@{$reference_mean_limit_singlesim[$i]} = (0) x ($no_sim);
	}

	my $no_perc_limits = scalar(@perc_limit);
	my @limit = (0) x $no_perc_limits;
	my @lower_limit_ci = (0) x $no_perc_limits;
	my @upper_limit_ci = (0) x $no_perc_limits;
	my @limit_real = (0) x $no_perc_limits;
	my @limit_index= (0) x $no_perc_limits;
	my @limit_singlesim;
	for (my $i=0; $i<$no_perc_limits; $i++){
		@{$limit_singlesim[$i]} = (0) x ($no_sim);
	}
	my @limit_mirrors;
	if (defined $self->mirrors){
		for (my $i=0; $i<$no_perc_limits; $i++){
			@{$limit_mirrors[$i]} = (0) x ($self->mirrors);
		}
	}


	## Prepare general run info for output file, and bin labels
	my %return_section;
	$return_section{'name'} = 'VPC run info';
	my $modelname= (defined $self->orig_table)?'unknown' : $self->models->[0]->filename();
	my $extra_value='auto-generated';
	if (defined $self->sim_model()){
		my $fil = $self->sim_model();
		if ( $fil =~ /\/$/ ){
			$fil =~ s/\/$//;
		}elsif ( $fil =~ /\\$/ ){ 
			$fil =~ s/\\$//;
		}
		my ($file_volume,$file_directory, $file_file) = File::Spec -> splitpath( $fil);
		$extra_value=$file_file;
	} elsif ($self->flip_comments()){
		$extra_value="flip comments $modelname";
	}

	my @run_info_labels=('Date','observations','simulations','Modelfile','Simulation model',
						 'Independent variable','Dependent variable','PsN version','NONMEM version');


	my @datearr=localtime;
	my $the_date=($datearr[5]+1900).'-'.($datearr[4]+1).'-'.($datearr[3]);
	
	my @run_info_values =($the_date,$no_observations,$no_sim,$modelname,$extra_value,
						  $self->idv,$self->dv, 'v' . $PsN::version, $self->nm_version);

	$return_section{'labels'} =[[],\@run_info_labels];
	$return_section{'values'} = [\@run_info_values];
	push( @{$self->results->[0]{'own'}}, \%return_section );


	my %additional_sec;
	my @additional_row;
	my @additional_val;
	my @empty_row=();

	if ($self->predcorr){
		my @col_lab=('Prediction correction');
		push(@additional_row,'Additional feature',' ');
		push (@additional_val,\@empty_row,\@col_lab);
	}
	if ($self->varcorr){
		my @col_lab=('Variability correction');
		push(@additional_row,'Additional feature',' ');
		push (@additional_val,\@empty_row,\@col_lab);
	}
	if ($self->detection_censored or (defined $self->censor())){
		my @col_lab = ('Censored data',' ');
		my @tmp1 = ('LLOQ','ULOQ','MISSING');
		my @tmp2;
		if (defined $self->lloq){
			push (@tmp2,$self->lloq);
		}else{
			push (@tmp2,'');
		}
		if (defined $self->uloq){
			push (@tmp2,$self->uloq);
		}else{
			push (@tmp2,'');
		}
		#comment out if xpose trouble
		if (defined $self->censor()){
			push (@tmp2,$self->censor());
		}else{
			push (@tmp2,'');
		}
		push(@additional_row,'Additional feature',' ',' ',' ');
		push (@additional_val,\@empty_row,\@col_lab,\@tmp1,\@tmp2);
	}
	if ($self->categorized){
		my @col_lab=('Categorization');
		my @tmp1;
		my @tmp2;
		my $bound_count=1;
		foreach my $bound (@{$self->levels}){
			push (@col_lab,' ') if ($bound_count > 1);
			push (@tmp1,'Boundary '.$bound_count);
			push (@tmp2,$bound);
			$bound_count=$bound_count+1;
		}
		push(@additional_row,'Additional feature',' ',' ',' ');
		push (@additional_val,\@empty_row,\@col_lab,\@tmp1,\@tmp2);
	}
	

	if (scalar(@additional_row)>0){
		$additional_sec{'labels'} =[\@additional_row, []];
		$additional_sec{'values'} = \@additional_val;
		push( @{$self->results->[0]{'own'}},\%additional_sec );
	}



	my @result_column_labels=("< $col",'<=','no. of obs');
	for (my $i=0; $i<$no_perc_limits; $i++){
		my $text = "$perc_limit[$i]\%";
		if ($perc_limit[$i] eq $meantext){
			$text = $meantext;
		}
		if ($perc_limit[$i] eq $deltameantext){
			$text = $deltameantext;
		}
		push (@result_column_labels,"$text real");
		foreach my $lab (@{$self->mirror_labels}){
			push (@result_column_labels,"$text ".$lab);
		}
		push (@result_column_labels,"$text sim","$c_i\%CI for $text from",
			  "$c_i\%CI for $text to");
	}

	#startof censored labels
	my @censored_result_column_labels=("< $col",'<=','uncensored obs','Real left censored');
	foreach my $lab (@{$self->mirror_labels}){
		push (@censored_result_column_labels, $lab." left censored");
	}
	push (@censored_result_column_labels,'Simulated left censored',
		  $self->confidence_interval().'% CI for left censored from',
		  $self->confidence_interval().'% CI for left censored to',
		  'Real right censored');
	foreach my $lab (@{$self->mirror_labels}){
		push (@censored_result_column_labels, $lab." right censored");
	}
	push (@censored_result_column_labels,'Simulated right censored',
		  $self->confidence_interval().'% CI for right censored from',
		  $self->confidence_interval().'% CI for right censored to',
		  'Real missing');

	foreach my $lab (@{$self->mirror_labels}){
		push (@censored_result_column_labels, $lab." missing");
	}
	push (@censored_result_column_labels,'Simulated missing',
		  $self->confidence_interval().'% CI for missing from',
		  $self->confidence_interval().'% CI for missing to');
	#endof censored labels


	#startof categorized labels
	my @categorized_result_column_labels=("< $col",'<=','no. of obs');
	
	my $lower_bound='';
	foreach my $bound (@{$self->levels}){
		my $bound_label= $lower_bound.'<= '.$bound;
		$lower_bound='>'.$bound.'; ';
		push (@categorized_result_column_labels,'Real '.$bound_label);

		foreach my $lab (@{$self->mirror_labels}){
			push (@categorized_result_column_labels, $lab." ".$bound_label);
		}
		push (@categorized_result_column_labels,'Sim '.$bound_label,
			  $self->confidence_interval().'% CI for '.$bound_label.' from', 
			  $self->confidence_interval().'% CI for '.$bound_label.' to');
	}
	push (@categorized_result_column_labels,'Real '.$lower_bound);
    foreach my $lab (@{$self->mirror_labels}){
		push (@categorized_result_column_labels, $lab." ".$lower_bound);
    }
	push (@categorized_result_column_labels,'Sim '.$lower_bound,
		  $self->confidence_interval().'% CI for '.$lower_bound.' from',
		  $self->confidence_interval().'% CI for '.$lower_bound.' to');

	#endof categorized labels

	#For VPC diagnostics

	my $npc_pi_offset=0;
	my $npc_pi_skip=0;
	foreach my $pi (@pred_int){
		if ($pi == 0){
			$npc_pi_offset=1; #one for median
			$npc_pi_skip=1; #skip mean
			$npc_pi_skip=2 if (defined $self->refstrat()); #skip mean and deltamean
		}else {
			push (@result_column_labels,"PI $pi\% False pos (\%)","PI $pi\% False neg (\%)");
		}
	}


	my ($npc_result_column_labels,$npc_result_row_labels);
	($npc_result_column_labels,$npc_result_row_labels) = 
		$self->get_npc_result_labels('ci' => $c_i,'pred_intervals' => \@pred_int);
	my @npc_result_labels = ($npc_result_row_labels,$npc_result_column_labels);
#    my @npc_section_array; have insdie strata loop

	#call get_npc_indices
	#with dropout npc indices will be different for each bin,
	#will be recomputed inside npc
	#if there are bins where all obs for a dataset censored. Likely if dropout.
	
	my ($npc_lower_index,$npc_upper_index,$npc_low_ind,$npc_high_ind);
	($npc_lower_index,$npc_upper_index,$npc_low_ind,$npc_high_ind)= 
		$self->get_npc_indices('ci' => $c_i,
							   'no_sim' => $no_sim,
							   'pred_intervals' => \@pred_int);
	
	#endof prep VPC diagnostics

	##done general run info and labels

	#Loop over strata.

	for (my $strat_ind=0; $strat_ind<$no_of_strata; $strat_ind++){
		#report strata header, use labels...
		my @result_row_labels = ('first interval is closed');
		my %return_section; #continuous
		my @result_values=();

		my @censored_result_row_labels = ('first interval is closed');
		my %censored_return_section;
		my @censored_result_values=();

		my @categorized_result_row_labels = ('first interval is closed');
		my %categorized_return_section;
		my @categorized_result_values=();

		my $no_strata_obs=scalar(@{$self->strata_matrix->[$strat_ind]});
		my $result_name;
		$result_name = "\nVPC results ".$self->strata_labels->[$strat_ind].
			"\nContinuous data".
			"\n$no_strata_obs observations out of $no_observations";

		$return_section{'name'} = $result_name;

		my $censored_result_name = "\nVPC results ".$self->strata_labels->[$strat_ind].
			"\nCensored data".
			"\n$no_strata_obs observations out of $no_observations";
		$censored_return_section{'name'} = $censored_result_name;

		my $categorized_result_name = "\nVPC results ".$self->strata_labels->[$strat_ind].
			"\nCategorized data".
			"\n$no_strata_obs observations out of $no_observations";
		$categorized_return_section{'name'} = $categorized_result_name;

		my @npc_section_array; #possibly move out again
		my @missing_section_array; #possibly move out again
		my @censored_npc_section_array; 

		my @sum_falsepos= (0) x scalar(@pred_int);
		my @sum_falseneg= (0) x scalar(@pred_int);
		my $sum_no_obs=0;
		my $no_bins = scalar(@{$self->binned_data->[$strat_ind]});

		for (my $bin_index=0;$bin_index<$no_bins;$bin_index++){
			#Loop over bins. Check URS

			my $n_merged_censored_sim=0; 
			my @merged_censored_sim=();
			my $n_censored_sim=0;
			my @merged_uncensored_simvalues=();# @uncensored_simvalues
			my @merged_censor_values_sim=(); #@censor_simvalues
			my $orig_value;
			my $orig_cens;
			my @tmp=();
			my @censor=();
			my @singleset=();
			my @censored_real=(); #@singleset
			my @uncensored_real=(); #@singleset
			my $n_non_missing_real=0;
			my @n_non_missing_sim= (0) x $no_sim;
			my $max_bin_observations=0;

			#variables for censored
			my $real_count_below_lloq=0;
			my $real_count_above_uloq=0;
			my $real_count_missing=0;
			my @sim_count_below_lloq=(0) x $no_sim;
			my @sim_count_above_uloq=(0) x $no_sim;
			my @sim_count_missing=(0) x $no_sim;
			my @mirror_count_below_lloq = (0) x $self->mirrors;
			my @mirror_count_above_uloq = (0) x $self->mirrors;
			my @mirror_count_missing = (0) x $self->mirrors;
			my $median_fraction_sim_below_lloq;
			my $median_fraction_sim_above_uloq;      
			my $median_fraction_sim_missing;      
			my $lloq_ci_from=0;
			my $lloq_ci_to=0;
			my $uloq_ci_from=0;
			my $uloq_ci_to=0;
			my $missing_ci_from=0;
			my $missing_ci_to=0;
			my @npc_display_after_censoring;
			#endof varibles for censored

			#variables for categorized
			my $no_categories = 0;
			$no_categories = (scalar(@{$self->levels})+1) if $self->categorized; 
			my @real_category_count = (0) x $no_categories;
			my @mirror_category_count;
			my @sim_category_count;
			for (my $i=0; $i<$no_categories; $i++){
				push(@mirror_category_count,[(0) x $self->mirrors]);
				push(@sim_category_count,[(0) x $no_sim]);
			}
			#endof variables for categorized

			#a) pool all simulated values in bin
			#sort simvalues, compute indices for this particular $n_merged_censored_sim;
			#and save perclimit
			#for censored data: count observation below lloq, above uloq separately for each simulation
			my $row_index = 0;
			foreach my $row  (@{$self->binned_data->[$strat_ind]->[$bin_index]}){
				($orig_value,@tmp) = split(/,/,$row); 
				if (defined $self->censor_binned_data){
					($orig_cens,@censor) = 
						split(/,/,$self->censor_binned_data->[$strat_ind]->[$bin_index]->[$row_index]);
					$row_index++;
					if ($orig_cens != 1){
						$n_non_missing_real++;
					}
					#if predcorr then both missing and lloq/uloq should be removed
					#if not predcorr then only missing should be removed
					if ($self->predcorr){
						if ($orig_cens == 0){
							push(@censored_real,$orig_value); 
						}
					}elsif ($orig_cens != 1){
						push(@censored_real,$orig_value); 
					}
					for (my $j=0; $j< $no_sim; $j++){
						if ($censor[$j] != 1){
							$n_non_missing_sim[$j] = $n_non_missing_sim[$j]+1;
						}
						if ($self->predcorr){
							if ($censor[$j] == 0){
								push(@merged_censored_sim,$tmp[$j]) ;
								$n_merged_censored_sim++;
							}
						}elsif ($censor[$j] != 1){
							push(@merged_censored_sim,$tmp[$j]) ;
							$n_merged_censored_sim++;
						}
					}
					push(@merged_uncensored_simvalues,@tmp);
					push(@merged_censor_values_sim,@censor);
				}else{
					push(@merged_censored_sim,@tmp);
					push(@censored_real,$orig_value);
					$n_merged_censored_sim += $no_sim;
					$n_non_missing_real++;
					for (my $j=0; $j< $no_sim; $j++){
						$n_non_missing_sim[$j] = $n_non_missing_sim[$j]+1;
					}
				}
				push(@uncensored_real,$orig_value);
				$max_bin_observations++;

				#startof censored
				if (defined $self->lloq){
					$real_count_below_lloq++ if ($orig_cens == 2);
					for (my $ii=0; $ii< $no_sim; $ii++){
						$sim_count_below_lloq[$ii] = ($sim_count_below_lloq[$ii]+1)  if ($censor[$ii] == 2);
					}
					my $ii=0;
					foreach my $mi (@{$self->mirror_set}){
						$mirror_count_below_lloq[$ii] = ($mirror_count_below_lloq[$ii]+1)  if ($censor[$mi] == 2);
						$ii++;
					}
				}
				if (defined $self->uloq){
					$real_count_above_uloq++  if ($orig_cens == 3);
					for (my $ii=0; $ii< $no_sim; $ii++){
						$sim_count_above_uloq[$ii] = ($sim_count_above_uloq[$ii]+1)  if ($censor[$ii] == 3);
					}
					my $ii=0;
					foreach my $mi (@{$self->mirror_set}){
						$mirror_count_above_uloq[$ii] = ($mirror_count_above_uloq[$ii]+1)  if ($censor[$mi] == 3);
						$ii++;
					}
				}
				if (defined $self->censor()){
					$real_count_missing++  if ($orig_cens == 1);
					for (my $ii=0; $ii< $no_sim; $ii++){
						$sim_count_missing[$ii] = ($sim_count_missing[$ii]+1)  if ($censor[$ii] == 1);
					}
					my $ii=0;
					foreach my $mi (@{$self->mirror_set}){
						$mirror_count_missing[$ii] = ($mirror_count_missing[$ii]+1)  if ($censor[$mi] == 1);
						$ii++;
					}
				}

				#endof censored
				#startof categorized. Only count if censor != 1
				if ($self->categorized){
					for (my $categ=0; $categ < scalar(@{$self->levels}); $categ++){
						last if ($orig_cens == 1); #count nothing if dropout
						my $lev=$self->levels->[$categ];
						if ($orig_value <= $lev){
							$real_category_count[$categ]=$real_category_count[$categ]+1;
							last;
						}
						if (($categ+1) == scalar(@{$self->levels})){ #last category
							$real_category_count[($categ+1)]=$real_category_count[($categ+1)]+1;
						}
					}
					foreach (my $j=0; $j<$no_sim;$j++){
						next if ( $censor[$j]== 1); #count nothing if dropout
						my $val=$tmp[$j];
						for (my $categ=0;$categ<scalar(@{$self->levels});$categ++){
							my $lev=$self->levels->[$categ];
							if ($val <= $lev){
								$sim_category_count[$categ]->[$j]=$sim_category_count[$categ]->[$j]+1;
								last;
							}
							if (($categ+1) == scalar(@{$self->levels})){ #last category
								$sim_category_count[($categ+1)]->[$j]=$sim_category_count[($categ+1)]->[$j]+1;
							}
						}
					}
					foreach (my $j=0; $j<$self->mirrors;$j++){
						next if ( $censor[($self->mirror_set->[$j])]== 1); #count nothing if dropout
						my $val=$tmp[($self->mirror_set->[$j])];
						for (my $categ=0;$categ<scalar(@{$self->levels});$categ++){
							my $lev=$self->levels->[$categ];
							if ($val <= $lev){
								$mirror_category_count[$categ]->[$j]=$mirror_category_count[$categ]->[$j]+1;
								last;
							}
							if (($categ+1) == scalar(@{$self->levels})){ #last category
								$mirror_category_count[($categ+1)]->[$j]=$mirror_category_count[($categ+1)]->[$j]+1;
							}
						}
					}
				}
				#endof categorized

			} 
			next if ($max_bin_observations == 0);

			my $nonzero_count=0;
			for (my $j=0; $j<$no_sim; $j++){
				unless ($n_non_missing_sim[$j] == 0){
					$nonzero_count++;
				}
			}
			next if (($nonzero_count == 0) and ($n_non_missing_real == 0));
			my $low_ci_ind = $self->round('number'=>((100-$c_i)*($nonzero_count-1)/200));
#    my $low_ci_ind = int(((100-$c_i)*($no_sim-1)/200)+0.5); #index of start of ci % interval
			my $high_ci_ind = $nonzero_count - $low_ci_ind - 1; #index of end  of  c_i% interval
			
			#if defined $self->censor()
			#add loop over n_non_missing_real, n_non_missing_sim, warn if any below 10

			#for censored data
			my $st_cens= ' ';
			$st_cens = $self->bin_floors->[$strat_ind]->[$bin_index] 
				if (defined $self->bin_floors->[$strat_ind] );

			my @censored_result_row_values=($st_cens,$self->bin_ceilings->[$strat_ind]->[$bin_index],$max_bin_observations);
			if (defined $self->lloq){
				#$fraction_real_below_lloq
				if ($n_non_missing_real == 0){
					push(@censored_result_row_values,'');
				}else{
					push(@censored_result_row_values,$real_count_below_lloq/$n_non_missing_real);
				}
				my @sim_frac_censored;
				for (my $j=0; $j<$no_sim; $j++){
					unless ($n_non_missing_sim[$j] == 0){
						push(@sim_frac_censored,$sim_count_below_lloq[$j]/$n_non_missing_sim[$j]);
					}
				}
				if (scalar(@sim_frac_censored) == 0){
					foreach my $mi (@{$self->mirror_set}){
						push(@censored_result_row_values,'');
					}
					push(@censored_result_row_values,'','','');
				}else{
					my @sorted_sim_frac = sort {$a <=> $b} @sim_frac_censored;
					$lloq_ci_from=$sorted_sim_frac[$low_ci_ind];
					$lloq_ci_to=$sorted_sim_frac[$high_ci_ind];
					
					$median_fraction_sim_below_lloq = 
						($self->median('sorted_array' => \@sorted_sim_frac));
					
					my $ii = 0;
					foreach my $mi (@{$self->mirror_set}){
						if ($n_non_missing_sim[$mi] == 0){
							push(@censored_result_row_values,'');
						}else{
							push(@censored_result_row_values,($mirror_count_below_lloq[$ii]/$n_non_missing_sim[$mi]));
						}
						$ii++;
					}
					push(@censored_result_row_values,$median_fraction_sim_below_lloq,
						 $lloq_ci_from,$lloq_ci_to);
				}
			}else{
#	push(@censored_result_row_values,'NA','NA','NA','NA');
				push(@censored_result_row_values,'','','','');
				foreach my $ii (1..($self->mirrors)){
#	  push(@censored_result_row_values,'NA');
					push(@censored_result_row_values,'');
				}
			}
			if (defined $self->uloq){
				#$fraction_real_above_uloq
				if ($n_non_missing_real == 0){
					push(@censored_result_row_values,'');
				}else{
					push(@censored_result_row_values,$real_count_above_uloq/$n_non_missing_real);
				}

				my @sim_frac_censored;
				for (my $j=0; $j<$no_sim; $j++){
					unless ($n_non_missing_sim[$j] == 0){
						push(@sim_frac_censored,$sim_count_above_uloq[$j]/$n_non_missing_sim[$j]);
					}
				}
				if (scalar(@sim_frac_censored) == 0){
					foreach my $mi (@{$self->mirror_set}){
						push(@censored_result_row_values,'');
					}
					push(@censored_result_row_values,'','','');
				}else{
					my @sorted_sim_frac = sort {$a <=> $b} @sim_frac_censored;
					$uloq_ci_from=$sorted_sim_frac[$low_ci_ind];
					$uloq_ci_to=$sorted_sim_frac[$high_ci_ind];
					$median_fraction_sim_above_uloq = 
						($self->median('sorted_array' => \@sorted_sim_frac));
					
					my $ii = 0;
					foreach my $mi (@{$self->mirror_set}){
						if ($n_non_missing_sim[$mi] == 0){
							push(@censored_result_row_values,'');
						}else{
							push(@censored_result_row_values,($mirror_count_above_uloq[$ii]/$n_non_missing_sim[$mi]));
						}
						$ii++;
					}
					push(@censored_result_row_values,$median_fraction_sim_above_uloq,
						 $uloq_ci_from,$uloq_ci_to);
				}
			}else{
				push(@censored_result_row_values,'','','','');
				foreach my $ii (1..($self->mirrors)){

					push(@censored_result_row_values,'');
				}
			}

			if (defined $self->censor() and ($max_bin_observations > 0)){
				#$fraction_real_missing
				push(@censored_result_row_values,$real_count_missing/$max_bin_observations);

				my @sim_frac_censored;
				for (my $j=0; $j<$no_sim; $j++){
					push(@sim_frac_censored,$sim_count_missing[$j]/$max_bin_observations);
				}
				my @sorted_sim_frac = sort {$a <=> $b} @sim_frac_censored;
				$missing_ci_from=$sorted_sim_frac[$low_ci_ind];
				$missing_ci_to=$sorted_sim_frac[$high_ci_ind];
				$median_fraction_sim_missing = 
					($self->median('sorted_array' => \@sorted_sim_frac));
				
				my $ii = 0;
				foreach my $mi (@{$self->mirror_set}){
					push(@censored_result_row_values,
						 ($mirror_count_missing[$ii]/$max_bin_observations));
					$ii++;
				}
				push(@censored_result_row_values,$median_fraction_sim_missing,
					 $missing_ci_from,$missing_ci_to);
			}else{
				push(@censored_result_row_values,'','','','');
				foreach my $ii (1..($self->mirrors)){
					push(@censored_result_row_values,'');
				}
			}

			push (@censored_result_values,\@censored_result_row_values);
			#endof censored data
			
			#categorized
			my @categorized_result_row_values=
				($st_cens,$self->bin_ceilings->[$strat_ind]->[$bin_index],$max_bin_observations);
			if ($self->categorized){
				foreach my $categ (0..($no_categories-1)){
					#$fraction_real
					if ($n_non_missing_real == 0){
						push(@categorized_result_row_values,'');
					}else{
						push(@categorized_result_row_values,($real_category_count[$categ]/$n_non_missing_real));
					}

					my $ii = 0;
					foreach my $mi (@{$self->mirror_set}){
						if ($n_non_missing_sim[$mi] == 0){
							push(@categorized_result_row_values,'');
						}else{
							push(@categorized_result_row_values,
								 ($mirror_category_count[$categ]->[$ii]/$n_non_missing_sim[$mi]));
						}
						$ii++;
					}
					my @sim_frac_censored;
					for (my $j=0; $j<$no_sim; $j++){
						unless ($n_non_missing_sim[$j] == 0){
							push(@sim_frac_censored,$sim_category_count[$categ]->[$j]/$n_non_missing_sim[$j]);
						}
					}
					if (scalar(@sim_frac_censored) == 0){
						push(@categorized_result_row_values,'','','');
					}else{
						my @sorted_sim_frac = sort {$a <=> $b} @sim_frac_censored;
						my $median_count_fraction=($self->median('sorted_array' => \@sorted_sim_frac));
						push(@categorized_result_row_values,$median_count_fraction,
							 $sorted_sim_frac[$low_ci_ind],
							 $sorted_sim_frac[$high_ci_ind]);
					}
				}
			}
			push (@categorized_result_values,\@categorized_result_row_values);
			#endof categorized

			if ($n_merged_censored_sim == 0){
				for (my $i=0; $i<$no_perc_limits; $i++){
					$limit[$i]= undef;
					#must check everywhere if $limit[ defined 
				}
			}else{
				my @sorted_sim_values = sort {$a <=> $b} @merged_censored_sim;
				for (my $i=0; $i<$no_perc_limits; $i++){
					if ($perc_limit[$i] eq $meantext){
						$limit[$i]= $self->mean(array => \@sorted_sim_values);
					}elsif ($perc_limit[$i] eq $deltameantext){
						if ($strat_ind == 0){
							#reference stratum
							$limit[$i]= 0;
							$reference_mean_limit[$bin_index] = $self->mean(array => \@sorted_sim_values); 
						}else{
							$limit[$i]= $self->mean(array => \@sorted_sim_values) - $reference_mean_limit[$bin_index];
						}
					}elsif ($perc_limit[$i]==50){
						#take median
						$limit[$i]= $self->median('sorted_array' => \@sorted_sim_values);
					}else{
						$limit_index[$i] = $self->round('number'=>$perc_limit[$i]*($n_merged_censored_sim-1)/100);
						$limit[$i]=$sorted_sim_values[$limit_index[$i]];
					}
				}	   
			} 

			##VPC diagnostics. Before b,c) because need singleset of real values
			
			#subset npc must be done on same set as below: take out lloq/uloq if varcorr, 
			#otherwise only missing. lower and upper index depend on this choice, recomputed
			#must also recompute low_ind high_ind inside npc?
			my ($npc_result_values,$npc_realpos,$npc_stats_warnings);
			($npc_result_values,$npc_realpos,$npc_stats_warnings)=
				$self->subset_npc_analyze('bin_index' => $bin_index,
										  'strata_index' => $strat_ind,
										  'low_ind' => $npc_low_ind,
										  'high_ind' => $npc_high_ind,
										  'lower_index' => $npc_lower_index,
										  'upper_index' => $npc_upper_index, 
										  'pred_intervals' => \@pred_int);
			#realpos has one arrayref per PI, each array one integer per obs. -1 below, 0 in, 1 above
			#-99 for missing
			
			my @diagnostics=();
			#the offset is to skip interval 50-50 which is of course empty
			#what about mean and deltamean???
			for (my $i=$npc_pi_offset; $i<scalar(@pred_int); $i++){ #skip 50-50 interval in pred_int
				my $false_pos=0;
				my $false_neg=0;
				for (my $j=0; $j<$max_bin_observations; $j++){
					if ($npc_realpos->[$i]->[$j] == 0){
						#NPC inside
						if (((defined $limit[($i*2-$npc_pi_offset+$npc_pi_skip)]) and 
							 ($uncensored_real[$j] < $limit[($i*2-$npc_pi_offset+$npc_pi_skip)])) ||
							((defined $limit[($i*2-$npc_pi_offset+$npc_pi_skip+1)]) and 
							 ($uncensored_real[$j] > $limit[($i*2-$npc_pi_offset+$npc_pi_skip+1)]))){
							#VPC outside
							$false_pos++;
						}
					} elsif (($npc_realpos->[$i]->[$j] == -1) || ($npc_realpos->[$i]->[$j] == 1)){
						#NPC outside
						if (((defined $limit[($i*2-$npc_pi_offset+$npc_pi_skip)]) and 
							 ($uncensored_real[$j] >= $limit[($i*2-$npc_pi_offset+$npc_pi_skip)])) &&
							((defined $limit[($i*2-$npc_pi_offset+$npc_pi_skip+1)]) and 
							 ($uncensored_real[$j] <= $limit[($i*2-$npc_pi_offset+$npc_pi_skip+1)]))){
							#VPC inside
							$false_neg++;
						}
					}
				}
				if (scalar(@censored_real) != 0){
					push (@diagnostics,($false_pos*100/scalar(@censored_real),$false_neg*100/scalar(@censored_real)));
				}else{
					push (@diagnostics,('',''));
				}
				$sum_falsepos[$i] += $false_pos;
				$sum_falseneg[$i] += $false_neg;
			}
			$sum_no_obs += $n_non_missing_real;

			my %npc_return_section;
			my $obsstring;
			$obsstring = $max_bin_observations." observations";
			$npc_return_section{'name'} = "\nNPC results ".$self->strata_labels->[$strat_ind].
				"\nbin ".($bin_index+1).": ".$obsstring;
			$npc_return_section{'labels'} = \@npc_result_labels;
			$npc_return_section{'values'} = $npc_result_values;
			push( @npc_section_array,\%npc_return_section );


			my %missing_return_section;
			$missing_return_section{'name'} = "\nMissing data warnings ".$self->strata_labels->[$strat_ind].
				"\nbin ".($bin_index+1).": ".$obsstring." before censoring";
			my @missing_result_labels = (['Non-missing observations'],['Real','Minimum in sim','N sim with =0',
																	   'N sim with <10','Median in sim']);
			$missing_return_section{'labels'} = \@missing_result_labels;
			my $all_missing_sim=0;
			my $less_than_10=0;
			my @sorted_nonmiss = sort {$a <=> $b} @n_non_missing_sim;
			my $median_nonmiss = $self->median('sorted_array' => \@sorted_nonmiss);

			for (my $j=0; $j<$no_sim; $j++){
				if ($n_non_missing_sim[$j] == 0){
					$all_missing_sim++;
					$less_than_10++;
				}elsif ($n_non_missing_sim[$j]< 10 ){
					$less_than_10++;
				}
			}

			my @missing_result_values = ([$n_non_missing_real,$sorted_nonmiss[0],$all_missing_sim,
										  $less_than_10,$median_nonmiss]);
			$missing_return_section{'values'} = \@missing_result_values;
			if (($less_than_10>0) or ($n_non_missing_real<10)){
				push( @missing_section_array,\%missing_return_section ) ;
				$warn_about_missing_data=1;
			}
			#endof VPC diagnostics (print below)


			#b) compute PI:s for original, sort values and save lower and upper limits

			if (scalar(@censored_real) == 0){
				for (my $i=0; $i<$no_perc_limits; $i++){
					$limit_real[$i] = undef; 
				}
			}else{
				my @sorted_singleset = sort {$a <=> $b} @censored_real;

				for (my $i=0; $i<$no_perc_limits; $i++){
					if ($perc_limit[$i] eq $meantext){
						$limit_real[$i] = $self->mean(array => \@sorted_singleset);
					}elsif ($perc_limit[$i] eq $deltameantext){
						if ($strat_ind == 0){
							$reference_mean_limit_real[$bin_index] = $self->mean(array => \@sorted_singleset);
							$limit_real[$i] = 0;
						}else{
							$limit_real[$i] = $self->mean(array => \@sorted_singleset) - $reference_mean_limit_real[$bin_index]; 
						}
					}elsif ($perc_limit[$i]==50){
						$limit_real[$i] = $self->median('sorted_array' => \@sorted_singleset);
					}else{
						my $index = $self->round('number'=>($perc_limit[$i]*(scalar(@censored_real)-1)/100));
						$limit_real[$i] = $sorted_singleset[$index];
					}
				}
			}

			#c) compute PI:s for each simset. Use clever indexing in @merged_uncensored_simvalues to extract values.
			#then determine and save ci:s for each PI boundary	    
			#recompute low_ci_ind after possibly stricter censoring


			my $uncensored_count=0;
			for (my $col=0;$col<$no_sim; $col++){
				@singleset=();
				for (my $row=0; $row<$max_bin_observations; $row++){
					if (defined $self->censor() || $self->detection_censored){
						if ($self->predcorr){
							push(@singleset,$merged_uncensored_simvalues[$col+$row*$no_sim])
								if ($merged_censor_values_sim[$col+$row*$no_sim] == 0);
						}else{
							push(@singleset,$merged_uncensored_simvalues[$col+$row*$no_sim])
								if ($merged_censor_values_sim[$col+$row*$no_sim] != 1);
						}
					}else{
						push(@singleset,$merged_censored_sim[$col+$row*$no_sim]);
					}
				}
				if (scalar(@singleset) == 0){
					for (my $i=0; $i<$no_perc_limits; $i++){
						$limit_singlesim[$i]->[$col] = undef;
					}
				}else{
					$uncensored_count++;
					my @sorted_singleset = sort {$a <=> $b} @singleset;
					for (my $i=0; $i<$no_perc_limits; $i++){
						if ($perc_limit[$i] eq $meantext){
							$limit_singlesim[$i]->[$col]  = $self->mean(array => \@sorted_singleset);
						}elsif ($perc_limit[$i] eq $deltameantext){
							if ($strat_ind == 0){
								$reference_mean_limit_singlesim[$bin_index]->[$col]  = $self->mean(array => \@sorted_singleset);
								$limit_singlesim[$i]->[$col]  = 0;
							}else{
								$limit_singlesim[$i]->[$col]  = $self->mean(array => \@sorted_singleset) - $reference_mean_limit_singlesim[$bin_index]->[$col] ;
							}
						}elsif ($perc_limit[$i]==50){
							$limit_singlesim[$i]->[$col]  = $self->median('sorted_array' => \@sorted_singleset);
						}else{
							my $index = $self->round('number'=>($perc_limit[$i]*(scalar(@singleset)-1)/100));
							$limit_singlesim[$i]->[$col] = $sorted_singleset[$index];
						}
					}
				}
			}

			#for mirror plots
			if (defined $self->mirrors){
				for my $mi (0 .. ($self->mirrors-1)){
					my $col = $self->mirror_set->[$mi];
					for (my $i=0; $i<$no_perc_limits; $i++){
						$limit_mirrors[$i]->[$mi]=$limit_singlesim[$i]->[$col];
					}
					$mi++;
				}
			}
			my $low_ci_ind = $self->round('number'=>((100-$c_i)*($uncensored_count-1)/200));
			my $high_ci_ind = $uncensored_count - $low_ci_ind - 1; #index of end  of  c_i% interval

			for (my $i=0; $i<$no_perc_limits; $i++){
				#sort only def values of limit
				my @remaining=();
				foreach my $lim (@{$limit_singlesim[$i]}){
					push(@remaining,$lim) if (defined $lim);
				}
				if (scalar(@remaining) == 0){
					$lower_limit_ci[$i] = undef;
					$upper_limit_ci[$i] = undef;		
				}else{
					my @val_arr = sort {$a <=> $b} @remaining;
					$lower_limit_ci[$i] = $val_arr[$low_ci_ind];
					$upper_limit_ci[$i] = $val_arr[$high_ci_ind];		
				}
			}

			my $st= ' ';
			$st = $self->bin_floors->[$strat_ind]->[$bin_index] 
				if (defined $self->bin_floors->[$strat_ind] );
			my @result_row_values=($st,$self->bin_ceilings->[$strat_ind]->[$bin_index],$max_bin_observations);


			#separate loop for censored data, otherwise too messy.

			if ($self->detection_censored and (not $self->predcorr)){
				for (my $i=0; $i<$no_perc_limits; $i++){
					my $nn = $limit_real[$i];
					$nn = '' unless (defined $nn);
					if ((defined $self->lloq) && (defined $nn && $self->lloq > $nn)){
						$nn='';
					} elsif ((defined $self->uloq) && (defined $nn && $self->uloq < $nn)){
						$nn='';
					}
					push (@result_row_values,$nn);

					for my $mi (0 .. ($self->mirrors-1)){
						$nn = $limit_mirrors[$i]->[$mi];
						$nn = '' unless (defined $nn);
						if ((defined $self->lloq) && (defined $nn && $self->lloq > $nn)){
							$nn='';
						} elsif ((defined $self->uloq) && (defined $nn && $self->uloq < $nn)){
							$nn='';
						}
						push (@result_row_values,$nn);
					}
					#Register in array whether below lloq/above uloq, used for censoring NPC section
					if (not (defined  $limit[$i])){
						push (@npc_display_after_censoring,0);
						#push one extra if perclimit is 50
						push (@npc_display_after_censoring,0) 
							if (($perc_limit[$i]==50) or ($perc_limit[$i] eq $meantext) or ($perc_limit[$i] eq $deltameantext));
					}elsif ((defined $self->lloq) && $self->lloq > $limit[$i]){
						push (@npc_display_after_censoring,0);
						#push one extra if perclimit is 50
						push (@npc_display_after_censoring,0)
							if (($perc_limit[$i]==50) or ($perc_limit[$i] eq $meantext) or ($perc_limit[$i] eq $deltameantext));
					} elsif ((defined $self->uloq) && $self->uloq < $limit[$i]){
						push (@npc_display_after_censoring,0);
						push (@npc_display_after_censoring,0)
							if (($perc_limit[$i]==50) or ($perc_limit[$i] eq $meantext) or ($perc_limit[$i] eq $deltameantext));
					}else {
						push (@npc_display_after_censoring,1);
						push (@npc_display_after_censoring,1)
							if (($perc_limit[$i]==50) or ($perc_limit[$i] eq $meantext) or ($perc_limit[$i] eq $deltameantext));
					}
					if (defined $limit[$i]){
						push (@result_row_values,$limit[$i]);
					}else{
						push (@result_row_values,'');
					}
					if (defined $lower_limit_ci[$i]){
						push (@result_row_values,$lower_limit_ci[$i]);
					}else{
						push (@result_row_values,'');
					}
					if (defined $upper_limit_ci[$i]){
						push (@result_row_values,$upper_limit_ci[$i]);
					}else{
						push (@result_row_values,'')
					}
				}

			}else{ #no uloq/lloq, or with predcorr
				for (my $i=0; $i<$no_perc_limits; $i++){
					if (defined $limit_real[$i]){
						push (@result_row_values,$limit_real[$i]);
					}else{
						push (@result_row_values,'');
					}

					for my $mi (0 .. ($self->mirrors-1)){
						if (defined $limit_mirrors[$i]->[$mi]){
							push (@result_row_values,$limit_mirrors[$i]->[$mi]);
						}else{
							push (@result_row_values,'');
						}
					}

					if (defined $limit[$i]){
						push (@result_row_values,$limit[$i]);
					}else{
						push (@result_row_values,'');
					}
					if (defined $lower_limit_ci[$i]){
						push (@result_row_values,$lower_limit_ci[$i]);
					}else{
						push (@result_row_values,'');
					}
					if (defined $upper_limit_ci[$i]){
						push (@result_row_values,$upper_limit_ci[$i]);
					}else{
						push (@result_row_values,'')
					}
				}
			}

			#for censored npc section
			my %censored_npc_return_section;
			$censored_npc_return_section{'name'} = "\nNPC results ".$self->strata_labels->[$strat_ind].
				"\nbin ".($bin_index+1).": ".$max_bin_observations." observations";
			$censored_npc_return_section{'labels'} = \@npc_result_labels;
			my @censored_npc_result_values;
			for (my $i=0; $i<scalar(@pred_int); $i++){

				my @censored_npc_row_values;
				if ($npc_display_after_censoring[($i*2)]){
					push(@censored_npc_row_values,$npc_result_values->[$i]->[0],$npc_result_values->[$i]->[1],
						 $npc_result_values->[$i]->[2]);
				}else{
					push(@censored_npc_row_values,'','','');
				}
				push(@censored_npc_row_values,$npc_result_values->[$i]->[3],$npc_result_values->[$i]->[4]);

				if ($npc_display_after_censoring[($i*2+1)]){
					push(@censored_npc_row_values,$npc_result_values->[$i]->[5],$npc_result_values->[$i]->[6],
						 $npc_result_values->[$i]->[7]);
				}else{
					push(@censored_npc_row_values,'','','');
				}
				push(@censored_npc_row_values,$npc_result_values->[$i]->[8],$npc_result_values->[$i]->[9]);

				push(@censored_npc_result_values,\@censored_npc_row_values);
			}
			$censored_npc_return_section{'values'} = \@censored_npc_result_values;
			push( @censored_npc_section_array,\%censored_npc_return_section );
			#endof censored npc section


			#VPC diagnostics
			unless ($self->detection_censored){ 
				push (@result_row_values,@diagnostics);
			}
			#endof diagnostics
			

			push (@result_values,\@result_row_values);

		} #endof loop over bins

		my @result_labels = (\@result_row_labels,\@result_column_labels);
		$return_section{'labels'} = \@result_labels;
		$return_section{'values'} = \@result_values;
		push( @{$self->results->[0]{'own'}},\%return_section );

		if ($self->detection_censored or (defined $self->censor())){
			my @censored_result_labels = (\@censored_result_row_labels,\@censored_result_column_labels);
			$censored_return_section{'labels'} = \@censored_result_labels;
			$censored_return_section{'values'} = \@censored_result_values;
			push( @{$self->results->[0]{'own'}},\%censored_return_section );
		}
		if ($self->categorized){
			my @categorized_result_labels = (\@categorized_result_row_labels,\@categorized_result_column_labels);
			$categorized_return_section{'labels'} = \@categorized_result_labels;
			$categorized_return_section{'values'} = \@categorized_result_values;
			push( @{$self->results->[0]{'own'}},\%categorized_return_section );
		}

		my @diag_labels=();
		my @diag_vals=();
		
		my %diag_section;
		if ($self->categorized){
			$diag_section{'name'}="\nDiagnostics VPC Continuous data".$self->strata_labels->[$strat_ind];
		} else {
			$diag_section{'name'}="\nDiagnostics VPC ".$self->strata_labels->[$strat_ind];
		}
		for (my $i=$npc_pi_offset; $i<scalar(@pred_int); $i++){
			push (@diag_labels,"PI $pred_int[$i]\% ");
			if ($sum_no_obs > 0){
				push (@diag_vals,[$sum_falsepos[$i]*100/$sum_no_obs,$sum_falseneg[$i]*100/$sum_no_obs]);
			}else{
				push (@diag_vals,['','']);
			}
		}
		$diag_section{'labels'}=[\@diag_labels,["False pos (\%)","False neg (\%)"]];
		$diag_section{'values'}=\@diag_vals;

		unless ($self->detection_censored){ 
			push( @{$self->results->[0]{'own'}},\%diag_section );
		}
		if (defined $self->censor()){
			push( @{$self->results->[0]{'own'}},@missing_section_array);
		}
		if ($self->detection_censored and (not $self->predcorr)){
			push( @{$self->results->[0]{'own'}},@censored_npc_section_array);
		} else {
			push( @{$self->results->[0]{'own'}},@npc_section_array);
		}
	} #endof loop over strata

	if ($warn_about_missing_data){
		if (defined $self->censor){
			ui -> print (category=>'vpc', 
						 message=>"Warning: One or more datasets in one or more bins contained ".
						 "less than 10 non-missing observations. See details in the ".
						 "section 'Missing data warnings' in vpc_results.csv");
		}else{
			1;
		}
	}
}

sub npc_analyze
{
	my $self = shift;

	my $c_i = $self->confidence_interval(); 
	my @pred_int = sort {$a <=> $b} 0,20,40,50,60,80,90,95;
	my $no_sim= $self->n_simulations;
	my $no_observations = $self->n_observations;
	my $no_of_strata = scalar(@{$self->strata_labels});
	
	my $no_pred_ints = scalar(@pred_int);
	my ($lower_index,$upper_index,$low_ind,$high_ind);
	my ($result_column_labels,$result_row_labels);


	if (-e $self->directory."/".$self->results_file()){
		my $fname = $self->results_file();
		$fname =~ s/\.csv$// ;

		my $addnum=1;
		while (-e $self->directory."/$fname"."-old$addnum".'.csv'){
			$addnum++;
		}
		my $newname = "$fname"."-old$addnum".'.csv';
		mv( $self->directory.$fname.'.csv', $self->directory.$newname);

		ui -> print (category=>'npc', 
					 message=>"Renamed old $fname".
					 ".csv to $newname to protect old output. New output is $fname".".csv.");
	}


	
	## Prepare general run info for output file
	my %return_sec;
	$return_sec{'name'} = 'NPC run info';
	my $modelname= (defined $self->orig_table)?'unknown' : $self->models->[0]->filename();
	my $extra_value='auto-generated';
	if (defined $self->sim_model()){
		my $fil = $self->sim_model();
		if ( $fil =~ /\/$/ ){
			$fil =~ s/\/$//;
		}elsif ( $fil =~ /\\$/ ){ 
			$fil =~ s/\\$//;
		}
		my ($file_volume,$file_directory, $file_file) = File::Spec -> splitpath( $fil);
#    print "$file_volume : $file_directory : $file_file\n";
		$extra_value=$file_file;
	} elsif ($self->flip_comments()){
		$extra_value="flip comments $modelname";
	}

	$return_sec{'labels'} = [[],['Date','observations','simulations','Modelfile',
								 'Simulation model','Dependent variable','PsN version','NONMEM version']];
	
	my @datearr=localtime;
	my $the_date=($datearr[5]+1900).'-'.($datearr[4]+1).'-'.($datearr[3]);
	
	$return_sec{'values'} = [[$the_date,$no_observations,$no_sim,$modelname,$extra_value,
							  $self->dv,$PsN::version,$self->nm_version]];
	
	push( @{$self->results->[0]{'own'}},\%return_sec );
	
	##done general run info
	
	($result_column_labels,$result_row_labels) = $self->get_npc_result_labels('ci' => $c_i,
																			  'pred_intervals' => \@pred_int);
	my @result_labels = ($result_row_labels,$result_column_labels);


	($lower_index,$upper_index,$low_ind,$high_ind)= $self->get_npc_indices('ci' => $c_i,
																		   'no_sim' => $no_sim,
																		   'pred_intervals' => \@pred_int);

	if ($self->verbose){
		for (my $i=0; $i<$no_pred_ints; $i++){
			print "$pred_int[$i] $lower_index->[$i] $upper_index->[$i]\n";
		}
		print "\n" ;
	}
	
	my $analyzed_points=0;
	my $realpos; #dirt
	for (my $strat_ind=0; $strat_ind<$no_of_strata; $strat_ind++){
		my %return_section;
		my ($result_values,$stats_warnings);
		($result_values,$realpos,$stats_warnings)=$self->subset_npc_analyze('strata_index' => $strat_ind,
																			'low_ind' => $low_ind,
																			'high_ind' => $high_ind,
																			'lower_index' => $lower_index,
																			'upper_index' => $upper_index, 
																			'pred_intervals' => \@pred_int);
		
		my $point_counter=scalar(@{$self->strata_matrix->[$strat_ind]});
		my $result_name = "\nNPC results ".$self->strata_labels->[$strat_ind].
			"\n$point_counter observations out of $no_observations";
		$return_section{'name'} = $result_name;
		
		$return_section{'labels'} = \@result_labels;
		$return_section{'values'} = $result_values;
		push( @{$self->results->[0]{'own'}},\%return_section );
		
		my %diag_section;
		$diag_section{'name'} = "\nNPC * (warning) statistics";
		$diag_section{'labels'} = [[],['Real data * count','Sim. data * count mean','Theoretical mean','Sim. data * count median',
									   'Sim. data * count '.$self->confidence_interval().'% CI from','to']];
		$diag_section{'values'} = [$stats_warnings];
		push( @{$self->results->[0]{'own'}},\%diag_section );
		
		$analyzed_points+=$point_counter;
	}
	
	#check that all individuals were grouped to a strata
	unless ($analyzed_points == $no_observations){
		croak("Sum of observations $analyzed_points in each strata does not\n".
			  "equal total number of observations $no_observations. Something went wrong.");
	}
}

sub get_npc_indices
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  pred_intervals => { isa => 'Ref', optional => 1 },
							  ci => { isa => 'Num', optional => 1 },
							  no_sim => { isa => 'Int', optional => 1 }
		);
	my $pred_intervals = $parm{'pred_intervals'};
	my $ci = $parm{'ci'};
	my $no_sim = $parm{'no_sim'};
	my @lower_index;
	my @upper_index;
	my $low_ind;
	my $high_ind;

	#in pred_intervals ci no_sim
	#out lower_index upper_index low_ind high_ind
	
	#For each prediction interval pred_int: Compute lower_index, i.e. index 
	#of first value in sorted array which is inside this interval. 
	#Compute upper index, index of last value inside interval

	my @pred_int = sort {$a <=> $b} @{$pred_intervals};
	my $no_pred_ints = scalar(@pred_int);

	$low_ind = $self->round('number'=>((100-$ci)*($no_sim-1)/200)); #index of start of ci % interval
	$high_ind = $no_sim - $low_ind - 1; #index of end  of  c_i% interval

	for (my $i=0; $i<$no_pred_ints; $i++){
		push (@lower_index, $self->round('number'=>((100-$pred_int[$i])*($no_sim-1)/200))); 
		push (@upper_index, $no_sim - $lower_index[$i] -1);
	}

	return \@lower_index ,\@upper_index ,$low_ind ,$high_ind;
}

sub get_npc_result_labels
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  pred_intervals => { isa => 'Ref', optional => 1 },
							  ci => { isa => 'Num', optional => 1 }
		);
	my $pred_intervals = $parm{'pred_intervals'};
	my $ci = $parm{'ci'};
	my @result_column_labels;
	my @result_row_labels;

	#in pred_intervals ci
	#out result_column_labels result_row_labels
	
	my @pred_int = sort {$a <=> $b} @{$pred_intervals};
	my $no_pred_ints = scalar(@pred_int);
	
	@result_column_labels=('points below PI (count)','points below PI (%)','outside CI for below PI',
						   "$ci\% CI below: from (\%)","$ci\% CI below: to (\%)" ); 
	push (@result_column_labels,('points above PI (count)','points above PI (%)','outside CI for above PI',
								 "$ci\% CI above: from (\%)","$ci\% CI above: to (\%)"));
	
	for (my $i=0; $i<$no_pred_ints; $i++){
		push (@result_row_labels,"$pred_int[$i]% PI");
	}

	return \@result_column_labels ,\@result_row_labels;
}

sub subset_npc_analyze
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  bin_index => { isa => 'Int', optional => 1 },
							  strata_index => { isa => 'Int', optional => 1 },
							  pred_intervals => { isa => 'Ref', optional => 1 },
							  lower_index => { isa => 'Ref', optional => 1 },
							  upper_index => { isa => 'Ref', optional => 1 },
							  low_ind => { isa => 'Int', optional => 1 },
							  high_ind => { isa => 'Int', optional => 1 }
		);
	my $bin_index = $parm{'bin_index'};
	my $strata_index = $parm{'strata_index'};
	my $pred_intervals = $parm{'pred_intervals'};
	my $lower_index = $parm{'lower_index'};
	my $upper_index = $parm{'upper_index'};
	my $low_ind = $parm{'low_ind'};
	my $high_ind = $parm{'high_ind'};
	my @result_values;
	my @real_positions;
	my @stats_warnings;

	$self->verbose(0);

	#start function, in is bin_index,strata_index,lower_index,upper_index,
	#pred_intervals,$high_ind,$low_ind
	# if  bin_index undefined then assume all
	#out is ref to result_values and real_positions and stats_warnings
	#npoints not needed, length of input @row_indices

	#must handle censored!!!
	#realpos should have one value per possible observation,
	#but if real observation is missing or predcorr and lloq/uloq
	#then set -99
	#other result_values should be '' if cannot be computed
	#if censored must recompute all indices, cannot use $high_ind, $low_ind, $lower_index, $upper_index
	
	my $censored = 0;
	$censored = 1 if (defined $self->censor() or ($self->detection_censored and $self->predcorr));

	my $ci = $self->confidence_interval();
	my $no_sim= $self->n_simulations;
	my $point_counter=0;
	if (defined $bin_index){
		$point_counter = scalar(@{$self->binned_data->[$strata_index]->[$bin_index]});
	} else {
		$point_counter = scalar(@{$self->stratified_data->[$strata_index]});
	}
	if ($point_counter < 1){ #empty set of observations
		return;
	}
	
	#need to guarantee sorted pred_ints
	my @pred_int = sort {$a <=> $b} @{$pred_intervals};
	my $no_pred_ints = scalar(@pred_int);
	my @upper_limit = (0) x $no_pred_ints;
	my @lower_limit = (0) x $no_pred_ints;
	my (@values,@sorted_sim_values);
	my $orig_value;
	my @sum_warnings = (0) x ($no_sim+1);
	my (@upper_count, @lower_count);
	my @count_max = (0) x ($no_sim+1);

	for (my $i=0; $i<$no_pred_ints; $i++){
		@{$upper_count[$i]} = (0) x ($no_sim+1);
		@{$lower_count[$i]} = (0) x ($no_sim+1);    
		@{$real_positions[$i]} = (0) x ($point_counter);    
	}

	my $obs_counter=0;
	
#    print "\n\n\n";
	my $alert_string="\n\nWarning: NPC results may be misleading. There may be too many\n".
		"identical values to make it possible to report correct counts above and below the PI.\n\n";
	
	for (my $point_index=0; $point_index < $point_counter; $point_index++){
		my @lowers=();
		my @uppers=();
		my $row;
		if (defined $bin_index){
			$row = $self->binned_data->[$strata_index]->[$bin_index]->[$point_index];
		} else {
			$row = $self->stratified_data->[$strata_index]->[$point_index];
		}

		($orig_value,@values) = split(/,/,$row); 

		my @sorted_sim_values=();
		my ($orig_cens,@censor);

		if ($censored){
			my @remaining = ();
			my $cens;
			if (defined $bin_index){
				$cens = $self->censor_binned_data->[$strata_index]->[$bin_index]->[$point_index];
			}else{
				$cens = $self->censor_stratified_data->[$strata_index]->[$point_index];
			}
			($orig_cens,@censor) = split(/,/,$cens); 
			for (my $j=0; $j< $no_sim; $j++){
				if ($self->predcorr){
					push(@remaining,$values[$j]) if ($censor[$j] == 0); #fully known
				}else{
					push(@remaining,$values[$j]) if ($censor[$j] != 1); #not missing
				}
			}
			@sorted_sim_values = sort {$a <=> $b} @remaining; #sort numerically ascending
		}else{
			@sorted_sim_values = sort {$a <=> $b} @values; #sort numerically ascending
		}

		if (scalar(@sorted_sim_values) == 0){
			for (my $i=0; $i<$no_pred_ints; $i++){
				$lower_limit[$i] = '';
				$upper_limit[$i] = '';
			}
			$obs_counter++;
		}else{
			for (my $i=0; $i<$no_pred_ints; $i++){
				#need to recompute index if censored
				my $lower = $lower_index->[$i];
				my $upper = ($upper_index->[$i]);
				if ($censored){
					$lower = $self->round('number'=>((100-$pred_int[$i])*(scalar(@sorted_sim_values)-1)/200)); 
					$upper = (scalar(@sorted_sim_values) - $lower -1);
				}
				$lower_limit[$i] = $sorted_sim_values[$lower];
				$upper_limit[$i] = $sorted_sim_values[$upper];	
				unless ($self->npc_alert_written){
					if (($lower > 1) && 
						(($lower_limit[$i] == $sorted_sim_values[$lower -1]) ||
						 ($lower_limit[$i] == $sorted_sim_values[$lower +1]))) {
						print $alert_string;
						$self->npc_alert_written(1);
					} elsif (($upper < scalar(@sorted_sim_values)) && 
							 (($upper_limit[$i] == $sorted_sim_values[$upper -1]) ||
							  ($upper_limit[$i] == $sorted_sim_values[$upper +1]))) {
						print $alert_string;
						$self->npc_alert_written(1);
					}
				}
			}
			
			unshift @values,($orig_value); #put original value back at the beginning
			unshift @censor,($orig_cens) if ($censored);
			
			#Since the pred_ints are sorted in ascending order (e.g 80, 90, 95),
			#if value is below limit of first prediction interval, cannot be above
			#limit of any interval. Then if value is not below limit of 2nd, 3rd... 
			#interval it cannot be below limit of any of the remaining.
			
			for (my $j=0; $j<= $no_sim; $j++){
				next if ($censored and (($self->predcorr and $censor[$j] != 0) or
										$censor[$j]==1) );
				$count_max[$j] += 1;
				if ($values[$j] < $lower_limit[0]){
					$lower_count[0]->[$j] +=1;
					for (my $i=1; $i<$no_pred_ints; $i++){
						if ($values[$j] < $lower_limit[$i]){
							$lower_count[$i]->[$j] +=1;
						} else {
							last; #goto next column (next value in @values)
						}
					}
				} elsif ($values[$j] > $upper_limit[0]){
					$upper_count[0]->[$j] +=1;
					for (my $i=1; $i<$no_pred_ints; $i++){
						if ($values[$j] > $upper_limit[$i]){
							$upper_count[$i]->[$j] +=1;
						} else {
							last; #goto next $value
						}
					}
				}

			}


			##For VPC diagnostics: build integer matrix w/ under/above/in info for real data only
			# 0 means inside.

			if ($censored and (($self->predcorr and $censor[0] != 0) or
							   $censor[0]==1) ){
				$real_positions[0]->[$obs_counter]=-99;
				for (my $i=1; $i<$no_pred_ints; $i++){ #is 1 here for skipping 50-50?
					$real_positions[$i]->[$obs_counter]=-99;
				}
			}elsif ($values[0] < $lower_limit[0]){
				$real_positions[0]->[$obs_counter]=-1;
				for (my $i=1; $i<$no_pred_ints; $i++){
					if ($values[0] < $lower_limit[$i]){
						$real_positions[$i]->[$obs_counter]=-1;
					} else {
						last;
					}
				}
			} elsif ($values[0] > $upper_limit[0]){
				$real_positions[0]->[$obs_counter]= 1;
				for (my $i=1; $i<$no_pred_ints; $i++){
					if ($values[0] > $upper_limit[$i]){
						$real_positions[$i]->[$obs_counter]= 1;
					} else {
						last;
					}
				}
			}

			$obs_counter++;
			### end VPC diagnostics
			
		} #endof if at least 1 uncensored sim

	} #endof foreach row

#  my $const=100/$point_counter; #conversion from count to percent
	if ($self->verbose){
		for (my $i=0; $i<$no_pred_ints; $i++){
			printf "%.0f %.3f %.3f\n",$pred_int[$i],$lower_limit[$i],$upper_limit[$i];
			for (my $j=0; $j<= $no_sim; $j++){
				printf "%.3f ",$lower_count[$i]->[$j];
			}
			print "\n";
			for (my $j=0; $j<= $no_sim; $j++){
				printf "%.3f ",$upper_count[$i]->[$j];
			}
			print "\n\n";
		}
		print "\n";
	}  

	#high_ind and lowind should be based on nonzero_obs
	my $non_zeros = 0;
	$sum_warnings[0] = -99 if ($count_max[0] == 0);
	for (my $j=1; $j<= $no_sim; $j++){
		$non_zeros++ if ($count_max[$j] > 0);
		$sum_warnings[$j] = -99 if ($count_max[$j] == 0);
	}
	$low_ind = $self->round('number'=>((100-$ci)*($non_zeros-1)/200)); #index of start of ci % interval
	$high_ind = $non_zeros - $low_ind - 1; #index of end  of  c_i% interval

	#loop over prediction intervals, compute results for each interval
	for (my $i=0; $i<$no_pred_ints; $i++){
		my $warn=' '; 
		my $realperc = undef;
		if ($count_max[0] > 0){
			$realperc = 100*$lower_count[$i]->[0]/$count_max[0] ;
		}
		my @perc_arr =();
		for (my $j=1; $j<= $no_sim; $j++){
			if ($count_max[$j] > 0){
				push(@perc_arr, 100*$lower_count[$i]->[$j]/$count_max[$j]) ;
			}
		}
		my @sorted_arr = sort {$a <=> $b} @perc_arr;
		
		if ($non_zeros> 0){
			if ((defined $realperc) and (( $realperc<$sorted_arr[$low_ind]) ||($realperc>$sorted_arr[$high_ind]))){
				$warn='*';
				$sum_warnings[0] += 1; #NPC diagnostics
			}
		}

		#For NPC diagnostics:
		for (my $si=1; $si<= $no_sim; $si++){
			if ($count_max[$si] > 0){
				if (( (100*$lower_count[$i]->[$si]/$count_max[$si]) < $sorted_arr[$low_ind]) 
					||((100*$lower_count[$i]->[$si]/$count_max[$si]) > $sorted_arr[$high_ind])){
					$sum_warnings[$si] += 1;
				}
			}
		}
		#endof NPC diagnostics
		
		if ($self->verbose){
			print "\n $high_ind $low_ind\n";
			print $lower_count[$i]->[0]." $realperc $warn ".$sorted_arr[$low_ind].
				" ".$sorted_arr[$high_ind]."\n";
		}

		my @result_row_values =($lower_count[$i]->[0],$realperc,$warn,$sorted_arr[$low_ind],
								$sorted_arr[$high_ind]);

		$warn=' ';
		$realperc = undef;
		if ($count_max[0] > 0){
			$realperc = 100*$upper_count[$i]->[0]/$count_max[0] ;
		}
		my @perc_arr =();
		for (my $j=1; $j<= $no_sim; $j++){
			if ($count_max[$j] > 0){
				push(@perc_arr, 100*$upper_count[$i]->[$j]/$count_max[$j]);
			}
		}
		my @sorted_arr = sort {$a <=> $b} @perc_arr;
		
		if ($non_zeros> 0){
			#high_ind and lowind based on nonzero_obs, computed above
			if ((defined $realperc) and (( $realperc<$sorted_arr[$low_ind]) ||($realperc>$sorted_arr[$high_ind]))){
				$warn='*';
				$sum_warnings[0] += 1; #NPC diagnostics
			}
		}
		
		#For NPC diagnostics:
		for (my $si=1; $si<= $no_sim; $si++){
			if ($count_max[$si] > 0){
				if (( (100*$upper_count[$i]->[$si]/$count_max[$si]) < $sorted_arr[$low_ind]) 
					||((100*$upper_count[$i]->[$si]/$count_max[$si]) > $sorted_arr[$high_ind])){
					$sum_warnings[$si] += 1;
				}
			}
		}
		#endof NPC diagnostics
		
		if ($self->verbose){
			print $upper_count[$i]->[0]." $realperc $warn ".$sorted_arr[$low_ind].
				" ".$sorted_arr[$high_ind]."\n";
		}
		push (@result_row_values,($upper_count[$i]->[0], $realperc,$warn,$sorted_arr[$low_ind],
								  $sorted_arr[$high_ind]));
		
		push (@result_values,\@result_row_values);

	} #endof loop over prediction intervals

	#warning statistics
	my $i = shift @sum_warnings; #real count
	push(@stats_warnings,$i); 
	my $sum_sums=0;
	my @detected_sum_warnings = ();
	foreach my $i (@sum_warnings){
		if ($i != -99){
			$sum_sums += $i;
			push ( @detected_sum_warnings,$i);
		}
	} 
	unless ($non_zeros == scalar(@detected_sum_warnings)){
		print "\n\nWarning:error in npc_subsection, counting nonzero bins  \n ";
	}
	if ($non_zeros == 0){
		push(@stats_warnings,'','','','','');
	}else{
		push(@stats_warnings,$sum_sums/$non_zeros);
		#theoretical mean, 
		#number of PI times times 2*2 for above and below lower and upper PI limit
		#number absolute number outside a limit ($low_ind) div by total number of values
		push(@stats_warnings,($no_pred_ints*2*2*$low_ind/$non_zeros)); #ok

		my @sorted_sums = sort {$a <=> $b} @detected_sum_warnings;
		
		#median
		push(@stats_warnings,$self->median('sorted_array'=>\@sorted_sums)); 

		push(@stats_warnings,$sorted_sums[$low_ind]); #start CI 
		push(@stats_warnings,$sorted_sums[$high_ind]); #end CI 
	}
	$self->verbose(0);

	return \@result_values ,\@real_positions ,\@stats_warnings;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
