package common_options;

#use Carp;
use FindBin qw($Bin);
use lib "$Bin/../lib";
use include_modules;
use Getopt::Long;
use Text::Wrap;
use Math::Random;
use Cwd;
use OSspecific;
use PsN;

## Configure the command line parsing
Getopt::Long::config("auto_abbrev");

@tool_options = ( "abort_on_fail",
		  "accepted_ofv_difference:f",
		  "add_retries!",
		  "check_nmtran!",
		  "clean:i",
		  "compress!",
		  "condition_number_limit:f",
		  "correlation_limit:f",
		  "crash_restarts:i",
				  "degree:f",
		  "directory:s",
		  "display_iterations!",
		  "email_address:s",
		  "handle_msfo",
		  "handle_crashes!",
		  "large_theta_cv_limit:f",
		  "large_omega_cv_limit:f",
		  "large_sigma_cv_limit:f",
		  "last_est_complete!",
		  "lsf_job_name:s",
		  "lsf_options:s",
		  "lsf_project_name:s",
		  "lsf_queue:s",
		  "lsf_resources:s",
		  "lsf_sleep:i",
		  "lsf_ttl:s",
		  "max_runtime:s",
		  "min_retries:i",
		  "missing_data_token:i",
		  "nice:i",
		  "niter_eonly:i",
		  "nm_output:s",
		  "nm_version:s",
		  "nmfe!",
		  "nmfe_options:s",
		  "nmqual!",
		  "nodes:i",
		  "nonparametric_etas",
		  "nonparametric_marginals",
		  "parafile:s",
		  "picky!",
		  "prepend_model_file_name!",
		  "quick_summarize|quick_summary",
		  "retries:i",
		  "run_on_lsf!",
		  "run_on_ud!",
		  "run_on_sge!",
		  "run_on_slurm!",
		  "run_on_torque!",
		  "run_on_zink!",
		  "send_email!",
		  "sge_resource:s",
		  "sge_queue:s",
		  "sge_prepend_flags:s",
		  "seed:s",
		  "shrinkage",
		  "significant_digits_accept:f",
		  "sign_digits_off_diagonals:i",
		  "slurm_prepend_flags:s",
		  "slurm_account:s",
		  "slurm_partition:s",
		  "stop_motion:i",
		  "threads:i",
		  "torque_queue:s",
		  "torque_prepend_flags:s",
		  "tweak_inits!",
		  "verbose!",
		  "near_bound_sign_digits:i",
		  "near_zero_boundary_limit:f",
		  "precision:i",
    );


@model_options = ("extra_files:s",
				  "extra_output:s",
				  "d2u!",
				  "maxevals:i",
				  "missing_data_token:i",
				  "tbs!",
				  "dtbs!",
				  "tbs_lambda:s",
				  "tbs_zeta:s",
				  "tbs_delta:s",
				  "sde",
				  "cwres",
				  "mirror_plots:i",
				  "iofv",
				  "mirror_from_lst!",
				  "omega_before_pk!",
				  "outputfile:s",
				  "last_est_complete!",
				  "niter_eonly:i"
    );

my @script_options = ( "debug:i",
		       "h|?",
		       "help",
		       "html_help",
		       "silent",
		       "version",
		       "warn_with_trace:i"
		       );

@get_opt_strings = (sort(@tool_options), sort(@model_options), sort(@script_options));

@extra_files;
@extra_output;
$parameters = options_to_parameters([@tool_options,'top_tool']);

my $options_ref;



sub options_to_parameters {
  #inparameter is ref to array of option names
  my $opts = shift;
  my @options = @{$opts};

  my $parameter_string = '( ';

  foreach my $opt ( @options ){
    $opt =~ s/[!:|].*//g; #get rid of :s |? :i etcetera
    $parameter_string .= "$opt => \$options{'$opt'},\n";
  }
  $parameter_string .= ' )';
  return $parameter_string;
}


sub setup {
  my $opts = shift;
  my $command = shift;

  get_defaults( $opts, $command );
  
  unless (defined $opts -> {'seed'}){
    $opts -> {'seed'} = random_uniform_integer( 1, 100000, 999999 );
  }
  set_globals( $opts, $command );
  sanity_checks( $opts, $command );
  store_common_options($opts );

}

sub store_common_options {
  $options_ref = shift;
  #global variable options_ref was declared above
}

sub restore_options {
  my @relevant_options = @_;
  my %options = %{$options_ref}; #store_common_options must have been called
  #in bin/<utility> (e.g. nonpb) for this to work

  my %stored_opts = eval( options_to_parameters( \@relevant_options ) );
  #options_to_parameters returns a string. eval() evaluates this as perl code
  #In the returned string the variable %options, declared in this function,
  #is used for substituting variables ($hash{'key'}) to values. 

  return \%stored_opts; #return reference to stored_opts
}
  


sub set_globals {
  my $opts = shift;
  my $command = shift;
  my %options = %{$opts};

  random_set_seed_from_phrase( $options{'seed'} ) if ( defined $options{'seed'} ); #not optimal, must change seed by at least 10 to get different sequence

  ui -> category( $command );
  ui -> silent(1) if( $options{'silent'} );
  
  my $version = 'default';
  if (defined $options{'nm_version'}){
    $version = $options{'nm_version'};
  }
  PsN::set_nonmem_info($version);

	$PsN::warnings_enabled = $options{'debug'};
	$Carp::Verbose = $options{'warn_with_trace'};
}

sub get_defaults {
  my $options = shift;
  my $tool    = shift;

  my $nm_string='';
  if (exists $options -> {'nm_version'}){
	  #if nm_version is set on the command-line, even if set explicitly to 'default'
	  # we do nothing if nm_version is set in config file, then this feature is not invoked
	  $nm_string = $options -> {'nm_version'};
  }

  if (length($nm_string)>0){
	  foreach my $default_option ( keys %{$PsN::config -> {'default_'.$tool.'_options_'.$nm_string}} ){
		  unless( exists $options -> {$default_option} ){
			  #unless already set on command line
			  $options -> {$default_option} = $PsN::config -> {'default_'.$tool.'_options_'.$nm_string} -> {$default_option};
		  }
	  }
  }

  foreach my $default_option ( keys %{$PsN::config -> {'default_'.$tool.'_options'}} ){
    unless( exists $options -> {$default_option} ){
		#unless already set on command line or above
		$options -> {$default_option} = $PsN::config -> {'default_'.$tool.'_options'} -> {$default_option};
    }
  }

  if (length($nm_string)>0){
	  foreach my $default_option ( keys %{$PsN::config -> {'default_options_'.$nm_string}} ){
		  unless( exists $options -> {$default_option} ){
			  #unless already set on command line or above
			  $options -> {$default_option} = $PsN::config -> {'default_options_'.$nm_string} -> {$default_option};
		  }
	  }
  }

  foreach my $default_option ( keys %{$PsN::config -> {'default_options'}} ){
    unless( exists $options -> {$default_option} ){
		#unless already set on command line OR above
		$options -> {$default_option} = $PsN::config -> {'default_options'} -> {$default_option};
    }
  }
  $options -> {'top_tool'} = 1;
}

sub sanity_checks {
	my $options = shift;
	my $tool = shift;


	if (defined $options->{'degree'}){
		if ($options->{'degree'} <=0 or $options->{'degree'}>=1){
			my $mes = "option degree must be larger than 0 and smaller than 1.\n";
			croak($mes);
		}
	}


	unless ($options -> {'nmfe'} or $options -> {'nmqual'}){
		#assume user wants nmfe if none set
		$options -> {'nmfe'} = 1;
	}
	if ($options -> {'nmfe'} and $options -> {'nmqual'}){
		#assume user wants nmqual if both set
		$options -> {'nmfe'} = 0;
	}

	if( $options -> {'max_runtime'} ){
		die "--max_runtime is only allowed with -run_on_slurm"
			unless ( $options -> {'run_on_slurm'} );
	}
	if( $options -> {'email_address'} and $options -> {'send_email'}){
		unless ($options -> {'email_address'} =~ /@/){
			die "ERROR: ".$options -> {'email_address'}." does not look like an email address.\n";
		}
	}
	if( $options -> {'sde'} ){
		if( $options -> {'omega_before_pk'} ){
			die "You cannot set both sde and omega_before_pk";
		}
	}
	if(defined $options-> {'tbs_zeta'} or defined $options-> {'tbs_delta'}){
		if ($options->{'tbs'}){
			die ("You cannot set -tbs with -tbs_zeta or -tbs_delta, you should use -dtbs");
		}
		$options -> {'dtbs'}=1;
	}
	if( defined $options-> {'tbs_zeta'} and defined $options-> {'tbs_delta'}){
		die "You cannot set both tbs_zeta and tbs_delta";
	}
	if( defined $options -> {'tbs_lambda'} and (not defined $options-> {'dtbs'} or (not $options->{'dtbs'}))){
		$options -> {'tbs'}=1;
	}

	if( $options -> {'run_on_slurm'} ){
		if( $options -> {'run_on_sge'} ){
			die "You cannot set both run_on_sge and -run_on_slurm";
		}
		if (defined $options ->{'max_runtime'}){
			if ($options ->{'max_runtime'} =~ /^[0-9]+$/){
				print "You have set the maximal runtime to ".$options ->{'max_runtime'}.
					" minutes.\n\n";
			}elsif ($options ->{'max_runtime'} =~ /^([0-9]+)\:([0-9]+)\:([0-9]+)$/){
				print "You have set the maximal runtime to $1 h $2 min $3 sec.\n\n";
			}elsif ($options ->{'max_runtime'} =~ /^([0-9]+)-([0-9]+)$/){
				print "You have set the maximal runtime to $1 day(s) and $2 h.\n\n";
			}else{
				die "ERROR: max_runtime must have format minutes, ".
					"hours:minutes:seconds, or days-hours.\n";
			}
		}else{
		}
	}
	if( $options -> {'stop_motion'} ){
		if( $options -> {'run_on_sge'} ){
			die "-stop_motion is not allowed together with -run_on_sge";
		}
		if( $options -> {'run_on_slurm'} ){
			die "-stop_motion is not allowed together with -run_on_slurm";
		}
	}

	if( $options -> {'accepted_ofv_difference'} &&
		$options -> {'accepted_ofv_difference'}<0){
		die "accepted_ofv_difference must not be negative\n";
	}

	if( $options -> {'min_retries'} && $options -> {'retries'}){
		if( $options -> {'min_retries'}> $options -> {'retries'}){
			die "min_retries must not be larger than retries\n";
		}
	}


	if( $PsN::nm_major_version == '7' ){
		if ($options -> {'iofv'}){
			my $mes = "\nWarning:\n"."option -iofv is not supported for NONMEM7,\n".
				"iotab file will not be produced. Individual ofv values\n".
				"can, for classical estimation methods, be found the additional output phi-file.\n\n";
			print $mes;
			$options -> {'iofv'}=0;
		}
	}
	if(( $PsN::nm_major_version == '5' ) and ($options -> {'nmfe'})){
		my $mes = "\n\n*****Warning:******\n"."option nmfe is not tested for NONMEM5,\n".
			"execution is likely to fail.\n\n";
		print "$mes";
	}
	if($options -> {'run_on_lsf'}){
		#check set options contain characters, no space only commands
		if (defined $options -> {'lsf_job_name'}){
			$options -> {'lsf_job_name'} = undef 
				if ($options -> {'lsf_job_name'} =~ /^\s*$/);
		}
		if (defined $options -> {'lsf_options'}){
			$options -> {'lsf_options'} = undef 
				if ($options -> {'lsf_options'} =~ /^\s*$/);
		}
		if (defined $options -> {'lsf_pre_exec_command'}){
			$options -> {'lsf_pre_exec_command'} = undef 
				if ($options -> {'lsf_pre_exec_command'} =~ /^\s*$/);
		}
		if (defined $options -> {'lsf_post_exec_command'}){
			$options -> {'lsf_post_exec_command'} = undef 
				if ($options -> {'lsf_post_exec_command'} =~ /^\s*$/);
		}
		if (defined $options -> {'lsf_project_name'}){
			$options -> {'lsf_project_name'} = undef 
				if ($options -> {'lsf_project_name'} =~ /^\s*$/);
		}
		if (defined $options -> {'lsf_queue'}){
			$options -> {'lsf_queue'} = undef 
				if ($options -> {'lsf_queue'} =~ /^\s*$/);
		}
		if (defined $options -> {'lsf_resources'}){
			$options -> {'lsf_resources'} = undef 
				if ($options -> {'lsf_resources'} =~ /^\s*$/);
		}
		if (defined $options -> {'lsf_sleep'}){
			$options -> {'lsf_sleep'} = undef 
				if ($options -> {'lsf_sleep'} =~ /^\s*$/);
		}
		if (defined $options -> {'lsf_ttl'}){
			$options -> {'lsf_ttl'} = undef 
				if ($options -> {'lsf_ttl'} =~ /^\s*$/);
		}
	}

	if (defined $options->{'nmfe_options'}) {
 		if ($options->{'nmfe_options'} =~ /\w+,\w+/) {
			print "\nWarning: You have set -nmfe_options=".$options->{'nmfe_options'}." on the commandline or in psn.conf\n";
			print "This does not look like valid options to the nmfe script.\n";
			print "Note that PsN4 will pass them on unchanged to the nmfe script, for help use\nexecute -help nmfe_options\n\n";
		} else {
			my @options_list = qw(background prsame prdefault prcompile trskip xmloff);
			foreach $opt (@options_list) {
				if ($options->{'nmfe_options'} =~ /\A$opt\Z/) {
					print "\nWarning: You have set -nmfe_options=".$options->{'nmfe_options'}." on the commandline or in psn.conf\n";
					print "This does not look a valid option to the nmfe script (the - sign is missing).\n";
					print "Note that PsN4 will pass this on unchanged to the nmfe script, for help use\nexecute -help nmfe_options\n\n";
				}
			}
		}
	}

	if(defined $options -> {'nm_output'}) {
		my @nmout = split( /,/ ,$options -> {'nm_output'});
		my %hash;
		foreach my $ext (@PsN::nm7_extensions){
			my $copy =$ext; #otherwise we modify the original array
			$copy =~ s/^\.//;
			$hash{$copy}=1;
		}
		foreach my $out (@nmout) {
			$out =~ s/^\.//;
			if ($out =~ /^lst$/) {
				print "\nInformation: The lst-file will always be copied back, no need to set it with option -nm_output.\n";
			}
			unless ($hash{$out}) {
				print "\nWarning: NM output file extension $out set in -nm_output is not recognized, it will be ignored.\n";
			}
			
		}
	}
	if ($options -> {'shrinkage'} && $options -> {'mirror_plots'}>0 ){
		my $mes = "Options -shrinkage and -mirror_plots ".
			"are incompatible. Please unset one of them.\n";
		croak($mes);
	}

	if (defined $options -> {'parafile'}){
		unless ($PsN::nm_major_version == 7 
				and defined $PsN::nm_minor_version and $PsN::nm_minor_version >1){
			my $ver='default';
			$ver = $options -> {'nm_version'} if (defined $options -> {'nm_version'});
			print "Option parafile can only be used with NONMEM version 7.2 or later.\n".
				"Make sure the version number for NM version '$ver' is correctly\n".
				"defined in psn.conf if you get this message even if you are using 7.2\n";
			exit;
		}
		if (-e $options -> {'parafile'}){
			if( defined $options -> {'extra_files'} ){
				$options -> {'extra_files'} = $options -> {'extra_files'}.','.$options -> {'parafile'};
			}else{
				$options -> {'extra_files'} = $options -> {'parafile'};
			}
			
			#   #check that nodes is not greater than number of names defined in parafile.
			#   #Then reset nodes and print warning
			#   open( FILE,  $options -> {'parafile'}) ||
			
		}else{
			print "Parafile ".$options -> {'parafile'}." does not exist.\n";
			exit;
		}
		
	}

}

sub print_help {
  my( $command, $required, $optional ) = @_;
  my %is_required;
  my %all_options = (%{$required},%{$optional});

  foreach my $req( keys %{$required} ){
    $is_required{$req} = 1;
  }

  my $option_help;

  $option_help .= "[ -h | -? ] [ --help ]\n" . ' ' x (1+length($command));
  
  my @loop_array;
  if( $command eq 'execute' ){
    @loop_array = sort(@get_opt_strings,keys %{$required}, keys %{$optional});
  } elsif( $command eq 'psn_options' ){
    @loop_array = sort(@get_opt_strings);
  } else {
    @loop_array = sort(keys %{$required}, keys %{$optional});
  }

  foreach my $help( @loop_array ) {
    next if( $help eq 'help' or $help eq 'h|?' );
    unless( $is_required{$help} ){
      $option_help .= "[ ";
    } else {
      $option_help .= "  ";
    }
    if( $all_options{$help} ne '' ){
      $help =~ /^([^:]+)/;
      $option_help .= "--$1=\'" . $all_options{$help} . "\'";
    } elsif( $help =~ /(.+):s/ ){
      $option_help .= "--$1=\'string\'";
    } elsif( $help =~ /(.+):i/ ){
      $option_help .= "--$1=\'integer\'";
    } elsif( $help =~ /(.+):f/ ){
      $option_help .= "--$1=\'number\'";
    } elsif( $help =~ /(.+):(\d)/ ){
      $option_help .= "--$1=$2";
    } elsif( $help =~ /(.+)[^:]$/ ){
      $option_help .= "--$help";
    }
    unless( $is_required{$help} ){
      $option_help .= " ]";
    }
    $option_help .= "\n".' ' x (1+length($command));
  }

  return $option_help;
}

sub model_parameters {
  my $options = shift;

  if( defined $options -> {'extra_files'} ){
    my @array = split(/,/,$options -> {'extra_files'});
    $options -> {'extra_files'} = \@array;
  }

  if( defined $options -> {'extra_output'} ){
    my @array = split( /,/ , $options -> {'extra_output'} );
    $options -> {'extra_output'} = \@array;
  }

  return options_to_parameters(\@model_options);

}


sub online_help {

  my $command = shift;
  my $opts = shift;
  my $help_text = shift;
  my $required_options = shift;
  my $optional_options = shift;
  my %options = %{$opts};
  
  my %help_hash;

    $help_hash{Options} = <<'EOF';
  <h3 class="heading1">Options:</h3>

    The options are given here in their long form. Any option may be
    abbreviated to any nonconflicting prefix. The <span class="style2">-threads</span> option may
    be abbreviated to <span class="style2">-thr</span>
    <br><br>
    The following options are valid:
EOF

    $help_hash{'-?'} = <<'EOF';
    <p class="style2">-h | -?</p>

    With <span class="style2">-h</span> or <span class="style2">-?</span> the script prints the list of available options 
    and exit.
EOF

    $help_hash{-help} = <<'EOF';
    <p class="style2">-help</p>
    With <span class="style2">-help</span> a longer help message will be printed.
EOF

    $help_hash{-nm_version} = <<'EOF';
    <p class="style2">-nm_version='string'</p>
    If you have more than one installation of NONMEM you can choose
    between them using the <span class="style2">-nm_version</span> option. The installations must be
    specified in the psn.conf file.
EOF
    $help_hash{-check_nmtran} = <<'EOF';
    <p class="style2">-check_nmtran</p>
	Make PsN run NMtran on the model file before submitting the complete nmfe run to a cluster/grid
	or forking on a local computer. This adds a bit of overhead but on a cluster this still 
	saves time in the case of syntax errors in the model file, since the user does not 
	have to wait for a slot on the cluster/grid before the error is detected. 
	On a local computer the error handling is improved.

	When running multiple copies of a model with different data sets, e.g. in a bootstrap,
	only the first model will be checked.

	The nmtran check requires that it is the installation directory to NONMEM that is set in psn.conf, 
	rather than the full path to an executable script. If the path to a script is given instead
	of an NM install directory the nmtran check will not be performed.
EOF
    $help_hash{-niter_eonly} = <<'EOF';
    <p class="style2">-niter_eonly</p>
      
      Only applies if NONMEM7 and last $EST is IMP or IMPMAP. Only for scripts
      vpc (any options), cdd (if option -xv) and execute (if option -mirror_plots 
      and/or nonparametric_etas). Undefined by default.
      User-chosen value of NITER in IMP or IMPMAP when estimation is turned off
      by setting EONLY=1. See PsN_and_NONMEM7.pdf for details.
EOF

    $help_hash{-last_est_complete} = <<'EOF';
    <p class="style2">-last_est_complete</p>
      
      Only applies if NONMEM7 and multiple $ESTIMATION records. Then only for scripts
      vpc (any options), cdd (if option -xv) and execute (if option -mirror_plots 
      and/or nonparametric_etas). Setting this option tells PsN that all 
      options needed for the last estimation step, such as LIKELIHOOD 
      (defaults can be omitted), are set explicitly in the 
      last $ESTIMATION record, i.e. no options need to carry over from previous $EST.
      Then PsN will skip the step where all $ESTIMATION records are searched for
      relevant options before removing all but the last $ESTIMATION and setting
      MAXEVAL=0 or EONLY=1. More details in the userguide.
EOF

    $help_hash{-nmfe} = <<'EOF';
    <p class="style2">-nmfe</p>
    Default set.
    Invoke NONMEM via the nmfe script (or a custom wrapper) from within PsN. 
    Unless option -nmqual is set, option -nmfe is 
    set automatically. Also, -nmfe is set in the default configuration file.
EOF
    $help_hash{-nmfe_options} = <<'EOF';
    <p class="style2">-nmfe_options='options for nmfe'</p>
    Only relevant if NONMEM7.2 or later is used.
    The text set with this option will be copied verbatim to the nmfe script call. 
    PsN will not check that the options are appropriate. When set on the PsN commandline 
    the string must be enclosed by quotes if it contains any spaces, but when set in 
    psn.conf it must never be enclosed by quotes even if it contains spaces. 
    Note that before PsN4 this option was given as a comma-separated list of options 
    that PsN would reformat. What would for PsN3 be specified as 
    -nmfe_options=xmloff,prdefault
    must now for PsN4 be specified on the commandline as 
    -nmfe_options='-xmloff -prdefault'
    or in psn.conf 
    nmfe_options=-xmloff -prdefault
EOF

    $help_hash{-nmqual} = <<'EOF';
    <p class="style2">-nmqual</p>
    Default not used. Run an NMQual-installed NONMEM via autolog.pl. Only NMQual8 is supported. 
    When set, PsN will locate the autolog.pl file and log.xml in the nmqual subdirectory of the NONMEM installation directory, and then run
    perl autolog.pl log.xml run ce workdir psn (extra NM options)
EOF

    $help_hash{-threads} = <<'EOF';
    <p class="style2">-threads='integer'</p>
    Use the threads option to enable parallel execution of multiple
    NONMEM runs. On a desktop computer it is recommended to set
    <span class="style2">-threads</span> to the number of CPUs in the system plus one. You can
    specify more threads, but it will probably not increase the
    performance. If you are running on a computer cluster, you should
    consult your system administrator to find out how many threads
    you can specify. The <span class="style2">-threads</span> option will be ignored if you run on
    a grid system, since grids have their own scheduling algoritms. The
    default value for the <span class="style2">-threads</span> option is 1.
EOF

    $help_hash{-nice} = <<'EOF';
    <p class="style2">-nice='integer'</p>
    This option only has effect on unix like operating systems. It
    sets the priority (or nice value) on a process. You can give any
    value that is legal for the "nice" command, likely it is between 0
    and 19, where 0 is the highest priority. Check "man nice" for
    details.
EOF

    $help_hash{-display_iterations} = <<'EOF';
    <p class="style2">-display_iterations</p>
    This option turns on display the iterations output from NONMEM during
    the model run. The option can be disabled with -no-display_iterations.
EOF

    $help_hash{-directory} = <<'EOF';
    <p class="style2">-directory='string'</p>
    The directory option sets the directory in which PsN will run
    NONMEM. The default directory name is 'modelfit_dirX' for execute,
    where X will be increased by one each time you run the script.
    For other scripts/tools, the default is 'toolname_dirX', for 
    example bootstrap_dir1. You do not have to create the directory, 
    it will be done for you.

    If you abort the run or if your system crashes you can use the
    '<span class="style2">-directory</span>' option set to the directory of the run that
    crashed. PsN will then not run the modelfiles that had
    finished before the crash, thereby saving some time. Notice that
    is important that you give exactly the same options that you gave
    the first time (exception npc and vpc tools, see npc manual).
EOF


    $help_hash{-extra_files} = <<'EOF';
    <p class="style2">-extra_files='extra_file1.dta, extra_file2.dta'</p>
    If you need extra files in the directory where NONMEM is run you
    specify them in the comma separated <span class="style2">-extra_files</span> list. It could for 
    example be fortran subroutines you need compiled with NONMEM.
EOF

    $help_hash{-maxevals} = <<'EOF';
    <p class="style2">-maxevals=100000</p>
    NONMEM only allows 9999 function evaluations. PsN can expand this
    limit by adding an MSFO option to $ESTIMATION. Later when NONMEM
    hits the max number of function evaluations allowed by NONMEM (9999) 
    PsN will remove intial estimates from the modelfile and add $MSFI 
    and restart NONMEM. This will be repeated until the number of function
    evaluations specified with option maxevals has been reached.
EOF

    $help_hash{-seed} = <<'EOF';
    <p class="style2">-seed='string'</p>
    You can set your own random seed to make PsN runs reproducible.
	The random seed is a string, and may include spaces if the whole string 
	is enclosed with single	quotes as in -seed='123 abc'. It is important to 
    know that, because of the way the Perl pseudo-random number generator works, 
    for two similar string seeds the random sequences may be identical. 
    This is the case e.g. with the two different seeds 123 and 122. 
    Setting the same seed guarantees the same sequence, but setting two slightly different 
    seeds does not guarantee two different random sequences, that must be verified.
EOF

    $help_hash{-verbose} = <<'EOF';
    <p class="style2">-verbose</p>
    With <span class="style2">verbose</span> set to 1, PsN will print
    more details about NONMEM runs. More precisely PsN will print the
    minimization message for each successfull run and a R:X for each
    retry PsN makes of a failed run, where X is the run number.
EOF

    $help_hash{-lsf_job_name} = <<'EOF';
    <p class="style2">-lsf_job_name='string'</p>
    <span class="style2">lsf_job_name</span> sets the name of the LSF job name of every NONMEM run, 
    they all get the same name.
EOF

    $help_hash{-lsf_options} = <<'EOF';
    <p class="style2">-lsf_options='string'</p>
    LSF jobs are submitted using bsub and all LSF related options are
    translated to corresponding bsub options. For maximum flexibility
    we allow any string to be passed as options to bsub, so if a specific 
    bsub feature not available through any ot the other -lsf_ options 
    is needed, use <span class="style2">lsf_options</span> to pass any option to bsub.
EOF

    $help_hash{-lsf_project_name} = <<'EOF';
    <p class="style2">-lsf_project_name='string'</p>
    Use <span class="style2">lsf_project_name</span> to assign a
    project name to your LSF runs.
EOF

    $help_hash{-lsf_resources} = <<'EOF';
    <p class="style2">-lsf_resources='string'</p>
    <span class="style2">lsf_resources</span> specifies which LSF resources is required when submiting
    NONMEM runs.
EOF

    $help_hash{-lsf_ttl} = <<'EOF';
    <p class="style2">-lsf_ttl='string'</p>
    <span class="style2">lsf_ttl</span> sets the maximum time a NONMEM run should be allowed to run on 
    the LSF grid.
EOF
    $help_hash{-lsf_sleep} = <<'EOF';
    <p class="style2">-lsf_sleep='string'</p>
    Pause for this many seconds after bsub submission, before continuing running PsN.
EOF

    $help_hash{-lsf_queue} = <<'EOF';
    <p class="style2">-lsf_queue='string'</p>
    <span class="style2">lsf_queue</span> specifies which LSF queue PsN should submit NONMEM runs 
    to and is used in conjuction with -run_on_lsf
EOF

    $help_hash{-min_retries} = <<'EOF';
    <p class="style2">-min_retries='integer'</p>
    <span class="style2">min_retries</span> forces PsN to try
    several initial values for each estimate and selecting the best
    one. The best model is the one with highest number of significant 
    digits and an ofv value no more than five units above than the 
    lowest ofv value among all models. If <span class="style2">-picky</span>
    is used, only models which first pass the picky test will be considered.
EOF

    $help_hash{-clean} = <<'EOF';
    <p class="style2">-clean='integer 0-3'</p>
    The <span class="style2">-clean</span> clean option can take four different values:
    0 - means that nothing is removed, 
    1 - NONMEM binary and intermediate files except INTER are removed, and files specified with 
    option -extra_files (this is the default), 
    2 - model and output files generated by PsN restarts are removed, and data files 
    in the NM_run directory,
    3 - the whole NM_run directory is removed 
    and if it is not an "execute" command, all modelfit_dirs will be removed.
EOF

    $help_hash{-missing_data_token} = <<'EOF';
    <p class="style2">-missing_data_token='string'</p>
    <span class="style2">missing_data_token</span> sets the number
    that PsN accepts as missing data, default is -99.
EOF

    $help_hash{-compress} = <<'EOF';
    <p class="style2">-compress</p>
    PsN will compress the contents of 'NM_runX' to the
    file 'nonmem_files.tgz' if the <span class="style2">-compress</span> option is used and if you
    have the archive and compress programs <strong>tar</strong> and <strong>gzip</strong> installed. If
    you use the <span class="style2">-clean</span> options, run files will be
    removed before the compression. The <span class="style2">-compress</span> option obviously has
    no effect if you also use the <span class="style2">-clean</span> option.
EOF

    $help_hash{-tweak_inits} = <<'EOF';
    <p class="style2">-tweak_inits</p>
    <!--/>If NONMEM terminates nonsuccessfully, PsN can perturb the initial
    estimates and run NONMEM again. The generation of new initial
    estimates init_i for the i:th retry are performed according to

    init_i = init_0 + rand_uniform(+-degree*init_0)

    where init_0 are the initial estimates of the original run and
	degree is set with option degree. The
    updating procedure makes sure that boundary conditions on the
    parameters are still valid. For this option to have effect, the
    -retries option must be set to number larger than zero. The
    default setting uses tweak_inits.<-->
    <?php print '<p>  If NONMEM terminates nonsuccessfully, PsN can perturb the initial estimates  and run NONMEM again. The generation of new initial estimates <img src="images/init1.gif"> for the <em>i</em>:th retry are performed according to</p><p align="center"><img src="images/perturb1.gif" width="236" height="32"></p> <p>where <img src="images/init_orig1.gif" width="29" height="28"> are the initial estimates of the original run. The updating procedure makes sure that boundary conditions on the parameters are still valid. For this option to be valid, the <span class="style2">-retries</span> option must be set to a number larger than zero. The default setting uses tweak_inits. </p>'; ?>
EOF

    $help_hash{-outputfile} = <<'EOF';
    <p class="style2">-outputfile='string'</p>
    The <span class="style2">-outputfile</span> option specifies the output file name for the
    NONMEM run. Currently this option is only valid when a single
    model is supplied.
EOF

$help_hash{-parafile} = <<'EOF';
    <p class="style2">-parafile='filename'</p>
    NONMEM 7.2 (or later) parafile. Appends "-parafile=filename"
    to the nmfe call, and makes PsN copy 'filename' to the NM_run directory.
    Only works if option nmfe or nmqual is set. Note that -nmfe is sometimes set 
    automatically, see help for -nmfe.
EOF

$help_hash{-nodes} = <<'EOF';
    <p class="style2">-nodes='number'</p>
    Only relevant together with option -parafile. Appends "[nodes]=option_value"
    to the nmfe call. The nodes option is completely independent of the threads 
    option. It is possible to e.g. set threads to 1 and nodes to 10.
EOF

    $help_hash{-picky} = <<'EOF';
    <p class="style2">-picky</p>
    The <span class="style2">-picky</span> option is only valid together with <span class="style2">-tweak_inits</span>. 
    Normally PsN only tries new initial estimates if 
    '<span class="style2">MINIMZATION SUCCESSFUL</span>' is not found in the NONMEM output
    file. With the <span class="style2">-picky</span> option, PsN will regard any of the
    following messages as a signal for rerunning:
<p class="style2">
    0ESTIMATE OF THETA IS NEAR THE BOUNDARY<br>
    0PARAMETER ESTIMATE IS NEAR ITS BOUNDARY<br>
    0R MATRIX ALGORITHMICALLY SINGULAR<br>
    0S MATRIX ALGORITHMICALLY SINGULAR</p>
EOF

    $help_hash{'-quick_summarize|quick_summary'} = <<'EOF';
    <p><span class="style2">-quick_summarize</span> or <span class="style2">-quick_summary</span></p>
    If either of <span class="style2">quick_summarize</span> and <span class="style2">quick_summary</span> is used, PsN will print 
    the ofv value and minimization message for each NONMEM run.
EOF

    $help_hash{-run_on_lsf} = <<'EOF';
    <p class="style2">-run_on_lsf</p>
    PsN connects with Platform Load Sharing Facility (LsF). With 
    <span class="style2">-run_on_lsf</span>. PsN will submit to the queue defined in "psn.conf" 
    unless specified with <span class="style2">-lsf_queue</span>.
EOF

    $help_hash{-run_on_ud} = <<'EOF';
    <p class="style2">-run_on_ud</p>
    PsN connects with United Devices Grid MP. With <span class="style2">-run_on_ud</span> PsN will submit to the UD grid
    with parameters defined in the "uduserconf" file.
EOF

    $help_hash{-retries} = <<'EOF';
    <p class="style2">-retries='integer'</p>
    The <span class="style2">-retries</span> option tells PsN how many times it
    shall try to rerun a NONMEM job if it fails according to given criterias. In
    the current version of PsN (2.2), the <span class="style2">-retries</span> option is only
    valid together with <span class="style2">-tweak_inits</span>. The default value of the
    <span class="style2">-retries</span> option is 0.
EOF

    $help_hash{-crash_restarts} = <<'EOF';
    <p class="style2">-crash_restarts='integer'</p>
    If a NONMEM outputfile is produced but PsN is unable to read it
    properly it is assumed that NONMEM crashed, probably due to
    something in the operating system, and PsN will start the run
    again. But PsN will not consider it a retry and will not change
    initial estimates. The default value is 4.
EOF
    $help_hash{-significant_digits_accept} = <<'EOF';
    <p class="style2">-significant_digits_accept='number'</p>
    The <span class="style2">-significant_digits_accept</span> option is only valid together with <span class="style2">-tweak_inits</span>. 
    Normally PsN tries new initial estimates if 
    '<span class="style2">MINIMZATION SUCCESSFUL</span>' is not found in the NONMEM output
    file. With the <span class="style2">-significant_digits_accept</span>, PsN will only rerun if 
    the resulting significant digits is lower than the value 
    specified with this option.
EOF

    $help_hash{-abort_on_fail} = <<'EOF';
    <p class="style2">-abort_on_fail</p>
    If the <span class="style2">-abort_on_fail</span> option is set and one of the NONMEM runs
    fails, PsN will stop scheduling more runs and try to stop
    those that are currently running. A run is considered failed if it
    fails to produce a list file which PsN can read. This can occur
    if a nonmem run crashes or gets killed.
EOF
    $help_hash{-add_retries} = <<'EOF';
    <p class="style2">-add_retries</p>

    By default, PsN will never do retries on a model when a run is restarted if
    the file stats-runs.csv is found in the NM_run subdirectory, since the
    existence of this file indicates that all retries have finished and the
    best try has been selected. If option -add_retries is set, PsN will ignore 
    that stats-runs.csv exists, and check again if retries are needed based on 
    the existing tries in the NM_run directory. This makes it possible to restart
    a run using different settings for retries (e.g. -retries, -min_retries, -picky).
EOF

    $help_hash{-silent} = <<'EOF';
    <p class="style2">-silent</p>
    The silent option turns off all output from PsN. Results and log
    files are still written to disk, but nothing is printed to the
    screen.
EOF

    $help_hash{-debug} = <<'EOF';
    <p class="style2">-debug='integer'</p>
    The <span class="style2">-debug</span> option is mainly intended for developers who wish to
    debug PsN. By default <span class="style2">-debug</span> is set to zero but you can set
    it to '1' to enable warning messages. If you run into problems that require support
		set this to 1 and send the output to the developers.
EOF

    $help_hash{-warn_with_trace} = <<'EOF';
    <p class="style2">-warn_with_trace='integer'</p>
    If <span class="style2">-warn_with_trace</span> is set, PsN will print a stack 
    trace for all warning and error messages.
    This is only for developers.
EOF

    $help_hash{-sde} = <<'EOF';
    <p class="style2">-sde</p>
    If you are running SDE models, you may have to use this option, otherwise
    PsN will print the records of the modelfile in the wrong order, and the NONMEM runs
    will fail.
EOF
    $help_hash{-omega_before_pk} = <<'EOF';
    <p class="style2">-omega_before_pk</p>
    For some models, NONMEM requires that $OMEGA is printed before $PK. 
    If you have such a model, this option must be set, otherwise
    PsN will print the records of the modelfile in the wrong order, and the NONMEM runs
    will fail.
EOF

    $help_hash{-condition_number_limit} = <<'EOF';
    <p class="style2">-condition_number_limit='number'</p>
    An error will be raised in the output from sumo
    if the condition number is greater than this number. 
EOF

    $help_hash{-nm_output} = <<'EOF';
    <p class="style2">-nm_output='comma-separated list of file extensions'</p>

    NONMEM generates many output files per run. The lst-file will always be 
	copied back to the calling directory. The option -nm_output decides which of the 
	additional files should be copied back to the calling directory. The default is none. 
    NM output files which are not copied to the calling directory can still be 
    found inside the run directory.

  Example: -nm_output=ext,cov

EOF
    $help_hash{-correlation_limit} = <<'EOF';
    <p class="style2">-correlation_limit='number'</p>
    All correlations above this number will be listed in the output from sumo.
EOF

    $help_hash{-accepted_ofv_difference} = <<'EOF';
    <p class="style2">-accepted_ofv_difference='number'</p>
    Default 0.1. This option is used by PsN only when selecting the best retry 
    out of the whole set, provided that -picky was not used or no try fulfilled
    the picky conditions. The selection will be based on the 'corrected ofv'. 
    For tries that did not have minimization successful the corrected ofv is 
    equal to the ofv. For tries that had minimization successful the corrected 
    ofv is the ofv minus 'accepted_ofv_difference'. This means that this option 
    decides how much preference should be given to runs that have minimization 
    successful.
EOF

    $help_hash{-handle_msfo} = <<'EOF';
    <p class="style2">-handle_msfo</p>
    Experimental feature for handling resumes using msfo and msfi files.
EOF

    $help_hash{-handle_crashes} = <<'EOF';
    <p class="style2">-handle_crashes</p>
    PsN tries to recognize NONMEM runs that crashed for various reasons,
    e.g. a computer crash or a NONMEM run deliberaterly killed, and restart
    those runs without changing initial parameter estimates.
EOF

    $help_hash{-large_theta_cv_limit} = <<'EOF';
    <p class="style2">-large_theta_cv_limit='number'</p>
    Coefficients of variation larger than this number will produce
    warnings in the output from sumo.
EOF

    $help_hash{-large_omega_cv_limit} = <<'EOF';
    <p class="style2">-large_omega_cv_limit='number'</p>
    Coefficients of variation larger than this number will produce
    warnings in the output from sumo.
EOF

    $help_hash{-large_sigma_cv_limit} = <<'EOF';
    <p class="style2">-large_sigma_cv_limit='number'</p>
    Coefficients of variation larger than this number will produce
    warnings in the output from sumo.
EOF

    $help_hash{-max_runtime} = <<'EOF';
    <p class="style2">-max_runtime='integer'</p>
    A limit on how long a slurm run may go on before being aborted
    (option -t to sbatch). Format is either minutes, e.g. -max_runtime=10, 
    or hours:minutes:seconds, e.g. -max_runtime=4:0:0, or days-hours, 
    e.g. -max_runtime=3-0
EOF

    $help_hash{-near_bound_sign_digits} = <<'EOF';
    <p class="style2">-near_bound_sign_digits='integer'</p>
    If a parameter estimate is equal to a bound with this many
    significant digits, a warning will be printed. Valid only with sumo.
EOF

    $help_hash{-near_zero_boundary_limit} = <<'EOF';
    <p class="style2">-near_zero_boundary_limit='number'</p>
    When the bound is zero, the check using -near_bound_sign_digits is not valid. 
    Use this limit instead. Valid only with sumo.
EOF

    $help_hash{-prepend_model_file_name} = <<'EOF';
    <p class="style2">-prepend_model_file_name</p>
    Table files by default have generic names, e.g. patab. If multiple
    models are run, files will be overwritten when multiple files
    with the same name are copied back to the same directory.
    This options prevents this by prepending the model file name,
    without extension, thus making the file names unique.
EOF

    $help_hash{-run_on_sge} = <<'EOF';
    <p class="style2">-run_on_sge</p>
    Use Sun Grid Engine queueing system.
EOF


    $help_hash{-run_on_torque} = <<'EOF';
    <p class="style2">-run_on_torque</p>
    Use Torque batch queueing system.
EOF

    $help_hash{-torque_queue} = <<'EOF';
    <p class="style2">-torque_queue='string'</p>
    Only valid with -run_on_torque. Maps to qsub option -q
EOF

    $help_hash{-torque_prepend_flags} = <<'EOF';
    <p class="style2">-torque_prepend_flags='string'</p>
    Only valid with -run_on_torque. The - signs must be included in the 
    string. The extra flags will be prepended to standard set in qsub call.

EOF

    $help_hash{-run_on_zink} = <<'EOF';
    <p class="style2">-run_on_zink</p>
    Experimental clustering on Windows machine.
EOF

    $help_hash{-sge_resource} = <<'EOF';
    <p class="style2">-sge_resource='string'</p>
    Only valid with -run_on_sge. Maps to qsub option -l
EOF

    $help_hash{-sge_queue} = <<'EOF';
    <p class="style2">-sge_queue='string'</p>
    Only valid with -run_on_sge. Maps to qsub option -q
EOF

    $help_hash{-sge_prepend_flags} = <<'EOF';
    <p class="style2">-sge_prepend_flags='string'</p>
    Only valid with -run_on_sge. The - signs must be included in the 
    string. The extra flags will be prepended to standard set in qsub call.

EOF
    $help_hash{-slurm_prepend_flags} = <<'EOF';
    <p class="style2">-slurm_prepend_flags='string'</p>
    Only valid with -run_on_slurm. The - signs must be included in the 
    string. The extra flags will be prepended to standard set in sbatch call.

EOF
    $help_hash{-slurm_account} = <<'EOF';
    <p class="style2">-slurm_account='string'</p>
    Only valid with -run_on_slurm. Maps to sbatch -A option.

EOF
    $help_hash{-slurm_partition} = <<'EOF';
    <p class="style2">-slurm_partition='string'</p>
    Only valid with -run_on_slurm. Maps to sbatch -p option.

EOF

    $help_hash{-send_email} = <<'EOF';
    <p class="style2">-send_email</p>
    Only valid with -run_on_slurm and -email_address in combination, otherwise ignored.
    Determines if sbatch options --mail_type=ALL --mail_user=<email_address> should be set.
EOF

    $help_hash{-email_address} = <<'EOF';
    <p class="style2">-email_address='string'</p>
    Only valid with -run_on_slurm and -send_email in combination, otherwise ignored.
    The email address in sbatch options --mail_type=ALL --mail_user=<email_address>.
EOF
    $help_hash{-shrinkage} = <<'EOF';
    <p class="style2">-shrinkage</p>
    Calculate the shrinkage for the model run.  Shrinkage is
    calculated as 1-(sd(eta(x))/omega(x)) and measures the shrinkage of the
    empirical Bayes estimates (EBEs) towards the mean of the expected
    distribution.  A 'large' shrinkage means that diagnostics using EBEs cannot
    be trusted.
EOF

    $help_hash{-sign_digits_off_diagonals} = <<'EOF';
    <p class="style2">-sign_digits_off_diagonals='integer'</p>
    The off-diagonal elements are checked against +-1 with this many
    significant digits. Valid only in sumo.
EOF

    $help_hash{-precision} = <<'EOF';
    <p class="style2">-precision='integer'</p>
    Precision in sumo output.
EOF
    $help_hash{-degree} = <<'EOF';
    <p class="style2">-degree=number</p>
	When tweaking initial estimates in retries/parallel_retries, this number decides the range for the 
    new estimates. The new number will be within 'degree'*oldinitial from the old initial estimate,
	unless restricted by upper or lower boundaries.
    A number larger than 0 and smaller than 1. Default 0.1.
EOF

    $help_hash{-extra_output} = <<'EOF';
    <p class="style2">-extra_output='file1,file2'</p>
    If NONMEM generates a file which PsN normally does not copy back
    to the working directory, specifying a comma-separated list
    of such files with this options will make PsN copy the listed files.
    An example is output generated by verbatim code.
EOF

    $help_hash{-cwres} = <<'EOF';
    <p class="style2">-cwres</p>
    Compute the conditional weighted residuals (CWRES) for a model run. 
    Option is only supported for NONMEM5 and NONMEM6. In NONMEM7, CWRES can
    be requested directly from $TABLE.
EOF
    $help_hash{-tbs} = <<'EOF';
    <p class="style2">-tbs</p>
    Default not set. Invokes Transform Both Sides method. Model must be coded
    'the Uppsala way', i.e. with IWRES and W and SIGMA 1 FIX.
    See the userguide common_options_defaults_versions_psn for details.
EOF
    $help_hash{-tbs} = <<'EOF';
    <p class="style2">-dtbs</p>
    Default not set. Invokes Dynamic Transform Both Sides method. Model must be coded
    'the Uppsala way', i.e. with IWRES and W and SIGMA 1 FIX.
    See the userguide common_options_defaults_versions_psn for details.
EOF

    $help_hash{-tbs_lambda} = <<'EOF';
    <p class="style2">-tbs_lambda</p>
    Default not set. Initial value string, using NM-TRAN syntax, 
    for parameter in Transform Both Sides 
    method, e.g. '(-1, 0.5, 1)' or 'O FIX'. The string must be enclosed 
    in single quotes and not include any comments.
    If tbs_lambda is set then option -tbs will be set automatically 
    unless -dtbs or -tbs_delta or -tbs_zeta is set.
    See the userguide common_options_defaults_versions_psn for details.
EOF
    $help_hash{-tbs_zeta} = <<'EOF';
    <p class="style2">-tbs_zeta</p>
    Default not set. Initial value string, using NM-TRAN syntax, 
    for parameter zeta in Transform Both Sides 
    method, e.g. '(-1, 0.5, 1)' or 'O FIX'. The string must be enclosed 
    in single quotes and not include any comments.
	Cannot be used in combination with tbs_delta.
    If tbs_zeta is set then option -dtbs will be set automatically.
    See the userguide common_options_defaults_versions_psn for details.
EOF
    $help_hash{-tbs_delta} = <<'EOF';
    <p class="style2">-tbs_delta</p>
    Default not set. Initial value string, using NM-TRAN syntax, 
    for parameter delta in Transform Both Sides 
    method, e.g. '(-1, 0.5, 1)' or 'O FIX'. The string must be enclosed 
    in single quotes and not include any comments.
	Cannot be used in combination with tbs_zeta.
    If tbs_delta is set then option -dtbs will be set automatically.
    See the userguide common_options_defaults_versions_psn for details.
EOF

    $help_hash{-mirror_plots} = <<'EOF';
    <p class="style2">-mirror_plots='integer'</p>
    This command creates a set of simulations from a model file that can then 
    be read into Xpose 4 for mirror plotting. The command requires an integer 
    value -mirror_plots=XX where XX is an integer representing the number of
    simulations to perform. This command uses the MSFO file created by
    runN.mod to get final estimates used in the simulations. If this file is
    not available run1.mod is run again.  If run times are long, and you did
    not create an MSFO file with your initial NONMEM run, you can combine the
    above command with the -mirror_from_lst option to avoid running the model
    again (PsN then reads from the *.lst file to get final parameter estimates
    for the simulations).
EOF

    $help_hash{-iofv} = <<'EOF';
    <p class="style2">-iofv</p>
    Compute the individual contributions to the objective function.
    Option is only supported for NONMEM5 and NONMEM6. In NONMEM7, individual ofv
    values can be found in the addtional output phi file.
EOF

    $help_hash{-mirror_from_lst} = <<'EOF';
    <p class="style2">-mirror_from_lst</p>
    Can only be used in combination with -mirror_plots=XX where XX is an 
    integer representing the number of simulations to perform. These commands 
    create a set of simulations from a model file and output file that can 
    then be read into Xpose 4 for mirror plotting.  The -mirror_from_lst option
    reads from the *.lst file of a NONMEM run to get final parameter estimates 
    for the simulations. 
EOF

    $help_hash{-html_help} = <<'EOF';
    <p class="style2">-html_help</p>
    Generate help text suitable for PsN homepage.
EOF

    $help_hash{-version} = <<'EOF';
    <p class="style2">-version</p>
    Print PsN version of script called.
EOF

    $help_hash{'-h'} = $help_hash{'-?'};


  if(( $command eq 'execute' ) || ( $command eq 'psn_options' )){
    if( defined $help_text ){
      my %temphash = %{$help_text};
      foreach my $ke (keys %temphash){
	$help_hash{$ke} = $temphash{$ke}; 
      }
    }
  } else {
    %help_hash = %{$help_text} if( defined $help_text );
  }

  if( $options{'version'} ){
    print "PsN version: $PsN::version\n";
    exit;
  }


  my $help;

  if($options{'h'} or $options{'?'} or $options{'help'} ) {

    if( $options{'html_help'} ){
      
      open(EXAMPLES, '>', 'html/' . $command . '_examples.php' );
      print EXAMPLES $help_hash{Examples};
      close( EXAMPLES );

      open(SYNOPSIS, '>', 'html/' . $command . '_synopsis.php' );
      print SYNOPSIS $help_hash{Pre_help_message},"\n";
      print SYNOPSIS "<h3 class=\"heading1\">Synopsis</h3>\n";
      print SYNOPSIS "<span class=\"option\">\n";
      print SYNOPSIS "<pre>$command " . common_options::print_help($command,$required_options, $optional_options)."\n</pre></span>\n" ;
      close( SYNOPSIS );

      open(OPTIONS, '>', 'html/' . $command . '_options.php' );
      my $opt_help;

      if( $command eq 'execute' ){
	@loop_array = sort(@get_opt_strings,keys %{$required_options}, keys %{$optional_options});
      } elsif ( $command eq 'psn_options' ){
	@loop_array = sort(@get_opt_strings);
      } else {
	@loop_array = sort(keys %{$required_options}, keys %{$optional_options});
      }
      
      foreach my $option( @loop_array ){
	$option =~ s/[^\w]*$|:.*//;
	if( exists $help_hash{'-'.$option}){
	  $opt_help .= $help_hash{'-'.$option}."\n";
	} else {
	  $opt_help .= "      <p class=\"option\">-$option</p>     <p>No help available for '$option'</p>";
	}
      }
      print OPTIONS $help_hash{Options} . $opt_help;
      close( OPTIONS );
      
      open(DESC, '>', 'html/' . $command . '_description.php' );
      print DESC $help_hash{Description};
      close( DESC );

      exit;
    } else {

      if( scalar( @ARGV ) > 0 ){
	foreach my $option ( @ARGV ){
	  
	  if( exists $help_hash{'-'.$option} ){
	    $help .= "\n".$help_hash{'-'.$option}. "\n";
	  } else {
	    $help .= "\nNo help available for '$option'\n\n";
	  }
	} 

	$help =~ s/<\?.*\?>//g;
        $help =~ s/<[^>]*>//g;
	print $help;
	exit;
      }
      
      $help .= "\n" . $help_hash{Pre_help_message} . "\n";
      
      if( $options{'help'} ){
	
	$help .= "\n\n".$help_hash{Description}."\n";
	$help .= $help_hash{Examples}."\n";
	$help .= $help_hash{Options}."\n";
	
	my @loop_array;
	
	if( $command eq 'execute' ){
	  @loop_array = sort(@get_opt_strings,keys %{$required_options}, keys %{$optional_options});
	} elsif ( $command eq 'psn_options' ){
	  @loop_array = sort(@get_opt_strings);
	} else {
	  @loop_array = sort(keys %{$required_options},keys %{$optional_options});
	}
	
	foreach my $option( @loop_array ){
	  $option =~ s/[^\w]*$|:.*//;
	  if( exists $help_hash{'-'.$option}){
	    $help .= $help_hash{'-'.$option}."\n";
	  } else {
	    $help .= "      -$option\n\n      No help available for '$option'\n\n";
	  }
	}
	
	$help .= $help_hash{Post_help_message} . "\n";
	
      } else { 
	$help .= common_options::print_help($command,$required_options, $optional_options);

	$help .= "\n    Options enclosed by [ ] are optional."; 
	$help .= "\n    Exclamation mark, !, after the option name means option can be disabled".
	         "\n    using '-no-option', for example -no-handle_crashes."; 
	$help .= "\n    Use '$command -help' for a longer description.\n"; 
	$help .= $help_hash{Post_help_message} . "\n";
      } 

      $help =~ s/<\?.*\?>//g;
      $help =~ s/<[^>]*>//g;
      print $help;

      exit;
    }
  }
}

1;
