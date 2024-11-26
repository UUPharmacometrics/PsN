package common_options;

use include_modules;
use Getopt::Long;
use Text::Wrap;
use random;
use Cwd;
use OSspecific;
use PsN;
use ui;
use citations;

## Configure the command line parsing
Getopt::Long::config("auto_abbrev");

@tool_options = ( "abort_on_fail!",
          "accepted_ofv_difference:f",
          "add_retries!",
          "always_datafile_in_nmrun!",
          "check_nmtran!",
          "citations!",
          "clean:i",
          "compress!",
          "condition_number_limit:f",
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
          "nodes:i",
          "parafile:s",
          "picky!",
          "prepend_model_file_name!",
          "quick_summarize|quick_summary",
          "retries:i",
          "rmarkdown!",
          "rplots:i",
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
          "slurm_cluster:s",
          "template_file_rplots:s",
          "template_directory_rplots:s",
          "subset_variable_rplots:s",
          "threads:i",
          "torque_queue:s",
          "torque_prepend_flags:s",
          "tweak_inits!",
          "verbose!",
          "near_bound_sign_digits:i",
          "precision:i",
          "so!",
          "zip!",
          "model_subdir!",
          "debug_rmd!",
          "html!",
          "pdf!",
    );


@both_model_and_tool_options = ("last_est_complete!", "missing_data_token:i", "niter_eonly:i");

@model_only_options = ("extra_files:s",
                  "extra_output:s",
                  "maxevals:i",
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
                  "psn_record_order!",
    );

@model_options = (@both_model_and_tool_options, @model_only_options);

my @script_options = (
    "h|?",
    "help",
    "silent",
    "version",
    "warn_with_trace!",
);

@get_opt_strings = (sort(@tool_options), sort(@model_only_options), sort(@script_options));

@extra_files;
@extra_output;
$parameters = options_to_parameters([@tool_options,'top_tool']);

my $options_ref;

sub get_option
{
    my $option = shift;
    return $options_ref->{$option};
}

sub set_option
{
    my $option = shift;
    my $value = shift;
    $options_ref->{$option} = $value;
}

sub options_to_parameters
{
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

sub setup
{
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

sub store_common_options
{
  $options_ref = shift;
  #global variable options_ref was declared above
}

sub restore_options
{
  my @relevant_options = @_;
  my %options = %{$options_ref}; #store_common_options must have been called
  #in bin/<utility> (e.g. nonpb) for this to work

  my %stored_opts = eval( options_to_parameters( \@relevant_options ) );
  #options_to_parameters returns a string. eval() evaluates this as perl code
  #In the returned string the variable %options, declared in this function,
  #is used for substituting variables ($hash{'key'}) to values.

  return \%stored_opts; #return reference to stored_opts
}

sub set_globals
{
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

    $Carp::Verbose = $options{'warn_with_trace'};
}

sub get_defaults
{
  my $options = shift;
  my $tool    = shift;

  my $nm_string='';
  if (exists $options -> {'nm_version'}){
      #if nm_version is set on the command-line, even if set explicitly to 'default'.
      # We do nothing if nm_version is set in config file, then this feature is not invoked
      $nm_string = $options -> {'nm_version'};
  }

  my $try_local_R_template_dir;
  my $warn_R_template = 0;
  if (exists $options -> {'template_directory_rplots'}){
      $warn_R_template = 1;
  }else{
      #template directory not set on commandline
      $try_local_R_template_dir=getcwd();
      if (exists $options -> {'template_file_rplots'}){
          $warn_R_template = 1;
      }
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

  unless( exists $options -> {'template_file_rplots'} ){
      $options -> {'template_file_rplots'} = $tool.'_default.R';
  }
  if (defined $try_local_R_template_dir){
      #check if template file exists relative local directory when template directory was not set on commandline
      #local should be used before anything set in psn.conf
      #what happens if global path template file set??? should work
      my ($dir,$file) = OSspecific::absolute_path($try_local_R_template_dir,$options->{'template_file_rplots'});
      if (-e $dir.$file){
          $options->{'template_directory_rplots'} = $try_local_R_template_dir;
      }
      if (-e $dir.$file.'md'){ # check if .Rmd file exists in in local R template directory
          $options->{'template_directory_rplots'} = $try_local_R_template_dir;
          $options -> {'template_file_rplots'} = $tool.'_default.Rmd';
      }
  }
  if( exists $options -> {'template_directory_rplots'} ){
      #make sure path is absolute if it was not already
      my ($dir,$file) = OSspecific::absolute_path($options -> {'template_directory_rplots'},'file');
      $options -> {'template_directory_rplots'} = $dir;
  }else{
      $options -> {'template_directory_rplots'} = $PsN::Rscripts_dir;
  }

  #check if there is an .Rmd file
  my ($dir, $file) = OSspecific::absolute_path($options->{'template_directory_rplots'},$options->{'template_file_rplots'});
  if (-e $dir.$file.'md'){
      $options -> {'template_file_rplots'} = $tool.'_default.Rmd';
  }

  if ($warn_R_template ){
      my ($dir, $file) = OSspecific::absolute_path($options->{'template_directory_rplots'},$options->{'template_file_rplots'});
      my $template_file = $dir.$file;
      my $template_file_Rmd = $dir.$file.'md';
      unless ((-e $template_file) || (-e $template_file_Rmd)){
          croak ("template_file_rplots ".$options->{'template_file_rplots'}." does not exist in ".$options->{'template_directory_rplots'})
      }
  }


}

sub sanity_checks
{
    my $options = shift;
    my $tool = shift;

    if (defined $options->{'degree'}) {
        if ($options->{'degree'} <= 0) {
            croak("option degree must be a positive number.\n");
        }
    }

    if (defined $options->{'silent'} and $options->{'silent'}){
        $options->{'display_iterations'}=0;
    }

    if (not $options->{'nmfe'}) {
        #assume user wants nmfe if none set
        $options->{'nmfe'} = 1;
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
        if( $options -> {'psn_record_order'} ){
            die "You cannot set both sde and psn_record_order";
        }
    }
    if( $options -> {'omega_before_pk'} ){
        if( $options -> {'psn_record_order'} ){
            die "You cannot set both omega_before_pk and psn_record_order";
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
        my @options_list = qw(background prsame prdefault prcompile trskip xmloff maxlim tprdefault locfile);
        foreach $opt (@options_list) {
            if ($options->{'nmfe_options'} =~ /\A$opt\Z/) {
                print "\nWarning: You have set -nmfe_options=".$options->{'nmfe_options'}." on the commandline or in psn.conf\n";
                print "This does not look a valid option to the nmfe script (the - sign is missing).\n";
                print "Note that PsN4 will pass this on unchanged to the nmfe script, for help use\nexecute -help nmfe_options\n\n";
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

sub print_help
{
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

sub model_parameters
{
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

sub online_help
{

  my $command = shift;
  my $opts = shift;
  my $help_text = shift;
  my $required_options = shift;
  my $optional_options = shift;
  my %options = %{$opts};

  my %help_hash;

    $help_hash{'-?'} = <<'EOF';
    -h | -?

    With -h or -? the script prints the list of available options
    and exit.
EOF

    $help_hash{-help} = <<'EOF';
    -help
    With -help a longer help message will be printed.
EOF

    $help_hash{-nm_version} = <<'EOF';
    -nm_version='string'
    If you have more than one installation of NONMEM you can choose
    between them using the -nm_version option. The installations must be
    specified in the psn.conf file.
EOF
    $help_hash{-check_nmtran} = <<'EOF';
    -check_nmtran
    Make PsN run NMtran on the model file before submitting the complete nmfe run to a cluster/grid
    or forking on a local computer. This adds a bit of overhead but on a cluster this still
    saves time in the case of syntax errors in the model file, since the user does not
    have to wait for a slot on the cluster/grid before the error is detected.
    On a local computer the error handling is improved.

    When running multiple copies of a model with different data sets, e.g. in a bootstrap,
    only the first model will be checked.

    If the model contains verbatim Fortran code NMTRAN will not be able to detect undefined variables
    in abbreviated code. This can potentially lead to errors that are very hard to detect. In this
    case PsN will try to detect undefined variables in abbreviated code for you. If a variable is
    suspected to be undefined PsN will print a warning telling the user to double check that the
    variable is defined. If the variable is defined in verbatim code then the warning will be printed
    even if all is well, but this information is included in the warning.

    The nmtran check requires that it is the installation directory to NONMEM that is set in psn.conf,
    rather than the full path to an executable script. If the path to a script is given instead
    of an NM install directory the nmtran check will not be performed.
EOF
    $help_hash{-niter_eonly} = <<'EOF';
    -niter_eonly

      Only applies if NONMEM7 and last $EST is IMP or IMPMAP. Only for scripts
      vpc (any options), cdd (if option -xv) and execute (if option -mirror_plots).
      Undefined by default.
      User-chosen value of NITER in IMP or IMPMAP when estimation is turned off
      by setting EONLY=1. See PsN.pdf for details.
EOF

    $help_hash{-last_est_complete} = <<'EOF';
    -last_est_complete

      Only applies if NONMEM7 and multiple $ESTIMATION records. Then only for scripts
      vpc (any options), cdd (if option -xv) and execute (if option -mirror_plots).
      Setting this option tells PsN that all
      options needed for the last estimation step, such as LIKELIHOOD
      (defaults can be omitted), are set explicitly in the
      last $ESTIMATION record, i.e. no options need to carry over from previous $EST.
      Then PsN will skip the step where all $ESTIMATION records are searched for
      relevant options before removing all but the last $ESTIMATION and setting
      MAXEVAL=0 or EONLY=1. More details in the userguide.
EOF

    $help_hash{-nmfe} = <<'EOF';
    -nmfe
    Default set.
    Invoke NONMEM via the nmfe script (or a custom wrapper) from within PsN.
    Option -nmfe is set automatically. Also, -nmfe is set in the default configuration file.
EOF
    $help_hash{-nmfe_options} = <<'EOF';
    -nmfe_options='options for nmfe'
    Only relevant if NONMEM7.2 or later is used.
    The text set with this option will be copied verbatim to the nmfe script call.
    PsN will not check that the options are appropriate. When set on the PsN commandline
    the string must be enclosed by quotes if it contains any spaces, but when set in
    psn.conf it must never be enclosed by quotes even if it contains spaces.
    On unix-type systems, but not on windows, any parentheses must be escaped with backslash.
  Example:
    -nmfe_options="-xmloff -prdefault"
    or in psn.conf
    nmfe_options=-xmloff -prdefault
EOF

    $help_hash{-threads} = <<'EOF';
    -threads='integer'
    Use the threads option to enable parallel execution of multiple
    NONMEM runs. On a desktop computer it is recommended to set
    -threads to the number of CPUs in the system plus one. You can
    specify more threads, but it will probably not increase the
    performance. If you are running on a computer cluster, you should
    consult your system administrator to find out how many threads
    you can specify. The -threads option will be ignored if you run on
    a grid system, since grids have their own scheduling algoritms. The
    default value for the -threads option is 1.
EOF
    $help_hash{-rplots} = <<'EOF';
    -rplots='integer'
    Automatically create R plots to visualize results when a template file is available,
    either provided by the user via option -template_file_rplots or in the default set of template files
    for a subset of the PsN tools.
    When a template file is available, the R script will always be generated and saved in the main
    run directory. If R is configured in psn.conf or command 'R' is available and rplots is set > 0 the script will
    also be run and a number of pdf-format plots be created.
    -rplots<0 means no script is generated
    -rplots=0 (default) means script is generated but not run
    -rplots=1 means basic plots are generated
    -rplots=2 means basic and extended plots are generated
EOF
    $help_hash{-template_directory_rplots} = <<'EOF';
    -template_directory_rplots=path
    PsN can look for the rplots template file in a number of places. The priority order is the
    following :
    1) template_directory_rplots set on command-line
    2) calling directory (where PsN is started)
    3) template_directory_rplots set in psn.conf
    4) R-scripts subdirectory of the PsN installation directory
EOF
    $help_hash{-template_file_rplots} = <<'EOF';
    -template_file_rplots=file
    When the rplots feature is used, the default template PsN will use is <toolname>_default.R,
    for example scm_default.R. The user can choose a different template file
    by setting option -template_file_rplots to a different file.
    PsN will first look for the file relative to the current working directory,
    and after that in the -template_directory_rplots directory.
EOF
    $help_hash{-subset_variable_rplots} = <<'EOF';
    -subset_variable_rplots=name
    Default not set. The user can specify a subset variable to be used with the rplots feature. This variable
    will, if set, be used in for example the execute default R template to create separate plots for
    subsets of the data, via xpose options 'subset' and 'by'. The user must ensure that the variable
    is printed to one of the xpose tables, for example sdtab.
EOF

    $help_hash{-nice} = <<'EOF';
    -nice='integer'
    This option only has effect on unix like operating systems. It
    sets the priority (or nice value) on a process. You can give any
    value that is legal for the "nice" command, likely it is between 0
    and 19, where 0 is the highest priority. Check "man nice" for
    details.
EOF

    $help_hash{-debug_rmd} = <<'EOF';
    -debug_rmd
    Turn on to have rplots retain the .tex file for debugging.
EOF

    $help_hash{-html} = <<'EOF';
    -html
    Render to html. This option makes rplots not dependent on LaTeX being available.
    Can be combined with -pdf
EOF

    $help_hash{-pdf} = <<'EOF';
    -pdf
    Render to pdf. This is the default. Can be combined with -html
EOF

    $help_hash{-display_iterations} = <<'EOF';
    -display_iterations
    This option turns on display the iterations output from NONMEM during
    the model run. The option can be disabled with -no-display_iterations.
EOF

    $help_hash{-directory} = <<'EOF';
    -directory='string'
    The directory option sets the directory in which PsN will run
    NONMEM. The default directory name is 'modelfit_dirX' for execute,
    where X will be increased by one each time you run the script.
    For other scripts/tools, the default is 'toolname_dirX', for
    example bootstrap_dir1. You do not have to create the directory,
    it will be done for you.

    If you abort the run or if your system crashes you can use the
    '-directory' option set to the directory of the run that
    crashed. PsN will then not run the modelfiles that had
    finished before the crash, thereby saving some time. Notice that
    is important that you give exactly the same options that you gave
    the first time (exception npc and vpc tools, see npc manual).
EOF


    $help_hash{-extra_files} = <<'EOF';
    -extra_files=mysubroutine.f90,run10.phi
    If you need extra files in the directory where NONMEM is run you
    specify them in the comma separated -extra_files list. It could for
    example be fortran subroutines you need compiled with NONMEM,
    or a phi-file set in record $ETAS with option FILE.
EOF

    $help_hash{-maxevals} = <<'EOF';
    -maxevals=100000
    NONMEM only allows 9999 function evaluations. PsN can expand this
    limit by adding an MSFO option to $ESTIMATION. Later when NONMEM
    hits the max number of function evaluations allowed by NONMEM (9999)
    PsN will remove intial estimates from the modelfile and add $MSFI
    and restart NONMEM. This will be repeated until the number of function
    evaluations specified with option maxevals has been reached.
EOF

    $help_hash{-seed} = <<'EOF';
    -seed='string'
    You can set your own random seed to make PsN runs reproducible.
    The random seed is a string, and may include spaces if the whole string
    is enclosed with single    quotes as in -seed='123 abc'. It is important to
    know that, because of the way the Perl pseudo-random number generator works,
    for two similar string seeds the random sequences may be identical.
    This is the case e.g. with the two different seeds 123 and 122.
    From limited tests it seems as if the final character is ignored and a work around
    to be sure to set different seeds would be to add a dummy final character. 
    Setting the same seed guarantees the same sequence, but setting two slightly different
    seeds does not guarantee two different random sequences, that must be verified.
EOF

    $help_hash{-verbose} = <<'EOF';
    -verbose
    With verbose set to 1, PsN will print
    more details about NONMEM runs. More precisely PsN will print the
    minimization message for each successfull run and a R:X for each
    retry PsN makes of a failed run, where X is the run number.
EOF

    $help_hash{-lsf_job_name} = <<'EOF';
    -lsf_job_name='string'
    lsf_job_name sets the name of the LSF job name of every NONMEM run,
    they all get the same name.
EOF

    $help_hash{-lsf_options} = <<'EOF';
    -lsf_options='string'
    LSF jobs are submitted using bsub and all LSF related options are
    translated to corresponding bsub options. For maximum flexibility
    we allow any string to be passed as options to bsub, so if a specific
    bsub feature not available through any ot the other -lsf_ options
    is needed, use lsf_options to pass any option to bsub.
EOF

    $help_hash{-lsf_project_name} = <<'EOF';
    -lsf_project_name='string'
    Use lsf_project_name to assign a
    project name to your LSF runs.
EOF

    $help_hash{-lsf_resources} = <<'EOF';
    -lsf_resources='string'
    lsf_resources specifies which LSF resources is required when submiting
    NONMEM runs.
EOF

    $help_hash{-lsf_ttl} = <<'EOF';
    -lsf_ttl='string'
    lsf_ttl sets the maximum time a NONMEM run should be allowed to run on
    the LSF grid.
EOF
    $help_hash{-lsf_sleep} = <<'EOF';
    -lsf_sleep='string'
    Pause for this many seconds after bsub submission, before continuing running PsN.
EOF

    $help_hash{-lsf_queue} = <<'EOF';
    -lsf_queue='string'
    lsf_queue specifies which LSF queue PsN should submit NONMEM runs
    to and is used in conjuction with -run_on_lsf
EOF

    $help_hash{-min_retries} = <<'EOF';
    -min_retries='integer'
    min_retries forces PsN to try
    several initial values for each estimate and selecting the best
    one. The best model is the one with highest number of significant
    digits and an ofv value no more than five units above than the
    lowest ofv value among all models. If -picky
    is used, only models which first pass the picky test will be considered.
EOF

$help_hash{-clean} = <<'EOF';
    -clean='integer 0-5'

    Default is 1. The clean option can take six different values
    The -clean clean option can take four different values:
    0 - Nothing is removed
    1 - NONMEM binary and intermediate files except INTER are removed, and files specified with option -extra\_files.
    2 - model and output files generated by PsN restarts are removed, and data files in the NM\_run directory.
    3 - All NM\_run directories are completely removed. If the PsN tool has created modelfit\_dir:s inside the main run directory, these  will also be removed.
    4 - All NM\_run directories and all m1 directories are completely removed.
    5 - The entire run directory is removed. This is only useful for execute. The lst-file will be copied even if the run failed.
EOF

    $help_hash{-missing_data_token} = <<'EOF';
    -missing_data_token='string'
    missing_data_token sets the number
    that PsN accepts as missing data, default is -99.
EOF

    $help_hash{-compress} = <<'EOF';
    -compress
    PsN will compress the contents of 'NM_runX' to the
    file 'nonmem_files.tgz' if the -compress option is used and if you
    have the archive and compress programs tar and gzip installed. If
    you use the -clean options, run files will be
    removed before the compression. The -compress option obviously has
    no effect if you also use the -clean option.
EOF

    $help_hash{-tweak_inits} = <<'EOF';
    -tweak_inits
    If NONMEM terminates nonsuccessfully, PsN can perturb the initial
    estimates and run NONMEM again. The generation of new initial
    estimates init_i for the i:th retry are performed according to

    init_i = init_0 + rand_uniform(+-degree*init_0)

    where init_0 are the initial estimates of the original run and
    degree is set with option degree. The
    updating procedure makes sure that boundary conditions on the
    parameters are still valid. For this option to have effect, the
    -retries option must be set to number larger than zero. The
    default setting of tweak_inits is 'on'.
EOF

    $help_hash{-outputfile} = <<'EOF';
    -outputfile='string'
    The -outputfile option specifies the output file name for the
    NONMEM run. Currently this option is only valid when a single
    model is supplied.
EOF

$help_hash{-parafile} = <<'EOF';
    -parafile='filename'
    NONMEM 7.2 (or later) parafile. Appends "-parafile=filename"
    to the nmfe call, and makes PsN copy 'filename' to the NM\_run directory.
    Only works if option nmfe is set. Note that -nmfe is always set
    automatically, see help for -nmfe.
    Note that the filename must have a full path to work with some tools (i.e. scm).
EOF

$help_hash{-nodes} = <<'EOF';
    -nodes='number'
    Only relevant together with option -parafile. Appends "[nodes]=option_value"
    to the nmfe call. The nodes option is completely independent of the threads
    option. It is possible to e.g. set threads to 1 and nodes to 10.
EOF

    $help_hash{-picky} = <<'EOF';
    -picky
    The -picky option is only valid together with -tweak_inits.
    Normally PsN only tries new initial estimates if
    'MINIMZATION SUCCESSFUL' is not found in the NONMEM output
    file. With the -picky option, PsN will regard any of the
    following messages as a signal for rerunning:

    0ESTIMATE OF THETA IS NEAR THE BOUNDARY
    0PARAMETER ESTIMATE IS NEAR ITS BOUNDARY
    0R MATRIX ALGORITHMICALLY SINGULAR
    0S MATRIX ALGORITHMICALLY SINGULAR
EOF

    $help_hash{'-quick_summarize|quick_summary'} = <<'EOF';
    -quick_summarize or -quick_summary
    If either of quick_summarize and quick_summary is used, PsN will print
    the ofv value and minimization message for each NONMEM run.
EOF

$help_hash{-run_on_lsf} = <<'EOF';
    -run_on_lsf
    PsN connects with Platform Load Sharing Facility (LsF). With
    -run_on_lsf. PsN will submit to the queue defined in "psn.conf"
    unless specified with -lsf_queue.
EOF

    $help_hash{-run_on_ud} = <<'EOF';
    -run_on_ud
    PsN connects with United Devices Grid MP. With -run_on_ud PsN will submit to the UD grid
    with parameters defined in the "uduserconf" file.
EOF

    $help_hash{-retries} = <<'EOF';
    -retries='integer'
    The -retries option tells PsN how many times it
    shall try to rerun a NONMEM job if it fails according to given criterias.
    The -retries option is only valid together with -tweak_inits.
    The default value of the -retries option is 0.
EOF

    $help_hash{-crash_restarts} = <<'EOF';
    -crash_restarts='integer'
    If a NONMEM outputfile is produced but PsN is unable to read it
    properly it is assumed that NONMEM crashed, probably due to
    something in the operating system, and PsN will start the run
    again. But PsN will not consider it a retry and will not change
    initial estimates. The default value is 4.
EOF
    $help_hash{-significant_digits_accept} = <<'EOF';
    -significant_digits_accept='number'
    The -significant_digits_accept option is only valid together with -tweak_inits.
    Normally PsN tries new initial estimates if
    'MINIMZATION SUCCESSFUL' is not found in the NONMEM output
    file. With the -significant_digits_accept, PsN will only rerun if
    the resulting significant digits is lower than the value
    specified with this option.
EOF

    $help_hash{-abort_on_fail} = <<'EOF';
    -abort_on_fail
    If the -abort_on_fail option is set and one of the NONMEM runs
    fails, PsN will stop with an error message. This option
    is mostly for the system tests, where it is known beforehand that no
    NONMEM runs should fail if there are no bugs in PsN.
EOF
    $help_hash{-add_retries} = <<'EOF';
    -add_retries

    By default, PsN will never do retries on a model when a run is restarted if
    the file stats-runs.csv is found in the NM_run subdirectory, since the
    existence of this file indicates that all retries have finished and the
    best try has been selected. If option -add_retries is set, PsN will ignore
    that stats-runs.csv exists, and check again if retries are needed based on
    the existing tries in the NM_run directory. This makes it possible to restart
    a run using different settings for retries (e.g. -retries, -min_retries, -picky).
EOF
    $help_hash{-always_datafile_in_nmrun} = <<'EOF';
    -always_datafile_in_nmrun

    Not set by default.
    By default, PsN will often not copy the datafile to the NM_run
    subdirectories, but instead include the path to the datafile in $DATA in
    the control stream copy inside NM_run. This is the case in for example
    the bootstrap and randtest programs.
    If -always_datafile_in_nmrun is set, then PsN will always copy
    the datafile to NM_run and set the datafile name without path
    in $DATA. This behaviour may be useful when running on
    a grid where only the contents of NM_run are available to NONMEM at
    runtime. Option -always_datafile_in_nmrun will override -no-copy_data,
    if -no-copy_data is set.
EOF

    $help_hash{-silent} = <<'EOF';
    -silent
    The silent option turns off all output from PsN. Results and log
    files are still written to disk, but nothing is printed to the
    screen.
EOF

    $help_hash{-warn_with_trace} = <<'EOF';
    -warn_with_trace
    If -warn_with_trace is set, PsN will print a stack
    trace for all warning and error messages.
    This is only for developers.
EOF

    $help_hash{-sde} = <<'EOF';
    -sde
    Default not set. In PsN version 3.4.4 and earlier, this option made PsN print the records
    in a particular order suitable for SDE models.
    The new default is to keep the record order of the input model file. To use the old
    SDE print order, set option -sde.
EOF

    $help_hash{-psn_record_order} = <<'EOF';
    -psn_record_order
    If this option is set the build in record order of PsN will be used. Default is
    to preserve the record order of the input model. This option is mainly present
    for backward compatibility reasons.
EOF

    $help_hash{-omega_before_pk} = <<'EOF';
    -omega_before_pk
    Default not set. In PsN version 3.4.4 and earlier, $OMEGA was always printed before $PK.
    The new default is to keep the record order of the input model file. To use the old print order,
    set option -omega_before_pk.
EOF

    $help_hash{-condition_number_limit} = <<'EOF';
    -condition_number_limit='number'
    An error will be raised in the output from sumo
    if the condition number is greater than this number.
EOF

    $help_hash{-nm_output} = <<'EOF';
    -nm_output='comma-separated list of file extensions'

    NONMEM generates many output files per run. The lst-file will always be
    copied back to the calling directory. The option -nm_output decides which of the
    additional files should be copied back to the calling directory. The default is none.
    NM output files which are not copied to the calling directory can still be
    found inside the run directory.

  Example: -nm_output=ext,cov

EOF

    $help_hash{-accepted_ofv_difference} = <<'EOF';
    -accepted_ofv_difference='number'
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
    -handle_msfo
    Experimental feature for handling resumes using msfo and msfi files.
EOF

    $help_hash{-handle_crashes} = <<'EOF';
    -handle_crashes
    PsN tries to recognize NONMEM runs that crashed for various reasons,
    e.g. a computer crash or a NONMEM run deliberaterly killed, and restart
    those runs without changing initial parameter estimates.
EOF

    $help_hash{-large_theta_cv_limit} = <<'EOF';
    -large_theta_cv_limit='number'
    Coefficients of variation larger than this number will produce
    warnings in the output from sumo.
EOF

    $help_hash{-large_omega_cv_limit} = <<'EOF';
    -large_omega_cv_limit='number'
    Coefficients of variation larger than this number will produce
    warnings in the output from sumo.
EOF

    $help_hash{-large_sigma_cv_limit} = <<'EOF';
    -large_sigma_cv_limit='number'
    Coefficients of variation larger than this number will produce
    warnings in the output from sumo.
EOF

    $help_hash{-max_runtime} = <<'EOF';
    -max_runtime='string'
    A limit on how long a slurm run may go on before being aborted
    (option -t to sbatch). Format is either minutes, e.g. -max_runtime=10,
    or hours:minutes:seconds, e.g. -max_runtime=4:0:0, or days-hours,
    e.g. -max_runtime=3-0
EOF

    $help_hash{-near_bound_sign_digits} = <<'EOF';
    -near_bound_sign_digits='integer'
    If a parameter estimate is equal to a bound with this many
    significant digits, a warning will be printed. Valid only with sumo.
EOF

    $help_hash{-prepend_model_file_name} = <<'EOF';
    -prepend_model_file_name
    Table files by default have generic names, e.g. patab. If multiple
    models are run, files will be overwritten when multiple files
    with the same name are copied back to the same directory.
    This options prevents this by prepending the model file name,
    without extension, thus making the file names unique.
EOF

    $help_hash{-run_on_sge} = <<'EOF';
    -run_on_sge
    Submit jobs using qsub and sge options
EOF
    $help_hash{-run_on_slurm} = <<'EOF';
    -run_on_slurm
    Submit jobs using sbatch and slurm options
EOF

    $help_hash{-run_on_torque} = <<'EOF';
    -run_on_torque
    Use Torque batch queueing system.
EOF

    $help_hash{-torque_queue} = <<'EOF';
    -torque_queue='string'
    Only valid with -run_on_torque. Maps to qsub option -q
EOF

    $help_hash{-torque_prepend_flags} = <<'EOF';
    -torque_prepend_flags='string'
    Only valid with -run_on_torque. The - signs must be included in the
    string. The extra flags will be prepended to standard set in qsub call.

EOF

    $help_hash{-run_on_zink} = <<'EOF';
    -run_on_zink
    Experimental clustering on Windows machine.
EOF

    $help_hash{-sge_resource} = <<'EOF';
    -sge_resource='string'
    Only valid with -run_on_sge. Maps to qsub option -l
EOF

    $help_hash{-sge_queue} = <<'EOF';
    -sge_queue='string'
    Only valid with -run_on_sge. Maps to qsub option -q
EOF

    $help_hash{-sge_prepend_flags} = <<'EOF';
    -sge_prepend_flags='string'
    Only valid with -run_on_sge. The - signs must be included in the
    string. The extra flags will be prepended to standard set in qsub call.

EOF
    $help_hash{-slurm_prepend_flags} = <<'EOF';
    -slurm_prepend_flags='string'
    Only valid with -run_on_slurm. The - signs must be included in the
    string. The extra flags will be prepended to standard set in sbatch call.

EOF
    $help_hash{-slurm_account} = <<'EOF';
    -slurm_account='string'
    Only valid with -run_on_slurm. Maps to sbatch -A option.

EOF

$help_hash{-slurm_cluster} = <<'EOF';
    -slurm_cluster='string'
    Only valid with -run_on_slurm. Maps to sbatch -M option
    and is also used when monitoring slurm job.
EOF

    $help_hash{-slurm_partition} = <<'EOF';
    -slurm_partition='string'
    Only valid with -run_on_slurm. Maps to sbatch -p option.

EOF

    $help_hash{-send_email} = <<'EOF';
    -send_email
    Only valid with -run_on_slurm and -email_address in combination, otherwise ignored.
    Determines if sbatch options --mail_type=ALL --mail_user=<email_address> should be set.
EOF

    $help_hash{-email_address} = <<'EOF';
    -email_address='string'
    Only valid with -run_on_slurm and -send_email in combination, otherwise ignored.
    The email address in sbatch options --mail_type=ALL --mail_user=<email_address>.
EOF
    $help_hash{-shrinkage} = <<'EOF';
    -shrinkage
    Calculate the shrinkage for the model run.  Shrinkage is
    calculated as 1-(sd(eta(x))/omega(x)) and measures the shrinkage of the
    empirical Bayes estimates (EBEs) towards the mean of the expected
    distribution.  A 'large' shrinkage means that diagnostics using EBEs cannot
    be trusted.
EOF

    $help_hash{-sign_digits_off_diagonals} = <<'EOF';
    -sign_digits_off_diagonals='integer'
    The off-diagonal elements are checked against +-1 with this many
    significant digits. Valid only in sumo.
EOF

    $help_hash{-precision} = <<'EOF';
    -precision='integer'
    Precision in sumo output.
EOF
    $help_hash{-degree} = <<'EOF';
    -degree=number
    When tweaking initial estimates in retries/parallel_retries, this number decides the range for the
    new estimates. The new number will be within 'degree'*oldinitial from the old initial estimate,
    unless restricted by upper or lower boundaries.
    A number larger than 0. Default 0.1
EOF

    $help_hash{-extra_output} = <<'EOF';
    -extra_output='file1,file2'
    If NONMEM generates a file which PsN normally does not copy back
    to the working directory, specifying a comma-separated list
    of such files with this options will make PsN copy the listed files.
    An example is output generated by verbatim code.
EOF

    $help_hash{-cwres} = <<'EOF';
    -cwres
    Compute the conditional weighted residuals (CWRES) for a model run.
    Option is only supported for NONMEM5 and NONMEM6. In NONMEM7, CWRES can
    be requested directly from $TABLE.
EOF
    $help_hash{-tbs} = <<'EOF';
    -tbs
    Default not set. Invokes Transform Both Sides method. Model must be coded
    'the Uppsala way', i.e. with IWRES and W and SIGMA 1 FIX.
    See the userguide common_options_defaults_versions_psn for details.
EOF
    $help_hash{-dtbs} = <<'EOF';
    -dtbs
    Default not set. Invokes Dynamic Transform Both Sides method. Model must be coded
    'the Uppsala way', i.e. with IWRES and W and SIGMA 1 FIX.
    See the userguide common_options_defaults_versions_psn for details.
EOF

    $help_hash{-tbs_lambda} = <<'EOF';
    -tbs_lambda
    Default not set. Initial value string, using NM-TRAN syntax,
    for parameter in Transform Both Sides
    method, e.g. '(-1, 0.5, 1)' or 'O FIX'. The string must be enclosed
    in single quotes and not include any comments.
    If tbs_lambda is set then option -tbs will be set automatically
    unless -dtbs or -tbs_delta or -tbs_zeta is set.
    See the userguide common_options_defaults_versions_psn for details.
EOF
    $help_hash{-tbs_zeta} = <<'EOF';
    -tbs_zeta
    Default not set. Initial value string, using NM-TRAN syntax,
    for parameter zeta in Transform Both Sides
    method, e.g. '(-1, 0.5, 1)' or 'O FIX'. The string must be enclosed
    in single quotes and not include any comments.
    Cannot be used in combination with tbs_delta.
    If tbs_zeta is set then option -dtbs will be set automatically.
    See the userguide common_options_defaults_versions_psn for details.
EOF
    $help_hash{-tbs_delta} = <<'EOF';
    -tbs_delta
    Default not set. Initial value string, using NM-TRAN syntax,
    for parameter delta in Transform Both Sides
    method, e.g. '(-1, 0.5, 1)' or 'O FIX'. The string must be enclosed
    in single quotes and not include any comments.
    Cannot be used in combination with tbs_zeta.
    If tbs_delta is set then option -dtbs will be set automatically.
    See the userguide common_options_defaults_versions_psn for details.
EOF

    $help_hash{-mirror_plots} = <<'EOF';
    -mirror_plots='integer'
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
    -iofv
    Compute the individual contributions to the objective function.
    Option is only supported for NONMEM5 and NONMEM6. In NONMEM7, individual ofv
    values can be found in the addtional output phi file.
EOF

    $help_hash{-mirror_from_lst} = <<'EOF';
    -mirror_from_lst
    Can only be used in combination with -mirror_plots=XX where XX is an
    integer representing the number of simulations to perform. These commands
    create a set of simulations from a model file and output file that can
    then be read into Xpose 4 for mirror plotting.  The -mirror_from_lst option
    reads from the *.lst file of a NONMEM run to get final parameter estimates
    for the simulations.
EOF

    $help_hash{-version} = <<'EOF';
    -version
    Print PsN version of script called.
EOF

    $help_hash{-so} = <<'EOF';
    -so

    Create an additional DDMoRe standard output xml file.
    Note that the created file will have SO version 0.3.1
EOF

$help_hash{-zip} = <<'EOF';
    -zip
    All m1 directories will be zipped to save space.
    PsN will automatically unzip the m1 folders if needed.
EOF

$help_hash{-citations} = <<'EOF';
    -citations
    Print a list of references for this tool.
    The list will be in BibTeX format.
EOF

$help_hash{-rmarkdown} = <<'EOF';
    -rmarkdown
    If it is possible the Rmarkdown file instead of
    R file for R plots will be created.
EOF

$help_hash{-model_subdir} = <<'EOF';
    -model_subdir

    Use an alternative directory structure for PsN.
    An extra directory level unique to each model is
    introduced between the calling directory and
    the rundirectory.
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

 if ($options{'citations'}) {
     citations::print_citations();
     exit;
 }

  my $help;
my $indentation = '    ';
  if($options{'h'} or $options{'?'} or $options{'help'} ) {

      if( scalar( @ARGV ) > 0 ){
          foreach my $option ( @ARGV ){
              $help .= format_help_text(\%help_hash,$indentation,'-'.$option);
          }

#do not remove html-like
#          $help =~ s/<\?.*\?>//g;
#          $help =~ s/<[^>]*>//g;
          print $help;
          exit;
      }

$help .= "\n".$command."\n";
      $help .= format_help_text(\%help_hash,$indentation,'Pre_help_message');

      if( $options{'help'} ){

          $help .=format_help_text(\%help_hash,$indentation,'Description');
          $help .=format_help_text(\%help_hash,$indentation,'Examples');
          $help .=format_help_text(\%help_hash,$indentation,'Options');

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
              $help .= format_help_text(\%help_hash,$indentation,'-'.$option);
          }

          $help .= format_help_text(\%help_hash,$indentation,'Post_help_message');

      } else {
          $help .= common_options::print_help($command,$required_options, $optional_options);
          unless ($command eq 'psn'){
              $help .= "\n$indentation"."Options enclosed by [ ] are optional.\n";
              $help .= "$indentation"."Exclamation mark, !, after the option name means option can be disabled\n".
                  "$indentation"."using '-no-option', for example -no-handle_crashes.\n";
          }
          $help .= "$indentation"."Use '$command -help' for a longer description.\n";
          $help .= format_help_text(\%help_hash,$indentation,'Post_help_message');
      }

# do not clean html
#      $help =~ s/<\?.*\?>//g;
#      $help =~ s/<[^>]*>//g;
      print $help;

      exit;
  }
}

sub format_help_text
{
    my $hash = shift;
    my $indentation = shift;
    my $key= shift;

    my $newtext = '';
    return $newtext unless (defined $hash);

    my $text = $hash->{$key};
    if (defined $text and length($text)>0){
        if (($key eq 'Description') or
            ($key eq 'Examples')){
            $newtext .= $key.":\n\n";
        }
        if (($key eq 'Options')){
            $newtext .= "Input:\n\n";
        }

        #split on newline
        my @lines = split("\n",$text);

        my $isoption=1;
        $isoption = 0 unless ($key =~ /^-/);
        my $printcount=0;
        foreach my $line (@lines){
            chomp($line);
            $line =~ s/^\s*//; #leading whitespace
            $line =~ s/\s*$//; #trailing whitespace
            if ($isoption and $printcount==0){
                $newtext .= $line."\n"; #option name
            }elsif($isoption and $printcount==1 and ($line =~ /^\s*$/ )){
                #we have only printed option name, and line is empty
                #skip it
                next;
            }else{
                $newtext .= $indentation.$line."\n";
            }
            $printcount++;
        }
        #extra linebreak at end
        $newtext .= "\n";
    }elsif(exists $hash->{$key}){
        1;
    }elsif($key eq 'Description'){
        1;
    }elsif($key eq 'Pre_help_message'){
        1;
    }elsif($key eq 'Post_help_message'){
        $newtext .= $indentation."Also see 'psn_options -h' for a description of common options.\n";
    }elsif($key eq 'Options'){
        $newtext .= $key.":\n\n".$indentation."The following options are valid:\n\n";
    }elsif($key eq 'Examples'){
        1;
    }elsif (defined $key and ($key =~ /^-/)){
        my $option = $key;
        $newtext = "$key\n\n".$indentation."No help available for '$key'\n\n";
    }
    return $newtext;
}

sub get_option_array
{
    # Convert option string to array
    # Input: String to convert
    # Result: Array or undef if unsuccessful

    my $option_string = shift;
    my @array;

    foreach my $v (split(/,/, $option_string)) {
        if (length($v) > 0) {
            push(@array, $v);
        }
    }
    if (scalar(@array) == 0) {
        return undef;
    }

    return \@array;
}

sub get_option_matrix
{
    # Convert option string to matrix
    # Input: String to convert
    # Output: Matrix (ref to array of refs to arrays) or undef is unsuccessful

    my $option_string = shift;
    my @matrix;

    foreach my $row (split(/:/, $option_string)) {
        my $array = get_option_array($row);
        if (not defined $array) {
            return undef;
        }
        push(@matrix, $array);
    }
    if (scalar(@matrix) == 0) {
        return undef;
    }

    return \@matrix;
}

1;
