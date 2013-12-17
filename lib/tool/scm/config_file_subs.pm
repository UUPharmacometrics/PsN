# {{{ include statements
start include statements
#use Carp;
	use PsN;
use ext::Config::Tiny;
end include statements
# }}}

# {{{ new

start new
    {
      unless( defined $this -> file ){
	croak('You must give a "file" argument to config_file -> new' );
      }
      my %valid_scalar_options;
      my %valid_array_options;
      my %valid_hash_options;
      my %valid_code_options;
      # Get the types of the possible options.
      foreach my $key ( keys %{$this} ){
	if( ref( $this -> $key ) eq 'SCALAR' ) {
	  if( ${$this -> $key} ne '' ){
	    $valid_scalar_options{$key} = $this -> $key;
	  } else {
	    $valid_scalar_options{$key} = 1;
	  }
	  $this -> {$key} = undef; #FIXME
	} elsif( ref( $this -> $key ) eq 'ARRAY' ) {
	  $valid_array_options{$key} = 1;
	  $this -> {$key} = undef; #FIXME
	} elsif( ref( $this -> $key ) eq 'HASH' ) {
	  if( keys %{ $this -> $key } > 0 ){
	    my @list = keys %{ $this -> $key };
	    if( $list[0] eq 'ARRAY' ){
	      $valid_hash_options{$key} = 'ARRAY';
	    } else {
	      carp("Type specification of $key is weird\n" );
	    }
	  } else {
	    $valid_hash_options{$key} = 'SCALAR';
	  }
	  $this -> {$key} = undef; #FIXME
	} elsif( ref( $this -> $key ) eq 'CODE' ){
#	  print "Found a valid code option $key\n";
	  $valid_code_options{$key} = 1;
	  $this -> {$key} = undef; #FIXME
	}
      }
      $this -> valid_scalar_options(\%valid_scalar_options);
      $this -> valid_array_options(\%valid_array_options);
      $this -> valid_hash_options(\%valid_hash_options);
      $this -> valid_code_options(\%valid_code_options);

      my $string;
      open( FILE, $this -> file -> full_name );
      while( <FILE> ){
	s/\s*\\\s*$/\\/;
	s/[\t\r\f]*//g;
	s/^\s*//;
	$string .= $_ ;
      }
      close( FILE );

      my $config_tiny = ext::Config::Tiny -> read_string( $string );

      unless( defined $config_tiny ){
	croak("In configuration file [ " . $this -> file -> name . " ]: " . $ext::Config::Tiny::errstr );
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

      $this -> parse_config( config_tiny => $config_tiny );
    }
end new

# }}}

# {{{ parse_config

start parse_config
    {
      foreach my $section ( keys %{$config_tiny} ) {
	if( $self -> valid_hash_options->{$section} eq 'ARRAY' ){
#	  print "$section hash array\n";
	  foreach my $left_side( keys %{$config_tiny -> {$section}} ){
	    my $right_side = $config_tiny -> {$section} -> {$left_side};
	    
	    if( $section eq 'code' ){
	      $right_side =~ s/\\/\n/g;
#	      print "code\n$right_side\n";
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
#	  print "$section hash scalar\n";
	  
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
end parse_config

# }}}

# }}}


start write
{
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
end write

# {{{ _check_included_relations
start _check_included_relations
{
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
end _check_included_relations
# }}}

# {{{ _check_various
start _check_various
{
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
end _check_various
# }}}


# {{{ parameters
start parameters
    {
      @parameters = keys %{$self -> test_relations};
    }
end parameters
# }}}
