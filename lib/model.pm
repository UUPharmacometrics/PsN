use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package model;
use Carp;
use Digest::MD5 'md5_hex';
use Cwd;
use File::Copy 'cp';
use Config;
use OSspecific;
use Storable;
use Data::Dumper;
use POSIX qw(ceil floor);
use model::shrinkage_module;
use Math::Random qw(random_multivariate_normal);
use Scalar::Util qw(looks_like_number);
my @nm7_extensions = ('.ext','.cov','.cor','.coi','.phi','.phm',
		      '.shk','.grd','.xml','.smt','.rmt');
use debug;


#---------------------------------------------------------------------
#         Used Packages
#---------------------------------------------------------------------
use model::iofv_module;
use model::nonparametric_module;
use model::shrinkage_module;
use output;
use data;
use model::problem;

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'datas' => 'ARRAY', 'outputs' => 'ARRAY',
			'shrinkage_modules' => 'ARRAY', 'nonparametric_modules' => 'ARRAY',
			'iofv_modules' => 'ARRAY', 'active_problems' => 'ARRAY',
			'skip_data_parsing' => 'SCALAR', 'd2u' => 'SCALAR',
			'maxevals' => 'SCALAR', 'cwres' => 'SCALAR',
			'iofv' => 'SCALAR', 'mirror_plots' => 'SCALAR',
			'mirror_from_lst' => 'SCALAR', 'directory' => 'SCALAR',
			'extra_files' => 'ARRAY', 'extra_output' => 'ARRAY',
			'filename' => 'm_SCALAR', 'model_id' => 'SCALAR',
			'ignore_missing_data' => 'SCALAR', 'ignore_missing_files' => 'SCALAR',
			'ignore_missing_output_files' => 'SCALAR',
			'outputfile' => 'SCALAR', 'run_no' => 'SCALAR',
			'sde' => 'SCALAR', 'omega_before_pk' => 'SCALAR',
			'tbs' => 'SCALAR', 'tbs_param' => 'SCALAR',
			'tbs_thetanum' => 'SCALAR', 'synced' => 'SCALAR',
			'target' => 'SCALAR', 'reference_object' => '',
			'last_est_complete' => 'SCALAR', 'niter_eonly' => 'SCALAR',
			'drop_dropped' => 'SCALAR', 'quick_reload' => 'SCALAR',
			'data_ids' => 'ARRAY' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
		$parm{'reference_object'} = undef;
	}
	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->new: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'skip_data_parsing'} = defined $parm{'skip_data_parsing'} ? $parm{'skip_data_parsing'} : 0 unless defined $this -> {'skip_data_parsing'};
	$this -> {'d2u'} = defined $parm{'d2u'} ? $parm{'d2u'} : 0 unless defined $this -> {'d2u'};
	$this -> {'maxevals'} = defined $parm{'maxevals'} ? $parm{'maxevals'} : 0 unless defined $this -> {'maxevals'};
	$this -> {'cwres'} = defined $parm{'cwres'} ? $parm{'cwres'} : 0 unless defined $this -> {'cwres'};
	$this -> {'iofv'} = defined $parm{'iofv'} ? $parm{'iofv'} : 0 unless defined $this -> {'iofv'};
	$this -> {'mirror_plots'} = defined $parm{'mirror_plots'} ? $parm{'mirror_plots'} : 0 unless defined $this -> {'mirror_plots'};
	$this -> {'mirror_from_lst'} = defined $parm{'mirror_from_lst'} ? $parm{'mirror_from_lst'} : 0 unless defined $this -> {'mirror_from_lst'};
	$this -> {'ignore_missing_data'} = defined $parm{'ignore_missing_data'} ? $parm{'ignore_missing_data'} : 0 unless defined $this -> {'ignore_missing_data'};
	$this -> {'ignore_missing_files'} = defined $parm{'ignore_missing_files'} ? $parm{'ignore_missing_files'} : 0 unless defined $this -> {'ignore_missing_files'};
	$this -> {'ignore_missing_output_files'} = defined $parm{'ignore_missing_output_files'} ? $parm{'ignore_missing_output_files'} : 1 unless defined $this -> {'ignore_missing_output_files'};
	$this -> {'run_no'} = defined $parm{'run_no'} ? $parm{'run_no'} : 0 unless defined $this -> {'run_no'};
	$this -> {'sde'} = defined $parm{'sde'} ? $parm{'sde'} : 0 unless defined $this -> {'sde'};
	$this -> {'omega_before_pk'} = defined $parm{'omega_before_pk'} ? $parm{'omega_before_pk'} : 0 unless defined $this -> {'omega_before_pk'};
	$this -> {'tbs'} = defined $parm{'tbs'} ? $parm{'tbs'} : 0 unless defined $this -> {'tbs'};
	$this -> {'synced'} = defined $parm{'synced'} ? $parm{'synced'} : 0 unless defined $this -> {'synced'};
	$this -> {'target'} = defined $parm{'target'} ? $parm{'target'} : 'mem' unless defined $this -> {'target'};
	$this -> {'last_est_complete'} = defined $parm{'last_est_complete'} ? $parm{'last_est_complete'} : 0 unless defined $this -> {'last_est_complete'};
	$this -> {'drop_dropped'} = defined $parm{'drop_dropped'} ? $parm{'drop_dropped'} : 0 unless defined $this -> {'drop_dropped'};
	$this -> {'quick_reload'} = defined $parm{'quick_reload'} ? $parm{'quick_reload'} : 0 unless defined $this -> {'quick_reload'};

	bless $this, $class;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($this). '-> new');
# line 135 "lib/model_subs.pm" 
    {
	if ( defined $parm{'problems'} ) {
	    $this->problems($parm{'problems'});
	} else {
		my $dir;
		($dir, $this -> {'filename'}) = OSspecific::absolute_path( $this->directory, $this -> {'filename'} );    #FIXME: Nonstandard accessor. Fix with Moose
		$this->directory($dir);
		$this -> _read_problems;
		$this->synced(1);
	}

	if ($this->maxevals > 0){
	  if ( defined $this->problems ) {
	    my $n_prob = scalar(@{$this->problems});
	    for (my $probnum=1; $probnum<= $n_prob; $probnum++){
	      unless ($this->is_option_set(record=>'estimation',
					   name=>'MSFO',
					   problem_number => $probnum,
					   fuzzy_match => 1)){
		$this->add_option(record_name=>'estimation',
				  option_name=>'MSFO',
				  problem_numbers=> [$probnum],
				  option_value => ( $probnum == 1 ? 'psn_msf':('psn_msf_pr'.$probnum)), 
				  add_record=>0);
	      }
	    }
	  }
	}

	if ( defined $parm{'active_problems'} ) {
	    $this->active_problems($parm{'active_problems'});
	} elsif ( defined $this->problems ) {
	    my @active = ();
	    for ( @{$this->problems} ) {
				push( @active, 1 );
	    }
	    $this->active_problems(\@active);
	}

	if ( defined $this->extra_files ) {
	  for( my $i; $i < scalar @{$this->extra_files}; $i++ ) {
	    my ( $dir, $file ) = OSspecific::absolute_path( $this->directory, $this->extra_files->[$i]);
	    $this->extra_files->[$i] = $dir . $file;
	  }
	}
	
	# Read datafiles, if any.
        unless( defined $this -> {'datas'} and not $this->quick_reload ) {		# FIXME: Nonstandard accessor. Fix with Moose.
	    my @idcolumns = @{$this -> idcolumns};
	    my @datafiles = @{$this -> datafiles('absolute_path' => 1)};
	    for ( my $i = 0; $i <= $#datafiles; $i++ ) {
	      my $datafile = $datafiles[$i];
	      if ($this->d2u() and -e $datafile){
		my $doit= `od -c $datafile | grep -c '\\r'`;
		chomp ($doit);
		if ($doit > 0){
		  print "Converting $datafile using dos2unix\n";
		  system("dos2unix -q $datafile");
		}
	      }
	      my $idcolumn = $idcolumns[$i];
	      my $ignoresign = defined $this -> ignoresigns ? $this -> ignoresigns -> [$i] : undef;
	      my @model_header = @{$this->problems->[$i]->header};
	      #should the model header not be used here?
	      if ( defined $idcolumn ) {
		push ( @{$this -> {'datas'}}, data ->
		       new( idcolumn             => $idcolumn,
			    filename             => $datafile,
			    ignoresign           => $ignoresign,
			    directory            => $this->directory,
			    ignore_missing_files => $this->ignore_missing_files || $this->ignore_missing_data,
			    skip_parsing         => $this->skip_data_parsing,
			    target               => $this -> {'target'}) );		#FIXME: Nonstandard accessor. Fix with Moose
	      } else {
		croak("New model to be created from ".$this -> full_name().
				". Data file is ".$datafile.
				". No id column definition found in the model file." );
	      }
	    }
	}

	# Read outputfile, if any.
        if ( !defined $this->outputs ) {
					unless( defined $this -> {'outputfile'} ){		# FIXME: Nonstandard accessor. Fix with Moose.
						if( $this -> filename() =~ /\.mod$/ ) {
							($this -> {'outputfile'} = $this -> {'filename'}) =~ s/\.mod$/.lst/;
						} else {
							$this -> outputfile( $this -> filename().'.lst' );
						}
					}
					$this->outputs([]);
					push ( @{$this->outputs}, output ->
							new( filename             => $this -> {'outputfile'},
								directory            => $this->directory,
								ignore_missing_files =>
								$this->ignore_missing_files || $this->ignore_missing_output_files,
								target               => $this -> {'target'}));
				}

	# Adding mirror_plots module here, since it can add
	# $PROBLEMS. Also it needs to know wheter an lst file exists
	# or not.

	if( $this->mirror_plots > 0 ){
	  my $mirror_plot_module = model::mirror_plot_module -> new( base_model => $this, 
								     nr_of_mirrors => $this->mirror_plots,
								     cwres => $this->cwres,
								     mirror_from_lst => $this->mirror_from_lst,
								     niter_eonly => $this->niter_eonly,
								     last_est_complete => $this->last_est_complete);
	  push( @{$this -> {'mirror_plot_modules'}}, $mirror_plot_module );    #FIXME: Should have had an accessor. Fix with Moose
	}

	if ( $this->iofv > 0 ) {
	  my $iofv_module = model::iofv_module -> new( base_model => $this);
		$this->iofv_modules([]) unless defined $this->iofv_modules;
	  push( @{$this->iofv_modules}, $iofv_module );
	}

      }
# line 235 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($this). '-> new');
	# End of Non-Dia code #

	return $this;
};

sub problems {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'problems'} = $parm;
	} else {
		return $self -> {'problems'};
	}
}

sub datas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> datas');
# line 1120 "lib/model_subs.pm" 
      {
	my $nprobs = scalar @{$self->problems};
	if ( defined $parm ) {
	  if ( ref($parm) eq 'ARRAY' ) {
	    my @new_datas = @{$parm};
	    # Check that new_headers and problems match
	    croak("The number of problems $nprobs and".
			    " new data ". ($#new_datas+1) ." don't match in ".
			    $self -> full_name ) unless ( $#new_datas + 1 == $nprobs );
	    if ( defined $self->problems ) {
	      for( my $i = 0; $i < $nprobs; $i++ ) {
		$self -> _option_name( position	  => 0,
				       record	  => 'data',
				       problem_number => $i+1,
				       new_name	  => $new_datas[$i] -> filename);
	      }
	    } else {
	      croak("No problems defined in ".
			      $self -> full_name );
	    }
	  } else {
	    croak("Supplied new value is not an array" );
	  }
	}
      }
# line 288 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> datas');
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'datas'} = $parm;
	} else {
		return $self -> {'datas'};
	}
}

sub outputs {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'outputs'} = $parm;
	} else {
		return $self -> {'outputs'};
	}
}

sub shrinkage_modules {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> shrinkage_modules');
# line 339 "lib/model_subs.pm" 
{
  if( defined $parm ){
    if( ref $parm ne 'ARRAY' 
	or
	not ( scalar @{$parm} == scalar @{$self->problems} ) ){
      croak('New number of shrinkage modules must be equal to number of problems' );
    }
    my $probnum = 0;
    foreach my $prob( @{$self->problems} ){
      $probnum++;
      my $new_module = shift( @{$parm} );
      $new_module -> nomegas( $self -> nomegas() -> [$probnum-1] );
      $new_module -> directory( $self -> directory() );
      $new_module -> problem_number( $probnum );
      my $data = $self -> datas -> [0];
      $prob -> shrinkage_module( $new_module );
    }

  } else {
    my @return_array;
    foreach my $prob( @{$self->problems} ){
      push( @return_array, $prob -> shrinkage_module );
    }
    return \@return_array;
  }
}
# line 346 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> shrinkage_modules');
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'shrinkage_modules'} = $parm;
	} else {
		return $self -> {'shrinkage_modules'};
	}
}

sub nonparametric_modules {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nonparametric_modules'} = $parm;
	} else {
		return $self -> {'nonparametric_modules'};
	}
}

sub iofv_modules {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'iofv_modules'} = $parm;
	} else {
		return $self -> {'iofv_modules'};
	}
}

sub active_problems {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'active_problems'} = $parm;
	} else {
		return $self -> {'active_problems'};
	}
}

sub skip_data_parsing {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'skip_data_parsing'} = $parm;
	} else {
		return $self -> {'skip_data_parsing'};
	}
}

sub d2u {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'d2u'} = $parm;
	} else {
		return $self -> {'d2u'};
	}
}

sub maxevals {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'maxevals'} = $parm;
	} else {
		return $self -> {'maxevals'};
	}
}

sub cwres {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'cwres'} = $parm;
	} else {
		return $self -> {'cwres'};
	}
}

sub iofv {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'iofv'} = $parm;
	} else {
		return $self -> {'iofv'};
	}
}

sub mirror_plots {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'mirror_plots'} = $parm;
	} else {
		return $self -> {'mirror_plots'};
	}
}

sub mirror_from_lst {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'mirror_from_lst'} = $parm;
	} else {
		return $self -> {'mirror_from_lst'};
	}
}

sub directory {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'directory'} = $parm;
	} else {
		return $self -> {'directory'};
	}
}

sub extra_files {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'extra_files'} = $parm;
	} else {
		return $self -> {'extra_files'};
	}
}

sub extra_output {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'extra_output'} = $parm;
	} else {
		return $self -> {'extra_output'};
	}
}

sub filename {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> filename');
# line 4503 "lib/model_subs.pm" 
      {
	if ( defined $parm and $parm ne $self -> {'filename'} ) {
	  $self -> {'filename'} = $parm;
	  $self -> {'model_id'} = undef;
	}
      }
# line 552 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> filename');
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'filename'} = $parm;
	} else {
		return $self -> {'filename'};
	}
}

sub model_id {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'model_id'} = $parm;
	} else {
		return $self -> {'model_id'};
	}
}

sub ignore_missing_data {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'ignore_missing_data'} = $parm;
	} else {
		return $self -> {'ignore_missing_data'};
	}
}

sub ignore_missing_files {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'ignore_missing_files'} = $parm;
	} else {
		return $self -> {'ignore_missing_files'};
	}
}

sub ignore_missing_output_files {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'ignore_missing_output_files'} = $parm;
	} else {
		return $self -> {'ignore_missing_output_files'};
	}
}

sub outputfile {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> outputfile');
# line 3399 "lib/model_subs.pm" 
      {
	# Usage:
	#
	# This method is a (partially) automatically generated accessor for the
	# outputfile attribute of the model class. Since no named argument is needed
	# for accessors, the two possible ways of calling outputfile are:
	#
        #   $modelObject -> outputfile( 'newfilename.lst' );
        #
        #   $outputfilename = $modelObject -> outputfile;
	#
	# The first alternative sets a new name for the output file, and the second
	# retrieves the value.
	#
	# The extra feature for this accessor, compared to other accessors, is that
	# if a new name is given, the accessor tries to create a new output object
	# based on this.

	if( defined $parm ) {
	  $self->{'outputs'} = 
	    [ output ->
	      new( filename             => $parm,
		   ignore_missing_files => ( $self -> ignore_missing_files() || $self -> ignore_missing_output_files() ),
		   target               => $self -> target())];
	}
      }
# line 652 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> outputfile');
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'outputfile'} = $parm;
	} else {
		return $self -> {'outputfile'};
	}
}

sub run_no {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'run_no'} = $parm;
	} else {
		return $self -> {'run_no'};
	}
}

sub sde {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sde'} = $parm;
	} else {
		return $self -> {'sde'};
	}
}

sub omega_before_pk {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'omega_before_pk'} = $parm;
	} else {
		return $self -> {'omega_before_pk'};
	}
}

sub tbs {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'tbs'} = $parm;
	} else {
		return $self -> {'tbs'};
	}
}

sub tbs_param {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'tbs_param'} = $parm;
	} else {
		return $self -> {'tbs_param'};
	}
}

sub tbs_thetanum {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'tbs_thetanum'} = $parm;
	} else {
		return $self -> {'tbs_thetanum'};
	}
}

sub synced {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'synced'} = $parm;
	} else {
		return $self -> {'synced'};
	}
}

sub target {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> target');
# line 3772 "lib/model_subs.pm" 
      {
	if ( $parm eq 'disk' ) {
	  $self -> {'target'} = 'disk';
	  $self -> flush;
	} elsif ( $parm eq 'mem' and $self -> {'target'} eq 'disk' ) {
	  $self -> {'target'} = 'mem';
	  $self -> synchronize;
	}
      }
# line 777 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> target');
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'target'} = $parm;
	} else {
		return $self -> {'target'};
	}
}

sub reference_object {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'reference_object'} = $parm;
	} else {
		return $self -> {'reference_object'};
	}
}

sub last_est_complete {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'last_est_complete'} = $parm;
	} else {
		return $self -> {'last_est_complete'};
	}
}

sub niter_eonly {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'niter_eonly'} = $parm;
	} else {
		return $self -> {'niter_eonly'};
	}
}

sub drop_dropped {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> drop_dropped');
# line 4427 "lib/model_subs.pm" 
      {
	for( my $i = 0; $i < scalar @{$self->problems}; $i++ ) {
	  $self -> {'datas'}[$i] -> drop_dropped( model_header => $self->problems->[$i] -> header );
	  $self->problems->[$i] -> drop_dropped( );
	}
      }
# line 843 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> drop_dropped');
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'drop_dropped'} = $parm;
	} else {
		return $self -> {'drop_dropped'};
	}
}

sub quick_reload {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'quick_reload'} = $parm;
	} else {
		return $self -> {'quick_reload'};
	}
}

sub data_ids {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'data_ids'} = $parm;
	} else {
		return $self -> {'data_ids'};
	}
}

sub add_iofv_module {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_iofv_module given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'iofv_modules'}},
		model::iofv_module -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_nonparametric_module {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_nonparametric_module given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'nonparametric_modules'}},
		model::nonparametric_module -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_shrinkage_module {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_shrinkage_module given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'shrinkage_modules'}},
		model::shrinkage_module -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_output {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_output given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'outputs'}},
		output -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_data {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_data given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'datas'}},
		data -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_problem {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_problem given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'problems'}},
		model::problem -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_records {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'type' => 'm_SCALAR', 'record_strings' => 'm_ARRAY',
			'problem_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->add_records: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->add_records: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->add_records: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->add_records: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->add_records: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $type = $parm{'type'};
	my @record_strings = defined $parm{'record_strings'} ? @{$parm{'record_strings'}} : ();
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> add_records');
# line 681 "lib/model_subs.pm" 
      {
	unless(scalar(@problem_numbers)>0 ){
	$self->problems([]) unless defined $self->problems;
	  @problem_numbers = (1 .. $#{$self->problems}+1);
	}

	my @problems = @{$self->problems};
	foreach my $i ( @problem_numbers ) {
	  if ( defined $problems[ $i-1 ] ) {
	    $problems[$i-1] -> add_records( 'type' => $type,
					    'record_strings' => \@record_strings );
	  } else {
	    croak("Problem number $i does not exist.");
	  } 
	}
# else {
#	    croak("Model -> add_records: No Problems in model object.") ;
#	}
      }
# line 1044 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> add_records');
	# End of Non-Dia code #

}

sub copy {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'skip_data_parsing' => 'SCALAR', 'directory' => 'SCALAR',
			'filename' => 'SCALAR', 'copy_data' => 'SCALAR',
			'copy_output' => 'SCALAR', 'output_same_directory' => 'SCALAR',
			'data_file_names' => 'ARRAY', 'target' => 'SCALAR',
			'update_shrinkage_tables' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->copy: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->copy: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->copy: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->copy: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->copy: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $skip_data_parsing = defined $parm{'skip_data_parsing'} ? $parm{'skip_data_parsing'} : 0;
	my $directory = $parm{'directory'};
	my $filename = $parm{'filename'};
	my $new_model;
	my $copy_data = defined $parm{'copy_data'} ? $parm{'copy_data'} : 0;
	my $copy_output = defined $parm{'copy_output'} ? $parm{'copy_output'} : 0;
	my $output_same_directory = defined $parm{'output_same_directory'} ? $parm{'output_same_directory'} : 0;
	my @data_file_names = defined $parm{'data_file_names'} ? @{$parm{'data_file_names'}} : ();
	my $target = defined $parm{'target'} ? $parm{'target'} : $self -> {'target'};
	my $update_shrinkage_tables = defined $parm{'update_shrinkage_tables'} ? $parm{'update_shrinkage_tables'} : 1;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> copy');
# line 890 "lib/model_subs.pm" 
      {
	# PP_TODO fix a nice copying of modelfile data
	# preferably in memory copy. Perhaps flush data ?

	# Check sanity of the length of data file names argument
	if ( scalar @data_file_names > 0 ) {
	  croak("model -> copy: The number of specified new data file " .
			  "names ". scalar @data_file_names. "must\n match the number".
			  " of data objects connected to the model object".
			  scalar @{$self -> {'datas'}} ) 
	      unless ( scalar @data_file_names == scalar @{$self -> {'datas'}} );
	} else {
	  my $d_filename;
	  ($d_filename = $filename) =~ s/\.mod$//;
	  for ( my $i = 1; $i <= scalar @{$self -> {'datas'}}; $i++ ) {
	    # Data filename is created in this directory (no directory needed).
	    push( @data_file_names, $d_filename."_data_".$i."_copy.dta" );
	  }
	}

	($directory, $filename) = OSspecific::absolute_path( $directory, $filename );

	# New copy:

	# save references to own data and output objects
	my $datas   = $self -> {'datas'};
	my $outputs = $self->outputs;
	my @problems = @{$self->problems};

	my ( @new_datas, @new_outputs );

	$self -> synchronize if not $self->synced;
	
	# remove ref to data and output object to speed up the
	# cloning
	$self -> {'datas'}   = undef;
	$self -> {'outputs'} = undef;			#FIXME: Fix with Moose

	# Copy the data objects if so is requested
	if ( defined $datas ) {
	  my $i = 0;
	  foreach my $data ( @{$datas} ) {
	    if ( $copy_data == 1 ) {
	      #this copies datafile with local name to NM_run
	      push( @new_datas, $data ->
		    copy( filename => $data_file_names[$i]) ); #attribute skip_parsing is copied
	    } else {
	      # This line assumes one data per problem! May be a source of error.
	      my $ignoresign = defined $self -> ignoresigns ? $self -> ignoresigns -> [$i] : undef;
	      my @model_header = @{$self -> problems -> [$i] -> header};
	      push @new_datas, data ->
		new( filename		  => $data -> filename,
		     directory		  => $data -> directory,
		     target		  => 'disk',
		     ignoresign		  => $ignoresign,
		     skip_parsing         => ($self -> skip_data_parsing() or $skip_data_parsing),
		     idcolumn		  => $data -> idcolumn );
	    }
	    $i++;
	  }
	}

	# Clone self into new model object and set synced to 0 for
	# the copy
	$new_model = Storable::dclone( $self );
	$new_model->synced(0);

	# Restore the data and output objects for self
	$self -> {'datas'} = $datas;
	$self->outputs($outputs);

	# Set the new file name for the copy
	$new_model -> directory( $directory );
	$new_model -> filename( $filename );
	
	# {{{ update the shrinkage modules

	my @problems = @{$new_model -> problems};
	for( my $i = 1; $i <= scalar @problems; $i++ ) {
	  $problems[ $i-1 ] -> shrinkage_module -> nomegas( $new_model -> nomegas()->[$i-1] );
	}

	# }}} update the shrinkage modules

	# Copy the output object if so is requested (only one output
	# object defined per model object)
	if ( $copy_output == 1 ) {
	  if ( defined $outputs ) {
	    foreach my $output ( @{$outputs} ) {
	      push( @new_outputs, $output -> copy );
	    }
	  }
	  $new_model->outputs(\@new_outputs);
	}else{
	  my $new_out = $filename;
	  if( $new_out =~ /\.mod$/ ) {
	    $new_out =~ s/\.mod$/\.lst/;
	  } else {
	    $new_out = $new_out.'.lst';
	  }
	  $new_model ->ignore_missing_output_files(1);
	  if ($output_same_directory){
	      $new_out = $new_model->directory().$new_out;
	  }
	  $new_model -> outputfile($new_out);
	}

	# Add the copied data objects to the model copy
	$new_model -> datas( \@new_datas );

	#if relative .. or absolute path
	if ((($data_file_names[0] =~ /^\.\.\//) or ($data_file_names[0] =~ /^\//))
	    and (not $copy_data==1 )){
	  #change to filename with relative path
	  for ( my $i = 0; $i <= $#problems; $i++ ) {
	    $new_model -> _option_name( position	  => 0,
					record	  => 'data',
					problem_number => $i+1,
					new_name	  => $data_file_names[$i]);
	  }
	}

	$new_model -> _write;

	$new_model -> synchronize if $target eq 'disk';
      }
# line 1221 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> copy');
	# End of Non-Dia code #

	return $new_model;
}

sub datafiles {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'new_names' => 'ARRAY', 'problem_numbers' => 'ARRAY',
			'absolute_path' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->datafiles: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->datafiles: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->datafiles: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->datafiles: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->datafiles: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @new_names = defined $parm{'new_names'} ? @{$parm{'new_names'}} : ();
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my $absolute_path = defined $parm{'absolute_path'} ? $parm{'absolute_path'} : 0;
	my @names;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> datafiles');
# line 1232 "lib/model_subs.pm" 
      {
	# The datafiles method retrieves or sets the names of the
	# datafiles specified in the $DATA record of each problem. The
	# problem_numbers argument can be used to control which
	# problem that is affected. If absolute_path is set to 1, the
	# returned file names are given with absolute paths.

	unless( scalar(@problem_numbers)>0 ){
		$self->problems([]) unless defined $self->problems;
	  @problem_numbers = (1 .. $#{$self->problems}+1);
	}
	if ( scalar @new_names > 0 ) {
	  my $i = 0;
	  my @idcolumns = @{$self ->
			      idcolumns( problem_numbers => \@problem_numbers )};
	  foreach my $new_name ( @new_names ) {
	    if ( $absolute_path ) {
	      my $tmp;
	      ($tmp, $new_name) = OSspecific::absolute_path('', $new_name );
	      $new_name = $tmp . $new_name;
	    }

	    $self -> _option_name( position	  => 0, 
				   record	  => 'data', 
				   problem_number => $problem_numbers[$i],
				   new_name	  => $new_name);
	    my $ignoresign = defined $self -> ignoresigns ? $self -> ignoresigns -> [$i] : undef;
	    my @model_header = @{$self -> problems -> [$i] -> header};
	    $self -> {'datas'} -> [$problem_numbers[$i]-1] = data ->
	      new( idcolumn             => $idcolumns[$i],
		   ignoresign           => $ignoresign,
		   filename             => $new_name,
		   ignore_missing_files => $self->ignore_missing_files,
		   skip_parsing         => $self -> skip_data_parsing(),
		   target               => $self -> {'target'} );
	    $i++;
	  }
	} else {
	  foreach my $prob_num ( @problem_numbers ) {
	    if ( $absolute_path ) {
	      my ($d_dir, $d_name);
	      ($d_dir, $d_name) =
		  OSspecific::absolute_path($self->directory, $self ->_option_name( position	 => 0,
											  record	 => 'data',
											  problem_number => $prob_num ) );
	      push( @names, $d_dir . $d_name );
	    } else {
	      my $name = $self -> _option_name( position       => 0,
						record	       => 'data',
						problem_number => $prob_num );
	      $name =~ s/.*[\/\\]//;
	      push( @names, $name );
	    }
	  }
	}
      }
# line 1320 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> datafiles');
	# End of Non-Dia code #

	return \@names;
}

sub set_file {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'new_name' => 'SCALAR', 'problem_number' => 'SCALAR',
			'record' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->set_file: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->set_file: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->set_file: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->set_file: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->set_file: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $new_name = $parm{'new_name'};
	my $problem_number = defined $parm{'problem_number'} ? $parm{'problem_number'} : 0;
	my $record = $parm{'record'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> set_file');
# line 1294 "lib/model_subs.pm" 
      {
	my @problem_numbers;
	if ( $problem_number == 0 ){
		$self->problems([]) unless defined $self->problems;
	  @problem_numbers = (1 .. $#{$self->problems}+1);
	}else {
	  push (@problem_numbers,$problem_number);
	}
	foreach my $num (@problem_numbers){
	  $self -> _option_name( position	  => 0, 
				 record	  => $record, 
				 problem_number => $num,
				 new_name	  => $new_name);

	  if ($record eq 'data'){
	    my @idcolumns = @{$self ->
				  idcolumns( problem_numbers => \@problem_numbers )};
	    
	    my $ignoresign = defined $self -> ignoresigns ? $self -> ignoresigns -> [($num-1)] : undef;
	    my @model_header = @{$self -> problems -> [($num-1)] -> header};
	    $self -> {'datas'} -> [($num-1)] = data ->
		new( idcolumn             => $idcolumns[($num-1)],
		     ignoresign           => $ignoresign,
		     filename             => $new_name,
		     ignore_missing_files => $self->ignore_missing_files,
		     skip_parsing         => $self -> skip_data_parsing(),
		     target               => $self -> {'target'} );
	  }

	}
      }
# line 1393 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> set_file');
	# End of Non-Dia code #

}

sub covariance {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'enabled' => 'ARRAY', 'problem_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->covariance: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->covariance: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->covariance: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->covariance: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->covariance: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @enabled = defined $parm{'enabled'} ? @{$parm{'enabled'}} : ();
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @indicators;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> covariance');
# line 1060 "lib/model_subs.pm" 
      {
	if ( scalar(@problem_numbers)>0 ){
	  if ( $#problem_numbers != $#enabled ) {
	    croak("The number of problem_numbers ".($#problem_numbers+1).
			  "and enabled/disabled covariance records ".($#enabled+1).
			  "do not match" );
	  }
	}
	unless( $#problem_numbers > 0 ){
		$self->problems([]) unless defined $self->problems;
	  @problem_numbers = (1 .. $#{$self->problems}+1);
	}
	my @problems = @{$self->problems};
	my $j = 0;
        foreach my $i ( @problem_numbers ) {
	  if ( defined $problems[ $i-1 ] ) {
	    if ( defined $enabled[ $j ] ) {
	      $problems[ $i-1 ] -> covariance( enabled => $enabled[ $j ] );
	    } else {
	      push( @indicators, $problems[ $i-1 ] -> covariance );
	    }
	  } else {
	    croak("Problem number $i does not exist!" );
	  }
	  $j++;
	}	
      }
# line 1460 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> covariance');
	# End of Non-Dia code #

	return \@indicators;
}

sub eigen {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'enabled' => 'ARRAY', 'problem_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->eigen: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->eigen: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->eigen: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->eigen: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->eigen: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @enabled = defined $parm{'enabled'} ? @{$parm{'enabled'}} : ();
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @indicators;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> eigen');
# line 1351 "lib/model_subs.pm" 
      {
	$self->problems->[0]->eigen;
      }
# line 1504 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> eigen');
	# End of Non-Dia code #

	return \@indicators;
}

sub __des {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'new_des' => 'ARRAY', 'problem_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->__des: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->__des: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->__des: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->__des: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->__des: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @new_des = defined $parm{'new_des'} ? @{$parm{'new_des'}} : ();
	my $problem_number = defined $parm{'problem_number'} ? $parm{'problem_number'} : 1;
	my @des;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@des;
}

sub error {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'new_error' => 'ARRAY', 'problem_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->error: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->error: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->error: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->error: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->error: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @new_error = defined $parm{'new_error'} ? @{$parm{'new_error'}} : ();
	my $problem_number = defined $parm{'problem_number'} ? $parm{'problem_number'} : 1;
	my @error;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> error');
# line 1360 "lib/model_subs.pm" 
      {
	# sets or gets the error code for a given problem in the
	# model object. The new_pk argument should be an array where
	# each element contains a row of a valid NONMEM $PK block,

	my @prob = @{$self -> problems};
	
	unless( defined $prob[$problem_number - 1] ){
	  croak("Problem number $problem_number does not exist" );
	}
	
	my $errors = $prob[$problem_number - 1] -> errors;
	if( scalar @new_error > 0 ) {
	  if( defined $errors and scalar @{$errors} > 0 ){
	    $prob[$problem_number - 1] -> errors -> [0] -> code(\@new_error);
	  } else {
	    croak("No \$ERROR record" );
	  }
	} else {
	  if ( defined $errors and scalar @{$errors} > 0 ) {
	    @error = @{$prob[$problem_number - 1] -> errors -> [0] -> code};
	  }
	}
      }
# line 1606 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> error');
	# End of Non-Dia code #

	return \@error;
}

sub fixed {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'parameter_type' => 'SCALAR', 'parameter_numbers' => 'ARRAY',
			'problem_numbers' => 'ARRAY', 'new_values' => 'ARRAY',
			'with_priors' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->fixed: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->fixed: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->fixed: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->fixed: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->fixed: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $parameter_type = $parm{'parameter_type'};
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
	my $with_priors = defined $parm{'with_priors'} ? $parm{'with_priors'} : 0;
	my @fixed;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> fixed');
# line 1747 "lib/model_subs.pm" 
      {
	  # Sets or gets the 'fixed' status of a (number of)
	  # parameter(s). 1 correspond to a parameter being fixed and
	  # 0 not fixed. The returned parameter is a reference to a
	  # two-dimensional array, indexed by problems and parameter
	  # numbers.
	  # Valid parameter types are 'theta', 'omega' and 'sigma'.

	@fixed = @{ $self -> _init_attr
		      ( parameter_type    => $parameter_type,
			parameter_numbers => \@parameter_numbers,
			problem_numbers           => \@problem_numbers,
			new_values        => \@new_values,
			with_priors       => $with_priors,
			attribute         => 'fix')};
      }
# line 1668 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> fixed');
	# End of Non-Dia code #

	return \@fixed;
}

sub idcolumn {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->idcolumn: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->idcolumn: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->idcolumn: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->idcolumn: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->idcolumn: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $problem_number = defined $parm{'problem_number'} ? $parm{'problem_number'} : 1;
	my $col;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> idcolumn');
# line 1801 "lib/model_subs.pm" 
      {
	# Usage:
	#
	#   @idcolumns = @{$modelObject -> idcolumns( problem_numbers => [2,3] );
	#
	# idcolumns returns the idcolumn index in the datafile for the
	# specified problem.

	my $junk_ref;
	( $junk_ref, $col ) = $self ->
	  _get_option_val_pos( name => 'ID', 
			       record_name => 'input', 
			       problem_numbers => [$problem_number] );
	
	if ( $problem_number ne 'all' ) {
	  $col = @{$col}[0];
	}
      }
# line 1726 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> idcolumn');
	# End of Non-Dia code #

	return $col;
}

sub idcolumns {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->idcolumns: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->idcolumns: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->idcolumns: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->idcolumns: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->idcolumns: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @column_numbers;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> idcolumns');
# line 1827 "lib/model_subs.pm" 
      {
	# Usage:
	#
	#   @column_numbers = @{$modelObject -> idcolumns( problem_numbers => [2] )};
	#
	# idcolumns returns the idcolumn indexes in the datafile for the
	# specified problems.

	my ( $junk_ref, $col_ref ) = $self ->
	  _get_option_val_pos( name            => 'ID',
			       record_name     => 'input',
			       problem_numbers => \@problem_numbers );
	# There should only be one instance of $INPUT and hence we collapse
	# the two-dim return from _get_option_pos_val to a one-dim array:

	foreach my $prob ( @{$col_ref} ) {
	  foreach my $inst ( @{$prob} ) {
	    push( @column_numbers, $inst );
	  }
	}
      }
# line 1787 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> idcolumns');
	# End of Non-Dia code #

	return \@column_numbers;
}

sub ignoresigns {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->ignoresigns: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->ignoresigns: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->ignoresigns: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->ignoresigns: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->ignoresigns: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @ignore;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> ignoresigns');
# line 1882 "lib/model_subs.pm" 
      {
	# Usage:
	#
	#   @ignore_signs = @{$modelObject -> ignoresigns( problem_numbers => [2,4] )};
	#
	# ignoresigns returns the ignore signs in the datafile for the
	# specified problems

	foreach my $prob ( @{$self->problems} ) {
	  my @datarecs = @{$prob -> datas};
	  if ( defined $datarecs[0] ) {
	    push( @ignore, $datarecs[0] -> ignoresign );
	  } else {
	    push( @ignore, '#' );
	  }
	}
      }
# line 1844 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> ignoresigns');
	# End of Non-Dia code #

	return \@ignore;
}

sub initial_values {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'parameter_type' => 'SCALAR', 'parameter_numbers' => 'ARRAY',
			'problem_numbers' => 'ARRAY', 'new_values' => 'ARRAY',
			'add_if_absent' => 'SCALAR', 'with_priors' => 'SCALAR',
			'get_same' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->initial_values: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->initial_values: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->initial_values: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->initial_values: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->initial_values: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $parameter_type = $parm{'parameter_type'};
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
	my $add_if_absent = defined $parm{'add_if_absent'} ? $parm{'add_if_absent'} : 0;
	my $with_priors = defined $parm{'with_priors'} ? $parm{'with_priors'} : 0;
	my $get_same = defined $parm{'get_same'} ? $parm{'get_same'} : 0;
	my @initial_values;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> initial_values');
# line 1980 "lib/model_subs.pm" 
      {
	  # initial_values either sets or gets the initial values of
	  # the parameter specified in "parameter_type" for each
	  # problem specified in problem_numbers. For each element
	  # in problem_numbers there must be a reference in
	  # parameter_numbers to an array that specify the indices
	  # of the parameters in the subproblem for which the initial
	  # values are set, replaced or retrieved.
	  # 
	  # The add_if_absent argument tells the method to add an init
	  # (theta,omega,sigma) if the parameter number points to a
	  # non-existing parameter with parameter number one higher
	  # than the highest presently included. Only applicable if
	  # new_values are set. Valid parameter types are 'theta',
	  # 'omega' and 'sigma'.

	@initial_values = @{ $self -> _init_attr
				 ( parameter_type    => $parameter_type,
				   parameter_numbers => \@parameter_numbers,
				   problem_numbers   => \@problem_numbers,
				   new_values        => \@new_values,
				   attribute         => 'init',
				   get_same          => $get_same,
				   with_priors       => $with_priors,
				   add_if_absent     => $add_if_absent )};
      }
# line 1919 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> initial_values');
	# End of Non-Dia code #

	return \@initial_values;
}

sub labels {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'parameter_type' => 'SCALAR', 'parameter_numbers' => 'ARRAY',
			'problem_numbers' => 'ARRAY', 'new_values' => 'ARRAY',
			'generic' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->labels: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->labels: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->labels: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->labels: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->labels: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $parameter_type = $parm{'parameter_type'};
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
	my @labels;
	my $generic = defined $parm{'generic'} ? $parm{'generic'} : 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> labels');
# line 2199 "lib/model_subs.pm" 
      {
	# Usage:
	#
	#   @labels = @{$modobj -> labels( parameter_type => 'theta' )};
	#
	# This basic usage takes one arguments and returns names
	# of the specified parameter. The parameter_type argument
	# is mandatory. It returns the labels of all parameters of type given by
	# $parameter_type.
	# @labels will be a two-dimensional array:
	# [[label1][label2][label3]...]
	#
	#   $labels -> labels( parameter_type  => 'theta',
	#                      problem_numbers => [2,4] );
	#
	# To get labels of specific problems, the problem_numbers argument can be used.
	# It should be a reference to an array containing the numbers
	# of all problems whos labels should be retrieved.
	#
	#   $modobj -> labels( parameter_type    => 'theta',
	#                      problem_numbers   => [2,4],
	#                      parameter_numbers => [[1,3][4,6]]);
	#
	# The retrieval can be even more specific by using the parameter_numbers
	# argument. It should be a reference to a two-dimensional array, where
	# the inner arrays holds the numbers of the parameters that should be
	# fetched. In the example above, parameters one and three from problem two
	# plus parameters four and six from problem four are retrieved.
	#
	#   $modobj -> labels( parameter_type    => 'theta',
	#                      problem_numbers   => [2,4],
	#                      parameter_numbers => [[1,3][4,6]],
	#                      generic           => 1 );
	#
	# To get generic labels for the parameters - e.g. OM1, OM2_1, OM2 etc -
	# set the generic argument to 1.
	#
	# $modobj -> labels( parameter_type     => 'theta',
	#                     problem_numbers   => [2],
	#                     parameter_numbers => [[1,3]],
	#                     new_values        => [['Volume','Clearance']] );
	#
	# The new_values argument can be used to give parameters new labels. In
	# the above example, parameters one and three in problem two are renamed
	# Volume and Clearance.
	#
	# if record is SAME then indexes will still be returned for those positions
	# if new values is set or parameter_numbers this will problably have bugs

	if ((scalar(@parameter_numbers)>0 or scalar(@new_values)>0)
	    and ($parameter_type ne 'theta')){
	  croak("only theta can be given new labels / be retrieved by param number");
	}

	#add attribute prior default 0 to init_record

	my ( @index, $idx );
	@labels = @{ $self -> _init_attr
		       ( parameter_type    => $parameter_type,
			 parameter_numbers => \@parameter_numbers,
			 problem_numbers           => \@problem_numbers,
			 new_values        => \@new_values,
			 attribute         => 'label',
			 with_priors       => 0)};

	#indexes function is changed to return name plus index, e.g THETA2, OMEGA(2,1)
	@index = @{$self -> indexes( parameter_type => $parameter_type,
				     parameter_numbers => \@parameter_numbers,
				     problem_numbers => \@problem_numbers,
				     with_priors => 0)};


	for ( my $i = 0; $i <= $#labels; $i++ ) {
	  #problems
	  for ( my $j = 0; $j < scalar @{$labels[$i]}; $j++ ) {
	    #parameters
	    $idx = $index[$i][$j];
	    $labels[$i][$j] = $idx unless ( defined $labels[$i][$j] and not $generic );

	  }
	  if (scalar @{$labels[$i]} == 0){
	    if ( ($i == 1) 
		 and (defined $self->problems()) and (defined $self->problems()->[1]) 
		 and (defined $self->problems()->[0]) 
		 and (defined $self->problems()->[1]->msfis()) 
		 and (scalar(@{$self->problems()->[1]->msfis()})>0)
		 and ($self -> is_option_set ( name           => 'MSFO', 
					       record         => 'estimation', 
					       problem_number => 1,
					       fuzzy_match    => 1 ))){
	      for ( my $j = 0; $j < scalar @{$labels[0]}; $j++ ) {
		$labels[$i][$j] = $labels[0][$j];
	      }
	    }elsif (0) {
	      #dangerous to turn this on, some models do not have thetas/sigmas/omegas,
	      # do not want to add them.
	      #must check for msfi and 1st prob
	      my $namefunc = $parameter_type.'names';
	      if (defined $self -> outputs()
		  and defined $self -> outputs()-> [0] 
		  and defined $self -> outputs()-> [0]-> $namefunc ()){
		my $namesref = $self -> outputs()-> [0]-> $namefunc ();
		if (defined $namesref->[$i] and defined $namesref->[$i]->[0]){
		  for ( my $j = 0; $j < scalar @{$namesref->[$i]->[0]}; $j++ ) {
		    $labels[$i][$j]=$namesref->[$i]->[0]->[$j];
		  }
		}
	      }
	    }
	  } #end if scalar == 0
	}

      }
# line 2078 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> labels');
	# End of Non-Dia code #

	return \@labels;
}

sub get_values_to_labels {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'category' => 'm_SCALAR', 'label_model' => 'model' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->get_values_to_labels: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->get_values_to_labels: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->get_values_to_labels: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->get_values_to_labels: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->get_values_to_labels: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $category = $parm{'category'};
	my $label_model = $parm{'label_model'};
	my @out_values;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> get_values_to_labels');
# line 4075 "lib/model_subs.pm" 
      {
	# Usage:$modobj -> get_values_to_labels ( category => $categ);
	# return array of arrays of arrays of values that belong to the strings returned from labels
	# assume model has $self->{'outputs'}->[0]
	# works for theta, omega, sigma, setheta, seomega, sesigma,
	# cvsetheta, cvseomega, cvsesigma, comega, csigma

	unless (defined $self -> outputs -> [0]){
	 croak("get_values_to_labels can only be called where output object exists"); 
	}
	unless ( $self -> outputs -> [0] -> parsed_successfully() ){
	  croak("get_values_to_labels can only be called where output object parsed successfully" ); 
	}

	#zeros may be present, they are ambigous (really 0 or not estimated)
	#no padding ever
	my $param = $category;

	#prefix can be cvse, c, se or none
	$param =~ s/^cvse// ;
	$param =~ s/^c// ;
	$param =~ s/^se// ;

	#one element per problem
	my @coordinates;
	if (defined $label_model){
	  @coordinates = @{$label_model -> labels( parameter_type => $param, generic => 1)}; #without se
	}else{
	  @coordinates = @{$self -> labels( parameter_type => $param, generic => 1)}; #without se
	}
	my $access = $category.'coordval'; #with se, if it is there
	#array over problems of array over subproblems of arrays of values
	my $valref = $self -> outputs -> [0] -> $access ();
	croak("No accessor $access for output object") unless (defined $valref);
	my @from_coordval = @{$valref};

	for ( my $i = 0; $i <= $#coordinates; $i++ ) {
	  #loop over problems
	  if (defined $from_coordval[$i] and defined $coordinates[$i]) {
	    my @prob_values = ();
	    my @coords = @{$coordinates[$i]};
	    foreach my $hashref (@{$from_coordval[$i]}) {
	      #loop subprobs
	      if (defined $hashref){
		my @values =();
		my %coordval = %{$hashref};
#		if (scalar(@coords)==0){
#		  print "warning: no parameter labels return from method labels, might be problems in raw_results\n";
#		}
		foreach my $coord (@coords){
		  if (defined $coordval{$coord}){
		    push (@values,$coordval{$coord});
		  }else {
		    # only not stored are undefs/NA
		    print "undefined value from labels(), bug!\n" unless (defined $coord);
		    push (@values,undef);
		  }
		}
		push (@prob_values,\@values);
	      }else{
		push (@prob_values,undef);
	      }
	    }
	    push (@out_values,\@prob_values);
	  }else {
	    push (@out_values,undef);
	  }
	}

      }
# line 2189 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> get_values_to_labels');
	# End of Non-Dia code #

	return \@out_values;
}

sub get_coordslabels {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'parameter_type' => 'SCALAR', 'problem_numbers' => 'ARRAY',
			'with_priors' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->get_coordslabels: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->get_coordslabels: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->get_coordslabels: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->get_coordslabels: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->get_coordslabels: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $parameter_type = $parm{'parameter_type'};
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my $with_priors = defined $parm{'with_priors'} ? $parm{'with_priors'} : 0;
	my @coordslabels;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> get_coordslabels');
# line 2813 "lib/model_subs.pm" 
      {
	# Usage:
	#
	#   @coordslabels = @{$modobj -> get_coordslabels( parameter_type => 'theta' )};
	#
	# This basic usage takes one argument and returns matched coords and labels
	# of the specified parameter. The parameter_type argument
	# is mandatory. It returns coords and labels of all parameters of type given by
	# $parameter_type. If label is undefined it will be set to coordinate string 
	# (THETA1, OMEGA(1,1)....
	#
	# @coordslabels will be an array of hashes:
	# [hash1 hash2 ...]
	#
	#   $modobj -> get_coordslabels( parameter_type  => 'theta',
	#                      problem_numbers => [2,4] );
	#
	# To get_coordslabels of specific problems, the problem_numbers argument can be used.
	# It should be a reference to an array containing the numbers
	# of all problems whos coordslabels should be retrieved.
	#
	#   $modobj -> get_coordslabels( parameter_type    => 'theta',
	#                      problem_numbers   => [2,4]);
	#

	unless( scalar @problem_numbers > 0 ){
		$self->problems([]) unless defined $self->problems;
	  @problem_numbers = (1 .. $#{$self->problems}+1);
	}
	my @problems = @{$self->problems};
	
	foreach my $prob (@problem_numbers){
	  my $prob_index = $prob -1;
	  my %hash;
	  if ( defined $problems[ $prob_index ] ) {
	    my $accessor = $parameter_type.'s';
	    unless( $problems[ $prob_index ] -> can($accessor) ){
	      croak("Error unknown parameter type: $parameter_type" );
	    }
	    my $ref =  eval( '$problems[ $prob_index ] -> '.$accessor.'()' );
	    unless (defined $ref){
	      push (@coordslabels,\%hash);
	      next;
	    }
	    my  @records = @{$ref};
	    foreach my $record ( @records ) {
	      next if ($record->prior() and (not $with_priors));
	      unless ( $record -> same() ) {
		if ( defined $record -> options ) {
		  foreach my $option ( @{$record -> options} ) {
		    next if ($option->prior() and (not $with_priors));
		    my $coord = $option -> coordinate_string();
		    my $label = $coord;
		    if (defined $option -> label()){
		      $label = $option -> label();
		    }
		    unless (defined $coord){
		      croak("Error undefined coordinate string" );
		    }
		    $hash{$coord} = $label; #if not label defined then this will be coord-coord
		  }
		} else {
		  carp("no options defined in record ".ref($record) );
		}
	      }
	    }
	  } else {
	    croak("Problem number $prob does not exist!" );
	  }
	  push (@coordslabels,\%hash);
	}

      }
# line 2305 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> get_coordslabels');
	# End of Non-Dia code #

	return \@coordslabels;
}

sub get_rawres_params {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'filename' => 'm_SCALAR', 'filter' => 'ARRAY',
			'string_filter' => 'ARRAY', 'require_numeric_ofv' => 'SCALAR',
			'offset' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->get_rawres_params: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->get_rawres_params: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->get_rawres_params: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->get_rawres_params: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->get_rawres_params: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $filename = $parm{'filename'};
	my @filter = defined $parm{'filter'} ? @{$parm{'filter'}} : ();
	my @string_filter = defined $parm{'string_filter'} ? @{$parm{'string_filter'}} : ();
	my $require_numeric_ofv = defined $parm{'require_numeric_ofv'} ? $parm{'require_numeric_ofv'} : 0;
	my $offset = $parm{'offset'};
	my @allparams;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> get_rawres_params');
# line 2393 "lib/model_subs.pm" 
      {
	#input is filename + offset and possibly array filter and possibly array string_filter
	  #input require_numeric_ofv is special filter, default false, if true then check that looks_like_number(ofv)
	  #input 
	#output is hash of arrays of hashes allparams

	my @thetalabels = @{$self -> labels( parameter_type => 'theta', generic => 0)};
	my @omegalabels = @{$self -> labels( parameter_type => 'omega', generic => 0)};
	my @sigmalabels = @{$self -> labels( parameter_type => 'sigma', generic => 0)};

	if (scalar(@thetalabels) != 1 or scalar(@omegalabels) != 1 or scalar(@sigmalabels) != 1){
	  croak("get_rawres_params can only be done if exactly one \$PROB");
	}
	unless (defined $thetalabels[0] and defined $omegalabels[0] and defined $sigmalabels[0]){
	  croak("all labels references are not defined in get_rawres_params");
	}

	my %thetapos;
	my %omegapos;
	my %sigmapos;
	
	croak("file $filename does not exist") unless ( -e $filename );

	open( RRES, $filename) or die "could not open $filename";
	my @read_file = <RRES>;
	close( RRES );
	my @file;

	foreach (@read_file){
	  chomp;
	  if (/\"/ ){
	      #if any quotes at all
	      #remove one column header at a time, check for each if enclosed in double quotes or not
	      my $header = $_;
	      my @tmp =();
	      while (length($header)>0){
			  $header =~ s/^\s*//; #remove leading whitespace
			  my $col;
			  if ($header =~ /^\"/){
				  #enclosed double quotes, handle more than one in a row
				  if ($header =~ /^\"+([^"]+)\"+\s*\,?/){
					  $header =~ s/^\"+([^"]+)\"+\s*\,?//; #" 
					  $col = $1; 
				  }else{
					  croak("Failed parsing the header of the rawres input file\n$header");
				  }
			  }else{
				  #no quotes
				  $header =~ s/([^,]+)\,?// ; #" 
				  $col = $1; 
			  }
			  # we allow empty matches
			  push(@tmp,$col);
	      }
	      push (@file,\@tmp);
	  } else {
	    my @tmp = split(',',$_);
	    push (@file,\@tmp);
	  }
	}

	my $ref = shift @file;
	my @header = @{$ref};
	my $sum = scalar(@{$thetalabels[0]})+scalar(@{$omegalabels[0]})+scalar(@{$sigmalabels[0]});
	$sum += scalar(@filter); #@filter is always defined, but may be empty - bug, may count some cols twice here
	
	unless (scalar(@header) > $sum and (($header[0] eq 'model') or ($header[1] eq 'model') ) ){
	    print "\n\nThe found headers are\n".join("   ",@header)."\n\n";

	    croak("The file $filename does not follow the format rules.\n".
		       "Either first or second column should be model, you have ".$header[0]." and ".$header[1].
		       ", need $sum cols and have ".scalar(@header)."\n");
	}
	if (($header[0] eq 'hypothesis') and ($offset == 1)){
	    print "\nWarning: Your rawres_input file looks like an sse raw results file,\n".
		"but you use offset_rawres=1 which is the default suitable for bootstrap\n".
		"raw results files. If you want to include also the first model\n".
		"from the raw results file then rerun with offset_rawres=0.\n\n";
	}

	#parse filter
	my ($ref1,$ref2,$ref3);
	($ref1,$ref2,$ref3) = $self->setup_filter(filter => \@filter, header => \@header)
	    if (scalar(@filter)>0);
	my @filter_column_index = @{$ref1} if (defined $ref1);
	my @filter_relation = @{$ref2} if (defined $ref2);
	my @filter_value = @{$ref3} if (defined $ref3);
	if (scalar(@string_filter)>0){
	    my ($r1,$r2,$r3) = $self->setup_filter(filter => \@string_filter, header => \@header, string_filter =>1);
	    push(@filter_column_index,@{$r1}) if (defined $r1);
	    push(@filter_relation,@{$r2}) if (defined $r3);
	    push(@filter_value,@{$r3}) if (defined $r3);
	}

	my $pos=-1;
	my $ofvindex=-1;
	my $modelindex=-1;
	#scan for ofv label and first theta label. Then following should be rest of theta,omega,sigma
	for (my $i=0; $i<scalar(@header);$i++){
	    if ($header[$i] eq 'ofv'){
		$ofvindex = $i;
	    }elsif ($header[$i] eq 'model'){
		$modelindex = $i;
	    }elsif ($header[$i] eq $thetalabels[0]->[0]){
		$pos = $i;
		last;
	    }
	}
	if ($pos == -1){
	  croak("could not find header ".$thetalabels[0]->[0]." in rawres ".
		     "header\n".join(' ',@header)."\n");
	}
	if (($ofvindex == -1) and ($require_numeric_ofv)){
	    croak("could not find header ofv in rawres ".
		       "header\n".join(' ',@header)."\n");
	}
	foreach my $lab (@{$thetalabels[0]}){
	  $thetapos{$lab} = $pos;
	  $pos++;
	}
	foreach my $lab (@{$omegalabels[0]}){
	  $omegapos{$lab} = $pos;
	  $pos++;
	}
	foreach my $lab (@{$sigmalabels[0]}){
	  $sigmapos{$lab} = $pos;
	  $pos++;
	}

	if ($pos > scalar(@header)){
	  croak("assigned position for theta/omega/sigma greater than number ".
	      "of items in raw_res header");
	}
	
	#skip the offset first lines of @file
	for (my $i=0; $i< $offset; $i++){
	    my $dirt = shift @file;
	}
	#loop through remaining lines, check if should be filtered out or saved to result hash
	foreach my $line (@file){
	  my $skip = 0;
	  if ($require_numeric_ofv and (not looks_like_number($line->[$ofvindex]))){
	      $skip=1;
	  }else {
	      for (my $i=0; $i< scalar(@filter_column_index);$i++){
		  my $val = $line->[$filter_column_index[$i]];
 		  if ($filter_relation[$i] =~ /(==|!=|>|<)/){
		      #numeric relation
		      if (($val eq 'NA') or ($val eq '')){
			  $skip=1;
			  last;
		      }elsif(not looks_like_number($val)){
			  print "\nError: value $val in input filter column ".
			      $header[$filter_column_index[$i]]." does not look numeric. All input ".
			      "filter columns must be numeric, skipping this line\n";
			  $skip=1;
			  last;
		      }
		  }
		  #if we get here then $val was ok
		  my $string;
 		  if ($filter_relation[$i] =~ /(==|!=|>|<)/){
		      #numeric relation
		      $string=$val.$filter_relation[$i].$filter_value[$i];
		  }else{
		      $string ="\'".$val."\' $filter_relation[$i] \'".$filter_value[$i]."\'";
		  }
		  unless (eval($string)){
		      $skip=1;
		      last;
		  }else{
		      
		  }
	      }
	  }
	  next if ($skip);
	  my %theta;
	  my %omega;
	  my %sigma;
	  foreach my $label (keys %thetapos){
	    my $val = $line->[$thetapos{$label}];
	    unless (looks_like_number($val) ){
		$skip =1;
		print "\nWarning rawres input: $val in column $label does not look like a parameter value\n";
	    }
	    $theta{$label} = $val;
	  }
	  foreach my $label (keys %omegapos){
	    my $val = $line->[$omegapos{$label}];
	    unless (looks_like_number($val) ){
		$skip =1;
		print "\nWarning rawres input: $val in column $label does not look like a parameter value\n";
	    }
	    $omega{$label} = $val;
	  }
	  foreach my $label (keys %sigmapos){
	    my $val = $line->[$sigmapos{$label}];
	    unless (looks_like_number($val) ){
		$skip =1;
		print "\nWarning rawres input: $val in column $label does not look like a parameter value\n";
	    }
	    $sigma{$label} = $val;
	  }
	  next if ($skip);

	  my %allpar;
	  $allpar{'theta'} = \%theta;
	  $allpar{'omega'} = \%omega;
	  $allpar{'sigma'} = \%sigma;
	  if ($require_numeric_ofv){
	      $allpar{'ofv'} = $line->[$ofvindex];
	  }
	  if ($modelindex >= 0){
	      $allpar{'model'} = $line->[$modelindex];
	  }
	  push (@allparams,\%allpar);
	}  


      }
# line 2571 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> get_rawres_params');
	# End of Non-Dia code #

	return \@allparams;
}

sub setup_filter {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'filter' => 'ARRAY', 'string_filter' => 'SCALAR',
			'header' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->setup_filter: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->setup_filter: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->setup_filter: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->setup_filter: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->setup_filter: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @filter = defined $parm{'filter'} ? @{$parm{'filter'}} : ();
	my $string_filter = defined $parm{'string_filter'} ? $parm{'string_filter'} : 0;
	my @header = defined $parm{'header'} ? @{$parm{'header'}} : ();
	my @filter_column;
	my @filter_relation;
	my @filter_value;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> setup_filter');
# line 2320 "lib/model_subs.pm" 
      {
	#input is array filter and possibly array header
	  #boolean string_filter, default false
	#output is @filter_column,@filter_relation,@filter_value

	foreach my $filt (@filter){
	  $filt =~ s/(\.gt\.|\.lt\.|\.eq\.|\.ne\.)(.*)$//;
	  my $rel;
	  if ($1 eq '.eq.'){
	      if ($string_filter){
		  $rel = 'eq'; #space important
	      }else{
		  $rel ='==';
	      }
	  }elsif ($1 eq '.ne.'){
	      if ($string_filter){
		  $rel = 'ne'; #spaces important
	      }else{
		  $rel ='!=';
	      }
	  }elsif ($1 eq '.gt.'){
	      if ($string_filter){
		  print "error in string filter, relation .gt. not allowed for strings. Ignoring filter $filt\n";
		  next;
	      }
	    $rel = '>';
	  }elsif ($1 eq '.lt.'){
	      if ($string_filter){
		  print "error in string filter, relation .lt. not allowed for strings. Ignoring filter $filt\n";
		  next;
	      }
	    $rel = '<';
	  }else{
	    print "Error identifying relation $1, ignoring filter $filt\n";
	    next;
	  }
	  my $val = $2;
	  if ((not looks_like_number($val)) and (not $string_filter)){
	      print "Error in filter analysis, value $val does not look like a number. Ignoring filter $filt\n";
	      next;
	  }
	  my $col = $filt;
	  my $pos=-1;
	  if (scalar(@header)>0){
	    #scan for filter column
	    for (my $i=0; $i<scalar(@header);$i++){
	      if ($header[$i] eq $col){
		$pos = $i;
		last;
	      }
	    }
	    if ($pos == -1){
	      croak("could not find filter header $col in ".
			 "header\n".join(' ',@header)."\n");
	    }
	    #store index
	  }else{
	    $pos = $col;
	    #store string
	  }
	  push(@filter_column,$pos);
	  push(@filter_relation,$rel);
	  push(@filter_value,$val);
	}

      }
# line 2682 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> setup_filter');
	# End of Non-Dia code #

	return \@filter_column ,\@filter_relation ,\@filter_value;
}

sub get_covariance_params {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'filename' => 'm_SCALAR', 'samples' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->get_covariance_params: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->get_covariance_params: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->get_covariance_params: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->get_covariance_params: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->get_covariance_params: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $filename = $parm{'filename'};
	my $samples = $parm{'samples'};
	my @allparams;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> get_covariance_params');
# line 2620 "lib/model_subs.pm" 
      {
	#input is filename, samples
	#output is hash of arrays of hashes allparams
	#will take parameter estimates from this model object as means,
	#assuming any update_inits from output_object or output_file
	#has already been performed

	my @thetalabels = @{$self -> labels( parameter_type => 'theta', generic => 0)};
	my @omegalabels = @{$self -> labels( parameter_type => 'omega', generic => 0)};
	my @sigmalabels = @{$self -> labels( parameter_type => 'sigma', generic => 0)};
	if (scalar(@thetalabels) != 1 or scalar(@omegalabels) != 1 or scalar(@sigmalabels) != 1){
	  croak("get_covariance_params can only be done if exactly one \$PROB");
	}
	unless (defined $thetalabels[0] and defined $omegalabels[0] and defined $sigmalabels[0]){
	  croak("all labels references are not defined in get_covariance_params");
	}


	#indexes function is changed to return name plus index, e.g THETA2, OMEGA(2,1)
	#returns array over problems
	my @thetacoords = @{$self -> indexes( parameter_type => 'theta',
				     problem_numbers => [1] )};
	my @omegacoords = @{$self -> indexes( parameter_type => 'omega',
				     problem_numbers => [1] )};
	my @sigmacoords = @{$self -> indexes( parameter_type => 'sigma',
				     problem_numbers => [1] )};

	unless (defined $thetacoords[0] and defined $omegacoords[0] and defined $sigmacoords[0]){
	  croak("all coords references are not defined in get_covariance_params");
	}
	my @thetainits = @{$self-> initial_values(parameter_type    => 'theta',
						  problem_numbers   => [1])};
	my @omegainits = @{$self-> initial_values(parameter_type    => 'omega',
						  problem_numbers   => [1])};
	my @sigmainits = @{$self-> initial_values(parameter_type    => 'sigma',
						  problem_numbers   => [1])};

	my %coordsinits;
	my %coordslabels;
	my %coordstype;
	for (my $i=0;$i<scalar(@{$thetainits[0]});$i++){
	  $coordsinits{$thetacoords[0]->[$i]} = $thetainits[0]->[$i];
	  $coordslabels{$thetacoords[0]->[$i]} = $thetalabels[0]->[$i];
	  $coordstype{$thetacoords[0]->[$i]} = 'theta';
	}
	for (my $i=0;$i<scalar(@{$omegainits[0]});$i++){
	  $coordsinits{$omegacoords[0]->[$i]} = $omegainits[0]->[$i];
	  $coordslabels{$omegacoords[0]->[$i]} = $omegalabels[0]->[$i];
	  $coordstype{$omegacoords[0]->[$i]} = 'omega';
	}
	for (my $i=0;$i<scalar(@{$sigmainits[0]});$i++){
	  $coordsinits{$sigmacoords[0]->[$i]} = $sigmainits[0]->[$i];
	  $coordslabels{$sigmacoords[0]->[$i]} = $sigmalabels[0]->[$i];
	  $coordstype{$sigmacoords[0]->[$i]} = 'sigma';
	}

	unless (defined $thetainits[0] and defined $omegainits[0] and defined $sigmainits[0]){
	  croak("all inits references are not defined in get_covariance_params");
	}
	
	croak("file $filename does not exist") unless ( -e $filename );

	open( RRES, $filename) or die "could not open $filename";
	my @read_file = <RRES>;
	close( RRES );
	my @covar=();
	my @coordarr=();
	my @header=();
	my $found_table=0;
	my $found_name=0;
	my %remove_columns;
	foreach (@read_file){
	  chomp;
	  if (/^TABLE NO/ ){
	    @covar=(); #reset when find new table, want the last one
	    @coordarr=();
	    %remove_columns=();
	    $found_table=1;
	    next;
	  }
	  if (/^ NAME/){
	    $found_name=1;
	    chomp;
	    my @tmp = split;#(/\s*/,$_); #$_ on whitespace
	    shift(@tmp);
	    @header = @tmp;
	    next;
	  }
	  chomp;
	  my @tmp = split;#(/\s*/,$_); #$_ on whitespace
	  croak("splitting row in $filename on whitespace gave only one item. ".
	      "The delimiter in the file must be wrong") if (scalar(@tmp)==1);
	  my $coord= shift(@tmp);
	  unless ($coord =~ /^(THETA|OMEGA|SIGMA)/){
	    croak("$filename does not look like a NM7 covariance file. ".
		       "$coord does not look like a label for THETA/OMEGA/SIGMA");
	  }
	  #need to remove all all-zero rows and columns
	  my $all_zeros=1;
	  foreach my $val (@tmp){
	    $all_zeros = 0 unless ($val == 0);
	  }
	  if ($all_zeros){
	    $remove_columns{$coord}=1;
	  }else{
	    push(@coordarr,$coord);
	    push (@covar,\@tmp);
	  }
	}

	if (%remove_columns){
	  my @new_matrix =();
	  foreach my $row (@covar){
	    my @newrow=();
	    for (my $i=0;$i<scalar(@{$row});$i++){ 
	      push(@newrow,$row->[$i]) unless
		  (defined $remove_columns{$header[$i]});
	    }
	    push(@new_matrix,\@newrow);
	  }
	  @covar = @new_matrix;
	}

	unless ($found_table and $found_name){
	    croak("$filename does not look like a NM7 covariance file. ".
		       "Did not find TABLE NO and NAME lines.");
	}

	my @mean =();
	my @labels=();
	my @type=();
	foreach my $coord (@coordarr){
	  if (defined $coordsinits{$coord} and defined $coordslabels{$coord}){
	    push(@mean,$coordsinits{$coord});
	    push(@type,$coordstype{$coord});
	    if (defined $coordslabels{$coord}){
	      push(@labels,$coordslabels{$coord});
	    }else{
	      push(@labels,$coord);
	    }
#	    print $labels[$#labels]."\n";
	  }else{
	    unless ($coord =~ /(OMEGA|SIGMA)\(([\d]+)\,([\d]+)\)/){
	      croak("$coord does not look like off-diagonal OMEGA/SIGMA");
	    }
	    if ($1 == $2){
	      croak("$coord is diagonal OMEGA/SIGMA, but not found in model");
	    } 
	    #if off-diagonal sigma or omega ok, set init 0 and label to coord
	    push(@mean,0);
	    push(@labels,$coord);
	  }
	}

	my @newsets = random_multivariate_normal($samples, @mean, @covar);

	if (0){
	  open( FILE, '>', 'diagnose_covar.csv' );
	  foreach my $set (@newsets){
	    print FILE join(',',@{$set})."\n";
	  }
	  close FILE;
	  die;
	}

	foreach my $set (@newsets){
	  my %theta;
	  my %omega;
	  my %sigma;

	  for (my $i=0; $i< scalar(@labels);$i++){
	    $theta{$labels[$i]} = $set->[$i] if ($type[$i] eq 'theta');
	    $omega{$labels[$i]} = $set->[$i] if ($type[$i] eq 'omega');
	    $sigma{$labels[$i]} = $set->[$i] if ($type[$i] eq 'sigma');
	  }
	  my %allpar;
	  $allpar{'theta'} = \%theta;
	  $allpar{'omega'} = \%omega;
	  $allpar{'sigma'} = \%sigma;
	  push (@allparams,\%allpar);
	}  


      }
# line 2907 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> get_covariance_params');
	# End of Non-Dia code #

	return \@allparams;
}

sub lower_bounds {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'parameter_type' => 'SCALAR', 'parameter_numbers' => 'ARRAY',
			'problem_numbers' => 'ARRAY', 'new_values' => 'ARRAY',
			'with_priors' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->lower_bounds: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->lower_bounds: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->lower_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->lower_bounds: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->lower_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $parameter_type = $parm{'parameter_type'};
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
	my $with_priors = defined $parm{'with_priors'} ? $parm{'with_priors'} : 0;
	my @lower_bounds;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> lower_bounds');
# line 2158 "lib/model_subs.pm" 
      {
	  # lower_bounds either sets or gets the initial values of the
	  # parameter specified in the argument parameter_type for
	  # each problem specified in problem_numbers. See L</fixed>.
	  
	@lower_bounds = @{ $self -> _init_attr
			     ( parameter_type    => $parameter_type,
			       parameter_numbers => \@parameter_numbers,
			       problem_numbers           => \@problem_numbers,
			       new_values        => \@new_values,
			       with_priors       => $with_priors,
			       attribute         => 'lobnd')};
      }
# line 2966 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> lower_bounds');
	# End of Non-Dia code #

	return \@lower_bounds;
}

sub on_diagonal {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'parameter_type' => 'SCALAR', 'parameter_numbers' => 'ARRAY',
			'problem_numbers' => 'ARRAY', 'with_priors' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->on_diagonal: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->on_diagonal: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->on_diagonal: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->on_diagonal: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->on_diagonal: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $parameter_type = $parm{'parameter_type'};
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my $with_priors = defined $parm{'with_priors'} ? $parm{'with_priors'} : 0;
	my @on_diagonal;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> on_diagonal');
# line 2178 "lib/model_subs.pm" 
      {
	  # on_diagonal gets the values for
	  #  argument parameter_type for
	  # each problem specified in problem_numbers. See L</fixed>.
	  
	@on_diagonal = @{ $self -> _init_attr
			      ( parameter_type    => $parameter_type,
				parameter_numbers => \@parameter_numbers,
				problem_numbers           => \@problem_numbers,
				with_priors       => $with_priors,
				attribute         => 'on_diagonal')};
      }
# line 3022 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> on_diagonal');
	# End of Non-Dia code #

	return \@on_diagonal;
}

sub maxeval {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'new_values' => 'ARRAY', 'problem_numbers' => 'ARRAY',
			'exact_match' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->maxeval: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->maxeval: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->maxeval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->maxeval: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->maxeval: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my $exact_match = defined $parm{'exact_match'} ? $parm{'exact_match'} : 0;
	my @values;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> maxeval');
# line 3137 "lib/model_subs.pm" 
      {
# 	# Usage:
# 	#
# 	#   @maxev = @{$modobj -> maxeval};
# 	#
# 	# This basic usage takes no arguments and returns the value of the
# 	# MAXEVAL option in the $ESTIMATION record of each problem.
# 	# @maxev will be a two dimensional array:
# 	# [[maxeval_prob1][maxeval_prob2][maxeval_prob3]...]
# 	#
# 	#   $modobj -> maxeval( new_values => [[0],[999]];
# 	#
# 	# If the new_values argument of maxeval is given, the values of the
# 	# MAXEVAL options will be changed. In this example, MAXEVAL will be
# 	# set to 0 in the first problem and to 999 in the second.
# 	# The number of elements in new_values must match the number of problems
# 	# in the model object $modobj.
# 	#
# 	#   $modobj -> maxeval( new_values => [[0],[999]],
# 	#                       problem_numbers    => [2,4] );
# 	#
# 	# To set the MAXEVAL of specific problems, the problem_numbers argument can
# 	# be used. It should be a reference to an array containing the numbers
# 	# of all problems where the MAXEVAL should be changed or retrieved.
# 	# If specified, the size of new_values must be the same as the size
# 	# of problem_numbers.
# 	#
	


 	my ( $val_ref, $junk ) = $self -> 
 	  _option_val_pos( name            => 'MAX',
 			   record_name     => 'estimation',
 			   problem_numbers => \@problem_numbers,
 			   new_values      => \@new_values,
 			   exact_match     => $exact_match );
 	@values = @{$val_ref};
      }
# line 3103 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> maxeval');
	# End of Non-Dia code #

	return \@values;
}

sub set_maxeval_zero {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_number' => 'SCALAR', 'print_warning' => 'SCALAR',
			'last_est_complete' => 'SCALAR', 'need_ofv' => 'SCALAR',
			'niter_eonly' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->set_maxeval_zero: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->set_maxeval_zero: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->set_maxeval_zero: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->set_maxeval_zero: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->set_maxeval_zero: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $problem_number = defined $parm{'problem_number'} ? $parm{'problem_number'} : 0;
	my $print_warning = defined $parm{'print_warning'} ? $parm{'print_warning'} : 0;
	my $last_est_complete = defined $parm{'last_est_complete'} ? $parm{'last_est_complete'} : 0;
	my $need_ofv = defined $parm{'need_ofv'} ? $parm{'need_ofv'} : 0;
	my $niter_eonly = $parm{'niter_eonly'};
	my $success;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> set_maxeval_zero');
# line 3048 "lib/model_subs.pm" 
      {
	#the intent here is to unset the estimation for a run where we only want
	#to get PRED defined values without changning the initial parameter estimates
	#or get filtered table output after IGNOREs and ACCEPTs.
	#For NONMEM5 and 6 it is very easy, just set MAXEVAL=0
	#for NONMEM7 and single $EST it is almost as easy
	#for NONMEM7 and multiple $EST it is more complicated, the problem being that
	#we need to remove all but the last $EST while options from previous $EST can carry over.

	#if NONMEM5 or 6 just set MAXEVAL=0, done.
	my $n_probs = 0;
	$n_probs = scalar(@{$self -> problems()}) if (defined $self->problems());
	my @problem_numbers;
	#default problem_number is 0 which means all problems
	if ($problem_number > 0){
	  push(@problem_numbers,$problem_number);
	} elsif ($problem_number == 0){
	  @problem_numbers = 1 .. $n_probs;
	}elsif ($problem_number == -1) {
	  push(@problem_numbers,$n_probs);
	}else{
	  croak("illegal input problem_number $problem_number");
	}

	$success = 1;
	if ($PsN::nm_major_version < 7){
	  $self -> set_option(record_name => 'estimation',option_name => 'MAXEVALS',
			      fuzzy_match => 1, option_value => '0',
			      problem_numbers => \@problem_numbers);
	} else {
	  if (not $last_est_complete) {
	    $self -> set_union_estimation_record(problem_numbers => \@problem_numbers,
						 need_ofv => $need_ofv);
	  } else {
	    #If NONMEM7 and multiple $EST #remove all but last $EST. Continue to next step.
	    $self -> remove_records( type => 'estimation', keep_last => 1,
				     problem_numbers => \@problem_numbers);
	  }
	  foreach my $i (@problem_numbers){
	    my $meth = $self-> get_option_value (record_name => 'estimation',option_name=>'METHOD',
						 problem_index => ($i-1), record_index=>0, 
						 option_index=>0);
	    if ((not defined $meth) or ($meth =~ /^(0|1|ZER|CON|HYB)/ )){
	      #undef is ok, default method is classical
	      #will get undef also if no estimation at all...
	      # if classical method (including no METH which means default) set MAXEVAL=0, done
	      $self -> set_option(record_name => 'estimation',option_name => 'MAXEVALS',
				  fuzzy_match => 1, option_value => '0', 
				  problem_numbers => [($i)]);
	    }elsif ($meth =~ /^IMP/ ){
	      # if IMP or IMPMAP set EONLY=1
	      $self -> set_option(record_name => 'estimation',option_name => 'EONLY',
				  fuzzy_match => 1, option_value => '1', 
				  problem_numbers => [($i)]);
	      if (defined $niter_eonly){
		croak("illegal value niter_eonly $niter_eonly") unless ($niter_eonly >= 0);
		$self -> set_option(record_name => 'estimation',option_name => 'NITER',
				    fuzzy_match => 1, option_value => $niter_eonly, 
				    problem_numbers => [($i)]);
	      } else {
		unless ($need_ofv){
		  #need to adjust NITER. set 1 or 0 unless need ofv
		  $self -> set_option(record_name => 'estimation',option_name => 'NITER',
				      fuzzy_match => 1, option_value => '0', 
				      problem_numbers => [($i)]);
		}
	      }
	    }else{
	      if (defined $niter_eonly){
		croak("illegal value niter_eonly $niter_eonly") unless ($niter_eonly >= 0);
		$self -> set_option(record_name => 'estimation',option_name => 'NITER',
				    fuzzy_match => 1, option_value => $niter_eonly, 
				    problem_numbers => [($i)]);
	      }
	      #if other method return error no success
	      #should this be changed to replace last est with something classical?
	      $success = 0;
	    }
	  }
	  my $warning = "\n\nMETHOD in last \$EST was not classical nor IMP/IMPMAP. Cannot set MAXEVAL=0 or EONLY=1.\n";
	  print $warning if ($print_warning and not $success);
	}
      }
# line 3232 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> set_maxeval_zero');
	# End of Non-Dia code #

	return $success;
}

sub set_union_estimation_record {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_numbers' => 'm_ARRAY', 'need_ofv' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->set_union_estimation_record: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->set_union_estimation_record: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->set_union_estimation_record: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->set_union_estimation_record: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->set_union_estimation_record: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my $need_ofv = $parm{'need_ofv'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> set_union_estimation_record');
# line 2891 "lib/model_subs.pm" 
      {
	#this will not work with CHAIN
	#Take first $EST. Loop over the following $EST. 
	#Loop over options in following EST, set each in first EST.
	#set record EST (remove all existing) to union record.
#	my @skiplist =('NOTITLE','NOLABEL','FORMAT','FILE','MSFO');
	my @skiplist =('NOTITLE','NOLABEL','FILE','MSFO');
	push (@skiplist,'IACCEPT','PACCEPT','OACCEPT','NSIGDIGITS','SIGDIGITS');
	push (@skiplist,'NBURN');
	push (@skiplist,'THETABOUNDTEST','NOTHETABOUNDTEST','NOTBT');
	push (@skiplist,'OMEGABOUNDTEST','NOOMEGABOUNDTEST','NOOBT');
	push (@skiplist,'SIGMABOUNDTEST','NOSIGMABOUNDTEST','NOSBT');
	push (@skiplist,'MAXEVALS','EONLY'); #will be set anyway
	my @skip_exact =('ISAMPLE_M1','ISAMPLE_M2','ISAMPLE_M3');
	push (@skip_exact,'PSAMPLE_M1','PSAMPLE_M2','PSAMPLE_M3');
	push (@skip_exact,'OSAMPLE_M1','OSAMPLE_M2','OSAMPLE_M3');

	my @problems;
	@problems = @{$self -> problems()} if (defined $self->problems());
	foreach my $probnum (@problem_numbers){
	  my $problem = $problems[$probnum-1];
	  my @estimations;
	  @estimations = @{$problem ->estimations()} if (defined $problem ->estimations());
	  next unless (scalar(@estimations)>0);
	  my $default_seed = '14455';
	  my @namesarray=('LAPLACIAN','INTERACTION','FO','CENTERING');
	  my @valuesarray=('NO','NO','NO','NO');
	  push(@namesarray,'SLOW','NUMERICAL','POSTHOC','ABORT','REPEAT','REPEAT1','REPEAT2');
	  push(@valuesarray,'NO','NO','NO','set','NO','NO','NO');
	  push (@namesarray,,'ETABARCHECK','SORT','DF','SEED','MUM');
	  push(@valuesarray, 'NO','NO','0',$default_seed,'unset');
	  die unless (scalar(@namesarray) == scalar(@valuesarray));
	  my @option_strings;
	  my $method;
	  my $niter;
	  my $pred='unset';
	  my $isample;
	  #value is 'set', 'NO' or 'value' or 'unset' 
	  foreach my $est (@estimations){
	    my @options;
	    @options =@{$est->options()} if (defined $est->options());
	    foreach my $opt (@options){
	      next unless (defined $opt->name());
	      my $name = $opt->name();
	      my $skip=0;
	      foreach my $string (@skiplist){
		if ($name eq $string or (index($string,$name) == 0)){
		  $skip=1;
		  last;
		}
	      }
	      next if ($skip);
	      foreach my $string (@skip_exact){
		if ($name eq $string ){
		  $skip=1;
		  last;
		}
	      }
	      next if ($skip);
	      if ($name eq 'METHOD' or (index('METHOD',$name) == 0)){
		croak("METHOD in \$EST requires a value") 
		    unless (defined $opt->value());
		$method = $opt->value();
		next;
	      }
	      if ($name eq 'NITER' or (index('NITER',$name) == 0)){
		croak("NITER in \$EST requires a value") 
		    unless (defined $opt->value());
		$niter = $opt->value();
		next;
	      }
	      if ($name eq 'ISAMPLE' or (index('ISAMPLE',$name) == 0)){
		croak("ISAMPLE in \$EST requires a value") 
		    unless (defined $opt->value());
		$isample = $opt->value();
		next;
	      }
	      if ($name eq 'PREDICTION' or (index('PREDICTION',$name) == 0)){
		$pred = 'unset';
		next;
	      }
	      if ($name eq 'LIKELIHOOD' or (index('LIKELIHOOD',$name) == 0)){
		$pred = $name;
		next;
	      }
	      if ($name eq '-2LOGLIKELIHOOD' or (index('-2LOGLIKELIHOOD',$name) == 0)
		  or $name eq '-2LLIKELIHOOD' or (index('-2LLIKELIHOOD',$name) == 0)){
		$pred = $name;
		next;
	      }
	      
	      my $val='set';
	      if (defined $opt->value() and ($opt->value() =~ /[^\s]/)){
		#value has something that is not whitespace
		$val = $opt->value();   
	      }else {
		#we have skipped NOTITLE, and NOPRIOR has a value and will not end up here
		#get rid of leading NO, do matching on the rest
		$val = 'NO' if ($name =~ s/^NO//);
	      } 
	      
	      for (my $i=0; $i< scalar(@namesarray);$i++){
		if (index($namesarray[$i],$name) == 0 ){
		  $valuesarray[$i]=$val;
		  $skip=1;
		  last;
		}
	      }
	      next if ($skip);

	      #not thar many should end up here, but format should
	      push(@namesarray,$name);
	      push(@valuesarray,$val);
	    }
	  } #end loop estimations

	  my $record_string;
	  if (not defined $method){
	    #undef is ok, default method is classical
	  }elsif ($method =~ /^(0|1|ZER|CON|HYB)/ ){
	    $record_string = 'METHOD='.$method; #maxeval handled outside this function
	  }elsif ($method =~ /^IMP/ ){
	    $niter = 10 unless (defined $niter);
	    $record_string = 'METHOD='.$method.' NITER='.$niter; #EONLY handled outside
	    $record_string .= ' ISAMPLE='.$isample if (defined $isample);
	  }else {
	    #set to IMP and hope for the best. 
	    # if not need ofv set isample to 1, otherwise leave isample to default 
	    $niter = 10 unless (defined $niter and $niter < 10); 
	    $record_string = 'METHOD=IMP NITER='.$niter; #EONLY handled outside
	    $record_string .= ' ISAMPLE=1' unless ($need_ofv);
	  }
	  $record_string .= ' '.$pred unless ($pred eq 'unset');

	  for (my $i=0;$i<scalar(@namesarray);$i++){
	    next if ($valuesarray[$i] eq 'unset');
	    if ($valuesarray[$i] eq 'NO'){
	      $record_string .= ' NOABORT' if ($namesarray[$i] eq 'ABORT');
	    }elsif  ($valuesarray[$i] eq 'set'){
	      $record_string .= ' '.$namesarray[$i] unless ($namesarray[$i] eq 'ABORT');
	    }elsif ($valuesarray[$i] eq '0'){
	      $record_string .= ' '.$namesarray[$i].'=0' unless ($namesarray[$i] eq 'DF');
	    }elsif ($valuesarray[$i] eq $default_seed){
	      $record_string .= ' '.$namesarray[$i].'=0' unless ($namesarray[$i] eq 'SEED');
	    }else{
	      $record_string .= ' '.$namesarray[$i].'='.$valuesarray[$i];
	    }
	  }
	  #replace all records
	  $self->set_records(problem_numbers=>[$probnum],
			     type => 'estimation',
			     record_strings => [$record_string]);
	} #end loop problems
      }
# line 3426 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> set_union_estimation_record');
	# End of Non-Dia code #

}

sub nomegas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_numbers' => 'ARRAY', 'with_correlations' => 'SCALAR',
			'with_same' => 'SCALAR', 'with_priors' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->nomegas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->nomegas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->nomegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->nomegas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->nomegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @nomegas;
	my $with_correlations = defined $parm{'with_correlations'} ? $parm{'with_correlations'} : 0;
	my $with_same = defined $parm{'with_same'} ? $parm{'with_same'} : 1;
	my $with_priors = defined $parm{'with_priors'} ? $parm{'with_priors'} : 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> nomegas');
# line 3347 "lib/model_subs.pm" 
      {
	  # returns the number of omegas in the model for the given
	  # problem number.
	unless( scalar(@problem_numbers)>0 ){
		$self->problems([]) unless defined $self->problems;
	  @problem_numbers = (1 .. $#{$self->problems}+1);
	}

	my @problems = @{$self->problems};
	foreach my $i ( @problem_numbers ) {
	  if ( defined $problems[ $i-1 ] ) {
	    push( @nomegas, $problems[$i-1]->nomegas( 
		    with_correlations => $with_correlations,
		    with_same => $with_same, with_priors => $with_priors));
	  } else {
	    croak("Problem number $i does not exist.");
	  } 
	}
      }
# line 3488 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> nomegas');
	# End of Non-Dia code #

	return \@nomegas;
}

sub nproblems {
	my $self = shift;
	my $number_of_problem;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> nproblems');
# line 3310 "lib/model_subs.pm" 
      {
        # nproblems returns the number of problems in the modelobject.
	
	$number_of_problem = scalar @{$self->problems};
      }
# line 3507 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> nproblems');
	# End of Non-Dia code #

	return $number_of_problem;
}

sub nsigmas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_numbers' => 'ARRAY', 'with_correlations' => 'SCALAR',
			'with_same' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->nsigmas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->nsigmas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->nsigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->nsigmas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->nsigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @nsigmas;
	my $with_correlations = defined $parm{'with_correlations'} ? $parm{'with_correlations'} : 0;
	my $with_same = defined $parm{'with_same'} ? $parm{'with_same'} : 1;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> nsigmas');
# line 3374 "lib/model_subs.pm" 

# returns the number of sigmas in the model for the given problem number.

unless( scalar(@problem_numbers)>0 ){
	$self->problems([]) unless defined $self->problems;
  @problem_numbers = (1 .. $#{$self->problems}+1);
}

my @problems = @{$self->problems};
foreach my $i ( @problem_numbers ) {
  if ( defined $problems[ $i-1 ] ) {
    push( @nsigmas, $problems[ $i-1 ] -> nsigmas( with_correlations => $with_correlations,
						  with_same => $with_same));
  } else {
    croak("Problem number $i does not exist.");
  } 
}

# line 3568 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> nsigmas');
	# End of Non-Dia code #

	return \@nsigmas;
}

sub nthetas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_number' => 'SCALAR', 'with_priors' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->nthetas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->nthetas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->nthetas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->nthetas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->nthetas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $problem_number = defined $parm{'problem_number'} ? $parm{'problem_number'} : 1;
	my $with_priors = defined $parm{'with_priors'} ? $parm{'with_priors'} : 0;
	my $nthetas;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> nthetas');
# line 3323 "lib/model_subs.pm" 
      {
	  # returns the number of thetas in the model for the given
	  # problem number.

	unless( defined $self->problems()->[$problem_number - 1] ){
	  croak("problem number $problem_number does not exist" );
	}

	if((not $with_priors) and 
	   defined $self->problems()->[$problem_number - 1] -> nwpri_ntheta()){
	  $nthetas = $self->problems()->[$problem_number - 1] -> nwpri_ntheta();
	}else{
	  $nthetas = 
	      $self -> _parameter_count( 'record' => 'theta', 
					 'problem_number' => $problem_number );
	}
      }
# line 3626 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> nthetas');
	# End of Non-Dia code #

	return $nthetas;
}

sub pk {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'new_pk' => 'ARRAY', 'problem_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->pk: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->pk: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->pk: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->pk: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->pk: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @new_pk = defined $parm{'new_pk'} ? @{$parm{'new_pk'}} : ();
	my $problem_number = defined $parm{'problem_number'} ? $parm{'problem_number'} : 1;
	my @pk;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> pk');
# line 3432 "lib/model_subs.pm" 
      {
	# sets or gets the pk code for a given problem in the
	# model object. The new_pk argument should be an array where
	# each element contains a row of a valid NONMEM $PK block,

	my @prob = @{$self -> problems};
	
	unless( defined $prob[$problem_number - 1] ){
	  croak("Problem number $problem_number does not exist" );
	}
	
	my $pks = $prob[$problem_number - 1] -> pks;
	if( scalar @new_pk > 0 ) {
	  if( defined $pks and scalar @{$pks} > 0 ){
	    $prob[$problem_number - 1] -> pks -> [0] -> code(\@new_pk);
	  } else {
	    croak("No \$PK record" );
	  }
	} else {
	  if ( defined $pks and scalar @{$pks} > 0 ) {
	    @pk = @{$prob[$problem_number - 1] -> pks -> [0] -> code};
	  }
	}
      }
# line 3691 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> pk');
	# End of Non-Dia code #

	return \@pk;
}

sub pred {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'new_pred' => 'ARRAY', 'problem_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->pred: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->pred: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->pred: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->pred: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->pred: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @new_pred = defined $parm{'new_pred'} ? @{$parm{'new_pred'}} : ();
	my $problem_number = defined $parm{'problem_number'} ? $parm{'problem_number'} : 1;
	my @pred;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> pred');
# line 3463 "lib/model_subs.pm" 
      {
	# Sets or gets the pred code for a given problem in the model
	# object. See L</pk> for details.
	my @prob = @{$self -> problems};
	
	unless( defined $prob[$problem_number - 1] ){
	  croak("problem number $problem_number does not exist" );
	}
	
	if( scalar @new_pred > 0 ) {
	  if( defined $prob[$problem_number - 1] -> preds ){
	    $prob[$problem_number - 1] -> preds -> [0] -> code(\@new_pred);
	  } else {
	    croak("No \$PRED record" );
	  }
	} else {
	  if ( defined $prob[$problem_number - 1] -> preds ) {
	    @pred = @{$prob[$problem_number - 1] -> preds -> [0] -> code};
	  } else {
	    croak("No \$PRED record" );
	  }
	}
      }
# line 3755 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> pred');
	# End of Non-Dia code #

	return \@pred;
}

sub print {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> print');
# line 3493 "lib/model_subs.pm" 
      {
	# Prints the formatted model to standard out.
	
	my ( @formatted );
	foreach my $problem ( @{$self->problems} ) {
	    foreach my $line (@{$problem-> _format_problem}){
		print $line;
	    }
	}
      }
# line 3778 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> print');
	# End of Non-Dia code #

}

sub record {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'record_name' => 'SCALAR', 'new_data' => 'ARRAY',
			'problem_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->record: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->record: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->record: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->record: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->record: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $record_name = $parm{'record_name'};
	my @new_data = defined $parm{'new_data'} ? @{$parm{'new_data'}} : ();
	my $problem_number = defined $parm{'problem_number'} ? $parm{'problem_number'} : 1;
	my @data;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> record');
# line 3548 "lib/model_subs.pm" 
     {
	 # If the argument new_data is given, record sets new_data in
	 # the model objects member specified with record_name. The
	 # format of new_data is an array of strings, where each
	 # element corresponds to a line of code as it would have
	 # looked like in a valid NONMEM modelfile. If new_data is left
	 # undefined, record returns lines of code belonging to the
	 # record specified by record_name in a format that is valid in
	 # a NONMEM modelfile.
	 
	 my @problems = @{$self->problems};
	 my $records;
	 
	 if ( defined $problems[ $problem_number - 1 ] ) {
	     if ( scalar(@new_data) > 0 ){
		 my $rec_class = "model::problem::$record_name";
		 my $record = $rec_class -> new('record_arr' => \@new_data );
	     } else {
		 $record_name .= 's';
		 $records = $problems[ $problem_number - 1 ] -> {$record_name};
		 foreach my $record( @{$records} ){
		     push(@data, $record -> _format_record);
		 }
	     }
	 }
     }
# line 3846 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> record');
	# End of Non-Dia code #

	return \@data;
}

sub get_option_value {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'record_name' => 'm_SCALAR', 'option_name' => 'm_SCALAR',
			'problem_index' => 'SCALAR', 'record_index' => 'SCALAR',
			'option_index' => 'SCALAR', 'fuzzy_match' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->get_option_value: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->get_option_value: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->get_option_value: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->get_option_value: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->get_option_value: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $record_name = $parm{'record_name'};
	my $option_name = $parm{'option_name'};
	my $problem_index = defined $parm{'problem_index'} ? $parm{'problem_index'} : 0;
	my $record_index = defined $parm{'record_index'} ? $parm{'record_index'} : 0;
	my $option_index = defined $parm{'option_index'} ? $parm{'option_index'} : 0;
	my $fuzzy_match = defined $parm{'fuzzy_match'} ? $parm{'fuzzy_match'} : 1;
	my $return_value;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> get_option_value');
# line 5027 "lib/model_subs.pm" 
    {
	#$modelObject -> get_option_value(record_name => 'recordName', option_name => 'optionName',
        #                         problem_index => <index>, record_index => <index>/'all', 
	#                         option_index => <index>/'all',
	#                         fuzzy_match => 1/0)
	# record_name and option_name are required. All other have default 0. Fuzzy match default 1.
	#record_index and option_index may either be scalar integer or string 'all'.
	# Depending on input parameters the return value can be 
	# Case 1. a scalar for record_index => integer, option_index => integer
	# Case 2. a reference to an array of scalars for (record_index=>'all',option_index => integer) 
	# Case 3. a reference to an array of scalars for (record_index=>integer,option_index => 'all') 
	# Case 4. a reference to an array of references to arrays for (record_index=>'all',option_index => 'all')  
	my ( @problems, @records, @options );
	my $accessor = $record_name.'s';
	my @rec_arr;
	my $fail;

	#Basic error checking. Error return type is undef for Case 1
	#and reference to empty array for Case 2 and 3 and 4.
	
	if (lc($record_index) eq 'all' || lc($option_index) eq 'all' ){
	    $fail = [];
	} else {
	    $fail =  undef;
	}

	if ( defined $self->problems ) {
	    @problems = @{$self->problems};
	} else {
	    carp("No problems defined in model" );
	    return $fail;
	}
	unless( defined $problems[$problem_index] ){
	    carp("model -> get_option_value: No problem with ".
			     "index $problem_index defined in model" );
	    return $fail;
	}
	
	if ( defined $problems[$problem_index] -> $accessor ) {
	    @records = @{$problems[$problem_index] -> $accessor};
	} else {
	    carp("model -> get_option_value: No record $record_name defined" .
			     " in problem with index $problem_index." );
	    return $fail;
	}

	#go through all records, whole array is of correct type.
	#if current record is the single we want, investigare option values and break out of loop
	#if we want to look at all records, investigare option values and continue with loop
      REC: for (my $ri=0; $ri<scalar(@records); $ri++){
	  if ((lc($record_index) eq 'all') || $record_index==$ri){
	      my @val_arr = ();
	      unless ((defined $records[$ri]) &&( defined $records[$ri] -> options )){
		  carp("model -> get_option_value: No options for record index ".
				   "$record_index defined in problem." );
		  if (lc($record_index) eq 'all'){
		      if (lc($option_index) eq 'all'){
			  push(@rec_arr,[]); #Case 4
		      } else {
			  push(@rec_arr,undef); #Case 2
		      }
		      next REC;
		  } else {
		      if (lc($option_index) eq 'all'){
			  $return_value = []; #Case 3
		      } else {
			  $return_value = undef; #Case 1
		      }
		      last REC; #we are done
		  }
	      }
	      @options = @{$records[$ri] -> options};
	      my $oi=-1;
	      my $val;
	      #go through all options (array contains all options, regardless of name). 
	      # For each check if it the correct type, if so 
	      #increase counter $oi after possibly storing the option value
	      #if current correct option is the single we want value for, then 
	      #store value and break out of loop. If want to store values for 
	      #all correct options, store value and then continue with loop
	      foreach my $option ( @options ) {
		  if (defined $option and 
		      (($option->name eq $option_name) || ($fuzzy_match and  index($option_name,$option ->name ) == 0))){
		      $oi++; #first is 0
		      if (lc($option_index) eq 'all' || $option_index == $oi){
			  if ( (defined $option->value) and ($option->value ne '')){
			      $val = $option->value;
			  } else {
			      $val = undef;
			  }
			  if (lc($option_index) eq 'all'){
			      push(@val_arr,$val); #Case 3 and 4
			  } else {
			      last; #Case 1 and 2.  Take care of $val outside loop over options
			  }
		      }
		  }
	      }
	      if (lc($record_index) eq 'all'){
		  if (lc($option_index) eq 'all'){
		      push(@rec_arr,\@val_arr); #Case 4
		  } else {
		      push(@rec_arr,$val); #Case 2
		  }
		  next REC;
	      } else {
		  if (lc($option_index) eq 'all'){
		      $return_value = \@val_arr; #Case 3
		  } else {
		      $return_value = $val; #Case 1
		  }
		  last REC;
	      }
	  }
      }
	if (lc($record_index) eq 'all'){
	    $return_value = \@rec_arr; #Case 2 and 4
	}
	
    }
# line 4013 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> get_option_value');
	# End of Non-Dia code #

	return $return_value;
}

sub restore_inits {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> restore_inits');
# line 3695 "lib/model_subs.pm" 
      {
	# restore_inits brings back initial values previously stored
	# using store_inits. This method pair allows a user to store
	# the currents initial values in a backup, replace them with
	# temporary values and later restore them.

	if ( defined $self->problems ) {
	  foreach my $problem ( @{$self->problems} ){
	    $problem -> restore_inits;
	  }
	}
      }
# line 4038 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> restore_inits');
	# End of Non-Dia code #

}

sub set_records {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'type' => 'SCALAR', 'record_strings' => 'm_ARRAY',
			'problem_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->set_records: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->set_records: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->set_records: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->set_records: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->set_records: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $type = $parm{'type'};
	my @record_strings = defined $parm{'record_strings'} ? @{$parm{'record_strings'}} : ();
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> set_records');
# line 743 "lib/model_subs.pm" 
      {
	unless( scalar(@problem_numbers)>0 ){
		$self->problems([]) unless defined $self->problems;
	  @problem_numbers = (1 .. $#{$self->problems}+1);
	}

	my @problems = @{$self->problems};
	foreach my $i ( @problem_numbers ) {
	  if ( defined $problems[ $i-1 ] ) {
	    $problems[$i-1] -> set_records( 'type' => $type,
					    'record_strings' => \@record_strings );
	  } else {
	    croak("Problem number $i does not exist." );
	  } 
	}
      }
# line 4095 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> set_records');
	# End of Non-Dia code #

}

sub store_inits {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> store_inits');
# line 3714 "lib/model_subs.pm" 
      {
	# store_inits stores initial values that can later be
	# brought back using restore_inits. See L</restore_inits>.

	if ( defined $self->problems ) {
	  foreach my $problem ( @{$self->problems} ){
	    $problem -> store_inits;
	  }
	}
      }
# line 4117 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> store_inits');
	# End of Non-Dia code #

}

sub __sync_output {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub synchronize {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> synchronize');
# line 3731 "lib/model_subs.pm" 
      {
	  # Synchronize checks the I<synced> object attribute to see
	  # if the model is in sync with its corresponding file, given
	  # by the objetc attribute I<filename>. If not, it checks if
	  # the model contains any defined problems and if it does, it
	  # writes the formatted model to disk, overwriting any
	  # existing file of name I<filename>. If no problem is
	  # defined, synchronize tries to parse the file I<filename>
	  # and set the object internals to match it.
	unless( $self->synced ){
	  if( defined $self->problems and scalar @{$self->problems} > 0 ){
	    $self -> _write;
	  } else {
	    if( -e $self -> full_name ){
	      $self -> _read_problems;
	    } else {
	      return;
	    }
	  }
	}
	$self->synced(1);
      }
# line 4159 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> synchronize');
	# End of Non-Dia code #

}

sub msfi_names {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'new_names' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->msfi_names: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->msfi_names: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->msfi_names: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->msfi_names: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->msfi_names: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @new_names = defined $parm{'new_names'} ? @{$parm{'new_names'}} : ();
	my @names = ();

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> msfi_names');
# line 3792 "lib/model_subs.pm" 
# Usage:
#
#    @msfiNames = @{$modobj -> msfi_names};
#
#    or better:
#
#    $msfiNamesRef = $modobj -> msfi_names;
#    @msfiNames = @{$msfiNamesRef} if (defined $msfiNamesRef);
#
# This basic usage takes no arguments and returns the value of
# the MSFI option in the $ESTIMATION NONMEM record of each
# problem. @msfiNames will be a two-dimensional array:
#
#   [[msfiName_prob1],[msfiName_prob2],[msfiName_prob3]...]
#

my @problems;
if ( defined $self->problems ) {
  @problems = @{$self->problems};
} else {
  croak("No problems defined in model" );
}

if( scalar @new_names > 0 ) {
  my $i = 0;
  foreach my $prob ( @problems ) {
    if( defined $new_names[$i] ) {
      if ( defined $prob -> msfis() ) {
	my @instances = @{$prob -> msfis()};
	my @prob_names;
	my $j=0;
	foreach my $instance ( @instances ) {
	  if (defined $new_names[$i]->[$j]){
	    my @options;
	    if ( defined $instance -> options() ) {
	      @options = @{$instance -> options()};
	    }
	    if ( defined $options[0] ) {
	      $options[0] -> name($new_names[$i]->[$j]);
	    }
	  }
	  $j++;
	}
      }else{
	$prob -> add_records( type           => 'msfi',
			      record_strings => $new_names[$i] );      
      }
    }
    $i++;
  }
} else {
  foreach my $prob ( @problems ) {
    if ( defined $prob -> msfis() ) {
      my @instances = @{$prob -> msfis()};
      my @prob_names;
      foreach my $instance ( @instances ) {
	my @options;
	if ( defined $instance -> options() ) {
	  @options = @{$instance -> options()};
	}
	if ( defined $options[0] ) {
	  push( @prob_names, $options[0] -> name );
	} else {
	  push( @prob_names, undef );
	}	
      }
      push( @names, \@prob_names );
    }else{
      push( @names, undef );
    }
  }
}

# line 4271 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> msfi_names');
	# End of Non-Dia code #

	return \@names;
}

sub msfo_names {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'new_names' => 'ARRAY', 'problem_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->msfo_names: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->msfo_names: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->msfo_names: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->msfo_names: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->msfo_names: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @new_names = defined $parm{'new_names'} ? @{$parm{'new_names'}} : ();
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @names = ();

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> msfo_names');
# line 3907 "lib/model_subs.pm" 
# Usage:
#
#    @msfoNames = @{$modobj -> msfo_names};
#
#    or better:
#
#    $msfoNamesRef = $modobj -> msfo_names;
#    @msfoNames = @{$msfoNamesRef} if (defined $msfoNamesRef);
#
# This basic usage takes no arguments and returns the value of
# the MSFO option in the $ESTIMATION NONMEM record of each
# problem. @msfoNames will be an array:
#
#   [msfoName_prob1,msfoName_prob2,msfoName_prob3...]
#
#
# If the I<new_names> argument of msfo_names is given, the
# values of the MSFO options will be changed.
#
# To set the MSFO of specific problems, the I<problem_numbers>
# argument can be used. It should be a reference to an array
# containing the numbers of all problems where the FILE should
# be changed or retrieved. If specified, the size of
# I<new_names> must be the same as the size of
# I<problem_numbers>.

my ( $name_ref, $junk ) = $self -> 
    _option_val_pos( name	     => 'MSFO',
		     record_name     => 'estimation',
		     problem_numbers => \@problem_numbers,
		     new_values	     => \@new_names );


my ( $nonp_name_ref, $junk ) = $self ->
    _option_val_pos( name            => 'MSFO',
		     record_name     => 'nonparametric',
		     problem_numbers => \@problem_numbers,
		     new_values      => \@new_names );

if( scalar( @{$name_ref -> [0]} > 0 ) ){
  push( @names, @{$name_ref} );
}

if( scalar( @{$nonp_name_ref -> [0]} > 0 ) ){
  push( @names, @{$nonp_name_ref} );
}

# line 4359 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> msfo_names');
	# End of Non-Dia code #

	return \@names;
}

sub table_names {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'new_names' => 'ARRAY', 'problem_numbers' => 'ARRAY',
			'ignore_missing_files' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->table_names: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->table_names: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->table_names: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->table_names: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->table_names: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @new_names = defined $parm{'new_names'} ? @{$parm{'new_names'}} : ();
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @names = ();
	my $ignore_missing_files = defined $parm{'ignore_missing_files'} ? $parm{'ignore_missing_files'} : 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> table_names');
# line 3962 "lib/model_subs.pm" 
      {
	# Usage:
	#
	#    @tableNames = @{$modobj -> table_names};
	#
	# This basic usage takes no arguments and returns the value of
	# the FILE option in the $TABLE NONMEM record of each
	# problem. @tableNames will be a two dimensional array:
	#
	#   [[tableName_prob1][tableName_prob2][tableName_prob3]...]
	#
	#
	# If the I<new_names> argument of table_names is given, the
	# values of the FILE options will be changed.
	#
	# To set the FILE of specific problems, the I<problem_numbers>
	# argument can be used. It should be a reference to an array
	# containing the numbers of all problems where the FILE should
	# be changed or retrieved.  If specified, the size of
	# I<new_names> must be the same as the size of
	# I<problem_numbers>.
	#
	# The I<ignore_missing_files> boolean argument can be used to
	# set names of table that does not exist yet (e.g. before a
	# run has been performed).

	my ( $name_ref, $junk ) = $self -> 
	  _option_val_pos( name		   => 'FILE',
			   record_name	   => 'table',
			   problem_numbers => \@problem_numbers,
			   new_values	   => \@new_names );
	if ( $#new_names >= 0 ) {
	  my @problems = @{$self->problems};
	  unless( $#problem_numbers > 0 ){
			$self->problems([]) unless defined $self->problems;
	    @problem_numbers = (1 .. $#{$self->problems}+1);
	  }
	  foreach my $i ( @problem_numbers ) {
	    $problems[$i-1] -> _read_table_files( ignore_missing_files => $ignore_missing_files || $self->ignore_missing_output_files);
	  }
	}
	@names = @{$name_ref};
      }
# line 4445 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> table_names');
	# End of Non-Dia code #

	return \@names;
}

sub units {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'parameter_type' => 'SCALAR', 'parameter_numbers' => 'ARRAY',
			'problem_numbers' => 'ARRAY', 'new_values' => 'ARRAY',
			'with_priors' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->units: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->units: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->units: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->units: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->units: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $parameter_type = $parm{'parameter_type'};
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
	my $with_priors = defined $parm{'with_priors'} ? $parm{'with_priors'} : 0;
	my @units;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> units');
# line 4052 "lib/model_subs.pm" 
      {
	  # Sets or gets the units of a (number of) parameter(s). The
	  # unit is not a proper NONMEM syntax but is recognized by
	  # the PsN model class. A unit (and a label) can be specified
	  # as a comments after a parameter definition. e.g.:
          #
          #    $THETA (0,13.2,100) ; MTT; h
          #
          # which will give this theta the label I<MTT> and unit I<h>.
	@units = @{ $self -> _init_attr( parameter_type    => $parameter_type,
					 parameter_numbers => \@parameter_numbers,
					 problem_numbers           => \@problem_numbers,
					 new_values        => \@new_values,
					 with_priors       => $with_priors,
					 attribute              => 'unit')};
      }
# line 4507 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> units');
	# End of Non-Dia code #

	return \@units;
}

sub add_randomized_columns {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'column_headers' => 'ARRAY', 'filename' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->add_randomized_columns: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->add_randomized_columns: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->add_randomized_columns: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->add_randomized_columns: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->add_randomized_columns: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @column_headers = defined $parm{'column_headers'} ? @{$parm{'column_headers'}} : ();
	my $filename = $parm{'filename'};
	my @xcolumn_names;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> add_randomized_columns');
# line 260 "lib/model_subs.pm" 
      {
	#first prob only 
	#in array column_headers
	#in scalar datafilename, modelfilename
	#out array xcolumn_names

	@xcolumn_names = @{$self -> datas -> [0] -> add_randomized_columns(
			     filename => $filename,
			     directory => $self->directory(),
			     column_headers => \@column_headers)}; #writes to own filename
	#after changing it to directory/filename
	

	foreach my $xcol (@xcolumn_names){
	  $self -> add_option( record_name  => 'input',
			       problem_numbers => [1],
			       option_name  => $xcol);
	}
	$self -> _option_name( position	  => 0,
			       record	  => 'data',
			       problem_number => 1,
			       new_name	  => $filename);

	$self->_write(write_data => 0); #data already written

      }
# line 4574 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> add_randomized_columns');
	# End of Non-Dia code #

	return \@xcolumn_names;
}

sub update_inits {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'from_output' => 'output', 'from_output_file' => 'SCALAR',
			'from_model' => 'model', 'from_hash' => 'REF',
			'problem_number' => 'SCALAR', 'ignore_missing_parameters' => 'SCALAR',
			'ensure_diagonal_dominance' => 'SCALAR', 'update_omegas' => 'SCALAR',
			'update_sigmas' => 'SCALAR', 'update_thetas' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->update_inits: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->update_inits: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->update_inits: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->update_inits: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->update_inits: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $from_output = $parm{'from_output'};
	my $from_output_file = $parm{'from_output_file'};
	my $from_model = $parm{'from_model'};
	my $from_hash = $parm{'from_hash'};
	my $problem_number = $parm{'problem_number'};
	my $ignore_missing_parameters = defined $parm{'ignore_missing_parameters'} ? $parm{'ignore_missing_parameters'} : 0;
	my $ensure_diagonal_dominance = defined $parm{'ensure_diagonal_dominance'} ? $parm{'ensure_diagonal_dominance'} : 0;
	my $update_omegas = defined $parm{'update_omegas'} ? $parm{'update_omegas'} : 1;
	my $update_sigmas = defined $parm{'update_sigmas'} ? $parm{'update_sigmas'} : 1;
	my $update_thetas = defined $parm{'update_thetas'} ? $parm{'update_thetas'} : 1;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> update_inits');
# line 4153 "lib/model_subs.pm" 
      {
	# Usage:
	#
	#   $modobj -> update_inits ( from_output => $outobj );
	#
	# alt
	#
	#   $modobj -> update_inits ( from_output_file => $outfile );
	#
	# This basic usage takes the parameter estimates from the
	# output object I<$outobj> or from the output file I<$outfile>
	# and updates the initial estimates in the model object
	# I<$modobj>. The number of problems and parameters must be
	# the same in the model and output objects unless parameter
	# problem_number has been defined. If there exists
	# more than one subproblem per problem in the output object,
	# only the estimates from the first subproblem will be
	# transferred.
	#
	#   $modobj -> update_inits ( from_output               => $outobj,
	#                             ignore_missing_parameters => 1 );
	#
	# If the ignore_missing_parameters argument is set to 1, the number of
	# parameters in the model and output objects do not need to match. The
	# parameters that exist in both objects are used for the update of the
	# model object.
	#
	#   $modobj -> update_inits ( from_model                => $from_modobj );
	#
	# If the from_model argument is given, update_inits tries to match the
	# parameter names (labels) given in $from_modobj and $modobj and
	# and thereafter updating the $modobj object. See L</units> and L</labels>.
	#

	croak("update_inits: No output object defined and" . 
		      " no output object found through the model object specified." )
	    unless ( ( defined $from_model and 
		       ( defined $from_model -> outputs and 
			 defined $from_model -> outputs->[0] ) ) or
		     defined $from_output or
		     defined $from_output_file or
		     defined $from_hash);
	if ((defined $from_model) and ((defined  $from_output )or (defined  $from_output_file ))){
	  croak("update_inits: Illegal usage, cannot specify both from_model" . 
			  " and from_output(_file)." )
	}
	if ((defined $from_hash) and ((defined  $from_output )or (defined  $from_output_file ))){
	  croak("update_inits: Illegal usage, cannot specify both from_hash" . 
			  " and from_output(_file)." )
	}
	if ((defined $from_hash) and (defined  $from_model )){
	  croak("update_inits: Illegal usage, cannot specify both from_hash" . 
			  " and from_model." )
	}
	my %allparams;
	if ( defined $from_output ) {
	  carp("using output object ".
			 "specified as argument\n" );
	} elsif ( defined $from_hash ) {
	  $from_output = undef;
	  %allparams = %{$from_hash};
	} elsif ( defined $from_output_file ) {
	  $from_output = output -> new( filename => $from_output_file);
	} else {
	  $from_output = @{$from_model -> outputs}[0]; #assume 1st $PROB??
	}

	my @params = ();
	if( $update_thetas ){
	  push( @params, 'theta' );
	}
	if( $update_omegas ) {
	  push( @params, 'omega' );
	}
	if( $update_sigmas ) {
	  push( @params, 'sigma' );
	}

	foreach my $param ( @params ) {
	  my ( @intermediate_coordslabels, @from_coordval );
	  my $access = $param.'coordval';
	  my $from_string;
	  my @problems = @{$self->problems};
	  my %param_hash;

	  if (defined $from_hash){
	    unless (($#problems == 0) or (defined $problem_number)){
	      croak("Updating initial estimates from hash ".
			      "can only be done with single PROB models unless parameter ".
			      "problem number has been defined");
	    }
	    #go directly to defining %namesvalues below
	    $from_string = 'input hash';
	  }else {
	    # Since initial estimates are specified on the problem level and not on
	    # the subproblem level we use the estimates from the outputs first subproblem
	    @from_coordval = @{$from_output -> $access ( subproblems => [1] )};
	    if ( defined $from_model ) {
	      @intermediate_coordslabels = 
		  @{$from_model -> get_coordslabels( parameter_type => $param )};
	      $from_string = 'from-model '.$from_model -> full_name();
	      croak("The number of problems are not the same in ".
			      "$from_string (".($#intermediate_coordslabels+1).")".
			      " and the model to be updated ".$self -> full_name." (".
			      ($#problems+1).")" ) 
		  unless ($#problems == $#intermediate_coordslabels );
	    } else {
	      $from_string = 'from-output '.$from_output -> full_name();
	      if (defined $problem_number){
		croak("The problem number to update ($problem_number) ".
				"does not exist in $from_string (only ".($#from_coordval+1)." problems)")
		    unless ($problem_number<=($#from_coordval +1));
		croak("The problem number to update ($problem_number) ".
				"does not exist in the model to be updated ".
				$self -> full_name." ( only ".($#problems+1)." problems)")
		    unless ($problem_number <=($#problems +1));
	      }else{
		croak("The number of problems are not the same in $from_string ".
				" (".($#from_coordval+1).")".
				" and the model to be updated ".
				$self -> full_name." (".
				($#problems+1).")" ) unless ( $#problems == $#from_coordval );
	      }
	    }
	  }

	  # Loop over the problems:
	  for ( my $i = 0; $i <= $#problems; $i++ ) {
	    next if (defined $problem_number and (($problem_number-1) != $i));
	    my $problem = $problems[$i];
	    unless ( defined  $problem) {
	      croak("Problem number ".($i+1)." does not exist" );
	    }
	    my $accessor = $param.'s';
	    unless( $problem-> can($accessor) ){
	      croak("Error unknown parameter type: $param" );
	    }
	    my @records;
	    if (defined $problem -> $accessor()) {
	      @records = @{$problem -> $accessor()};
	    }
	    next unless (scalar(@records) > 0); #no parameter in this problem

	    my @diagnostics;
	    my %namesvalues;

	    #if we have intermediate then first match coordinates between intermediate and from,
	    #and replace 'from' coordinates with the intermediate labels, if available

	    if (defined $from_hash){
	      %namesvalues = %{$allparams{$param}};
	    }else{
	      if (defined $from_coordval[$i]){
		unless (defined $from_coordval[$i]->[0]){
		  croak("No $param values read from output for problem ".($i+1));
		}
		if ( defined $from_model and defined $intermediate_coordslabels[$i]) {
		  my %fromval = %{$from_coordval[$i]->[0]};
		  my %intermediate = %{$intermediate_coordslabels[$i]};
		  foreach my $coord (keys %fromval){
		    #if there is no label for the coord, get_coordslabels stores the coordinate string 
		    #as the hash value. Always defined.
		    my $name = $intermediate{$coord}; #this is either coordinate string or label
		    $namesvalues{$name} = $fromval{$coord}; 
		  }
		}else {
		  %namesvalues = %{$from_coordval[$i]->[0]};
		}
	      }
	    }
	    #loop through records and options
	    #if record is same then skip
	    #if fix but not ignore_missing we still match values to see if any missing
	    #name of own param is label if from_model defined and label defined, otherwise coord
	    #look up value in namesvalues hash, replace value with "matched"

	    my $any_same=0;
	    foreach my $record (@records){
	      next if ($record->prior());
	      my $store_rec = 1;
	      if  ($record->same() ){
		$any_same=1; # we can match nothing, cannot do error check
		next;
	      }
	      if  ($record->fix()){
		$store_rec = 0; 
		next if ($ignore_missing_parameters or $any_same);
	      }
	      unless (defined $record -> options()){
		  croak("$param record has no values");
	      }
	      foreach my $option (@{$record -> options()}) {
		next if ($option->prior());
		my $store_val = $store_rec;
		if ($option->fix()){
		  $store_val = 0;
		  next if ($ignore_missing_parameters or $any_same);
		}
		my $name = $option -> coordinate_string();
		if (((defined $from_model) or (defined $from_hash)) and 
		    (defined $option -> label())){
		  $name = $option -> label();#do matching on label instead of coordinate
		}
		if (defined $namesvalues{$name}){
		  my $value = $namesvalues{$name};
		  croak("Multiple instances of label $name in problem to update, ".
		      "ambiguous parameter matching by label.") 
		      if ($value eq 'matched');
		  if ($store_val){
		    push( @diagnostics,
			  $option -> check_and_set_init( new_value => $value ) );
		  }
		  $namesvalues{$name} = 'matched';
		} else {
		  unless ($ignore_missing_parameters){
		    unless ($option->value() == 0 and (not $option->on_diagonal())){
		      my $mes = "update_inits: No match for $param $name found in $from_string";
		      carp($mes);
		      print $mes."\n";
		    }
		  }
		}
	      }
	    }

	    #check if any values not matched
	    #we store same but they will not be matched even if all correct,
	    #so this only catches errors if no same
	    unless ($ignore_missing_parameters or $any_same){
	      foreach my $name (keys %namesvalues){
		croak("No match for $param ".
			    "$name found in problem to update") 
		    if (($namesvalues{$name} ne 'matched') and ($namesvalues{$name} != 0));
	      }
	    }
	    if (($param eq @params[$#params]) and ($update_omegas or $update_sigmas) and $ensure_diagonal_dominance){
		$problem->ensure_diagonal_dominance(verbose => 1); #ensure_diagonal_dominance only set from update_inits program
	    }
	  } #each problem
	} #each param
      }
# line 4867 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> update_inits');
	# End of Non-Dia code #

}

sub upper_bounds {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'parameter_type' => 'SCALAR', 'parameter_numbers' => 'ARRAY',
			'problem_numbers' => 'ARRAY', 'with_priors' => 'SCALAR',
			'new_values' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->upper_bounds: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->upper_bounds: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->upper_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->upper_bounds: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->upper_bounds: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $parameter_type = $parm{'parameter_type'};
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my $with_priors = defined $parm{'with_priors'} ? $parm{'with_priors'} : 0;
	my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
	my @upper_bounds;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> upper_bounds');
# line 4403 "lib/model_subs.pm" 
      {
	# upper_bounds either sets or gets the initial values of the
	# parameter specified in I<parameter_type> for each
	# subproblem specified in I<problem_numbers>. For each
	# element in I<problem_numbers> there must be an array in
	# I<parameter_numbers> that specify the indices of the
	# parameters in the subproblem for which the upper bounds
	# are set, replaced or retrieved.

	@upper_bounds = @{ $self -> _init_attr
			     ( parameter_type    => $parameter_type,
			       parameter_numbers => \@parameter_numbers,
			       problem_numbers           => \@problem_numbers,
			       new_values        => \@new_values,
			       with_priors       => $with_priors,
			       attribute         => 'upbnd')};
      }
# line 4929 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> upper_bounds');
	# End of Non-Dia code #

	return \@upper_bounds;
}

sub _write {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'filename' => 'SCALAR', 'number_format' => 'SCALAR',
			'write_data' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->_write: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->_write: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->_write: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->_write: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->_write: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $filename = defined $parm{'filename'} ? $parm{'filename'} : $self -> full_name;
	my $number_format = $parm{'number_format'};
	my $write_data = defined $parm{'write_data'} ? $parm{'write_data'} : 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _write');
# line 4440 "lib/model_subs.pm" 
      {
	# 
	# $model -> _write( filename => 'model.mod' );
	#
	# Writes the content of the modelobject to disk. Either to the
	# filename given, or to the string returned by model::full_name.

	my @formatted;

	# An element in the active_problems array is a boolean that
	# corresponds to the element with the same index in the problems
	# array.  If the boolean is true, the problem will be run. All
	# other will be commented out.
	my @active = @{$self->active_problems};

	# loop over all problems.
	for ( my $i = 0; $i < scalar @{$self->problems}; $i++ ) {
	  # Call on the problem object to format it as text. The
	  # filename and problem numbers are needed to make some
	  # autogenerated files (msfi, tabels etc...) unique to the
	  # model and problem
	  my @preformatted = @{$self->problems -> [$i] ->
 				   _format_problem( filename => $self -> filename,
						    problem_number => ($i+1),
						    number_format => $number_format) };
	  # Check if the problem is NOT active, if so comment it out.
	  unless ( $active[$i] ) {
	    for ( my $j = 0; $j <= $#preformatted; $j++ ) {
	      $preformatted[$j] = '; '.$preformatted[$j];
	    }
	  }
	  # Add extra line to avoid problems with execution of NONMEM
	  push(@preformatted,"\n");
	  push( @formatted, @preformatted );
	}

	# Open a file and print the formatted problems.
	# TODO Add some errorchecking.
	open( FILE, '>'. $filename );
	for ( @formatted ) {
	  chomp;
	  print FILE;
	  print FILE "\n";
	}
	close( FILE );

	if ( $write_data ) {
	  foreach my $data ( @{$self -> {'datas'}} ) {
	    $data -> _write;
	  }
	}

	if( $self->iofv_modules ) {
	  $self->iofv_modules->[0]->post_process;
	}

      }
# line 5028 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _write');
	# End of Non-Dia code #

}

sub is_option_set {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'name' => 'SCALAR', 'record' => 'SCALAR', 'problem_number' => 'SCALAR',
			'record_number' => 'SCALAR', 'fuzzy_match' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->is_option_set: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->is_option_set: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->is_option_set: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->is_option_set: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->is_option_set: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $name = $parm{'name'};
	my $record = $parm{'record'};
	my $problem_number = defined $parm{'problem_number'} ? $parm{'problem_number'} : 1;
	my $record_number = defined $parm{'record_number'} ? $parm{'record_number'} : 0;
	my $found = 0;
	my $fuzzy_match = defined $parm{'fuzzy_match'} ? $parm{'fuzzy_match'} : 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> is_option_set');
# line 2015 "lib/model_subs.pm" 
      {
	# Usage:
	# 
	# if( $modelObject -> is_option_set( record => 'recordName', name => 'optionName' ) ){
	#     print "problem_number 1 has option optionName set in record recordName";
	# }
	#
	# is_option_set checks if an option is set in a given record in given problem.
	#if record_number is 0 it means 'all', this is the default. -1 means last

	my ( @problems, @records, @options );
	my @record_numbers;
	my $accessor = $record.'s';
	if ( defined $self->problems ) {
	    @problems = @{$self->problems};
	} else {
	  croak("No problems defined in model" );
	}
	unless( defined $problems[$problem_number - 1] ){
	    carp("model -> is_option_set: No problem number $problem_number defined in model" );
	    return 0; # No option can be set if no problem exists.
	}
	
	if ( defined $problems[$problem_number - 1] -> $accessor ) {
	    @records = @{$problems[$problem_number - 1] -> $accessor};
	} else {
	    carp("model -> is_option_set: No record $record defined" .
			   " in problem number $problem_number." );
	    return 0;
	}


	if ($record_number > 0){
	  push(@record_numbers,$record_number);
	} elsif ($record_number == 0) {
	  #all record_numbers
	  @record_numbers = 1 .. scalar(@records);
	} elsif ($record_number == -1) {
	  #last
	  push(@record_numbers,scalar(@records));
	}else {
	  croak("illegal input record_number $record_number to is_option_set1");
	}
	
	foreach my $inst (@record_numbers){
	  unless(defined $records[$inst - 1] ){
	    carp("model -> is_option_set: No record number $inst defined in model." );
	    next;
	  }
	  if ( defined $records[$inst - 1] -> options ) {
	    @options = @{$records[$inst - 1] -> options};
	  } else {
	    carp("No option defined in record: $record in problem number $problem_number." );
	    next;
	  }
	  foreach my $option ( @options ) {
	    if ( defined $option and $option -> name eq $name ){
	      $found = 1 ;
	    }elsif( $fuzzy_match ){
	      if( index( $name, $option -> name ) == 0 ){
		$found = 1;
	      }
	    }
	    last if ($found);
	  }
	  last if ($found);
	}
      }
# line 5140 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> is_option_set');
	# End of Non-Dia code #

	return $found;
}

sub is_run {
	my $self = shift;
	my $return_value = 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> is_run');
# line 2090 "lib/model_subs.pm" 
      {
	# Usage:
	# 
	# is_run returns true if the outputobject owned by the
	# modelobject has valid outpudata either in memory or on disc.
	if ( defined $self->outputs ) {
	  if ( @{$self->outputs}[0] -> have_output ){
	    $return_value = 1;
	  }
	} else {
	  $return_value = 0;
	}
      }
# line 5167 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> is_run');
	# End of Non-Dia code #

	return $return_value;
}

sub indexes {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'parameter_type' => 'SCALAR', 'parameter_numbers' => 'ARRAY',
			'with_priors' => 'SCALAR', 'problem_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->indexes: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->indexes: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->indexes: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->indexes: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->indexes: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $parameter_type = $parm{'parameter_type'};
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my $with_priors = defined $parm{'with_priors'} ? $parm{'with_priors'} : 0;
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @indexes = ();

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> indexes');
# line 1931 "lib/model_subs.pm" 
      {
	# Usage:
	#
	#   @indexArray = @{$modelObject -> indexes( 'parameter_type' => 'omega' )};
	# 
	# A call to I<indexes> returns the indexes of all parameters
	# specified in I<parameter_numbers> from the subproblems
	# specified in I<problem_numbers>. The method returns a reference to an array that has
	# the same structure as parameter_numbers but for each
	# array of numbers is instead an array of indices. The method
	# uses a method from the model::problem class to format the
	# indices, so here are a few lines from the code comments in
	# model/problem.pm that describes the returned value:
	#
	# New: returns name and index with new format
	# THETA1, THETA2, THETA3...
	# OMEGA(1,1) OMEGA (1,2)...
	# SIGMA (1,1) SIGMA (2,2)...
        # <snip> indexes_old
	# The Indexes method calculates the index for a
	# parameter. Off-diagonal elements will get a index 'i_j', where i
	# is the row number and j is the column number
	# </snip>

	unless( scalar(@problem_numbers) > 0 ){
		$self->problems([]) unless defined $self->problems;
	  @problem_numbers = (1 .. $#{$self->problems}+1);
	}
	my @problems = @{$self->problems};
        foreach my $i ( @problem_numbers ) {
	  if ( defined $problems[ $i-1 ] ) {
	    push( @indexes,
		  $problems[ $i-1 ] ->
		  indexes( parameter_type => $parameter_type,
			   parameter_numbers => $parameter_numbers[ $i-1 ],
			   with_priors => $with_priors) );
	  } else {
	    croak("Problem number $i does not exist!" );
	  }
	}
      }
# line 5252 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> indexes');
	# End of Non-Dia code #

	return \@indexes;
}

sub _option_val_pos {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'name' => 'SCALAR', 'record_name' => 'SCALAR',
			'problem_numbers' => 'ARRAY', 'instance_numbers' => 'ARRAY',
			'exact_match' => 'SCALAR', 'new_values' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->_option_val_pos: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->_option_val_pos: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->_option_val_pos: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->_option_val_pos: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->_option_val_pos: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $name = $parm{'name'};
	my $record_name = $parm{'record_name'};
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @instance_numbers = defined $parm{'instance_numbers'} ? @{$parm{'instance_numbers'}} : ();
	my $exact_match = defined $parm{'exact_match'} ? $parm{'exact_match'} : 1;
	my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
	my @values;
	my @positions;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _option_val_pos');
# line 4925 "lib/model_subs.pm" 
      {
	unless( scalar(@problem_numbers)>0 ){
	$self->problems([]) unless defined $self->problems;
	  @problem_numbers = (1 .. $#{$self->problems}+1);
	}
	my @problems = @{$self->problems};
	if ( $#new_values >= 0 ) {
	  croak("Trying to set option $name in record $record_name but the ".
			  "number of new value sets (".
			  ($#new_values+1).
			  "), do not match the number of problems specified (".
			  ($#problem_numbers+1).")" )
	      unless(($#new_values == $#problem_numbers) );
	  if ( $#instance_numbers > 0 ) {
	      croak("The number of instance number sets (".
			      ($#instance_numbers+1).
			      "),do not match the number of problems specified (".
			      ($#problem_numbers+1).")" )
		unless(($#instance_numbers == $#problem_numbers) );
	  }
	}

        foreach my $i ( @problem_numbers ) {
	  if ( defined $problems[ $i-1 ] ) {
	    my $rn_ref = $#instance_numbers >= 0 ? \@{$instance_numbers[ $i-1 ]} : [];
	    if ( scalar @new_values > 0) {
	      # {{{ Update values

	      if( not defined $new_values[ $i-1 ] ) {
		croak(" The specified new_values was undefined for problem $i" );
	      }

	      if( not ref( $new_values[ $i-1 ] ) eq 'ARRAY' ) {
		croak(" The specified new_values for problem $i is not an array as it should be but a ".
			      ( defined ref( $new_values[ $i-1 ] ) ?
				ref( $new_values[ $i-1 ] ) : 'undef' ) );
	      }
  
	      $problems[ $i-1 ] ->
		_option_val_pos( record_name      => $record_name,
				 instance_numbers => $rn_ref,
				 new_values       => \@{$new_values[ $i-1 ]},
				 name             => $name,
				 exact_match      => $exact_match );

	      # }}} Update values
	    } else {
	      # {{{ Retrieve values
	      my ( $val_ref, $pos_ref ) =
		$problems[ $i-1 ] ->
		  _option_val_pos( record_name      => $record_name,
				   instance_numbers => $rn_ref,
				   name             => $name,
				   exact_match      => $exact_match );
	      push( @values, $val_ref );
	      push( @positions, $pos_ref );
	      # }}} Retrieve values
	    }
	  } else {
	    croak("Problem number $i does not exist!" );
	  }
	}
      }
# line 5363 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _option_val_pos');
	# End of Non-Dia code #

	return \@values ,\@positions;
}

sub name_val {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_numbers' => 'ARRAY', 'parameter_type' => 'SCALAR',
			'parameter_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->name_val: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->name_val: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->name_val: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->name_val: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->name_val: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @names_values;
	my $parameter_type = $parm{'parameter_type'};
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@names_values;
}

sub input_files {
	my $self = shift;
	my @file_names;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> input_files');
# line 1420 "lib/model_subs.pm" 
{

  # TODO: Skip the dataset for now, when I [PP] rewrite the
  # "model::copy" routine, I will revisit this.

  if( 0 ){
    foreach my $data ( @{$self -> datas} ) {
      my $filename = $data -> filename;
      
    }
  }
  
  # msfi files
  if( scalar @{$self -> msfi_names()} > 0 ){
    foreach my $msfi_files( @{$self -> msfi_names()} ){
      foreach my $msfi_file( @{$msfi_files} ){
	my ( $dir, $filename ) = OSspecific::absolute_path($self -> directory,
							   $msfi_file );
	push( @file_names, [$dir, $filename] );
      }
    }
  } else {

    # If we don't have $MSFI we can consider $EST MSFO as input.

    foreach my $msfo_files( @{$self -> msfo_names()} ){
      foreach my $msfo_file( @{$msfo_files} ){
	my ( $dir, $filename ) = OSspecific::absolute_path($self -> directory,
							   $msfo_file );
	push( @file_names, [$dir, $filename] );
      }
    }
  }

  # TODO: as with data files, revisit this when model::copy is
  # rewritten.

  # Copy extra fortran files specified in "$SUBROUTINE"

  if( defined( $self -> subroutine_files ) ){
    foreach my $sub_file ( @{$self -> subroutine_files} ){
      my ( $dir, $filename ) = OSspecific::absolute_path( $self -> directory,
							  $sub_file );
      push( @file_names, [$dir, $filename] );
    }
  }

  # Copy extra files the user specified.

  if( defined $self -> extra_files ){
    foreach my $x_file (@{$self -> extra_files}){
      my ( $dir, $filename ) = OSspecific::absolute_path( $self -> directory,
							  $x_file );
      #add check that file exists
      croak("File $dir$filename listed as ".
		      "extra input to NONMEM, but the file does not exist.")
	  unless (-e $dir.$filename);
      push( @file_names, [$dir, $filename] );
    }
  }  
}
# line 5477 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> input_files');
	# End of Non-Dia code #

	return \@file_names;
}

sub output_files {
	my $self = shift;
	my @file_names;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> output_files');
# line 1514 "lib/model_subs.pm" 
{

  push( @file_names, $self -> outputs -> [0] -> filename );


  if (defined $self -> outputs -> [0] -> filename_root()){
    foreach my $ext (@nm7_extensions){
# copy also raw in this loop
#      next if ($ext eq '.ext');
      push( @file_names, $self -> outputs -> [0] -> filename_root().$ext);
    }
  }
  
  if( defined $self -> table_names ){
    foreach my $table_files( @{$self -> table_names} ){
      foreach my $table_file( @{$table_files} ){
	my ($dir, $filename) = OSspecific::absolute_path( undef,
							  $table_file );
	push( @file_names, $filename );
      }
    }
  }

  if( defined $self -> msfo_names() ){
    foreach my $msfo_files( @{$self -> msfo_names()} ){
      foreach my $msfo_file( @{$msfo_files} ){
	my ( $dir, $filename ) = OSspecific::absolute_path( undef,
							    $msfo_file );
	push( @file_names, $filename );
      }
    }
  }

  if ( defined $self->extra_output ) {
    foreach my $extra_out ( @{$self->extra_output} ){
      push( @file_names, $extra_out );
    }
  }


  my @problems = @{$self -> problems};
  for( my $i = 0; $i <= $#problems; $i++ ) {
    if( $problems[$i-1] -> shrinkage_module -> enabled ) {
      my ( $dir, $eta_filename ) =
	  OSspecific::absolute_path( undef,
				     $problems[$i] -> shrinkage_module -> eta_tablename );
      
      push( @file_names, $eta_filename );
      
      my ( $dir, $wres_filename ) =
	  OSspecific::absolute_path( undef,
				     $problems[$i] -> shrinkage_module -> wres_tablename );
      
      push( @file_names, $wres_filename );
    }
  }

}
# line 5549 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> output_files');
	# End of Non-Dia code #

	return \@file_names;
}

sub factors {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'column' => 'SCALAR', 'column_head' => 'SCALAR',
			'problem_number' => 'SCALAR', 'return_occurences' => 'SCALAR',
			'unique_in_individual' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->factors: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->factors: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->factors: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->factors: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->factors: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $column = $parm{'column'};
	my $column_head = $parm{'column_head'};
	my $problem_number = $parm{'problem_number'};
	my $return_occurences = defined $parm{'return_occurences'} ? $parm{'return_occurences'} : 0;
	my $unique_in_individual = defined $parm{'unique_in_individual'} ? $parm{'unique_in_individual'} : 1;
my %factors;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> factors');
# line 1638 "lib/model_subs.pm" 
      {
	# Calls <I>factors</I> on the data object of a specified
	# problem. See <I>data -> factors</I> for details.
	my $column_number;
	if ( defined $column_head ) {
	  # Check normal data object first
	  my ( $values_ref, $positions_ref ) = $self ->
	    _get_option_val_pos ( problem_numbers => [$problem_number], 
				  name        => $column_head,
				  record_name => 'input',
				  global_position => 1 );
	  $column_number = $positions_ref -> [0];

	  croak("Unknown column \"$column_head\"" )
	      unless ( defined $column_number );
	} else {
	  $column_number = $column;
	}
	if ( defined $column_number) {
	  %factors = %{$self -> {'datas'} -> [$problem_number-1] ->
			 factors( column => $column_number,
				  unique_in_individual => $unique_in_individual,
				  return_occurences => $return_occurences )};
	}
      }
# line 5620 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> factors');
	# End of Non-Dia code #

	return \%factors;
}

sub have_missing_data {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_number' => 'SCALAR', 'column' => 'SCALAR',
			'column_head' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->have_missing_data: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->have_missing_data: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->have_missing_data: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->have_missing_data: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->have_missing_data: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $problem_number = $parm{'problem_number'};
	my $column = $parm{'column'};
	my $column_head = $parm{'column_head'};
	my $return_value;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> have_missing_data');
# line 1770 "lib/model_subs.pm" 
      {
	# Calls <I>have_missing_data</I> on the data object of a specified
	# problem. See <I>data -> have_missing_data</I> for details.
	my $column_number;
	if ( defined $column_head ) {
	  # Check normal data object first
	  my ( $values_ref, $positions_ref ) = $self ->
	    _get_option_val_pos ( problem_numbers => [$problem_number], 
				  name        => $column_head,
				  record_name => 'input',
				  global_position => 1  );
	  $column_number = $positions_ref -> [0];

	  croak("Unknown column \"$column_head\"" )
	      unless ( defined $column_number );
	} else {
	  $column_number = $column;
	}
	if ( defined $column_number) {
	  $return_value = $self -> {'datas'} -> [$problem_number-1] ->
	    have_missing_data( column => $column_number );
	}
    }
# line 5686 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> have_missing_data');
	# End of Non-Dia code #

	return $return_value;
}

sub median {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_number' => 'SCALAR', 'column_head' => 'SCALAR',
			'column' => 'SCALAR', 'unique_in_individual' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->median: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->median: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->median: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->median: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->median: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $problem_number = $parm{'problem_number'};
	my $column_head = $parm{'column_head'};
	my $column = $parm{'column'};
	my $unique_in_individual = $parm{'unique_in_individual'};
	my $median;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> median');
# line 3182 "lib/model_subs.pm" 
      {
	# Calls <I>median</I> on the data object of a specified
	# problem. See <I>data -> median</I> for details.
	my $column_number;
	if ( defined $column_head ) {
	  # Check normal data object first
	  my ( $values_ref, $positions_ref ) = $self ->
	    _get_option_val_pos ( problem_numbers => [$problem_number], 
				  name        => $column_head,
				  record_name => 'input',
				  global_position => 1  );
	  $column_number = $positions_ref -> [0];
	  
	  croak("Unknown column \"$column_head\"" )
	      unless ( defined $column_number );
	} else {
	  $column_number = $column;
	}

	if ( defined $column_number) {
	  $median = $self -> {'datas'} -> [$problem_number-1] ->
	    median( column => $column_number,
		    unique_in_individual => $unique_in_individual );
	}
    }
# line 5755 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> median');
	# End of Non-Dia code #

	return $median;
}

sub mean {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_number' => 'SCALAR', 'column_head' => 'SCALAR',
			'column' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->mean: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->mean: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->mean: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->mean: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->mean: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $problem_number = $parm{'problem_number'};
	my $column_head = $parm{'column_head'};
	my $column = $parm{'column'};
	my $mean;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> mean');
# line 3215 "lib/model_subs.pm" 
      {
	# Calls <I>mean</I> on the data object of a specified
	# problem. See <I>data -> mean</I> for details.
	my $column_number;
	if ( defined $column_head ) {
	  # Check normal data object first
	  my ( $values_ref, $positions_ref ) = $self ->
	    _get_option_val_pos ( problem_numbers => [$problem_number], 
				  name        => $column_head,
				  record_name => 'input',
				  global_position => 1  );
	  $column_number = $positions_ref -> [0];

	  croak("Unknown column \"$column_head\"" )
	      unless ( defined $column_number );
	} else {
	  $column_number = $column;
	}

	if ( defined $column_number) {
	  $mean = $self -> {'datas'} -> [$problem_number-1] ->
	      mean( column => $column_number);
	}
    }
# line 5822 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> mean');
	# End of Non-Dia code #

	return $mean;
}

sub max {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_number' => 'SCALAR', 'column' => 'SCALAR',
			'column_head' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->max: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->max: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->max: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->max: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->max: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $problem_number = $parm{'problem_number'};
	my $column = $parm{'column'};
	my $column_head = $parm{'column_head'};
	my $max;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> max');
# line 3247 "lib/model_subs.pm" 
      {
	# Calls <I>max</I> on the data object of a specified
	# problem. See <I>data -> max</I> for details.
	my $column_number;
	if ( defined $column_head ) {
	  # Check normal data object first
	  my ( $values_ref, $positions_ref ) = $self ->
	    _get_option_val_pos ( problem_numbers => [$problem_number], 
				  name        => $column_head,
				  record_name => 'input',
				  global_position => 1  );
	  $column_number = $positions_ref -> [0];

	  croak("Unknown column \"$column_head\"" )
	      unless ( defined $column_number );
	} else {
	  $column_number = $column;
	}

	if ( defined $column_number) {
	  $max = $self -> {'datas'} -> [$problem_number-1] ->
	    max( column => $column_number );
	}
      }
# line 5889 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> max');
	# End of Non-Dia code #

	return $max;
}

sub min {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_number' => 'SCALAR', 'column' => 'SCALAR',
			'column_head' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->min: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->min: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->min: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->min: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->min: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $problem_number = $parm{'problem_number'};
	my $column = $parm{'column'};
	my $column_head = $parm{'column_head'};
	my $min;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> min');
# line 3278 "lib/model_subs.pm" 
      {
	# Calls <I>min</I> on the data object of a specified
	# problem. See <I>data -> min</I> for details.
	my $column_number;
	if ( defined $column_head ) {
	  # Check normal data object first
	  my ( $values_ref, $positions_ref ) = $self ->
	    _get_option_val_pos ( problem_numbers => [$problem_number], 
				  name        => $column_head,
				  record_name => 'input',
				  global_position => 1  );
	  $column_number = $positions_ref -> [0];

	  croak("Unknown column \"$column_head\"" )
	      unless ( defined $column_number );
	} else {
	  $column_number = $column;
	}

	if ( defined $column_number) {
	  $min = $self -> {'datas'} -> [$problem_number-1] ->
	    min( column => $column_number );
	}
    }
# line 5956 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> min');
	# End of Non-Dia code #

	return $min;
}

sub remove_inits {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'type' => 'SCALAR', 'labels' => 'ARRAY', 'indexes' => 'ARRAY',
			'problem_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->remove_inits: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->remove_inits: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->remove_inits: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->remove_inits: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->remove_inits: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $type = $parm{'type'};
	my @labels = defined $parm{'labels'} ? @{$parm{'labels'}} : ();
	my @indexes = defined $parm{'indexes'} ? @{$parm{'indexes'}} : ();
	my $problem_number = defined $parm{'problem_number'} ? $parm{'problem_number'} : 1;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> remove_inits');
# line 3581 "lib/model_subs.pm" 
    {
      # Usage
      # 
      # $model -> remove_inits( type => 'theta',
      #                         indexes => [1,2,5,6] )
      #

      # In all cases the type must be set to theta. Removing Omegas and
      # Sigmas is not allowed, (If need that feature, send us a
      # mail). In the above example the thetas 1, 2, 5 and 6 will be
      # removed from the modelfile. Notice that this alters the theta
      # numbering, so if you later decide that theta number 7 must be
      # removed as well, you must calculate its new position in the
      # file. In this case the new number would be 3. Also notice that
      # numbering starts with 1.
      #
      # $model -> remove_inits( type => 'theta',
      #                         labels => ['V', 'CL'] )
      #

      # If you have specified labels in you modelfiles(a label is
      # string inside a comment on the same row as the theta) you can
      # specify an array with labels, and the corresponding theta, if
      # it exists, will be removed. This is a much better approach
      # since you don't need to know where in order the theta you wish
      # to remove appears. If you specify both labels and indexes, the
      # indexes will be ignored.

      croak('does not have the functionality for removing $OMEGA or $SIGMA options yet' )
	  if ( $type eq 'omega' or $type eq 'sigma' );
      my $accessor = $type.'s';

      # First pick out a referens to the theta records array.
      my $records_ref = $self -> problems -> [$problem_number -1] -> $accessor;
      
      # If we have any thetas at all:
      if ( defined $records_ref ) {
	my @records_array = @{$records_ref};

	# If labels are specified, we translate the labels into
	# indexes.
	if ( scalar @labels > 0 ) {
	  @indexes = ();
	  my $i = 1;
	  # Loop over theta records
	  foreach my $init ( @records_array ) {
	    # Loop over the individual thetas inside
	    foreach my $option ( @{$init -> options} ) {
	      # Loop over all given labels.
	      foreach my $label ( @labels ) {
		# Push the index number if a given label match the
		# theta label
		push( @indexes, $i ) if ( $option -> label eq $label);
	      }
	      # $i is the count of thetas so far
	      $i++;
	    }
	  }
	}

	# We don't really remove thetas, we do a loop over all thetas
	# and recording which we like to keep. We do that by selecting
	# an index, from @indexes, that shall be removed and loop over
	# the thetas, all thetas that doesn't match the index are
	# stored in @keep_options. When we find a theta that matches,
	# we pick a new index and continue the loop. So by makeing
	# sure that @indexes is sorted, we only need to loop over the
	# thetas once.

	@indexes = sort {$a <=> $b} @indexes; #sort input

	my $index = 0;
	my $nr_options = 1;
	my @keep_records;
	
	# Loop over all records
        RECORD_LOOP: foreach my $record ( @records_array ){
	  my @keep_options = ();
	  # Loop over all thetas
	  foreach my $option ( @{$record -> options} ) {
	    if( $indexes[ $index ] == $nr_options ){
	      # If a theta matches an index, we take the next index
	      # and forget the theta.
	      unless( $index > $#indexes ){
		$index++;
	      }
	    } else {
	      # Otherwise we rember it.
	      push(@keep_options,$option);
	    }
	    $nr_options++;
	  }
	  if( scalar(@keep_options) > 0 ){
	    # If we remember some thetas, we must also remember the
	    # record which they are in.
	    $record -> options( \@keep_options );
	    push( @keep_records, $record );
	  }
	}
	
	# Set the all kept thetas back into the modelobject.
	@{$records_ref} = @keep_records;

      } else {
	croak("No init of type $type defined" );
      }
    }
# line 6106 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> remove_inits');
	# End of Non-Dia code #

}

sub fractions {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'column' => 'SCALAR', 'column_head' => 'SCALAR',
			'problem_number' => 'SCALAR', 'unique_in_individual' => 'SCALAR',
			'ignore_missing' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->fractions: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->fractions: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->fractions: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->fractions: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->fractions: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $column = $parm{'column'};
	my $column_head = $parm{'column_head'};
	my $problem_number = $parm{'problem_number'};
	my $unique_in_individual = defined $parm{'unique_in_individual'} ? $parm{'unique_in_individual'} : 1;
my %fractions;
	my $ignore_missing = $parm{'ignore_missing'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> fractions');
# line 1714 "lib/model_subs.pm" 
      {
	# Calls <I>fractions</I> on the data object of a specified
	# problem. See <I>data -> fractions</I> for details.
	my $column_number;
	if ( defined $column_head ) {
	  # Check normal data object first
	  my ( $values_ref, $positions_ref ) = $self ->
	    _get_option_val_pos ( problem_numbers => [$problem_number], 
				  name        => $column_head,
				  record_name => 'input',
				  global_position => 1 );
	  $column_number = $positions_ref -> [0];

	  croak("Unknown column \"$column_head\"")
	      unless ( defined $column_number );
	} else {
	  $column_number = $column;
	}
	if ( defined $column_number) {
	  %fractions = %{$self -> {'datas'} -> [$problem_number-1] ->
			   fractions( column => $column_number,
				      unique_in_individual => $unique_in_individual,
				      ignore_missing       => $ignore_missing )};
	}
      }
# line 6176 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> fractions');
	# End of Non-Dia code #

	return \%fractions;
}

sub remove_records {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'type' => 'm_SCALAR', 'keep_last' => 'SCALAR',
			'problem_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->remove_records: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->remove_records: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->remove_records: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->remove_records: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->remove_records: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $type = $parm{'type'};
	my $keep_last = defined $parm{'keep_last'} ? $parm{'keep_last'} : 0;
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> remove_records');
# line 797 "lib/model_subs.pm" 
      {
	unless( scalar(@problem_numbers)>0 ){
		$self->problems([]) unless defined $self->problems;
	  @problem_numbers = (1 .. $#{$self->problems}+1);
	}

	my @problems;
	@problems = @{$self -> problems()} if (defined $self ->problems());
	foreach my $i ( @problem_numbers ) {
	  if ( defined $problems[ $i-1 ] ) {
	    $problems[$i-1] -> remove_records( 'type' => $type, keep_last => $keep_last );
	  } else {
	    croak("Problem number $i, does not exist" );
	  } 
	}
# else {
#	  croak("No Problems in model object." );
#	}
      }
# line 6237 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> remove_records');
	# End of Non-Dia code #

}

sub table_files {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->table_files: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->table_files: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->table_files: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->table_files: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->table_files: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @table_files;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> table_files');
# line 4013 "lib/model_subs.pm" 
      {
	# Usage:
	#
	#    @table_files = @{$modobj -> table_files};
	#
	# This basic usage takes no arguments and returns the table
	# files objects for all problems.  @table_files will be a
	# two dimensional array:
	#
	#   [[table_file_object_prob1][table_file_object_prob2]...]
	#
	#
	# To retrieve the table file objects from specific problems,
	# the I<problem_numbers> argument can be used. It should be
	# a reference to an array containing the numbers of all
	# problems from which the table file objects should be
	# retrieved.

	unless(scalar(@problem_numbers)>0 ){
		$self->problems([]) unless defined $self->problems;
	  @problem_numbers = (1 .. $#{$self->problems}+1);
	}
	my @problems = @{$self->problems};
        foreach my $i ( @problem_numbers ) {
	  if ( defined $problems[ $i-1 ] ) {
	    push( @table_files, $problems[$i-1] -> table_files );
	  } else {
	    croak("Problem number $i does not exist!" );
	  }
	}
      }
# line 6307 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> table_files');
	# End of Non-Dia code #

	return \@table_files;
}

sub full_name {
	my $self = shift;
	my $full_name;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> full_name');
# line 583 "lib/model_subs.pm" 
    {
	$full_name =  $self->directory . $self -> {'filename'};
    }
# line 6324 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> full_name');
	# End of Non-Dia code #

	return $full_name;
}

sub is_estimation {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->is_estimation: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->is_estimation: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->is_estimation: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->is_estimation: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->is_estimation: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $problem_number = defined $parm{'problem_number'} ? $parm{'problem_number'} : 0;
	my $is_est = 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> is_estimation');
# line 2109 "lib/model_subs.pm" 
    {
      #this function is used to check whether we should care about minimization status 
      #for possible retries. We only care of we are doing a real estimation
      
      $is_est = 1;
      my $problems = $self->problems;
      if( defined $problems -> [$problem_number - 1] ) {
	my $problem = $problems -> [$problem_number - 1];
	# If we don't have an ESTIMATION record we are simulating.
	$is_est = 0 unless( defined $problem->estimations and
			    scalar( @{$problem->estimations} ) > 0 );
	
	# If we have a ONLYSIM option in the simulation record.
	$is_est = 0 if( $self -> is_option_set ( name           => 'ONLYSIM', 
						 record         => 'simulation', 
						 problem_number => $problem_number ));

	if ($PsN::nm_major_version == 7){
	  # If single estimation step and max evaluations is zero we are not estimating
	  if ( defined $problem->estimations and
	       scalar( @{$problem->estimations} ) == 1 ){
	    my $max = $self -> get_option_value(record_name => 'estimation', option_name => 'MAXEVALS',
						problem_index => 0, record_index => 0,option_index => 0);
	    
	    $is_est = 0 if (defined $max and $max == 0);
	    my $eonly = $self -> get_option_value(record_name => 'estimation', option_name => 'EONLY',
						problem_index => 0, record_index => 0,option_index => 0);
	    
	    $is_est = 0 if (defined $eonly and $eonly == 1);
	  }
	} else {
	  $is_est = 0 if( defined $self -> maxeval(problem_numbers => [$problem_number]) and
			  defined $self -> maxeval(problem_numbers => [$problem_number])->[0][0] and
			  $self -> maxeval(problem_numbers => [$problem_number])->[0][0] == 0 );
	}
	# Anything else?

	# If non of the above is true, we are estimating.
      } else {
	carp('Problem nr. $problem_number not defined. Assuming estimation' );
      }
    }
# line 6406 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> is_estimation');
	# End of Non-Dia code #

	return $is_est;
}

sub subroutine_files {
	my $self = shift;
	my @fsubs;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> subroutine_files');
# line 4995 "lib/model_subs.pm" 
{
  my %fsubs;
  foreach my $subr( 'PRED','CRIT', 'CONTR', 'CCONTR', 'MIX', 'CONPAR', 'OTHER', 'PRIOR', 'INFN' ){
    my ( $model_fsubs, $junk ) = $self -> _option_val_pos( record_name => 'subroutine',
							   name => $subr );
    if( @{$model_fsubs} > 0 ){
      foreach my $prob_fsubs ( @{$model_fsubs} ){
	foreach my $fsub( @{$prob_fsubs} ){
	  $fsubs{$fsub} = 1;
	}
      }
    }
  }

  # BUG , nonmem6 might not require the file to be named .f And I've
  # seen examples of files named .txt

  @fsubs = keys %fsubs;
  if( @fsubs > 0  ){
    for( my $i = 0; $i <= $#fsubs; $i ++ ){
      unless( $fsubs[$i] =~ /\.f$/ ){
	$fsubs[$i] .= '.f';
      }
    }
  }
}
# line 6446 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> subroutine_files');
	# End of Non-Dia code #

	return \@fsubs;
}

sub randomize_inits {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'degree' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->randomize_inits: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->randomize_inits: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->randomize_inits: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->randomize_inits: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->randomize_inits: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $degree = $parm{'degree'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> randomize_inits');
# line 3536 "lib/model_subs.pm" 
      {
	foreach my $prob ( @{$self->problems} ) {
	  $prob -> set_random_inits ( degree => $degree );
	}
      }
# line 6490 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> randomize_inits');
	# End of Non-Dia code #

}

sub flush_data {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> flush_data');
# line 556 "lib/model_subs.pm" 
      {
	if ( defined $self -> {'datas'} ) {
	  foreach my $data ( @{$self -> {'datas'}} ) {
	    $data -> flush;
	  }
	}
      }
# line 6509 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> flush_data');
	# End of Non-Dia code #

}

sub register_in_database {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'force' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->register_in_database: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->register_in_database: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->register_in_database: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->register_in_database: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->register_in_database: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $force = defined $parm{'force'} ? $parm{'force'} : 0;
	my $model_id;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $model_id;
}

sub remove_option {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_numbers' => 'ARRAY', 'record_name' => 'SCALAR',
			'record_number' => 'SCALAR', 'option_name' => 'SCALAR',
			'fuzzy_match' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->remove_option: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->remove_option: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->remove_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->remove_option: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->remove_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my $record_name = $parm{'record_name'};
	my $record_number = defined $parm{'record_number'} ? $parm{'record_number'} : 0;
	my $option_name = $parm{'option_name'};
	my $fuzzy_match = defined $parm{'fuzzy_match'} ? $parm{'fuzzy_match'} : 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> remove_option');
# line 4902 "lib/model_subs.pm" 

unless( scalar(@problem_numbers)>0 ){
	$self->problems([]) unless defined $self->problems;
  @problem_numbers = (1 .. $#{$self->problems}+1);
}

my @problems = @{$self->problems};
foreach my $i ( @problem_numbers ) {
  if ( defined $problems[ $i-1 ] ) {
    $problems[$i-1] -> remove_option( record_name => $record_name,
				      record_number => $record_number,
				      option_name => $option_name,
				      fuzzy_match => $fuzzy_match);
  }
}

# line 6605 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> remove_option');
	# End of Non-Dia code #

}

sub add_option {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_numbers' => 'ARRAY', 'record_number' => 'SCALAR',
			'record_name' => 'SCALAR', 'option_name' => 'SCALAR',
			'option_value' => 'SCALAR', 'add_record' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->add_option: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->add_option: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->add_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->add_option: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->add_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my $record_number = defined $parm{'record_number'} ? $parm{'record_number'} : 0;
	my $record_name = $parm{'record_name'};
	my $option_name = $parm{'option_name'};
	my $option_value = $parm{'option_value'};
	my $add_record = defined $parm{'add_record'} ? $parm{'add_record'} : 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> add_option');
# line 4878 "lib/model_subs.pm" 

unless( scalar(@problem_numbers)>0 ){
	$self->problems([]) unless defined $self->problems;
  @problem_numbers = (1 .. $#{$self->problems}+1);
}

my @problems = @{$self->problems};
foreach my $i ( @problem_numbers ) {
  if ( defined $problems[ $i-1 ] ) {
    $problems[$i-1] -> add_option( record_name  => $record_name,
				   record_number => $record_number,
				   option_name  => $option_name,
				   option_value => $option_value,
				   add_record   => $add_record );
  }
}

# line 6667 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> add_option');
	# End of Non-Dia code #

}

sub set_option {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_numbers' => 'ARRAY', 'record_name' => 'SCALAR',
			'record_number' => 'SCALAR', 'option_name' => 'SCALAR',
			'option_value' => 'SCALAR', 'fuzzy_match' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->set_option: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->set_option: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->set_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->set_option: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->set_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my $record_name = $parm{'record_name'};
	my $record_number = defined $parm{'record_number'} ? $parm{'record_number'} : 0;
	my $option_name = $parm{'option_name'};
	my $option_value = $parm{'option_value'};
	my $fuzzy_match = defined $parm{'fuzzy_match'} ? $parm{'fuzzy_match'} : 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> set_option');
# line 4846 "lib/model_subs.pm" 
#record is not added if not already there. Could set option add_record in add_option
unless( scalar(@problem_numbers)>0 ){
	$self->problems([]) unless defined $self->problems;
  @problem_numbers = (1 .. $#{$self->problems}+1);
}

my @problems = @{$self->problems};
foreach my $i ( @problem_numbers ) {
  if ( defined $problems[ $i-1 ] ) {
    my $found = $self -> is_option_set( 'problem_number' => $i,
					'record'         => $record_name,
					'record_number'  => $record_number,
					'name'           => $option_name,
					'fuzzy_match'    => $fuzzy_match );
    $problems[$i-1] -> remove_option( record_name  => $record_name,
				      record_number  => $record_number,
				      option_name  => $option_name,
				      fuzzy_match  => $fuzzy_match ) if ( $found );
    $problems[$i-1] -> add_option( record_name  => $record_name,
				   record_number  => $record_number,
				   option_name  => $option_name,
				   option_value => $option_value );
  }
}

# line 6737 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> set_option');
	# End of Non-Dia code #

}

sub add_marginals_code {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'problem_numbers' => 'ARRAY', 'nomegas' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->add_marginals_code: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->add_marginals_code: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->add_marginals_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->add_marginals_code: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->add_marginals_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @nomegas = defined $parm{'nomegas'} ? @{$parm{'nomegas'}} : ();

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> add_marginals_code');
# line 594 "lib/model_subs.pm" 

# add_marginals_code takes two arguments.
#
# - problem_numbers is an array holding the numbers of the problems in
# which code should be added.
#
# - nomegas which is an array holding the number of (diagonal-element)
# omegas of each problem given by problem_numbers.
#
# For each omega in each problem, verbatim code is added to make the
# marginals available for printing (e.g. to a table file). COM(1) will
# hold the nonparametric density, COM(2) the marginal cumulative value
# for the first eta, COM(2) the marginal cumulative density for the
# second eta and so on.

unless( scalar(@problem_numbers)>0 ){
	$self->problems([]) unless defined $self->problems;
  @problem_numbers = (1 .. $#{$self->problems}+1);
}

my @problems = @{$self->problems};
my $j = 0;
foreach my $i ( @problem_numbers ) {
  if ( defined $problems[ $i-1 ] ) {
    $problems[$i-1] -> add_marginals_code( nomegas => $nomegas[ $j ]  );
  } else {
    croak("Problem number $i does not exist.");
  } 
  $j++;
}

# line 6807 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> add_marginals_code');
	# End of Non-Dia code #

}

sub problem_structure {
	my $self = shift;
	my @subproblems;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> problem_structure');
# line 3510 "lib/model_subs.pm" 

my ( $val, $pos ) = $self -> _option_val_pos( record_name => 'simulation',
					      name        => 'SUBPROBLEMS' );
if( defined $val ) {
  my @vals = @{$val};
  for( my $i = 0; $i <= $#vals; $i++ ) {
    if( defined $vals[$i] ) {
      if( scalar @{$vals[$i]} > 0 ) {
	$subproblems[$i] = $vals[$i][0];
      } else {
	$subproblems[$i] = 1;
      }
    } else {
      $subproblems[$i] = 1;
    }
  }
}

# line 6838 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> problem_structure');
	# End of Non-Dia code #

	return \@subproblems;
}

sub add_nonparametric_code {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> add_nonparametric_code');
# line 485 "lib/model_subs.pm" 

$self -> set_records( type           => 'nonparametric',
		       record_strings => [ 'MARGINALS UNCONDITIONAL' ] );
$self -> set_option( record_name => 'estimation',
		      option_name => 'POSTHOC' );
my ( $msfo_ref, $junk ) = $self ->
    _get_option_val_pos( name            => 'MSFO',
			 record_name     => 'estimation' );
my @nomegas = @{$self -> nomegas};

for( my $i = 0; $i <= $#nomegas; $i++ ) { # loop the problems
  my $marg_str = 'ID';
  for( my $j = 0; $j <= $nomegas[$i]; $j++ ) {
    $marg_str = $marg_str.' COM('.($j+1).')=MG'.($j+1);
  }
  $marg_str = $marg_str.' FILE='.$self->filename.'.marginals'.
      ' NOAPPEND ONEHEADER NOPRINT';
  $self -> add_records( problem_numbers => [($i+1)],
			 type            => 'table',
			 record_strings  => [ $marg_str ] );
  $self -> remove_option( record_name => 'abbreviated',
			   option_name => 'COMRES' );
  $self -> add_option( record_name  => 'abbreviated',
		       option_name  => 'COMRES',
		       option_value => ($nomegas[$i]+1),
		       add_record   => 1 );  #Add $ABB if not existing

  $self -> add_marginals_code( problem_numbers => [($i+1)],
			       nomegas         => [ $nomegas[$i] ] );
}

if( not defined $msfo_ref ) {
  for( my $i = 0; $i < $self -> nproblems; $i++ ) {
    $self -> add_option( record_name =>  'estimation',
			  option_name =>  'MSFO',
			  option_value => $self -> filename.'.msfo'.($i+1) );
  }
} else {
  for( my $i = 0; $i < scalar @{$msfo_ref}; $i++ ) {
    if( not defined $msfo_ref->[$i] or not defined $msfo_ref->[$i][0] ) {
      $self -> add_option( record_name =>  'estimation',
			    option_name =>  'MSFO',
			    option_value => $self -> filename.'.msfo'.($i+1) );
    }
  }
}

# line 6898 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> add_nonparametric_code');
	# End of Non-Dia code #

}

sub nonparametric_code {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'enabled' => 'ARRAY', 'problem_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->nonparametric_code: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->nonparametric_code: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->nonparametric_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->nonparametric_code: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->nonparametric_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @enabled = defined $parm{'enabled'} ? @{$parm{'enabled'}} : ();
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @indicators;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> nonparametric_code');
# line 451 "lib/model_subs.pm" 

if ( $#problem_numbers > 0 and $#enabled > 0 ){
  if ( $#problem_numbers != $#enabled ) {
    croak("The number of problem_numbers ".($#problem_numbers+1).
		    "and enabled/disabled nonparametric_code ".($#enabled+1).
		    "do not match" );
  }
}
unless( scalar(@problem_numbers) > 0 ){
	$self->problems([]) unless defined $self->problems;
  @problem_numbers = (1 .. $#{$self->problems}+1);
}
my @problems = @{$self->problems};
my $j = 0;
foreach my $i ( @problem_numbers ) {
  if ( defined $problems[ $i-1 ] ) {
    if ( defined $enabled[ $j ] ) {
      $problems[ $i-1 ] -> nonparametric_code( $enabled[ $j ] );
    } else {
      push( @indicators, $problems[ $i-1 ] -> nonparametric_code );
    }
  } else {
    croak("Problem number $i does not exist!" );
  }
  $j++;
}	

# line 6965 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> nonparametric_code');
	# End of Non-Dia code #

	return \@indicators;
}

sub shrinkage_stats {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'enabled' => 'SCALAR', 'problem_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->shrinkage_stats: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->shrinkage_stats: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->shrinkage_stats: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->shrinkage_stats: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->shrinkage_stats: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $enabled = $parm{'enabled'};
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @indicators;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> shrinkage_stats');
# line 293 "lib/model_subs.pm" 

if ( $#problem_numbers > 0 and ref $enabled eq 'ARRAY' ){
  if ( $#problem_numbers != ( scalar @{$enabled} - 1 ) ) {
    croak("The number of problem_numbers ".($#problem_numbers+1).
		    "and enabled/disabled shrinkage_stats ".scalar @{$enabled}.
		    " do not match" );
  }
}
unless( scalar(@problem_numbers) > 0 ) {
	$self->problems([]) unless defined $self->problems;
  @problem_numbers = (1 .. $#{$self->problems}+1);
}
my @en_arr;
if( ref \$enabled eq 'SCALAR' ) {
  for ( @problem_numbers ) {
    push( @en_arr, $enabled );
  }
} elsif ( not ref $enabled eq 'ARRAY' ) {
  croak('enabled must be a scalar or a reference to an array, '.
		'not a reference to a '.ref($enabled).'.' );
}

my @problems = @{$self->problems};
my $j = 0;
foreach my $i ( @problem_numbers ) {
  if ( defined $problems[ $i-1 ] ) {
    if ( defined $en_arr[ $j ] ) {
      if( $en_arr[ $j ] ) {
	$problems[ $i-1 ] -> shrinkage_module -> enable;
      } else {
	$problems[ $i-1 ] -> shrinkage_module -> disable;
      }
    } else {
      push( @indicators, $problems[ $i-1 ] -> shrinkage_module -> status );
    }
  } else {
    croak("Problem number $i does not exist!" );
  }
  $j++;
}	

# line 7047 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> shrinkage_stats');
	# End of Non-Dia code #

	return \@indicators;
}

sub eta_shrinkage {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'eta_filename' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->eta_shrinkage: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->eta_shrinkage: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->eta_shrinkage: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->eta_shrinkage: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->eta_shrinkage: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @eta_shrinkage;
	my $eta_filename = $parm{'eta_filename'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> eta_shrinkage');
# line 424 "lib/model_subs.pm" 

my @problems = @{$self->problems};
my $problem_number = 0;
foreach my $problem ( @problems ) {
  $problem_number++;
  push( @eta_shrinkage, $problem -> eta_shrinkage( model => $self, 
						   probnum => $problem_number,
						   directory => $self -> directory,
						   eta_filename => $eta_filename) );
}

# line 7098 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> eta_shrinkage');
	# End of Non-Dia code #

	return \@eta_shrinkage;
}

sub iwres_shrinkage {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'iwres_filename' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->iwres_shrinkage: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->iwres_shrinkage: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->iwres_shrinkage: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->iwres_shrinkage: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->iwres_shrinkage: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @iwres_shrinkage;
	my $iwres_filename = $parm{'iwres_filename'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> iwres_shrinkage');
# line 388 "lib/model_subs.pm" 

my @problems = @{$self->problems};
my $problem_number = 0;
foreach my $problem ( @problems ) {
  $problem_number++;
  push( @iwres_shrinkage, $problem -> iwres_shrinkage( model => $self, 
						       probnum => $problem_number,
						       directory => $self -> directory,
						       iwres_filename => $iwres_filename) );
}

# line 7149 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> iwres_shrinkage');
	# End of Non-Dia code #

	return \@iwres_shrinkage;
}

sub flush {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'force' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->flush: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->flush: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->flush: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->flush: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->flush: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $force = defined $parm{'force'} ? $parm{'force'} : 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> flush');
# line 3759 "lib/model_subs.pm" 
#FIXME: Remove this method
	# synchronizes the object with the file on disk and empties
	# most of the objects attributes to save memory.
	if( defined $self->problems and ( !$self->synced or $force ) ) {
		$self -> _write;
	}
	$self -> {'problems'} = undef;	#FIXME: Change for Moose
	$self->synced(0);
# line 7196 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> flush');
	# End of Non-Dia code #

}

sub update_prior_information {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->update_prior_information: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->update_prior_information: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->update_prior_information: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->update_prior_information: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->update_prior_information: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> update_prior_information');
# line 440 "lib/model_subs.pm" 
      {
	foreach my $problem (@{$self->problems}) {
	  $problem -> update_prior_information() if (defined $problem);
	}
      }
# line 7238 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> update_prior_information');
	# End of Non-Dia code #

}

sub _read_problems {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_problems');
# line 4711 "lib/model_subs.pm" 
    {
      # To read problems from a modelfile we need its full name
      # (meaning filename and path). And we need an array for the
      # modelfile lines and an array with indexes telling where
      # problems start in the modelfile array.

      
      my $file = $self -> full_name;
      my ( @modelfile, @problems );
      my ( @problem_start_index );
      
      # Check if the file is missing, and if that is ok.
      # TODO Check accessor what happens if the file is missing.

      return if( not (-e $file) && $self->ignore_missing_files );

      # Open the file, slurp it and close it

      if( not (-e $file)){
				croak("The model file ".$self->filename()." in ".
			$self->directory()." does not exist.");
      }else{
	if ($self->d2u()){
	  system("dos2unix -q $file");
	}

	open( FILE, "$file" ) ||
	    croak("Model -> _read_problems: Could not open $file".
			    " for reading" );
	
	@modelfile = <FILE>;
	close( FILE );

      }
      my $start_index = 0;
      my $end_index;
      my $first = 1;
      my $prob_num = 0;
      my $warning_printed=0;
      my $prev_was_not_sizes = 1;

      # It may look like the loop takes one step to much, but its a
      # trick that helps parsing the last problem.
      for ( my $i = 0; $i <= @modelfile; $i++ ) {
	if( $i <= $#modelfile ){
	  $_ = $modelfile[$i];
	}

	if ($first and not (/^\s*(;|\$PROB|$|\$SIZ)/ )){
	  croak('Model -> _read_problems: '.
			  "First non-comment line in modelfile $file \n".
			  'is not a $PROB or $SIZES record. NONMEM syntax violation.');
	}
	
	# In this if statement we use the lazy evaluation of logical
	# or to make sure we only execute search pattern when we have
	# a line to search. Which is all cases but the very last loop
	# iteration.

	if( $i > $#modelfile or (( /^\s*\$PROB/ and $prev_was_not_sizes)or (/^\s*\$SIZ/ ) ) ){
	  $end_index = $i;
	  
	  # The if statement here is only necessary in the first loop
	  # iteration. When start_index == end_index == 0 we want to
	  # skip to the next iteration looking for the actual end of
	  # the first problem.
	  
	  if( $end_index > $start_index and not $first ){
	    # extract lines of code:
	    my @problem_lines = @modelfile[$start_index .. $end_index-1];	    
	    # reset the search for problems by moving the problem start
	    # forwards:
	    $start_index = $i;
	    my $problem_number = scalar @problems + 1;


	    my $sh_mod = model::shrinkage_module -> new ( nomegas => $self -> nomegas -> [0],
							  directory => $self -> directory(),
							  problem_number => $problem_number );

	    my $prob = model::problem ->
		new ( directory                   => $self->directory,
		      ignore_missing_files        => $self->ignore_missing_files,
		      ignore_missing_output_files => $self->ignore_missing_output_files,
		      sde                         => $self->sde,
		      omega_before_pk             => $self->omega_before_pk,
		      cwres                       => $self->cwres,
		      tbs                         => $self->tbs,
		      tbs_param                    => $self->tbs_param,
		      mirror_plots                => $self->mirror_plots,
		      prob_arr                    => \@problem_lines,
		      shrinkage_module            => $sh_mod );
	    if (defined $prob->tbs_thetanum()){
	      $self->tbs_thetanum($prob->tbs_thetanum());
	    }

	    push( @problems, $prob );
	    if ( $self -> cwres() ) {
              my @eo;
	      if ( defined $self -> extra_output() ) {
		@eo = @{$self -> extra_output()};
	      }
	      if ( $prob->cwres_modules ) {
					push( @eo, @{$prob->cwres_modules->[0]->cwtab_names()} );
	      }
	      $self -> extra_output( \@eo );
	    }

	    $prob_num++;
	  }
	  $first = 0;
	}
	if (/^\s*\$/ ){
	  if (/^\s*\$SIZ/ ){
	    $prev_was_not_sizes = 0;
	  }else{
	    $prev_was_not_sizes = 1;
	  }
	}
      }
      
      # Set the problems in the modelobject.
      if (scalar(@problems)<1){
	  croak('Model -> _read_problems: '.
			  "Could not find any problem in modelfile $file");
      }
      $self -> problems(\@problems);
    }
# line 7378 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_problems');
	# End of Non-Dia code #

}

sub _get_option_val_pos {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'name' => 'SCALAR', 'record_name' => 'SCALAR',
			'problem_numbers' => 'ARRAY', 'instances' => 'ARRAY',
			'global_position' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->_get_option_val_pos: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->_get_option_val_pos: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->_get_option_val_pos: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->_get_option_val_pos: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->_get_option_val_pos: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $name = $parm{'name'};
	my $record_name = $parm{'record_name'};
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my @instances = defined $parm{'instances'} ? @{$parm{'instances'}} : ();
	my @values;
	my @positions;
	my $global_position = defined $parm{'global_position'} ? $parm{'global_position'} : 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _get_option_val_pos');
# line 4515 "lib/model_subs.pm" 
      {
	# Usage:
	#
	#   ( $values_ref, $positions_ref ) ->
	#               _get_option_val_pos ( name        => 'ID',
        #                                     record_name => 'input' );
	#   my @values = @{$values_ref};
	#   my @positions = @{$positions_ref};
	#
	# This basic usage returns the name of the third option in the first
	# instance of the record specified by I<record_name> for all problems
	#
	# If global_position is set to 1, only one value and position
	# pair is returned per problem. If there are more than one
	# match in the model; the first will be returned for each
	# problem.
	#
	# Private method, should preferably not be used outside model.pm
	
	my $accessor = $record_name.'s';
	croak("model::_get_option_val_pos No problems defined")
	    unless (defined $self->problems);
	my @problems = @{$self->problems};
	unless( scalar(@problem_numbers)>0 ){
		$self->problems([]) unless defined $self->problems;
	  @problem_numbers = (1 .. $#{$self->problems}+1);
	}
        foreach my $i ( @problem_numbers ) {
	  my $rec_ref = $problems[ $i-1 ] -> $accessor;
	  if ( defined $problems[ $i-1 ] and defined $rec_ref ) {
	    my @records = @{$rec_ref};
	    unless( $#instances > 0 ){
	      @instances = (1 .. $#records+1);
	      
	    }
	    my @inst_values    = ();
	    my @inst_positions = ();
	    my $glob_pos = 1;
	    my ( $glob_value, $glob_position );
INSTANCES:  foreach my $j ( @instances ) {
	      if ( defined $records[ $j-1 ] ) {
		my $k = 1;
		my ( $value, $position );
		foreach my $option ( @{$records[$j-1]->options} ) {
		  if ( defined $option and $option -> name eq $name) {
		    if ( $global_position ) {
		      $glob_value = $option -> value;
		      $glob_position = $glob_pos;
		      last INSTANCES;
		    } else {
		      $value = $option -> value;
		      $position = $k;
		    }
		  }
		  $k++;
		  $glob_pos++;
		}
		push( @inst_values, $value );
		push( @inst_positions, $position );
	      } else {
		croak("Instance $j in problem number $i does not exist!" )
	      }
	    }
	    if ( $global_position ) {
	      push( @values, $glob_value );
	      push( @positions, $glob_position );
	    } else {
	      push( @values, \@inst_values );
	      push( @positions, \@inst_positions );
	    }
	  } else {
	    croak("Problem number $i does not exist!" );
	  }
	}
      }
# line 7499 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _get_option_val_pos');
	# End of Non-Dia code #

	return \@values ,\@positions;
}

sub _option_name {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'position' => 'SCALAR', 'record' => 'SCALAR',
			'problem_number' => 'SCALAR', 'instance' => 'SCALAR',
			'new_name' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->_option_name: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->_option_name: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->_option_name: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->_option_name: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->_option_name: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $position = defined $parm{'position'} ? $parm{'position'} : 1;
	my $record = $parm{'record'};
	my $problem_number = defined $parm{'problem_number'} ? $parm{'problem_number'} : 1;
	my $instance = defined $parm{'instance'} ? $parm{'instance'} : 1;
	my $new_name = $parm{'new_name'};
	my $name;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _option_name');
# line 4658 "lib/model_subs.pm" 
      {

	my ( @problems, @records, @options, $i );
	my $accessor = $record.'s';
	if ( defined $self->problems ) {
	  @problems = @{$self->problems};
	} else {
	  croak("No problems defined in model" );
	}
	if ( defined $problems[$problem_number - 1] -> $accessor ) {
	  @records = @{$problems[$problem_number - 1] -> $accessor};
	} else {
	  croak("No record $record defined in ".
			  "problem number $problem_number." );
	}
	if ( defined $records[$instance - 1] -> options ) {
	  @options = @{$records[$instance - 1] -> options};
	} else {
	  croak("model -> _option_name: No option defined in record ".
			  "$record in problem number $problem_number." );
	}
	$i = 0;
	foreach my $option ( @options ) {
	  if ( $i == $position ) {
	    if ( defined $new_name ){
	      $option -> name($new_name) if ( defined $option );
	    }else{
	      $name = $option -> name if ( defined $option );
	    }
	  }
	  $i++;
	}
      }
# line 7578 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _option_name');
	# End of Non-Dia code #

	return $name;
}

sub _parameter_count {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'record' => 'SCALAR', 'problem_number' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->_parameter_count: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->_parameter_count: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->_parameter_count: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->_parameter_count: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->_parameter_count: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $record = $parm{'record'};
	my $problem_number = defined $parm{'problem_number'} ? $parm{'problem_number'} : 1;
	my $count = 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _parameter_count');
# line 4697 "lib/model_subs.pm" 
      {
	if( defined $self->problems ){
	  my $problems = $self->problems;
	  if( defined $problems->[$problem_number - 1] ){
	    $count = $problems->[$problem_number - 1] -> record_count( 'record_name' => $record );
	  }
	}
      }
# line 7627 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _parameter_count');
	# End of Non-Dia code #

	return $count;
}

sub _init_attr {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'parameter_type' => 'SCALAR', 'with_priors' => 'SCALAR',
			'get_same' => 'SCALAR', 'parameter_numbers' => 'ARRAY',
			'attribute' => 'SCALAR', 'new_values' => 'ARRAY',
			'problem_numbers' => 'ARRAY', 'add_if_absent' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model->_init_attr: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model->_init_attr: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model->_init_attr: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->_init_attr: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model->_init_attr: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $parameter_type = $parm{'parameter_type'};
	my $with_priors = defined $parm{'with_priors'} ? $parm{'with_priors'} : 0;
	my $get_same = defined $parm{'get_same'} ? $parm{'get_same'} : 0;
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my $attribute = $parm{'attribute'};
	my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
	my @problem_numbers = defined $parm{'problem_numbers'} ? @{$parm{'problem_numbers'}} : ();
	my $add_if_absent = defined $parm{'add_if_absent'} ? $parm{'add_if_absent'} : 0;
	my @parameter_values;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _init_attr');
# line 4597 "lib/model_subs.pm" 
      {
	# The I<add_if_absent> argument tells the method to add an init (theta,omega,sigma)
	# if the parameter number points to a non-existing parameter with parameter number
	# one higher than the highest presently included. Only applicatble if
	# I<new_values> are set. Default value = 0;
	
	unless( scalar @problem_numbers > 0 ){
		$self->problems([]) unless defined $self->problems;
	  @problem_numbers = (1 .. $#{$self->problems}+1);
	}
	my @problems = @{$self->problems};
	if ( $#new_values >= 0 ) {
	  croak("The number of new value sets " . 
			  ($#new_values+1) . " do not" . 
			  " match the number of problems " . ($#problem_numbers+1) . " specified" )
	      unless(($#new_values == $#problem_numbers) );
	  if ( $#parameter_numbers > 0 ) {
	    croak("The number of parameter number sets do not" .
			    " match the number of problems specified" )
		unless(($#parameter_numbers == $#problem_numbers) );
	  }
	}

	my $new_val_idx = 0;
        foreach my $i ( @problem_numbers ) {
	  if ( defined $problems[ $i-1 ] ) {
	    if ( scalar @new_values > 0) {
	      # {{{ Update values
	      # Use attribute parameter_values to collect diagnostic outputs
	      push( @parameter_values,
		    $problems[ $i-1 ] ->
		    _init_attr( parameter_type    => $parameter_type,
				parameter_numbers => $parameter_numbers[ $new_val_idx ],
				new_values        => \@{$new_values[ $new_val_idx ]},
				attribute         => $attribute,
			        add_if_absent     => $add_if_absent ) );
	      # }}} Update values
	    } else {
	      # {{{ Retrieve values
	      push( @parameter_values,
		    $problems[ $i-1 ] ->
		    _init_attr( parameter_type    => $parameter_type,
				parameter_numbers => $parameter_numbers[ $i-1 ],
				with_priors       => $with_priors,
				get_same          => $get_same,
				attribute         => $attribute ) );
	      # }}} Retrieve values
	    }
	  } else {
	    croak("Problem number $i does not exist!" );
	  }
	  $new_val_idx++;
	}
      }
# line 7731 libgen/model.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _init_attr');
	# End of Non-Dia code #

	return \@parameter_values;
}

1;

