use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package model::problem;
use Carp;
use Data::Dumper;
my @print_order_omega_before_pk = ('sizes','problem','abbreviated','input','bind','data','msfi','contr','subroutine','prior','model','tol','infn','omega','anneal','pk','level','aesinitial','aes','des','error','pred','mix','theta','sigma','etas','phis','simulation','estimation','covariance','nonparametric','table','scatter');
my @print_order = ('sizes','problem','abbreviated','input','bind','data','msfi','contr','subroutine','prior','model','tol','infn','pk','level','aesinitial','aes','des','error','pred','mix','theta','omega','anneal','sigma','etas','phis','simulation','estimation','covariance','nonparametric','table','scatter');
my @sde_print_order = ('sizes','problem','abbreviated','input','bind','data','msfi','contr','subroutine','prior','model','tol','infn','theta','omega','anneal','sigma','etas','phis','pk','level','aesinitial','aes','des','error','pred','mix','simulation','estimation','covariance','nonparametric','table','scatter');
my %abbreviations;
my %unsupported_records;

# Here we intialize a hash used to find long names for abbreviated
# record names. We use the print_order array which contains all
# allowed record types.

foreach my $record_name( @print_order,'warnings','finedata' ){
  my $uc_short_type = substr(uc($record_name),0,3);;
  $uc_short_type = $uc_short_type.' ' if ( $record_name eq 'aes' );
  $uc_short_type = $uc_short_type.'I' if ( $record_name eq 'aesinitial' );
  $abbreviations{$uc_short_type} = $record_name;
}
foreach my $rec ('THETAI','THI','THETAR','THR','THETAP','THETAPV','OMEGAP','OMEGAPD','SIGMAP','SIGMAPD'){
    $unsupported_records{$rec}=1;
}


use debug;


#---------------------------------------------------------------------
#         Used Packages
#---------------------------------------------------------------------
use model::mirror_plot_module;
use model::cwres_module;
use model::problem::nonparametric;
use table_file;
use model::problem::theta;
use model::problem::sigma;
use model::problem::omega;
use model::problem::tol;
use model::problem::estimation;
use model::problem::pred;
use model::problem::mix;
use model::problem::aesinitial;
use model::problem::prior;
use model::problem::table;
use model::problem::simulation;
use model::problem::bind;
use model::problem::etas;
use model::problem::level;
use model::problem::phis;
use model::problem::anneal;
use model::problem::scatter;
use model::problem::sizes;
use model::problem::msfi;
use model::problem::model;
use model::problem::contr;
use model::problem::abbreviated;
use model::problem::aes;
use model::problem::pk;
use model::problem::des;
use model::problem::error;
use model::problem::covariance;
use model::problem::infn;
use model::problem::subroutine;
use model::problem::data;
use model::problem::input;
use model::problem::problem;

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'problems' => 'ARRAY', 'inputs' => 'ARRAY',
			'datas' => 'ARRAY', 'subroutines' => 'ARRAY',
			'infns' => 'ARRAY', 'covariances' => 'ARRAY',
			'errors' => 'ARRAY', 'dess' => 'ARRAY', 'pks' => 'ARRAY',
			'aess' => 'ARRAY', 'abbreviateds' => 'ARRAY',
			'contrs' => 'ARRAY', 'models' => 'ARRAY', 'msfis' => 'ARRAY',
			'sizess' => 'ARRAY', 'scatters' => 'ARRAY',
			'anneals' => 'ARRAY', 'phiss' => 'ARRAY', 'levels' => 'ARRAY',
			'etass' => 'ARRAY', 'binds' => 'ARRAY', 'simulations' => 'ARRAY',
			'tables' => 'ARRAY', 'priors' => 'ARRAY', 'aesinitials' => 'ARRAY',
			'mixs' => 'ARRAY', 'preds' => 'ARRAY', 'estimations' => 'ARRAY',
			'tols' => 'ARRAY', 'omegas' => 'ARRAY', 'sigmas' => 'ARRAY',
			'thetas' => 'ARRAY', 'table_files' => 'ARRAY',
			'nonparametrics' => 'ARRAY', 'cwres_modules' => 'ARRAY',
			'mirror_plot_modules' => 'ARRAY', 'cwres' => 'SCALAR',
			'mirror_plots' => 'SCALAR', 'directory' => 'SCALAR',
			'ignore_missing_files' => 'SCALAR', 'ignore_missing_output_files' => 'SCALAR',
			'prob_arr' => 'ARRAY', 'sde' => 'SCALAR', 'nwpri_ntheta' => 'SCALAR',
			'nwpri_neta' => 'SCALAR', 'omega_before_pk' => 'SCALAR',
			'tbs' => 'SCALAR', 'tbs_param' => 'SCALAR',
			'tbs_thetanum' => 'SCALAR', 'shrinkage_module' => 'model::shrinkage_module',
			'eigen_value_code' => 'SCALAR', 'nonparametric_code' => 'SCALAR',
			'shrinkage_code' => 'SCALAR', 'iwres_shrinkage_table' => 'SCALAR',
			'eta_shrinkage_table' => 'SCALAR' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
		$parm{'reference_object'} = undef;
	}
	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->new: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'cwres'} = defined $parm{'cwres'} ? $parm{'cwres'} : 0 unless defined $this -> {'cwres'};
	$this -> {'mirror_plots'} = defined $parm{'mirror_plots'} ? $parm{'mirror_plots'} : 0 unless defined $this -> {'mirror_plots'};
	$this -> {'ignore_missing_files'} = defined $parm{'ignore_missing_files'} ? $parm{'ignore_missing_files'} : 1 unless defined $this -> {'ignore_missing_files'};
	$this -> {'ignore_missing_output_files'} = defined $parm{'ignore_missing_output_files'} ? $parm{'ignore_missing_output_files'} : 1 unless defined $this -> {'ignore_missing_output_files'};
	$this -> {'sde'} = defined $parm{'sde'} ? $parm{'sde'} : 0 unless defined $this -> {'sde'};
	$this -> {'omega_before_pk'} = defined $parm{'omega_before_pk'} ? $parm{'omega_before_pk'} : 0 unless defined $this -> {'omega_before_pk'};
	$this -> {'tbs'} = defined $parm{'tbs'} ? $parm{'tbs'} : 0 unless defined $this -> {'tbs'};
	$this -> {'eigen_value_code'} = defined $parm{'eigen_value_code'} ? $parm{'eigen_value_code'} : 1 unless defined $this -> {'eigen_value_code'};
	$this -> {'nonparametric_code'} = defined $parm{'nonparametric_code'} ? $parm{'nonparametric_code'} : 1 unless defined $this -> {'nonparametric_code'};
	$this -> {'shrinkage_code'} = defined $parm{'shrinkage_code'} ? $parm{'shrinkage_code'} : 1 unless defined $this -> {'shrinkage_code'};

	bless $this, $class;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($this). '-> new');
# line 34 "lib/model/problem_subs.pm" 
    {
      
      unless ( defined $parm{'problems'} ) {
	# Parse given problem lines.
	$this -> _read_records();

	delete $this -> {'prob_arr'};		# FIXME: This is better fixed with Moose

	$this->update_prior_information();

	if (defined $this -> estimations() and ($PsN::nm_major_version > 6)){
	  my $default_format='s1PE12.5';
	  my $reset_file = 0;
	  my $reset_nolabel = 0;
	  my $reset_notitle = 0;
	  my $reset_format = 0;
	  my $found_order = 0;
	  my @estims = @{$this -> estimations()};
	  for (my $i=0; $i <= $#estims; $i++){
	    my $est = $estims[$i];
	    my $opt_ref = $est -> options();
	    if ( defined $opt_ref ) {
	      foreach my $option ( @{$opt_ref} ) {
		if (index('ORDER',$option->name())==0){
		  $found_order = 1 ;
		  next;
		}
		if (index('FILE',$option->name())==0){
		  $reset_file = 1 ;
		  if ($i == $#estims){
		    $option->value('psn.ext');
		    $reset_file = 0; #already done
		  }
		  next;
		}
		if (index('NOTITLE',$option->name())==0){
		  $reset_notitle = 1 ;
		  if ($i == $#estims){
		    $option->value('0');
		    $reset_notitle = 0; #already done
		  }
		  next;
		}
		if (index('NOLABEL',$option->name())==0){
		  $reset_nolabel = 1 ;
		  if ($i == $#estims){
		    $option->value('0');
		    $reset_nolabel = 0; #already done
		  }
		  next;
		}
	      } #end loop options
	    }
	    if ($i == $#estims){
	      #now we know that if we need to reset something, that option was not 
	      #set in last $EST, only in a previous one. Means we can add option
	      $est->_add_option(option_string => 'NOLABEL=0') if ($reset_nolabel);
	      $est->_add_option(option_string => 'NOTITLE=0') if ($reset_notitle);
	      $est->_add_option(option_string => 'FILE=psn.ext') if ($reset_file);
	      $est->_add_option(option_string => 'FORMAT='.$default_format) if ($reset_format);
	    }
	  }
	  if ($found_order){
	    $this -> remove_option( record_name => 'estimation',
				    record_number => 0,
				    option_name => 'ORDER',
				    fuzzy_match => 1 );
	    print "\n***Warning***\n".
		"Option ORDER in \$EST is not yet supported by PsN. It has been removed from ".
		"the control stream.\n";
	  }
	}
      }
      # Initialize table file objects (if any)
      $this -> _read_table_files( ignore_missing_files =>
				  $this->ignore_missing_output_files );

      if ( $this->cwres ) {

	$this -> add_cwres_module( 'init_data' => { problem => $this,
						    mirror_plots => $this->mirror_plots } );
	
      }

      if( $this->tbs ){
	if (defined $this->nwpri_ntheta()){
	  ui -> print( category => 'all',
		       message => "Warning: \$PRIOR NWPRI is not supported in combination with -tbs.",
		       newline => 1);
	}

	$this -> tbs_transform();
      }

    }
# line 245 libgen/model/problem.pm 
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

sub inputs {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'inputs'} = $parm;
	} else {
		return $self -> {'inputs'};
	}
}

sub datas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'datas'} = $parm;
	} else {
		return $self -> {'datas'};
	}
}

sub subroutines {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'subroutines'} = $parm;
	} else {
		return $self -> {'subroutines'};
	}
}

sub infns {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'infns'} = $parm;
	} else {
		return $self -> {'infns'};
	}
}

sub covariances {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'covariances'} = $parm;
	} else {
		return $self -> {'covariances'};
	}
}

sub errors {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'errors'} = $parm;
	} else {
		return $self -> {'errors'};
	}
}

sub dess {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'dess'} = $parm;
	} else {
		return $self -> {'dess'};
	}
}

sub pks {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'pks'} = $parm;
	} else {
		return $self -> {'pks'};
	}
}

sub aess {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'aess'} = $parm;
	} else {
		return $self -> {'aess'};
	}
}

sub abbreviateds {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'abbreviateds'} = $parm;
	} else {
		return $self -> {'abbreviateds'};
	}
}

sub contrs {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'contrs'} = $parm;
	} else {
		return $self -> {'contrs'};
	}
}

sub models {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'models'} = $parm;
	} else {
		return $self -> {'models'};
	}
}

sub msfis {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'msfis'} = $parm;
	} else {
		return $self -> {'msfis'};
	}
}

sub sizess {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sizess'} = $parm;
	} else {
		return $self -> {'sizess'};
	}
}

sub scatters {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'scatters'} = $parm;
	} else {
		return $self -> {'scatters'};
	}
}

sub anneals {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'anneals'} = $parm;
	} else {
		return $self -> {'anneals'};
	}
}

sub phiss {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'phiss'} = $parm;
	} else {
		return $self -> {'phiss'};
	}
}

sub levels {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'levels'} = $parm;
	} else {
		return $self -> {'levels'};
	}
}

sub etass {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'etass'} = $parm;
	} else {
		return $self -> {'etass'};
	}
}

sub binds {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'binds'} = $parm;
	} else {
		return $self -> {'binds'};
	}
}

sub simulations {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'simulations'} = $parm;
	} else {
		return $self -> {'simulations'};
	}
}

sub tables {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'tables'} = $parm;
	} else {
		return $self -> {'tables'};
	}
}

sub priors {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'priors'} = $parm;
	} else {
		return $self -> {'priors'};
	}
}

sub aesinitials {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'aesinitials'} = $parm;
	} else {
		return $self -> {'aesinitials'};
	}
}

sub mixs {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'mixs'} = $parm;
	} else {
		return $self -> {'mixs'};
	}
}

sub preds {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'preds'} = $parm;
	} else {
		return $self -> {'preds'};
	}
}

sub estimations {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'estimations'} = $parm;
	} else {
		return $self -> {'estimations'};
	}
}

sub tols {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'tols'} = $parm;
	} else {
		return $self -> {'tols'};
	}
}

sub omegas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'omegas'} = $parm;
	} else {
		return $self -> {'omegas'};
	}
}

sub sigmas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'sigmas'} = $parm;
	} else {
		return $self -> {'sigmas'};
	}
}

sub thetas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'thetas'} = $parm;
	} else {
		return $self -> {'thetas'};
	}
}

sub table_files {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'table_files'} = $parm;
	} else {
		return $self -> {'table_files'};
	}
}

sub nonparametrics {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nonparametrics'} = $parm;
	} else {
		return $self -> {'nonparametrics'};
	}
}

sub cwres_modules {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'cwres_modules'} = $parm;
	} else {
		return $self -> {'cwres_modules'};
	}
}

sub mirror_plot_modules {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'mirror_plot_modules'} = $parm;
	} else {
		return $self -> {'mirror_plot_modules'};
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

sub prob_arr {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'prob_arr'} = $parm;
	} else {
		return $self -> {'prob_arr'};
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

sub nwpri_ntheta {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nwpri_ntheta'} = $parm;
	} else {
		return $self -> {'nwpri_ntheta'};
	}
}

sub nwpri_neta {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nwpri_neta'} = $parm;
	} else {
		return $self -> {'nwpri_neta'};
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

sub shrinkage_module {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'shrinkage_module'} = $parm;
	} else {
		return $self -> {'shrinkage_module'};
	}
}

sub eigen_value_code {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'eigen_value_code'} = $parm;
	} else {
		return $self -> {'eigen_value_code'};
	}
}

sub nonparametric_code {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nonparametric_code'} = $parm;
	} else {
		return $self -> {'nonparametric_code'};
	}
}

sub shrinkage_code {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'shrinkage_code'} = $parm;
	} else {
		return $self -> {'shrinkage_code'};
	}
}

sub iwres_shrinkage_table {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'iwres_shrinkage_table'} = $parm;
	} else {
		return $self -> {'iwres_shrinkage_table'};
	}
}

sub eta_shrinkage_table {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'eta_shrinkage_table'} = $parm;
	} else {
		return $self -> {'eta_shrinkage_table'};
	}
}

sub add_mirror_plot_module {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_mirror_plot_module given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'mirror_plot_modules'}},
		model::mirror_plot_module -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_cwres_module {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_cwres_module given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'cwres_modules'}},
		model::cwres_module -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_nonparametric {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_nonparametric given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'nonparametrics'}},
		model::problem::nonparametric -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_table_file {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_table_file given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'table_files'}},
		table_file -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_theta {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_theta given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'thetas'}},
		model::problem::theta -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_sigma {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_sigma given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'sigmas'}},
		model::problem::sigma -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_omega {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_omega given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'omegas'}},
		model::problem::omega -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_tol {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_tol given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'tols'}},
		model::problem::tol -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_estimation {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_estimation given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'estimations'}},
		model::problem::estimation -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_pred {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_pred given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'preds'}},
		model::problem::pred -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_mix {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_mix given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'mixs'}},
		model::problem::mix -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_aesinitial {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_aesinitial given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'aesinitials'}},
		model::problem::aesinitial -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_prior {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_prior given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'priors'}},
		model::problem::prior -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_table {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_table given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'tables'}},
		model::problem::table -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_simulation {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_simulation given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'simulations'}},
		model::problem::simulation -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_bind {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_bind given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'binds'}},
		model::problem::bind -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_etas {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_etas given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'etass'}},
		model::problem::etas -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_level {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_level given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'levels'}},
		model::problem::level -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_phis {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_phis given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'phiss'}},
		model::problem::phis -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_anneal {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_anneal given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'anneals'}},
		model::problem::anneal -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_scatter {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_scatter given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'scatters'}},
		model::problem::scatter -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_sizes {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_sizes given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'sizess'}},
		model::problem::sizes -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_msfi {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_msfi given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'msfis'}},
		model::problem::msfi -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_model {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_model given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'models'}},
		model::problem::model -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_contr {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_contr given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'contrs'}},
		model::problem::contr -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_abbreviated {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_abbreviated given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'abbreviateds'}},
		model::problem::abbreviated -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_aes {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_aes given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'aess'}},
		model::problem::aes -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_pk {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_pk given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'pks'}},
		model::problem::pk -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_des {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_des given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'dess'}},
		model::problem::des -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_error {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_error given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'errors'}},
		model::problem::error -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_covariance {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_covariance given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'covariances'}},
		model::problem::covariance -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_infn {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_infn given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'infns'}},
		model::problem::infn -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_subroutine {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_subroutine given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'subroutines'}},
		model::problem::subroutine -> new ( %{$parm{'init_data'}} ) );
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
		model::problem::data -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_input {
	my $self = shift;
	my %parm = @_;
	my @valid_parm = ( 'init_data' );
	my %isvalid = undef;
	foreach my $givenp ( keys %parm ){
		foreach my $validp ( @valid_parm ) {
			$isvalid{ $givenp } = 1 if ( $givenp eq $validp );
		}
		'debug' -> die( message => "Error in add_input given paramter $givenp is not valid\n" ) unless( $isvalid{$givenp} );
	}
	push( @{$self -> {'inputs'}},
		model::problem::input -> new ( %{$parm{'init_data'}} ) );
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
		model::problem::problem -> new ( %{$parm{'init_data'}} ) );
	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub add_prior_distribution {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'from_output' => 'output', 'problem_number' => 'SCALAR',
			'df_string' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->add_prior_distribution: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->add_prior_distribution: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->add_prior_distribution: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->add_prior_distribution: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->add_prior_distribution: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $from_output = $parm{'from_output'};
	my $problem_number = defined $parm{'problem_number'} ? $parm{'problem_number'} : 0;
	my $df_string = $parm{'df_string'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> add_prior_distribution');
# line 395 "lib/model/problem_subs.pm" 
    {
      #First check that do not already have prior
      if ((defined $self->priors()) and scalar(@{$self -> priors()})>0 ){
	print "Warning: Cannot add prior to \$PROB which already has a \$PRIOR\n";
	return;
      }
      #Set ntheta=number of THETAs in input model.
      #Set neta=number of diagonal OMEGAs in input model.
      #Set neps=0
      #Set nthp=ntheta
      #Set netp=neta
      my $ntheta = $self->record_count(record_name=> 'theta');
      my $nthp = $ntheta;
      my $neta = $self->nomegas('with_same'=>1, 'with_correlations' => 0);
      my $netp=$neta;
      my $neps=0;
      #Add record to simulation model
      #$PRIOR NWPRI NTHETA=ntheta,NETA=neta,NEPS=neps,NTHP=nthp,NETP=netp
      #Need to set NPEXP?? number of prior experiments
      my $plev='';
      $plev= 'PLEV=0.99' 
	  if ((defined $self->simulations()) and scalar(@{$self -> simulations()})>0 );
      my $record_string=" NWPRI NTHETA=$ntheta NETA=$neta NTHP=$nthp NETP=$netp $plev";
      $self -> set_records( 'type' => 'prior',
			    'record_strings' => [$record_string] );


      #Add ntheta new $THETA FIX. Initial estimates are final THETA estimates from lst-file.
      my $ref = $from_output->get_single_value(attribute=>'thetacoordval',
					       problem_index => ($problem_number-1),
					       subproblem_index => 0);
      unless (defined $ref){
	print "Cannot add prior if output data lacks THETA estimates\n";
	return;
      }
      my %thetas = %{$ref};
      for (my $i=1;$i<=$nthp;$i++){
	my $val=$thetas{'THETA'.$i};
	$self -> add_records(type => 'theta',
			     record_strings => ["$val FIX"]);
      }
      
      my $ref = $from_output->get_single_value(attribute=>'covariance_matrix',
					       problem_index=>($problem_number-1),
					       subproblem_index=>0);
      unless (defined $ref){
	print "Cannot add prior if output data lacks covariance matrix\n";
	return;
      }

      #Add diagonal $OMEGA FIX where initial estimates is the leading ntheta block 
      #from the variance-covariance matrix in .cov
      my @record_strings = ();
      my $index=0;
      for (my $i=0;$i<$nthp;$i++){
	my @arr;
	for (my $j=0;$j<=$i;$j++){
	  if ($j == $i){
	    my $init = sprintf("%.15G",$ref->[$index]);#must format with E here
	    push(@arr,$init,'FIX');
	  }
	  $index++;
	}
	push(@record_strings,join(' ',@arr));
      }
      my @omega_variance;
      for (my $i=1+$nthp;$i<=($netp+$nthp);$i++){
	my $index=-1+$i*$i/2;
	push(@omega_variance,$ref->[$index]);
      }
      $self -> add_records(type => 'omega',
			   record_strings => \@record_strings);

      #Add $OMEGA FIX where size is neta and initial estimates are final $OMEGA estimate 
      #from lst. Form must match original $OMEGA form in lst.
      my $ref = $from_output->get_single_value(attribute=>'omegacoordval',
					       problem_index => ($problem_number-1),
					       subproblem_index => 0);

      unless (defined $ref){
	print "Cannot add prior if output data lacks OMEGA estimates\n";
	return;
      }
      my %omegas = %{$ref};

      #loop over this models omega records
      #create copy of record
      #use coordinate strings to replace inits with values from output

      my $set_prior_etas=0;
      my @all_formatted;
      my $size=0;
      foreach my $record (@{$self->omegas()}){
	last if ($set_prior_etas >= $neta);
	my @record_strings;
	my $block = 0;
	if ($record->type() eq 'BLOCK'){
	  $block = 1;
	  if ($record->same()){
	    @record_strings = ('BLOCK SAME');
	    $self->add_records(type=> 'omega',
			       record_strings => \@record_strings);
	    $set_prior_etas += $size;
	    next;
	  }else{
	    $size = $record->size();
	    @record_strings = ("BLOCK($size) FIX");
	  }
	}else{
	  $size = 0;
	}
	foreach my $opt (@{$record->options()}){
	  my $label = $opt->coordinate_string();
	  my $init = sprintf("%.15G",$omegas{$label});#must format with E here
	  if ($block){
	    push(@record_strings,"$init");
	  }else{
	    $size++;
	    push(@record_strings,"$init FIX");
	  }
	}
	$set_prior_etas += $size;
	print "Error too many new omegas\n" if ($set_prior_etas > $neta);
	$self->add_records(type=> 'omega',
			   record_strings => \@record_strings);
      }

      my @dflist = split(/,/,$df_string);
      foreach my $df (@dflist){
	$self -> add_records(type => 'theta',
			     record_strings => ["$df FIX"]);
      }


      #immediately after adding $PRIOR etc must run update_prior_information on problem
      $self->update_prior_information();

    }
# line 1843 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> add_prior_distribution');
	# End of Non-Dia code #

}

sub store_inits {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> store_inits');
# line 1301 "lib/model/problem_subs.pm" 
      {
	if ( defined $self->thetas ) {
	  foreach my $theta ( @{$self->thetas} ) {
	    $theta -> store_inits;
	  }
	}
	if ( defined $self->omegas ) {
	  foreach my $omega ( @{$self->omegas} ) {
	    $omega -> store_inits;
	  }
	}
	if ( defined $self->sigmas ) {
	  foreach my $sigma ( @{$self->sigmas} ) {
	    $sigma -> store_inits;
	  }
	}
      }
# line 1872 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> store_inits');
	# End of Non-Dia code #

}

sub restore_inits {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> restore_inits');
# line 1022 "lib/model/problem_subs.pm" 
      {
	if ( defined $self->thetas ) {
	  foreach my $theta ( @{$self->thetas} ) {
	    $theta -> restore_inits;
	  }
	}
	if ( defined $self->omegas ) {
	  foreach my $omega ( @{$self->omegas} ) {
	    $omega -> restore_inits;
	  }
	}
	if ( defined $self->sigmas ) {
	  foreach my $sigma ( @{$self->sigmas} ) {
	    $sigma -> restore_inits;
	  }
	}
      }
# line 1901 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> restore_inits');
	# End of Non-Dia code #

}

sub set_random_inits {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'degree' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->set_random_inits: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->set_random_inits: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->set_random_inits: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->set_random_inits: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->set_random_inits: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $degree = defined $parm{'degree'} ? $parm{'degree'} : 0.1;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> set_random_inits');
# line 1044 "lib/model/problem_subs.pm" 
      {
	if ( defined $self->thetas ) {
	  foreach my $theta ( @{$self->thetas} ) {
	    $theta -> set_random_inits( degree => $degree );
	  }
	}
	if ( defined $self->omegas ) {
	  foreach my $omega ( @{$self->omegas} ) {
	    $omega -> set_random_inits( degree => $degree );
	  }
	}
	if ( defined $self->sigmas ) {
	  foreach my $sigma ( @{$self->sigmas} ) {
	    $sigma -> set_random_inits( degree => $degree );
	  }
	}

	$self->ensure_diagonal_dominance();
      }
# line 1958 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> set_random_inits');
	# End of Non-Dia code #

}

sub record_count {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'record_name' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->record_count: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->record_count: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->record_count: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->record_count: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->record_count: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $record_name = $parm{'record_name'};
	my $return_value = 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> record_count');
# line 1006 "lib/model/problem_subs.pm" 
    {
	$return_value = 0;
	my $accessor = $record_name . 's';
	if( defined $self->$accessor ){
	    foreach my $record ( @{$self->$accessor} ){
		if( defined $record -> options ){
		    $return_value += @{$record -> options};
		}
	    }
	}
    }
# line 2008 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> record_count');
	# End of Non-Dia code #

	return $return_value;
}

sub _init_attr {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'parameter_type' => 'SCALAR', 'get_same' => 'SCALAR',
			'with_priors' => 'SCALAR', 'parameter_numbers' => 'ARRAY',
			'attribute' => 'SCALAR', 'new_values' => 'ARRAY',
			'add_if_absent' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->_init_attr: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->_init_attr: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->_init_attr: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->_init_attr: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->_init_attr: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $parameter_type = $parm{'parameter_type'};
	my $get_same = defined $parm{'get_same'} ? $parm{'get_same'} : 0;
	my $with_priors = defined $parm{'with_priors'} ? $parm{'with_priors'} : 0;
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my $attribute = $parm{'attribute'};
	my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
	my $add_if_absent = defined $parm{'add_if_absent'} ? $parm{'add_if_absent'} : 0;
	my @parameter_values;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _init_attr');
# line 1380 "lib/model/problem_subs.pm" 
      {
	# Private method, should preferably not be used outside model.pm
	# The add_if_absent argument tells the method to add an init (theta,omega,sigma)
	# if the parameter number points to a non-existing parameter with parameter number
	# one higher than the highest presently included. Only applicatble if
	# new_values are set. Default value = 0;
	
	my $accessor = $parameter_type.'s';
	unless( $self -> can($accessor) ){
	    croak("problem -> _init_attr: Error unknown parameter type: $parameter_type" );
	}

	my @records;
	if( defined $self->$accessor ){
	  @records = @{$self->$accessor};
	} else {
	  @records = ();
	}
	
	my @options = ();

	# {{{ Check that the size of parameter_numbers and new_values match
	my %num_val;
	if ( $#parameter_numbers >= 0 and $#new_values >= 0 ) {
	  if ( $#parameter_numbers == $#new_values ) {
	    for ( my $i = 0; $i <= $#new_values; $i++ ) {
	      $num_val{$parameter_numbers[$i]} = $new_values[$i];
	    }
	  } else {
	    die "Model::problem -> _init_attr: The number of specified ".
	      "parameters (@parameter_numbers) and values (@new_values) do not match for parameter $parameter_type".
		" and attribute $attribute\n";
	  }
	}
	# }}}

	my $prev_size = 1;
	if ( scalar @new_values > 0 ) {
	  # {{{ Update values

	  # OBS! We are using 'normal' numbering in parameter_numbers, i.e. they begin
	  # at one (1).
	  my $opt_num = 1;
	  # Ugly solution to add non-existing options:
	  my %found;
	  foreach my $num ( @parameter_numbers) {
#	    print "inpn: $num\n";
	    $found{$num} = 0;
	  }

	  my @diagnostics = ();
	  foreach my $record ( @records ) {
	    if ( $record -> same() ) {
	      # SAME == true: Nothing to be done. Just move forward to next $OMEGA but
	      # increase counter first

	      $opt_num += $prev_size;
	    } else {
	      foreach my $option ( @{$record -> options} ) {
		if ( scalar @parameter_numbers > 0 ) {
		  foreach my $num ( @parameter_numbers ) {
		    if ( $num == $opt_num ) {
		      $found{$num}++;
		      if ( $attribute eq 'init' ) {
			push( @diagnostics,
			      $option -> check_and_set_init( new_value => $num_val{$num} ) );
		      } elsif( $attribute eq 'fix' and defined $record -> size() 
			       and ($record-> type() eq 'BLOCK') ){
			# size() tells us this MIGHT be a block and we must fix on record level.
			#check type also
			$record -> fix( $num_val{$num} );
		      } else {
			$option -> $attribute( $num_val{$num} );
		      }
		    }
		  }
		} else {
		  if ( $attribute eq 'init' ) {
		    push( @diagnostics,
			  $option -> check_and_set_init( new_value => shift( @new_values ) ) );
		  } elsif( $attribute eq 'fix' and defined $record -> size()
			   and ($record-> type() eq 'BLOCK')){
		    # size() tells us this MIGHT a block and we must fix on record level.Check type also
		    $record -> fix( shift( @new_values ) );
		  } else {
		    $option -> $attribute( shift( @new_values ) );
		  }
		}
		$opt_num++;
	      }
	      if( $parameter_type eq 'theta' ){
		$prev_size = scalar @{$record -> options};
	      } else {
		my $size = $record -> size;
		if( (defined $size) and ($record->type eq 'BLOCK') ) {
		  $prev_size = ($size*($size+1))/2;
		} else {
		  $prev_size = scalar @{$record -> options};
		}
	      }
	    }
	  }
	  # If $add_if_absent is set, any parameters that were not found above are
	  # added below:
	  
	  my @nums = sort {$a<=>$b} keys %found;
	  my $new_record = "model::problem::$parameter_type" -> new();
	  my $do_add_record;
	  my $added_thetas=1;
	  my $added_sigmas=1;
	  my $added_omegas=1;
	  foreach my $num ( @nums ) {
	    if ( $add_if_absent and
		 not $found{$num} ) {
	      $do_add_record = 1;
	      unless($num == $opt_num) {
		croak("Attempt to add a parameter with higher number ($num) than the number\n".
				           "of parameters + 1 ($opt_num)\n" );
	      }
	      # Get the last record of $parameter_type
	      # my $new_record = $records[$#records];
	      my $option_class;

	      my $coordinate_string;
	      if( $parameter_type eq 'theta' ){
		$option_class = 'model::problem::record::theta_option';
		my $index = $self->record_count('record_name' => 'theta')+$added_thetas;
		$coordinate_string='THETA'.$index;
		$added_thetas++;
	      } else {
		$option_class = 'model::problem::record::init_option';
		if( $parameter_type eq 'omega' ){
		  my $index = $self->nomegas('with_correlations' => 0,'with_same' => 1)+$added_omegas;
		  $coordinate_string='OMEGA('.$index.','.$index.')';
		  $added_omegas++;
		}else {
 		  my $index = $self->sigmas('with_correlations' => 0,'with_same' => 1)+$added_sigmas;
		  $coordinate_string='SIGMA('.$index.','.$index.')';
		  $added_sigmas++;
		}
	      }

	      # Push a new option to this last record
	      my $option = $option_class -> new(coordinate_string => $coordinate_string);
	      if ( $attribute eq 'init' ) {
		$option -> check_and_set_init( new_value => $num_val{$num} );
	      } elsif( $attribute eq 'fix' and defined $new_record -> size()
		       and ($new_record-> type() eq 'BLOCK')){

		# size() tells us this is MIGHT be a block and we must fix on
		# record level. This will never happen, as we can't
		# add BLOCKS, at least not like this.

		$new_record -> fix( $num_val{$num} );
	      } else {
		$option -> $attribute( $num_val{$num} );
	      }
	      $new_record->options([]) unless (defined $new_record->options());
	      push( @{$new_record->options}, $option );

	      # So we've added a parameter. Possible to add more,
	      # lets increase the highest found:
	      $opt_num++;
	    }
	  }
	  if ( $attribute eq 'init' ) {
	    # We're updating but might be returning diagnostics
	    # Use the default return parameter parameter_values for this
	    @parameter_values = @diagnostics;
	  }

	  if( $do_add_record ){
	    push( @records, $new_record );
	    $self->$accessor(\@records);
	  }

	  # }}} Update values
	} else {
	  # {{{ Retrieve values

	  my @prev_values = ();
	  my $done=0;
	  foreach my $record ( @records ) {
	    last if ($done);
	    last if ($record->prior() and (not $with_priors));
	    unless ( $record -> same() ) {
	      @prev_values = ();
	      if ( defined $record -> options ) {
		foreach my $option ( @{$record -> options} ) {
		  if ($option->prior() and (not $with_priors)){
		    $done=1;
		    last;
		  }
		  push( @prev_values, $option -> $attribute );
		}
	      } else {
		carp("Trying to get attribute $attribute, ".
				 "but no options defined in record ".ref($record) );
	      }
	      $prev_size = $record -> size unless ( $record -> same );
	    }
	    if( $record -> same() and (not $get_same)) {
	      for( my $i = 0; $i <= $#prev_values; $i++ ) {
		$prev_values[$i] = undef;
	      }
	    }
	    push( @parameter_values, @prev_values );
	  }
	  
	  if ( scalar @parameter_numbers > 0 ) {
	    my @part_vals = ();
	    foreach my $num ( @parameter_numbers ) {
	      push( @part_vals, $parameter_values[$num -1] );
	    }
	    @parameter_values = @part_vals;
	  } else {
	    carp("Model::problem -> _init_attr: parameter_numbers undefined, using all." );
	  }
	  
	  # }}} Retrieve values
	}
      }
# line 2279 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _init_attr');
	# End of Non-Dia code #

	return \@parameter_values;
}

sub indexes {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'parameter_type' => 'SCALAR', 'parameter_numbers' => 'ARRAY',
			'with_priors' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->indexes: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->indexes: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->indexes: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->indexes: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->indexes: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $parameter_type = $parm{'parameter_type'};
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my $with_priors = defined $parm{'with_priors'} ? $parm{'with_priors'} : 0;
	my @indexes;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> indexes');
# line 754 "lib/model/problem_subs.pm" 
  # The Indexes method returns the coordinate_string for all init_options
  # THETA1, THETA2 or OMEGA(1,1) etc or SIGMA(1,1) SIGMA(2,1)etc
  # indexes are also returned if BLOCK SAME 
      {
	my $row = 1;
	my $accessor = $parameter_type.'s';
	my $previous_size = 0;
	my @previous;
	my $done=0;

	if( defined $self->$accessor ){

	  foreach my $record ( @{$self -> $accessor} ) {
	    last if ($done);
	    last if ($record->prior() and (not $with_priors));
	    if( $record -> same() ) {
	      if( $previous_size == 0 ){
		croak("You can't have an $parameter_type ".
				"estimate defined as SAME if there is no previous estimate" );
	      }
	      #add $previous_size to all @previous rows and cols
	      my @these=();
	      foreach my $coord ( @previous ) {
		if ($coord =~ /THETA(\d+)/){
		  push( @these, 'THETA'.($1+$previous_size)); 
		  
		}elsif ($coord =~ /(OMEGA|SIGMA)\((\d+)\,(\d+)\)/ ){
		  push( @these, $1.'('.($2+$previous_size).','.($3+$previous_size).')');
		} else {
		  croak("Unknown coordinate string $coord");
		}
	      }
	      push( @indexes,@these);
	      @previous = @these;
	    }elsif ( defined $record -> options() ) {
	      if (defined $record ->size()){
		$previous_size = $record ->size();
	      }else {
		#if no size and not same then must be diagonal
		$previous_size = scalar(@{$record -> options()});
	      }
	      @previous = ();
	      foreach my $option ( @{$record -> options()} ) {
		if ($option->prior() and (not $with_priors)){
		  $done=1;
		  last;
		}
#		print "str ".$option->coordinate_string()."\n";
		push( @indexes, $option->coordinate_string() );
		push( @previous, $option->coordinate_string() );
	      }
	    }
	  }
	}
	
	if ( scalar @parameter_numbers > 0 ) {
	  my @part_indexes = ();
	  foreach my $num ( @parameter_numbers ) {
	    if ( $num < 1 or $num > scalar @indexes ) {
	      croak("$parameter_type number " . $num . " does not exist in this model::problem\n" .
			      "(" . scalar @indexes . " exists)\n" );
	    }
	    push( @part_indexes, $indexes[$num -1] );
	  }
	  @indexes = @part_indexes;
	} else {
	  carp("Model::problem -> indexes: parameter_numbers undefined, using all." );
	}
      }
# line 2391 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> indexes');
	# End of Non-Dia code #

	return \@indexes;
}

sub covariance {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'enabled' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->covariance: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->covariance: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->covariance: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->covariance: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->covariance: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $enabled = $parm{'enabled'};
	my $indicator = 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> covariance');
# line 699 "lib/model/problem_subs.pm" 
      {
	my @records;
	if( defined $self->covariances ) {
	  @records = @{$self->covariances} ;
	}
	if ( defined $enabled ) {
	  if ( $enabled and $#records < 0 ) {
	    $self -> add_records( type           => 'covariance',
				  record_strings => [''] );
	  } elsif ( not $enabled and $#records >= 0 ) {
	    $self -> {'covariances'} = undef;	# FIXME: Fix this with Moose
	  }
	} else {
	  if ( $#records >= 0 ) {
	    $indicator = 1;
	  } else {
	    $indicator = 0;
	  }
	}
      }
# line 2451 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> covariance');
	# End of Non-Dia code #

	return $indicator;
}

sub eigen {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'enabled' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->eigen: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->eigen: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->eigen: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->eigen: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->eigen: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $enabled = $parm{'enabled'};
	my $indicator = 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> eigen');
# line 725 "lib/model/problem_subs.pm" 
      {
	my ( $print_ref, $position ) = $self -> _option_val_pos( record_name => 'covariance',
							     name        => 'PRINT' );
      }
# line 2495 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> eigen');
	# End of Non-Dia code #

	return $indicator;
}

sub _option_val_pos {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'record_name' => 'SCALAR', 'instance_numbers' => 'ARRAY',
			'name' => 'SCALAR', 'new_values' => 'ARRAY',
			'exact_match' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->_option_val_pos: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->_option_val_pos: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->_option_val_pos: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->_option_val_pos: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->_option_val_pos: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $record_name = $parm{'record_name'};
	my @instance_numbers = defined $parm{'instance_numbers'} ? @{$parm{'instance_numbers'}} : ();
	my $name = $parm{'name'};
	my @new_values = defined $parm{'new_values'} ? @{$parm{'new_values'}} : ();
	my $exact_match = defined $parm{'exact_match'} ? $parm{'exact_match'} : 1;
	my @values;
	my @positions;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _option_val_pos');
# line 1879 "lib/model/problem_subs.pm" 
      {
	#
	# _option_val_pos( record_name => 'subroutine',
	#                  name => 'OTHER',
	#                  val => 'get_cov')
	#

	# _option_val_pos sets, or gets, the value of an option (given
	# as the 'name' parameter. Name must be uppercase) in a record
	# (given as the 'record_name' parameter. Record name should be
	# the record class name in the model diagram.)
	
	my $accessor = $record_name.'s';
	unless( $self -> can($accessor) ){
	  croak("Unknown record name: $record_name" );
	}

	my @records;
	if( defined $self->$accessor ) {
	  @records = @{$self->$accessor} ;
	} else {
	  carp("No records of type $accessor" );
	  @records = ();
	}
	my @options = ();

	# {{{ Check that the size of instance_numbers and new_values match

	my %num_val;
	if ( $#instance_numbers >= 0 and $#new_values >= 0 ) {
	  if ( $#instance_numbers == $#new_values ) {
	    for ( my $i = 0; $i <= $#new_values; $i++ ) {
	      $num_val{$instance_numbers[$i]} = $new_values[$i];
	    }
	  } else {
	    croak("Model::problem -> _option_val_pos: The number of specified " .
			    "parameters " . $#instance_numbers+1 . " and values " .
			    $#new_values+1 . " do not match" );
	  }
	}

	# }}}

	if ( scalar @new_values > 0 ) {
	  # {{{ Update values

	  my $opt_num = 1;
	  foreach my $record ( @records ) {
	    foreach my $option ( @{$record -> options} ) {
	      my $test_name = $exact_match ? uc($option -> name) :
		  uc(substr($option -> name,0,length($name)));
	      if ( $test_name eq $name) {
		if ( scalar @instance_numbers > 0 ) {
		  foreach my $num ( @instance_numbers ) {
		    $option -> value( $num_val{$num} ) if $num == $opt_num;
		  }
		} else {
		  $option -> value( shift( @new_values ) );
		}
		$opt_num++;
	      }
	    }
	  }

	  # }}} Update values
	} else {
	  # {{{ Retrieve values

	  foreach my $record ( @records ) {
	    my $i = 1;
	    if ( defined $record -> options ) {
	      foreach my $option ( @{$record -> options} ) {
		my $test_name = $exact_match ? uc($option -> name) :
		  uc(substr($option -> name,0,length($name)));
		if ( $test_name eq $name) {
		  push( @values, $option -> value );
		  push( @positions, $i );
		}
		$i++;
	      }
	    }
	  }
	  if ( $#instance_numbers > 0 ) {
	    my @part_vals = ();
	    my @part_pos = ();
	    foreach my $num ( @instance_numbers ) {
	      push( @part_vals, $values[$num -1] );
	      push( @part_pos, $positions[$num -1] );
	    }
	    @values = @part_vals;
	    @positions = @part_pos;
	  } 

	  # }}} Retrieve values
	}
      }	
# line 2638 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _option_val_pos');
	# End of Non-Dia code #

	return \@values ,\@positions;
}

sub name_val {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'parameter_type' => 'SCALAR', 'parameter_numbers' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->name_val: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->name_val: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->name_val: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->name_val: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->name_val: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $parameter_type = $parm{'parameter_type'};
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my @names_values;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> name_val');
# line 1609 "lib/model/problem_subs.pm" 
# line 2679 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> name_val');
	# End of Non-Dia code #

	return \@names_values;
}

sub remove_records {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'type' => 'm_SCALAR', 'keep_last' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->remove_records: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->remove_records: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->remove_records: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->remove_records: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->remove_records: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $type = $parm{'type'};
	my $keep_last = defined $parm{'keep_last'} ? $parm{'keep_last'} : 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> remove_records');
# line 1202 "lib/model/problem_subs.pm" 
      {
	  my $rec_class = "model::problem::$type";
	  my $accessor = $type.'s';
	  if( $self -> can($accessor) ){
	    if ($keep_last){
	      my @recs;
	      my $last_rec = undef;
	      @recs = @{$self -> $accessor} if (defined $self -> $accessor); 
	      $last_rec = $recs[-1] if (defined $recs[-1]);
	      $self -> $accessor([$last_rec]);
	    }else {
	      $self -> {$accessor} = undef;		# FIXME: Fix this with Moose
	    }
	  } else {
	      die "Error in problem -> remove_records: Trying to remove unknown record: $type\n";
	  }
      }
# line 2736 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> remove_records');
	# End of Non-Dia code #

}

sub _read_table_files {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'ignore_missing_files' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->_read_table_files: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->_read_table_files: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->_read_table_files: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->_read_table_files: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->_read_table_files: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $ignore_missing_files = defined $parm{'ignore_missing_files'} ? $parm{'ignore_missing_files'} : 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_table_files');
# line 637 "lib/model/problem_subs.pm" 
      {
	$self->table_files([]);
	my ( $table_name_ref, $junk ) = $self -> _option_val_pos( record_name => 'table',
								  name        => 'FILE' );
	if ( defined $table_name_ref and scalar @{$table_name_ref} >= 0 ) {
	  $self->table_files([]);
	  foreach my $table_name ( @{$table_name_ref} ) {
	    carp("Creating new table_file object from $table_name" );
	    my $new_table = data -> new( directory            => $self->directory,
					 filename             => $table_name,
					 ignore_missing_files => $ignore_missing_files,
					 target               => 'disk',
					 table_file           => 1 );
	    push( @{$self->table_files}, $new_table );
	  }
	}
      }
# line 2791 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_table_files');
	# End of Non-Dia code #

}

sub header {
	my $self = shift;
	my @header;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> header');
# line 735 "lib/model/problem_subs.pm" 
      {
	my $inp_ref = $self -> inputs;
	if ( defined $inp_ref and defined $inp_ref -> [0] ) {
	  my $input = $inp_ref -> [0];
	  my $opt_ref = $input -> options;
	  if ( defined $opt_ref ) {
	    my @options = @{$opt_ref};
	    foreach my $option ( @options ) {
	      push ( @header, [$option -> name, $option -> value] );
	    }
	  }
	}
      }
# line 2817 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> header');
	# End of Non-Dia code #

	return \@header;
}

sub dropped_columns {
	my $self = shift;
	my @dropped_columns;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> dropped_columns');
# line 589 "lib/model/problem_subs.pm" 
      {
	my $inp_ref = $self -> inputs;
	if ( defined $inp_ref and defined $inp_ref -> [0] ) {
	  my $input = $inp_ref -> [0];
	  my $opt_ref = $input -> options;
	  if ( defined $opt_ref ) {
	    my @options = @{$opt_ref};
	    foreach my $option ( @options ) {
	      my $dropped = ( $option -> value eq 'DROP' or
			      $option -> value eq 'SKIP' ) ? 1 : 0;
	      push ( @dropped_columns, $dropped );
	    }
	  }
	}
      }
# line 2846 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> dropped_columns');
	# End of Non-Dia code #

	return \@dropped_columns;
}

sub drop_dropped {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> drop_dropped');
# line 611 "lib/model/problem_subs.pm" 
      {
	my $inp_ref = $self -> inputs;
	# Important that the drop_dropped method of the data class is
	# in sync with this method.
	if ( defined $inp_ref and defined $inp_ref -> [0] ) {
	  my $input = $inp_ref -> [0];
	  my $opt_ref = $input -> options;
	  if ( defined $opt_ref ) {
	    my @options = @{$opt_ref};
	    my @keep;
	    foreach my $option ( @options ) {
	      push ( @keep, $option ) if ( not ($option -> value eq 'DROP' or $option -> value eq 'SKIP'
					   or $option -> name eq 'DROP' or $option -> name eq 'SKIP') or
					   $option -> name =~ /DAT(E|1|2|3)/ );
	    }
	    $input -> options( \@keep );
	  }
	}
      }
# line 2878 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> drop_dropped');
	# End of Non-Dia code #

}

sub remove_option {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'record_name' => 'SCALAR', 'record_number' => 'SCALAR',
			'option_name' => 'SCALAR', 'fuzzy_match' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->remove_option: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->remove_option: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->remove_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->remove_option: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->remove_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $record_name = $parm{'record_name'};
	my $record_number = defined $parm{'record_number'} ? $parm{'record_number'} : 0;
	my $option_name = $parm{'option_name'};
	my $fuzzy_match = defined $parm{'fuzzy_match'} ? $parm{'fuzzy_match'} : 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> remove_option');
# line 1267 "lib/model/problem_subs.pm" 

my $accessor = $record_name.'s';
unless( $self -> can($accessor) ){
  croak("Unknown record name: $record_name" );
}
if( defined $self->$accessor ) {
  my @records = @{$self->$accessor};
  my @record_numbers;
  if ($record_number > 0){
    push(@record_numbers,$record_number);
  }elsif ($record_number == 0){
    @record_numbers = 1 .. scalar(@records);
  }elsif ($record_number == -1){
    #last
    push(@record_numbers,scalar(@records));
  }else {
    croak("illegal input record_number $record_number to remove_option");
  }

  foreach my $recnum ( @record_numbers ) {
    #numbering starts at 1, unlike array indices
    $records[$recnum-1] -> remove_option( name => $option_name,
					  fuzzy_match => $fuzzy_match );
  }
} else {
  carp("No records of type $accessor" );
}

# line 2948 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> remove_option');
	# End of Non-Dia code #

}

sub add_option {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'record_number' => 'SCALAR', 'record_name' => 'SCALAR',
			'option_name' => 'SCALAR', 'option_value' => 'SCALAR',
			'add_record' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->add_option: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->add_option: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->add_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->add_option: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->add_option: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $record_number = defined $parm{'record_number'} ? $parm{'record_number'} : 0;
	my $record_name = $parm{'record_name'};
	my $option_name = $parm{'option_name'};
	my $option_value = $parm{'option_value'};
	my $add_record = defined $parm{'add_record'} ? $parm{'add_record'} : 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> add_option');
# line 1225 "lib/model/problem_subs.pm" 

my $accessor = $record_name.'s';
unless( $self -> can($accessor) ){
  croak("Unknown record name: $record_name" );
}
if( defined $self->$accessor ) {
  my @records = @{$self->$accessor};
  my @record_numbers;

  if ($record_number > 0){
    push(@record_numbers,$record_number);
  }elsif ($record_number == 0){
    @record_numbers = 1 .. scalar(@records);
  }elsif ($record_number == -1){
    #last
    push(@record_numbers,scalar(@records));
  }else {
    croak("illegal input record_number $record_number to add_option");
  }

  foreach my $recnum ( @record_numbers ) {
    #numbering starts at 1, unlike array indices
    $records[$recnum-1]-> add_option( init_data => { name  => $option_name,
						     value => $option_value } );
  }
} else {
  if( $add_record ) {
    $self -> add_records( type           => $record_name,
			  record_strings => ["$option_name=$option_value"] );
  } else {
    carp("No records of type $accessor and add_option ".
		      "set not to add one" );
  }
}

# line 3027 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> add_option');
	# End of Non-Dia code #

}

sub add_marginals_code {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'nomegas' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->add_marginals_code: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->add_marginals_code: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->add_marginals_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->add_marginals_code: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->add_marginals_code: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $nomegas = $parm{'nomegas'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> add_marginals_code');
# line 538 "lib/model/problem_subs.pm" 

# add_marginals_code takes one argument.
#
# - nomegas which is the number of (diagonal-element)
# omegas.
#
# For each omega, verbatim code is added to make the marginals
# available for printing (e.g. to a table file). COM(1) will hold the
# nonparametric density, COM(2) the marginal cumulative value for the
# first eta, COM(2) the marginal cumulative density for the second eta
# and so on.
# The code is added to the $ERROR record

my $record_ref = $self -> errors;
if( defined $record_ref and defined $record_ref -> [0] ) {
  my ( @first_params, @last_params );
  $last_params[0] = '"   COM(1) = DENM';
  $first_params[0] = '"     X ';
  my $j = 0;
  my $comma;
  for( my $i = 1; $i <= $nomegas; $i++ ) {
    $comma = $i == $nomegas ? '' : ',';
    if( not ($i % 4) ) { # break line every fifth omega
      $j++;
      $first_params[$j] = '"     X ';
    }
    $first_params[$j] = $first_params[$j]."DEN$i$comma";
    push( @last_params, '"   COM('.($i+1).") = DEN$i" );
  }
  my $first_code = $record_ref -> [0] -> verbatim_first;
  push( @{$first_code}, ( '"  COMMON /ROCM18/ DENM,', @first_params,
			  '"  DOUBLE PRECISION DENM,', @first_params ) );
  $record_ref -> [0] -> verbatim_first( $first_code );
  my $last_code = $record_ref -> [0] -> verbatim_last;
  push( @{$last_code}, @last_params );
  $record_ref -> [0] -> verbatim_last( $last_code );
  last; # Only insert the code in the first record found (of the ones specified above)
} else {
  carp("No \$ERROR record was found. Can't add verbatim code".
		    " to access nonparametric marginals" );
}

# line 3107 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> add_marginals_code');
	# End of Non-Dia code #

}

sub nomegas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'with_priors' => 'SCALAR', 'with_correlations' => 'SCALAR',
			'with_same' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->nomegas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->nomegas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->nomegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->nomegas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->nomegas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $nomegas;
	my $with_priors = defined $parm{'with_priors'} ? $parm{'with_priors'} : 0;
	my $with_correlations = defined $parm{'with_correlations'} ? $parm{'with_correlations'} : 0;
	my $with_same = defined $parm{'with_same'} ? $parm{'with_same'} : 1;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> nomegas');
# line 924 "lib/model/problem_subs.pm" 


my $prev =undef;
$nomegas=0;
$self->omegas([]) unless defined $self->omegas;
foreach my $omega ( @{$self->omegas} ) {
  last if ($omega->prior() and (not $with_priors));

  my $size = $omega -> size;
  my $type = $omega -> type;
  if ($omega->same()){
    croak("First \$OMEGA cannot be SAME")
	unless (defined $prev);
    $nomegas += $prev if $with_same;
  } elsif( defined $size ) {
      
    # If the record has a size, it is of block form with diagonal of
    # length given by $size. The actual values in the model file is
    # then the arithmetic sum: (n*(n+1))/2
    
    #Kajsa: size also for diagonal matrix! Added type check below.
    
    if( $with_correlations and ($type eq 'BLOCK')){
      $nomegas += ($size*($size+1))/2; 
      $prev = ($size*($size+1))/2;
    } else {
      $nomegas += $size;
      $prev = $size;
    }
  } elsif (defined $omega->options) {
    $nomegas += scalar @{$omega -> options};
    $prev = scalar @{$omega -> options};
  } else {
    croak("Failed to parse \$OMEGA." );
  }
}


# line 3187 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> nomegas');
	# End of Non-Dia code #

	return $nomegas;
}

sub nsigmas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'with_correlations' => 'SCALAR', 'with_same' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->nsigmas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->nsigmas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->nsigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->nsigmas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->nsigmas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $nsigmas;
	my $with_correlations = defined $parm{'with_correlations'} ? $parm{'with_correlations'} : 0;
	my $with_same = defined $parm{'with_same'} ? $parm{'with_same'} : 1;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> nsigmas');
# line 969 "lib/model/problem_subs.pm" 

my $prev =undef;
$self->sigmas([]) unless defined $self->sigmas;
foreach my $sigma ( @{$self->sigmas} ) {
  my $size = $sigma -> size;
  my $type = $sigma -> type;
  if ($sigma->same() and (defined $prev) ) {
    $nsigmas += $prev if $with_same;
  } elsif( defined $size ) {

    # If the record has a size, it is of block form with diagonal of
    # length given by $size. The actual values in the model file is
    # then the arithmetic sum: (n*(n+1))/2
    #Kajsa: size also for diagonal matrix! Added check below.

    if( $with_correlations and ($type eq 'BLOCK')){
      $nsigmas += ($size*($size+1))/2;
      $prev = ($size*($size+1))/2;
    } else {
      $nsigmas += $size;
      $prev = $size;
    }
  } elsif (defined $sigma->options) {
    $nsigmas += scalar @{$sigma -> options};
    $prev = scalar @{$sigma -> options};
  } else {
    croak("Failed to parse \$SIGMA." );
  }
}

# line 3258 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> nsigmas');
	# End of Non-Dia code #

	return $nsigmas;
}

sub eta_shrinkage {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model' => 'model', 'probnum' => 'SCALAR', 'eta_filename' => 'SCALAR',
			'directory' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->eta_shrinkage: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->eta_shrinkage: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->eta_shrinkage: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->eta_shrinkage: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->eta_shrinkage: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @eta_shrinkage;
	my $model = $parm{'model'};
	my $probnum = $parm{'probnum'};
	my $eta_filename = $parm{'eta_filename'};
	my $directory = $parm{'directory'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> eta_shrinkage');
# line 1982 "lib/model/problem_subs.pm" 

@eta_shrinkage = @{$self->shrinkage_module -> eta_shrinkage( model => $model, 
								   probnum => $probnum,
								   directory => $directory,
								   eta_filename => $eta_filename) };

# line 3308 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> eta_shrinkage');
	# End of Non-Dia code #

	return \@eta_shrinkage;
}

sub iwres_shrinkage {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model' => 'model', 'iwres_filename' => 'SCALAR',
			'probnum' => 'SCALAR', 'directory' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->iwres_shrinkage: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->iwres_shrinkage: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->iwres_shrinkage: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->iwres_shrinkage: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->iwres_shrinkage: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @iwres_shrinkage;
	my $model = $parm{'model'};
	my $iwres_filename = $parm{'iwres_filename'};
	my $probnum = $parm{'probnum'};
	my $directory = $parm{'directory'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> iwres_shrinkage');
# line 1995 "lib/model/problem_subs.pm" 

    @iwres_shrinkage = @{$self->shrinkage_module -> iwres_shrinkage( model => $model, 
									   probnum => $probnum,
									   directory => $directory,
									   iwres_filename => $iwres_filename)};

# line 3358 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> iwres_shrinkage');
	# End of Non-Dia code #

	return \@iwres_shrinkage;
}

sub add_records {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'record_strings' => 'm_ARRAY', 'type' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->add_records: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->add_records: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->add_records: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->add_records: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->add_records: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @record_strings = defined $parm{'record_strings'} ? @{$parm{'record_strings'}} : ();
	my $type = $parm{'type'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> add_records');
# line 660 "lib/model/problem_subs.pm" 
    {
      # add_records( type => 'subroutine',
      #               record_strings => ['OTHER=get_cov', 'OTHER=read'] )
      # TODO change name from record to records.

      # To read add a record, we figure out what its full class name
      # is. Then we check if we have an accessor for the record type,
      # if we do then the record is valid and we call the appropriate
      # contructor. Both record_strings an type are mandatory.

      my $rec_class = "model::problem::$type";
      my $accessor = $type.'s';
      my $n_previous_rows = 0;
      if ($type eq 'omega'){
				$n_previous_rows = $self->nomegas('with_correlations' => 0,'with_same' => 1);
      }elsif ($type eq 'sigma'){
				$n_previous_rows = $self->nsigmas('with_correlations' => 0,'with_same' => 1);
      }elsif ($type eq 'theta'){
				#this will be with priors
				$n_previous_rows = $self->record_count('record_name' => 'theta');
      }

      if( $self -> can($accessor) ){
				$self->$accessor([]) unless defined $self->$accessor;
				if (($type eq 'omega') or ($type eq 'sigma') or ($type eq 'theta')){
					push( @{$self->$accessor}, $rec_class -> new ( record_arr => \@record_strings,
								n_previous_rows => $n_previous_rows));
				} else {
					push( @{$self->$accessor}, $rec_class -> new ( record_arr => \@record_strings ));
				} 
      } else {
				croak("Trying to add unknown record: $type" );
      }
    }
# line 3432 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> add_records');
	# End of Non-Dia code #

}

sub set_records {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'record_strings' => 'm_ARRAY', 'type' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->set_records: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->set_records: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->set_records: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->set_records: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->set_records: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @record_strings = defined $parm{'record_strings'} ? @{$parm{'record_strings'}} : ();
	my $type = $parm{'type'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> set_records');
# line 1187 "lib/model/problem_subs.pm" 
      {
	  my $rec_class = "model::problem::$type";
	  my $accessor = $type.'s';
	  if( $self -> can($accessor) ){
	    $self->$accessor([$rec_class -> new ( record_arr => \@record_strings) ]);
	  } else {
	      die "Error in problem -> set_records: Trying to set unknown record: $type\n";
	  }
      }
# line 3480 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> set_records');
	# End of Non-Dia code #

}

sub _read_records {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'type' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->_read_records: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->_read_records: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->_read_records: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->_read_records: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->_read_records: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $type = $parm{'type'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_records');
# line 1654 "lib/model/problem_subs.pm" 
    {

      # We parse the lines of a problem by looping over the them and
      # look for records(lines starting with a $). When a record is
      # found we set its index in the array as the end of the previous
      # record we found. We then know which lines to send to the
      # record object constructor. Then we set the end index of the
      # previous record as the start index of the next record. It is
      # assumed that the first record starts at line zero. The end of
      # the last record is the last line.

      my $start_index = 0;
      my $record_index = 0;
      my $end_index;
      my $first = 1;

      # It may look like the loop takes one step to much, but its a
      # trick that helps parsing the last record.
			$self->prob_arr([]) unless defined $self->prob_arr;
      for( my $i = 0; $i <= @{$self->prob_arr}; $i++ ){

	# This if statement makes sure we dont access the array in the
	# last iteration of the loop. In all other iterations we need
	# a line of code to look for records starting lines.

	if( $i <= $#{$self->prob_arr} ){
	  $_ = $self->prob_arr -> [$i];
	}

	# In this if statement we use the lazy evaluation of logical
	# or to make sure we only execute search pattern when we have
	# a line to search. Which is all cases but the very last loop
	# iteration.

	if( $i > $#{$self->prob_arr} or /^\s*\$(\w+)/ ){
	  $end_index = $i;

	  # The if statement here is only necessary in the first loop
	  # iteration. When start_index == end_index == 0 we want to
	  # skip to the next iteration looking for the actual end of
	  # the first record.

	  if( $end_index > $start_index and not $first){
	    # extract lines of code:
	    my @record_lines = @{$self->prob_arr}[$start_index .. $end_index-1];
	    # extract the record name and get its long name:
	    $self->prob_arr -> [$record_index] =~ /^\s*\$(\w+)/;
	    my $record_name = $1;
	    my $record_type = $self -> _normalize_record_name( record_name => $record_name );
	    
	    unless( length($record_type) > 0 ){
	      croak("Record $record_name is not valid" );
	    }

	    # reset the search for records by moving the record start
	    # forwards:
	    $start_index = $i;
	    
	    # let add_records create the object if appropriate

	    if( $record_type eq 'warnings' ) {
	      print "\nWarning: Record \$WARNINGS is deleted by PsN.\n";
	    }elsif( $record_type eq 'finedata' ) {
	      print "\nWarning: Record \$FINEDATA is deleted by PsN.\n";
	    } elsif( $record_type eq 'table' ) {
	      my $et_found = 0;
	      my $wr_found = 0;
	      if (defined $self->shrinkage_module){
		  my $eta_name  = $self->shrinkage_module -> eta_tablename;
		  my $wres_name = $self->shrinkage_module -> wres_tablename;
		  foreach my $row ( @record_lines ) {
		      $et_found++ if( $row =~ /$eta_name/ );
		      $wr_found++ if( $row =~ /$wres_name/ );
		  }
	      }
	      if( $et_found or $wr_found ) {
		$self->shrinkage_module -> enable;
	      } else {
		$self -> add_records( record_strings => \@record_lines, 
				      type => $record_type );
	      }
	    } else {
	      $self -> add_records( record_strings => \@record_lines, 
				    type => $record_type );
	    }
	  }
	  $first = 0;
	  $record_index = $i;
	}  
      }
    }
# line 3609 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_records');
	# End of Non-Dia code #

}

sub get_full_omega {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'covmatrix' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->get_full_omega: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->get_full_omega: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->get_full_omega: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->get_full_omega: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->get_full_omega: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $covmatrix = $parm{'covmatrix'};
	my @new_omega = ();

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> get_full_omega');
# line 1750 "lib/model/problem_subs.pm" 
{
    #create one big full omega block as new_omega->[$row][$col]
    #input is optional covmatrix to be used for 0 off-diags
    #if old off-diagonals not present then set small values to ensure strict diagonal dominance
    my $size = $self->nomegas(with_correlations => 0, with_same => 1);

    for (my $i=0; $i < $size; $i++){
	push(@new_omega,[0 x $size]);
    }
    

    ##Determine minimum difference between off-diagonal absolute sum and diagonal,
    #needed to determine appropriate values to fill in
    #at the same time store current values in matrix @new_omega
    my $minimum_difference;
    my @records;
    if (defined $self -> omegas()) {
	@records = @{$self -> omegas()};
    }
    my @off_diagonal_sum = 0 x $size; 
    my @diagonal_value = 0 x $size; 
    my $block_size;
    my $prev_rows=0;
    foreach my $record (@records){
	if  ($record->same() ){
	    #store values in new_omega
	    my $old_start = $prev_rows - $block_size;
	    for (my $row=0; $row< $block_size; $row++){
		for (my $col=0; $col<=$row; $col++){
		    my $value = $new_omega[$old_start+$row][$old_start+$col];
		    $new_omega[$prev_rows+$row][$prev_rows+$col] = $value;
		}
	    }
	    next; #need not compute sums if same
	}
	unless (defined $record -> options()){
	    croak("OMEGA record has no values");
	}

	if ($record -> type() eq 'BLOCK'){
	    $block_size = $record->size();
	}

	foreach my $option (@{$record -> options()}) {
	    my $name = $option -> coordinate_string();
	    croak("unknown coord $name ") unless ($name =~ /A\((\d+),(\d+)\)/ );
	    croak("row in $name outside size $size") if ($1 > $size );
	    croak("col in $name outside size $size") if ($2 > $size );
	    my $value = $option ->init();
	    $new_omega[($1-1)][($2-1)] = $value;
	    my $val = abs($value);
	    if ($option->on_diagonal()){
		croak("col and row in $name not diagonal element") unless ($2 == $1 );
		$diagonal_value[($1-1)] = $val;
		$prev_rows++;
	    }else{
		$off_diagonal_sum[($1-1)] += $val;
		$off_diagonal_sum[($2-1)] += $val;
	    }
	}
    }

    $minimum_difference = $diagonal_value[0]-$off_diagonal_sum[0];
    for (my $i=1; $i<$size; $i++){
	my $diff = $diagonal_value[$i]-$off_diagonal_sum[$i];
	$minimum_difference = $diff if ($diff< $minimum_difference and ($diff>0));
    }

    my $max_off_diagonal = 0.01; #check Ron's hands on for typical value here
    my $temp = ($minimum_difference/($size-1));
    $max_off_diagonal = $temp*(0.9) if ($temp < $max_off_diagonal);
    #print "max off diag is $max_off_diagonal\n";
    #fill off-diagonals in new_omega
    my $k=1;
    for (my $row=0; $row< $size; $row++){
	for (my $col=0; $col<$row; $col++){
	    if ($new_omega[$row][$col] == 0){
		if (defined $covmatrix){
		    $new_omega[$row][$col] = $covmatrix->[$row][$col];
		}else{
		    $new_omega[$row][$col] = ($max_off_diagonal - 0.0001*($k % 10));
		    $k++;
		}
	    }
	}
    }

    if (0){
	print "prob: printing new_omega\n";
	for (my $row=0; $row< $size; $row++){
	    for (my $col=0; $col<$size; $col++){
		printf("  %.4f",$new_omega[$row][$col]); #matlab format
	    }
	    print "\n";
	}
	print "\n";
    }
}
# line 3746 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> get_full_omega');
	# End of Non-Dia code #

	return \@new_omega;
}

sub add_omega_block {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'new_omega' => 'REF', 'labels' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->add_omega_block: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->add_omega_block: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->add_omega_block: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->add_omega_block: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->add_omega_block: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $new_omega = $parm{'new_omega'};
	my $labels = $parm{'labels'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> add_omega_block');
# line 1851 "lib/model/problem_subs.pm" 
{
    #input is $new_omega
    #
    # add new BLOCK(size)

    my $size = scalar(@{$new_omega});
    return if ($size < 1);
    my @record_lines=();
    push(@record_lines,'BLOCK('.$size.') ');
    for (my $row=0; $row< $size; $row++){
	my $line;
	for (my $col=0; $col<=$row; $col++){
	    my $str= sprintf("  %.6f",$new_omega->[$row][$col]);
	    $line = $line.' '.$str;
	}
	my $comment ='';
	$comment = '; '.$labels->[$row] if (defined $labels and scalar(@{$labels}) > $row);
	push(@record_lines,$line.$comment);
    }
    $self -> add_records( record_strings => \@record_lines, 
			  type => 'omega' );

}
# line 3809 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> add_omega_block');
	# End of Non-Dia code #

}

sub _format_problem {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'filename' => 'SCALAR', 'problem_number' => 'SCALAR',
			'number_format' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->_format_problem: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->_format_problem: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->_format_problem: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->_format_problem: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->_format_problem: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $filename = $parm{'filename'};
	my $problem_number = $parm{'problem_number'};
	my $number_format = $parm{'number_format'};
	my @formatted;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _format_problem');
# line 1324 "lib/model/problem_subs.pm" 
      {
	# problem::_format_problem()

	# format_problem will return an array of strings of the
	# problem in NONMEM modelfile format.

	# Loop over the print_order array that contains strings of
	# valid record types in the order they should appear in a
	# NONMEM modelfile. So if the order of some records are
	# interchangable and the file from which the object was
	# initialized has records in an order different from
	# print_order, the file will still be valid, but will look
	# different from what it used to.
	my $record_order = \@print_order;
	if ($self->sde){
	  $record_order = \@sde_print_order;
	}elsif ($self->omega_before_pk){
	  $record_order = \@print_order_omega_before_pk;
	}
	foreach my $type ( @${record_order} ) {
	  # Create an accessor string for the record being formatted
	  my $accessor = $type.'s';

	  # Se if we have one or more records of the type given in
	  # print_order
	  if ( defined $self->$accessor ) {
	    # Loop over all such records and call on the record object
	    # to format itself.

	    foreach my $record ( @{$self->$accessor} ){
	      push( @formatted,
		    @{$record ->
			  _format_record( number_format => $number_format,
					  nonparametric_code => $self->nonparametric_code,
					  shrinkage_code     => $self->shrinkage_code,
					  eigen_value_code   => $self->eigen_value_code ) } );
	    }
	  }
	  if( $self->shrinkage_module -> enabled and $type eq 'table' ) {
	    push( @formatted,
		  @{$self->shrinkage_module -> format_shrinkage_tables } );
	  }	
	}

	if( $self->cwres_modules ){
	  $self->cwres_modules -> [0] -> post_process;
	}
	
      }
# line 3900 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _format_problem');
	# End of Non-Dia code #

	return \@formatted;
}

sub ensure_diagonal_dominance {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'verbose' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->ensure_diagonal_dominance: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->ensure_diagonal_dominance: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->ensure_diagonal_dominance: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->ensure_diagonal_dominance: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->ensure_diagonal_dominance: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $verbose = defined $parm{'verbose'} ? $parm{'verbose'} : 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> ensure_diagonal_dominance');
# line 1068 "lib/model/problem_subs.pm" 
      {

	#check here that omega and sigma strictly diagonally dominant
	#otherwise decrease magnitude of diagonal elements 
	# by enough to achieve strict diagonal dominance
	# do row by row
	#check number of diagonal elements
	#make array of zeros for off_diagonal_sums, one per diagonal value
	#go through records
	#check that block, and not fixed or same
	#need only check values wihtin block, row sums not affected by anything outside
	#go through all options, if on-diagonal then skip. get row, col indexes
	#add value  to sum for diagonal(row) and diagonal (col)
	#loop through options again, if not on-diagonal, skip.
	#if on-diagonal, check that value strictly greater than sum.
	  #otherwise compute deflation factor

	#dont touch priors
	foreach my $param ('omega','sigma') {
	  my $adjusted = 0;
	  my $accessor = $param.'s';
	  my $size_accessor = 'n'.$param.'s';
	  my @records;
	  if (defined $self -> $accessor()) {
	    @records = @{$self -> $accessor()};
	  }
	  next unless (scalar(@records) > 0); #no parameter in this problem
	  my $size = $self->$size_accessor('with_correlations' => 0,'with_same' => 1);
	  my @off_diagonal_sum = 0 x $size; 
	  foreach my $record (@records){
	    next unless ($record -> type() eq 'BLOCK');
	    if  ($record->same() or $record->fix() or $record->prior()){
	      next;
	    }
	    next if ($record->size() < 2);
	    unless (defined $record -> options()){
	      croak("$param record has no values");
	    }
	    foreach my $option (@{$record -> options()}) {
	      next if ($option->on_diagonal());
	      my $name = $option -> coordinate_string();
	      croak("unknown coord $name ") unless ($name =~ /A\((\d+),(\d+)\)/ );
	      croak("row in $name outside size $size") if ($1 > $size );
	      croak("col in $name outside size $size") if ($2 > $size );
	      my $val = abs($option ->init());
	      $off_diagonal_sum[($1-1)] += $val;
	      $off_diagonal_sum[($2-1)] += $val;
	    }
	    my %adjust_row = {};
	    foreach my $option (@{$record -> options()}) {
	      next unless ($option->on_diagonal());
	      my $name = $option -> coordinate_string();
	      croak("unknown coord $name ") unless ($name =~ /A\((\d+),(\d+)\)/ );
	      croak("row in $name outside size $size") if ($1 > $size );
	      croak("col and row in $name not diagonal element") unless ($2 == $1 );
	      my $val = $option ->init();
	      unless ($val > $off_diagonal_sum[($1-1)] ){
		  my $ratio = $val/$off_diagonal_sum[($1-1)]; # less than 1, larger than 0 (abs sum, pos diag)
		  $adjust_row{$1} = $ratio*(0.99);
		  #skip inflation
		  if (0){
		      my $new_val = 1.05*$off_diagonal_sum[($1-1)]; #five percent larger than sum
		      $option -> check_and_set_init( new_value => $new_val );
		  }
	      }
	    }
	    #new loop here to decrease off-diag
	    if (1){
		foreach my $option (@{$record -> options()}) {
		    next if ($option->on_diagonal());
		    my $name = $option -> coordinate_string();
		    croak("unknown coord $name ") unless ($name =~ /A\((\d+),(\d+)\)/ );
		    croak("row in $name outside size $size") if ($1 > $size );
		    croak("col in $name outside size $size") if ($2 > $size );
		    my $deflate = 1;
		    foreach my $row (keys %adjust_row){
			if ($row == $1 or $row == $2){
			    $deflate = $adjust_row{$row} if ($adjust_row{$row} < $deflate);
			}
		    }
		    next unless ($deflate < 1);
		    my $val = $option ->init();
		    my $value = $val*$deflate;
		    if ($value < 1 and $value > 0){
			$value = sprintf "%.5f", $value; #need to control so dont get e notation
			$value     = '0' if eval($value) == 0;
		    }elsif ($value > -1 and $value < 0){
			$value = sprintf "%.4f", $value; #need to control so dont get e notation
			$value     = '0' if eval($value) == 0;
		    }else{
			$value = sprintf "%6.2f", $value; #need to control so dont get e notation
			my ($big,$small) = split('\.',$value);
			$small           = substr($small,0,3);
			if ((length($big)+ length($small)) > 7){
			    $value = $big;
			}else{
			    $value     = $big.'.'.$small;
			}
			$value     = '0' if eval($value) == 0;
		    }
		    
		    $adjusted = 1;
		    $option -> check_and_set_init( new_value => $value );
		    
		}
	    }
	  }
	  if ($adjusted and $verbose){
	      print "Decreased off-diagonal values of $param from input to ensure strict diagonal dominance in output model.\n";
	  }
	}
      }
# line 4051 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> ensure_diagonal_dominance');
	# End of Non-Dia code #

}

sub update_prior_information {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->update_prior_information: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->update_prior_information: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->update_prior_information: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->update_prior_information: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->update_prior_information: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> update_prior_information');
# line 268 "lib/model/problem_subs.pm" 
    {
      #if $PRIOR NWPRI then get NTHETA and NETA
      #loop through $THETA and $OMEGA and set prior=1 for 
      #all params after the estimated ones


      if ((defined $self->priors()) and scalar(@{$self -> priors()})>0 ){
	my $nwpri=0;
	foreach my $rec (@{$self -> priors()}){
	  unless ((defined $rec) &&( defined $rec -> options )){
	    carp("No options for rec \$PRIOR" );
	  }
	  foreach my $option ( @{$rec -> options} ) {
	    if ((defined $option) and 
		(($option->name eq 'NWPRI') || (index('NWPRI',$option ->name ) == 0))){
	      $nwpri=1;
	    }
	  }
	}
	if ($nwpri){
	  my $neta;
	  my $ntheta;
	  foreach my $rec (@{$self -> priors()}){
	    foreach my $option ( @{$rec -> options} ) {
	      if (defined $option){
		if  ($option->name eq 'NETA'){ #NONMEM does not allow abbrev
		  if ( (defined $option->value) and ($option->value ne '')){
		    #now we must split on ,
		    my @rest;
		    ($neta,@rest) = split(',',$option->value);
		  }
		}elsif  ($option->name eq 'NTHETA'){ #NONMEM does not allow abbrev
		  if ( (defined $option->value) and ($option->value ne '')){
		    #now we must split on ,
		    my @rest;
		    ($ntheta,@rest) = split(',',$option->value);
		  }
		}
		if ( (defined $option->value) and ($option->value ne '')){
		  if ($option->value =~  /NETA/){
		    my @opts = split(',',$option->value);
		    foreach my $opt (@opts){
		      if ($opt =~ /^NETA=([0-9]+)$/){
			$neta = $1;
		      }
		    }
		  }
		  if ($option->value =~  /NTHETA/){
		    my @opts = split(',',$option->value);
		    foreach my $opt (@opts){
		      if ($opt =~ /^NTHETA=([0-9]+)$/){
			$ntheta = $1;
		      }
		    }
		  }
		}
	      }
	    }
	  }
	  unless (defined $neta){
	    print "\nWarning: Did not find NETA in \$PRIOR\n";
	    $neta=0;
	  }
	  unless (defined $ntheta){
	    print "\nWarning: Did not find NTHETA in \$PRIOR\n";
	    $ntheta=0;
	  }
	  $self->nwpri_ntheta($ntheta);
	  $self->nwpri_neta($neta);
	  
	  #set prior in params
	  if( defined $self->thetas ){
	    my $counter=0;
	    foreach my $record ( @{$self->thetas} ){
	      if( defined $record -> options ){
		foreach my $opt( @{$record -> options}){
		  $counter++;
		  if ($counter > $ntheta){
		    #set prior
		    $opt->prior(1);
		  }
		}
	      }
	    }
	  }
	  if( defined $self->omegas ){
	    my $prevdiag =undef;
	    my $ndiag=0;
	    my $exact=0;
	    foreach my $omega ( @{$self->omegas} ) {
	      my $size = $omega -> size;
	      my $type = $omega -> type;
	      
	      #assume do not start priors in the middle of a block.
	      #Only check after each new record if reached $neta
	      if ($omega->same()) {
		croak("First \$OMEGA cannot be SAME")
		    unless (defined $prevdiag);
		$ndiag += $prevdiag;
	      } elsif( defined $size ) {
		$ndiag += $size;
		$prevdiag = $size;
	      } elsif (defined $omega->options) {
		$ndiag += scalar @{$omega -> options};
		$prevdiag = scalar @{$omega -> options};
	      } else {
		croak("Failed to parse \$OMEGA." );
	      }
	      if ($ndiag == $neta){
		$exact = 1;
	      }elsif ($ndiag > $neta){
		croak("It seems that one \$OMEGA record contains both ".
			   "estimated and prior OMEGAs. PsN requires that this ".
			   "\$OMEGA is split into two so that priors are separate.")
		    unless ($exact);
		$omega->prior(1);
	      }
	    }
	  }
	}
      }

    }
# line 4211 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> update_prior_information');
	# End of Non-Dia code #

}

sub tbs_transform {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( '' => '' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->tbs_transform: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->tbs_transform: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->tbs_transform: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->tbs_transform: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->tbs_transform: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}


	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> tbs_transform');
# line 134 "lib/model/problem_subs.pm" 
    {
      if ((defined $self->priors()) and scalar(@{$self -> priors()})>0 ){
	croak("The current version of PsN does not support \$PRIOR and ".
	    "option -tbs in combination");
      }      

      my $newthetanum;
      $newthetanum=$self -> record_count( record_name => 'theta' )+1;
      $self->tbs_thetanum($newthetanum);
      if (defined $self->tbs_param()){
	$self->add_records( type => 'theta',
			    record_strings => [$self->tbs_param().' ; tbs_lambda'] );
      }else{
	$self->add_records( type => 'theta',
			    record_strings => ['1 ; tbs_lambda'] );
      }
      #PRED or ERROR
      my @code;
      @code = @{$self -> errors()->[0]->code()} if (defined $self -> errors());
      my $use_pred = 0;
      unless ( $#code > 0 ) {
	@code = @{$self -> preds()->[0]->code()} if (defined $self -> preds());
	$use_pred = 1;
      }
      if ( $#code <= 0 ) {
	croak("Neither ERROR or PRED defined, cannot use -tbs\n" );
      }

      #locate first use IWRES in right-hand-side, same for W, IPRED. Check Uppsala style
      #IF ELSE around IPRED, IWRES and Y

      my $found=0;
      for (my $i=$#code; $i>=0;$i--){
	if (($code[$i] =~ /[^a-zA-Z_0-9]Y\s*=/) or
	    ($code[$i] =~ /^Y\s*=/)){
	  if ($found){
	    croak("Cannot handle multiple-line definitions of Y with option -tbs.");
	  }else{
	    #remove Y line
	    @code =  (@code[0..$i-1],
		      @code[$i+1..$#code]);
	    $found=1;
	  }
	}
      }
      croak("Failed to find Y definition row in \$PK/\$ERROR") unless ($found);

      my $ipredrow=0;
      for (my $i=$#code; $i>=0;$i--){
	if (($code[$i] =~ /[^a-zA-Z_0-9]IPRED\s*=/) or
	    ($code[$i] =~ /^IPRED\s*=/)){
	  @code =  (@code[0..$i],
		    " LAMBDA = THETA($newthetanum)\n",
		    " IPRTR=IPRED\n",
		    " IF (LAMBDA .NE. 0 .AND. IPRED .NE.0) THEN\n",
		    "    IPRTR=(IPRED**LAMBDA-1)/LAMBDA\n",
		    " ENDIF\n",
		    " IF (LAMBDA .EQ. 0 .AND. IPRED .NE.0) THEN\n",
		    "    IPRTR=LOG(IPRED)\n",
		    " ENDIF\n",
		    " IF (LAMBDA .NE. 0 .AND. IPRED .EQ.0) THEN\n",
		    "    IPRTR=-1/LAMBDA\n",
		    " ENDIF\n",
		    " IF (LAMBDA .EQ. 0 .AND. IPRED .EQ.0) THEN\n",
		    "    IPRTR=-1000000000\n",
		    " ENDIF\n",
		    " IPRED=IPRTR\n",
		    @code[$i+1..$#code]);
	  $ipredrow = $i+14;
	  $found=1;
	  last;
	}
      }
      croak("Failed to find IPRED definition row in \$PK/\$ERROR. ".
		 "Do not know where in code to add IPRED transformation.") unless ($found);
      $found=0;
      for (my $i=$#code; $i>=0;$i--){
	if (($code[$i] =~ /[^a-zA-Z_0-9]IWRES\s*=/) or
	    ($code[$i] =~ /^IWRES\s*=/)){
	  my $row=$i;
	  $row=$ipredrow if ($ipredrow > $i);
	  @code =  (@code[0..$row],
		    " IWRTR=IWRES\n",
		    " IF (LAMBDA.NE.0 .AND. DV.NE.0 .AND. W.NE.0) THEN\n",
		    "    IWRTR=((DV**LAMBDA-1)/LAMBDA-IPRED)/W\n",
		    " ENDIF\n",
		    " IF (LAMBDA.EQ.0 .AND. DV.NE.0 .AND. W.NE.0) THEN\n",
		    "    IWRTR=(LOG(DV)-IPRED)/W\n",
		    " ENDIF\n",
		    " IF (LAMBDA.NE.0 .AND. DV.EQ.0 .AND. W.NE.0) THEN\n",
		    "    IWRTR=(-1/LAMBDA-IPRED)/W\n",
		    " ENDIF\n",
		    " IF (LAMBDA.EQ.0 .AND. DV.EQ.0 .AND. W.NE.0) THEN\n",
		    "    IWRTR=(-1000000000-IPRED)/W\n",
		    " ENDIF\n",
		    " IWRES=IWRTR\n",
		    " Y=IPRED+EPS(1)*W\n",
		    @code[$row+1..$#code]);
	  $found=1;
	  last;
	}
      }
      croak("Failed to find IWRES definition row in \$PK/\$ERROR. ".
		 "Do not know where in code to add IWRES transformation.") unless ($found);

      if ( $use_pred ) {
	$self -> preds ->[0] ->code (\@code );
      } else {
	$self -> errors ->[0] ->code (\@code );
      }

      #$SUBS. check NM major version. Option to modelfit so do not need to print copy 
      # everywhere?
      #need to copy ccontr etc also.

      $self-> add_option(option_name  => 'CONTR',
			 option_value => 'contr.txt',
			 record_name => 'subroutine',
			 add_record => 1,
			 record_number => -1);
      
      my $val = 'ccontra_nm7.txt';
      if ($PsN::nm_major_version < 7){
	$val = 'ccontra_nm6.txt';
      }
      $self-> add_option(option_name  => 'CCONTR',
			 option_value => $val,
			 record_name => 'subroutine',
			 record_number => -1);

    }
# line 4379 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> tbs_transform');
	# End of Non-Dia code #

}

sub _normalize_record_name {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'record_name' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem->_normalize_record_name: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem->_normalize_record_name: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem->_normalize_record_name: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->_normalize_record_name: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem->_normalize_record_name: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $record_name = $parm{'record_name'};
	my $normalized_name;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _normalize_record_name');
# line 1616 "lib/model/problem_subs.pm" 
    {

      # This code takes a recordname (which likely is uppercase and
      # semilong), creates its short uppercase format and looks up the
      # long, lowercase, name in the abbreviations hash that was
      # initialized in "new". The name is assumed to be valid, if its
      # not, an empty string will be returned, but no error produced (
      # a warning might be nice though ) (Errorhandling is now done in
      # "read_records".

	if ($unsupported_records{uc($record_name)} > 0){
	    debug->die(message => "\nPsN does not yet support record \$".$record_name." in the control stream, but adding support is on the todo-list.\n");
	}
      my $uc_short_type = substr(uc($record_name),0,3);
      if ($uc_short_type eq 'AES'){
	  if (length($record_name)>3){
	      #this must be aesinitial
	      $uc_short_type = $uc_short_type.'I';
	  }else{
	      #must be aes
	      $uc_short_type = $uc_short_type.' ' ;
	  }
      }
      $normalized_name = $abbreviations{$uc_short_type};
      unless (length($normalized_name)>0){
	  debug->die(message => "\nPsN does not support record \$".$record_name." in the control stream\n");

      }

#      print "normal $normalized_name short $uc_short_type inputname $record_name\n";
    }
# line 4449 libgen/model/problem.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _normalize_record_name');
	# End of Non-Dia code #

	return $normalized_name;
}

1;

