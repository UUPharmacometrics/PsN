use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package model::problem;

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
	# End of Non-Dia code #

}

sub store_inits {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub restore_inits {
	my $self = shift;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

}

sub header {
	my $self = shift;
	my @header;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@header;
}

sub dropped_columns {
	my $self = shift;
	my @dropped_columns;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@dropped_columns;
}

sub drop_dropped {
	my $self = shift;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

	return $normalized_name;
}

1;

