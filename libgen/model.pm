use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package model;

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
	# End of Non-Dia code #

	return \@nomegas;
}

sub nproblems {
	my $self = shift;
	my $number_of_problem;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

	return \@pred;
}

sub print {
	my $self = shift;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

	return $return_value;
}

sub restore_inits {
	my $self = shift;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

}

sub store_inits {
	my $self = shift;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

	return $found;
}

sub is_run {
	my $self = shift;
	my $return_value = 0;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

	return \@file_names;
}

sub output_files {
	my $self = shift;
	my @file_names;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

	return \@table_files;
}

sub full_name {
	my $self = shift;
	my $full_name;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

	return $is_est;
}

sub subroutine_files {
	my $self = shift;
	my @fsubs;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

}

sub flush_data {
	my $self = shift;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

}

sub problem_structure {
	my $self = shift;
	my @subproblems;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@subproblems;
}

sub add_nonparametric_code {
	my $self = shift;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

}

sub _read_problems {
	my $self = shift;

	# Start of Non-Dia code #
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
	# End of Non-Dia code #

	return \@parameter_values;
}

1;

