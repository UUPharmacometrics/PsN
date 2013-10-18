use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package tool::modelfit;

#---------------------------------------------------------------------
#         Inherited Class Packages
#---------------------------------------------------------------------
use base qw(tool);

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my %superParms;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'data_path' => 'SCALAR', 'tail_output' => 'SCALAR',
			'full_path_nmtran' => 'SCALAR', 'nmtran_error_file' => 'SCALAR',
			'base_msfo_name' => 'SCALAR', 'max_hash' => 'REF',
			'full_path_nmfe' => 'SCALAR', 'wintail_exe' => 'SCALAR',
			'wintail_command' => 'SCALAR', 'cutoff' => 'SCALAR',
			'cutoff_thetas' => 'ARRAY', 'cut_thetas_rounding_errors' => 'SCALAR',
			'cut_thetas_maxevals' => 'SCALAR', 'handle_hessian_npd' => 'SCALAR',
			'logfile' => 'REF', '_raw_results_callback' => '' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
	}
	foreach my $givenp ( keys %parm ) {
		$superParms{$givenp} = $parm{$givenp} and next unless( defined $valid_parm{$givenp});

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'tail_output'} = defined $parm{'tail_output'} ? $parm{'tail_output'} : 0 unless defined $this -> {'tail_output'};
	$this -> {'nmtran_error_file'} = defined $parm{'nmtran_error_file'} ? $parm{'nmtran_error_file'} : 'nmtran_error.txt' unless defined $this -> {'nmtran_error_file'};
	$this -> {'max_hash'} = defined $parm{'max_hash'} ? $parm{'max_hash'} : {} unless defined $this -> {'max_hash'};
	$this -> {'cut_thetas_rounding_errors'} = defined $parm{'cut_thetas_rounding_errors'} ? $parm{'cut_thetas_rounding_errors'} : 0 unless defined $this -> {'cut_thetas_rounding_errors'};
	$this -> {'cut_thetas_maxevals'} = defined $parm{'cut_thetas_maxevals'} ? $parm{'cut_thetas_maxevals'} : 0 unless defined $this -> {'cut_thetas_maxevals'};
	$this -> {'handle_hessian_npd'} = defined $parm{'handle_hessian_npd'} ? $parm{'handle_hessian_npd'} : 0 unless defined $this -> {'handle_hessian_npd'};
	$this -> {'logfile'} = defined $parm{'logfile'} ? $parm{'logfile'} : ['modelfit.log'] unless defined $this -> {'logfile'};

	bless $this, $class;
	tool::new($this,%superParms);

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $this;
};

sub data_path {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'data_path'} = $parm;
	} else {
		return $self -> {'data_path'};
	}
}

sub tail_output {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'tail_output'} = $parm;
	} else {
		return $self -> {'tail_output'};
	}
}

sub full_path_nmtran {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'full_path_nmtran'} = $parm;
	} else {
		return $self -> {'full_path_nmtran'};
	}
}

sub nmtran_error_file {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'nmtran_error_file'} = $parm;
	} else {
		return $self -> {'nmtran_error_file'};
	}
}

sub base_msfo_name {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'base_msfo_name'} = $parm;
	} else {
		return $self -> {'base_msfo_name'};
	}
}

sub max_hash {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'max_hash'} = $parm;
	} else {
		return $self -> {'max_hash'};
	}
}

sub full_path_nmfe {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'full_path_nmfe'} = $parm;
	} else {
		return $self -> {'full_path_nmfe'};
	}
}

sub wintail_exe {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'wintail_exe'} = $parm;
	} else {
		return $self -> {'wintail_exe'};
	}
}

sub wintail_command {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'wintail_command'} = $parm;
	} else {
		return $self -> {'wintail_command'};
	}
}

sub cutoff {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'cutoff'} = $parm;
	} else {
		return $self -> {'cutoff'};
	}
}

sub cutoff_thetas {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'cutoff_thetas'} = $parm;
	} else {
		return $self -> {'cutoff_thetas'};
	}
}

sub cut_thetas_rounding_errors {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'cut_thetas_rounding_errors'} = $parm;
	} else {
		return $self -> {'cut_thetas_rounding_errors'};
	}
}

sub cut_thetas_maxevals {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'cut_thetas_maxevals'} = $parm;
	} else {
		return $self -> {'cut_thetas_maxevals'};
	}
}

sub handle_hessian_npd {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'handle_hessian_npd'} = $parm;
	} else {
		return $self -> {'handle_hessian_npd'};
	}
}

sub logfile {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'logfile'} = $parm;
	} else {
		return $self -> {'logfile'};
	}
}

sub _raw_results_callback {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'_raw_results_callback'} = $parm;
	} else {
		return $self -> {'_raw_results_callback'};
	}
}

sub run {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'resuming' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->run: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->run: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->run: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->run: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->run: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $resuming = defined $parm{'resuming'} ? $parm{'resuming'} : 0;
	my @results;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@results;
}

sub select_best_model {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'run_no' => 'SCALAR', 'nm_version' => 'SCALAR',
			'queue_info' => 'm_HASH' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->select_best_model: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->select_best_model: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->select_best_model: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->select_best_model: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->select_best_model: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $run_no = $parm{'run_no'};
	my $nm_version = $parm{'nm_version'};
	my %queue_info = defined $parm{'queue_info'} ? %{$parm{'queue_info'}} : {};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub summarize {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub get_retry_name {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'retry' => 'SCALAR', 'crash' => 'SCALAR', 'filename' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->get_retry_name: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->get_retry_name: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->get_retry_name: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->get_retry_name: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->get_retry_name: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $retry = $parm{'retry'};
	my $crash = $parm{'crash'};
	my $filename = $parm{'filename'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $filename;
}

sub set_msfo_to_msfi {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'candidate_model' => 'model', 'retry' => 'SCALAR',
			'queue_info' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->set_msfo_to_msfi: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->set_msfo_to_msfi: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->set_msfo_to_msfi: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->set_msfo_to_msfi: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->set_msfo_to_msfi: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $candidate_model = $parm{'candidate_model'};
	my $retry = $parm{'retry'};
	my $queue_info = $parm{'queue_info'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub reset_msfo {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'candidate_model' => 'model', 'basic_model' => 'model' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->reset_msfo: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->reset_msfo: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->reset_msfo: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->reset_msfo: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->reset_msfo: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $candidate_model = $parm{'candidate_model'};
	my $basic_model = $parm{'basic_model'};
	my $model_modified = 0;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $model_modified;
}

sub cut_thetas {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'candidate_model' => 'model', 'cutoff_thetas' => 'ARRAY',
			'output_file' => 'output' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->cut_thetas: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->cut_thetas: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->cut_thetas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->cut_thetas: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->cut_thetas: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $candidate_model = $parm{'candidate_model'};
	my @cutoff_thetas = defined $parm{'cutoff_thetas'} ? @{$parm{'cutoff_thetas'}} : ();
	my $output_file = $parm{'output_file'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub prepare_raw_results {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model_number' => 'SCALAR', 'header' => 'ARRAY',
			'models' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->prepare_raw_results: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->prepare_raw_results: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->prepare_raw_results: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->prepare_raw_results: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->prepare_raw_results: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model_number = $parm{'model_number'};
	my @header = defined $parm{'header'} ? @{$parm{'header'}} : ();
	my @models = defined $parm{'models'} ? @{$parm{'models'}} : ();
	my @new_header;
	my @raw_results;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \@new_header ,\@raw_results;
}

sub print_raw_results {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub _get_run_options {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'run_id' => 'SCALAR', 'work_dir' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->_get_run_options: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->_get_run_options: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->_get_run_options: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->_get_run_options: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->_get_run_options: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $run_id = $parm{'run_id'};
my %options_hash;
	my $work_dir = $parm{'work_dir'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return \%options_hash;
}

sub ud_submit {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model' => 'm_model' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->ud_submit: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->ud_submit: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->ud_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->ud_submit: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->ud_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model = $parm{'model'};
	my $jobId = -1;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $jobId;
}

sub ud_monitor {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'jobId' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->ud_monitor: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->ud_monitor: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->ud_monitor: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->ud_monitor: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->ud_monitor: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $jobId = $parm{'jobId'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub ud_retrieve {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'jobId' => 'SCALAR', 'run_no' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->ud_retrieve: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->ud_retrieve: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->ud_retrieve: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->ud_retrieve: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->ud_retrieve: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $jobId = $parm{'jobId'};
	my $run_no = $parm{'run_no'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub ud_retrieve2 {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'jobId' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->ud_retrieve2: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->ud_retrieve2: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->ud_retrieve2: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->ud_retrieve2: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->ud_retrieve2: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $jobId = $parm{'jobId'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub sge_submit {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model' => 'model', 'nm_version' => 'SCALAR',
			'queue_info' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->sge_submit: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->sge_submit: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->sge_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->sge_submit: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->sge_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $jobId = -1;
	my $model = $parm{'model'};
	my $nm_version = $parm{'nm_version'};
	my $queue_info = $parm{'queue_info'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $jobId;
}

sub sge_nmfe_submit {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model' => 'model', 'nm_version' => 'SCALAR',
			'nodes' => 'SCALAR', 'queue_info' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->sge_nmfe_submit: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->sge_nmfe_submit: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->sge_nmfe_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->sge_nmfe_submit: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->sge_nmfe_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $jobId = -1;
	my $model = $parm{'model'};
	my $nm_version = $parm{'nm_version'};
	my $nodes = defined $parm{'nodes'} ? $parm{'nodes'} : 0;
	my $queue_info = $parm{'queue_info'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $jobId;
}

sub lsf_nmfe_submit {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model' => 'model', 'nm_version' => 'SCALAR',
			'nodes' => 'SCALAR', 'queue_info' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->lsf_nmfe_submit: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->lsf_nmfe_submit: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->lsf_nmfe_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->lsf_nmfe_submit: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->lsf_nmfe_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $jobId = -1;
	my $model = $parm{'model'};
	my $nm_version = $parm{'nm_version'};
	my $nodes = defined $parm{'nodes'} ? $parm{'nodes'} : 0;
	my $queue_info = $parm{'queue_info'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $jobId;
}

sub sge_monitor {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'jobId' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->sge_monitor: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->sge_monitor: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->sge_monitor: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->sge_monitor: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->sge_monitor: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $jobId = $parm{'jobId'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub sge_nmfe_monitor {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'jobId' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->sge_nmfe_monitor: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->sge_nmfe_monitor: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->sge_nmfe_monitor: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->sge_nmfe_monitor: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->sge_nmfe_monitor: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $jobId = $parm{'jobId'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub lsf_nmfe_monitor {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'jobId' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->lsf_nmfe_monitor: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->lsf_nmfe_monitor: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->lsf_nmfe_monitor: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->lsf_nmfe_monitor: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->lsf_nmfe_monitor: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $jobId = $parm{'jobId'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub slurm_submit {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model' => 'model', 'nm_version' => 'SCALAR',
			'nodes' => 'SCALAR', 'queue_info' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->slurm_submit: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->slurm_submit: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->slurm_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->slurm_submit: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->slurm_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $jobId = -1;
	my $model = $parm{'model'};
	my $nm_version = $parm{'nm_version'};
	my $nodes = defined $parm{'nodes'} ? $parm{'nodes'} : 0;
	my $queue_info = $parm{'queue_info'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $jobId;
}

sub nmfe_setup {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'nm_version' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->nmfe_setup: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->nmfe_setup: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->nmfe_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->nmfe_setup: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->nmfe_setup: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $nm_version = $parm{'nm_version'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub slurm_monitor {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'jobId' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->slurm_monitor: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->slurm_monitor: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->slurm_monitor: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->slurm_monitor: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->slurm_monitor: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $jobId = $parm{'jobId'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub zink_submit {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model' => 'model', 'nm_version' => 'SCALAR',
			'queue_info' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->zink_submit: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->zink_submit: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->zink_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->zink_submit: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->zink_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $jobId = -1;
	my $model = $parm{'model'};
	my $nm_version = $parm{'nm_version'};
	my $queue_info = $parm{'queue_info'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $jobId;
}

sub zink_monitor {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'jobId' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->zink_monitor: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->zink_monitor: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->zink_monitor: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->zink_monitor: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->zink_monitor: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $jobId = $parm{'jobId'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub lsf_monitor {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'jobId' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->lsf_monitor: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->lsf_monitor: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->lsf_monitor: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->lsf_monitor: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->lsf_monitor: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $jobId = $parm{'jobId'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub torque_submit {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model' => 'model', 'nm_version' => 'SCALAR',
			'queue_info' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->torque_submit: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->torque_submit: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->torque_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->torque_submit: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->torque_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $jobId = -1;
	my $model = $parm{'model'};
	my $nm_version = $parm{'nm_version'};
	my $queue_info = $parm{'queue_info'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $jobId;
}

sub torque_monitor {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'jobId' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->torque_monitor: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->torque_monitor: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->torque_monitor: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->torque_monitor: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->torque_monitor: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $jobId = $parm{'jobId'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub run_nonmem {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'nm_version' => 'SCALAR', 'queue_info' => 'REF',
			'run_no' => 'm_SCALAR', 'queue_map' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->run_nonmem: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->run_nonmem: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->run_nonmem: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->run_nonmem: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->run_nonmem: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $nm_version = $parm{'nm_version'};
	my $queue_info = $parm{'queue_info'};
	my $run_no = $parm{'run_no'};
	my $queue_map = $parm{'queue_map'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub restart_needed {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'retries' => 'SCALAR', 'cut_thetas_rounding_errors' => 'SCALAR',
			'cut_thetas_maxevals' => 'SCALAR', 'handle_hessian_npd' => 'SCALAR',
			'tweak_inits' => 'SCALAR', 'maxevals' => 'SCALAR',
			'picky' => 'SCALAR', 'cutoff_thetas' => 'ARRAY',
			'significant_digits_accept' => 'SCALAR', 'run_no' => 'SCALAR',
			'nm_version' => 'SCALAR', 'queue_info' => 'm_HASH' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->restart_needed: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->restart_needed: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->restart_needed: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->restart_needed: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->restart_needed: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $retries = $parm{'retries'};
	my $marked_for_rerun = 0;
	my $cut_thetas_rounding_errors = defined $parm{'cut_thetas_rounding_errors'} ? $parm{'cut_thetas_rounding_errors'} : 0;
	my $cut_thetas_maxevals = defined $parm{'cut_thetas_maxevals'} ? $parm{'cut_thetas_maxevals'} : 0;
	my $handle_hessian_npd = defined $parm{'handle_hessian_npd'} ? $parm{'handle_hessian_npd'} : 0;
	my $tweak_inits = defined $parm{'tweak_inits'} ? $parm{'tweak_inits'} : 1;
	my $maxevals = defined $parm{'maxevals'} ? $parm{'maxevals'} : 0;
	my $picky = defined $parm{'picky'} ? $parm{'picky'} : 0;
	my @cutoff_thetas = defined $parm{'cutoff_thetas'} ? @{$parm{'cutoff_thetas'}} : ();
	my $significant_digits_accept = $parm{'significant_digits_accept'};
	my $run_no = $parm{'run_no'};
	my $nm_version = $parm{'nm_version'};
	my %queue_info = defined $parm{'queue_info'} ? %{$parm{'queue_info'}} : {};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $marked_for_rerun;
}

sub compute_iofv {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'run_no' => 'SCALAR', 'queue_info' => 'm_HASH' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->compute_iofv: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->compute_iofv: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->compute_iofv: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->compute_iofv: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->compute_iofv: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $run_no = $parm{'run_no'};
	my %queue_info = defined $parm{'queue_info'} ? %{$parm{'queue_info'}} : {};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub run_nmtran {
	my $self = shift;
	my $ok;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $ok;
}

sub compute_cwres {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'run_no' => 'SCALAR', 'queue_info' => 'm_HASH' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->compute_cwres: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->compute_cwres: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->compute_cwres: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->compute_cwres: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->compute_cwres: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $run_no = $parm{'run_no'};
	my %queue_info = defined $parm{'queue_info'} ? %{$parm{'queue_info'}} : {};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub copy_model_and_input {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model' => 'm_model', 'source' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->copy_model_and_input: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->copy_model_and_input: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->copy_model_and_input: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->copy_model_and_input: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->copy_model_and_input: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model = $parm{'model'};
	my $source = $parm{'source'};
	my $candidate_model;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $candidate_model;
}

sub write_tbs_files {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'thetanum' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->write_tbs_files: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->write_tbs_files: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->write_tbs_files: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->write_tbs_files: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->write_tbs_files: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $thetanum = $parm{'thetanum'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub copy_model_and_output {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'final_model' => 'm_model', 'model' => 'm_model',
			'use_run' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->copy_model_and_output: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->copy_model_and_output: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->copy_model_and_output: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->copy_model_and_output: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->copy_model_and_output: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $final_model = $parm{'final_model'};
	my $model = $parm{'model'};
	my $use_run = defined $parm{'use_run'} ? $parm{'use_run'} : '';

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub lsf_submit {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model' => 'm_model', 'nm_version' => 'm_SCALAR',
			'work_dir' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->lsf_submit: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->lsf_submit: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->lsf_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->lsf_submit: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->lsf_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $jobId = -1;
	my $model = $parm{'model'};
	my $nm_version = $parm{'nm_version'};
	my $work_dir = $parm{'work_dir'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $jobId;
}

sub umbrella_submit {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'model' => 'm_model', 'nm_version' => 'm_SCALAR',
			'prepare_jobs' => 'SCALAR', 'queue_info' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->umbrella_submit: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->umbrella_submit: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->umbrella_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->umbrella_submit: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->umbrella_submit: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $model = $parm{'model'};
	my $nm_version = $parm{'nm_version'};
	my $job_id = -1;
	my $prepare_jobs = defined $parm{'prepare_jobs'} ? $parm{'prepare_jobs'} : 0;
	my $queue_info = $parm{'queue_info'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $job_id;
}

sub register_in_database {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub calculate_raw_results_width {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub print_finish_message {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'run' => 'm_SCALAR', 'candidate_model' => 'model' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->print_finish_message: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->print_finish_message: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->print_finish_message: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->print_finish_message: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->print_finish_message: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $run = $parm{'run'};
	my $candidate_model = $parm{'candidate_model'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub create_sub_dir {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'subDir' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in tool::modelfit->create_sub_dir: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in tool::modelfit->create_sub_dir: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in tool::modelfit->create_sub_dir: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->create_sub_dir: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in tool::modelfit->create_sub_dir: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $subDir = $parm{'subDir'};
	my $tmp_dir;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $tmp_dir;
}

1;

