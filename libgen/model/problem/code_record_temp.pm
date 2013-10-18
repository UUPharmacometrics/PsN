use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package model::problem::code_record;


#---------------------------------------------------------------------
#         Inherited Class Packages
#---------------------------------------------------------------------
use base qw(model::problem::record);

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my %superParms;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'code' => 'ARRAY', 'pre_verbatim' => 'ARRAY',
			'verbatim_last' => 'ARRAY', 'verbatim_first' => 'ARRAY' );

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
			'debug' -> die( message => "ERROR in model::problem::code_record->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem::code_record->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::code_record->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::code_record->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}


	bless $this, $class;
	model::problem::record::new($this,%superParms);

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $this;
};

sub code {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'code'} = $parm;
	} else {
		return $self -> {'code'};
	}
}

sub pre_verbatim {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'pre_verbatim'} = $parm;
	} else {
		return $self -> {'pre_verbatim'};
	}
}

sub verbatim_last {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'verbatim_last'} = $parm;
	} else {
		return $self -> {'verbatim_last'};
	}
}

sub verbatim_first {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'verbatim_first'} = $parm;
	} else {
		return $self -> {'verbatim_first'};
	}
}

sub _format_record {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'nonparametric_code' => 'SCALAR', 'number_format' => 'SCALAR',
			'shrinkage_code' => 'SCALAR', 'eigen_value_code' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::problem::code_record->_format_record: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem::code_record->_format_record: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem::code_record->_format_record: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::code_record->_format_record: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::code_record->_format_record: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $nonparametric_code = $parm{'nonparametric_code'};
	my $number_format = $parm{'number_format'};
	my $shrinkage_code = $parm{'shrinkage_code'};
	my $eigen_value_code = $parm{'eigen_value_code'};
	my @formatted;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _format_record');
# line 4 "lib/model/problem/code_record_subs.pm" 
      {
	  my $fname;
	  if ( defined $self->verbatim_first
	       or defined $self->pre_verbatim
	       or defined $self->code
	       or defined $self->verbatim_last ) {
	      my @class_names = split('::',ref($self));
	      $fname = uc(pop(@class_names));
	      $fname = "\$".$fname; #and then prepend to $formatted[0] at the very end so that do not get line break
	  }

	  if ( defined $self->pre_verbatim ) {
	      push( @formatted, @{$self->pre_verbatim} );
	  }
	  if ( defined $self->verbatim_first ) {
	      push( @formatted, '"FIRST' );
	      push( @formatted, @{$self->verbatim_first} );
	  }
	  if ( defined $self->code ) {
	      push( @formatted, @{$self->code} );
	  }
	  if ( defined $self->verbatim_last ) {
	      push( @formatted, '"LAST' );
	      push( @formatted, @{$self->verbatim_last} );
	  }
	  if (scalar(@formatted)>0){
	      if ($formatted[0] =~/^\s*;/){ #first code line is a comment
		  unshift(@formatted,$fname);
	      }else{
		  $formatted[0] = $fname.' '.$formatted[0]; 
	      }
	  }
      }
# line 188 libgen/model/problem/code_record.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _format_record');
	# End of Non-Dia code #

	return \@formatted;
}

sub _read_options {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_options');
# line 44 "lib/model/problem/code_record_subs.pm" 
      {
	my $in = 0;
	if ( defined $self->record_arr ) {
	  $self->code([]);
	  my ( $first, $last, $have_first ) = ( 0, 0, 0 );
	  my @pre_verbatim = ();
	  for ( @{$self->record_arr} ) {
	    # Get rid of $RECORD and unwanted spaces
	    s/^\s*\$\w+//;
	    if ( /\" (\w+) = EVTREC\((\d+),(\d+)\)/ ) {
	      next;
	    }
	    if( /^\"\s*FIRST/ ) {
	      $first = 1;
	      $have_first = 1;
	      next;
	    }
	    if( /^\"\s*LAST/ ) {
	      $first = 0;
	      $last  = 1;
	      next;
	    }
	    if( $first or $last ) {
	      if( /^\"/ ) {
		if( $first ) {
			$self->verbatim_first([]) unless defined $self->verbatim_first;
		  push( @{$self->verbatim_first}, $_ );
		} else {
			$self->verbatim_last([]) unless defined $self->verbatim_last;
		  push( @{$self->verbatim_last}, $_ );
		}		  
	      } else {
		$first = 0;
		$last  = 0;
		push @{$self->code}, $_;
	      }
	    } else {
	      if ($have_first){
		push @{$self->code}, $_;
	      }else{
		push (@pre_verbatim,$_);
	      }
	    }
	  }
	  if ($have_first){
			$self->pre_verbatim([]) unless defined $self->pre_verbatim;
	    push( @{$self->pre_verbatim}, @pre_verbatim);
	  }else{
	    unshift @{$self->code}, @pre_verbatim;
	  }
	}
      }
# line 253 libgen/model/problem/code_record.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_options');
	# End of Non-Dia code #

}

1;

