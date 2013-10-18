use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package data::individual;
use Carp;
use ui;
use debug;


sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'idcolumn' => 'SCALAR', 'data_id' => 'SCALAR',
			'idnumber' => 'SCALAR', 'subject_data' => 'ARRAY',
			'init_data' => 'ARRAY' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
		$parm{'reference_object'} = undef;
	}
	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in data::individual->new: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in data::individual->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in data::individual->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'idcolumn'} = defined $parm{'idcolumn'} ? $parm{'idcolumn'} : 1 unless defined $this -> {'idcolumn'};
	$this -> {'data_id'} = defined $parm{'data_id'} ? $parm{'data_id'} : 1 unless defined $this -> {'data_id'};

	bless $this, $class;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($this). '-> new');
# line 12 "lib/data/individual_subs.pm" 
	# The idnumber attribute does not have an explicit default value but
	# is, if no number is given, instead set to the value of the first
	# id column which is assumed only to contain one value.
	# 
	# Either subject_data or init_data must be given. Subject data is
	# a two-dimensional array holding the values of the subject. Init_data
	# is an one-dimensional array of strings, probably extracted from a
	# file where each row is one element in the array. This array is the
	# parsed by the constructor and moved to subject_data.

	die "Error in data::individual -> new: No ID column specified.\n"
           unless ( defined $this->idcolumn );
	if ( defined $this->subject_data ) {
	  if ( ! defined $this->idnumber ) {
	    my @data = split( /,/ , $this->subject_data->[0]);
	    $this -> idnumber(@data[$this->idcolumn - 1] );
	  }
	} else {
	  die "Error in data::individual -> new: No init_data specified.\n"
	    unless ( defined $this->init_data );
	  $this -> _read_data;
	}
# line 82 libgen/data/individual.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($this). '-> new');
	# End of Non-Dia code #

	return $this;
};

sub idcolumn {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'idcolumn'} = $parm;
	} else {
		return $self -> {'idcolumn'};
	}
}

sub data_id {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'data_id'} = $parm;
	} else {
		return $self -> {'data_id'};
	}
}

sub idnumber {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> idnumber');
# line 137 "lib/data/individual_subs.pm" 
	if ( defined $parm ) {
	  for( my $i = 0 ; $i < scalar(@{$self->subject_data}); $i++ ) {
	    my @row = split( /,/, $self->subject_data->[$i] );
	    $row[ $self->idcolumn - 1 ] = $parm;
	    $self->subject_data->[$i] = join(',', @row);
	  }
	}
# line 131 libgen/data/individual.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> idnumber');
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'idnumber'} = $parm;
	} else {
		return $self -> {'idnumber'};
	}
}

sub subject_data {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'subject_data'} = $parm;
	} else {
		return $self -> {'subject_data'};
	}
}

sub copy {
	my $self = shift;
	my $individual_copy;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> copy');
# line 37 "lib/data/individual_subs.pm" 
	my @new_data = ();
	foreach my $row ( @{$self->subject_data} ) {
		my $new_row = $row;
	  push ( @new_data, $new_row );
	}

	$individual_copy = data::individual -> new (
						idcolumn     => $self->idcolumn,
				    idnumber     => $self->idnumber,
				    subject_data => \@new_data );
# line 173 libgen/data/individual.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> copy');
	# End of Non-Dia code #

	return $individual_copy;
}

sub evaluate_expression {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'column' => 'SCALAR', 'expression' => 'SCALAR',
			'all_rows' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in data::individual->evaluate_expression: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in data::individual->evaluate_expression: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in data::individual->evaluate_expression: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->evaluate_expression: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->evaluate_expression: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $column = defined $parm{'column'} ? $parm{'column'} : 1;
	my $expression = $parm{'expression'};
	my $all_rows = defined $parm{'all_rows'} ? $parm{'all_rows'} : 0;
	my $result = 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> evaluate_expression');
# line 108 "lib/data/individual_subs.pm" 
#not used anywhere, build on this for randtest?
	my $new_expr;
  my $data = $self->subject_data;
	if ( defined $expression ) {
	  if ( $all_rows ) {
	    $result = 1;
	    foreach my $row ( @{$data} ) {
	      my @row = split( /,/ , $row );
	      ( $new_expr = $expression ) =~ s/{}/\$row[ \$column-1 ]/g;
	      $result = $result * eval( $new_expr );
	    }
	  } else {
	    my @row = split( /,/, $data -> [0] );
	    ( $new_expr = $expression ) =~ s/{}/\$row[ \$column-1 ]/g;
	    $result = eval( $new_expr );
	  }
	}
# line 233 libgen/data/individual.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> evaluate_expression');
	# End of Non-Dia code #

	return $result;
}

sub add_frem_lines {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'type_index' => 'm_SCALAR', 'occ_index' => 'SCALAR',
			'mdv_index' => 'SCALAR', 'evid_index' => 'SCALAR',
			'missing_data_token' => 'SCALAR', 'cov_indices' => 'REF',
			'first_timevar_type' => 'm_SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in data::individual->add_frem_lines: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in data::individual->add_frem_lines: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in data::individual->add_frem_lines: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->add_frem_lines: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->add_frem_lines: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $type_index = $parm{'type_index'};
	my $occ_index = $parm{'occ_index'};
	my $mdv_index = $parm{'mdv_index'};
	my $evid_index = $parm{'evid_index'};
	my $missing_data_token = defined $parm{'missing_data_token'} ? $parm{'missing_data_token'} : "-99";
	my $cov_indices = $parm{'cov_indices'};
	my $first_timevar_type = $parm{'first_timevar_type'};
	my @invariant_values;
	my @timevar_values;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> add_frem_lines');
# line 164 "lib/data/individual_subs.pm" 
{
    sub median{
	#input reference to array
	#return median which is middle value if uneven number of values
	#or arithemtic mean of two middle values if even number of values
	#if empty input array then return 0
	#not yet verified with matlab
	#does not handle undef, since have filtered out missing_data_token and assume all other numeric  
	my $ref = shift;
	return 0 if (scalar(@{$ref})<1);
	my @sorted = sort ({$a <=> $b} @{$ref});
	
	if( scalar( @sorted ) % 2 ){
	    return $sorted[$#sorted/2];
	} else {
	    return ($sorted[@sorted/2]+$sorted[(@sorted-2)/2]) / 2;
	}
	
    }

    sub format_array{
	my $arr = shift;
	for (my $i=0; $i < scalar(@{$arr}); $i++){
	    $arr->[$i] =  sprintf("%.12G",$arr->[$i]);
	}
    }


    my $format_data=1; 

    #in is ref of array cov_indices where position index is value to set in type column and the value is the column index
    #           for the covariate in the data set. this array must be longer than 1. first pos(index 0) is dv
    #in occ_index of occasion column, can be undef 
    #in mdv_index of mdv column, can be undef
    #in evid_index of evid column, can be undef
    #in type_index of type column, cannot be undef
    #in missing_data_token
    #out array invariant_values with data value found in covariate columns
    #out array of arrays timevar_matrix with data values found in timevar columns
    
    my @data = @{$self -> subject_data()};
    my $dv_index = $cov_indices->[0];
    my @timevar_matrix;

    my @newlines =();
    my @check_index =();
    push(@check_index, $mdv_index) if (defined $mdv_index);
    push(@check_index, $evid_index) if (defined $evid_index);
    unless (scalar(@check_index)>0){
			croak("both evid and mdv indices undefined in add_frem_lines");
    }

    my $done_invariant =0;
    $done_invariant = 1 if ($first_timevar_type == 1); #no time invariant at all
    my $n_var = scalar(@{$cov_indices}) - $first_timevar_type; #number of time-varying cov
    #initialize timevar_matrix
    for (my $type= $first_timevar_type; $type < scalar(@{$cov_indices}) ;$type++){
       push(@timevar_matrix,[]);
    }

    #TODO: set to 0 in firstline find AMT RATE II ADDL SS and set to 0 if
    #no obs line found for individual
    #change CMT?


    #loop for error check only, tab error here
    if (0){
	for ( my $i=1; $i < scalar(@data); $i++ ){
	    my @row = split( /,/ , $data[$i] );
	    for (my $type=1; $type < $first_timevar_type; $type++){
		unless (($row[$cov_indices->[$type]] == $invariant_values[$type-1]) or
			($row[$cov_indices->[$type]] == 0) or
			($row[$cov_indices->[$type]] == $missing_data_token)){
		    #non-unique covariate value found for individual
		    print "Warning: non-unique covariate value ".$row[$cov_indices->[$type]]." found for ".
			" covariate in column ".($cov_indices->[$type] +1)."\n";
		}
	    }
	}
    }

    my %occasions={};
    for ( my $i = 0; $i <= $#data; $i++ ) {
	my @data_row = split( /,/ , $data[$i] );
	my $not_obs=0;
	foreach my $index (@check_index){ #check that MDV and EVID are 0
	    $not_obs = 1 unless ($data_row[$index] == 0);
	}
	unless ($done_invariant or $not_obs){
	    #first loop over invariate covariates
	    for (my $type=1; $type < $first_timevar_type; $type++){
		my $cov_index = $cov_indices->[$type];
		my @row = @data_row; #copy
		$row[$type_index] = $type; #type value
		$row[$dv_index] = $row[$cov_index]; #set DV column to whatever cov value
		push(@invariant_values,$row[$cov_index]);
		if ($row[$cov_index] == $missing_data_token){
		    #cov value is missing
		    $row[$mdv_index]=1 if (defined $mdv_index);
		    $row[$evid_index]=2 if (defined $evid_index) ;
		}else{
		    #cov value not missing
		    $row[$mdv_index]=0 if (defined $mdv_index);
		    $row[$evid_index]=0 if (defined $evid_index) ;
		}
		
		if ($format_data){
		    format_array(\@row);
		}
		push(@newlines,join( ',', @row));
	    }
	    $done_invariant = 1;
	}

	my $occ = $data_row[$occ_index] if (defined $occ_index);
	unless (($n_var < 1) or defined $occasions{$occ} or $not_obs){
            #no timevar at all OR
	    #have not already handled this occasion OR this is not observation
	    #found new occasion, loop over time-varying
	    $occasions{$occ}=1;
	    for (my $type= $first_timevar_type; $type < scalar(@{$cov_indices}) ;$type++){
		my $cov_index = $cov_indices->[$type];
		my @row = @data_row; #copy
		$row[$type_index] = $type; #type value
		$row[$dv_index] = $row[$cov_index]; #set DV column to whatever cov value is here
		if ($row[$cov_index] == $missing_data_token){
		    #cov value is missing
		    $row[$mdv_index]=1 if (defined $mdv_index);
		    $row[$evid_index]=2 if (defined $evid_index) ;
		    
		}else{
		    #cov value not missing
		    push(@{$timevar_matrix[$type-$first_timevar_type]},$row[$cov_index]);
		    $row[$mdv_index]=0 if (defined $mdv_index);
		    $row[$evid_index]=0 if (defined $evid_index) ;
		}
		format_array(\@row) if ($format_data); 
		push(@newlines,join( ',', @row));
	    }
	}
	#even if old occasion or not observation sort old data line in here
	if ($format_data){
	    format_array(\@data_row);
	    push(@newlines,join( ',', @data_row));
	}else{
	    push(@newlines,$data[$i]);
	}
    }
    #compute medians over occasions
    for (my $cov=0; $cov < $n_var; $cov++){
	if (scalar(@{$timevar_matrix[$cov]}<1)){
	    #no non-missing values at all
	    push(@timevar_values,$missing_data_token);
	}else{
            #median over occasions
	    push(@timevar_values,median($timevar_matrix[$cov]));
	}
    }
    unless ($done_invariant){
	print "error in add_frem_lines, found no observation line to use for invariant cov\n";
    }
    
    $self->subject_data(\@newlines); #replace old data
    

}
# line 449 libgen/data/individual.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> add_frem_lines');
	# End of Non-Dia code #

	return \@invariant_values ,\@timevar_values;
}

sub recalc_column {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'column' => 'SCALAR', 'expression' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in data::individual->recalc_column: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in data::individual->recalc_column: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in data::individual->recalc_column: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->recalc_column: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->recalc_column: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $column = $parm{'column'};
	my $expression = $parm{'expression'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> recalc_column');
# line 147 "lib/data/individual_subs.pm" 
	my ( $new_expr );
	for( my $i = 0 ; $i < scalar(@{$self->subject_data}); $i++ ) {
	  my @row = split( /,/, $self->subject_data->[$i] );
    ( $new_expr = $expression ) =~ s/{}/\$row[ \$column-1 ]/g;
    $row[ $column-1 ] = eval( $new_expr );
	  $self->subject_data->[$i] = join(',', @row);
	}
# line 496 libgen/data/individual.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> recalc_column');
	# End of Non-Dia code #

}

sub write {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'subj_data' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in data::individual->write: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in data::individual->write: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in data::individual->write: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->write: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->write: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @subj_data = defined $parm{'subj_data'} ? @{$parm{'subj_data'}} : ();

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub STORABLE_freeze {
	my $self = shift;
	my $return_value = '';

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $return_value;
}

sub STORABLE_thaw {
	my $self = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

sub drop_columns {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'drop' => 'ARRAY' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in data::individual->drop_columns: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in data::individual->drop_columns: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in data::individual->drop_columns: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->drop_columns: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->drop_columns: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my @drop = defined $parm{'drop'} ? @{$parm{'drop'}} : ();

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> drop_columns');
# line 92 "lib/data/individual_subs.pm" 
	my @new_data;
	my @data = @{$self->subject_data};
	for( my $i = 0; $i <= $#data; $i++ ) {
	  my @new_row;
	  my @data_row = split( /,/, $data[$i] );
	  for( my $j = 0; $j < scalar @data_row; $j++ ) {
	    push( @new_row, $data_row[$j] ) if ( not $drop[$j] );
	  }
	  push( @new_data, join( ',', @new_row ) );
	}
	$self->subject_data(\@new_data);
# line 597 libgen/data/individual.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> drop_columns');
	# End of Non-Dia code #

}

sub append_column {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'new_values' => 'm_REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in data::individual->append_column: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in data::individual->append_column: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in data::individual->append_column: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->append_column: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->append_column: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $new_values = $parm{'new_values'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> append_column');
# line 335 "lib/data/individual_subs.pm" 
{
    #in is array of numbers. Verify that length of array is equal to number of lines,
    #otherwise die with error
    #append values at each line. No return value

    my @data = @{$self -> subject_data()};
    unless (scalar(@data) == scalar(@{$new_values})){
	croak("lines of subject data(".scalar(@data).") and length of new_values(".scalar(@{$new_values}).
		   ") must be equal in individual->append_column");
    }
    my @newlines=();
    for (my $i=0; $i< scalar(@data); $i++){
	push(@newlines,$data[$i].','.$new_values->[$i]);
    }

    $self->subject_data(\@newlines); #replace old data

}
# line 653 libgen/data/individual.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> append_column');
	# End of Non-Dia code #

}

sub append_individual {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'new_individual' => 'REF' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in data::individual->append_individual: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in data::individual->append_individual: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in data::individual->append_individual: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->append_individual: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->append_individual: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $new_individual = $parm{'new_individual'};

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> append_individual');
# line 356 "lib/data/individual_subs.pm" 
{
    #in is right hand side individual. Verify that length of data arrays are equal
    #otherwise die with error
    #append values at each line. No return value

    my @data = @{$self -> subject_data()};
    my @append_data = @{$new_individual -> subject_data()};
    unless (scalar(@data) == scalar(@append_data)){
	croak("lines of subject data(".scalar(@data).") and length of new individual(".scalar(@append_data).
		   ") must be equal in individual->append_individual");
    }
    my @newlines=();
    for (my $i=0; $i< scalar(@data); $i++){
	push(@newlines,$data[$i].','.$append_data[$i]);
    }

    $self->subject_data(\@newlines); #replace old data

}
# line 710 libgen/data/individual.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> append_individual');
	# End of Non-Dia code #

}

sub diff {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'against_individual' => 'm_data::individual',
			'columns' => 'm_ARRAY', 'absolute_diff' => 'SCALAR',
			'largest' => 'SCALAR', 'smallest' => 'SCALAR',
			'diff_as_fraction' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in data::individual->diff: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in data::individual->diff: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in data::individual->diff: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->diff: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->diff: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $against_individual = $parm{'against_individual'};
	my @columns = defined $parm{'columns'} ? @{$parm{'columns'}} : ();
	my $absolute_diff = defined $parm{'absolute_diff'} ? $parm{'absolute_diff'} : 1;
	my $largest = defined $parm{'largest'} ? $parm{'largest'} : 1;
	my $smallest = defined $parm{'smallest'} ? $parm{'smallest'} : 0;
my %diff_results;
	my $diff_as_fraction = defined $parm{'diff_as_fraction'} ? $parm{'diff_as_fraction'} : 1;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> diff');
# line 52 "lib/data/individual_subs.pm" 
  my @data      = @{$self->subject_data};
  my @diff_data = @{$against_individual -> subject_data};
  for ( my $i = 0; $i <= $#data; $i++ ) {
    my @data_row = split( /,/ , $data[$i] );
    my @diff_data_row = split( /,/ , $diff_data[$i] );
    if( $largest ) {
      for( my $j = 0; $j <= $#columns; $j++ ) {
				my $diff = $data_row[$columns[$j] - 1] - $diff_data_row[$columns[$j] - 1];
		  	$diff = abs($diff) if( $absolute_diff );
				if( $diff_as_fraction ) {
	  			if ( defined $diff_data_row[$columns[$j] - 1] and not $diff_data_row[$columns[$j] - 1] == 0 ) {
	    			$diff = $diff / $diff_data_row[$columns[$j] - 1];
	  			} elsif ( not $diff == 0 ) { # If $diff == 0 we can leave it as it is even if we formally
	    			print "ID: ",$self->idcolumn," ID2: ",$against_individual->idcolumn,"\n";
	    			print "DATA1: ", join("\n", @data), "\n";
	    			print "DATA2: ", join("\n", @diff_data), "\n";
	    			# would require a division by the original value
	    			croak("The difference of column ".$columns[$j].
			  			" was requested as a fraction but the original value was ".
			  			"found to be zero: a division is impossible." );
	  			}
				}

				if ( not defined $diff_results{$columns[$j]} or not defined $diff_results{$columns[$j]}{'diff'} or $diff > $diff_results{$columns[$j]}{'diff'} ) {
	  			$diff_results{$columns[$j]}{'diff'} = $diff;
	  			$diff_results{$columns[$j]}{'self'} = $data_row[$columns[$j]-1];
	  			$diff_results{$columns[$j]}{'test'} = $diff_data_row[$columns[$j]-1];
				}
			}
    } else {
      die "individual -> diff is only implemented for finding the largest difference at any observation at this point\n";
    }
  }
# line 790 libgen/data/individual.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> diff');
	# End of Non-Dia code #

	return \%diff_results;
}

sub factors {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'column' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in data::individual->factors: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in data::individual->factors: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in data::individual->factors: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->factors: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->factors: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $column = $parm{'column'};
my %factors;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> factors');
# line 128 "lib/data/individual_subs.pm" 
	my @data = @{$self->subject_data};
#factors is hash of values to ref of array of which line indices those values occur at.
	for ( my $i = 0; $i <= $#data; $i++ ) {
	  my @data_row = split( /,/, $data[$i] );
	  push( @{$factors{$data_row[$column-1]}}, $i );
	}
# line 836 libgen/data/individual.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> factors');
	# End of Non-Dia code #

	return \%factors;
}

sub _read_data {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> _read_data');
# line 157 "lib/data/individual_subs.pm" 
	if ( defined $self->init_data ) {
	  $self->subject_data = $self->init_data;
	  $self->init_data = undef;
	}
# line 853 libgen/data/individual.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> _read_data');
	# End of Non-Dia code #

}

sub _set_idnumber {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'newnum' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in data::individual->_set_idnumber: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in data::individual->_set_idnumber: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in data::individual->_set_idnumber: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->_set_idnumber: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in data::individual->_set_idnumber: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $newnum = $parm{'newnum'};

	# Start of Non-Dia code #
	# End of Non-Dia code #

}

1;

