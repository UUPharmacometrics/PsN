# {{{ include

start include statements
use Carp;
use ui;
end include

# }}} include statements


start new
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
end new

start copy
	my @new_data = ();
	foreach my $row ( @{$self->subject_data} ) {
		my $new_row = $row;
	  push ( @new_data, $new_row );
	}

	$individual_copy = data::individual -> new (
						idcolumn     => $self->idcolumn,
				    idnumber     => $self->idnumber,
				    subject_data => \@new_data );
end copy

# {{{ diff

start diff
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
end diff

# }}} diff

# {{{ drop_columns

start drop_columns
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
end drop_columns

# }}} drop_columns

start evaluate_expression
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
end evaluate_expression

start factors
	my @data = @{$self->subject_data};
#factors is hash of values to ref of array of which line indices those values occur at.
	for ( my $i = 0; $i <= $#data; $i++ ) {
	  my @data_row = split( /,/, $data[$i] );
	  push( @{$factors{$data_row[$column-1]}}, $i );
	}
end factors

start idnumber
	if ( defined $parm ) {
	  for( my $i = 0 ; $i < scalar(@{$self->subject_data}); $i++ ) {
	    my @row = split( /,/, $self->subject_data->[$i] );
	    $row[ $self->idcolumn - 1 ] = $parm;
	    $self->subject_data->[$i] = join(',', @row);
	  }
	}
end idnumber

start recalc_column
	my ( $new_expr );
	for( my $i = 0 ; $i < scalar(@{$self->subject_data}); $i++ ) {
	  my @row = split( /,/, $self->subject_data->[$i] );
    ( $new_expr = $expression ) =~ s/{}/\$row[ \$column-1 ]/g;
    $row[ $column-1 ] = eval( $new_expr );
	  $self->subject_data->[$i] = join(',', @row);
	}
end recalc_column

start _read_data
	if ( defined $self->init_data ) {
	  $self->subject_data = $self->init_data;
	  $self->init_data = undef;
	}
end _read_data

start add_frem_lines
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
end add_frem_lines



start append_column
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
end append_column

start append_individual
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
end append_individual

start split_
{
	my @new_data = ();
	foreach my $row ( @{$self->subject_data} ) {
		my $new_row = $row;
	  push ( @new_data, $new_row );
	}

	$individual_copy = data::individual -> new (
						idcolumn     => $self->idcolumn,
				    idnumber     => $self->idnumber,
				    subject_data => \@new_data );
}
end copy
