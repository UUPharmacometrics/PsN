package data::individual;

use include_modules;
use ui;
use Moose;
use MooseX::Params::Validate;
use math;
use array;

has 'idcolumn' => ( is => 'rw', isa => 'Int', required => 1 );
has 'idnumber' => ( is => 'rw', isa => 'Num' );
has 'subject_data' => ( is => 'rw', isa => 'ArrayRef', required => 1 );

sub BUILD
{
	my $self = shift;

	# The idnumber attribute does not have an explicit default value but
	# is, if no number is given, instead set to the value of the first
	# id column which is assumed only to contain one value.

	if (not defined $self->idnumber) {
		my @data = split(/,/, $self->subject_data->[0]);
		unless (defined $data[$self->idcolumn - 1] and length($data[$self->idcolumn - 1]) > 0) {
			croak("The value in the id-column is empty");
		}
		if (math::usable_number($data[$self->idcolumn - 1])) {
			$self->idnumber($data[$self->idcolumn - 1]);
            $self->update_idnumber();
		} else {
			print "data is " . $self->subject_data->[0] . "\n";
			print "idcolumn is " . $self->idcolumn . "\n";
			croak("The value in the id column, " . $data[$self->idcolumn - 1] . ", does not look like a number\n");
		}
	}
}

sub update_idnumber
{
    # updates the id_number of subject_data
    my $self = shift;

    for (my $i = 0 ; $i < scalar(@{$self->subject_data}); $i++) {
        my @row = split(/,/, $self->subject_data->[$i]);
        $row[$self->idcolumn - 1] = $self->idnumber;
        $self->subject_data->[$i] = join(',', @row);
    }
}

sub copy
{
    my $self = shift;
    my $individual_copy;

    my @new_data = ();
    foreach my $row (@{$self->subject_data}) {
        my $new_row = $row;
        push (@new_data, $new_row);
    }

    $individual_copy = data::individual -> new(
        idcolumn     => $self->idcolumn,
        idnumber     => $self->idnumber,
        subject_data => \@new_data );

    return $individual_copy;
}


sub append_bivariate_columns
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  categorical_indices => { isa => 'ArrayRef', optional => 0 },
							  mapping => { isa => 'ArrayRef', optional => 0 },
							  missing_data_token => {isa => 'Maybe[Num]'}
	);
	my $categorical_indices = $parm{'categorical_indices'};
	my $mapping = $parm{'mapping'};
	my $missing_data_token = $parm{'missing_data_token'};

	for( my $i = 0 ; $i < scalar(@{$self->subject_data}); $i++ ) {
		my @values = split( /,/ , $self->subject_data->[$i] );
		my @newvalues = ();
		for (my $j=0; $j< scalar(@{$categorical_indices}); $j++){
			my $oldval = $values[$categorical_indices->[$j]];
			my $missing = 0;
			$missing = 1 if ($oldval == $missing_data_token);
			for (my $k=0; $k< scalar(@{$mapping->[$j]}); $k++){
				if ($oldval == $mapping->[$j]->[$k]){
					push(@newvalues,$oldval);
				}elsif ($missing){
					push(@newvalues,$missing_data_token);
				}else{
					#which is the 'not' value?
					if ($mapping->[$j]->[$k] == 0){
						push(@newvalues,1); #true values is 0, not value is 1
					}else{
						push(@newvalues,0); #true value is nonzero, not value is 0
					}
				}
			}
		}
		$self->subject_data->[$i] .= ','.join(',',@newvalues) if (scalar(@newvalues)>0);		
	}
	
}
sub add_frem_lines
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 type_index => { isa => 'Int', optional => 0 },
		 N_parameter_blocks => { isa => 'Int', optional => 0 },
		 occ_index => { isa => 'Maybe[Int]', optional => 1 },
		 mdv_index => { isa => 'Maybe[Int]', optional => 1 },
		 dv_index => { isa => 'Int', optional => 0 },
		 evid_index => { isa => 'Maybe[Int]', optional => 1 },
		 missing_data_token => { isa => 'Str', default => "-99", optional => 1 },
		 cov_indices => { isa => 'ArrayRef', optional => 0 },
		 is_log => { isa => 'ArrayRef', optional => 0 },
		 first_timevar_type => { isa => 'Int', optional => 0 }
	);
	my $type_index = $parm{'type_index'};
	my $N_parameter_blocks = $parm{'N_parameter_blocks'};
	my $occ_index = $parm{'occ_index'};
	my $mdv_index = $parm{'mdv_index'};
	my $dv_index = $parm{'dv_index'};
	my $evid_index = $parm{'evid_index'};
	my $missing_data_token = $parm{'missing_data_token'};
	my $cov_indices = $parm{'cov_indices'};
	my $is_log = $parm{'is_log'};
	my $first_timevar_type = $parm{'first_timevar_type'};
	my @invariant_values;
	my @timevar_values;

	if ($N_parameter_blocks > 99){
		croak("Not more than 99 parameter blocks supported");
	}
	sub format_array{
		my $arr = shift;
		for (my $i=0; $i < scalar(@{$arr}); $i++){
			$arr->[$i] =  sprintf("%.12G",$arr->[$i]);
		}
	}

	my $format_data=1; 

	#in is ref of array cov_indices where position index is value to set in type column and the value is the column index
	#           for the covariate in the data set. this array must be longer than 1. first pos(index 0) is not dv anymore, first cov
	#in occ_index of occasion column, can be undef 
	#in mdv_index of mdv column, can be undef
	#in evid_index of evid column, can be undef
	#in type_index of type column, cannot be undef
	#in missing_data_token
	#out array invariant_values with data value found in covariate columns
	#out array of arrays timevar_matrix with data values found in timevar columns

	my @data = @{$self -> subject_data()};
	my @timevar_matrix;

	my @newlines = ();
	my @check_index = ();
	push(@check_index, $mdv_index) if (defined $mdv_index);
	push(@check_index, $evid_index) if (defined $evid_index);
	unless (scalar(@check_index)>0){
		croak("both evid and mdv indices undefined in add_frem_lines");
	}

	my $done_invariant = 0;
	$done_invariant = 1 if ($first_timevar_type == 0); #no time invariant at all
	my $n_var = scalar(@{$cov_indices}) - $first_timevar_type; #number of time-varying cov
	#initialize timevar_matrix
	for (my $type= $first_timevar_type; $type < scalar(@{$cov_indices}) ;$type++){
		push(@timevar_matrix, []);
	}

	my %occasions = {};
	for ( my $i = 0; $i <= $#data; $i++ ) {
		my @data_row = split( /,/ , $data[$i] );
		my $not_obs=0;
		foreach my $index (@check_index){ #check that MDV and EVID are 0
			$not_obs = 1 unless ($data_row[$index] == 0);
		}
		unless ($done_invariant or $not_obs){
			#first loop over invariate covariates
			for (my $pos=0; $pos < $first_timevar_type; $pos++){
				my $cov_index = $cov_indices->[$pos];
				my @row = @data_row; #copy
				if ($is_log->[$pos] and ($row[$cov_index] != $missing_data_token )){
					$row[$dv_index] = log($row[$cov_index]); 
				}else{
					$row[$dv_index] = $row[$cov_index]; #set DV column to whatever cov value
				}
				push(@invariant_values,$row[$dv_index]);
				if ($row[$cov_index] == $missing_data_token){
					#cov value is missing
					$row[$mdv_index]=1 if (defined $mdv_index);
					$row[$evid_index]=2 if (defined $evid_index) ;
				}else{
					#cov value not missing
					$row[$mdv_index]=0 if (defined $mdv_index);
					$row[$evid_index]=0 if (defined $evid_index) ;
				}

				for (my $k= 0; $k<$N_parameter_blocks; $k++){
					#add one line per parameter block
					$row[$type_index] = (($pos+1)+(0.01*$k))  ; #fremtype value
					if ($format_data){
						format_array(\@row);
					}
					push(@newlines,join( ',', @row));
				}
			}
			$done_invariant = 1;
		}

		my $occ = $data_row[$occ_index] if (defined $occ_index);
		unless (($n_var < 1) or defined $occasions{$occ} or $not_obs){
			#no timevar at all OR
			#have not already handled this occasion OR this is not observation
			#found new occasion, loop over time-varying
			$occasions{$occ}=1;
			for (my $pos= $first_timevar_type; $pos < scalar(@{$cov_indices}) ;$pos++){
				my $cov_index = $cov_indices->[$pos];
				my @row = @data_row; #copy
				$row[$type_index] = ($pos+1); #fremtype value
				$row[$dv_index] = $row[$cov_index]; #set DV column to whatever cov value is here
				if ($row[$cov_index] == $missing_data_token){
					#cov value is missing
					$row[$mdv_index]=1 if (defined $mdv_index);
					$row[$evid_index]=2 if (defined $evid_index) ;

				}else{
					#cov value not missing
					push(@{$timevar_matrix[$pos-$first_timevar_type]},$row[$cov_index]);
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
			push(@timevar_values, array::median($timevar_matrix[$cov]));
		}
	}
	unless ($done_invariant){
		print "error in add_frem_lines, found no observation line to use for invariant cov\n";
	}

	$self->subject_data(\@newlines); #replace old data

	return \@invariant_values ,\@timevar_values;
}

sub append_column
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		new_values => { isa => 'Ref', optional => 0 }
	);
	my $new_values = $parm{'new_values'};

	#in is array of numbers. Verify that length of array is equal to number of lines,
	#otherwise die with error
	#append values at each line. No return value

	my @data = @{$self->subject_data};
	unless (scalar(@data) == scalar(@{$new_values})) {
		croak("lines of subject data(" . scalar(@data) . ") and length of new_values(" . scalar(@{$new_values}) .
			") must be equal in individual->append_column");
	}
	my @newlines = ();
	for (my $i = 0; $i < scalar(@data); $i++) {
		push(@newlines,$data[$i].','.$new_values->[$i]);
	}

	$self->subject_data(\@newlines); #replace old data
}

sub append_individual
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		new_individual => { isa => 'data::individual', optional => 1 }
	);
	my $new_individual = $parm{'new_individual'};

	#in is right hand side individual. Verify that length of data arrays are equal
	#otherwise die with error
	#append values at each line. No return value

	my @data = @{$self->subject_data};
	my @append_data = @{$new_individual->subject_data};
	unless (scalar(@data) == scalar(@append_data)) {
		croak("lines of subject data(" . scalar(@data) . ") and length of new individual(" . scalar(@append_data) .
			") must be equal in individual->append_individual");
	}
	my @newlines = ();
	for (my $i = 0; $i < scalar(@data); $i++) {
		push(@newlines, $data[$i] . ',' . $append_data[$i]);
	}

	$self->subject_data(\@newlines); #replace old data
}

sub factors
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 column => { isa => 'Int', optional => 1 }			# Counting from one
	);
	my $column = $parm{'column'};
	my %factors;

	my @data = @{$self->subject_data};
	#factors is hash of values to ref of array of which line indices those values occur at.
	for (my $i = 0; $i <= $#data; $i++) {
	  my @data_row = split(/,/, $data[$i]);
	  push(@{$factors{$data_row[$column - 1]}}, $i);
	}

	return \%factors;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
