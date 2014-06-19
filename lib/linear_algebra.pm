package linear_algebra;

use FindBin qw($Bin);
use lib "$Bin/../lib";
use strict;


sub house { 
    my $xvec = shift;
    #add checking for 0 div here
    my $n=scalar(@{$xvec});
    my $sigma=0;
    my @vvec=@{$xvec};
    my $beta=0;
    $vvec[0]=1;
    for (my $i=1;$i<$n;$i++){
	$sigma += ($xvec->[$i])**2;
	$vvec[$i]=$xvec->[$i];
    }
    if ($sigma == 0){
	$beta=0;
    }else{
	my $mu = sqrt(($xvec->[0])**2 + $sigma);
	if ($xvec->[0] <= 0){
	    $vvec[0] = $xvec->[0]-$mu;
	}else{
	    $vvec[0] = -$sigma/($xvec->[0]+$mu);
	}
	$beta = 2*($vvec[0])**2/($sigma+($vvec[0])**2);
	for (my $i=1;$i<$n;$i++){
	    $vvec[$i]=$vvec[$i]/$vvec[0];
	}
	$vvec[0]=1;
    }
    my %answer={};
    $answer{'beta'}=$beta;
    $answer{'vvec'}=\@vvec;
    return \%answer;
} 




sub QR_factorize { 
    
    #verified against matlab for small matrices
    #householder method transform
    #assume Amatrix is column format
    #returned rmatrix is column format Rmat->[$col][$row]

    my $ref =shift;
    my $Rmatrix =shift;
    my @Amatrix = @{$ref};

    my $ncol= scalar(@Amatrix);
    my $endcol=$ncol-1;
    my $mrow = scalar(@{$Amatrix[0]});
    my $endrow=$mrow-1;
    my $input_error = 2;
    my $numerical_error = 1;

    
    for (my $j=0;$j<$ncol;$j++){
	my @xvec = @{$Amatrix[$j]}[$j..$endrow];
	#print join(' ',@xvec)."\n";
	#die;
	my $href = house(\@xvec);
	my $beta = $$href{'beta'}; #double $ ?
	my $vvec = $$href{'vvec'};
	#house transform A(j:endrow,j:endcol)
	#for first col know only first comp is number, rest is 0 (R matrix)
	#w=beta Atrans v
	my @wvec;
	for (my $i=0;$i<($ncol-$j);$i++){
	    my $col=$i+$j;
	    $wvec[$i]=0;
	    for (my $k=0;$k<($mrow-$j);$k++){
		$wvec[$i] += $beta*($vvec->[$k])*$Amatrix[$col][$k+$j];
	    }      
	}
	#col $j gives R
	my @rcol;
	@rcol = @{$Amatrix[$j]}[0..($j-1)] if ($j>0);
	push(@rcol,($Amatrix[$j][$j]-$wvec[0]*$vvec->[0]));
	push(@{$Rmatrix},\@rcol);
	#check that rest practically 0 
	for (my $k=1;$k<($mrow-$j);$k++){
	    my $val = $Amatrix[$j][$j+$k]-$wvec[0]*$vvec->[$k];
	    unless ( $val < 0.00001){
		print "error in house transformation j $j k $k val $val\n";
		return $numerical_error;
	    }
	}
	#tranform rest of A cols for next iteration
	for (my $i=1;$i<($ncol-$j);$i++){
	    for (my $k=0;$k<($mrow-$j);$k++){
		$Amatrix[$i+$j][$j+$k] = $Amatrix[$i+$j][$j+$k]-$wvec[$i]*$vvec->[$k];
	    }
	}
    }

    return 0;
} 

sub cholesky {
    #input is lower triangle, including diagonal, of symmetric positive definite matrix 
    #in *column format*, A->[col][row]
    #this matrix is overwritten with lower triangular Cholesky factor (G^T) 
    #Golub p144 Alg 4.2.1
    #verified with matlab
    #verified call by reference

    my $Aref=shift;
    my $input_error = 2;
    my $numerical_error = 1;
    my $ncol= scalar(@{$Aref});
    my $mrow = scalar(@{$Aref->[0]});
    return $input_error unless ($mrow == $ncol);

    for (my $j=0; $j< $ncol; $j++){
		if ($j>0) {
			#i=j
			my $sum=0;
			for (my $k=0; $k<$j ; $k++){
				$sum=$sum+($Aref->[$k][$j])**2;
			}
			my $diff = $Aref->[$j][$j]-$sum;
			return $numerical_error if ($diff < 0);
			$Aref->[$j][$j]=sqrt($diff);
			return $numerical_error if ($Aref->[$j][$j] == 0);
			#i=j+1:n
			for (my $i=($j+1); $i<$ncol; $i++){
				my $sum=0;
				for (my $k=0; $k<$j ; $k++){
					$sum=$sum+($Aref->[$k][$j])*($Aref->[$k][$i]);
				}
				$Aref->[$j][$i]=($Aref->[$j][$i]-$sum)/($Aref->[$j][$j]);
			}
		} else {
			$Aref->[0][0]=sqrt($Aref->[0][0]);
			if ($Aref->[0][0] == 0){
				print "cholesky leading element is 0";
				return $numerical_error ;
			}
			for (my $i=1; $i< $ncol; $i++){
				$Aref->[0][$i]=$Aref->[0][$i]/($Aref->[0][0]);
			}
		}
    }

    return 0;
}


sub lower_triangular_identity_solve{
    #input is lower triangular matrix
    #in *column format*, A->[col][row]
    #and number of columns $nsolve to solve for
    #and reference to empty solution matrix
    #right hand side is first $ncol columns of identity matrix
    #algorithm Golub p 90, alg 3.1.3 adapted to identity right hand side and stop after $ncol
    #verified against matlab

    my $Aref=shift;
    my $nsolve=shift;
    my $solution = shift;
    my $input_error = 2;
    my $numerical_error = 1;

    my $ncol= scalar(@{$Aref});
    return $input_error if ($nsolve>$ncol);
    my $nrow = scalar(@{$Aref->[0]});
    return $input_error unless ($nrow == $ncol);

    #create part of identity matrix as right hand. Will be overwritten with solution
    for (my $i=0; $i<$nsolve; $i++){
	push(@{$solution},[(0) x $nrow]);
	$solution->[$i][$i]=1;
    }

    #solve by forward substitution
    for (my $i=0; $i< $nsolve; $i++){   
	if ($i < ($nrow-1)){
	    #right column is nonzero only for j=i
	    return $numerical_error if ($Aref->[$i][$i] == 0);
	    $solution->[$i][$i]=$solution->[$i][$i]/$Aref->[$i][$i];
	    for (my $k=($i+1);$k<$nrow;$k++){
		$solution->[$i][$k]=-$solution->[$i][$i]*$Aref->[$i][$k];
	    }
	}

	for (my $j=($i+1);$j<($nrow-1);$j++){
	    return $numerical_error if ($Aref->[$j][$j] == 0);
	    $solution->[$i][$j]=$solution->[$i][$j]/$Aref->[$j][$j];
	    for (my $k=($j+1);$k<$nrow;$k++){
		$solution->[$i][$k]=$solution->[$i][$k]-$solution->[$i][$j]*$Aref->[$j][$k];
	    }
	}
	return $numerical_error if ($Aref->[$nrow-1][$nrow-1] == 0);
	$solution->[$i][$nrow-1]=$solution->[$i][$nrow-1]/$Aref->[$nrow-1][$nrow-1];

    }
    return 0;


}

sub upper_triangular_solve{
    #input is upper triangular matrix
    #in *column format*, Umat->[col][row]
    #and reference to right hand vector which will be overwritten with solution
    #solve Umat*x=b
    #algorithm Golub p 89, alg 3.1.2
    #verified with matlab 

    my $Umat=shift;
    my $solution = shift;
    my $input_error = 2;
    my $numerical_error = 1;
    my $ncol= scalar(@{$Umat});

    return $numerical_error if ($Umat->[$ncol-1][$ncol-1] == 0);
    $solution->[$ncol-1]=$solution->[$ncol-1]/$Umat->[$ncol-1][$ncol-1];
    for (my $i=($ncol-2);$i>=0;$i--){
      my $sum=0;
      for (my $j=($i+1);$j<$ncol;$j++){
	$sum += ($Umat->[$j][$i])*$solution->[$j];
      }
      return $numerical_error if ($Umat->[$i][$i] == 0);
      $solution->[$i]=($solution->[$i]-$sum)/($Umat->[$i][$i]); 
    }
    return 0;

}

sub upper_triangular_transpose_solve{
    #input is upper triangular matrix
    #in *column format*, Umat->[col][row]
    #and reference to right hand vector which will be overwritten with solution
    #solve Umat'*x=b
    #algorithm Golub p 89, lower triangular alg 3.1.1
    #verified with matlab

    my $Umat=shift;
    my $solution = shift;
    my $input_error = 2;
    my $numerical_error = 1;
    my $ncol= scalar(@{$Umat});

    return $numerical_error if ($Umat->[0][0] == 0);
    $solution->[0]=$solution->[0]/$Umat->[0][0];

    for (my $i=1;$i<$ncol;$i++){
		my $sum=0;
		for (my $j=0;$j<$i;$j++){
			$sum += ($Umat->[$i][$j])*$solution->[$j];
		}
		return $numerical_error if ($Umat->[$i][$i] == 0);
		$solution->[$i]=($solution->[$i]-$sum)/$Umat->[$i][$i];
    }
	
    return 0;
	
}

sub upper_triangular_identity_solve{
    #input is upper triangular matrix
    #in *column format*, A->[col][row]
    #and reference to empty solution matrix
    #right hand side is identity matrix
    #algorithm Golub p 90, alg 3.1.4 adapted to identity right hand side
    #verified with matlab

    my $Aref=shift;
    my $solution = shift;
    my $input_error = 2;
    my $numerical_error = 1;
    my $ncol= scalar(@{$Aref});

    for (my $j=0;$j< $ncol;$j++){
	my $nrow = scalar(@{$Aref->[$j]});
#	print "nrow $nrow ncol $ncol\n";
	return $input_error unless ($nrow > $j);
    }

    #create identity matrix as right hand. Will be overwritten with solution
    for (my $i=0; $i<$ncol; $i++){
	push(@{$solution},[(0) x $ncol]);
	$solution->[$i][$i]=1;
    }
    for (my $i=0;$i<$ncol; $i++){
	if ($i>0){
	    return $numerical_error if ($Aref->[$i][$i] == 0);
	    $solution->[$i][$i]=$solution->[$i][$i]/$Aref->[$i][$i];
	    for (my $k=0; $k<$i;$k++){
		$solution->[$i][$k]=-$solution->[$i][$i]*$Aref->[$i][$k];
	    }
	}
	for (my $j=($i-1);$j>0;$j--){
	    return $numerical_error if ($Aref->[$j][$j] == 0);
	    $solution->[$i][$j]=$solution->[$i][$j]/$Aref->[$j][$j];
	    for (my $k=0; $k<$j; $k++){
		$solution->[$i][$k]=$solution->[$i][$k]-$solution->[$i][$j]*$Aref->[$j][$k];
	    }
	}
	return $numerical_error if ($Aref->[0][0] == 0);
	$solution->[$i][0]=$solution->[$i][0]/$Aref->[0][0];
    }
    return 0;
}

sub upper_triangular_UUT_multiply{
    #input is upper triangular matrix
    #in *column format*, R->[col][row]
    #and reference to empty solution matrix
    #multiply from the right with transpose, output only lower triangle of symmetric matrix
    #verified in matlab

    my $Rref=shift;
    my $solution = shift;
    my $input_error = 2;
    my $numerical_error = 1;
    my $ncol= scalar(@{$Rref});

    for (my $j=0;$j< $ncol;$j++){
	my $nrow = scalar(@{$Rref->[$j]});
#	print "nrow $nrow ncol $ncol\n";
	return $input_error unless ($nrow > $j);
    }

    #create zeros matrix as placeholder for solution
    for (my $i=0; $i<$ncol; $i++){
	push(@{$solution},[(0) x $ncol]);
    }

    for (my $i=0; $i< $ncol; $i++){
	for (my $j=0; $j<=$i; $j++){
	    my $sum=0;
	    for (my $k=$i; $k< $ncol; $k++){
		$sum=$sum+$Rref->[$k][$i]*$Rref->[$k][$j];
	    }
	    $solution->[$j][$i]=$sum;
	}
    }
    return 0;
}

sub column_cov{
    #input is reference to values matrix 
    #in *column format*, Aref->[col][row]
    #and reference to empty result matrix
    #compute square variance covariance matrix
    #Normalization is done with N-1, input error if N<2
    #verified with matlab cov function

    my $Aref=shift;
    my $varcov = shift;
    my $input_error = 2;
    my $numerical_error = 1;
    my $ncol= scalar(@{$Aref});
    my $nrow = scalar(@{$Aref->[0]});
    return $input_error if ($nrow < 2);

    my @sum = (0) x $ncol;
    my @mean = (0) x $ncol;

    #initialize square result matrix
    for (my $i=0; $i<$ncol; $i++){
	push(@{$varcov},[(0) x $ncol]);
    }

    for (my $col=0; $col< $ncol; $col++){
	return $input_error if (scalar(@{$Aref->[$col]}) != $nrow);

	foreach my $val (@{$Aref->[$col]}){
	    $sum[$col] = $sum[$col] + $val;
	}
	$mean[$col]=$sum[$col]/$nrow;

	#variance
	my $sum_errors_pow2=0;
	foreach my $val (@{$Aref->[$col]}){
	    $sum_errors_pow2 = $sum_errors_pow2 + ($val - $mean[$col])**2;
	}
	unless ( $sum_errors_pow2 == 0 ){
	    #if sum is 0 then assume all estimates 0, just ignore
	    $varcov->[$col][$col]= $sum_errors_pow2/($nrow-1);
	}

	#covariance
	#here $j is always smaller than $col, meaning that mean is already computed 
	for (my $j=0; $j< $col; $j++){
	    my $sum_errors_prod=0;
	    for (my $i=0; $i< $nrow; $i++){
		$sum_errors_prod = $sum_errors_prod + ($Aref->[$col][$i] - $mean[$col])*($Aref->[$j][$i] - $mean[$j]);
	    }
	    unless( $sum_errors_prod == 0 ){
		#if sum is 0 then assume all estimates 0, just ignore
		$varcov->[$j][$col]= $sum_errors_prod/($nrow-1);
		$varcov->[$col][$j]=$varcov->[$j][$col]; 
	    }
	}

    }

    return 0;
}

sub row_cov{
    #input is reference to values matrix 
    #in *row format*, Aref->[row][col]
    #and reference to empty result matrix
    #compute square variance covariance matrix
    #Normalization is done with N-1, input error if N<2
    #verified with matlab cov function

    
    my $Aref=shift;
    my $varcov = shift;
    my $debug=0;
    my $input_error = 2;
    my $numerical_error = 1;
    my $nrow= scalar(@{$Aref});
    return $input_error if ($nrow < 2);
    my $ncol = scalar(@{$Aref->[0]});
    for (my $row=1; $row< $nrow; $row++){
	return $input_error if (scalar(@{$Aref->[$row]}) != $ncol);
    }

    my @sum = (0) x $ncol;
    my @mean = (0) x $ncol;

    #initialize square result matrix
    for (my $i=0; $i<$ncol; $i++){
	push(@{$varcov},[(0) x $ncol]);
    }

    for (my $col=0; $col< $ncol; $col++){

	for (my $row=0; $row< $nrow; $row++){
	    $sum[$col] = $sum[$col] + $Aref->[$row][$col];
	}
	$mean[$col]=$sum[$col]/$nrow;
	print "mean $col is ".$mean[$col]."\n" if $debug;
	#variance
	my $sum_errors_pow2=0;
	for (my $row=0; $row< $nrow; $row++){
	    $sum_errors_pow2 = $sum_errors_pow2 + ($Aref->[$row][$col] - $mean[$col])**2;
	}
	unless ( $sum_errors_pow2 == 0 ){
	    #if sum is 0 then assume all estimates 0, just ignore
	    $varcov->[$col][$col]= $sum_errors_pow2/($nrow-1);
	    print "variance $col is ".$varcov->[$col][$col]."\n" if $debug;
	}

	#covariance
	#here $j is always smaller than $col, meaning that mean is already computed 
	for (my $j=0; $j< $col; $j++){
	    my $sum_errors_prod=0;
	    for (my $i=0; $i< $nrow; $i++){
		$sum_errors_prod = $sum_errors_prod + ($Aref->[$i][$col] - $mean[$col])*($Aref->[$i][$j] - $mean[$j]);
	    }
	    unless( $sum_errors_prod == 0 ){
		#if sum is 0 then assume all estimates 0, just ignore
		$varcov->[$j][$col]= $sum_errors_prod/($nrow-1);
		$varcov->[$col][$j]=$varcov->[$j][$col]; 
	    }
	}

    }

    return 0;
}

sub median{
    #input reference to array
    #return median which is middle value if uneven number of values
    #or arithemtic mean of two middle values if even number of values
    #if empty input array then return 0
    #not yet verified with matlab
    my $ref = shift;
    return 0 if (scalar(@{$ref})<1);
    my @sorted = sort ({$a <=> $b} @{$ref});

    if( scalar( @sorted ) % 2 ){
	return $sorted[$#sorted/2];
    } else {
	return ($sorted[@sorted/2]+$sorted[(@sorted-2)/2]) / 2;
    }

}

sub row_cov_median{
    #input is reference to values matrix 
    #in *row format*, Aref->[row][col]
    #and reference to empty result matrix covariance
    #and reference to empty result array median
    #and missing data token
    #compute square variance covariance matrix
    #Normalization is done with N-1, input error if N<2
    #compute median
    #handle missing values: skip in all computations, adjust N in mean and normalization
    #verified with matlab cov function when no missing values.
    #verified with matlab for replacing values equal to mean with missing in a pattern when only xor missing, not both missing
    
    my $Aref=shift;
    my $varcov = shift;
    my $median = shift;
    my $missing_data_token = shift;
    my $debug=0;
    my $input_error = 2;
    my $numerical_error = 1;
    my $nrow= scalar(@{$Aref});
    return $input_error if ($nrow < 2);
    my $ncol = scalar(@{$Aref->[0]});
    for (my $row=1; $row< $nrow; $row++){
		return $input_error if (scalar(@{$Aref->[$row]}) != $ncol);
    }

    my @sum = (0) x $ncol;
    my @mean = (0) x $ncol;
    my @N_array = (0) x $ncol; #smaller N if missing values

    #initialize square result matrix
    for (my $i=0; $i<$ncol; $i++){
		push(@{$varcov},[(0) x $ncol]);
    }
    @{$median} = (0) x $ncol;

    for (my $col=0; $col< $ncol; $col++){
		my @values=();
		for (my $row=0; $row< $nrow; $row++){
			unless ($Aref->[$row][$col] == $missing_data_token){
				$sum[$col] = $sum[$col] + $Aref->[$row][$col];
				$N_array[$col] = $N_array[$col]+1; 
				push(@values,$Aref->[$row][$col]);
			}
		}
		return $input_error if ($N_array[$col]<2);
		$mean[$col]=$sum[$col]/$N_array[$col];
		print "mean $col is ".$mean[$col]."\n" if $debug;
		$median->[$col]=median(\@values);
		#variance
		my $sum_errors_pow2=0;
		for (my $row=0; $row< $nrow; $row++){
			unless ($Aref->[$row][$col] == $missing_data_token){
				$sum_errors_pow2 = $sum_errors_pow2 + ($Aref->[$row][$col] - $mean[$col])**2;
			}
		}
		unless ( $sum_errors_pow2 == 0 ){
			#if sum is 0 then assume all estimates 0, just ignore
			$varcov->[$col][$col]= $sum_errors_pow2/($N_array[$col]-1);
			print "variance $col is ".$varcov->[$col][$col]."\n" if $debug;
		}

		#covariance
		#here $j is always smaller than $col, meaning that mean is already computed 
		#how handle missing in one but not the other...? 
		#if xor missing then add to N_local but not to sum_errors_prod. Is like assuming missin value is equal to mean, 
		#which is not too bad
		for (my $j=0; $j< $col; $j++){
			my $sum_errors_prod=0;
			my $N_local=0;
			for (my $i=0; $i< $nrow; $i++){
				if (($Aref->[$i][$col] != $missing_data_token) and ($Aref->[$i][$j] != $missing_data_token)){
					#have both values
					$sum_errors_prod = $sum_errors_prod + ($Aref->[$i][$col] - $mean[$col])*($Aref->[$i][$j] - $mean[$j]);
					$N_local++;
				}elsif (($Aref->[$i][$col] == $missing_data_token) xor ($Aref->[$i][$j] == $missing_data_token)){
					$N_local++;
					#have one value but not the other, pretend missing value is equal to mean.
				}
				#if both missing then just skip
			}
			unless( $sum_errors_prod == 0 ){
				#if sum is 0 then assume all estimates 0, just ignore
				$varcov->[$j][$col]= $sum_errors_prod/($N_local-1);
				$varcov->[$col][$j]=$varcov->[$j][$col]; 
			}
		}

    }
	
    return 0;
}
sub get_identity_matrix{
	my $dimension = shift;

	croak("dimension must be larger than 0 in get_identity_matrix") unless ($dimension > 0);
	my @result=();
	for (my $i=0; $i< $dimension; $i++){
		my @line = (0) x $dimension;
		$line[$i] = 1;
		push(@result,\@line);
	}
	return \@result;
}

sub invert_symmetric{
    #input is full symmetric positive definite matrix 
    #this matrix will be overwritten
	#and reference to empty result matrix
    #verified with matlab

    my $matrixref = shift;
	my $result = shift;

    my $err=cholesky($matrixref);
    if ($err > 0){
		print "cholesky error $err in invert_symmetric\n";
		return $err ;
    }
    my $refInv = [];
	my $ncol =scalar(@{$matrixref});
    $err = lower_triangular_identity_solve($matrixref,$ncol,$refInv);
    if ($err > 0){
		print "lower triang error $err in invert_symmetric\n";
		return $err ;
    }

#	for (my $i=0 ; $i< scalar(@{$refInv}); $i++){
#		for (my $j = 0; $j < scalar(@{$refInv->[$i]}); $j++){
#			print $refInv->[$i]->[$j].' ';
#		}
#		print "\n";
#	}

    $err = lower_triangular_UTU_multiply($refInv,$result);
    if ($err > 0){
		print "multiply error in invert_symmetric\n";
		return $err ;
    }

    return 0;

}
sub lower_triangular_UTU_multiply{
    #input is lower triangular matrix
    #in *column format*, R->[col][row]
    #and reference to empty solution matrix
    #multiply from the left with transpose, output is full symmetric square matrix
    #verified in matlab

    my $Rref=shift;
    my $solution = shift;
    my $input_error = 2;
    my $numerical_error = 1;
    my $ncol= scalar(@{$Rref});

    for (my $j=0;$j< $ncol;$j++){
		my $nrow = scalar(@{$Rref->[$j]});
#	print "nrow $nrow ncol $ncol\n";
		return $input_error unless ($nrow > $j);
    }

    #create zeros matrix as placeholder for solution
    for (my $i=0; $i<$ncol; $i++){
		push(@{$solution},[(0) x $ncol]);
    }

    for (my $i=0; $i< $ncol; $i++){
		for (my $j=0; $j<=$i; $j++){
			my $sum=0;
			for (my $k=$i; $k< $ncol; $k++){
				$sum=$sum+$Rref->[$i][$k]*$Rref->[$j][$k];
			}
			$solution->[$j][$i]=$sum;
			$solution->[$i][$j]=$sum;
		}
    }
    return 0;
}


sub frem_conditional_omega_block{
    #input is lower triangle, including diagonal, of symmetric positive definite matrix 
    #in *column format*, A->[col][row]
    #this matrix will be overwritten
    #input is also number of leading interesting etas
    #and reference to empty result matrix which will hold symmetric conditional omega block
    #verified with matlab

    my $omegaref = shift;
    my $n_eta = shift;
    my $result = shift;

    my $err=cholesky($omegaref);
    if ($err > 0){
		print "cholesky error $err in frem\n";
		return $err ;
    }
    my $refInv = [];
    $err = lower_triangular_identity_solve($omegaref,$n_eta,$refInv);
    if ($err > 0){
		print "lower triang error $err in frem\n";
		return $err ;
    }


    my $Rmat=[];
    $err = QR_factorize($refInv,$Rmat);
    if ($err > 0){
		print "QR error in frem\n";
		return $err ;
    }


    my $refRInv = [];
    $err = upper_triangular_identity_solve($Rmat,$refRInv);
    if ($err > 0){
		print "upper triang error in frem\n";
		return $err ;
    }
    return $err if ($err > 0);

    $err = upper_triangular_UUT_multiply($refRInv,$result);
    if ($err > 0){
		print "multiply error in frem\n";
		return $err ;
    }

    #fill in full matrix to avoid sorrows outside this function 
    my $dim = scalar(@{$result});
    for (my $row=0; $row<$dim; $row++){
		for (my $col=0; $col<$dim; $col++){
			$result->[$col][$row]=$result->[$row][$col];
		}
    }

    return 0;

}

if (0){
    my @Amatrix=();
    push(@Amatrix,[9.6900000e-02,6.7600000e-02,7.0400000e-02,-9.3500000e-01,1.9500000e+00,3.4300000e+00,-8.9000000e-03,-4.2900000e-02,3.6100000e-02,7.7700000e-03]);
    push(@Amatrix,[0,6.0600000e-02,-4.2100000e-03,-4.1300000e-01,2.5300000e+00,2.4500000e+00,-3.6600000e-02,-1.9100000e-02,1.3600000e-02,-7.8100000e-03]);
    push(@Amatrix,[0,0,2.3400000e+00,9.7000000e-01,-4.0800000e-01,-2.2500000e+00,-6.9700000e-02,-2.2500000e-01,-9.0900000e-02,7.2200000e-03]);
    push(@Amatrix,[0,0,0,6.0500000e+01,-1.7000000e+01,-7.9500000e+01,-3.5900000e-01,9.3000000e-01,-2.4200000e-01,-1.1400000e-01]);
    push(@Amatrix,[0,0,0,0,2.4800000e+02,2.3500000e+02,-3.1300000e+00,-2.6600000e-01,-4.0300000e-01,-1.6000000e+00]);
    push(@Amatrix,[0,0,0,0,0,4.7100000e+02,-2.0100000e+00,-2.1000000e+00,1.4100000e+00,-1.0600000e+00]);
    push(@Amatrix,[0,0,0,0,0,0,1.6200000e-01,-4.5400000e-03,2.0200000e-02,3.0700000e-02]);
    push(@Amatrix,[0,0,0,0,0,0,0,2.4900000e-01,-5.5100000e-02,1.8800000e-02]);
    push(@Amatrix,[0,0,0,0,0,0,0,0,2.3200000e-01,2.0500000e-02]);
    push(@Amatrix,[0,0,0,0,0,0,0,0,0,2.2800000e-01]);
    
    my $b = [1,2,3,4,5,6,7,8,9,10];

    my $result = [];
    my $err = frem_conditional_omega_block(\@Amatrix,3,$result);
    
    if (0){
	my $Rmat=[];
	my $err1 = QR_factorize(\@Amatrix,$Rmat);
	my $err2 = upper_triangular_transpose_solve($Rmat,$b);
    }

    for (my $row=0; $row<3; $row++){
	for (my $col=0; $col<3; $col++){
	    printf("  %.4f",$result->[$col][$row]); #matlab format
	}
	print "\n";
    }
    print "\n";

}
if (0){
    #to verify column_cov function
    my @Bmatrix=();
    push(@Bmatrix,[9.6900000e-02,6.7600000e-02,7.0400000e-02,-9.3500000e-01,1.9500000e+00,3.4300000e+00,-8.9000000e-03,-4.2900000e-02,3.6100000e-02,7.7700000e-03]);
    push(@Bmatrix,[0.01,6.0600000e-02,-4.2100000e-03,-4.1300000e-01,2.5300000e+00,2.4500000e+00,-3.6600000e-02,-1.9100000e-02,1.3600000e-02,-7.8100000e-03]);
    push(@Bmatrix,[0.01,0.01,2.3400000e+00,9.7000000e-01,-4.0800000e-01,-2.2500000e+00,-6.9700000e-02,-2.2500000e-01,-9.0900000e-02,7.2200000e-03]);

    my $covmatrix=[];
    my $err1 = column_cov(\@Bmatrix,$covmatrix);
    for (my $row=0; $row<3; $row++){
	for (my $col=0; $col<3; $col++){
	    printf("  %.4f",$covmatrix->[$row][$col]); #matlab format
	}
	print "\n";
    }
    print "\n";


}

if (0){
    #to verify row_cov_median function
    my @Bmatrix=();

    push(@Bmatrix,[-99,6.7600000e-02,7.0400000e-02,-9.3500000e-01,1.9500000e+00,3.4300000e+00,-8.9000000e-03,-4.2900000e-02,3.6100000e-02,7.7700000e-03]);
    push(@Bmatrix,[0.01,-99,-4.2100000e-03,-4.1300000e-01,2.5300000e+00,2.4500000e+00,-3.6600000e-02,-1.9100000e-02,1.3600000e-02,-7.8100000e-03]);
    push(@Bmatrix,[0.01,0.01,-99,9.7000000e-01,-4.0800000e-01,-2.2500000e+00,-6.9700000e-02,-2.2500000e-01,-9.0900000e-02,7.2200000e-03]);
    push(@Bmatrix,[9.6900000e-02,6.0600000e-02,2.3400000e+00,9.7000000e-01,-4.0800000e-01,-2.2500000e+00,-6.9700000e-02,-2.2500000e-01,-9.0900000e-02,7.2200000e-03]);


    my $covmatrix=[];
    my $median=[];
    my $err1 = row_cov_median(\@Bmatrix,$covmatrix,$median,'-99');
#    my $err1 = row_cov(\@Bmatrix,$covmatrix);
    for (my $row=0; $row<10; $row++){
	for (my $col=0; $col<10; $col++){
	    printf("  %.4f",$covmatrix->[$row][$col]); #matlab format
	}
	print "\n";
    }
    print "\n";

    for (my $row=0; $row<10; $row++){
	printf("  %.4f",$median->[$row]); #matlab format
    }
    print "\n";
}

1;
