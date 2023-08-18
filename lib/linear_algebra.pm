package linear_algebra;

use MouseX::Params::Validate;

use strict;
use array qw(:all);
use Math::Trig;
use math;
use include_modules;

sub subtract
{
    #Return difference between two matrices in new matrix
    my $A = shift;
    my $B = shift;

    my $C;
    for (my $row = 0; $row < scalar(@$A); $row++) {
        for (my $col = 0; $col < scalar(@{$A->[0]}); $col++) {
            $C->[$row]->[$col] = $A->[$row]->[$col] - $B->[$row]->[$col];
        }
    }

    return $C;
}

sub max
{
    #Return the maximum element of a matrix
    my $A = shift;

    my $maximum = -math::inf();

    for (my $row = 0; $row < scalar(@$A); $row++) {
        for (my $col = 0; $col < scalar(@{$A->[0]}); $col++) {
            if ($A->[$row]->[$col] > $maximum) {
                $maximum = $A->[$row]->[$col];
            }
        }
    }

    return $maximum;
}

sub absolute
{
    # In place elementwise absolute value of matrix
    my $A = shift;

    for (my $row = 0; $row < scalar(@$A); $row++) {
        for (my $col = 0; $col < scalar(@{$A->[0]}); $col++) {
            $A->[$row]->[$col] = abs($A->[$row]->[$col]);
        }
    }
}

sub pad_matrix
{
    # Augment a square matrix with unit matrix to new_size
    my $A = shift;
    my $new_size = shift;
    my $cur_size = scalar(@$A);

    for (my $row = 0; $row < $cur_size; $row++) {
        push @{$A->[$row]}, (0) x ($new_size - $cur_size);
    }
    for (my $i = scalar(@$A); $i < $new_size; $i++) {
        my @add = (0) x $new_size;
        $add[$i] = 1;
        push @$A, \@add;
    }
}

sub reduce_matrix
{
    # Reduce a square matrix to new size
    my $A = shift;
    my $new_size = shift;
    my $cur_size = scalar(@$A);

    for (my $row = 0; $row < $new_size; $row++) {
        splice @{$A->[$row]}, $new_size, $cur_size - $new_size;
    }
    splice @$A, $new_size, $cur_size - $new_size;
}

sub put_ones_on_diagonal_of_zero_lines
{
    # Replace all lines containing only zeros in matrix with ones on the diagonal
    my $A = shift;

    for (my $row = 0; $row < @$A; $row++) {
        my $all_zero = 1;
        for (my $col = 0; $col < @{$A->[$row]}; $col++) {
            if ($A->[$row]->[$col] != 0) {
                $all_zero = 0;
                last;
            }
        }
        if ($all_zero) {
            $A->[$row]->[$row] = 1;
        }
    }
}

sub triangular_symmetric_to_full
{
    # Convert a matrix in triangular vector form to full form assuming symmetry

    my $triangular = shift;
    my $A = [];

    my $row = 0;
    my $col = 0;
    foreach my $element (@$triangular) {
        $A->[$row]->[$col] = $element;
        $A->[$col]->[$row] = $element;
        $col++;
        if ($col > $row) {
            $col = 0;
            $row++;
        }
    }

    return $A;
}

sub flatten_symmetric
{
    # Flatten a symmetric into its lower triangular part
    # Assume row major
    my $A = shift;
    my @flat;

    my $take = 1;
    for my $row (@$A) {
        for (my $i = 0; $i < $take; $i++) {
            push @flat, $row->[$i];
        }
        $take++;
    }

    return \@flat;
}

sub mvnpdf_cholesky
{
    my $covar=shift;
    my $mu=shift;
    my $xvectors=shift;
    my $inflation=shift;
    my $relative=shift;

    my $ncol= scalar(@{$covar});
    my $mrow = scalar(@{$covar->[0]});
    croak("input covariance matrix to mvnpdf_cholesky is empty") unless ($ncol > 0);
    croak("input covariance matrix to mvnpdf_cholesky is not square, col $ncol rows $mrow") unless ($ncol == $mrow);
    croak("input mu to mvnpdf_cholesky undefined ") unless (defined $mu );
    croak("input mu to mvnpdf_cholesky has wrong dim ".scalar(@{$mu})) unless ($ncol == scalar(@{$mu}));
    croak("input xvectors to mvnpdf_cholesky undefined ") unless (defined $xvectors);
    croak("input xvectors to mvnpdf_cholesky is empty ") unless (scalar(@{$xvectors})>0);
    croak("input xvectors to mvnpdf_cholesky has wrong dim ".scalar(@{$xvectors->[0]})) unless ($ncol == scalar(@{$xvectors->[0]}));
    croak("input inflation to mvnpdf_cholesky has illegal length ".scalar(@{$inflation})) unless
        (scalar(@{$inflation}) == 0 or scalar(@{$inflation})== $ncol );
    croak("input relative to mvnpdf_cholesky is $relative") unless (($relative == 1) or ($relative==0));

    my @covar_copy=();
    for (my $i=0; $i<$ncol; $i++){
        $covar_copy[$i] =[0 x $ncol];
        for (my $j=0; $j<$ncol; $j++){
            $covar_copy[$i][$j] = $covar->[$i][$j];
        }
    }

    my $err=cholesky_transpose(\@covar_copy);

    croak("failed cholesky in mvnpdf_cholesky") unless ($err==0);
    my @arr=();
    for (my $i=0; $i< $ncol; $i++){
        my $value = $covar_copy[$i][$i]; #pick out diagonal elements
        unless (scalar(@{$inflation}) == 0){
            $value = $value*sqrt($inflation->[$i]);
        }
        push(@arr,$value);
    }
    my @sorted = sort { $a <=> $b } @arr; #sort ascending

    my $root_determinant = $sorted[0];
    for (my $i=1; $i< $ncol; $i++){
        $root_determinant = $root_determinant*$sorted[$i];
    }


    my @results=();

    foreach my $xvec (@{$xvectors}){
        my @diff = 0 x $ncol;
        for (my $j=0; $j< $ncol; $j++){
            $diff[$j]= ($xvec->[$j] - $mu->[$j]);
            unless (scalar(@{$inflation}) == 0){
                $diff[$j] = $diff[$j]/(sqrt($inflation->[$j]));
            }
        }
        $err = linear_algebra::upper_triangular_transpose_solve(\@covar_copy,\@diff);
        croak("failed solve in mvnpdf_cholesky") unless ($err==0);

        my @squared = ();
        for (my $i=0; $i< $ncol; $i++){
            push(@squared,($diff[$i])**2);
        }

        my @sorted = sort { $a <=> $b } @squared; #sort ascending for numerical safety
        my $sum = 0;
        for (my $i=0; $i< $ncol; $i++){
            $sum = $sum + ($sorted[$i]);
        }

        if ($relative){
            push(@results,exp(-0.5*$sum));
        }else{
            push(@results,(((2*pi)**(-$ncol/2))*(1/$root_determinant))*exp(-0.5*$sum));
        }
    }
    return \@results;
}

sub transpose
{
    # Transpose a matrix in place
    my $A = shift;
    for (my $row = 0; $row < @$A; $row++) {
        for (my $col = 0; $col < $row; $col++) {
            my $temp = $A->[$row]->[$col];
            $A->[$row]->[$col] = $A->[$col]->[$row];
            $A->[$col]->[$row] = $temp;
        }
    }
}

sub copy_and_reorder_square_matrix
{
    #copy the matrix while changing order of rows and columns according to order array

    my $A = shift;
    my $order = shift;

    my $ncol= scalar(@{$A});
    my $mrow= scalar(@{$A->[0]});
    unless ($ncol >0 and ($ncol == $mrow) and ($ncol == scalar(@{$order}))){
        print "\norder ".join(' ',@{$order})."\n";
        foreach my $line (@{$A}){
            print join(' ',@{$line})."\n";
        }
        croak("input error copy_and_rearrange_col_rows") ;
    }
    my @copy=();
    for (my $i=0; $i<$ncol; $i++){
        push(@copy,[0 x $ncol]);
        my $userow = $order->[$i];
        croak("error index $userow in order") if ($userow >= $ncol);
        for (my $j=0; $j<$ncol; $j++){
            my $usecol = $order->[$j];
            croak("error index $usecol in order") if ($usecol >= $ncol);
            $copy[$i][$j] = $A->[$userow][$usecol];
        }
    }
    return \@copy;
}

sub house
{
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

sub full_rank
{
    #assume Amatrix is row format
    my $ref =shift;
    my @Amatrix = @{$ref};

    my $full_rank=0;
    my $nparam = scalar(@{$Amatrix[0]});
    return $full_rank unless (scalar(@Amatrix)>= $nparam);

    my @Atranspose = ();
    for (my $i=0; $i<$nparam; $i++){
        push(@Atranspose,[]);
    }
    foreach my $line (@Amatrix){
        for (my $i=0; $i<$nparam; $i++){
            push(@{$Atranspose[$i]},$line->[$i]);
        }
    }
    my $Rmat=[];
    unless (QR_factorize(\@Atranspose,$Rmat)){
        $full_rank = 1;
    }
    return $full_rank;
}

sub QR_factorize
{

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

    my $singular=0;

    for (my $j=0;$j<$ncol;$j++){
        my @xvec = @{$Amatrix[$j]}[$j..$endrow];
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
        $singular = $numerical_error if (abs($rcol[-1])< 0.0000000000001);
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

    return $singular;
}

sub cholesky_of_vector_matrix
{
    #input is triangular, including diagonal, of symmetric positive definite matrix
    #as a single vector
    #output is square cholesky factor matrix with 0 on other half

    my $Aref=shift;
    my $input_error = 2;
    my $numerical_error = 1;

    my $copy = triangular_symmetric_to_full($Aref);
    my $err = cholesky_transpose($copy);
    return ($err,$copy);
}

sub cook_score_parameters
{
    my $standard_errors = shift;
    my $estimate_1 = shift;
    my $estimate_2 = shift;

    my $ncol = scalar(@{$standard_errors});
    return (2,[]) unless (
        ($ncol > 0) and
        ($ncol == scalar(@{$estimate_1})) and
        ($ncol == scalar(@{$estimate_2})));

    my @scores=();
    for (my $i=0; $i< $ncol; $i++){
        push(@scores,abs($estimate_1->[$i] - $estimate_2->[$i])/$standard_errors->[$i]);
    }
    return (0,\@scores);

}

sub cook_score_all
{
    my $cholesky = shift;
    my $estimate_1 = shift;
    my $estimate_2 = shift;

    my $input_error = 2;
    my $numerical_error = 1;

    my $ncol = scalar(@{$cholesky});
    return ($input_error,0) unless (
        ($ncol > 0) and
        ($ncol == scalar(@{$cholesky->[0]})) and
        ($ncol == scalar(@{$estimate_1})) and
        ($ncol == scalar(@{$estimate_2})));

    my @diff=();
    for (my $i=0; $i< $ncol; $i++){
        push(@diff,($estimate_1->[$i] - $estimate_2->[$i]));
    }
    my $sum_squares = 0;
    for (my $i=0; $i< $ncol; $i++){
        my $scalar_product=0;
        for (my $j=$i; $j<$ncol; $j++){
            $scalar_product += ($diff[$j] * $cholesky->[$j]->[$i]);
        }
        $sum_squares += ($scalar_product)**2;
    }
    return (0,sqrt($sum_squares));
}

sub sqrt_determinant_vector_covmatrix
{
    my $Aref = shift;
    my ($err,$copy) = cholesky_of_vector_matrix($Aref);
    return ($err,undef) unless ($err==0);
    return (0,diagonal_product($copy));
}

sub diagonal_product
{
    #the determinant of triangular matrix is product of diagonal elements
    #hence square root of determinant of positive definite matrix is product of cholesky factor diagonal
    my $matrix = shift;
    my $ncol = scalar(@{$matrix});

    my @arr=();
    for (my $i=0; $i< $ncol; $i++){
        push(@arr,$matrix->[$i]->[$i]); #pick out diagonal elements;
    }
    my @sorted = sort { $a <=> $b } @arr; #sort ascending

    my $product = $sorted[0];
    for (my $i=1; $i< $ncol; $i++){
        $product = $product*$sorted[$i];
    }
    return $product;
}

sub cholesky_transpose
{
    #input is lower triangle, including diagonal, of symmetric positive definite matrix
    #in *column format*, A->[col][row]
    my $Aref=shift;
    my $input_error = 2;
    my $numerical_error = 1;
    my $ncol= scalar(@{$Aref});
    my $mrow = scalar(@{$Aref->[0]});
    return $input_error unless ($mrow == $ncol);

    my $err = cholesky($Aref);
    return $err unless ($err==0);

    #fill up
    for (my $j=0; $j< $ncol; $j++){
        for (my $i=($j+1); $i<$ncol; $i++){
            $Aref->[$i][$j] = $Aref->[$j][$i];
            $Aref->[$j][$i]= 0;
        }
    }
    return 0;
}

sub record_index_to_letter
{
    my %parm = validated_hash(\@_,
                              index => { isa => 'Int', optional => 1 },
                              letter => { isa => 'Str', optional => 1 },
    );
    my $index = $parm{'index'};
    my $letter = $parm{'letter'};
    unless ( ((defined $index) or (defined $letter)) and (not ((defined $index) and (defined $letter)) )){
        croak("must input letter XOR index in record_index_to_letter");
    }

    my @alphabet=('A','B','C','D','E','F','G','H','I','J','K','L','M',
                  'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
                  'AA','AB','AC','AD','AE','AF','AG','AH','AI','AJ','AK','AL','AM',
                  'AN','AO','AP','AQ','AR','AS','AT','AU','AV','AW','AX','AY','AZ');

    my %inverse_alphabet;
    for (my $index=0; $index < scalar(@alphabet); $index++){
        $inverse_alphabet{$alphabet[$index]} = $index;
    }
    if (defined $index){
        croak("too high record index  ") if ($index > $#alphabet);
        return $alphabet[$index];
    }else{
        croak("undef letter $letter") unless (defined $inverse_alphabet{$letter});
        return $inverse_alphabet{$letter};
    }

}

sub string_cholesky_block
{
    my %parm = validated_hash(\@_,
                              value_matrix => { isa => 'ArrayRef', optional => 0 },
                              theta_count => { isa => 'Int', optional => 0 },
                              record_index => { isa => 'Int', optional => 0 },
                              testing => { isa => 'Bool', default => 0 },
                              bounded_theta => { isa => 'Bool', default => 1 },
                              fix => { isa => 'Bool', default => 0 },
                              correlation_cutoff => { isa => 'Num', default=> 0,optional => 1 },
                              correlation_limit => { isa => 'Num', default=> 0.9,optional => 1 },
        );
    my $value_matrix = $parm{'value_matrix'};
    my $theta_count = $parm{'theta_count'};
    my $record_index = $parm{'record_index'};
    my $testing = $parm{'testing'};
    my $bounded_theta = $parm{'bounded_theta'};
    my $fix = $parm{'fix'};
    my $correlation_cutoff = $parm{'correlation_cutoff'};
    my $correlation_limit = $parm{'correlation_limit'};

    croak("correlation_cutoff cannot be negative") if ($correlation_cutoff < 0);
    croak("correlation_limit cannot be negative") if ($correlation_limit < 0);

    my $FIX='';
    $FIX = ' FIX' if $fix;
    my $warnings = 0;

    my $letter=record_index_to_letter(index=>$record_index);

    my $dimension = scalar(@{$value_matrix});
    my @indices = ('1','2','3','4','5','6','7','8','9');
    if ($dimension > 9){
        @indices = ('01','02','03','04','05','06','07','08','09');
        for (my $i=10; $i<=$dimension; $i++){
            push(@indices,$i);
        }
    }

    my $sqrt='SQRT';
    $sqrt = 'sqrt' if $testing;
    my $par='';
    $par = '$' if $testing;
    my $term='';
    $term = ';' if $testing;
    my $sep='';
    my $sepd='_';

    my @sd_names=();
    my @corr_names=();
    my $stringmatrix=[];

    my @theta_inits=();
    my @code=();
    for (my $i=0; $i< $dimension; $i++){
        my $sdparam = $par.'SD'.$sepd.$letter.$indices[$i];
        push(@sd_names,$sdparam);
        push(@corr_names,[('') x $dimension]);
        push(@{$stringmatrix},[('')x $dimension]);
        my $init = sqrt($value_matrix->[$i]->[$i]); #sqrt of variance
        if (not $bounded_theta){
            $init = log($init);
        }
        if ($testing){
            if ($bounded_theta){
                push(@theta_inits,"$sdparam=$init;");
            }else{
                push(@theta_inits,"$sdparam=exp($init);");
            }
        }else{
            my $formatted = sprintf("%.8G",$init);
            if (($formatted == 0) and (not $fix)){
                $formatted = '0.000001';
            }
            $theta_count++;
            if ($bounded_theta){
                push(@theta_inits,'(0,'.$formatted.')'.$FIX.' ; '.$sdparam);
                push(@code,$sdparam.'=THETA('.$theta_count.')');
            }else{
                push(@theta_inits,$formatted.$FIX.' ; log '.$sdparam);
                push(@code,$sdparam.'=EXP(THETA('.$theta_count.'))');
            }
        }
        for (my $j=0; $j< $i; $j++){
            my $rho = $par.'COR'.$sepd.$letter.$indices[$i].$indices[$j];
            $corr_names[$i]->[$j]=$rho;
            my $init = ($value_matrix->[$i]->[$j])/(sqrt($value_matrix->[$i]->[$i])*sqrt($value_matrix->[$j]->[$j]));
            $warnings++ if (abs($init)> $correlation_limit);
            my $below_cutoff = 0;
            if (abs($init)<= $correlation_cutoff){
                $below_cutoff = 1;
            }
            if (not $bounded_theta){
                $init = math::correlation2unbounded($init);
            }
            if ($testing){
                if ($bounded_theta){
                    push(@theta_inits,"$rho=$init;");
                }else{
                    push(@theta_inits,"$rho=exp($init)*2/(exp($init)+1) -1;");
                }
            }else{
                if ($below_cutoff){
                    if ($bounded_theta){
                        push(@theta_inits,'0 FIX ; '.$rho.' ; initial '.$init.' <= '.$correlation_cutoff.' cutoff ');
                    }else{
                        #logit 0.5 is 0
                        push(@theta_inits,'0 FIX ; logit ('.$rho.'+1)/2 ; initial <= cutoff ');
                    }
                }else{
                    my $formatted = sprintf("%.8G",$init);
                    if (($formatted == 0) and (not $fix)){
                        $formatted = '0.000001';
                    }
                     if ($bounded_theta){
                        push(@theta_inits,'(-1,'.$formatted.',1)'.$FIX.' ; '.$rho); #ok bound if FIX?
                    }else{
                        push(@theta_inits,$formatted.$FIX.' ; logit ('.$rho.'+1)/2');
                    }
                }
                $theta_count++;
                if ($bounded_theta){
                    push(@code,$rho.'=THETA('.$theta_count.')');
                }else{
                    push(@code,$rho.'=EXP(THETA('.$theta_count.'))*2/(EXP(THETA('.$theta_count.'))+1) -1');
                }
            }
        }
    }

    for (my $i=0; $i< $dimension; $i++){
        $stringmatrix->[$i]->[$i] = '1';
        for (my $j=0; $j< $i; $j++){
            $stringmatrix->[$i]->[$j] = $corr_names[$i]->[$j];
            $stringmatrix->[$j]->[$i] = $corr_names[$i]->[$j]; #symmetry
        }
    }


    #Golub p144 Alg 4.2.1

    if (1){
        for (my $j=0; $j< $dimension; $j++){
            if ($j>0) {
                #i=j
                my $sum=$stringmatrix->[0][$j].'**2';
                for (my $k=1; $k<$j ; $k++){
                    $sum=$sum.'+'.$stringmatrix->[$k][$j].'**2';
                }
                my $diff = $stringmatrix->[$j][$j].'-('.$sum.')';
                $stringmatrix->[$j][$j]=$sqrt.'('.$diff.')';
                #i=j+1:n
                my $newvar=$par.'CH'.$sepd.$letter.$indices[$j].$indices[$j];
                push(@code,$newvar.'='.$stringmatrix->[$j][$j].$term);
                $stringmatrix->[$j][$j] = $newvar;
                for (my $i=($j+1); $i<$dimension; $i++){
                    my $sum=$stringmatrix->[0][$j].'*'.$stringmatrix->[0][$i];
                    my $parleft='';
                    my $parright='';
                    for (my $k=1; $k<$j ; $k++){
                        $parleft='(';
                        $parright=')';
                        $sum=$sum.'+'.$stringmatrix->[$k][$j].'*'.$stringmatrix->[$k][$i];
                    }
                    $stringmatrix->[$j][$i]='('.$stringmatrix->[$j][$i].'-'.$parleft.$sum.$parright.')/'.$stringmatrix->[$j][$j];
                }

                #create intermediate variable to replace with
                #only changing [j][j] and [j][i] where i>j
                for (my $i=($j+1); $i<$dimension; $i++){
                    my $newvar=$par.'CH'.$sepd.$letter.$indices[$i].$indices[$j];
                    push(@code,$newvar.'='.$stringmatrix->[$j][$i].$term);
                    $stringmatrix->[$j][$i] = $newvar;
                }
            } else {
                unless ($testing){
                    push(@code,';Comments below show CH variables for 1st column, too simple to need new variables');
                    for (my $i=$j; $i<$dimension; $i++){
                        my $newvar=$par.'CH'.$sepd.$letter.$indices[$i].$indices[$j];
                        push(@code,';'.$newvar.'='.$stringmatrix->[$j][$i].$term);
                    }
                }
            }
        }
    }

    if (1){
        for(my $i=0;$i<$dimension;$i++){
            for (my $j=0; $j<=$i; $j++){
                my $sdi = $sd_names[$i];
                my $par = 'CHOL_'.$letter.'_'.($i+1).'_'.($j+1);
                my $left='(';
                my $right=')';
                if ($j < 1){
                    $left='';
                    $right='';
                }
                if ($i==0 and $j==0){
                    $stringmatrix->[$j]->[$i]= $sdi;
                }else{
                    $stringmatrix->[$j]->[$i] = $stringmatrix->[$j]->[$i].'*'.$sdi;
                    $stringmatrix->[$i]->[$j] = $stringmatrix->[$j]->[$i];
                }
            }
        }
    }

    return ($stringmatrix,\@theta_inits,\@code,$warnings);

}

sub string_cholesky_diagonal
{
    my %parm = validated_hash(\@_,
                              value_matrix => { isa => 'ArrayRef', optional => 0 },
                              fix_vector => { isa => 'ArrayRef', optional => 0 },
                              theta_count => { isa => 'Int', optional => 0 },
                              record_index => { isa => 'Int', optional => 0 },
                              testing => { isa => 'Bool', default => 0 },
                              bounded_theta => { isa => 'Bool', default => 1 },
                              reparameterize_fix => { isa => 'Bool', default => 0 },
        );
    my $value_matrix = $parm{'value_matrix'};
    my $fix_vector = $parm{'fix_vector'};
    my $theta_count = $parm{'theta_count'};
    my $record_index = $parm{'record_index'};
    my $testing = $parm{'testing'};
    my $bounded_theta = $parm{'bounded_theta'};
    my $reparameterize_fix = $parm{'reparameterize_fix'};

    my $letter=record_index_to_letter(index=>$record_index);


    my @FIX=();
    foreach my $f (@{$fix_vector}){
        if ($f){
            push(@FIX,' FIX');
        }else{
            push(@FIX,'');
        }
    }

    my $dimension = scalar(@{$value_matrix});
    my @indices = ('1','2','3','4','5','6','7','8','9');
    if ($dimension > 9){
        @indices = ('01','02','03','04','05','06','07','08','09');
        for (my $i=10; $i<=$dimension; $i++){
            push(@indices,$i);
        }
    }

    my $sqrt='SQRT';
    $sqrt = 'sqrt' if $testing;
    my $par='';
    $par = '$' if $testing;
    my $term='';
    $term = ';' if $testing;
    my $sep='';
    my $sepd='_';

    my $stringarray=[];

    my @theta_inits=();
    my @code=();
    for (my $i=0; $i< $dimension; $i++){
        if ($fix_vector->[$i] and (not $reparameterize_fix)){
            push(@{$stringarray},undef);
            next;
        }
        my $sdparam = $par.'SD'.$sepd.$letter.$indices[$i];
        push(@{$stringarray},$sdparam);
        my $init = sqrt($value_matrix->[$i]); #sqrt of variance
        if (not $bounded_theta){
            $init = log($init);
        }
        if ($testing){
            if ($bounded_theta){
                push(@theta_inits,"$sdparam=$init;");
            }else{
                push(@theta_inits,"$sdparam=exp($init);");
            }
        }else{
            my $formatted = sprintf("%.8G",$init);
            if (($formatted == 0) and (length($FIX[$i])==0)){
                $formatted = '0.000001';
            }
            $theta_count++;
            if ($bounded_theta){
                push(@theta_inits,'(0,'.$formatted.')'.$FIX[$i].' ; '.$sdparam);
                push(@code,$sdparam.'=THETA('.$theta_count.')');
            }else{
                push(@theta_inits,$formatted.$FIX[$i].' ; log '.$sdparam);
                push(@code,$sdparam.'=EXP(THETA('.$theta_count.'))');
            }
        }
    }
    return ($stringarray,\@theta_inits,\@code);

}

sub get_inverse_parameter_list
{
    #cut out code between start tag and end tag
    my %parm = validated_hash(\@_,
                              code => { isa => 'ArrayRef', optional => 0 },
        );
    my $code = $parm{'code'};

    my $bounded_theta;
    my %etaparams;
    my %epsparams;
    my %thetaparams;
    my %record_indices;
    foreach my $line (@{$code}){
        if ($line =~ /^\s*ETA_(\d+)\s*=/){
            $etaparams{$1}=1;
        }elsif ($line =~ /^\s*EPS_(\d+)\s*=/){
            $epsparams{$1}=1;
        }elsif ($line =~ /^\s*(SD|COR)_([A-Z]+)\d+\s*=THETA\((\d+)\)/){
            #bounded
            $thetaparams{$3}=1;
            $record_indices{record_index_to_letter(letter => $2)}=1;
            if (defined $bounded_theta){
                if ($bounded_theta == 0){
                    #have found mix of bounded and unbounded
                    $bounded_theta = -1;
                }
            }else{
                $bounded_theta = 1;
            }
        }elsif ($line =~ /^\s*(SD|COR)_([A-Z]+)\d+\s*=EXP\(THETA\((\d+)\)/){
            #unbounded
            $thetaparams{$3}=1;
            $record_indices{record_index_to_letter(letter => $2)}=1;
            if (defined $bounded_theta){
                if ($bounded_theta == 1){
                    #have found mix of bounded and unbounded
                    $bounded_theta = -1;
                }
            }else{
                $bounded_theta = 0;
            }
        }

    }
    my @etalist = (sort {$a <=> $b} keys %etaparams);
    my @epslist = (sort {$a <=> $b} keys %epsparams);
    my @thetalist = (sort {$a <=> $b} keys %thetaparams);
    my @recordlist = (sort {$a <=> $b} keys %record_indices);

    return {'ETA' => \@etalist, 'EPS' => \@epslist, 'THETA' => \@thetalist, 'RECORD' => \@recordlist, 'bounded_theta' => $bounded_theta};

}

sub substitute_etas
{
    my %parm = validated_hash(\@_,
                              code => { isa => 'ArrayRef', optional => 0 },
                              eta_list => { isa => 'ArrayRef', optional => 0 },
                              sigma => { isa => 'Bool', default => 0 },
                              inverse => { isa => 'Bool', default => 0 },
        );
    my $code = $parm{'code'};
    my $eta_list = $parm{'eta_list'};
    my $sigma = $parm{'sigma'};
    my $inverse = $parm{'inverse'};

    foreach my $num (@{$eta_list}){
        if ($sigma){
            #error check
            if ($inverse){
                foreach (@{$code}){
                    if (/\bEPS\($num\)/ ){
                        croak("found parameter named EPS($num) in model to inverse cholesky reparameterize, ".
                              "something must have gone wrong ");
                    }
                }
            }else{
                foreach (@{$code}){
                    if (/\bEPS_$num\b/ ){
                        croak("found parameter named EPS_$num in model to cholesky reparameterize, ".
                              "this parameter name is reserved and must be replaced before ".
                              "reparameterization");
                    }
                }
            }
            #substitute
            if ($inverse){
                foreach (@{$code}){
                    s/\bEPS_$num\b/EPS($num)/g;
                }
            }else{
                foreach (@{$code}){
                    s/\bEPS\($num\)/EPS_$num/g;
                }
            }
        }else{
            #error check
            if ($inverse){
                foreach (@{$code}){
                    if (/\bETA\($num\)/ ){
                        croak("found parameter named ETA($num) in model to inverse cholesky reparameterize, ".
                              "something must have gone wrong ");
                    }
                }
            }else{
                foreach (@{$code}){
                    if (/\bETA_$num\b/ ){
                        croak("found parameter named ETA_$num in model to cholesky reparameterize, ".
                              "this parameter name is reserved and must be replaced before ".
                              "reparameterization");
                    }
                }
            }
            #substitute
            if ($inverse){
                foreach (@{$code}){
                    s/\bETA_$num\b/ETA($num)/g;
                }
            }else{
                foreach (@{$code}){
                    s/\bETA\($num\)/ETA_$num/g;
                }
            }
        }
    }

}

sub eta_cholesky_code
{
    my %parm = validated_hash(\@_,
                              stringmatrix => { isa => 'ArrayRef', optional => 0 },
                              eta_count => { isa => 'Int', optional => 0 },
                              diagonal => { isa => 'Bool', optional => 0 },
                              sigma => { isa => 'Bool', default => 0 },
        );
    my $stringmatrix = $parm{'stringmatrix'};
    my $eta_count = $parm{'eta_count'};
    my $diagonal = $parm{'diagonal'};
    my $sigma = $parm{'sigma'};


    my $ETA = 'ETA';
    $ETA = 'EPS' if $sigma;
    my @etalist=();
    my @code=();
    my $dimension = scalar(@{$stringmatrix});
    my @nums=();
    for (my $i=0; $i< $dimension; $i++){
        push(@nums,($eta_count+$i+1));
    }
    for (my $i=0; $i< $dimension; $i++){
        my $line = $ETA.'_'.$nums[$i].'=';
        if ($diagonal){
            next unless (defined $stringmatrix->[$i]);
            $line .= $ETA.'('.$nums[$i].')*'.$stringmatrix->[$i];
        }else{
            for (my $j=0; $j<=$i; $j++){
                $line .= '+' if ($j>0);
                $line .= $ETA.'('.$nums[$j].')*'.$stringmatrix->[$i]->[$j];
            }
        }
        push(@etalist,$nums[$i]);
        push(@code,$line);
    }


    return ($dimension,\@code,\@etalist);
}

sub cholesky
{
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
            return $numerical_error unless (($diff > 0) and math::usable_number($diff));
            $Aref->[$j][$j]=sqrt($diff);
            return $numerical_error unless ($Aref->[$j][$j] > 0);
            #i=j+1:n
            for (my $i=($j+1); $i<$ncol; $i++){
                my $sum=0;
                for (my $k=0; $k<$j ; $k++){
                    $sum=$sum+($Aref->[$k][$j])*($Aref->[$k][$i]);
                }
                $Aref->[$j][$i]=($Aref->[$j][$i]-$sum)/($Aref->[$j][$j]);
            }
        } else {
            return $numerical_error unless (($Aref->[0][0] > 0) and math::usable_number($Aref->[0][0]));
            $Aref->[0][0]=sqrt($Aref->[0][0]);
            unless ($Aref->[0][0] > 0){
                print "cholesky leading element not gt 0";
                return $numerical_error ;
            }
            for (my $i=1; $i< $ncol; $i++){
                $Aref->[0][$i]=$Aref->[0][$i]/($Aref->[0][0]);
            }
        }
    }

    return 0;
}

sub is_matrix_posdef
{
    my %parm = validated_hash(\@_,
        matrix => { isa => 'ArrayRef', optional => 0 },
    );
    my $matrix = $parm{'matrix'};

    #copy and try cholesky decomposition
    my @copy = map { [@$_] } @{$matrix};
    return !linear_algebra::cholesky(\@copy);
}

sub LU_factorization
{
    my $A_matrix = shift;
    my @A_temp = @{$A_matrix};

    #in *row format*, A->[row][col]
    #this matrix is overwritten with L(without the identity diagonal elements) and U
    #Golub p92 Algorithm 3.2.2

    my $i = 0;
    while($A_temp[$i][$i] != 0 && $i < $#A_temp + 1) {
        for(my $j = 0; $j < $#A_temp + 1; $j++) {
            for(my $k = 0; $k < $#A_temp + 1; $k++) {
            }
        }
        my @A_copy = map { [@$_] } @A_temp;

        for (my $j = $i + 1; $j < $#A_temp + 1; $j++) {
            my $tau = $A_temp[$j][$i] / $A_temp[$i][$i];
            for (my $k = $i + 1; $k < $#A_temp + 1; $k++) {
                $A_temp[$j][$k] = $A_temp[$j][$k] - $tau * $A_copy[$i][$k];
            }
            $A_temp[$j][$i] = $tau;
        }
        $i++;
    }
    if ($i != ($#A_temp)) {
        die "LU factorization failed. \n";
    }
    return 0;
}

sub get_symmetric_posdef
{
    # gets a symmetric positive definite matrix from adjusting small or negative negative
    # eigenvalues to a small, positive number
    my %parm = validated_hash(\@_,
        matrix => { isa => 'ArrayRef', optional => 0 },
        minEigen => { isa => 'Num', optional => 1, default => 1E-10 },
    );
    my $A = $parm{'matrix'};
    my $minEigen = $parm{'minEigen'};

    (my $eigenvalues, my $Q) = eigenvalue_decomposition($A);

    my $count = count_lower(array => $eigenvalues, limit => $minEigen);

    if ($count > 0) {
        my ($posdef, $diff) = spdarise(
            matrix => $A,
            eigenvalues => $eigenvalues,
            Q => $Q,
            minEigen => $minEigen,
        ); #new A,frob norm diff
        return ($posdef, $count);
    } else {
        return($A, 0);
    }
}

sub spdarise
{
    #Takes a symmetric matrix and modify it to the symmetric positive definite
    #matrix with the specified minimum eigen value (probably the closest to the
    #original matrix in Frobenius norm sense).
    my %parm = validated_hash(\@_,
        matrix => { isa => 'ArrayRef', optional => 0 },
        eigenvalues => { isa => 'ArrayRef', optional => 1 },
        Q => { isa => 'ArrayRef', optional => 1 },
        minEigen => { isa => 'Num', default => 0.0000000001 },
    );
    my $matrix = $parm{'matrix'};
    my $eigenvalues = $parm{'eigenvalues'};
    my $Q = $parm{'Q'};
    my $minEigen = $parm{'minEigen'};

    unless (defined $Q and defined $eigenvalues) {
        ($eigenvalues, $Q) = eigenvalue_decomposition($matrix);
    }
    unless ($minEigen > 0) {
        croak("minEigen $minEigen is not > 0");
    }

    my @posdefmatrix = map { [@$_] } @$matrix;
    my @tempA = map { [@$_] } @$matrix;

    for (my $index = 0; $index < scalar(@$eigenvalues); $index++) {
        if ($eigenvalues->[$index] < $minEigen) {
            $eigenvalues->[$index] = $minEigen;
        }
    }

    for (my $index1 = 0; $index1 < scalar(@{$matrix}); $index1++) {
        for (my $index2 = 0; $index2 < scalar(@{$matrix}); $index2++) {
            $tempA[$index1]->[$index2] = $Q->[$index1]->[$index2] * $eigenvalues->[$index2];
        }
    }
    my $fNormDiff = 0;
    for (my $index1 = 0; $index1 < scalar(@{$matrix}); $index1++) {
        for (my $index2 = 0; $index2 < scalar(@{$matrix}); $index2++) {
            $posdefmatrix[$index1]->[$index2] = 0;
            for (my $index3 = 0; $index3 < scalar(@{$matrix}); $index3++) {
                $posdefmatrix[$index1]->[$index2] =
                    $posdefmatrix[$index1]->[$index2] +  $tempA[$index1]->[$index3]*$Q->[$index2]->[$index3];
            }
            $fNormDiff = $fNormDiff + ($posdefmatrix[$index1]->[$index2]-$matrix->[$index1]->[$index2]) * ($posdefmatrix[$index1]->[$index2]-$matrix->[$index1]->[$index2]) ;
        }
    }

    return(\@posdefmatrix, $fNormDiff);
}

sub eigenvalue_decomposition
{
    # Perfor an eigenvalue decomposition of a symmetric matrix
    # using the Jacoby algorithm
    my $A = shift;

    my @eigenValMatrix = map { [@$_] } @$A;

    my $maxInd1 = 0;
    my $maxInd2 = 1;
    my $extremeVal = 10000;
    my $counter = 0;

    my @G;

    #initialise G to identity matrix
    for (my $index1 = 0; $index1 < scalar(@eigenValMatrix); $index1++) {
        for (my $index2 = 0; $index2 < $index1; $index2++) {
            $G[$index1][$index2] = 0;
            $G[$index2][$index1] = 0;
        }
        $G[$index1][$index1] = 1;
    }

    while (abs($extremeVal) > 0.000000000001 and $counter < 1000000) {

        for (my $index1 = 0; $index1 < scalar(@eigenValMatrix); $index1++) {
            for (my $index2 = $index1 + 1; $index2 < scalar(@eigenValMatrix); $index2++) {
                if ((abs($eigenValMatrix[$maxInd1][$maxInd2]) < abs($eigenValMatrix[$index1][$index2])) and ($index1 != $index2)) {
                    $maxInd1 = $index1;
                    $maxInd2 = $index2;
                    $extremeVal = $eigenValMatrix[$maxInd1][$maxInd2];
                }
            }
        }

        $extremeVal = $eigenValMatrix[$maxInd1][$maxInd2];

        my $theta;
        my $divisor = $eigenValMatrix[$maxInd2][$maxInd2] - $eigenValMatrix[$maxInd1][$maxInd1];
        if ($divisor == 0) {
            if ($eigenValMatrix[$maxInd1][$maxInd2] > 0) {
                $theta = pi / 4;
            } else {
                $theta = -(pi) / 4;
            }
        } else {
            $theta = (atan(2 * $eigenValMatrix[$maxInd1][$maxInd2] / ($eigenValMatrix[$maxInd2][$maxInd2] - $eigenValMatrix[$maxInd1][$maxInd1]))) / 2;
        }

        my $c = cos($theta);
        my $s = sin($theta);

        my @G_copy = map { [@$_] } @G;
        my @R_copy = map { [@$_] } @eigenValMatrix;

        for (my $index = 0; $index < scalar(@G); $index++) {
            $G[$maxInd1][$index] = $c * $G_copy[$maxInd1][$index] - $s * $G_copy[$maxInd2][$index];
            $G[$maxInd2][$index] = $s * $G_copy[$maxInd1][$index] + $c * $G_copy[$maxInd2][$index];
        }

        for (my $index = 0; $index < scalar(@eigenValMatrix); $index++) {
            $eigenValMatrix[$maxInd1][$index] = $c * $R_copy[$maxInd1][$index] - $s * $R_copy[$maxInd2][$index];
            $eigenValMatrix[$maxInd2][$index] = $s * $R_copy[$maxInd1][$index] + $c * $R_copy[$maxInd2][$index];
        }

        my @r1;
        my @r2;

        for (my $index = 0; $index < scalar(@eigenValMatrix); $index++) {
            $r1[$index] = $eigenValMatrix[$index][$maxInd1];
            $r2[$index] = $eigenValMatrix[$index][$maxInd2];
        }

        my @R_copy2 = map { [@$_] } @eigenValMatrix;

        for (my $index = 0; $index < scalar(@eigenValMatrix); $index++) {
            $eigenValMatrix[$index][$maxInd1] = $c * $R_copy2[$index][$maxInd1] - $s * $R_copy2[$index][$maxInd2];
            $eigenValMatrix[$index][$maxInd2] = $s * $R_copy2[$index][$maxInd1] + $c * $R_copy2[$index][$maxInd2];
        }

        $counter = $counter + 1;
    }

    my @eigenValues;

    for (my $index = 0; $index < scalar(@eigenValMatrix); $index++) {
        $eigenValues[$index] = $eigenValMatrix[$index][$index];
    }

    transpose(\@G);

    return (\@eigenValues, \@G);
}

sub upper_triangular_transpose_solve
{
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

sub upper_triangular_identity_solve
{
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

sub column_cov
{
    #input is reference to values matrix
    #in *column format*, Aref->[col][row]
    #and reference to empty result matrix
    #compute square variance covariance matrix
    #Normalization is done with N-1, input error if N<2

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

sub cap_correlation
{
    my $varcov=shift;
    my $capcorr = shift;

    my $input_error = -2;
    my $numerical_error = -1;

    if (defined $capcorr){
        return ($input_error,undef,[]) if (($capcorr > 1) or ($capcorr < 0));
    }

    my $nrow= scalar(@{$varcov});
    return ($input_error,undef,[]) if ($nrow < 1);

    my $modified=0;
    my $max_correlation=0;
    my @sd =();
    my @indices=(0,0);
    my @cap_indices=();
    for (my $row=0; $row< $nrow; $row++){
        return ($input_error,undef,[]) if (scalar(@{$varcov->[$row]}) != $nrow);
        if ($varcov->[$row]->[$row] <= 0){
            return ($input_error,undef,[]) ;
        }else{
            push(@sd,sqrt($varcov->[$row]->[$row]));
        }
    }
    for (my $row=0; $row< $nrow; $row++){
        for (my $col=0; $col< $row; $col++){
            my $corr = $varcov->[$row]->[$col]/($sd[$row]*$sd[$col]);
            if ((defined $capcorr) and (abs($corr) > $capcorr)){
                $modified++;
                push(@cap_indices,[$row,$col,$corr]);
                my $sign = 1;
                $sign = -1 if ($corr < 0);
                $varcov->[$row]->[$col] = $sign*($sd[$row]*$sd[$col])*$capcorr;
                $varcov->[$col]->[$row] = $varcov->[$row]->[$col];
                $corr = $sign*$capcorr;
            }
            if (abs($corr) > abs($max_correlation)){
                $max_correlation = $corr;
                $indices[0]=$row;
                $indices[1]=$col;
            }
        }
    }
    return ($modified,$max_correlation,\@indices,\@cap_indices);

}

sub covar2sdcorr
{
    my $varcov=shift;
    my $sdcorr = shift;
    my $input_error = 2;
    my $numerical_error = 1;

    my $nrow= scalar(@{$varcov});
    return $input_error if ($nrow < 1);
    my $ncol = scalar(@{$varcov->[0]});
    my @sd =();

    for (my $row=0; $row< $nrow; $row++){
        return $input_error if (scalar(@{$varcov->[$row]}) != $ncol);
        if ($varcov->[$row]->[$row] <= 0){
            return $input_error ;
        }else{
            push(@sd,sqrt($varcov->[$row]->[$row]));
        }
    }
    for (my $row=0; $row< $nrow; $row++){
        push(@{$sdcorr},[]);
        for (my $col=0; $col< $row; $col++){
            push(@{$sdcorr->[$row]},$sdcorr->[$col]->[$row]);
        }
        #diag
        push(@{$sdcorr->[$row]},$sd[$row]);
        for (my $col=($row+1); $col< $nrow; $col++){
            push(@{$sdcorr->[$row]},$varcov->[$row]->[$col]/($sd[$row]*$sd[$col]));
        }
    }
    return 0;
}

sub jackknife_inv_cholesky_mean_det
{

    my $Aref=shift;
    my $inv_cholesky = shift;
    my $mean = shift;
    my $stderr = shift;
    my $full_cov = shift;

    my $input_error = 2;
    my $numerical_error = 1;
    my $temprow= scalar(@{$Aref});
    return ($input_error,undef) if ($temprow < 2);
    my $ncol = scalar(@{$Aref->[0]});
    for (my $row=1; $row< $temprow; $row++){
        return ($input_error,undef) if (scalar(@{$Aref->[$row]})>0 and scalar(@{$Aref->[$row]}) != $ncol);
    }

    my @sum = (0) x $ncol;
    @{$mean} = (0) x $ncol;
    @{$stderr} = (0) x $ncol;
    for (my $col=0; $col< $ncol; $col++){
        my @line = (0) x $ncol;
        push(@{$full_cov},\@line);
    }
    my @centered = ();

    my $nrow=0;
    for (my $row=0; $row< $temprow; $row++){
        next unless (scalar(@{$Aref->[$row]})>0);
        $nrow++;
        for (my $col=0; $col< $ncol; $col++){
            $sum[$col] = $sum[$col] + $Aref->[$row][$col];
        }
    }
    return ($input_error,undef) if ($nrow < 2);
    for (my $col=0; $col< $ncol; $col++){
        $mean->[$col]=$sum[$col]/$nrow;
    }

    my $normfactor = sqrt(($nrow-1)/$nrow); #jackknife

    for (my $col=0; $col< $ncol; $col++){
        push(@centered,[]); #column format
        my $sum_squared_errors=0;
        for (my $row=0; $row< $temprow; $row++){
            next unless (scalar(@{$Aref->[$row]})>0);
            push(@{$centered[-1]},($Aref->[$row][$col]-$mean->[$col]));
            $sum_squared_errors += ($Aref->[$row][$col]-$mean->[$col])**2;
        }
        $stderr->[$col]=sqrt($sum_squared_errors)*$normfactor;
        $full_cov->[$col]->[$col]=$sum_squared_errors*($normfactor**2);
    }
    for (my $col1=0; $col1< $ncol; $col1++){
        for(my $col2=0; $col2<$col1; $col2++){
            my $sum_squared_errors=0;
            for (my $row=0; $row< $nrow; $row++){
                $sum_squared_errors += ($centered[$col1]->[$row])*($centered[$col2]->[$row]);
            }
            $full_cov->[$col1]->[$col2]=$sum_squared_errors*($normfactor**2);
            $full_cov->[$col2]->[$col1]=$full_cov->[$col1]->[$col2];
        }
    }

    my $Rmat=[];

    my $err = QR_factorize(\@centered,$Rmat);
    return ($err,undef) unless($err == 0);


    my $det = abs(diagonal_product($Rmat))*($normfactor**$ncol);
    return ($numerical_error,undef) unless ($det > 0);
    my $inv_determinant = 1/$det;

    #inverse R
    my $refRInv = [];
    $err = upper_triangular_identity_solve($Rmat,$refRInv);
    return ($err,undef) unless($err == 0);

    for (my $row=0; $row< $ncol; $row++){
        my @line =(0) x $ncol;
        push(@{$inv_cholesky},\@line);
        for (my $col=0; $col<= $row; $col++){
            $inv_cholesky->[$row]->[$col]= ($refRInv->[$row]->[$col])/$normfactor;
        }
    }

    return (0,$inv_determinant);
}

sub row_cov
{
    #input is reference to values matrix
    #in *row format*, Aref->[row][col]
    #and reference to empty result matrix
    #compute square variance covariance matrix
    #Normalization is done with N-1, input error if N<2

    my $Aref=shift;
    my $varcov = shift;
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
        #variance
        my $sum_errors_pow2=0;
        for (my $row=0; $row< $nrow; $row++){
            $sum_errors_pow2 = $sum_errors_pow2 + ($Aref->[$row][$col] - $mean[$col])**2;
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

sub row_cov_median_mean
{
    #input is reference to values matrix
    #in *row format*, Aref->[row][col]
    #and reference to empty result matrix covariance
    #and reference to empty result array median
    #and reference to empty result array mean
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
    my $mean = shift;
    my $missing_data_token = shift;
    my $input_error = 2;
    my $numerical_error = 1;
    my $nrow= scalar(@{$Aref});
    return $input_error if ($nrow < 2);
    my $ncol = scalar(@{$Aref->[0]});
    for (my $row=1; $row< $nrow; $row++){
        return $input_error if (scalar(@{$Aref->[$row]}) != $ncol);
    }

    my @sum = (0) x $ncol;
    push(@{$mean}, (0) x $ncol);
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
        $mean->[$col]=$sum[$col]/$N_array[$col];
        $median->[$col]=median(\@values);
        #variance
        my $sum_errors_pow2=0;
        for (my $row=0; $row< $nrow; $row++){
            unless ($Aref->[$row][$col] == $missing_data_token){
                $sum_errors_pow2 = $sum_errors_pow2 + ($Aref->[$row][$col] - $mean->[$col])**2;
            }
        }
        unless ( $sum_errors_pow2 == 0 ){
            #if sum is 0 then assume all estimates 0, just ignore
            $varcov->[$col][$col]= $sum_errors_pow2/($N_array[$col]-1);
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
                    $sum_errors_prod = $sum_errors_prod + ($Aref->[$i][$col] - $mean->[$col])*($Aref->[$i][$j] - $mean->[$j]);
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

sub get_identity_matrix
{
    my $dimension = shift;

    croak("dimension must be larger than 0 in get_identity_matrix") unless ($dimension > 0);
    my @result = ();
    for (my $i = 0; $i < $dimension; $i++) {
        my @line = (0) x $dimension;
        $line[$i] = 1;
        push(@result, \@line);
    }
    return \@result;
}

1;
