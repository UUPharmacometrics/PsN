package simeval_util;

use strict;
use include_modules;
use MouseX::Params::Validate;
use nmtablefile;
use array qw(any_nonzero max min find_zeros unique get_positions get_intersection is_zero);

our $missing=-99;

sub find_zero_etas
{
    my %parm = validated_hash(\@_,
                              filename => { isa => 'Str', optional => 0 },
                              eta_headers => { isa => 'ArrayRef', optional => 0 },
        );
    my $filename = $parm{'filename'};
    my $eta_headers = $parm{'eta_headers'};

    my %diagnostics=();

    my $nmtablefile = nmtablefile->new(filename => $filename);

    my @merged_indices=();
    my @headers_none_missing = ();
    my $length = scalar(@{$nmtablefile->tables->[0]->get_column(name=> 'ID')});
    croak("length ID col is 0") if ($length == 0);
    my @all_missing = (0 .. ($length-1));

    #$missing_matrix
    # reference to array  [over columns][over individuals]

    my @is_zero = ();
    foreach my $header (@{$eta_headers}){
        my $indices = find_zeros($nmtablefile->tables->[0]->get_column(name=> $header));
        push(@is_zero,is_zero($nmtablefile->tables->[0]->get_column(name=> $header)));
        if (scalar(@{$indices})==0){
            push(@headers_none_missing,$header);
            @all_missing = ();
        }else{
            push(@merged_indices,@{$indices});
            @all_missing = @{get_intersection(arr1 => $indices, arr2 =>\@all_missing)};
        }
    }

    my @any_missing = @{unique(\@merged_indices)};
    my @complete_after_filter = ();
    if (scalar(@any_missing) < $length){
        @complete_after_filter = @{$eta_headers};
    }

    $diagnostics{'row_index_all_missing'}=\@all_missing; #intersection of zero_indices, double check with OBJ?
    $diagnostics{'row_index_any_missing'}=\@any_missing; #union of zero_indices
    $diagnostics{'ETA_none_missing'}=\@headers_none_missing; #set of headers where indices array is empty
    $diagnostics{'ETA_none_missing_after_filter'}=\@complete_after_filter; # all headers if length of row_index_any_missing is shorter than original column, otherwise empty array
    $diagnostics{'is_zero'}=\@is_zero;

    return \%diagnostics;
}

sub get_nmtabledata
{
    my %parm = validated_hash(\@_,
                              filenames => { isa => 'ArrayRef', optional => 0 },
                              header_strings_array => { isa => 'ArrayRef', optional => 0 },
                              values_matrix_array => { isa => 'ArrayRef', optional => 0 },
                              mean_matrix_array => { isa => 'ArrayRef', optional => 0 },
                              filter_all_zero_array => { isa => 'ArrayRef', optional => 0 },
                              init_only_array => { isa => 'ArrayRef', optional => 0 },
        );
    my $filenames = $parm{'filenames'};
    my $header_strings_array = $parm{'header_strings_array'};
    my $values_matrix_array = $parm{'values_matrix_array'};
    my $mean_matrix_array = $parm{'mean_matrix_array'};
    my $filter_all_zero_array = $parm{'filter_all_zero_array'};
    my $init_only_array = $parm{'init_only_array'};

    my $input_error = 2;
    my $file_read_error = 1;

    return $input_error unless (scalar(@{$filenames})>0);

    unless (scalar(@{$header_strings_array}) == scalar(@{$values_matrix_array})){
        croak('header_strings_array and values_matrix_array not same length');
    }
    unless (scalar(@{$header_strings_array}) == scalar(@{$mean_matrix_array})){
        croak('header_strings_array and mean_matrix_array not same length');
    }
    unless (scalar(@{$header_strings_array}) == scalar(@{$filter_all_zero_array})){
        croak('header_strings_array and filter_all_zero not same length');
    }
    unless (scalar(@{$header_strings_array}) == scalar(@{$init_only_array})){
        croak('header_strings_array and init_only_array not same length');
    }

    my $init = 1;
    foreach my $file (@{$filenames}){
        my $nmtablefile = nmtablefile->new(filename => $file);
        for (my $i=0; $i< scalar(@{$header_strings_array}); $i++){
            my $filter_all_zero = 0;
            if ($init){
                $filter_all_zero = $filter_all_zero_array->[$i];
            }else{
                next if ($init_only_array->[$i]);
            }
            my $ok = add_columns_ids_samples(nmtablefile => $nmtablefile,
                                             header_strings => $header_strings_array->[$i],
                                             values_matrix => $values_matrix_array->[$i],
                                             sum_matrix => $mean_matrix_array->[$i], #this can be undef
                                             filter_all_zero => $filter_all_zero,
                                             init => $init);
        }
        $init = 0;
    }
    for (my $k=0; $k< scalar(@{$header_strings_array}); $k++){
        if (defined $mean_matrix_array->[$k]){
            #mean now contains sums
            #header 0 individual 0 , -1 for original
            return $file_read_error unless ((defined $values_matrix_array->[$k])
                                            and (defined $values_matrix_array->[$k][0])
                                            and (defined $values_matrix_array->[$k][0][0]));
            my $sample_count = scalar(@{$values_matrix_array->[$k][0][0]})-1;
            return $file_read_error unless ($sample_count > 0);
            for (my $j=0;$j<scalar(@{$header_strings_array->[$k]});$j++){
                for (my $i=0;$i<scalar(@{$mean_matrix_array->[$k][$j]});$i++){
                    #loop over individuals, change sums to means
                    $mean_matrix_array->[$k][$j][$i] = $mean_matrix_array->[$k][$j][$i]/$sample_count;
                }
            }
        }
    }
    return 0;
}

sub add_columns_ids_samples
{
    my %parm = validated_hash(\@_,
        nmtablefile => { isa => 'nmtablefile', optional => 0 },
        header_strings => { isa => 'ArrayRef', optional => 0 },
        values_matrix => { isa => 'ArrayRef', optional => 0 },
        sum_matrix => { isa => 'Maybe[ArrayRef]', optional => 1 },
        filter_all_zero => { isa => 'Bool', optional => 0 },
        init => { isa => 'Bool', optional => 0 },
    );
    my $nmtablefile = $parm{'nmtablefile'};
    my $header_strings = $parm{'header_strings'};
    my $values_matrix = $parm{'values_matrix'};
    my $sum_matrix = $parm{'sum_matrix'};
    my $filter_all_zero = $parm{'filter_all_zero'};
    my $init = $parm{'init'};

    my $get_sum = 0;
    $get_sum=1 if (defined $sum_matrix);

    my $input_error = 2;
    my $file_read_error = 1;

    my @column_indices=();
    return $input_error unless (scalar(@{$header_strings})>0);


    if ($filter_all_zero){
        my @filtered_headers =();
        foreach my $header (@{$header_strings}){
            if (any_nonzero($nmtablefile->tables->[0]->get_column(name=> $header))){
                push(@filtered_headers,$header);
            }else{
                ui->print(category=>'simeval',
                          message => "\nWarning: Removed ".$header.
                          " from npde calculation because no non-zero values found first table\n");
            }
        }
        $header_strings = \@filtered_headers;
    }

    my $first_table=$init; #1 or 0
    #FIXME empty files, empty tables
    foreach my $nmtable (@{$nmtablefile->tables}){
        if ($first_table){
            for (my $hi=0; $hi < scalar(@{$header_strings}); $hi++){
                my $col = $nmtable->get_column(name=> $header_strings->[$hi]);
                for (my $ind = 0; $ind < scalar(@{$col}); $ind++){
                    $values_matrix->[$hi][$ind]=[$col->[$ind]];
                    $sum_matrix->[$hi][$ind] = 0 if ($get_sum); #first table is original, do not count it
                }
            }
            $first_table = 0;
        }else{
            for (my $hi=0; $hi < scalar(@{$header_strings}); $hi++){
                my $col = $nmtable->get_column(name=> $header_strings->[$hi]);
                for (my $ind = 0; $ind < scalar(@{$col}); $ind++){
                    push(@{$values_matrix->[$hi][$ind]},$col->[$ind]);
                    $sum_matrix->[$hi][$ind] += ($col->[$ind]) if ($get_sum);
                }
            }
        }
    }

    return 0;
}

sub decorrelation_and_npde_records_by_id
{
    my ($estimate_matrix,$mean_matrix,$id_mdv_matrix,$have_mdv,$npde_vector,$original_outlier) =
        pos_validated_list(\@_,
            { isa => 'ArrayRef' },
            { isa => 'ArrayRef' },
            { isa => 'ArrayRef' },
            { isa => 'Bool' },
            { isa => 'ArrayRef' },
            { isa => 'ArrayRef' },
        );
    #this is for things like iwres and cwres, mulitple per ID
    #$estimate_matrix
    # reference to array  [over records][over samples/files]
    # $mean_matrix
    #reference to mean array  [over records]
    # $id_mdv_matrix ref.  ID is [0]->[over recrods]->[0]
    #                ref.  MDV is [1]->[over recrods]->[0]
    # $have_mdv Bool, false if no mdv recrods

    # $npde_vector reference to empty array to put npde [over records]

    my $input_error = 2;
    my $numerical_error = 1;
    my $message = '';
    my $result = 0;

    my $nresponse = scalar(@{$estimate_matrix});
    unless ($nresponse>0){
        $result = $input_error;
        $message .= "number of response variables (columns in estimate matrix) is 0\n";
    }
    my $datarecords = scalar(@{$estimate_matrix->[0]});
    unless ($datarecords>0){
        $result = $input_error;
        $message .= "number of datarecords (rows in estimate matrix) is 0\n";
    }
    unless (scalar(@{$id_mdv_matrix->[0]}) == $datarecords){
        $result = $input_error;
        $message .= "number of datarecords in id_array ".scalar(@{$id_mdv_matrix->[0]}).
            " is different from estimate matrix $datarecords\n";
    }
    if ($have_mdv){
        unless (scalar(@{$id_mdv_matrix->[1]}) == $datarecords){
            $result = $input_error;
            $message .= "number of datarecords in mdv_array ".scalar(@{$id_mdv_matrix->[1]}).
                " is different from estimate matrix $datarecords\n";
        }
    }

    my $samples = scalar(@{$estimate_matrix->[0]->[0]})-1; #-1 for original
    unless ($samples > 1){
        $result = $input_error;
        $message .= "number of samples (3rd dim in estimate matrix) is not > 1\n";
    }

    return ($result,$message) if ($result);

    #can have varying number of obs per id !!

    my $local_estimate_matrix =[];
    my $local_mean_matrix =[];
    my $local_stdev_arr=[];
    my $local_decorr=[];
    my $local_mdv=[];
    my $previous_id=-99;
    my $nobs = 0;
    for (my $i=0;$i<$datarecords;$i++){
        if ($i==0 or ($previous_id != $id_mdv_matrix->[0]->[$i]->[0])){
            #new id.
            $local_estimate_matrix =[];
            $local_mean_matrix =[];
            $local_stdev_arr=[];
            $local_decorr=[];
            $local_mdv=[];
            $nobs = 0;
        } #else same id

        $previous_id = $id_mdv_matrix->[0]->[$i]->[0];
        if ((not $have_mdv) or ($id_mdv_matrix->[1]->[$i]->[0] == 0)){
            #this record is an observation
            $local_estimate_matrix->[$nobs]->[0]=$estimate_matrix->[0]->[$i];
            my @arr = @{$estimate_matrix->[0]->[$i]}[1 .. $samples];
            my $max = max(\@arr);
            my $min = min(\@arr);
            if (($estimate_matrix->[0]->[$i]->[0] > $max) or
                ($estimate_matrix->[0]->[$i]->[0] < $min)){
                push(@{$original_outlier},1);
            }else{
                push(@{$original_outlier},0);
            }
            $local_mean_matrix->[$nobs][0] = $mean_matrix->[0][$i];
            $nobs++;
            push(@{$local_mdv},0);
        }else{
            push(@{$local_mdv},1);
            push(@{$original_outlier},0);
        }
        if (($i == ($datarecords-1)) or ($id_mdv_matrix->[0]->[$i+1]->[0] != $id_mdv_matrix->[0]->[$i]->[0] )){
            #this is last record, observation or not, of this id
            if ($nobs > 0){
                ($result,$message) = decorrelation($local_estimate_matrix,$local_mean_matrix,$local_decorr,$local_stdev_arr,[]);
                return ($result,$message.' in decorr per id') if ($result);
                my $pde_matrix =[];
                my $npde_matrix = [];
                ($result,$message) = npde_comp($local_decorr,$pde_matrix,$npde_matrix);
                return ($result,$message.' in decorr per id') if ($result);
                my $k=0;
                foreach my $mdv (@{$local_mdv}){
                    if ($mdv == 0){
                        push(@{$npde_vector},$npde_matrix->[$k]->[0]);
                        $k++;
                    }else{
                        push(@{$npde_vector},$missing);
                    }
                }
            }else{
                foreach my $mdv (@{$local_mdv}){
                    push(@{$npde_vector},$missing);
                }
            }
        }
    }
    return (0,$message);
}

sub decorrelation
{
    #has unit tests
    my ($estimate_matrix,$mean_matrix,$decorrelated_estmatrix,$stdev_arr,$missing_matrix) = pos_validated_list(\@_,
        { isa => 'ArrayRef' },
        { isa => 'ArrayRef' },
        { isa => 'ArrayRef' },
        { isa => 'ArrayRef' },
        { isa => 'ArrayRef' },
    );
    #$estimate_matrix
    # reference to array  [over columns][over individuals][over samples/files]
    # $mean_matrix
    #reference to mean array  [over columns][over individuals]

    # reference to empty array to put decorrelated results [over columns][over individuals][over samples]
    # $decorrelated_estmatrix
    # $stdev_arr

    #$missing_matrix 1 if missing 0 otherwise
    # reference to array  [over params][over individuals]

    my $input_error = 2;
    my $numerical_error = 1;
    my $message = '';
    my $result = 0;

    my $nparm = scalar(@{$estimate_matrix});
    unless ($nparm>0){
        $result = $input_error;
        $message .= "number of parameters (columns in estimate matrix) is 0\n";
    }
    my $individuals = scalar(@{$estimate_matrix->[0]});
    unless ($individuals>0){
        $result = $input_error;
        $message .= "number of individuals (rows in estimate matrix) is 0\n";
    }
    my $samples = scalar(@{$estimate_matrix->[0]->[0]})-1; #-1 for original
    unless ($samples > 1){
        $result = $input_error;
        $message .= "number of samples (3rd dim in estimate matrix) is not > 1\n";
    }

    return ($result,$message) if ($result);

    my $have_filter = 0;
    $have_filter = 1 if (scalar(@{$missing_matrix})>0);

    if ($nparm == 1){
        for (my $i=0;$i<$individuals;$i++){
            my $mean=$mean_matrix->[0]->[$i];
            my $sum_errors_pow2=0;
            my $origval = $estimate_matrix->[0]->[$i]->[0];
            #loop over simulations, start at 1 since 0 is original
            for (my $k = 1; $k < scalar(@{$estimate_matrix->[0]->[$i]}); $k++){
                $sum_errors_pow2 += (($estimate_matrix->[0]->[$i]->[$k]) - $mean)**2;
            }
            my $stdev=0;
            unless( $sum_errors_pow2 <= 0 ){
                $stdev= sqrt ($sum_errors_pow2/($samples-1)); #root of variance
            }

            push(@{$stdev_arr},$stdev);

            if ($stdev > 0) {
                my $original = ($origval-$mean)/$stdev;
                $decorrelated_estmatrix->[0]->[$i]=[$original];
                #loop over simulations, start at 1 since 0 is original
                for (my $k = 1; $k < scalar(@{$estimate_matrix->[0]->[$i]}); $k++){
                    my $transf= (($estimate_matrix->[0]->[$i]->[$k]) -$mean)/$stdev;
                    push (@{$decorrelated_estmatrix->[0]->[$i]},$transf);
                }
            }else{
                $decorrelated_estmatrix->[0]->[$i]=[$missing];
                #loop over simulations, start at 1 since 0 is original
                for (my $k = 1; $k < scalar(@{$estimate_matrix->[0]->[$i]}); $k++){
                    push (@{$decorrelated_estmatrix->[0]->[$i]},$missing);
                }
            }
        }
        #return here when nparm==1???????
    }else{

        my $sqrt=sqrt($samples-1);
        for (my $i=0;$i<$individuals;$i++){
            my @Amat=();
            my @original=();
            my @original_filtered=();
            my @meanvec=();
            my $acolumns=0;
            for (my $j=0;$j<$nparm;$j++){
                my $mean = $mean_matrix->[$j][$i];
                push (@meanvec,$mean);
                my $origval = $estimate_matrix->[$j]->[$i]->[0];
                push(@original,($origval-$mean));
                #loop over simulations, start at 1 since 0 is original
                next if ($have_filter and ($missing_matrix->[$j]->[$i] == 1));
                push(@original_filtered,($origval-$mean));
                $acolumns++;
                for (my $k = 1; $k < scalar(@{$estimate_matrix->[$j]->[$i]}); $k++){
                    push(@{$Amat[($acolumns-1)]},($estimate_matrix->[$j]->[$i]->[$k])-$mean);
                }
            }

            my $Rmat = [];
            my $numerr;
            if ($acolumns > 0){
                #for (my $i = 0; $i < scalar(@Amat); $i++) {
                #    for (my $j = 0; $j < scalar(@{$Amat[$i]}); $j++) {
                #        $Amat[$i]->[$j] = 0.0 if ($Amat[$i]->[$j] < 1e-15);
                #    }
                #}
                $numerr = linear_algebra::QR_factorize(\@Amat,$Rmat);
                unless ($numerr == 0){
                    $result = $numerr;
                    $message .= "QR factorization revealed that matrix was not full rank\n";
                    return ($result,$message);
                }
            }

            #want to multiply with inverse 'square root' (in a loose sense) of
            #empirical variance-covariance matrix of A'A
            #i.e. we want to multiply with inv(R*diag(1/sqrt(N-1))
            #i.e. we want to solve orig=(R*diag(1/sqrt(N-1))*transf

            #Rmat->[$col][$row]

            #have already subtracted $mean from original

            my $ncol = scalar(@{$Rmat});

            #solve diag(1/sqrt(N-1))*bvec=yvec
            for (my $j=0;$j<$ncol;$j++){
                $original_filtered[$j]=$original_filtered[$j]*$sqrt;
            }

            #solve R'* transf=bvec
            #Golub p 88


            if ($acolumns > 0){
                $numerr = linear_algebra::upper_triangular_transpose_solve($Rmat,\@original_filtered);
                unless ($numerr == 0){
                    $result = $numerr;
                    $message .= "Solving of triangular system 1 with R matrix broke down, diagonal element of R == 0\n";
                    return ($result,$message);
                }
            }

            my $anspos=0;
            for (my $j=0;$j<$nparm;$j++){
                if ($have_filter and ($missing_matrix->[$j]->[$i] == 1)){
                    $decorrelated_estmatrix->[$j]->[$i]=[$missing];
                }else{
                    $decorrelated_estmatrix->[$j]->[$i]=[$original_filtered[$anspos]];
                    $anspos++;
                }
            }

            #transform estmatrix for each simulation of id $i

            for (my $k = 1; $k < ($samples+1); $k++){
                my @simvec=();
                for (my $j=0;$j<$nparm;$j++){
                    next if ($have_filter and ($missing_matrix->[$j]->[$i] == 1));
                    #must subtract mean here also
                    push(@simvec,(($estimate_matrix->[$j]->[$i]->[$k]) - $meanvec[$j]));
                }

                #solve R'*x=simvec

                if ($acolumns > 0){
                    $numerr = linear_algebra::upper_triangular_transpose_solve($Rmat,\@simvec);
                    unless ($numerr == 0){
                        $result = $numerr;
                        $message .= "Solving of triangular system sim $k with R matrix broke down, diagonal element of R == 0\n";
                        return ($result,$message);
                    }
                }
                #solve diag(1/sqrt(N-1))*transf=x
                for (my $j=0;$j<$ncol;$j++){
                    $simvec[$j]=$simvec[$j]*$sqrt;
                }

                $anspos=0;
                for (my $j=0;$j<$nparm;$j++){
                    if ($have_filter and ($missing_matrix->[$j]->[$i] == 1)){
                        push(@{$decorrelated_estmatrix->[$j][$i]},$missing);
                    }else{
                        push(@{$decorrelated_estmatrix->[$j][$i]},$simvec[$anspos]);
                        $anspos++;
                    }
                }
            }

        } #end loop over id
    }
    return (0,$message);
}

sub pde
{
    #has unit tests
    my ($vector) = pos_validated_list(\@_,
        { isa => 'ArrayRef' },
    );

    my $pde;
    my $original = $vector->[0];
    my $samples = scalar(@{$vector})-1; #-1 for original
    if ($original == $missing){
        $pde = $missing;
    }else{
        my $count=0;
        for (my $k = 1; $k <= $samples; $k++){
            $count++ if (($vector->[$k])< $original);
        }
        if ($count == 0){
            $pde = (1/$samples);
        }elsif ($count == $samples){
            $pde = 1-(1/$samples);
        }else{
            $pde =$count/$samples;
        }
    }

    return $pde;
}

sub npde_comp
{
    #has unit tests
    #in matrix over params -> inds -> samples
    my ($decorrelated,$pde_matrix,$npde_matrix) = pos_validated_list(\@_,
        { isa => 'ArrayRef' },
        { isa => 'ArrayRef' },
        { isa => 'ArrayRef' },
    );
    #decorrelated matrix, empty matrix, empty matrix

    my $have_CDF=0;
    $have_CDF=1 if eval('require Statistics::Distributions'); #enough, now loaded

    #TODO input untransformed and compute normalized pd in addition to normalized pde
    my $input_error = 2;
    my $numerical_error = 1;

    return $input_error unless (scalar(@{$decorrelated}) >0);
    return $input_error unless (scalar(@{$decorrelated->[0]})>0);
    return $input_error unless (scalar(@{$decorrelated->[0]->[0]}) > 2);


    for (my $j=0;$j<scalar(@{$decorrelated});$j++){
        push(@{$pde_matrix},[]);
    }
    for (my $i=0;$i<scalar(@{$decorrelated->[0]});$i++){
        for (my $j=0;$j<scalar(@{$decorrelated}) ;$j++){
            print "i $i j $j \n" if (not defined $decorrelated->[$j]->[$i]);
            $pde_matrix->[$j]->[$i] = pde($decorrelated->[$j]->[$i]);
            if ($have_CDF and ($pde_matrix->[$j]->[$i] != $missing)){
                $npde_matrix->[$j]->[$i] = -(Statistics::Distributions::udistr($pde_matrix->[$j]->[$i]));
            }else{
                $npde_matrix->[$j]->[$i] =$missing;
            }
        }
    }
    return 0;
}

1;
