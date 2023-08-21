package boxcox;

use strict;
use warnings;
use MouseX::Params::Validate;
use include_modules;
use Statistics::Distributions qw(udistr);
use array qw(sum);

sub r_of_lambda
{
   my ($vector,$qvec,$lambda) = pos_validated_list(\@_,
        { isa => 'ArrayRef' },
        { isa => 'ArrayRef' },
        { isa => 'Num' },
    );

   #Assume $vector already sorted and correct length, $qvec correct length

   my $transformed = box_cox($vector,$lambda);

   my ($r,$drdlambda) = compute_r($transformed,$qvec,$lambda,$vector);
   my $numerr = 0;
   $numerr = 1 unless (defined $r);
   return ($r,$drdlambda,$numerr);
}

sub compute_r
{
   my ($xvec,$yvec,$lambda,$parvec) = pos_validated_list(\@_,
        { isa => 'ArrayRef' },
        { isa => 'ArrayRef' },
        { isa => 'Maybe[Num]' },
        { isa => 'Maybe[ArrayRef]' },
    );

   my $minimum = 1E-5;
   my $N=scalar(@{$xvec});

   my $derivative=undef;

   my $logterm;

   my @x2vec=();
   my @xyvec=();
   my @y2vec=();
   for (my $i=0; $i< $N; $i++){
       push(@x2vec,($xvec->[$i])**2);
       push(@xyvec,($xvec->[$i])*($yvec->[$i]));
       push(@y2vec,($yvec->[$i])**2);
   }

   my @sorted = sort {$a<=>$b} @x2vec; #sort ascending, all pos
   my $sum_xsquare = sum(\@sorted);
   @sorted = sort {$a<=>$b} @y2vec;
   my $sum_ysquare = sum(\@sorted);

   @sorted = sort {abs($a)<=>abs($b)} @xyvec;
   my $sum_pos = 0;
   my $sum_neg = 0;
   foreach my $val (@sorted){
       if ($val > 0){
           $sum_pos += $val;
       }else{
           $sum_neg += $val;
       }
   }
   my $sum_xy = ($sum_pos**2-$sum_neg**2)/($sum_pos-$sum_neg);   #mulitply with conjugate

   @sorted = sort {abs($a)<=>abs($b)} @{$xvec};
   $sum_pos = 0;
   $sum_neg = 0;
   foreach my $val (@sorted){
       if ($val > 0){
           $sum_pos += $val;
       }else{
           $sum_neg += $val;
       }
   }
   my $sum_x = ($sum_pos**2-$sum_neg**2)/($sum_pos-$sum_neg);   #mulitply with conjugate

   my $squared_x_term = ($sum_xsquare-($sum_x**2)/$N);
   return (undef, undef) unless ($squared_x_term > 0); #numerical error
   my $root_squared_x_term = sqrt($squared_x_term);
   my $root_squared_y_term = sqrt($sum_ysquare);
   my $xy_cross_term = ($sum_xy);

   return (undef, undef) unless (($root_squared_x_term*$root_squared_y_term) > 0); #numerical error
   my $r= $xy_cross_term/($root_squared_x_term*$root_squared_y_term);

   return ($r,undef); #undef is for derivative
}

sub get_lambda_delta
{
   my ($vector,$absmax,$extravalue) = pos_validated_list(\@_,
        { isa => 'ArrayRef' },
        { isa => 'Num' },
        { isa => 'Num' },
    );
   #extravalue is value that must also be positive after shift, but should not be used for optimization
   my $N = scalar(@{$vector});
   croak("length vector get_lambda_delta must be > 1") unless ($N > 1);
   croak("absmax must be positive and <= 4") unless ($absmax>0 and ($absmax <= 4));

   my ($shifted,$delta) = sort_and_shift_to_positive($vector,$extravalue);
   my $yvec = get_quantile_data($N);


   my %extra;
   $extra{'shifted'} = $shifted;
   $extra{'yvec'} = $yvec;
   $extra{'type'} = 'r';

   my $maxpow=6;  #visual check with matlab suggest this is sufficient
   my $maxeval=20; #visual check with matlab
   my $bestp = direct_search_maximize($absmax,$maxpow,$maxeval,\%extra);
   return (undef,0,1) unless (defined $bestp->[0]); #not any r computable

   return($bestp->[1],$delta,0); #best lambda -3 to 3
}

sub evaluate
{
    my ($l,$extra_args) = pos_validated_list(\@_,
                                             { isa => 'Num' },
                                             { isa => 'HashRef' },
        );

    if ($extra_args->{'type'} eq 'r'){
         my ($r,$deriv,$numerr) = r_of_lambda($extra_args->{'shifted'},$extra_args->{'yvec'},$l);
         return ($r,$deriv,$numerr);
    }elsif($extra_args->{'type'} eq 'second_degree'){
        my $f= ($extra_args->{'a'})*$l**2+($extra_args->{'b'})*$l+$extra_args->{'c'};
        my $deriv = ($extra_args->{'a'})*2*$l+($extra_args->{'b'});
        return ($f,$deriv,0);
    }else{
        croak("unknown type ".$extra_args->{'type'});
    }
}

sub insert_sort_ascending
{
    my ($array,$new,$pos) = pos_validated_list(\@_,
                                          { isa => 'ArrayRef' },
                                          { isa => 'ArrayRef' },
                                          { isa => 'Int' },
        );

    if (scalar(@{$array}) < 1) {
        push(@{$array},$new);
    }else{
        my $place = scalar(@{$array});
        while ($new->[$pos] < $array->[$place-1]->[$pos]){
            $place--;
            last if ($place == 0);
        }
        splice(@{$array},$place,0,$new);
    }
}

sub make_splits
{
    my ($matrix,$split_indices,$range,$extra_args) = pos_validated_list(\@_,
                                                                        { isa => 'ArrayRef' },
                                                                        { isa => 'ArrayRef' },
                                                                        { isa => 'Num' },
                                                                        { isa => 'HashRef' },
        );

    #essential start with largest index (smallest box)
    #sort descending
    my @sorted = sort {$b <=> $a } @{$split_indices};
    my $ypos=0;

    foreach my $index (@sorted){
        my $point = pop(@{$matrix->[$index]}); #last in arr, highest ofv
        my $step= $range/(3**($index+1));
        my $newx = $point->[1]+$step;
        my ($y,$deriv,$numerr) = evaluate($newx,$extra_args);
        insert_sort_ascending($matrix->[$index+1],[$y,$newx],$ypos) unless ($numerr);
        $newx = $point->[1]-$step;
        ($y,$deriv,$numerr) = evaluate($newx,$extra_args);
        insert_sort_ascending($matrix->[$index+1],[$y,$newx],$ypos) unless ($numerr);
        insert_sort_ascending($matrix->[$index+1],$point,$ypos) if (defined $point->[$ypos]); #in first iter can have undef here
    }
}

sub turn
{
    my ($p1,$p2,$p3) = pos_validated_list(\@_,
                                          { isa => 'ArrayRef' },
                                          { isa => 'ArrayRef' },
                                          { isa => 'ArrayRef' },
        );

    #0 colinear
    #+1 left - convex
    #-1 right - concave
    #we use neg of ofv to match original alg
    #1 is x 0 is y
    return ($p2->[1]-$p1->[1])*(-$p3->[0]+$p1->[0])-(-$p2->[0]+$p1->[0])*($p3->[1]-$p1->[1]);
}

sub get_split_indices
{
    my ($matrix,$range) = pos_validated_list(\@_,
                                             { isa => 'ArrayRef' },
                                             { isa => 'Num' },
        );

    my $max_index= scalar(@{$matrix})-1;

    my ($smallest,$best) = get_smallest_best_index($matrix);
    my @split=();

    my @distance = ();
    my @candidates = ();
    for (my $i=0; $i<=$max_index; $i++){
        push(@distance,$range/(2*(3**($i))));
        push(@candidates,$i) if ((scalar(@{$matrix->[$i]})>0) and ($i<= $best))  ;
    }
    croak("bug in get ind") unless ($candidates[-1] == $best);

    if (scalar(@candidates)>2){
        my $done = 0;
        my $index1 = pop(@candidates);
        my $index2 = pop(@candidates);
        my $index3 = pop(@candidates);
        while (not $done){
            my @p1=($matrix->[$index1]->[-1]->[0],$distance[$index1]);
            my @p2=($matrix->[$index2]->[-1]->[0],$distance[$index2]);
            my @p3=($matrix->[$index3]->[-1]->[0],$distance[$index3]);

            my $turn = turn(\@p1,\@p2,\@p3);
            if ($turn < 0){
                #drop p2
                if (scalar(@split)>0){
                    #go back and reevaluate
                    $index2 = $index1;
                    $index1 = pop(@split);
                }elsif (scalar(@candidates)>0){
                    #take next if any
                    $index2=$index3;
                    $index3=pop(@candidates);
                }else{
                    #done
                    push(@split,$index1);
                    push(@split,$index3);
                    $done=1;
                }
            }else{
                #straight or convex
                if (scalar(@candidates)>0){
                    #move forward, save p1
                    push(@split,$index1);
                    $index1 = $index2;
                    $index2 = $index3;
                    $index3 = pop(@candidates);
                }else{
                    #done
                    push(@split,$index1);
                    push(@split,$index2);
                    push(@split,$index3);
                    $done=1;
                }
            }
        }
        #take away best if == $max_index
        if ($split[0] == $max_index){
            shift(@split);
        }
    }else{
        if ($best < $max_index){
            push(@split,$best);
        }
        if ($smallest != $best){
            push(@split,$smallest);
        }
    }

    return \@split;
}

sub get_smallest_best_index
{
    my ($matrix) = pos_validated_list(\@_,
                                      { isa => 'ArrayRef' },
        );

    my $size= scalar(@{$matrix});

    my $smallest;
    my $best;

    for (my $i=0; $i<$size; $i++){
        if (scalar(@{$matrix->[$i]}) > 0){
            #not empty
            $smallest = $i unless (defined $smallest);
            if (defined $best){
                if ($matrix->[$best]->[-1]->[0] <= $matrix->[$i]->[-1]->[0]){
                    $best = $i;
                }
            }else{
                $best = $i;
            }
        }
    }
    return ($smallest,$best);
}

sub direct_search_maximize
{
    #this will search for highest
    # evaluated f over all evaluated points. Will not work
    # for finding minimum. Search area symmetric around 0

    my ($absmax,$max_pow,$maxeval,$extra_args) = pos_validated_list(\@_,
                                                           { isa => 'Num' },
                                                           { isa => 'Int' },
                                                           { isa => 'Int' },
                                                           { isa => 'HashRef' },
    );

    my $eval_count=0;
    my $range = 2*$absmax;
    my @matrix=();
    for (my $pow=0; $pow<=$max_pow; $pow++){
        push(@matrix,[]);
    }
    my $y;
    my $numerr;
    my $deriv;
    my $next_x = 0;
    ($y,$deriv,$numerr) = evaluate($next_x,$extra_args);
    $matrix[0] = [[$y,$next_x]]; #only est at pow 2
    $eval_count++;
    my $done = 0;
    while ($eval_count < $maxeval){
        #find and pull out boxes to split
        my $indices = get_split_indices(\@matrix,$range);
        unless (scalar(@{$indices})>0){
            last;
        }
        make_splits(\@matrix,$indices,$range,$extra_args);
        $eval_count += 2*scalar(@{$indices});
    }
    my ($smallest,$best) = get_smallest_best_index(\@matrix);
    return $matrix[$best]->[-1]; #the y and x value
}

sub report_matrix
{
    my ($matrix) = pos_validated_list(\@_,
                                      { isa => 'ArrayRef' },
        );

    my @arr=();

    for (my $i=0; $i<scalar(@{$matrix}); $i++){
        for (my $j=0; $j<scalar(@{$matrix->[$i]}); $j++){
            insert_sort_ascending(\@arr,$matrix->[$i]->[$j],1);
        }
    }

    foreach my $p (@arr){
        print $p->[1]."\t".$p->[0]."\n";
    }
    print "];\n";
}

sub grid_search_maximize
{

    my ($absmax,$grid_step,$extra_args) = pos_validated_list(\@_,
                                                             { isa => 'Num' },
                                                             { isa => 'Num' },
                                                             { isa => 'HashRef' },
    );

    my $next_x = -$absmax+$grid_step;
    my @best=();
    my $y;
    my $deriv;
    my $numerr;
    while ($next_x < ($absmax-(1E-5))){
        ($y,$deriv,$numerr) = evaluate($next_x,$extra_args);
        if ((not $numerr) and ((not defined $best[0]) or ($best[0]<$y))){
            @best = ($y,$next_x,$deriv);
        }

        $next_x += $grid_step;
    }
    return \@best;

}

sub secant_method_maximize
{
    #this will search for derivative 0 and then pick highest
    # evaluated f over all evaluated points.
    #assume input best y is defined

    my ($range,$min_step,$max_iter,$start,$extra_args) = pos_validated_list(\@_,
                                                                            { isa => 'Num' },
                                                                            { isa => 'Num' },
                                                                            { isa => 'Int' },
                                                                            { isa => 'ArrayRef' },
                                                                            { isa => 'HashRef' },
        );

    my $verbose=0;
    my $numerr = 0;

    croak("input values not defined") unless (defined $start->[0] and defined  $start->[1]);

    my @xvec=($start->[1]);
    my @ys=($start->[0]);
    my @derivatives=($start->[2]);
    my $y;
    my $deriv;
    my @best = ($start->[0],$start->[1]);

    my $right = $start->[1]+$range;
    my $left = $start->[1]-$range;
    my $next_x;
    my $done = 0;
    unless (defined $start->[2]){
        $next_x = $start->[1]+$range/4;
        ($y,$deriv,$numerr) = evaluate($next_x,$extra_args);
        if (defined $y and ($best[0]<$y)){
            @best = ($y,$next_x);
        }
        if ($numerr or (not defined $deriv)){
            $done=1;
        }else{
            push(@ys,$y);
            push(@derivatives,$deriv);
            push(@xvec,$next_x);
        }
    }

    unless ($done){
        if ($derivatives[-1] > 0){
            $next_x = $xvec[-1]+$range/8;
        }else{
            $next_x = $xvec[-1]-$range/8;
        }
    }
    my $step;

    while (not $done){
        if (($next_x >= $right) or ($next_x<= $left)) {
            $done =1;
        }else{
            ($y,$deriv,$numerr) = evaluate($next_x,$extra_args);
            if (defined $y and ($best[0]<$y)){
                @best = ($y,$next_x);
            }
            if ($numerr or (not defined $deriv)){
                $done=1;
            }else{
                push(@ys,$y);
                push(@derivatives,$deriv);
                push(@xvec,$next_x);

                my $index=$#xvec;
                ($step,$done)=next_secant_step($xvec[$index],$xvec[$index-1],$derivatives[$index],$derivatives[$index-1]);
                $next_x=$xvec[$index]+$step;
                if ($done){
                    print "small delta-derivative\n" if ($verbose);
                }elsif (abs($step) < $min_step){
                    print "small step\n" if ($verbose);
                    $done=1;
                }elsif($index > $max_iter){
                    print "maxloop\n" if ($verbose);
                    $done=1;
                }
            }
        }
    }

    return ($best[1],0); #numerr is 0
}

sub next_secant_step
{
   my ($xn,$xn_minus_1,$fn,$fn_minus_1) = pos_validated_list(\@_,
        { isa => 'Num' },
        { isa => 'Num' },
        { isa => 'Num' },
        { isa => 'Num' },
    );

   my $stop=0;
   my $step=0;
   my $smallnum=1e-7;

   if (abs($fn-$fn_minus_1)<$smallnum){
       $stop=1;
   }else{
       $step = -$fn*($xn-$xn_minus_1)/($fn-$fn_minus_1);
   }

   return($step,$stop);
}

sub inverse_box_cox
{
    my ($vector,$lambda) = pos_validated_list(\@_,
                                              { isa => 'ArrayRef' },
                                              { isa => 'Num' },
        );

    unless (scalar(@{$vector})>0){
        croak("inverse_box_cox vector must have length > 0");
    }
    my $minimum = 1E-5;
    my @transformed = ();

    if (abs($lambda) < $minimum){
        for (my $i=0; $i<scalar(@{$vector}); $i++){
            push(@transformed,exp($vector->[$i]));
        }
    }else{
        for (my $i=0; $i<scalar(@{$vector}); $i++){
            push(@transformed,(($vector->[$i])*$lambda+1)**(1/$lambda));
        }
    }

    return \@transformed;
}

sub shift_and_box_cox
{
    my %parm = validated_hash(\@_,
                              vector => { isa => 'ArrayRef', optional => 0 },
                              lambda => { isa => 'ArrayRef', optional => 0 },
                              delta => { isa => 'ArrayRef', optional => 0 },
                              inverse => {isa => 'Bool', optional => 0},
        );
    my $vector = $parm{'vector'};
    my $lambda = $parm{'lambda'};
    my $delta = $parm{'delta'};
    my $inverse = $parm{'inverse'};

    unless (scalar(@{$lambda}) == scalar(@{$delta})){
        croak("lambda array and delta array must have equal length");
    }
    unless ((scalar(@{$lambda}) == 1)    or (scalar(@{$lambda}) == scalar(@{$vector}))){
        croak("lambda array and vector must have equal length unless lambda has length 1");
    }

    #undef lambda means too close to 1, no transformation

    if (scalar(@{$lambda})==1){
        #whole vector should be tranformed with same lambda
        if (not defined $lambda->[0]){
            #do not transform, just copy input
            my @copy = ();
            for (my $i=0; $i< scalar(@{$vector}); $i++){
                push(@copy,$vector->[$i]);
            }
            return \@copy;
        }else{
            if ($inverse){
                my $inv = inverse_box_cox($vector,$lambda->[0]);
                my @results = ();
                for (my $i=0; $i< scalar(@{$inv}); $i++){
                    push(@results,($inv->[$i]-($delta->[0])));
                }
                return \@results;
            }else{
                my @shifted = ();
                for (my $i=0; $i< scalar(@{$vector}); $i++){
                    push(@shifted,($vector->[$i]+$delta->[0]));
                }
                return box_cox(\@shifted,$lambda->[0]);
            }
        }
    }else{
        #individual lambda for each vector element
        my @result=();
        for (my $i=0; $i< scalar(@{$vector}); $i++){
            if (not defined $lambda->[$i]){
                #do not transform, just copy input
                push(@result,$vector->[$i]);
            }else{
                if ($inverse){
                    my $ref = inverse_box_cox([$vector->[$i]],$lambda->[$i]);
                    push(@result,$ref->[0]-($delta->[$i]));
                }else{
                    my $ref = box_cox([$vector->[$i]+$delta->[$i]],$lambda->[$i]);
                    push(@result,$ref->[0]);
                }
            }
        }
        return \@result;
    }
}

sub box_cox
{
    my ($vector,$lambda) = pos_validated_list(\@_,
                                              { isa => 'ArrayRef' },
                                              { isa => 'Num' },
        );

    unless (scalar(@{$vector})>0){
        croak("box_cox vector must have length > 0");
    }

   my $minimum = 1E-5;
   my @transformed = ();

   #add check vector positive?
   if (abs($lambda) < $minimum){
       for (my $i=0; $i<scalar(@{$vector}); $i++){
           push(@transformed,(log($vector->[$i])));
       }
   }else{
       for (my $i=0; $i<scalar(@{$vector}); $i++){
           push(@transformed,((($vector->[$i])**$lambda-1)/$lambda));
       }
   }

   return \@transformed;
}

sub sort_and_shift_to_positive
{
   my ($vector,$extravalue) = pos_validated_list(\@_,
        { isa => 'ArrayRef' },
        { isa => 'Num' },
    );

   unless (scalar(@{$vector})>1){
       croak("sort_and_shift_to_positive must have length > 1");
   }

   my @sorted = sort {$a <=> $b} @{$vector};

   my $minimum = 0.00001;

   my $delta = $sorted[0] - $minimum;

   if (defined $extravalue and ($extravalue < $sorted[0])){
       $delta = $extravalue - $minimum;
   }

   if ($delta < 0){
       $delta = -$delta;

       for (my $i=0; $i<scalar(@sorted); $i++){
           $sorted[$i] = $sorted[$i] + $delta;
       }
   }else{
       $delta=0;
   }

   return(\@sorted,$delta);
}

sub get_quantile_data
{
    #we use Statistics::Distributions which has lower precision than Math::CDF but is
    #already used by other PsN modules, so no need to install new module
    my ($number) = pos_validated_list(\@_,
        { isa => 'Int' },
    );

    unless ($number > 1){
        croak("get_quantile_data must have input number > 1 but got $number");
    }

    my @vector=();

    for (my $i=1; $i<= $number; $i++){
        my $y=($i-0.5)/$number;
        my $val = udistr((1-$y));
        #on 32bit windows have seen that udistr(0.5), which should be simply 0, returns NaN
        if ((2*$i -1) == $number){
            $val = 0;
        }
        push(@vector,$val);
    }

    return \@vector;
}

1;
