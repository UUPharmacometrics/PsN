package boxcox;



use strict;
use warnings;
use MooseX::Params::Validate;
use include_modules;
use Statistics::Distributions qw(udistr);
#use Math::CDF qw(qnorm);
#use math qw(:all);

#require Exporter;
#our @ISA = qw(Exporter);
#our %EXPORT_TAGS = ('all' => [ qw(round eps inf ceil usable_number to_precision convert_float_string) ]);
#our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );


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

   my $sum_x=0;
   my $sum_xsquare=0;
   my $sum_y=0;
   my $sum_ysquare=0;
   my $sum_xy=0;
   my $sum_q = 0;
   my $sum_squared_q=0;
   my $sum_logterm=0;
   my $sum_xlogterm=0;
   my $sum_ylogterm=0;

   my $minimum = 1E-5;
   my $N=scalar(@{$xvec});

   my $derivative=undef;
   $derivative = 1 if (defined $lambda and (abs($lambda) >= $minimum));
   my $logterm;
   for (my $i=0; $i< $N; $i++){
	   $sum_x += $xvec->[$i];
	   $sum_xsquare += ($xvec->[$i])**2;
	   $sum_xy += ($xvec->[$i])*($yvec->[$i]);
#	   $sum_y += $yvec->[$i];
	   $sum_ysquare += ($yvec->[$i])**2;
	   if (defined $derivative){
		   $logterm = (log($parvec->[$i]))*($parvec->[$i])**($lambda);
		   $sum_ylogterm += ($yvec->[$i])*$logterm;
		   $sum_xlogterm += ($xvec->[$i])*$logterm;
		   $sum_logterm += $logterm;
	   }
   }


   my $squared_x_term = ($sum_xsquare-($sum_x**2)/$N);
   return (undef, undef) unless ($squared_x_term > 0); #numerical error
   my $root_squared_x_term = sqrt($squared_x_term);
#   my $root_squared_y_term = sqrt($sum_ysquare-($sum_y**2)/$N); sum_y is 0 
   my $root_squared_y_term = sqrt($sum_ysquare); 
   #my $xy_cross_term = ($sum_xy-($sum_x*$sum_y/$N)); sum_y is 0
   my $xy_cross_term = ($sum_xy);

   return (undef, undef) unless (($root_squared_x_term*$root_squared_y_term) > 0); #numerical error
   my $r= $xy_cross_term/($root_squared_x_term*$root_squared_y_term); 
   if (defined $derivative){
	   $derivative=(($sum_ylogterm-$sum_xy)-$xy_cross_term*($sum_xlogterm-$sum_xsquare-($sum_logterm-$sum_x)*$sum_x/$N )/$squared_x_term)/($root_squared_x_term*$root_squared_y_term*$lambda);
   }

   return ($r,$derivative);
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
   my $min_step = 0.005;
   my $max_iter=30;

   my %extra;
   $extra{'shifted'} = $shifted;
   $extra{'yvec'} = $yvec;
   $extra{'type'} = 'r';

   my ($lambda,$numerr) = secant_method_maximize($absmax,$min_step,$max_iter,\%extra);
   
   return($lambda,$delta,$numerr);
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




sub secant_method_maximize
{
	#this will search for derivative 0 and then pick highest
	# evaluated f over all evaluated points. Will not work
	# for finding minimum. Search area adapted to finding boxcox lambda
	#where derivative not defined for lambda=0

	my ($absmax,$min_step,$max_iter,$extra_args) = pos_validated_list(\@_,
																	  { isa => 'Num' },
																	  { isa => 'Num' },
																	  { isa => 'Int' },
																	  { isa => 'HashRef' },
    );

	my $verbose=0;
	my $numerr = 0;
   #Figure out search area, cannot run over lambda=0

	my @lambdas=();
	my @rs=();
	my @derivatives=();
	my $r;
	my $deriv;
	my $next_l=1;
	($r,$deriv,$numerr) = evaluate($next_l,$extra_args);
	return(undef,$numerr) if ($numerr);

	push(@lambdas,$next_l);
	push(@rs,$r);
	push(@derivatives,$deriv);

	my $left;
	my $right;
	my $done=0;
	my $step;
	my $index;

	if ($deriv < 0){
		#negative at 1
		$next_l = -1;
		($r,$deriv,$numerr) = evaluate($next_l,$extra_args);
		return(undef,$numerr) if ($numerr);
		push(@lambdas,$next_l);
		push(@rs,$r);
		push(@derivatives,$deriv);
		if ($deriv < 0){
			#negative at 1, negative at -1
			#search range -absmax to -1
			$left=-$absmax;
			$right=-1;
			$next_l=-1.5;
		}else{
			#positive at -1, negative at 1
			$next_l = -0.1;
			($r,$deriv,$numerr) = evaluate($next_l,$extra_args);
			return(undef,$numerr) if ($numerr);
			push(@lambdas,$next_l);
			push(@rs,$r);
			push(@derivatives,$deriv);
			if ($deriv < 0){
				#positive at -1, negative at -0.1
				#search range -1 to -0.1
				$left=-1;
				$right=-0.1;
				($step,$done)=next_secant_step($lambdas[2],$lambdas[1],$derivatives[2],$derivatives[1]);
				$next_l=$lambdas[2]+$step;
			}else{
				#positive at -0.1, negative at 1
				#evaluate 0
				$next_l=0;
				($r,$deriv,$numerr) = evaluate($next_l,$extra_args);
				return(undef,$numerr) if ($numerr);
				push(@lambdas,$next_l);
				push(@rs,$r);
				push(@derivatives,undef);
				$next_l = 0.1;
				($r,$deriv,$numerr) = evaluate($next_l,$extra_args);
				return(undef,$numerr) if ($numerr);
				push(@lambdas,$next_l);
				push(@rs,$r);
				push(@derivatives,$deriv);
				if ($deriv < 0){
					#positive at -0.1, negative at 0.1
					#take highest r so far and quit
					$done=1;
				}else{
					#positive at 0.1, negative at 1
					#search range 0.1 to 1
					$left=0.1;
					$right=1;
					($step,$done)=next_secant_step($lambdas[$#lambdas],$lambdas[0],$derivatives[$#lambdas],$derivatives[0]);
					$next_l=$lambdas[$#lambdas]+$step;
				}
			}
		}
	}else{
		#positive at 1
		#search range 1 to absmax
		$left=1;
		$right=$absmax;
		#evaluate one extra to get starting values
		$next_l = $left+($absmax-$left)/2;
	}

	while (not $done){
		if ($next_l > $right){
			$next_l = $right;
			$done=1;
		}elsif ($next_l < $left){
			$next_l = $left;
			$done =1;
		}

		($r,$deriv,$numerr) = evaluate($next_l,$extra_args);
		return(undef,$numerr) if ($numerr);
		push(@lambdas,$next_l);
		push(@rs,$r);
		push(@derivatives,$deriv);
		if ($done){
			print "outside\n" if ($verbose);
		}else{
			$index=$#lambdas;
#		   print "index $index, lambdas ".join(' ',@lambdas)." rs ".join(' ',@rs)." derivs ".join(' ',@derivatives)."\n";
			($step,$done)=next_secant_step($lambdas[$index],$lambdas[$index-1],$derivatives[$index],$derivatives[$index-1]);
			$next_l=$lambdas[$index]+$step;
			if ($done){
				print "small delta-derivative\n" if ($verbose);
			}elsif (abs($step) < $min_step){
				print "small step\n" if ($verbose);
				$done=1;
			}elsif($index > 30){
				print "maxloop\n" if ($verbose);
				$done=1;
			}
		}
	}

	#final, find largest r and that lambda
	$index = 0;
	for (my $i=1; $i< scalar(@rs); $i++){
		if ($rs[$i] > $rs[$index]){
			$index = $i;
		}
	}

	if ( $verbose){
		for (my $i=0; $i< scalar(@rs); $i++){
			print $lambdas[$i].','.$rs[$i].','.$derivatives[$i]."\n";;
		}
		print "\nFinal\n".$lambdas[$index].','.$rs[$index].','.$derivatives[$index]."\n\n\n\n";
	}
	return ($lambdas[$index],0); #numerr is 0
	

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
	unless ((scalar(@{$lambda}) == 1)	or (scalar(@{$lambda}) == scalar(@{$vector}))){
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
#		my $val = qnorm($y);
		my $val = udistr((1-$y));
#		if ($cdf){
#			$val = qnorm($y);
#		}else{
#			$val = udistr((1-$y)):
#		}
		push(@vector,$val);
	}

	return \@vector;


}

1;
