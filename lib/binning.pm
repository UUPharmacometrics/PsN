#!/usr/bin/perl

package binning;

require Exporter;
our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(number_of_bins_range number_of_bins_auto bin_data) ], );
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

use strict;
use include_modules;
use Math::Trig;	# For pi
use array qw(:all);
use POSIX qw(ceil);

sub bin_range
{
	my $idv = shift;
	my $range_min = shift;
	my $range_max = shift;
	my $min_points = shift;
	my $stratum_label = shift;

	$min_points = 10 unless (defined $min_points);

  my $temp_flush = $|;
  $| = 1;   # Temporary turn on autoflush
	my $bin_edges = chooseK($idv, $range_min, $range_max, $min_points, $stratum_label);
  $| = $temp_flush;

	return $bin_edges;
}

sub bin_auto
{
	my $idv = shift;
	my $min_points = shift;
	my $stratum_label = shift;

	my $n_min = 2;
	my $n_max = 20;

	$min_points = 10 unless (defined $min_points);			# Set default min_points

  my $temp_flush = $|;
  $| = 1;   # Temporary turn on autoflush
	my $bin_edges = chooseK($idv, $n_min, $n_max, $min_points, $stratum_label);
  $| = $temp_flush;

	return $bin_edges;
}


# A portable infinity.
sub inf
{
	return 9**9**9;
}

# Algorithm parameters
my $kurtosis_cutoff = 2.5;	# C
my $kernel_scaling = 0.25;	# 1/R
my $phi_scaling = 7.8;			# The constant C in alpha
my $index_selection = 2;		# The index for choosing K: 0 is the objective function, 1 is the CH index, 2 is the ratio


sub chooseK
{
#   CHOOSEK Function that determines the apppropriate number of bins
#   for firstMethod
#
###########################################################################
#
#   ALGORITHM EXPLAINED:
#   For all the K:s to be examined by the method an index called OlifIndex 
#   is calculated. The K value that gives the maximum value of the index is 
#   choosen as the appropriate number of bins. 
#
#   The index is further defined as the quotient of the CH index and the
#   optimal objective function value.
# 
###########################################################################
#
#   IN Parameters:
#   idvref -            reference of array independent variable
#   kmin -              minimum K value to be examined
#   kmax -              maximum K value to be examined
#   minPointsInBin -		Minimum number of data points required in each bin
#
#   OUT Parameters:
#   bestK -                 Determined number of bins

	my $idv = shift;
	my $kmin = shift;
	my $kmax = shift;
	my $minPointsInBin = shift;
	my $stratum_label = " in " . shift;

	croak("Need at least three arguments") unless (defined($idv) and defined($kmin) and defined($kmax));
	croak("Nmax $kmax is smaller than Nmin $kmin") if ($kmax < $kmin);
	croak("Nmin $kmin must be larger than 1") unless ($kmin > 1);
	croak("min_points_in_bin ($minPointsInBin) cannot be smaller than 2") if ($minPointsInBin < 2);

	# total number of data points
	my $Ntot = scalar(@$idv);
	croak("Too few data points ($Ntot)$stratum_label for $kmin bins given a minimum number of $minPointsInBin points in each bin.\n" .
		  "Either set option min_points_in_bin to a lower number than $minPointsInBin\n".
		  "or try a lower Nmin than $kmin in -auto_bin=Nmin,Nmax\n") 
		if ($Ntot < $kmin * $minPointsInBin);

	my $maxOlifIndex;
	my $bestK;
	my $bestBinEdges;
	my @all_bin_edges;

	# Loop over all Ki:s and calulate their OlifIndices:
	print "Auto binning evaluating N = ";

	for (my $k = $kmin; $k <= $kmax; $k++) {

		# Print the current K value that we are evaluating for
		print " $k";

		# Retrieve optimal binning for fixed K
		(my $binEdges, my $binCentersIdv, undef, undef, my $obj, my $BK) = firstMethod($k, $idv, $minPointsInBin, 1);
		$all_bin_edges[$k] = $binEdges;

		unless (defined($binEdges) and defined($binCentersIdv) and defined($obj) and defined($BK)) {
			print "\nEnding bin search early, Nmax ", $k - 1;
			last;
		}

		# Calulate the between bin variability
		my $B = sum($BK);
		my $W = 0;

		# Calculate the number of data points in each bin and the within
		# bin variability:
		for (my $i = 0; $i < $k; $i++) {
			my $sumSquares = 0;
			foreach my $val (@$idv) {
				if (($val >= $$binEdges[$i]) and ($val < $$binEdges[$i + 1])) {
					$sumSquares += ($val - ($$binCentersIdv[$i]))**2;
				}
			}
			$W += $sumSquares;
      }

		# Put a lower bound on the within bin variability so that in never
		# gets equal to zero:
		$W = 1.0e-20 if ($W < 1.0e-20);

		# Define the index used by Calinski and Harabaszs
		my $CH = ($B / ($k - 1)) / ($W / ($Ntot - $k));

		# Finally calculate our index:
		# A zero objective function means that it has been 'cancelled out' by the variability and that we have a "perfect" dataset.
		my $index;
		if ($obj != 0) {
			$index = $CH / $obj;
		} else {
			$index = inf;
		}

		# Select one of the indices C and obj
		if ($index_selection == 0) {
			$index = $obj;
		} elsif ($index_selection == 1) {
			$index = $CH;
		}
			

		#determine largest index and best K so far
		if ((not defined($maxOlifIndex)) or ($index > $maxOlifIndex)) {
			$maxOlifIndex = $index;
			$bestK = $k;
			$bestBinEdges = $binEdges;
		}

		last if ($index == inf);
	}

	print "\n";

	# Did we get a bin count?
	unless (defined $bestK) {
		die "Auto binning error: Too few data points. Try a lower min_points_in_bin\n";
	}

  print "Auto binning: using $bestK bins\n";
  my $bin_string = join ",", @{$all_bin_edges[$bestK]};
  print "Auto binning: bin edges $bin_string\n";

	return $all_bin_edges[$bestK];
}

sub any (&@)
{
	my $code = \&{shift @_};

	for (@_) {
		if ($code->($_)) {
			return 1;
		}
	}

	return 0;
}

sub firstMethod
{

#   FIRSTMETHOD Automatic binning of data for a given number of bins
#
###########################################################################
#
#   ALGORITHM EXPLAINED:
#
#   The algorithm seeks to place out K bins in a way such that the function
#   O = - sum(B_k) + sum(Phi) is minimized. The idea is to minimize the 
#   within bin variability W in the idv direction, which is equal to the 
#   total variability (T) minus the between cluster variability 
#   (B = sum(B_k)) in the idv direction. Since the total variance is 
#   independent of how the binning is performed, we can maximize the 
#   between bin variability instead (minimize -B).
#
#   The second term in the function O represents the data density, which
#   penelizes a placement of a bin edge somewhere where there are a lot of
#   data points. This term is used to avoid that the algorithm split 
#   cluster of dats points into several bins. This function is defined for 
#   every interval between two consecutive unique idv values. The interval 
#   between the unique values i-1 and i is further on represented as the 
#   unique idv value i.
# 
###########################################################################
#
#   IN Parameters:
#   K -                 Number of desired bins
#   IDV -               Indepent variable of the data set
#   MINPOINTSINBINS -   Minimum number of data points required in each bin
#   POT -               Flag controlling whether to use the data density 
#                       function
#
#   OUT Parameters:
#   BINEDGES -      Vector containing the bin edges idv values for the 
#                   binning performed
#   BINCENTERSIDV - The mean idv values of the data points in the bins 
#                   generated
#   BINN -          The number of data points in each of the bins generated
#   EDGES -         Vector containing the bin edges represented as indices 
#                   in the sorted unique idv value vector
#   OBJFUNVALUE -   The objective function value obtained after minimization
#                   of the objective function
#   BK -            A vector with the between bin variabillity contribution
#                   from every bin
#

#function [binEdges binCenterIdv binN Edges objFunValue BK] = firstMethod(K, idv, MINPOINTSINBINS,pot)

	my $K = shift;
	my $idv = shift;
	my $MINPOINTSINBINS = shift;
	my $pot = shift;

	# Use data density function if not specified
	$pot = 1 unless defined $pot;

	## Initialization of constants 
	###########################################################################

	# Maximum allowed iterations, used to make sure that the algorithm does not
	# run for to long if there is a problem:
	my $MAXITERATIONS = 5 * ($K**2) + 10;

	# The mean in the idv direction of all the data points in the data set:
	my $totMeanIdv = mean($idv);

	# Total variability in idv direction
	my $T = variability($idv, $totMeanIdv);

	# Find all unique idv values and the mapping to the first occurence of every
	# unique value in the vector of the unique values:

	my @sortedidv = sort { $a <=> $b } @$idv;
	(my $uniqueIdv_ref, my $uniqueMapIdv_ref) = unique(\@sortedidv);

	my @uniqueIdv = @$uniqueIdv_ref;
	my @uniqueMapIdv = @$uniqueMapIdv_ref;

	push @uniqueMapIdv, scalar(@$idv);

	# Find the min and max idv values:
	my $minIdv = min(\@uniqueIdv);
	my $maxIdv = max(\@uniqueIdv);

	# Number of different unique idv values in the data set:
	my $numberOfUniqueIdv = scalar(@uniqueIdv);

	# Calculate the number of data points at each unique idv value
	my @uniqueIdvN = @{diff(\@uniqueMapIdv)};

	# Cumulative function of the number of occurences of each unique idv value.
	# cumSumN(i) is the number of data points with idv values equal to any of 
	# idv(1),...,idv(i) 
	my @cumSumN = @uniqueMapIdv;				#  FIXME: -1 ?;

	# Cumulative function of the sum of all idv
	# cumSumIdv(i) is the sum of all idv values up to unique value i
	my @cumSumIdv_step = map { $uniqueIdvN[$_] * $uniqueIdv[$_] } 0..$#uniqueIdv;				# Done in steps to simplify debugging. Can be optimized.
	my @cumSumIdv = @{cumsum(\@cumSumIdv_step)};
	unshift @cumSumIdv, 0;

	#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	# Check weather the number of unique data points allow for K bins 
	if ($numberOfUniqueIdv < $K) {
		return;
	}

## If the data density term is used in the objective function 
###########################################################################
# First, run the algorithm without the data density term. Thereafter define
# a mesh to be used for the smoothing of the data density. The smoothing is
# done bin by bin where the smoothing parameter is given by the standard 
# deviation of in the idv direction in each bin. The kurtosis measure is 
# further used to adjust smoothing parameter if the data density isnt 
# gaussian-like in the bin. Finally the sum is taken of the contributions 
# to the smoothed data density function from all bins.
#
# For every interval between consecutive unique idv values where a bin
# can be placed, set the value of the phi function as the minimum of
# the smoothed data density function in the interval. If there are no
# points in the mesh used for the smoothed density function, take the
# minimum of the two neighbouring phi values.
#
# The final step is to scale the data density function to an appropriate
# scale. This is done with the maximal value of sum of the within bin 
# variability from the iteration before and a constant beta which was 
# empirically determined for best results. max(W) scales phi to be of the 
# same order of magnitude as the objective function.

	my $Edges;
	my @phi;
	my $binN;
	my $binCenterIdv;

	if ($pot) {

		# Run algorithm without the data density term:
		(undef, $binCenterIdv, $binN, $Edges) = firstMethod($K, $idv, $MINPOINTSINBINS, 0);

    unless (defined($binCenterIdv) and defined($binN) and defined($Edges)) {
      return;
    }

		# Define the mesh for the smoothing:

		my $h = min(diff(\@uniqueIdv));

		my $n = 4 * round(($maxIdv - $minIdv) / $h) + 1;
		$n = 2 ** 14 if (2 ** 14 < $n);

		my $idvMesh = linspace($minIdv, $maxIdv, $n);

		my @W;
		my @sumDensity = (0) x $n;

		# Do the smoothing for each bin individually:
		for (my $k = 0; $k < $K; $k++) {

			# Indices for the data points in the current bin:
			my $sliceStart = $$Edges[$k];        # currentIndices = Edges(k):Edges(k+1) - 1;
			my $sliceEnd = $$Edges[$k + 1] - 1;

			my @uniqueIdvSliced = @uniqueIdv[$sliceStart..$sliceEnd];
			my @uniqueIdvNSliced = @uniqueIdvN[$sliceStart..$sliceEnd];

			# The distances to the bin center for all these data points:
			my @distanceToBinCenter = map { $_ - $$binCenterIdv[$k] } @uniqueIdvSliced;           # distanceToBinCenter = uniqueIdv(currentIndices) - binCenterIdv(k);

			# Within-bin variance for the current bin:
			my @toSum = map { $uniqueIdvN[$_ + $sliceStart] * $distanceToBinCenter[$_] ** 2 } 0..$#distanceToBinCenter;		# uniqueIdvN(currentIndices) .* distanceToBinCenter .^ 2
			$W[$k] = sum(\@toSum);

			# Standard deviation for current bin. If the value is too small
			# replace it with the mesh spacing (h):
			my $currentStdIdv = sqrt($W[$k] / $$binN[$k]);
			$currentStdIdv = $h if ($h > $currentStdIdv);

			# Calculate Kurtosis:
			@toSum = map { $uniqueIdvN[$_ + $sliceStart] * $distanceToBinCenter[$_] ** 4 } 0..$#distanceToBinCenter;
			my $fourthMoment = sum(\@toSum) / $$binN[$k];
			my $currentKurtosis = $fourthMoment / ($currentStdIdv ** 4);

			# Smooth the data density function according to the Kurtosis value
			if ($currentKurtosis > $kurtosis_cutoff) {
				add(\@sumDensity, gaussFilter(\@uniqueIdvSliced, \@uniqueIdvNSliced, $idvMesh, $currentStdIdv));
			} else {
				add(\@sumDensity, gaussFilter(\@uniqueIdvSliced, \@uniqueIdvNSliced, $idvMesh, $kernel_scaling * $currentStdIdv));
			}

		}

		# Define the phi function for every unique idv value as the minimum in 
		# the mesh interval between idv(i-1) and idv(i):
		for (my $i = 1; $i < $numberOfUniqueIdv; $i++) {
			# Extract the minimum value in the interval:
			my @indexes = grep { $$idvMesh[$_] >= $uniqueIdv[$i - 1] and $$idvMesh[$_] <= $uniqueIdv[$i] } 0..$#$idvMesh;		# extract a part of the mesh
			my @extraction = @sumDensity[@indexes];
			
			# If there is no value in the interval, set phi to infinity and
			# handle later
			if (scalar(@extraction) > 0) {
				$phi[$i] = min(\@extraction);
			} else {
				$phi[$i] = inf;
			}
		}

		# Set all unset phi values to the min of its neighbours:
		for (my $i = 1; $i < $numberOfUniqueIdv; $i++) {
			if ($phi[$i] == inf) {
				$phi[$i] = min($phi[$i - 1], $phi[$i + 1]);
			}
		}

		# Scale the phi function:
		my $beta = $phi_scaling * max(\@W);

		@phi = map { $_ * $beta } @phi;

	}  # end if pot


## If the data density term is not used in the objective function 
###########################################################################
# The binning given by the method equal size is now used as a starting point 
# for the algorithm, given that it fulfills the requirement on the min 
# number of data points in every bin. If this requirement cannot be
# fulfilled, an error is given.

	if (!$pot) {

		# Set equal size as start binning
		($Edges) = equalSize($numberOfUniqueIdv, \@uniqueIdvN, $cumSumN[-1], $K, $MINPOINTSINBINS);

		# Count the number data point in each bin:
		my @step1 = @cumSumN[@$Edges];		# Unnecessary? Get binN from equalSize
		$binN = diff(\@step1);

		# If the minimum number of points in bins not satisfied, return an error:
		foreach (@$binN) {
			if ($_ < $MINPOINTSINBINS) {
				return (undef, undef, undef, undef, undef, undef);		# Error
			}
		}

		# Calculate the bin center coordinate in the idv direction:
		@step1 = @cumSumIdv[@$Edges];
		$binCenterIdv = diff(\@step1);
		$binCenterIdv = [ map { $$binCenterIdv[$_] / $$binN[$_] } 0..$#$binCenterIdv ];

	}

###########################################################################


## Initialization of variables
###########################################################################
# These variables are used to decide which edge is more rewarding to move
# in the optimization of the objective function. A bin edge will also only
# be moved if it gives a decrease in the objective function, and this is
# what the variable DFDX will indicate.
#
# F -       This is a matrix with three local objective function values for 
#           every bin edge k. The three values corresponds to the bin edge 
#           k being moved one step to the left, the current place of the 
#           bin edge k and thirdly the bin edge k being moved one step to 
#           the right. The local objective function value consists of the 
#           between cluster variability contribution of the two bins 
#           directly to the left and right of the bin edge k together with 
#           the smoothed data density value of the bin. This are the only
#           parts of the objective function that changes when moving the
#           bin edge k in between its to neighbouring bin edges.
# NEGB -    This is a vector with contributions to the negative between bin 
#           variability from the different bin edges.
# DFDX -    This is vector of the minimum changes that can be optained in 
#           the objective function by moving the bin edges one step to the 
#           left or to the right.

	my @f;
	$f[0] = [ (inf) x $K ];
	$f[1] = [ (inf) x $K ];
	$f[2] = [ (inf) x $K ];

	my $negB = [ (inf) x $K ];
	my $dfdx = [ (inf) x $K ];

	my $negBLeft;
	my $negBRight;

	# Loop through all interior bin edges ei to get the initial values of these 
	# variables:
	for (my $ei = 1; $ei < $K; $ei++) {

		# Calculate the current f value and the neg with calcPOF
		(undef, my $currentF, $negBLeft, $negBRight) = calcPOF($ei, [-1, 0, 1], $Edges, \@cumSumN, \@cumSumIdv, \@phi, $MINPOINTSINBINS, $totMeanIdv, $pot);

		# Set the negB value to the left of the current bin edge
		$$negB[$ei - 1] = $$negBLeft[1];

		# Set f for the current bin edge
		$f[0][$ei] = $$currentF[0];
		$f[1][$ei] = $$currentF[1];
		$f[2][$ei] = $$currentF[2];

		# Calculate the dfdx from f:

		$$dfdx[$ei] = min($f[0][$ei] - $f[1][$ei], $f[2][$ei] - $f[1][$ei]);   # f(ei,[1 3])-f(ei,2));
	}

	# Set the negB value for the last bin:
	$$negB[$K - 1] = $$negBRight[1];

###########################################################################



## Calculate the initial value of the objective function. 
###########################################################################
# If data point density used in the objective Function, add it here.

	my $objFunValue;

	if ($pot) {
		$objFunValue = sum($negB) + sum([@phi[@$Edges]]);		# objFunValue = sum(negB) + sum(phi(Edges));
	} else {
		$objFunValue = sum($negB);
	}

###########################################################################



## Optimization of the binning
###########################################################################
# Continue optimization until the stop criteria is fulfilled. 
# The stop criteria is fulfilled when there is no bin that can be
#
# 1. moved in between its two neighbours 
# 2. taken out and be placed in between any two other bin edges 
#
# such that the objective function is decreased while the requirement on 
# minimum data points in each bin is still valid.
#
# PART 1 of optimization
# Try moving the bin edges one by one within its two neighbours to decrease 
# the objective function.
#   Step 1: 
#       Choose the bin edge that gives the biggest decrease
#       (or smallest increase) when being moved one step to the left or right.
#       Then move try every possible possition between the two neighbour 
#       bin edges to get the biggest decrease 
#       of the objective function.
#   Step 2: 
#       Mark the moved bin edge as updated so that it cannot be moved again
#       unless any circumstances are changed.
#   Step 3:
#       If a bin edge has been moved, update the derivatives of the 
#       neighbouring bins so that they can be moved again.
#
#
# PART 2 of optimization
# Try taking out the bin edges one by one and placing them in between two 
# other bin edges to decrease the objective function
#   Step 1: 
#       Calculate the increase in the objective function for removing any 
#       of the bin edges (removeIncrease). Also calculate the decrease in the 
#       objective for adding an extra between bin edge between any two 
#       consecutive bin edges (addDecrease).
#   Step 2:
#       Find the lowest value of removeIncrease and the corresponding edge. Also
#       find the highest value of addGain and the corresponding bin. If the
#       bin to be removed is where...
#       If there is any move of an edge that is favorable
#       perform the movement that results in the biggest decrease and go
#       back to PART I, else stop.

# Initilize iteration variable for optimization algorithm
	my $iteration = 0;

# Check stop critera if intital equal size solution happen to be optimal
	my $stopCriteria = 1;

	foreach (@$dfdx) {				# stopCriteria = all(dfdx==inf);
		if ($_ != inf) {
			$stopCriteria = 0;
			last;
		}
	}

	while (!$stopCriteria and $MAXITERATIONS >= $iteration) {

		## Part 1:
		#######################################################################
    
		# Choose edge with steepest derivative to be moved
		(undef, my $edi) = min($dfdx);

		# For the edge with steepest derivative find the Part of Objective 
		# Function values for all possible new positions.
		(undef, my $POF, my $negBLeft, my $negBRight, my $indexArray, my $N1corped, my $N2corped, my $idvMeanLeft, my $idvMeanRight) = calcPOF($edi, undef, $Edges, \@cumSumN, \@cumSumIdv, \@phi, $MINPOINTSINBINS, $totMeanIdv, $pot);

		# Pick out the best positions according to POF:
		(my $minPOF, my $newPos) = min($POF);

		# Only perform movement if the optimal new position for the bin edge 
		# differ from the current one:
		if ($$Edges[$edi] != $$indexArray[$newPos]) {

			# Update the edge position:
			$$Edges[$edi] = $$indexArray[$newPos];

			# Update the number of data points in the bins to the left and
			# right of the moved bin edge:
			$$binN[$edi - 1] = $$N1corped[$newPos];
			$$binN[$edi] = $$N2corped[$newPos];

			# Update the bin centers in the idv direction for the bins to the 
			# left and right of the moved bin edge:
			$$binCenterIdv[$edi - 1] = $$idvMeanLeft[$newPos];
			$$binCenterIdv[$edi] = $$idvMeanRight[$newPos];

			# Update the ojective function value. Add the contribution from the 
			# new bin edge position and remove the contribution from the old:
			$objFunValue += $minPOF - $f[1][$edi];

			# Update the negB values for the bins to the left and right of the
			# moved bin edge:
			$$negB[$edi - 1] = $$negBLeft[$newPos];
			$$negB[$edi] = $$negBRight[$newPos];

			# Update f and dfdx for the neighboring bin edges since the move 
			# have effected them (They have to be checked again)
			updateF($edi - 1, $K, \@f, $Edges, $dfdx, \@cumSumN, \@cumSumIdv, \@phi, $MINPOINTSINBINS, $totMeanIdv, $pot);
			updateF($edi + 1, $K, \@f, $Edges, $dfdx, \@cumSumN, \@cumSumIdv, \@phi, $MINPOINTSINBINS, $totMeanIdv, $pot);
 		}

		# This edge is locally optimal for now so we mark this in f and dfdx
		$f[0][$edi] = inf;
		$f[1][$edi] = $minPOF;
		$f[2][$edi] = inf;

		$$dfdx[$edi] = inf;

		# Update iteration number:
		$iteration++;
    
		# Update stop criteria
		$stopCriteria = 1;
		foreach (@$dfdx) {
			if ($_ != inf) {
				$stopCriteria = 0;
				last;
			}
		}

		#######################################################################
		## Part 2:
		#######################################################################

		# If the stop criteria now is fulfilled and the number of desired bins
		# is not 2, then move to part 2:
		my $addDecrease;

		if ($stopCriteria and $K != 2) {

			##  Initialize variables keeping track of how much the objective
			#   function changes when a new bin edge is added:
			###################################################################

			# How much the objective function decreases if a new bin edge is
			# placed between bin edge K and K+1
			$addDecrease = [ (-inf()) x $K ];

			# The Part of the Objective Function for the added bin edge: 
			my $addPOF = [ (inf) x $K ];

			# The contribution to the between bin variability from the new bin 
			# to the left of the addded bin edge:
			my $newNegBLeft = [ (inf) x $K ];

			# The contribution to the between bin variability from the new bin 
			# to the right of the addded bin edge:
			my $newNegBRight = [ (inf) x $K ];

			# The optimal position (in unqiue idv index) for the new bin edges:
			my $addEdgeIndex = [ (0) x $K ];

			# How much the objective function increases if a bin edge is
			# removed:
			my $removeIncrease = [ (inf) x ($K - 1) ];

			# The contribution to the between bin variability from the new bin 
			# given by merging the bin to the left and to the right of the bin 
			# edge removed:
			my $removePOF = [ (inf) x $K ];

			###################################################################

			# Calculate the decrease in the objective function when adding a 
			# bin edge at different locations:
			for (my $i = 0; $i < $K; $i++) {
				($$addDecrease[$i], $$addPOF[$i], $$newNegBLeft[$i], $$newNegBRight[$i], $$addEdgeIndex[$i]) = calcAD($i, $Edges, \@cumSumN, \@cumSumIdv, \@phi, $MINPOINTSINBINS, $totMeanIdv, $pot, $negB);
			}

			# Continue as long as there is any allowed place to insert a new
			# bin edge:
			if (any { $_ > -inf() } @$addDecrease) {

				# Calculate the increase of the objective function when
				# removing any of the current bin edges:
				for (my $i = 1; $i < $K; $i++) {
					($$removeIncrease[$i], $$removePOF[$i]) = calcRI($i, \@cumSumN, \@cumSumIdv, $Edges, $f[1][$i], $totMeanIdv);
				}

				# Find the bin egde which gives the minimum increase of the 
				# objective function when removed 
				(my $minRemoveIncrease, my $minRIIndex) = min($removeIncrease);

				# Find the place where adding a bin edge gives the maximum
				# decrease of the objective function
				(my $maxAddLoss, my $maxADIndex) = max($addDecrease);

				# The total amount that the objective function would decrease:
				my $totalDecrease = $maxAddLoss - $minRemoveIncrease;

				# Perform the move of the bin edge as long as the total 
				# decrease of the objective function is bigger than zero. To 
				# avoid problems with round off errors we demand that 
				# totalDecrease is 100 times bigger that the floating points 
				# number distance for number of the same magnitude as the
				# objFuncValue.
				if ($totalDecrease > 100 * eps($objFunValue)) {

					# Get the indices of the bin edges that have to be updated 
					# due to the move. The edges that have to be updated
					# are those that lie to the left and right of the removed 
					# bin edge (minRIIndex) and those that lie to the left and 
					# right of the inserted bin edge (minADIndex). Note that
					# the move have not been performed yet.

					my $edgesThatNeedToBeUpdated = unique([-1 + $minRIIndex, 1 + $minRIIndex, $maxADIndex, 1 + $maxADIndex]);
					my @edgesThatNeedToBeUpdated = @$edgesThatNeedToBeUpdated;

					# The first and last bin edge never have to be updated
					# since they are fixed. So remove them from the
					# edgesThatNeedToBeUpdated if they appear there:
					@edgesThatNeedToBeUpdated = grep { $_ != 0 and $_ != $K } @edgesThatNeedToBeUpdated;

					# Perform the move:
					$$Edges[$minRIIndex] = $$addEdgeIndex[$maxADIndex];

					# Sort the bin edges so that they lie in order of the idv
					# values:
					my @sortI = sort { $$Edges[$a] <=> $$Edges[$b] } 0..$K - 1;			# Sorted indices
					@$Edges[0..$K - 1] = sort { $a <=> $b } @$Edges[0..$K - 1];		# [Edges(1:K),sortI] = sort(Edges(1:K));

					# To transform edgesThatNeedToBeUpdated to the new bin edge
					# sorting, we get the reverse sorting mapping 
					# The AGROW command to the left below is suppressing a 
					# warning in the matlab prompt
					my @reverseSort;
					@reverseSort[@sortI] = 0..$K - 1;

					# Do the reverse mapping edgesThatNeedToBeUpdated:
					@edgesThatNeedToBeUpdated = @reverseSort[@edgesThatNeedToBeUpdated];

					# Find out where the inserted bin edge lie in the newly 
					# sorted Edges vector:
					my $newEdgeIndex;

					if ($minRIIndex <= $maxADIndex) {

						# Since the removed bin edge had a lower index than the
						# inserted bin edge, the index of the inserted bin edge
						# will be the same as maxADIndex:
						$newEdgeIndex = $maxADIndex;

					} else {

						# If this is not the case the index is one higher
	               $newEdgeIndex = $maxADIndex + 1;

					}

					# Update the the contribution to the between bin 
					# variability from the bin which the removed bin edge 
					# earlier was in
					$$negB[$minRIIndex - 1] = $$removePOF[$minRIIndex];

					# Sort negB according to the mapping used for sorting Edges
					$negB = [@$negB[@sortI]];

					# Update the negB values for the two bins to the left and
					# to the right of the inserted bin edge:
					$$negB[$newEdgeIndex - 1] = $$newNegBLeft[$maxADIndex];
					$$negB[$newEdgeIndex] = $$newNegBRight[$maxADIndex];

					# Sort f according to the mapping used for sorting Edges:
					$f[0] = [@{$f[0]}[@sortI]];
					$f[1] = [@{$f[1]}[@sortI]];
					$f[2] = [@{$f[2]}[@sortI]];

					# Update the POF value for the moved bin edge:
					$f[1][$newEdgeIndex] = $$addPOF[$maxADIndex];

					# Update all edges that need to be updated so that they can
					# be moved again
					foreach my $i (@edgesThatNeedToBeUpdated) {
						updateF($i, $K, \@f, $Edges, $dfdx, \@cumSumN, \@cumSumIdv, \@phi, $MINPOINTSINBINS, $totMeanIdv, $pot);
					}

					# Update objective function value
					$objFunValue = $objFunValue - $totalDecrease;

					# Update the number och data points in each bin:
					$binN = diff([@cumSumN[@$Edges]]);

					# Update the bin centers in the idv direction for all bins:
					$binCenterIdv = diff([@cumSumIdv[@$Edges]]);
					$binCenterIdv = [map { $$binCenterIdv[$_] / $$binN[$_] } 0..$#$binCenterIdv];

					# Since bins have been inserted part 1 has to be repeated:
					$stopCriteria = 0;
				}
			}
		}

	    #######################################################################
	}

	# Display an error if the optimization have not finished in the maximum
	# number of iterations
	if ($iteration >= $MAXITERATIONS) {
		print "Error, Max Iterations reached\n";
	}

	# Convert from edge index to idv-coordinate
	my $binEdges = indexToIdv(\@uniqueIdv, $Edges, $$binCenterIdv[0], $$binCenterIdv[-1]);

	# Get the true objective function value which is missing the total
	# variability (constant that does not effect the minimization algorithm
	$objFunValue = $T + $objFunValue;
	
	# Get the contribution to the between bin variabillity from every bin. This
	# is returned from the function and used when determining the number of 
	# appropiate bins
	my $BK = [map { -$_ } @$negB];

	return ($binEdges, $binCenterIdv, $binN, $Edges, $objFunValue, $BK);
}

## Functions with shared variables used in first method: ##################



## UpdateF
# Function that updates the bin edge value of f and dfdx 
# 
###########################################################################
#
#   IN Parameter:
#   I - Index for for the bin edge to be updated

sub updateF
{
	my $i = shift;			# scalar
	my $K = shift;			# scalar
	my $f = shift;			# matrix ref
	my $Edges = shift;	# array ref
	my $dfdx = shift;		# array ref
	my $cumSumN = shift;			# array ref
	my $cumSumIdv = shift;		# array ref
	my $phi = shift;				# array ref
	my $MINPOINTSINBINS = shift;	# scalar
	my $totMeanIdv = shift;		# scalar
	my $pot = shift;				# scalar boolean

	# Only update interior bin edges
	if ($i > 0 && $K > $i) {
		(undef, my $POF) = calcPOF($i, [-1, 0, 1], $Edges, $cumSumN, $cumSumIdv, $phi, $MINPOINTSINBINS, $totMeanIdv, $pot);
		$$f[0][$i] = $$POF[0];
		$$f[1][$i] = $$POF[1];
		$$f[2][$i] = $$POF[2];

		$$dfdx[$i] = min($$f[0][$i] - $$f[1][$i], $$f[2][$i]- $$f[1][$i]);
	}
}



#function [ edges ] = indexToIdv( uniqueIdv, edgeIndices, leftMostBinCenter, rightMostBinCenter )
#   INDEXTOIDV Calculates the bin edges idv coordinate from its index
#   coordinate in the vector of unique idv of the data
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#   The interior bin edges are first represented by the indices in the 
#   unique idv vector corresponding to the data point directly to the left 
#   of the bin edges. Every bin edge's idv coordinate is then calculated as 
#   the idv position exactly in the middle of idv(i-1) and idv(i) where i 
#   is the index for which the egde originally was defined. 
#   For the first and last bin edges this approach cannot be used. Instead,
#   to get a natural placement the first bin edge will be placed so that 
#   the distance from it to the center of the first bin is the same as 
#   the distance from the center of the first bin to the second bin edge. 
#   The last bin edge is defined in the same way but from the right.
# 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#   IN Parameters:
#   UNIQUEIDV - Vector of unqiue idv values in which the edges origianlly
#   are represented
#   EDGEINDICES - The position of the edges as indices in the UNIQUEIDV. An edge 
#   with index i lies somewhere between the idv value i-1 and i
#   LEFTMOSTBINCENTER - The mass center in the idv direction of the data 
#   points in the left most bin. Used to set the first bin edge.
#   RIGHTMOSTBINCENTER - The mass center in the idv direction of the data 
#   points in the right most bin. Used to set the last bin edge.
#
#   OUT Parameters:
#   EDGES - The position of the edges as idv coordinates
#

sub indexToIdv
{
	my $uniqueIdv = shift;				# array ref
	my $edgeIndices = shift;			# array ref
	my $leftMostBinCenter = shift;	# scalar
	my $rightMostBinCenter = shift;	# scalar

	# Initialize variable edges
	my @edges = (0) x scalar(@$edgeIndices);

	# Calculate the idv coordinates for the interior bin edges
	for (my $i = 1; $i < scalar(@$edgeIndices); $i++) {
		$edges[$i] = 0.5 * ($$uniqueIdv[$$edgeIndices[$i] - 1] + $$uniqueIdv[$$edgeIndices[$i]]);
	}

	# Set left and right most edges as described earlier
	$edges[0] = $edges[1] - 2 * ($edges[1] - $leftMostBinCenter);
	$edges[-1] = $edges[-2] + 2 * ($rightMostBinCenter - $edges[-2]);

	# If this way of placing the boundary edges leaves some data points outside 
	# of the bins, then set the bin edges as the leftmost/rightmost idv value 
	# with a little pertubation so that the data points lies within the bins:

	if ($edges[0] >= min($uniqueIdv)) {
		$edges[0] = min($uniqueIdv) - (max($uniqueIdv) - min($uniqueIdv)) / 1000;
	}

	if ($edges[-1] <= max($uniqueIdv)) {
		$edges[-1] = max($uniqueIdv) + (max($uniqueIdv) - min($uniqueIdv)) / 1000;
	}

	return \@edges;
}

#   GAUSSFILTER
#   Performs a Gauss smoothing of the given data point density
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#   IN Parameters:
#   IDV - Unique idv values for the data points
#   DENSITY - Number of data points at each unique idv value
#   MESH - Mesh on which we calclate the smoothed data density
#   STD - Standard deviation in the idv direction of the data
#
#   OUT Parameters:
#   SMOOTHEDDENSITY - Smoothed density generated by the function
#
#function smoothedDensity = gaussFilter(idv,density,mesh,std)

sub gaussFilter
{
	my $idv = shift;			# Array ref
	my $density = shift;		# Array ref
	my $mesh = shift;			# Array ref
	my $std = shift;			# Scalar number

	# Number of data points
	my $N;
	foreach (@$density) {
		$N += $_;
	}

	# Optimal h for Gauss kernel under some assumptions of the distribution
	# of the data points. 
	# See report for more details.

	my $h = $std * ($N ** -0.2);

	my @smoothedDensity;

	my $spacing = (scalar(@$mesh) - 1) / ($$mesh[-1] - $$mesh[0]);
	my $mid;
	my $gauss;

	# Perform a convolution between the Gauss Kernel and the data density
	for (my $i = 0; $i < @$density; $i++) {
		$mid = ceil(($$idv[$i] - $$mesh[0]) * $spacing);

		# Loop forward from midpoint and backward from midpoint separately. Stop is the kernel function is close to zero.
		my $k = $mid;
		my $cont = 1;
		while ($cont && $k < scalar(@$mesh)) {
			$gauss = exp( (($$mesh[$k] - $$idv[$i]) / $h)**2 * (-1/2)) / sqrt(2 * pi);
			if ($gauss < 10**-12) {
				$cont = 0;
			} else {
				$smoothedDensity[$k] += $$density[$i] * $gauss;
			}
			$k++;
		}	

		my $k = $mid - 1;
		my $cont = 1;
		while ($cont && $k >= 0) {
			$gauss = exp( (($$mesh[$k] - $$idv[$i]) / $h)**2 * (-1/2)) / sqrt(2 * pi);
			if ($gauss < 10**-12) {
				$cont = 0;
			} else {
				$smoothedDensity[$k] += $$density[$i] * $gauss;
			}
			$k--;
		}

		#Old simpler inner loop
		#for (my $k = 0; $k < @$mesh; $k++) {
		#		$smoothedDensity[$k] += $$density[$i] * exp( (($$mesh[$k] - $$idv[$i]) / $h)**2 * (-1/2)) / sqrt(2 * pi);
		#	}	
	}

	# Normalize smoothed density so that the integral over it is 1.

	foreach (@smoothedDensity) {
		$_ /= ($h * $N);
	}

	return \@smoothedDensity;
}

#function [edgeIndices binN] = equalSize(uniqueN,valueCount,N,K,MINPOINTSINBIN)
#   EQUALSIZE Automatic binning so that every bin contains the same number
#   of data points
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#   ALGORITHM EXPLAINED:
#   We loop through all unique idv values and add them to the current bin 
#   as long as any of the following completness conditions are not 
#   fulfilled: 
#       1.  The minimum number of data points in the bin (MINPOINTSINBIN) 
#           has been fulfilled.
#       2.  Adding the data points with the next idv value gives a higher
#           global error than not adding them. The global error is defined 
#           as the sum of how much the number of data points in every 
#           completed bin deviates from the ideal number of data points in 
#           them (N/K). The data points will only be added if there still
#           are enough unique idv values left to make the remaining bins
#           filled with atleast one data point each.
#   Then we step to the next bin edge and continues until all unqiue idv 
#   values have been walked through or until all bin edges have been placed 
#   out.
#
#   Note that it may be so that the last bins does not fulfill the min
#   points in bin requiredment if the data set does not allow for it
#    
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#   IN Parameters:
#   UNIQUEN - Number of unique idv values
#   VALUECOUNT - Number of occurences of each unique idv value
#   N - Total number of data points
#   K - Number of desired bins
#   MINPOINTSINBINS - Minimum number of data points required in each bin
#
#   OUT Parameters:
#   EDGEINDICES - The position of the edges as indices in the UNIQUEIDV. An edge 
#   with index i lies somewhere between the idv value i-1 and i
#   BINN - Number of data points in each bin
#


sub equalSize
{
	my $uniqueN = shift;
	my $valueCount = shift;
	my $N = shift;
	my $K = shift;
	my $MINPOINTSINBIN = shift;

	## Initialization of variables:

	# Ideal number of data points in each bin:
	my $idealCount = $N / $K;

	# Index of the edge currently being placed out:
	my $edgeIndex = 1;

	# Number of data points in current bin minus ideal number of bins:  
	my $localError = -$idealCount;

	# The total error of all bins:
	my $globalError = 0;

	# Positionvector of edges as indices in the vector of unique idv values: 
	my @edgeIndices = (0) x $K;
	push @edgeIndices, $uniqueN;

	# Number of data points in each bin:
	my @binN = ();

	## Loop through all unique idv values:
	#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	for (my $valueIndex = 0; $valueIndex < $uniqueN; $valueIndex++) {

		# All bin edges have been placed out
		if ($edgeIndex == $K) {
			# Set the number of data point in the last bin and break:
			my $binNsum = 0;					# binN(K) = N - sum(binN(1:K - 1));
			for (my $i = 0; $i < $K - 1; $i++) {
				$binNsum += $binN[$i];
			}

			$binN[$K - 1] = $N - $binNsum;
			last;
		# The current bin does not fulfill the condition for completness yet  
		# (Min points in bin condition || (Global error condition && Still data 
		# points left to fill remaining bins))
		} elsif ($binN[$edgeIndex - 1] < $MINPOINTSINBIN or abs($globalError + $localError) > abs($globalError + $localError + $$valueCount[$valueIndex]) and ($uniqueN - $valueIndex - 1) > ($K - $edgeIndex)) {
			# Add the current unique idv value to the current bin and update
			# local error:
			$localError += $$valueCount[$valueIndex];
			$binN[$edgeIndex - 1] += $$valueCount[$valueIndex];
		# The current bin has now meet the completness conditions:
		} else {

			# Set the position of the current edge
			$edgeIndices[$edgeIndex] = $valueIndex;

			# Update global error with local error:
			$globalError += $localError;
        
			# Move to the next edge:
			$edgeIndex++;

			# Add the current unique idv value to the new current bin and
			# update local error:
			$localError = -$idealCount + $$valueCount[$valueIndex];
			$binN[$edgeIndex - 1] += $$valueCount[$valueIndex];
		}
	}

	return (\@edgeIndices, \@binN);
}


##  calcRI
#   This function calculates the increase in the objective function when
#   removing a bin edge
# 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#   IN Parameters:
#   EI -                The idv index for the bin edge to be removed
#
#   OUT Parameters:
#   REMOVEINCREASE -    The decrease in the objective function given when 
#                       removing the bin edge.
#   REMOVEPOF -         The contribution to the between bin variability 
#                       from the new bin given by merging the bin to the
#                       left and to the right of the bin edge to be removed

sub calcRI
{
	my $ei = shift;			# scalar
	my $cumSumN = shift;		# array reference
	my $cumSumIdv = shift;	# array reference
	my $Edges = shift;		# array reference
	my $f_e2 = shift;			# scalar f(ei, 2)
	my $totMeanIdv = shift;	# scalar

	# Number of data points in new bin given by merging the bin to the
	# left and to the right of the bin edge to be removed:
	my $N = @$cumSumN[@$Edges[$ei + 1]] - @$cumSumN[@$Edges[$ei - 1]];

	# Sum of the idv values of the data points in new bin:
	my $sumIdv = @$cumSumIdv[@$Edges[$ei + 1]] - @$cumSumIdv[@$Edges[$ei - 1]];

	# The mean idv value of the new bin calculated from sumIdv:
	my $meanIdv = $sumIdv / $N;

	# The contribution to the between bin variability from the new bin:
	my $removePOF = -($N * ($meanIdv - $totMeanIdv) ** 2);

	# The increase of the objective funtion when removing the bin edge:
	my $removeIncrease = $removePOF - $f_e2;

	return ($removeIncrease, $removePOF);
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##  CALCNEGB
#   calculates the contribution to the between bin variability from bins
#   with mean idv values meanIdv with the corresponding number of data
#   points in the bins N
#
###########################################################################
#
#   IN Parameters:
#   N -         Vector with the number of data points in the bins
#   MEANIDV -   Vector with the mean idv values in the bins
#
#   OUT Parameters:
#   NEGB -      The contribution to the between bin variability from the 
#               bin to the left of the considered bin edge depending this 
#               bin edge is moved 

sub calcNegB
{
	my $N = shift;				# array ref
	my $meanIdv = shift;		# array ref
	my $totMeanIdv = shift;	# scalar

	my @step1 = map { ($_ - $totMeanIdv) ** 2 } @$meanIdv;		#        negB = -(N .* (meanIdv-totMeanIdv) .^ 2);
	my @negB = map { -$$N[$_] * $step1[$_] } 0..@$N - 1;

	return \@negB;
}

##  CALCPOF 
#   calculates the Part of the Objective Function that is effected
#   when moving the given bin edge.
# 
###########################################################################
#
#   IN Parameters:
#   EDGEINDEX - The idv index of the bin edge to be moved around
#   LOOPRANGE - The relative positions to the edge index that we want 
#               to move the considered bin edge to. If given as empty
#               vector then all possible places will be looped through
#   EDGEARRAY - Vector containing the bin edges indices in the unique idv
#               value vector
#
#   OUT Parameters:
#   POF -       The value of the Part of the Objective Function effected by
#               by moving the considered bin edge. This value depends on
#               where the considered bin edge is moved
#   NEGBLEFT -  The contribution to the between bin variability from the 
#               bin to the left of the considered bin edge depending on 
#               where this bin edge is moved 
#   NEGBRIGHT - The contribution to the between bin variability from the 
#               bin to the right of the considered bin edge depending on 
#               where this bin edge is moved 
#   GRID -      Value of the objective function for the given binning
#

#    function [POF,negBleft,negBright,grid] = calcPOF(edgeIndex,loopRange,edgeArray)

sub calcPOF
{
	my $edgeIndex = shift;		# scalar
	my $loopRange = shift;		# array ref
	my $edgeArray = shift;		# array ref
	my $cumSumN = shift;			# array ref
	my $cumSumIdv = shift;		# array ref
	my $phi = shift;				# array ref
	my $MINPOINTSINBINS = shift;	# scalar
	my $totMeanIdv = shift;		# scalar
	my $pot = shift;				# scalar boolean

	# Extract all the possible indices to which the considered bin edge
	# can be moved:
	my $moveToFirst = $$edgeArray[$edgeIndex - 1];		# moveToIndices
	my $moveToLast = $$edgeArray[$edgeIndex + 1];

	# Cut out the corresponding part of the cumSumN vector
	my @relevantCumSumN = @$cumSumN[$moveToFirst..$moveToLast];

	# Calculate the numbers of points in the bin to the left (N1) and 
	# to the right (N2) of the bin edge to be moved as a function of 
	# where it is placed. 
	my @N1 = map { $_ - $relevantCumSumN[0] } @relevantCumSumN;
	my @N2 = map { $N1[-1] - $_ } @N1;

	# Cut out the corresponding part of the cumSumIdv vector
	my @relevantCumSumIdv = @$cumSumIdv[$moveToFirst..$moveToLast];

	# Calculate the cumulative sum of the idv values in the bin to the 
	# left(cumSumIdvLeft) and right(cumSumIdvRight) of where the bin 
	# edge is placed
	my @cumSumIdvLeft = map { $_ - $relevantCumSumIdv[0] } @relevantCumSumIdv;
	my @cumSumIdvRight = map { $cumSumIdvLeft[-1] - $_ } @cumSumIdvLeft;

	# Find boundary indices for the movement of the bin edge
	# considering the MINPOINTSINBINS restriction:
	my $leftIndex;
	for (my $i = 0; $i < @N1; $i++) {
		if ($N1[$i] >= $MINPOINTSINBINS) {
			$leftIndex = $i;
			last;
		}
	}

	my $rightIndex;
	for (my $i = $#N2; $i >= 0 ; $i--) {
		if ($N2[$i] >= $MINPOINTSINBINS) {
			$rightIndex = $i;
			last;
		}
	}

	# In part II of the optimization algorithm it might happen that 
	# there are not enough data points to add anouther bin edge in 
	# between two other bin edges. In that case, leftIndex will be 
	# larger than the rightIndex. If this happens throw an error which 
	# can be caught by the optimization algorithm.
	if ($leftIndex > $rightIndex) {
		return (1);               # error('VPCBinning:BinCantBePlaced','Error, bin contains to few points');
	}

	# Use the loopRange if it is nonempty.
	my $useLoopRange = defined($loopRange);

	if ($useLoopRange) {

		# Calculate the considered bin edge place relative to the bin 
		# edge directly to the left of it
		my $oldPlacement = $$edgeArray[$edgeIndex] - $$edgeArray[$edgeIndex - 1];
            
		# Define the loopRange relative to the bin edge
		# directly to the left of the considered bin edge
		$loopRange = [ map { $_ + $oldPlacement } @$loopRange ];

		# Take the boundary indices for the movement a the most 
		# restrictive ones of leftIndex/rightIndex and loopRange:

		$leftIndex = max($$loopRange[0], $leftIndex);
		$rightIndex = min($$loopRange[-1], $rightIndex);
	}

	# We now only consider the indices that are allowed by the 
	# MINPOINTSINBINS condition. These indices are relative to the bin 
	# edge at edgeIndex - 1
   # corpedInterval = leftIndex:rightIndex;
          
	# Redefine N1 and N2 so that they are only defined for the
	# placements allow by the MINPOINTSINBINS value and the loopRange 
	# vector

	my @N1corped = @N1[$leftIndex .. $rightIndex];
	my @N2corped = @N2[$leftIndex .. $rightIndex];

	# Calulate the lift and right bins mean idv value dependent on
	# where the bin edge is moved:
	my @idvMeanLeft = @cumSumIdvLeft[$leftIndex..$rightIndex];
	@idvMeanLeft = map { $idvMeanLeft[$_] / $N1corped[$_] } 0..$#idvMeanLeft;

	my @idvMeanRight = @cumSumIdvRight[$leftIndex..$rightIndex];
	@idvMeanRight = map { $idvMeanRight[$_] / $N2corped[$_] } 0..$#idvMeanRight;

	# Calculate the left and right bin contributions to the negative 
	# between bin depending on where the considered bin edge is moved

	my $POFLeft = calcNegB(\@N1corped, \@idvMeanLeft, $totMeanIdv);
	my $POFRight = calcNegB(\@N2corped, \@idvMeanRight, $totMeanIdv);

	# Define vector with the phi values for the different placements
	# for the considered bin edge:
	my @relevantPhi;

	my @negBleft;
	my @negBright;

	if ($useLoopRange) {

		# When we use loopRange we always want to return negB vectors
		# of the same length as loopRange. If some values are not
		# allowed, the value Inf will be returned for negB instead.

		# Initialize negB variables of same length as loopRange
		@negBleft = (inf) x scalar(@$loopRange);
		@negBright = (inf) x scalar(@$loopRange);

		# Then assign their values:
		for (my $i = 0; $i < @$POFLeft; $i++) {
			$negBleft[$leftIndex - $$loopRange[0] + $i] = $$POFLeft[$i];
			$negBright[$leftIndex - $$loopRange[0] + $i] = $$POFRight[$i];
		}

		# If the data density function phi used:
		if ($pot) {

			# Calculate the phi values depending on where the
			# considered bin edge has moved.

			@relevantPhi = (0) x scalar(@$loopRange);

			for (my $i = 0; $i < $rightIndex - $leftIndex + 1; $i++) {
				$relevantPhi[$leftIndex + $i - $$loopRange[0]] = $$phi[$leftIndex + $i + $$edgeArray[$edgeIndex - 1]];
			}

		}

	# When no loopRange have been given:
	} else {

		# Assign the values of the negB vectors
		@negBleft = @$POFLeft;
		@negBright = @$POFRight;

		# If the data density function phi used:
		if ($pot) {

			# Calculate the phi values depending on where the
			# considered bin edge has moved.
			@relevantPhi = @$phi[$leftIndex + $$edgeArray[$edgeIndex - 1] .. $rightIndex + $$edgeArray[$edgeIndex - 1]];		# REMOVED -1

		}

	}

	# Add upp the different contributions to the POF values
	my @POF = map { $negBleft[$_] + $negBright[$_] + $relevantPhi[$_] } 0..$#negBleft;

	# Grid that translates the local position of the bin edge to its 
	# global position

	my @moveTo = $moveToFirst .. $moveToLast;
	my @grid = @moveTo[$leftIndex .. $rightIndex];

	return (0, \@POF, \@negBleft, \@negBright, \@grid, \@N1corped, \@N2corped, \@idvMeanLeft, \@idvMeanRight);
}

##  calcAD
#   This function calculates the decrease in the objective function when
#   inserting a bin edge between bin edge k and k+1
# 
###########################################################################
#
#   IN Parameters:
#   K -             The idv index for the bin edge to the left of those two 
#                   in between which we want to insert a new bin edge.
#
#   OUT Parameters:
#   ADDDECREASE -   The decrease in the objective function for the optimal
#                   placing of a new bin edge between bin edge k and k+1.
#   NEWPOF -        The Part of the Ojective Function contributed by the 
#                   new bin edge
#   NEGBLEFT -      The contribution to the between bin variability from 
#                   the bin to the left of the inserted bin edge for its 
#                   optimal placement between bin edge k and k+1 
#   NEGBRIGHT -     The contribution to the between bin variability from 
#                   the bin to the right of the inserted bin edge for its 
#                   optimal placement between bin edge k and k+1 
#   EDGEINDEX -     The optimal bin edge place when inserting a bin edge 
#                   between bin edge k and k+1
#
#    function [addDecrease,newPOF,negBLeft,negBRight,edgeIndex] = calcAD(k)
 
sub calcAD
{
	my $k = shift;					# scalar
	my $Edges = shift;			# array ref

	my $cumSumN = shift;			# array ref
	my $cumSumIdv = shift;		# array ref
	my $phi = shift;				# array ref
	my $MINPOINTSINBINS = shift;	# scalar
	my $totMeanIdv = shift;		# scalar
	my $pot = shift;				# scalar boolean
	my $negB = shift;				# array ref

	# Try putting an bin edge between bin edge k and k+1:

	# Use the calcPOF function to get the Parts of the Objective 
	# Function for the different placings between the two bin edge 
	# k and k+1
	(my $error, my $localPOF, my $negBLeft, my $negBRight, my $grid) = calcPOF(1, undef, [$$Edges[$k], 0, $$Edges[$k + 1]], $cumSumN, $cumSumIdv, $phi, $MINPOINTSINBINS, $totMeanIdv, $pot);

	my $addDecrease;
	my $newPOF;
	my $edgeIndex;

	my $negBLeft_result;
	my $negBRight_result;

	if (not $error) {

		# Find the place where inserting a new bin edge gives the
		# largest decrease in the objective function:
		($newPOF, my $localIndex) = min($localPOF);

		# Get the global index for the bin edge which gives the largest
		# decrease in the objective function:
		$edgeIndex = $$grid[$localIndex];

 		# Calculate the decrease in the objective function when
		# inserting the new bin edge. Take the contribution to the between
		# bin variability from before the bin was inserted in bin k
		# (negB(k)) and subtract the two new bins contribution to the
		# between bin variability together with the phi function
		# contribution if used (newPOF)
		$addDecrease = $$negB[$k] - $newPOF;

		# Only return the negBLeft value of the optimal placing:
		$negBLeft_result = $$negBLeft[$localIndex];

		# Only return the negBRight value of the optimal placing:
		$negBRight_result = $$negBRight[$localIndex];

	# If we get an error, catch it and...
	} else {
		# Set the variables to the least profitable ones if this error
		# was due to that the bin edge could not be placed due to
		# MINPOINTSINBINS restriction

		# We set the following variables to -inf and inf to be able 
		# to recognize that the bin edge could not be placed:

		$addDecrease = -inf();
		$newPOF = inf;
		$negBLeft_result = inf;
		$negBRight_result = inf;
		$edgeIndex = 0;
	}

	return ($addDecrease, $newPOF, $negBLeft_result, $negBRight_result, $edgeIndex);
}

# Scalar round
sub round
{
	my $a = shift;

	if ($a == 0) {
		return 0;
	}

	return int($a + $a / abs($a * 2));
}

# Machine epsilon for different magnitudes.
# Gives the correct answer for eps(x) <= 1 otherwise returns 1
sub eps
{
	my $x = shift;
	$x = abs($x);		# eps is same for negatives 

	my $e = 1;

	while ($e + $x > $x)
	{
		$e /= 2;
	}

	$e *= 2;

	return($e);
}

sub variability
{
	my $ref = shift;
	my $mean = shift;

	croak("Inputs reference to variability() not defined") unless defined($ref);
	croak("Input array to variability() is empty") unless (@$ref > 0);

	# If mean value not supplied calculate one
	unless (defined($mean)) {
		$mean = mean($ref);
	}

	my $sumSquares = 0;
	foreach my $val (@$ref) {
		$sumSquares += ($val - $mean) ** 2;
	}

	return $sumSquares; #intentional not to divide by N or N-1 here
}

1;
