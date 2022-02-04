#!/usr/bin/perl

# This module collects routines that operates on arrays (using references to arrays)

package array;

use strict;

use MouseX::Params::Validate;
use include_modules;
use POSIX 'floor';
use math qw(round);

require Exporter;
our @ISA = qw(Exporter);
our %EXPORT_TAGS = ('all' => [ qw(not_empty is_empty diff cumsum max min linspace unique add sum mean median median_and_ci variance stdev is_int quantile percentile is_equal get_array_positions get_positions sem rse any_nonzero count_lower find_zeros get_intersection is_zero numerical_in string_in) ]);
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

sub find_zeros
{
    my $a = shift;        # Array reference

    my @indices=();
    for (my $i=0; $i<scalar(@{$a}); $i++) {
        if ($a->[$i] == 0){
            push(@indices,$i);
        }
    }

    return \@indices;
}

sub is_zero
{
    my $a = shift;        # Array reference

    my @is_zero=();
    for (my $i=0; $i<scalar(@{$a}); $i++) {
        if ($a->[$i] == 0){
            push(@is_zero,1);
        }else{
            push(@is_zero,0);
        }
    }
    return \@is_zero;
}

sub get_positions
{
    #static
    my %parm = validated_hash(\@_,
                              target => { isa => 'ArrayRef', optional => 0 },
                              keys => { isa => 'ArrayRef', optional => 0 },
        );
    my $target = $parm{'target'};
    my $keys = $parm{'keys'};
    my @cols= ();
    return \@cols unless (defined $target and defined $keys);

    for (my $i=0; $i< scalar(@{$keys}); $i++){
        my $matched = undef;
        for (my $j=0; $j< scalar(@{$target}); $j++){
            if ($keys->[$i] eq $target->[$j]){
                $matched = $j;
                last;
            }
        }
        push(@cols,$matched);
    }
    return \@cols; #this is sorted according to keys
}

sub get_intersection
{
    #static
    my %parm = validated_hash(\@_,
                              arr1 => { isa => 'ArrayRef', optional => 0 },
                              arr2 => { isa => 'ArrayRef', optional => 0 },
        );
    my $arr1 = $parm{'arr1'};
    my $arr2 = $parm{'arr2'};
    my @intersection= ();
    return \@intersection unless (defined $arr1 and defined $arr2);
    return \@intersection unless (scalar(@{$arr1})>0 and scalar(@{$arr2})>0);

    my @remaining = @{$arr2};

    for (my $i=0; $i< scalar(@{$arr1}); $i++){
        for (my $j=0; $j< scalar(@remaining); $j++){
            if ($arr1->[$i] eq $remaining[$j]){
                push(@intersection,$arr1->[$i]);
                splice(@remaining,$j,1); #remove matched element
                last;
            }
        }
    }
    return \@intersection;
}

sub get_array_positions
{
    #static
    my %parm = validated_hash(\@_,
                              target => { isa => 'ArrayRef', optional => 0 },
                              keys => { isa => 'ArrayRef', optional => 0 },
                              R_indexing => { isa => 'Bool', optional => 1, default => 1 },
        );
    my $target = $parm{'target'};
    my $keys = $parm{'keys'};
    my $R_indexing = $parm{'R_indexing'};
    my @cols= ();
    return \@cols unless (defined $target and defined $keys);

    my @remaining_keys=();
    foreach my $key (@{$keys}){
        push(@remaining_keys,$key);
    }

    my $start=undef;
    my $end=undef;
    my $matched = 0;
    for (my $i=0; $i< scalar(@{$target}); $i++){
        $matched = 0;
        for (my $j=0; $j< scalar(@remaining_keys); $j++){
            if ($target->[$i] eq $remaining_keys[$j]){
                if ($R_indexing){
                    if (defined $start){
                        $end = $i+1; #add 1 if use R indexing
                    }else{
                        $start = $i+1;
                    }
                }else{
                    $start = $i;
                }
                splice(@remaining_keys,$j,1); #remove matched element
                $matched = 1;
                last;
            }
        }
        if ($R_indexing){
            if ((not $matched) or (scalar(@remaining_keys)<1) or ($i == (scalar(@{$target})-1)))  {
                #either did not match or this is last match
                if (defined $start){
                    if (defined $end){
                        push(@cols,$start.':'.$end);
                    }else{
                        push(@cols,$start);
                    }
                    $start=undef;
                    $end=undef;
                }
            }
        }else{
            push(@cols,$start) if ($matched);
        }

        last if (scalar(@remaining_keys)<1);
    }

    return \@cols; #this is sorted according to target, not keys
}

sub not_empty
{
    # Check if an array is not empty.
    # Returns true if the array is not empty and false otherwise. Also returns false if the array ref is undef.

    my $x = shift;

    return defined $x ? scalar @$x : 0;
}

sub numerical_in
{
    # Check if numerical value is in array
    my $element = shift;
    my $array = shift;

    for my $e (@$array) {
        if ($e == $element) {
            return 1;
        }
    }
    return 0;
}

sub string_in
{
    # Check if string is an array
    my $element = shift;
    my $array = shift;

    for my $e (@$array) {
        if ($e eq $element) {
            return 1;
        }
    }
    return 0;
}

sub is_empty
{
    # Check if an array is empty
    # Returns true if the array is empty and false otherwise. Also returns true if the array ref is undef.

    my $x = shift;

    return defined $x ? !scalar @$x : 1;
}

sub diff
{
    # Calculate the difference between consecutive array elements
    # Returns a new array which is the difference between consecutive array elements in the original array.
    # That is: $result[i] = array[i + 1] - array[i]

    my $a = shift;        # Array reference
    my @d;

    for (my $i = 0; $i < @$a - 1; $i++) {
        $d[$i] = $$a[$i + 1] - $$a[$i];
    }

    return \@d;
}

sub any_nonzero
{
    # Does array have any non-zero element?
    my $a = shift;        # Array reference

    my $found_nonzero=0;
    foreach my $val (@{$a}) {
        if ($val != 0){
            $found_nonzero = 1;
            last;
        }
    }

    return $found_nonzero;
}

sub cumsum
{
    # Array cumulative sum
    # Returns a new array which is the cumulative sum of the elements of the input array.
    # That is: result[i] = sum(array[0] .. array[i])

    my $a = shift;        # Array reference

    my @result;
    my $cumsum = 0;

    foreach (@$a) {
        $cumsum += $_;
        push @result, $cumsum;
    }

    return \@result;
}

sub absolute
{
    # Calculate elementwise absolute value and return a new array

    my $a = shift;

    my @b;

    foreach my $element (@$a) {
        push @b, abs($element);
    }

    return \@b;
}

sub max
{
    # Maximum value in an array or list
    # Examples:
    # $max = max($array_ref);
    # $max = max(@array);
    # ($max, $index) = max($array_ref);
    # ($max, $index) = max(@array);

    # Returns the maximum value of an array by reference or list.
    # If in list context the index of the maximum value is also returned.

    my $a = $_[0];
    my $max;
    my $index;

    # Can handle both array reference and array
    if (ref $a eq "ARRAY") {
        $max = $$a[0];
        $index = 0;

        for (my $i = 0; $i < @$a; $i++) {
            if ($$a[$i] > $max) {
                $max = $$a[$i];
                $index = $i;
            }
        }

    } elsif (defined($a)) {
        $max = $a;
        $index = 0;

        for (my $i = 0; $i < @_; $i++) {
            if ($_[$i] > $max) {
                $max = $_[$i];
                $index = $i;
            }
        }

    } else {
        croak("input reference to max sub-routine not defined");
    }

    # Return index if in array context
    if (wantarray) {
        return ($max, $index);
    } else {
        return $max;
    }
}

sub count_lower
{
    # count_lower - Number of values below limit
    my %parm = validated_hash(\@_,
                              array => { isa => 'ArrayRef', optional => 0 },
                              limit => { isa => 'Num', optional => 0 },
        );
    my $array = $parm{'array'};
    my $limit = $parm{'limit'};

    my $count = 0;
    foreach my $value (@{$array}){
        $count++ if ($value < $limit);
    }
    return $count;
}

sub min
{
    # min - Minimum value in an array or list
    # Examples:
    # $min = min($array_ref);
    # $min = min(@array);
    # ($min, $index) = min($array_ref);
    # ($min, $index) = min(@array);

    # Returns the minimum value of an array by reference or list.
    # If in list context the index of the minimum value is also returned.

    my $a = $_[0];
    my $min;
    my $index;

    # Can handle both array reference and array
    if (ref $a eq "ARRAY") {
        $min = $$a[0];
        $index = 0;

        for (my $i = 0; $i < @$a; $i++) {
            if ($$a[$i] < $min) {
                $min = $$a[$i];
                $index = $i;
            }
        }

    } elsif (defined($a)) {
        $min = $a;
        $index = 0;

        for (my $i = 0; $i < @_; $i++) {
            if ($_[$i] < $min) {
                $min = $_[$i];
                $index = $i;
            }
        }

    } else {
        croak("input reference to min sub-routine not defined");
    }

    # Return index if in array context
    if (wantarray) {
        return ($min, $index);
    } else {
        return $min;
    }
}

sub linspace
{
    # Create an array of linearly spaced numbers
    # Example:
    # $array_ref = linspace($start, $end, $n);

    # Returns a reference to an array with $n linearly spaced numbers in the range $start .. $end

    my $start = shift;
    my $end = shift;
    my $n = shift;

    my @a;

    my $stepsize = ($end - $start) / ($n - 1);
    my $current = $start;

    for (my $i = 0; $i < $n; $i++)
    {
        push @a, $current;
        $current += $stepsize;
    }

    return \@a;
}

sub unique
{
    # Get the unique values of an array
    # Returns an array which contents are the unique values of the input array. The resulting array is sorted.

    my $ref = shift;
    croak("input reference to unique sub-routine not defined") unless defined($ref);

    # store index of each unique element in hash. element is key, index is value
    my %unique_hash;

    for (my $i = 0; $i < @$ref; $i++) {
        unless (defined($unique_hash{$$ref[$i]})) {
            #this is a new value
            $unique_hash{$$ref[$i]} = $i;
        }
    }

    my @uniqueValues = sort { $a <=> $b } keys %unique_hash;

    # Return indices if in array context
    if (wantarray) {
        my @uniqueIndices;
        foreach my $e (@uniqueValues) {
            push @uniqueIndices, $unique_hash{$e};
        }
        return (\@uniqueValues, \@uniqueIndices);
    } else {
        return \@uniqueValues;
    }
}

sub remove_adjacent_duplicates
{
    # Remove the adjacent dumplicates of elements in an array
    # Example: 1,1,1,2,2,2,1,1,1,2,2,2 becomes 1,2,1,2

    my $a = shift;

    if (not defined $a) {
        return;
    }

    my @result;


    my $previous;
    foreach my $e (@$a) {
        if ($e != $previous) {
            push @result, $e;
            $previous = $e;
        }
    }

    return \@result;
}

sub add
{
    # Add one array to another
    # Example:
    # add($dest, $source);

    # Elementwise add the $source array to the $dest array.

    my $dest = shift;
    my $source = shift;

    for (my $i = 0; $i < @$dest; $i++) {
        $$dest[$i] += $$source[$i];
    }
}

sub sum
{
    # Calculate the sum of an array
    # Example:
    # $the_sum = sum($array_ref);

    my ($ref) = pos_validated_list(\@_,
        { isa => 'ArrayRef' },
    );

    my $theSum = 0;

    my @sorted = sort {abs($a) <=> abs($b)} (@$ref);
    foreach my $val (@sorted) {
        $theSum += $val;
    }

    return $theSum;
}

sub mean
{
    # Calculate the mean of an array
    my ($ref) = pos_validated_list(\@_,
        { isa => 'ArrayRef' },
    );

    if (scalar(@$ref) == 0) {
        croak("Input array to mean() is empty");
    }

    return sum($ref) / scalar(@$ref);
}

sub median
{
    # Calculate the median of an array
    # return median which is middle value if uneven number of values
    # or arithemtic mean of two middle values if even number of values
    # if empty input array then return 0

    my $ref = shift;
    return 0 if (scalar(@{$ref}) < 1);
    my @sorted = sort ({$a <=> $b} @{$ref});

    if (scalar(@sorted) % 2) {
        return $sorted[$#sorted / 2];
    } else {
        return ($sorted[@sorted / 2] + $sorted[(@sorted - 2) / 2]) / 2;
    }
}

sub median_and_ci
{
    # Calculate the median and ci of an array
    # return median which is middle value if uneven number of values
    # or arithemtic mean of two middle values if even number of values
    # if empty input array then return undef,undef,undef
    #ci returned as lower and upper percentile limit

    my $ref = shift;
    my $ci = shift;
    my $count = scalar(@{$ref});
    return (undef,undef,undef) if  ($count< 1);
    if ($ci < 0 or $ci>100){
        croak("illegal ci $ci to median_and_ci");
    }
    my @sorted = sort ({$a <=> $b} @{$ref});

    my $median;
    if ($count % 2) {
        $median = $sorted[$#sorted / 2];
    } else {
        $median = ($sorted[@sorted / 2] + $sorted[(@sorted - 2) / 2]) / 2;
    }
    my $low_ci_ind = round((100-$ci)*($count-1)/200);
    my $high_ci_ind = $count - $low_ci_ind - 1; #index of end  of  c_i% interval
    return ($median,$sorted[$low_ci_ind],$sorted[$high_ci_ind]);
}

sub variance
{
    # Calculate the variance of an array
    my $ref = shift;
    my $mean = mean($ref);

    my $sum = 0;
    foreach my $val (@$ref) {
        $sum += ($val - $mean) ** 2;
    }

    return (1 / (scalar(@$ref) - 1)) * $sum;
}

sub rse
{
    #warning: this is for a specific sir case, is probably
    #a special definition of rse
    # Calculate the relative standard error of the expectation of an array.
    #expectation is the estimate
    my ($array,$expectation) = pos_validated_list(\@_,
                                           { isa => 'ArrayRef'},
                                           {isa => 'Num' },
        );
    my $result = 0;
    my $val_count = scalar(@{$array});
    return if (($val_count == 0) or ($val_count == 1));     # Must handle zero length array
    return if ((not defined $expectation) or ($expectation == 0));
    $result = stdev($array)/abs($expectation);
    return $result;
}

sub sem
{
    # Calculate the standard error of the mean of an array.

    my ($array) = pos_validated_list(\@_,
        { isa => 'ArrayRef' },
    );

    my $result = 0;
    my $val_count = scalar(@{$array});
    return if (($val_count == 0) or ($val_count == 1));     # Must handle zero length array
    $result = stdev($array)/sqrt($val_count);
    return $result;
}

sub stdev
{
    # Calculate the standard deviation of an array.
    # Uses sorting before summation to be more numerically stable

    my ($array) = pos_validated_list(\@_,
        { isa => 'ArrayRef' },
    );

    my $result;

    $result = 0;
    my $val_count = scalar(@{$array});
    return if (($val_count == 0) or ($val_count == 1));     # Must handle zero length array
    my @sorted = (sort {$a <=> $b} @{$array}); #sort ascending
    my $sum_values = 0;
    foreach my $val (@sorted) {
        $sum_values += $val;
    }

    my $mean = $sum_values / $val_count;
    my @squared_errors;
    foreach my $val (@sorted) {
        push(@squared_errors, ($val - $mean) ** 2);
    }

    @sorted = (sort {$a <=> $b} @squared_errors); #sort ascending
    my $sum_errors_pow2 = 0;
    foreach my $val (@sorted) {
        $sum_errors_pow2 += $val;
    }

    $result= sqrt ($sum_errors_pow2 / ($val_count - 1));
    return $result;
}

sub is_int
{
    # Test if a (possibly two dimensional) array is all integers
    my $a = shift;

    foreach my $val (@$a) {
        if (ref($val) eq 'ARRAY') {
            foreach my $subval (@$val) {
                if ($subval !~ /^\d+$/) {
                    return 0;
                }
            }
        } else {
            if ($val !~ /^\d+$/) {
                return 0;
            }
        }
    }
    return 1;
}

sub quantile
{
    #mimic R quantile(nubmers,type=2, probs= probs)
    # alternatively use groups instead of probs to create probs automatically. I.e. groups=>4 for quartiles
    #numbers must be sorted!

    my %parm = validated_hash(\@_,
        numbers => { isa => 'ArrayRef', optional => 0 },
        probs => { isa => 'ArrayRef', optional => 1 },
        groups => { isa => 'Int', optional => 1 },
    );
    my $numbers = $parm{'numbers'};
    my $probs = $parm{'probs'};
    my $groups = $parm{'groups'};

    my $n = scalar(@{$numbers});
    croak("Empty set of numbers to quantile") if ($n < 1);
    unless ((defined $probs and scalar(@{$probs}) >= 1) xor defined $groups) {
        croak("Must have either probs or groups");
    }

    if (defined $groups) {
        $probs = [];
        my $cur = 1 / $groups;
        for (my $i = 0; $i < $groups - 1; $i++) {
            push @{$probs}, $cur;
            $cur += 1 / $groups;
        }
    }

    my $m = 0; #R quantile type 2 m value

    my @ans = ();
    foreach my $perc (@{$probs}) {
        my $j = floor($n * $perc + $m);

        my $g = $n * $perc + $m - $j,
        my $gamma = 1;
        $gamma = 0.5 if ($g == 0);

        my $index = $j-1;
        my $quant= (1 - $gamma) * $numbers->[$index] + $gamma * $numbers->[$index + 1];
        #handle out of range
        if (($index+1) >= $n) {
            $quant= $numbers->[$index];
        }elsif ($index< 0 and $gamma == 0.5){
            $quant= $numbers->[$index + 1];
        }
        push (@ans,$quant);
    }
    return \@ans;
}

sub percentile
{
    #sort of the inverse of quantile
    #return percentiles of input test_values
    #the empirical cdf used is a step function that makes the step at each new value in sorted_numbers

    my %parm = validated_hash(\@_,
        test_values => { isa => 'ArrayRef', optional => 0 },
        sorted_numbers => { isa => 'ArrayRef', optional => 0 }
    );
    my $test_values = $parm{'test_values'};
    my $sorted_numbers = $parm{'sorted_numbers'};

    my $n = scalar(@{$sorted_numbers});
    croak("Empty set of sorted_numbers to empirical_distribution") if ($n < 1);
    croak("Empty set of test_values to sorted_numbers") if (scalar(@{$test_values}) < 1);


    my @p_values = ();
    foreach my $value (@{$test_values}){
        #index is pos in sorted_numbers that is larger than $value
        my $index = 0;
        while ($sorted_numbers->[$index] <= $value) {
            $index++;
            last if ($index >= $n); #out of range
        }

        #handle out of range
        if ($index == $n) {
            push(@p_values, 1);
        }elsif ($index == 0) {
            push(@p_values, 1 / ($n + 1));
        }else{
            push(@p_values, $index / $n);
        }
    }

    return \@p_values;
}

sub is_equal
{
    # Check if two arrays are equal
    my $a1 = shift;
    my $a2 = shift;

    if (scalar(@$a1) != scalar(@$a2)) {
        return 0;
    }

    if (scalar(@$a1) == (grep { $a1->[$_] == $a2->[$_] } 0 .. scalar(@$a1) - 1)) {
        return 1;
    } else {
        return 0;
    }
}

sub print
{
    my $array = shift;

    foreach my $e (@$array) {
        print "$e\n";
    }
}

sub remove_NaN
{
    my $array = shift;

    my @clean_array=();
    for (my $i=0; $i<scalar(@{$array}); $i++) {
        if ($array->[$i] ne 'NaN'){
            push(@clean_array,$array->[$i]);
        }
    }
    return \@clean_array;
}

1;
