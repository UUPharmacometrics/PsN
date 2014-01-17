#!/usr/bin/perl

=head1 array.pm

=head2 NAME

array.pm - A module to collect routines that operates on arrays (using references to arrays)

=head2 LICENSE

This is released under the GNU General Public License v2 (or any later version)

=head2 AUTHOR

Rikard Nordgren, <rikard.nordgren@farmbio.uu.se>

=head2 METHODS

=over 4

=cut

package array;

#use Carp;
use include_modules;

require Exporter;
our @ISA = qw(Exporter);
our %EXPORT_TAGS = ('all' => [ qw(not_empty is_empty diff cumsum max min linspace unique add sum mean variance) ]);
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );


=item not_empty

not_empty - Check if an array is not empty.

$boolean = not_empty($array_ref);

Returns true of the array referred to by $array_ref is not empty and false otherwise. Also returns
false if $array_ref is undef.

=cut

sub not_empty
{
	my $x = shift;

	return defined $x ? scalar @$x : 0;
}

=item is_empty

is_empty - Check if an array is empty.

$boolean = is_empty($array_ref);

Returns true of the array referred to by $array_ref is empty and false otherwise. Also returns
true if $array_ref is undef.

=cut

sub is_empty
{
	my $x = shift;

	return defined $x ? !scalar @$x : 1;
}

=item diff

diff - Difference between consecutive array elements

$result = diff($array_ref);

Returns a reference to an array which is the difference between consecutive array elements in the original array.
That is: $result[i] = array[i + 1] - array[i]

=cut

sub diff
{
	my $a = shift;		# Array reference
	my @d;

	for (my $i = 0; $i < @$a - 1; $i++) {
		$d[$i] = $$a[$i + 1] - $$a[$i];
	}

	return \@d;
}

=item cumsum

cumsum - Array cumulative sum

$result = cumsum($array_ref);

Returns a new array which is the cumulative sum of the elements of the input array.
That is: result[i] = sum(array[0] .. array[i])

=cut

sub cumsum
{
	my $a = shift;		# Array reference

	my @result;
	my $cumsum = 0;

	foreach (@$a) {
		$cumsum += $_;
		push @result, $cumsum;
	}

	return \@result;
}

=item max

max - Maximum value in an array or list

$max = max($array_ref);
$max = max(@array);
($max, $index) = max($array_ref);
($max, $index) = max(@array);

Returns the maximum value of an array by reference or list.
If in list context the index of the maximum value is also returned.

=cut


sub max
{
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

=item min

min - Maximum value in an array or list

$min = min($array_ref);
$min = min(@array);
($min, $index) = min($array_ref);
($min, $index) = min(@array);

Returns the minimum value of an array by reference or list.
If in list context the index of the minimum value is also returned.

=cut

sub min
{
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

=item linspace

linspace - Create an array of linearly spaced numbers

$array_ref = linspace($start, $end, $n);

Returns a reference to an array with $n linearly spaced numbers in the range $start .. $end

=cut


sub linspace
{
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

=item unique

unique - Get the unique values of an array

$array = unique($array_ref);

Returns an array which contents are the unique values of the array referenced by $array_ref. The resulting array is sorted.

=cut


sub unique
{
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

=item add

add - Add one array to another

add($dest, $source);

Elementwise add the $source array to the $dest array.

=cut

sub add
{
	my $dest = shift;
	my $source = shift;

	for (my $i = 0; $i < @$dest; $i++) {
		$$dest[$i] += $$source[$i];
	}
}

=item sum

sum - Calculate the sum of an array

$the_sum = sum($array_ref);

Calculates the sum of all the elements in the array referenced by $array_ref

=cut

sub sum
{
	my $ref = shift;
	croak("input reference to sum sub-routine not defined") unless defined($ref);
	my $theSum = 0;

	foreach my $val (@$ref) {
		$theSum += $val;
	}

	return $theSum;
}


sub mean
{
	my $ref = shift;
	
	croak("Input reference to mean() not defined") unless defined($ref);
	croak("Input array to mean() is empty") unless (@$ref > 0);

	return sum($ref) / scalar(@$ref);
}

sub variance
{
	my $ref = shift;
	my $mean = mean($ref);

	my $sum = 0;
	foreach my $val (@$ref) {
		$sum += ($val - $mean) ** 2;
	}

	return (1 / (scalar(@$ref) - 1)) * $sum;
}

=back
