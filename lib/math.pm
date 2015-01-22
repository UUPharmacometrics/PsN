package math;

# Package for mathematical function

use strict;
use warnings;
use MooseX::Params::Validate;
use include_modules;

require Exporter;
our @ISA = qw(Exporter);
our %EXPORT_TAGS = ('all' => [ qw(round eps inf ceil usable_number to_precision) ]);
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

sub round
{
    my ($number) = pos_validated_list(\@_,
        { isa => 'Num' },
    );

    my $integer_out;

    my $floor = int($number);
    my $rem = $number - $floor;
    if ($rem >= 0) {
        $integer_out = ($rem >= 0.5) ? $floor + 1 : $floor;
    } else {
        $integer_out = (abs($rem) >= 0.5) ? $floor - 1 : $floor;
    }

    return $integer_out;
}

sub ceil
{
    my ($number) = pos_validated_list(\@_,
        { isa => 'Num' },
    );

    my $integer_out;

    my $floor = int($number);
    my $rem = $number - $floor;
    if ($rem > 0) {
        $integer_out = $floor + 1;
    } else {
        #equal or negative
        $integer_out = $floor;
    } 

    return $integer_out;
}

sub eps
{
    # Machine epsilon for different magnitudes.
    # Gives the correct answer for eps(x) <= 1 otherwise returns 1

    my $x = shift;
	$x = abs($x);		# eps is same for negatives 

	my $e = 1;

	while ($e + $x > $x)
	{
		$e /= 2;
	}

	$e *= 2;

	return $e;
}

sub inf
{
    # A portable infinity.
	return 9**9**9;
}

sub usable_number
{
    my ($number) = pos_validated_list(\@_,
        { isa => 'Any' },
    );

	my $ok = 0;
	if ($number =~ /^\s*([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?\s*$/) {
		$ok = 1;
	}
	return $ok;
}

sub to_precision
{
    # Return a number with the correct precision as a string for output
    my $number = shift;
    my $precision = shift;

    my $form = '%.' . $precision . 'g';
    my $str = sprintf($form, $number);

    return $str;
}

1;
