package math;

# Package for mathematical function

use strict;
use warnings;
use MooseX::Params::Validate;
use include_modules;

require Exporter;
our @ISA = qw(Exporter);
our %EXPORT_TAGS = ('all' => [ qw(round eps inf ceil usable_number to_precision convert_float_string logit inverse_logit correlation2unbounded unbounded2correlation) ]);
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

sub logit
{
    my ($number) = pos_validated_list(\@_,
									  { isa => 'Num' },
    );
	if (($number >0) and ($number < 1)){
		return log($number/(1-$number));
	}else{
		return undef;
	}
}

sub inverse_logit
{
    my ($number) = pos_validated_list(\@_,
									  { isa => 'Num' },
		);
	return (exp($number)/(exp($number)+1));

}

sub correlation2unbounded
{
    my ($number) = pos_validated_list(\@_,
									  { isa => 'Num' },
		);
	
	return logit(($number+1)/2); #undef if out of range
}

sub unbounded2correlation
{
    my ($number) = pos_validated_list(\@_,
									  { isa => 'Num' },
		);
	return 2*(inverse_logit($number))-1;
}

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
	if ((defined $number) and $number =~ /^\s*([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?\s*$/) {
		$ok = 1;
		#special check for Windows where types of NaN can string evaluate to 0
		if ($number =~ /^\s*0\s*$/){
			$ok = 0 unless (($number+1) == 1);
		}
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

sub convert_float_string
{
    # Convert a string representing a float into another string
    # given the following rules:
    # 1. Exponential form will be converted to normal decimal form
    # 2. No trailing zeroes after decimal point
    # 3. The same number of decimals must be kept without rounding
    # 4. Remove starting + sign
    my $input = shift;

    # Disassemble number into its sign, integer, decimal and exponent parts
    $input =~ /^([-+]?[0-9]*\.?[0-9]+)([eE][-+]?[0-9]+)?$/;
    my $number = $1;
    my $exponent = $2;
    my $decimal;
    my $integer;
    # FIXME: Plus/minus sign on integerpart must also be disassembled!
    if ($number =~ /\./) {
        my @a = split /\./, $number;
        $integer = $a[0];
        $decimal = $a[1];
    } else {
        $integer = $number;
        $decimal = 0;
    }

    my $sign='';
    if ($integer =~ /^[-+]/) {
        $sign = substr($integer, 0, 1);
    }
    $integer =~ s/^[-+]//;
    if ((defined $sign) and $sign eq "+") {
        $sign = "";
    }

    # Handle exponent
    if (defined $exponent) {
        $exponent =~ s/(e|E)//;
        if ($exponent > 0) {
            for (my $i = 0; $i < $exponent; $i++) {
                if ($decimal ne "") {
                    $integer .= substr($decimal, 0, 1);
                    $decimal = substr($decimal, 1);
                } else {
                    $integer .= "0";
                }
            }
        } elsif ($exponent < 0) {
            for (my $i = 0; $i < abs($exponent); $i++) {
                if ($integer ne "") {
                    $decimal = substr($integer, -1) . $decimal;
                    $integer = substr($integer, 0, -1);
                } else {
                    $decimal = "0" . $decimal;
                }
            }
        }
    }

    # Remove trailing zeroes in decimal part and starting zeroes in the integer part 
    $decimal =~ s/0+$//;
    $integer =~ s/^0*//;
    if ($integer eq "") {
        $integer = "0";
    }

    # Put back together
    if ($decimal eq "") {
        return $sign . $integer;
    } else {
        return "$sign$integer.$decimal";
    }
}

1;
