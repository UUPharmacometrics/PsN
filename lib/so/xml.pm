package so::xml;

# Low level xml helper functions

use strict;
use warnings;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

sub IsStarting_character
{
    # Character class for the XML character class [\i-[:]] (first character in NCName)
    # Adapted from http://www.regular-expressions.info/shorthand.html#xml
    return <<END;
0041 005A
005F
0061 007A
00C0 00D6
00D8 00F6
00F8 02FF
0370 037D
037F 1FFF
200C 200D
2070 218F
2C00 2FEF
3001 D7FF
F900 FDCF
FDF0 FFFD
END
}

sub IsContinuing_character
{
    # Character class for the XML character class [\c-[:]] (following characters in NCName)
    # Adapted from http://www.regular-expressions.info/shorthand.html#xml
    return <<END;
002D
002E
0030 0039
0041 005A
005F
0061 007A
00B7
00C0 00D6
00D8 00F6
00F8 037D
037F 1FFF
200C 200D
203F
2040
2070 218F
2C00 2FEF
3001 D7FF
F900 FDCF
FDF0 FFFD
END
}

sub match_symbol_idtype
{
    # Check if a string is a legal SymbolIdType
    my $symbol = shift;
    
    if ($symbol =~ /^\p{IsStarting_character}\p{IsContinuing_character}*$/) {
        return 1;
    } else {
        return 0;
    }
}

sub mangle_symbol_idtype
{
    # Mangle a SymbolIdType by replacing all illegal characters with underscore
    my $symbol = shift;

    $symbol =~ s/^\P{IsStarting_character}/_/;
    $symbol =~ s/\P{IsContinuing_character}/_/g;

    return $symbol;
}

sub get_xpc
{
    # class (static) method
    my $xpc = XML::LibXML::XPathContext->new();
    $xpc->registerNs('x' => 'http://www.pharmml.org/so/0.1/StandardisedOutput');
    $xpc->registerNs('ds' => 'http://www.pharmml.org/pharmml/0.6/Dataset');
    $xpc->registerNs('ct' => 'http://www.pharmml.org/pharmml/0.6/CommonTypes');

    return $xpc;
}

1;
