package so::xml;

# Helper function Class for writing to an so xml file

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;

has 'precision' => ( is => 'rw', isa => 'Int', default => 10 );
has 'verbose' => ( is => 'rw', isa => 'Bool', default => 0 );
has '_document' => ( is => 'rw', isa => 'Ref' );                    # The XML document 

sub BUILD
{
    my $self = shift;
    my $doc = XML::LibXML::Document->new('1.0', 'utf-8');
    
    $self->_document($doc);
}

sub create_pharmml_ref
{
    # Add the PharmMLRef element
    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str' },
    );
    my $name = $parm{'name'};

    my $doc = $self->_document;
    my $pharmmlref = $doc->createElement("PharmMLRef");
    $pharmmlref->setAttribute("name", $name);

    return $pharmmlref;
}

sub create_typed_element
{
    # Create an element such as <ct:String>text</ct:String>
    my $self = shift;
    my %parm = validated_hash(\@_,
        type => { isa => 'Str' },
        content => { isa => 'Str' },
    );
    my $type = $parm{'type'};
    my $content = $parm{'content'};

    my $element = XML::LibXML::Element->new('ct:' . $type);
    $element->appendTextNode($content);

    return $element;
}

sub create_message
{
    # Create an Message element for TaskInformation
    my $self = shift;
    my %parm = validated_hash(\@_,
        message => { isa => 'HashRef' },
    );
    my $message = $parm{'message'};

    my $doc = $self->_document;

    my $node = $doc->createElement("Message");
    $node->setAttribute('type', $message->{'type'});
    my $toolname = $doc->createElement('Toolname');
    $toolname->appendChild($self->create_typed_element(type => 'String', content => $message->{'toolname'}));
    $node->appendChild($toolname);
    my $name = $doc->createElement('Name');
    $name->appendChild($self->create_typed_element(type => 'String', content => $message->{'name'}));
    $node->appendChild($name);
    my $content = $doc->createElement('Content');
    $content->appendChild($self->create_typed_element(type => 'String', content => $message->{'content'}));
    $node->appendChild($content);
    my $severity = $doc->createElement('Severity');
    $severity->appendChild($self->create_typed_element(type => 'Int', content => $message->{'severity'}));
    $node->appendChild($severity);

    if ($self->verbose) {
        if ($message->{'type'} eq 'ERROR' or $message->{'type'} eq 'WARNING') {
            print $message->{'type'}, " ", $message->{'name'}, ": ", $message->{'content'}, "\n";
        }
    }

    return $node;
}

sub create_matrix
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        rownames => { isa => 'ArrayRef' },
        colnames => { isa => 'ArrayRef' },
        matrix => { isa => 'ArrayRef[ArrayRef]' },
    );
    my $rownames = $parm{'rownames'};
    my $colnames = $parm{'colnames'};
    my $matrix = $parm{'matrix'};

    my $doc = $self->_document;

    my $matrix_xml = $doc->createElement("ct:Matrix");
    $matrix_xml->setAttribute(matrixType => "Any");

    my $rownames_xml = $doc->createElement("ct:RowNames");
    $matrix_xml->appendChild($rownames_xml);
    foreach my $name (@$rownames) {
        my $string = $doc->createElement("ct:String");
        $string->appendTextNode($name);
        $rownames_xml->appendChild($string);
    }

    my $colnames_xml = $doc->createElement("ct:ColumnNames");
    $matrix_xml->appendChild($colnames_xml);
    foreach my $name (@$colnames) {
        my $string = $doc->createElement("ct:String");
        $string->appendTextNode($name);
        $colnames_xml->appendChild($string);
    }

    for my $row (@$matrix) {
        my $matrix_row = $doc->createElement("ct:MatrixRow");
        $matrix_xml->appendChild($matrix_row);
        for my $element (@$row) {
            my $real = $doc->createElement("ct:Real");
            $real->appendTextNode(math::to_precision($element, $self->precision));
            $matrix_row->appendChild($real);
        }
    }

    return $matrix_xml;
}

sub find_or_create_node
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        root_node => { isa => 'Ref' },
        node_name => { isa => 'Str' },      # Can be a one level XPath
    );
    my $root_node = $parm{'root_node'};
    my $node_name = $parm{'node_name'};

    my $doc = $self->_document;
    my $node;
    if (not $root_node->exists($node_name)) {
        $node = $doc->createElement($node_name);
        $root_node->appendChild($node);
    } else {
        ($node) = $root_node->findnodes($node_name);
    }

    return $node;
}

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
    my $self = shift;
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
    my $self = shift;
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

no Moose;
__PACKAGE__->meta->make_immutable;
1;
