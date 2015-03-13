package standardised_output::table;

# Class representing a generic SO table

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use standardised_output::xml;
use math;

has 'columnId' => ( is => 'rw', isa => 'ArrayRef' );
has 'columnType' => ( is => 'rw', isa => 'ArrayRef' );
has 'valueType' => ( is => 'rw', isa => 'ArrayRef' ); 
has 'columns' => ( is => 'rw', isa => 'ArrayRef' );
has 'name' => ( is => 'rw', isa => 'Str' );
has 'table_file' => ( is => 'rw', isa => 'Str' );
has 'document' => ( is => 'rw', isa => 'Ref' );
has 'precision' => ( is => 'rw', isa => 'Int' );


sub parse
{
    my $self = shift;
    my $node = shift;

    $self->name = $node->nodeName;
    $self->document = $node->ownerDocument;

    my $xpc = standardised_output::xml::get_xpc();

    my @columns = $xpc->findnodes('ds:Definition/ds:Column', $node);

    $self->columnId([]);
    $self->columnType([]);
    $self->valueType([]);

    foreach my $column (@columns) {
        push @{$self->columnId},  $column->getAttribute('columnId');
        push @{$self->columnType},  $column->getAttribute('columnType');
        push @{$self->valueType},  $column->getAttribute('valueType');
    }

    my @rows = $xpc->findnodes('ds:Table/ds:Row', $node);

    $self->columns([[ () x scalar(@columns)] ]);

    for (my $row = 0; $row < scalar(@rows); $row++) {
        my @values = $rows[$row]->getChildrenByTagName('*');
        for (my $col = 0; $col < scalar(@values); $col++) {
            push @{$self->columns->[$col]}, $values[$col]->textContent;
        }
    }
}

sub xml
{
    my $self = shift;
    
    my $doc = $self->document;

    my $table = $doc->createElement($self->name);

    my $def = $doc->createElement("ds:Definition");
    $table->appendChild($def);
    for (my $col = 0; $col < scalar(@{$self->columnId}); $col++) {
        my $column = $doc->createElement("ds:Column");
        $column->setAttribute(columnId => $self->columnId->[$col]);
        $column->setAttribute(columnType => $self->columnType->[$col]);
        $column->setAttribute(valueType => $self->valueType->[$col]);
        $column->setAttribute(columnNum => $col + 1);
        $def->appendChild($column);
    }

    my $numcols = scalar(@{$self->columnId});
    my $numrows;
    $numrows = scalar(@{$self->columns->[0]});

    if (not defined $self->table_file) {
        my $tab = $doc->createElement("ds:Table");
        $table->appendChild($tab);
        for (my $row = 0; $row < $numrows; $row++) {
            my $row_xml = $doc->createElement("ds:Row");
            $tab->appendChild($row_xml);
            for (my $col = 0; $col < $numcols; $col++) {
                my $value_type = uc(substr($self->valueType->[$col], 0, 1)) . substr($self->valueType->[$col], 1);
                my $column_type = $self->columnType->[$col];
                my $element;
                $element = $self->columns->[$col]->[$row];
                my $value = $doc->createElement("ct:" . $value_type);
                if ($value_type eq 'String' and $column_type ne 'id') {
                    $value->appendTextNode($element);
                } else {
                    $value->appendTextNode(math::to_precision($element, $self->precision));
                }
                $row_xml->appendChild($value);
            }
        }
    } else {
        my $filename = $self->table_file . '_' . $self->name . '.csv';
        my $data = $doc->createElement("ds:ImportData");
        $table->appendChild($data);
        $data->setAttribute("oid", $self->name);
        my $path = $doc->createElement("ds:path");
        $data->appendChild($path);
        $path->appendTextNode($filename);
        my $format = $doc->createElement("ds:format");
        $data->appendChild($format);
        $format->appendTextNode("CSV");
        my $delimiter = $doc->createElement("ds:delimiter");
        $data->appendChild($delimiter);
        $delimiter->appendTextNode("COMMA");

        # Create the external table file
        open my $fh, ">", $self->_so_path . $filename;
        for (my $row = 0; $row < $numrows; $row++) {
            for (my $col = 0; $col < $numcols; $col++) {
                my $value_type = uc(substr($self->valueType->[$col], 0, 1)) . substr($self->valueType->[$col], 1);
                my $column_type = $self->columnType->[$col];
                my $element;
                $element = $self->columns->[$col]->[$row];
                if ($value_type eq 'String' and $column_type ne 'id') {
                    print $fh $element;
                } else {
                    print $fh $self->math::to_precision($element, $self->precision);
                }
                print $fh "," if ($col != $numcols - 1);
            }
            print $fh "\n";
        }
        close $fh;
    }

    return $table;
}

sub single_row
{
    # Helper method to fill table from single data row
    my $self = shift;
    my %parm = validated_hash(\@_,
        values => { isa => 'ArrayRef' },
    );
    my @values = @{$parm{'values'}};

    $self->columnType([ ("undefined") x scalar(@values) ]);
    $self->valueType([ ("real") x scalar(@values) ]);

    my @transpose;
    foreach $val (@values) {
        push @transpose, [ $val ];
    }

    $self->columns([@transpose]);
}

sub parameter_table
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str' },
        labels => { isa => 'ArrayRef' },
        values => { isa => 'ArrayRef' },
    );
    my $name = $parm{'name'};
    my $labels = $parm{'labels'};
    my $values = $parm{'values'};

    $self->columnId([ "parameter", $name ]);
    $self->columnType([ ("undefined") x 2 ]);
    $self->valueType([ "string", "real" ]);
    $self->columns([ $labels, $values ]);
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
