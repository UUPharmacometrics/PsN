package so::table;

# Class representing a generic SO table

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use math;
use so::xml;

has 'columnId' => ( is => 'rw', isa => 'ArrayRef' );
has 'columnType' => ( is => 'rw', isa => 'ArrayRef' );
has 'valueType' => ( is => 'rw', isa => 'ArrayRef' ); 
has 'columns' => ( is => 'rw', isa => 'ArrayRef' );
has 'name' => ( is => 'rw', isa => 'Str' );
has 'table_file' => ( is => 'rw', isa => 'Maybe[Str]' );

sub parse
{
    my $self = shift;
    my $node = shift;

    $self->name = $node->nodeName;

    my $xpc = so::xml::get_xpc();

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
            if ($values[$col]->nodeName eq "NA") {
                push @{$self->columns->[$col]}, undef;
            } else {
                push @{$self->columns->[$col]}, $values[$col]->textContent;
            }
        }
    }
}

sub xml
{
    my $self = shift;
    
    my $table = XML::LibXML::Element->new($self->name);

    my $def = XML::LibXML::Element->new("ds:Definition");
    $table->appendChild($def);
    for (my $col = 0; $col < scalar(@{$self->columnId}); $col++) {
        my $column = XML::LibXML::Element->new("ds:Column");
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
        my $tab = XML::LibXML::Element->new("ds:Table");
        $table->appendChild($tab);
        for (my $row = 0; $row < $numrows; $row++) {
            my $row_xml = XML::LibXML::Element->new("ds:Row");
            $tab->appendChild($row_xml);
            for (my $col = 0; $col < $numcols; $col++) {
                my $value_type = uc(substr($self->valueType->[$col], 0, 1)) . substr($self->valueType->[$col], 1);
                my $column_type = $self->columnType->[$col];
                my $element;
                $element = $self->columns->[$col]->[$row];
                my $value;
                if (not defined $element) {
                    $value = XML::LibXML::Element->new("ct:NA");
                } else {
                    $value = XML::LibXML::Element->new("ct:" . $value_type);
                    if ($value_type eq "Real") {
                        $value->appendTextNode(math::convert_float_string($element));
                    } else {
                        $value->appendTextNode($element);
                    }
                }
                $row_xml->appendChild($value);
            }
        }
    } else {
        my $filename = $self->table_file . '_' . $self->name . '.csv';
        my $data = XML::LibXML::Element->new("ds:ImportData");
        $table->appendChild($data);
        $data->setAttribute("oid", $self->name);
        my $path = XML::LibXML::Element->new("ds:path");
        $data->appendChild($path);
        $path->appendTextNode($filename);
        my $format = XML::LibXML::Element->new("ds:format");
        $data->appendChild($format);
        $format->appendTextNode("CSV");
        my $delimiter = XML::LibXML::Element->new("ds:delimiter");
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
                print $fh $element;
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
        types => { isa => 'ArrayRef', optional => 1 },
    );
    my @values = @{$parm{'values'}};
    my $types = $parm{'types'};

    if (defined $types) {
        $self->columnType($types) 
    } else {
        $self->columnType([ ("undefined") x scalar(@values) ]);
    }
    $self->valueType([ ("real") x scalar(@values) ]);

    my @transpose;
    foreach my $val (@values) {
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

    $self->columnId([ "Parameter", $name ]);
    $self->columnType([ ("undefined") x 2 ]);
    $self->valueType([ "string", "real" ]);
    $self->columns([ $labels, $values ]);
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
