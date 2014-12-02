package model::annotation;

# Class to parse and store model annotations (i.e. runrecord)

use include_modules;
use Moose;
use MooseX::Params::Validate;

has 'based_on' => ( is => 'rw', isa => 'Str' );
has 'description' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'label' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'structural_model' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'covariate_model' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'interindividual_variability' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'interoccasion_variability' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'residual_variability' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'estimation' => ( is => 'rw', isa => 'ArrayRef[Str]' );

my @tags = (
    [ 'Based on', 1, 'based_on' ],
    [ 'Description', 2, 'description' ],
    [ 'Label', 3, 'label' ],
    [ 'Structural model', 4, 'structural_model' ],
    [ 'Covariate model', 5, 'covariate_model' ],
    [ 'Inter-individual variability', 6, 'interindividual_variability' ],
    [ 'Inter-occasion variability', 7, 'interoccasion_variability' ],
    [ 'Residual variability', 8, 'residual_variability' ],
    [ 'Estimation', 9, 'estimation' ],
);

sub _get_attribute_from_tag
{
    my $tag = shift;

    foreach my $entry (@tags) {
        if ($entry->[0] eq $tag) {
            return $entry->[0];
        }
    }
    return undef;
}

sub _get_tag_from_attribute
{
    my $attribute = shift;

    foreach my $entry (@tags) {
        if ($entry->[2] eq $attribute) {
            return $entry->[2];
        }
    }
    return undef;
}

sub _get_number_from_attribute
{
    my $attribute = shift;

    foreach my $entry (@tags) {
        if ($entry->[2] eq $attribute) {
            return $entry->[1];
        }
    }
    return undef;
}

sub parse
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        annotation_rows => { isa => 'ArrayRef[Str]' },
    );
    my @annotation_rows = defined $parm{'annotation_rows'} ? @{$parm{'annotation_rows'}} : ();

    my $found;
    my $content;

    for (my $i = 0; $i < scalar(@annotation_rows); $i++) {
        $found = 0;
        my $attribute;
        foreach my $entry (@tags) {
            my $tag = $entry->[0];
            if ($annotation_rows[$i] =~ /^;;\s*(\d*\.\s*)?$tag:\s*(?<text>.*)$/) {
                $content = $+{text};
                $found = 1;
                $attribute = $entry->[2];
                last;
            }
        }
        if ($found) {
            if ($attribute eq 'based_on') { # Contents only rest of row
                $self->based_on($content);
            } else {
                while (++$i < scalar(@annotation_rows)) {
                    if ($annotation_rows[$i] =~ /^;;\s*.*\.\s*.*:/) {
                        $i--;
                        last;
                    } else {
                        $self->$attribute([]) unless defined $self->$attribute;
                        push @{$self->$attribute}, $annotation_rows[$i];
                    }
                }
            }
        }
    }
}

sub format
{
    my $self = shift;

    my @formatted;

    foreach my $entry (@tags) {
        my $attribute = $entry->[2];
        if (defined $self->$attribute) {
            my $line = ";; " . $entry->[1] . ". " . $entry->[0] . ":"; 
            if ($entry->[2] eq 'based_on') {
                $line .= ' ' . $self->based_on;
                push @formatted, $line;
            } else {
                push @formatted, $line;
                foreach my $row (@{$self->$attribute}) {
                    push @formatted, $row;
                }
            }
        }
    }

    return \@formatted;
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
