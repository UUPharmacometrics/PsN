package model::annotation;

# Class to parse and store model annotations (i.e. runrecord)

use include_modules;
use Moose;
use MooseX::Params::Validate;

has 'based_on' => ( is => 'rw', isa => 'Str' );
has 'nodOFV' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'census_style' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'description' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'label' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'structural_model' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'covariate_model' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'interindividual_variability' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'interoccasion_variability' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'residual_variability' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'estimation' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'unknown_tags' => ( is => 'rw', isa => 'ArrayRef[Str]' );

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

sub parse_model
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_lines => { isa => 'ArrayRef[Str]' },
    );
    my $model_lines = $parm{'model_lines'};

    my $found_annotation_block = 0;
    my $passed_annotation_block = 0;
    my @annotation_lines;
    my $first_line_of_annotation;
    my $last_line_of_annotation;

    if (not defined $model_lines) {
        return;
    }

    for (my $i = 0; $i < scalar(@{$model_lines}); $i++) {
        my $line = $model_lines->[$i];
        if ($line =~ /^;;(\s*.*\.\s*.*:|;C\s*[Pp]arent\s*=)/ and not $found_annotation_block) {
            $first_line_of_annotation = $i;
            $found_annotation_block = 1;
        }
        if ($line !~ /^(;;|\s*$)/ and $found_annotation_block and not $passed_annotation_block) {
            $last_line_of_annotation = $i - 1;
            $passed_annotation_block = 1;
        }
        if ($found_annotation_block and not $passed_annotation_block) {
            push @annotation_lines, $line;
        }
    }
    $self->parse(annotation_rows => \@annotation_lines);

    # Don't parse annotation further
    if (defined $last_line_of_annotation) {
        splice @{$model_lines}, $first_line_of_annotation, $last_line_of_annotation - $first_line_of_annotation + 1;
    }
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

    foreach my $row (@annotation_rows) {
        chomp $row;
    }

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
        if (not $found) {   # Check census style parent
            if ($annotation_rows[$i] =~ /^;;;C\s*[Pp]arent\s*=\s*(\d*)/) {
                $self->based_on($1);
                if ($1 eq '' or $1 == 0) {
                    $self->nodOFV(1);
                }
                $self->census_style(1);
                next;
            }
        }
        if ($found) {
            if ($attribute eq 'based_on') { # Contents only rest of row
                if ($content =~ /\[nodOFV\]/) {
                    $content =~ s/\s*\[nodOFV\]\s*//;
                    $self->nodOFV(1);
                }
                $self->based_on($content);
                $self->census_style(0);
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
        } else {
            $self->unknown_tags([]) unless defined $self->unknown_tags;
            push @{$self->unknown_tags}, $annotation_rows[$i];
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
                if (not $self->census_style) {
                    my $nodOFV = $self->nodOFV ? ' [nodOFV]' : '';
                    $line .= ' ' . $self->based_on . $nodOFV;
                } else {
                    my $number = $self->nodOFV ? '0' : $self->based_on;
                    $line = ";;;C parent = " . $number;
                }
                push @formatted, $line;
            } else {
                push @formatted, $line;
                foreach my $row (@{$self->$attribute}) {
                    push @formatted, $row;
                }
            }
        }
    }

    if (defined $self->unknown_tags) {
        push @formatted, @{$self->unknown_tags};
    }

    return \@formatted;
}

sub add_empty_tags
{
    my $self = shift;

    foreach my $entry (@tags) {
        my $attribute = $entry->[2];
        if ($attribute eq 'based_on') {
            $self->$attribute('') unless defined $self->$attribute;
        } else {
            $self->$attribute([]) unless defined $self->$attribute;
        }
    }
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
