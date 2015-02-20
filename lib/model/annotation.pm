package model::annotation;

# Class to parse and store model annotations (i.e. runrecord)

use include_modules;
use Moose;
use MooseX::Params::Validate;
use model::annotation::tag;

has 'tags' => ( is => 'rw', isa => 'ArrayRef[model::annotation::tag]' );


sub _is_start_of_annotation
{
    my %parm = validated_hash(\@_,
        line => { isa => 'Str' },
    );
    my $line = $parm{'line'};

    return $line =~ /^;;(\s*.*?\.\s*.*?:|;C\s*[Pp]arent\s*=)/;
}

sub parse_model
{
    # Find and parse the annotation block in a full model.
    # Remove lines containing the annotation
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_lines => { isa => 'ArrayRef' },
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
        if (_is_start_of_annotation(line => $line) and not $found_annotation_block) {
            $first_line_of_annotation = $i;
            $found_annotation_block = 1;
        }
        if ($line !~ /^;;/ and $found_annotation_block and not $passed_annotation_block) {
            $last_line_of_annotation = $i - 1;
            $passed_annotation_block = 1;
        }
        if ($found_annotation_block and not $passed_annotation_block) {
            push @annotation_lines, $line;
        }
    }

    $self->parse(annotation_rows => \@annotation_lines);

    # Remove the rows containing annotation to not parse it as normal comments
    if (defined $last_line_of_annotation) {
        splice @{$model_lines}, $first_line_of_annotation, $last_line_of_annotation - $first_line_of_annotation + 1;
    }
}

sub parse
{
    # Parse an array of annotation rows
    my $self = shift;
    my %parm = validated_hash(\@_,
        annotation_rows => { isa => 'ArrayRef[Str]' },
    );
    my @annotation_rows = defined $parm{'annotation_rows'} ? @{$parm{'annotation_rows'}} : ();

    my $found;
    my $content;

    my @tags = @{model::annotation::tag::get_defined_tags()};   # Needed?

    foreach my $row (@annotation_rows) {
        chomp $row;
    }

    my @range;
    for (my $i = 0; $i < scalar(@annotation_rows); $i++) {
        if (_is_start_of_annotation(line => $annotation_rows[$i])) {
            push @range, $i;
        }
    }
    push @range, scalar(@annotation_rows);

    $self->tags([]);
    for (my $i = 0; $i < scalar(@range) - 1; $i++) {
        my @entry = @annotation_rows[$range[$i] .. $range[$i + 1] - 1];
        my $tag = model::annotation::tag->new();

        $tag->parse(rows => \@entry);
        push @{$self->tags}, $tag;
    }
}

sub find_tag
{
    # Search for a tag by name. undef if not found
    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str' },
    );
    my $name = $parm{'name'};

    if (defined $self->tags) {
        foreach my $tag (@{$self->tags}) {
            if ($tag->name eq $name) {
                return $tag;
            }
        }
    }
    return;
}

sub get_tag_content
{
    # Get the content of a tag
    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str' },
    );
    my $name = $parm{'name'};

    if (defined $self->tags) {
        foreach my $tag (@{$self->tags}) {
            if ($tag->name eq $name) {
                return $tag->content;
            }
        }
    }
    return;
}

sub get_based_on
{
    my $self = shift;

    my $tag = $self->find_tag(name => 'Based on');
    if (not defined $tag) {
        return;
    }
    my $content = $tag->content;
    if (not defined $content) {
        return;
    }

    my $str = $content->[0];

    $str =~ s/^\s*//;
    $str =~ s/\s*$//;

    return $str;
}

sub set_based_on
{
    my $self = shift;
    my $value = shift;

    $self->set_tag(name => 'Based on', content => [ " $value" ] );
}

sub get_nodOFV
{
    my $self = shift;

    my $tag = $self->find_tag(name => 'Based on');
    if (not defined $tag) {
        return;
    }

    return $tag->nodOFV;
}

sub set_nodOFV
{
    # Currently only workds if 'Based on' is already present
    my $self = shift;
    my $value = shift;

    my $tag = $self->find_tag(name => 'Based on');
    $tag->nodOFV($value);
}

sub format
{
    my $self = shift;

    my @formatted;

    if (defined $self->tags) {
        foreach my $tag (@{$self->tags}) {
            my $format = $tag->format();
            push @formatted, @{$format};
        }
    }
    return \@formatted;
}

sub set_tag
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str' },
        content => { isa => 'ArrayRef' },
    );
    my $name = $parm{'name'};
    my $content = $parm{'content'};

    my $found = 0;

    if (defined $self->tags) {
        foreach my $tag (@{$self->tags}) {
            if ($tag->name eq $name) {
                $tag->content($content);
                $found = 1;
            }
        }
    }

    if (not $found) {
        my $tag = model::annotation::tag->new();
        $tag->set_tag(name => $name, content => $content);
        $self->tags([]) if not defined $self->tags;
        push @{$self->tags}, $tag;
    }

}

sub add_empty_tags
{
    my $self = shift;

    my $tag_names = model::annotation::tag::get_defined_tags();
    $self->tags([]);

    for (my $i = 0; $i < scalar(@$tag_names); $i++) {
        my $tag = model::annotation::tag->new();
        $tag->set_tag(name => $tag_names->[$i], content => []);
        push @{$self->tags}, $tag;
    }
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
