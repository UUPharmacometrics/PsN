package model::annotation::tag;

# Class for each information tag in the model annotation (i.e. runrecord)

use include_modules;
use Moose;
use MooseX::Params::Validate;

# A tag has the format
# ;; <number>. <name>: <content line 1>
# ;;<content line 2>

has 'number' => ( is => 'rw', isa => 'Str' );
has 'name' => ( is => 'rw', isa => 'Str' );
has 'known' => ( is => 'rw', isa => 'Bool' );
has 'content' => ( is => 'rw', isa => 'ArrayRef[Str]' );    # First line is allowed to be empty

has 'based_on' => ( is => 'rw', isa => 'Bool' );        # Special case for Based on
has 'census_style' => ( is => 'rw', isa => 'Bool' );
has 'nodOFV' => ( is => 'rw', isa => 'Bool' ); 


my @tags = (
    'Based on', 'Description', 'Label', 'Structural model', 'Covariate model',
    'Inter-individual variability', 'Inter-occasion variability', 'Residual variability', 'Estimation'
);


sub parse
{
    # Parse a tag from array of raw lines from model file

    my $self = shift;
    my %parm = validated_hash(\@_,
        rows => { isa => 'ArrayRef' },
    );
    my @rows = defined $parm{'rows'} ? @{$parm{'rows'}} : ();

    my $first = $rows[0];

    if ($first =~ /^;;;C\s*[Pp]arent\s*=(\s*\d*)/) {
        $self->based_on(1);
        $self->content([ $1 ]);
        $self->census_style(1);
        $self->known(1);
        $self->name("Based on");
        $self->number("1");
        if ($1 eq '' or $1 == 0) {
            $self->nodOFV(1);
        }
    } else {
        $first =~ /^;;\s*(.*?)\.\s*(.*?):(.*)/;
        $self->number($1);
        $self->name($2);
        $self->content([ $3 ]);

        foreach my $tag (@tags) {
            if ($tag eq $self->name) {
                $self->known(1);
                last;
            }
        }

        if ($self->name eq "Based on") {
            $self->based_on(1);
            $self->census_style(0);
            $self->nodOFV(0);

            if ($self->content->[0] =~ /\[nodOFV\]/) {
                $self->content->[0] =~ s/\s*\[nodOFV\]\s*//;
                $self->nodOFV(1);
            }
        }
    }

    foreach my $row (@rows[1 .. $#rows]) {
        $row =~ /^;;(.*)/;
        push @{$self->content}, $1;
    }

}

sub format
{
    # Format tag into array of lines for a model file

    my $self = shift;

    my @format;

    if ($self->census_style) {
        my $str = ";;;C parent =";
        if ($self->nodOFV) {
            $str .= ' 0';
        } else {
            my $base = $self->content->[0];
            if ($base !~ /^(\s|$)/) {
                $base .= " ";
            }
            $str .= $base;
        }
        push @format, $str;
    } else {
        my $str = ";; " . $self->number . ". " . $self->name . ":";
        my $first = 1;

        foreach my $row (@{$self->content}) {
            if (not $first) {
                $str = ";;"
            }
            if ($row !~ /^(\s|$)/) {
                $str .= " ";
            }
            $str .= $row;
            if ($first) {
                $first = 0;
                if ($self->based_on and $self->nodOFV) {
                    $str .= ' [nodOFV]';
                }
            }
            push @format, $str;
        }
        if (not defined $self->content or scalar(@{$self->content}) == 0) {
            push @format, $str;
        }
    }

    return \@format;
}

sub set_tag
{
    # Set tag name and content for a known tag

    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str' },
        content => { isa => 'ArrayRef' },
    );
    my $name =$parm{'name'};
    my $content = $parm{'content'};

    $self->known(1);
    for (my $i = 0; $i < scalar(@tags); $i++) {
        if ($tags[$i] eq $name) {
            $self->number($i + 1);
            last;
        }
    }
    $self->name($name);
    if ($name eq 'Based on') {
        $self->based_on(1);
        $self->census_style(0);
        $self->nodOFV(0);
        $self->content($content);
    } else {
        $self->content([ "", @$content ]);
    }
}

sub get_content_nospace
{
    my $self = shift;

    my @content;

    if (defined $self->content) {
        foreach my $row (@{$self->content}) {
            my $l = $row;
            $l =~ s/^\s*//;
            $l =~ s/\s*$//;
            push @content, $l;
        }
    }

    return \@content;
}

sub get_defined_tags
{
    return \@tags;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
