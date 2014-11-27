package model::annotation;

# Class to parse and store model annotations (i.e. runrecord)

use include_modules;
use Moose;
use MooseX::Params::Validate;

has 'based_on' => ( is => 'rw', isa => 'Str' );
has 'description' => ( is => 'rw', isa => 'Str' );
has 'label' => ( is => 'rw', isa => 'Str' );
has 'structural_model' => ( is => 'rw', isa => 'Str' );
has 'covariate_model' => ( is => 'rw', isa => 'Str' );
has 'interindividual_variability' => ( is => 'rw', isa => 'Str' );
has 'interoccasion_variability' => ( is => 'rw', isa => 'Str' );
has 'residual_variability' => ( is => 'rw', isa => 'Str' );
has 'estimation' => ( is => 'rw', isa => 'Str' );

%tag_hash = (
    'Based on' => 'based_on',
    'Description' => 'description',
    'Label' => 'label',
    'Structural model' => 'structural_model',
    'Covariate model' => 'covariate_model',
    'Inter-individual variability' => 'interindividual_variability',
    'Inter-occasion variability' => 'interoccasion_variability',
    'Residual variability' => 'residual_variability',
    'Estimation' => 'estimation',
);


sub parse
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        annotation_rows => { isa => 'ArrayRef[Str]' },
    );
    my @annotation_rows = defined $parm{'annotation_rows'} ? @{$parm{'annotation_rows'}} : ();

    my $found;

    for (my $i = 0; $i < scalar(@annotation_rows); $i++) {
        $found = 0;
        foreach my $tag (keys %tag_hash) {
            if ($annotation_rows[$i] =~ /^;;\s*(\d*\.\s*)?$tag:\s*(.*)$/) {
                $found = 1;
                last;
            }
        }
        if ($found) {
            if ($tag_hash{$tag} eq 'based_on') { # Contents only rest of row
                $self->based_on($1);
            } else {
                
            }
        }
    }
}

sub format
{

}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
