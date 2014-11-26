package annotation;

# Class to parse and store model annotations (i.e. runrecord)

use include_modules;
use Moose;
use MooseX::Params::Validate;

has '' => ( is => 'rw', isa => '' );

sub BUILD
{
    my $self = shift;
    my $params = shift;
    my %parm = validated_hash([$params],
        
        MX_PARAMS_VALIDATE_ALLOW_EXTRA => 1,
    );
    my $ = $parm{''};
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
