package tool::cdd::jackknife;

use Mouse;
use MouseX::Params::Validate;

extends 'tool::cdd';

has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['jackknife.log'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'jackknife_results.csv' );

sub BUILD
{
    my $self = shift;
    #we do not need to set bins here, default is already number of IDs

}

1;
