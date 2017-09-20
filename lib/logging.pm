package logging;

# This is the new logging module of PsN. The intention is to move all output messages here from the regular prints, include_module, ui-module and the log-module
# It aims to have a similar interface as the python logging module
# The actual logger class is logging::logger

use MooseX::Params::Validate;
use logging::logger;

require Exporter;
our @ISA = ('Exporter');
our @EXPORT = qw(get_logger);

sub get_logger
{
    my $name = shift;
    my $logger = logging::logger->new(name => $name);
    return $logger;
}

1;
