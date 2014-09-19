package log;

use MooseX::Params::Validate;

require Exporter;
our @ISA = ('Exporter');
our @EXPORT = qw(trace);

$stop_motion;        # Level for stop-motion. Undef: not used. Number: Pause program if message level is below or equal to stop_motion level 

sub trace
{
    my %parm = validated_hash(\@_,
        tool => { isa => 'Str' },
        message => { isa => 'Str' },
        level => { isa => 'Int' },
    );
    my $tool = $parm{'tool'};
    my $message = $parm{'message'};
    my $level = $parm{'level'};

    if (defined $stop_motion) {
        if ($stop_motion >= $level) {
            print "\n";
            print "PsN stop-motion: $tool\n";
            print "$message\n";
            print "(hit return to continue)";
            my $dummy = getc;
        }
    }
}

1;
