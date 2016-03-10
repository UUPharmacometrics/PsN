package citations;

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use File::Spec;

sub print_citations
{
    my $path = __FILE__;

    (my $volume, my $dirs, undef) = File::Spec->splitpath($path);
    my $libpath = File::Spec->catpath($volume, $dirs, "PsN.bib");
    my $devpath = File::Spec->catpath($volume, $dirs, "../doc/PsN.bib");

    my $fh;

    if (-e $libpath) {
        open $fh, "<", $libpath;
    } elsif (-e $devpath) {
        open $fh, "<", $devpath;
    } else {
        croak("Could not find bibliography database \"PsN.bib\". Please check your installation.");
    }

    my $command = $0;
    (undef, undef, $command) = File::Spec->splitpath($command);    

    my $in_ref;
    my $print_ref;
    my $citations_found;
    my @this_ref = ();

    while (<$fh>) {
        if (/^@/) {
            $in_ref = 1;
            push @this_ref, $_;
        } elsif (/^\}/) {
            push @this_ref, $_;
            $in_ref = 0;
            if ($print_ref) {
                print "@this_ref";
                $print_ref = 0;
                $citations_found = 1;
            }
            @this_ref = ();
        } elsif ($in_ref) {
            push @this_ref, $_;
            if (/^\s*keywords\s*=\s*\{\s*(.*?)\s*\}/) {
                my @keywords = split /\s*,\s*/, $1;
                if (grep { lc($_) eq lc($command) } @keywords) {
                    $print_ref = 1;
                }
            }
        }
    }

    close $fh;

    if (not $citations_found) {
        print "No citations found for this tool\n";
    }
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
