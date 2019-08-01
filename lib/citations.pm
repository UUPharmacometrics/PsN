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

    my $bibfile;

    if (-e $libpath) {
        $bibfile = $libpath;
    } elsif (-e $devpath) {
        $bibfile = $devpath;
    } else {
        die("Error: Could not find bibliography database \"PsN.bib\". Please check your installation.");
    }

    my $command = $0;
    (undef, undef, $command) = File::Spec->splitpath($command);

    my $references = _scan_bib_file(filename => $bibfile, keyword => $command);

    if (scalar (@$references) == 0) {
        print "No citations found for this tool\n";
    } else {
        print "@$references\n";
    }
}

sub _scan_bib_file
{
    # scan a .bib file. Return all entries containing the keyword.
    # The returned entries is line by line in an array
	my %parm = validated_hash(\@_,
        filename => { isa => 'Str' },
        keyword => { isa => 'Str' }
    );
	my $filename = $parm{'filename'};
	my $keyword = $parm{'keyword'};

    open my $fh, "<", $filename or die("Error: Could not open bibliography database \"PsN.bib\"");

    my $in_ref;
    my $print_ref;
    my @this_ref = ();
    my @ref = ();

    while (<$fh>) {
        if (/^@/) {
            $in_ref = 1;
            push @this_ref, $_;
        } elsif (/^\}/) {
            push @this_ref, $_;
            $in_ref = 0;
            if ($print_ref) {
                push @ref, @this_ref;
                $print_ref = 0;
            }
            @this_ref = ();
        } elsif ($in_ref) {
            push @this_ref, $_;
            if (/^\s*keywords\s*=\s*\{\s*(.*?)\s*\}/) {
                my @keywords = split /\s*,\s*/, $1;
                if (grep { lc($_) eq lc($keyword) } @keywords) {
                    $print_ref = 1;
                }
            }
        }
    }

    close $fh;

    return \@ref;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
