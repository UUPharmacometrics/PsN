package pharmml;

use strict;
use warnings;
use File::Spec::Functions qw(devnull);
use PsN;
use nonmemrun;

sub is_pharmml
{
	my $filename = shift;

	open my $fh, '<', $filename;
	my $line = <$fh>;
	if ($line =~ /^\<\?xml/) {	# Magic xml descriptor.
		seek $fh, 0, 0;
		while ($line = <$fh>) {			# Check if file contains start of PharmML element
			if ($line =~ /\<PharmML/) {
				close $fh;
				return 1;
			}
		}
	}

	close $fh;
	return 0;
}

sub is_java_installed
{
	if (system('java -version >' . devnull . ' 2>&1') == 0) {
		return 1;
	} else {
		return 0;
	}
}

sub _get_classpath
{
	my $classpath = $PsN::config->{'_'}->{'converter_path'};

	return $classpath;
}

sub convert_file
{
	my $filename = shift;
	my $classpath = _get_classpath;

	my $rc = system("java -cp \"$classpath/*\" eu.ddmore.convertertoolbox.cli.Main -in $filename -out . -sn PharmML -sv 0.3.0 -tn NMTRAN -tv 7.2.0");

	return $rc;
}

sub check_converted_model
{
	my $filename = shift;
	my $ok;

	# Run nmtran to test converted file before using it with PsN

	my $ref = nonmemrun::setup_paths(nm_version => $PsN::nm_version, nmqual => 0);
	my $command = $ref->{'full_path_nmtran'} . "<$filename";

	system($command);
	unlink('FCON', 'FSIZES', 'FSTREAM', 'prsizes.f90', 'FSUBS', 'FSUBS2', 'FSUBS.f90');
	unlink('FSUBS_MU.F90', 'FLIB', 'LINK.LNK', 'FWARN', 'trash.tmp');
	if (not -e 'FREPORT') {
		$ok = 0;
	} else {
		$ok = 1
	}

	unlink('FDATA', 'FREPORT');
	return $ok;
}

1;
