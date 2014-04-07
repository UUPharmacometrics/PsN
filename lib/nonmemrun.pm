package nonmemrun;

use Config;
use include_modules;
use Moose;
use MooseX::Params::Validate;

has 'job_id' => (is => 'rw', isa => 'Int' );
has 'nm_version' => ( is => 'rw', isa => 'Str', required => 1 );
has 'full_path_nmtran' => ( is => 'rw', isa => 'Str' );
has 'full_path_nmfe' => ( is => 'rw', isa => 'Str' );
has 'email_address' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'send_email' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'nmfe_options' => ( is => 'rw', isa => 'Str' );
has 'prepend_flags' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'max_runtime' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'parafile' => (is => 'rw', isa => 'Maybe[Str]' );
has 'nodes' => ( is => 'rw', isa => 'Int', default => 0 );
has 'model' => ( is => 'rw', isa => 'model' );
has 'nmfe_output_file' => ( is => 'rw', isa => 'Str', default => 'nmfe_output.txt' );
has 'nmqual' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'nmqual_xml' => ( is => 'rw', isa => 'Maybe[Str]' );

sub BUILD
{
	my $self = shift;

	$self->nmfe_setup_paths(nm_version => $self->nm_version);
}

sub create_command
{
	my $self = shift;
	my $cmd;

	if (not $self->nmqual) {
		$cmd = $self->_create_nmfe_command;
	} else {
		$cmd = $self->_create_nmqual_command;
	}

	return $cmd;
}

sub _create_nmfe_options
{
	my $self = shift;

	my $parastring = '';
	if ($PsN::nm_major_version >= 7) {
		$parastring .= '-background ';
	}
	if (defined $self->parafile) {
		if ($PsN::nm_major_version > 7 or ($PsN::nm_major_version == 7 and $PsN::nm_minor_version >= 2)) {
			$parastring .= '"-parafile=' . $self->parafile . '"';
		} else {
			croak("Cannot use parafile with NM7.1 or earlier");
		}
		if ($self->nodes > 0) {
			$parastring .= ' "[nodes]=' . $self->nodes . '"';
		}
	}
	if (defined $self->nmfe_options) {
		$parastring .= " " . $self->nmfe_options;
	}

	return $parastring;
}

sub _create_nmfe_command
{
	my $self = shift;

	my $options = $self->_create_nmfe_options;
	my $command = $self->full_path_nmfe . " psn.mod psn.lst " . $options;

	return $command;
}

sub _create_nmqual_command
{
	my $self = shift;

	my $command_string;

	if ($PsN::nm_major_version > 7 or ($PsN::nm_major_version == 7 and $PsN::nm_minor_version >= 2)) {
		my $options = $self->_create_nmfe_options;
		my $work_dir = cwd();
		my $xml_file = $self->nmqual_xml;
		$command_string = " $xml_file run ce $work_dir psn $options ";
	} else {
		$command_string = " psn.mod psn.nmqual_out ";
	}

	my $nmqual;
	if (-x $PsN::nmdir) {
		if (-d $PsN::nmdir) {
			croak("Error in PsN configuration, " . $PsN::nmdir . "\n" . "which should be used with option -nm_version=" . $self->nm_version .
				" according to psn.conf\n" . "is a directory, not a file, and thus cannot be an NMQual-generated perl-script.");
		} else {
			$nmqual = $PsN::nmdir;
		}
	} else {
		croak("Unable to find the NMQual script ". $PsN::nmdir . "\n" .
			"which should be used with option -nm_version=" . $self->nm_version . " according to psn.conf.");
	}

	$command_string = "perl $nmqual $command_string";

	return $command_string;
}

sub pre_compile_cleanup
{
	# leave cleaning to nmfe if NM7
	unless ($PsN::nm_major_version == 7 and defined $PsN::nm_minor_version and $PsN::nm_minor_version > 1) {
		print "\npre_compile_cleanup\n";
		unlink('FMSG', 'FLIB', 'FCON', 'FDATA', 'FREPORT', 'FSUBS', 'FSUBS.f', 'FSUBS.f90', 'FSUBS2', 'nmprd4p.mod');
		unlink('fsubs', 'fsubs.f90');
		unlink('LINK.LNK', 'FSTREAM', 'PRDERR', 'nonmem.exe', 'nonmem', 'FSUBS.for');
		unlink('nonmem5', 'nonmem6', 'nonmem7', 'nonmem5_adaptive', 'nonmem6_adaptive', 'nonmem7_adaptive');
		unlink('ifort.txt', 'g95.txt', 'gfortran.txt', 'gfcompile.bat', 'g95compile.bat');
		unlink('psn.lst', 'OUTPUT', 'output');
	}
	unlink('psn.nmqual_out') if (-e 'psn.nmqual_out');
	unlink('nmfe_error') if (-e 'nmfe_error');
	unlink('job_submission_error') if (-e 'job_submission_error');
	unlink('lsf_stderr_stdout', 'lsf_jobscript');
}

sub nmfe_setup_paths
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 nm_version => { isa => 'Str', optional => 1 }
	);
	my $nm_version = $parm{'nm_version'};

  PsN::set_nonmem_info($nm_version);
  my $nmdir = $PsN::nmdir;
  if (not defined $nmdir) {
    croak("Unknown NONMEM version $nm_version specified.\n");
  }
  my $minor = $PsN::nm_minor_version;
  my $major = $PsN::nm_major_version;
  
  if (not defined $major) {
    croak("No nonmem major version, error config.\n");
  }

  my $nmtr = "$nmdir/tr/nmtran.exe"; 
  if (-x $nmtr) {
	  $self->full_path_nmtran($nmtr);
  }
  my $found_nonmem = 0;

  my $suffix = '';
  if ($Config{osname} eq 'MSWin32') {
		$suffix = '.bat';
	}

	my @check_paths = ('/run/', '/util/', '/');
	if ($major == 7) {
		if (not defined $minor) {
			#Try to figure out the subversion
			my $found = 0;
			foreach my $subv (('1','2','3','4','5','6','7','8','9')) {
				last if $found;
				foreach my $path (@check_paths) {
					if (-x "$nmdir$path" . "nmfe7$subv$suffix") {
						$minor = $subv;
						$found_nonmem = 1;
						$self->full_path_nmfe("$nmdir$path" . "nmfe7$subv$suffix");
						$found = 1;
						last;
					} 
				}
			}
		}
		if (defined $minor) {
			#PsN.pm makes sure this is only one character, no dots
			#only want subversion number if subversion >1
			$minor = '' unless ($minor > 1);
		} else {
			$minor = '';
		}
	}
  
	unless ($found_nonmem) {
		foreach my $path (@check_paths) {
			if( -x "$nmdir$path"."nmfe$major$minor$suffix") {
				$self->full_path_nmfe("$nmdir$path"."nmfe$major$minor$suffix");
				$found_nonmem = 1;
				last;
			} 
		}
	}

  #check if $nmdir is in fact name of executable script, then take that as nmfe (wrapper)
	unless ($found_nonmem) {
		if ((-x "$nmdir") and (not -d "$nmdir")) {
			$self->full_path_nmfe("$nmdir");
			$found_nonmem = 1;

			if (not defined $major) {
				croak("NONMEM major version (5,6 or 7) is not defined in psn.conf for $nm_version");
			}
			if ($major == 7) {
				if (defined $minor) {
					#PsN.pm makes sure this is only one character, no dots
					#only want subversion number if subversion >1
					$minor='' unless ($minor > 1);
				} else {
					$minor = '';
				}
			} else {
				$minor = '';
			}
		}
	}

  if (not $found_nonmem) {
    my $looked_in = join ' or ', @check_paths;
    my $err_version = ( defined $nmdir and $nmdir ne '' ) ? $nmdir : '[not configured]';
    my $mess = "Unable to find executable nmfe$major$minor$suffix ".
			"in any of the subdirectories\n".
			"$looked_in of the NONMEM installation directory.\n".
			"The NONMEM installation directory is $err_version for version [".
			$nm_version."] according to psn.conf.";
    croak($mess);
  }
}

sub postprocessing
{
	my $self = shift;

	if ($self->nmqual) {
		my $outputfile = 'psn.lst';
		my $modelfile = 'psn.mod';
		if (not -e $outputfile) {
			# not NMQual8
			cp($modelfile, $outputfile); #always prepend model to output, as nmfe
			open(OUT, '>>', $outputfile);
			
			print OUT "\n\n";
			if ($PsN::nm_major_version >= 7) {
				open(FH, "<", "FMSG");
				while (<FH>) {
					chomp;
					print(OUT $_ . "\n");
				}
				close(FH);
				print OUT "\n\n";
			}
			my $nmout = "OUTPUT";
			$nmout = "output" unless (-e $nmout);
			if (-e $nmout) {
				open (FH, "<", $nmout);
				while (<FH>) {
					chomp;
					print OUT $_ . "\n";
				}
				close(FH);
				unlink($nmout);
			} else {
				carp("Warning: no file OUTPUT was produced by NMQual script\n");
			}
		}
	}
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
