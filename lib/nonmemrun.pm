package nonmemrun;

use Config;
use include_modules;
use Moose;
use MooseX::Params::Validate;

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

sub BUILD
{
	my $self = shift;

	$self->nmfe_setup_paths(nm_version => $self->nm_version);
}

sub create_nmfe_command
{
	my $self = shift;

	my $parastring = '';
	if ($PsN::nm_major_version >= 7) {
		$parastring .= '-background ';
	}
	if (defined $self->parafile) {
		if ($PsN::nm_major_version >= 7 and $PsN::nm_minor_version >= 2) {
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

	my $command = $self->full_path_nmfe . " psn.mod psn.lst " . $parastring;

	return $command;
}

sub pre_compile_cleanup
{
	# leave cleaning to nmfe
	unless ($PsN::nm_major_version == 7 and defined $PsN::nm_minor_version and $PsN::nm_minor_version > 1) {
		unlink('FMSG', 'FLIB', 'FCON', 'FDATA', 'FREPORT', 'FSUBS', 'FSUBS.f', 'FSUBS.f90', 'FSUBS2', 'nmprd4p.mod');
		unlink('fsubs', 'fsubs.f90');
		unlink('LINK.LNK', 'FSTREAM', 'PRDERR', 'nonmem.exe', 'nonmem', 'FSUBS.for');
		unlink('nonmem5', 'nonmem6', 'nonmem7', 'nonmem5_adaptive', 'nonmem6_adaptive', 'nonmem7_adaptive');
		unlink('ifort.txt', 'g95.txt', 'gfortran.txt', 'gfcompile.bat', 'g95compile.bat');
	}
	unlink('psn.lst', 'nmfe_error', 'OUTPUT', 'output', 'job_submission_error');
  unlink('lsf_stderr_stdout', 'lsf_jobscript');
}

sub nmfe_setup_paths
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 nm_version => { isa => 'Str', optional => 1 }
	);
	my $nm_version = $parm{'nm_version'};

  #in $nm_version
  # set $self->full_path_nmfe();
  # set $self->full_path_nmtran();

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

no Moose;
__PACKAGE__->meta->make_immutable;
1;
