package nonmemrun;

use Config;
use include_modules;
use Cwd;
use File::Copy 'cp';
use Moose;
use MooseX::Params::Validate;

has 'job_id' => (is => 'rw', isa => 'Int' );
has 'full_path_runscript' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'nm_version' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'email_address' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'send_email' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'nmfe_options' => ( is => 'rw', isa => 'Maybe[Str]' );
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
	unless (defined $self->full_path_runscript){
		my $ref = setup_paths(nm_version => $self->nm_version,
							  nmqual => $self->nmqual);
		$self->full_path_runscript($ref->{'full_path_runscript'});
		$self->nmqual_xml($ref->{'nmqual_xml'}) if ($self->nmqual);
	}

}

sub setup_paths
{
	my %parm = validated_hash(\@_,
							  nm_version => { isa => 'Str', optional => 0 },
							  nmqual => { isa => 'Bool', optional => 0 }
		);
	my $nm_version = $parm{'nm_version'};
	my $nmqual = $parm{'nmqual'};

	my $nmqual_xml = undef;
	my $full_path_runscript = undef;
	my $full_path_nmtran = undef;

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
	
	if ( defined $nmdir and $nmdir ne '' ) {
		unless (-e $nmdir) {
			if ($nmqual) {
				my $mess = "The NMQual installation directory/script $nmdir set for -nm_version=$nm_version does not exist\n";
				croak($mess);
			}else{
				my $mess = "The NONMEM installation directory/NONMEM run script $nmdir set for -nm_version=$nm_version does not exist\n";
				croak($mess);
			}
		}
	}else{
		#not configured
		if ($nmqual) {
			my $mess = "The NMQual installation is not configured for -nm_version=$nm_version. You must correct this in psn.conf\n";
			croak($mess);
		} else {
			my $mess = "The NONMEM installation directory is not configured for -nm_version=$nm_version. You must correct this in psn.conf\n";
			croak($mess);
		}
	}

	my $found_nonmem = 0;

	my @nmtran_candidates = ("$nmdir/tr/NMTRAN.exe", "$nmdir/tr/nmtran.exe", "$nmdir/../tr/NMTRAN.exe", "$nmdir/../tr/nmtran.exe");
	for my $i (0 .. $#nmtran_candidates) {
		if (-x $nmtran_candidates[$i]) {
			$full_path_nmtran = $nmtran_candidates[$i];
			last;
		}
	}

	if ($nmqual) {
		my $xmlpath;
		my $xmlname = 'log.xml';
		if (-d $nmdir) {
			#look for autolog.pl in nmqual subdir
			my $file1 = $nmdir . '/autolog.pl';
			my $file2 = $nmdir . '/nmqual/autolog.pl';		
			if (-e $file1) {
				$full_path_runscript = $file1;
				$xmlpath=$nmdir . '/' . $xmlname;
			} elsif (-e $file2) {
				$full_path_runscript = $file2;
				$xmlpath = $nmdir.'/nmqual/'.$xmlname;
			} else {
				croak("Option nmqual is set and $nmdir is set for -nm_version=$nm_version but PsN cannot find autolog.pl in $nmdir or in $nmdir/nmqual\n");
			}
		} else {
			#nmdir is not a directory
			#we have already checked that $nmdir exists, so it is a file.
			#Warn if not called autolog.pl, error if called nmfe something
			my ($volume, $directory, $file) = File::Spec->splitpath($nmdir);
			$xmlpath = File::Spec->catpath( $volume, $directory, $xmlname );
			if ($file =~ /nmfe/){
				print "\nWarning: $nmdir set for -nm_version=$nm_version looks like a nmfe script, not NMQual's autolog.pl.\n".
					"When option -nmqual is set autolog.pl should be used.\n";
			}elsif (not  $file =~ /autolog/ ){
				print "\nWarning: $nmdir set for -nm_version=$nm_version does not look like NMQual's autolog.pl.\n".
				"When option -nmqual is set autolog.pl should be used.\n";
			}
			$full_path_runscript=$nmdir;
		}
		if (-e $xmlpath){
			$nmqual_xml=$xmlpath;
		}else{
			croak("Option nmqual is set and $nmdir is ok for -nm_version=$nm_version but PsN cannot find log.xml at the expected location\n"."$xmlpath\n");
		}

	}else{
		#not nmqual
		if (-d $nmdir){
			my $suffix = '';
			if ($Config{osname} eq 'MSWin32') {
				$suffix = '.bat';
			}
			my @check_paths = ('/run/', '/util/', '/');
			if ($major == 7) {
				if (not defined $minor) {
					#Try to figure out the subversion
					my $found = 0;
					foreach my $subv (('','1','2','3','4','5','6','7','8','9')) {
						last if $found;
						foreach my $path (@check_paths) {
							if (-x "$nmdir$path" . "nmfe7$subv$suffix") {
								$minor = $subv;
								$found_nonmem = 1;
								$full_path_runscript = $nmdir.$path."nmfe7$subv$suffix";
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
						$full_path_runscript = $nmdir.$path."nmfe$major$minor$suffix";
						$found_nonmem = 1;
						last;
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


		}else{
			#not a directory
			if (-x $nmdir){
				$full_path_runscript =$nmdir;
			}else{
				my $mess = "$nmdir set in psn.conf for -nm_version=$nm_version exists but is not executable.\n".
					"This is not ok unless you are running with NMQual and option -nmqual is set\n";
				croak($mess);
			}
		}
	}
	my %answer;
	$answer{'full_path_runscript'} = $full_path_runscript;
	$answer{'full_path_nmtran'} = $full_path_nmtran;
	$answer{'nmqual_xml'} = $nmqual_xml;
	return \%answer;

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
			if ($self->nmqual) {
				#if nmqual is set then copy file to psn.pnm in cwd instead of putting in options
				cp( $self->parafile, 'psn.pnm' );
			}else{
				$parastring .= '"-parafile=' . $self->parafile . '"';
			}
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
	my $command = $self->full_path_runscript . " psn.mod psn.lst " . $options;

	return $command;
}

sub _create_nmqual_command
{
	my $self = shift;

	my $command_string;

	my $options = $self->_create_nmfe_options;
	my $work_dir = cwd();
	my $xml_file = $self->nmqual_xml;
	my $interface = 'run';
	$interface = 'para' if (defined $self->parafile);
	unless (defined $xml_file){
		croak("xml_file undefined in _create_nmqual_command, this is a bug");
	}
	$command_string = " $xml_file $interface ce $work_dir psn $options ";

	$command_string = "perl ".$self->full_path_runscript." $command_string";
	#print "\n$command_string\n";
	return $command_string;
}

sub pre_compile_cleanup
{
	# leave cleaning to nmfe if NM7
	unless ($PsN::nm_major_version == 7 and defined $PsN::nm_minor_version and $PsN::nm_minor_version > 1) {
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




no Moose;
__PACKAGE__->meta->make_immutable;
1;
