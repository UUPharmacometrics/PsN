package nonmemrun;

use Config;
use include_modules;
use Cwd;
use File::Copy 'copy';
use Mouse;
use MouseX::Params::Validate;

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

sub BUILD
{
    my $self = shift;
    unless (defined $self->full_path_runscript){
        my $ref = setup_paths(nm_version => $self->nm_version);
        $self->full_path_runscript($ref->{'full_path_runscript'});
    }
}

sub setup_paths
{
    my %parm = validated_hash(\@_,
        nm_version => { isa => 'Str', optional => 0 },
    );
    my $nm_version = $parm{'nm_version'};

    my $full_path_runscript = undef;
    my $full_path_nmtran = undef;

    PsN::set_nonmem_info($nm_version);
    my $nmdir = $PsN::nmdir; #if only name of executable set, and executable is in path, then this is the full path found in PsN.pm

    if ( defined $nmdir and $nmdir ne '' ) {
        unless (-e $nmdir) {
            my $mess = "The NONMEM installation directory/NONMEM run script $nmdir set for -nm_version=$nm_version does not exist\n";
            croak($mess);
        }
    }else{
        #not configured
        my $mess = "The NONMEM installation directory is not configured for -nm_version=$nm_version. You must correct this in psn.conf\n";
        croak($mess);
    }

    my $minor = $PsN::nm_minor_version;
    my $major = $PsN::nm_major_version;

    unless ((defined $major) and length($major)>0) {
        croak("No nonmem major version defined, error in psn.conf for -nm_version=$nm_version\n");
    }

    my $found_nonmem = 0;

    my @nmtran_candidates = ("$nmdir/tr/NMTRAN.exe", "$nmdir/tr/nmtran.exe", "$nmdir/../tr/NMTRAN.exe", "$nmdir/../tr/nmtran.exe");
    for my $i (0 .. $#nmtran_candidates) {
        if (-x $nmtran_candidates[$i]) {
            $full_path_nmtran = $nmtran_candidates[$i];
            last;
        }
    }

    if (-d $nmdir){
        my $found_full=    find_nmfe_script(nmdir=>$nmdir,major => $major,minor => $minor);

        if  (defined $found_full) {
            $full_path_runscript = $found_full;
        }else{
            my $mess = "Unable to find an executable nmfe script ".
                "in any of the subdirectories\n".
                " ./ or /run/ or /util/ of the NONMEM installation directory\n".
                "$nmdir\n that is set for -nm_version=".$nm_version." in psn.conf.";
            croak($mess);
        }
    }else{
        #not a directory
        if (-x $nmdir){
            $full_path_runscript =$nmdir;
        }else{
            my $mess = "$nmdir set in psn.conf for -nm_version=$nm_version exists but is not executable.\n";
            croak($mess);
        }
    }
    my %answer;
    $answer{'full_path_runscript'} = $full_path_runscript;
    $answer{'full_path_nmtran'} = $full_path_nmtran;
    return \%answer;

}

sub find_nmfe_script
{
    #static no shift
    my %parm = validated_hash(\@_,
                              nmdir => { isa => 'Str', optional => 0 },
                              major => { isa => 'Str', optional => 0 },
                              minor => { isa => 'Maybe[Str]', optional => 1 },
        );
    my $nmdir = $parm{'nmdir'};
    my $major = $parm{'major'};
    my $minor = $parm{'minor'};

    my $found_full_path=undef;

    my $suffix = '';
    if ($Config{osname} eq 'MSWin32') {
        $suffix = '.bat';
    }
    my @check_paths = ('/run/', '/util/', '/');
    my @check_subversions=();
    push (@check_subversions,$minor) if (defined $minor); #make sure this is checked first, if set
    push (@check_subversions,('','1','2','3','4','5','6','7','8','9'));

    foreach my $subv (@check_subversions) {
        last if (defined $found_full_path);
        foreach my $path (@check_paths) {
            my $fullp="$nmdir$path"."nmfe$major$subv$suffix";
            if (-x $fullp) {
                $found_full_path = $fullp;
                if (defined $minor and length($minor)>0 and ($minor ne $subv)){
                    #print warning here???
                    1;
                }

                last;
            }
        }
    }

    return $found_full_path;

}

sub create_command
{
    my $self = shift;
    my $cmd = $self->_create_nmfe_command;
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
    my $command = $self->full_path_runscript . " psn.mod psn.lst " . $options;

    return $command;
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
    unlink('nmfe_error') if (-e 'nmfe_error');
    unlink('job_submission_error') if (-e 'job_submission_error');
    unlink('lsf_stderr_stdout', 'lsf_jobscript');
}

1;
