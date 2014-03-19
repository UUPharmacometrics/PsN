package nonmemrun::ud;

use include_modules;
use Config;
use Moose;
use MooseX::Params::Validate;

extends 'nonmemrun';

has 'directory' => ( is => 'rw', isa => 'Str' );
has 'run_no' => ( is => 'rw', isa => 'Int' );  

sub submit
{
	my $self = shift;
	my $jobId = -1;

	my $script;
	unless(defined $PsN::config->{'_'}->{'ud_nonmem'}) {
		if( $Config{osname} eq 'MSWin32' ) {
			$script = 'nonmem.bat';
		} else {
			$script = 'nonmem.sh';
		}
	} else {
		$script = $PsN::config -> {'_'} -> {'ud_nonmem'};
	}
	
	if (system("$script -s " . $self->model->filename . "> nonmem_sh_stdout")) {
		my $error = "$!";
		print "UD submit script failed, check that $script is in your PATH.\nSystem error message: $error";
		chomp($error);
		system('echo ' . $error . ' > job_submission_error');
	}else{
		if (open(JOBFILE, "JobId")){ 
			$jobId = <JOBFILE>;
			close(JOBFILE);
		}else{
			my $error = "$!";
			print "UD submit script failed, could not open file JobID: $error";
			chomp($error);
			system('echo ' . $error . ' > job_submission_error');
		}
	}
	$self->job_id($jobId);
	return $jobId;
}

sub monitor
{
	my $self = shift;
	my $jobId = $self->job_id;

	#this cannot possible work, but leave it here if any user wants to use ud. 
	# will be easy to fix this
  my $script;
  unless (defined $PsN::config->{'_'}->{'ud_nonmem'}) {
    if ($Config{osname} eq 'MSWin32') {
      $script = 'nonmem.bat';
    } else {
      $script = 'nonmem.sh';
    }
  } else {
    $script = $PsN::config->{'_'}->{'ud_nonmem'};
  }

  my $stdout; # Variable to store output from "nonmem.bat"

  my $response = `$script -l $jobId 2>&1`;

  carp("$response");
  if ($response =~ /Job State:\s+Completed/) {  # regexp to find finished jobs.
		carp("Returning $jobId");
		$self->retrieve(jobId => $jobId, run_no => $self->run_no);
		return $jobId; # Return the jobId found.
  }

  return 0;
}

sub retrieve
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 jobId => { isa => 'Int', optional => 1 },
		 run_no => { isa => 'Int', optional => 1 }
	);
	my $jobId = $parm{'jobId'};
	my $run_no = $parm{'run_no'};

  my $script;
  unless (defined $PsN::config->{'_'}->{'ud_nonmem'}) {
    if ($Config{osname} eq 'MSWin32') {
      $script = 'nonmem.bat';
    } else {
      $script = 'nonmem.sh';
    }
  } else {
    $script = $PsN::config->{'_'}->{'ud_nonmem'};
  }

  my $subDir = "NM_run" . ($run_no + 1);
  my ($tmp_dir, $file) = OSspecific::absolute_path($self->directory . '/' . $subDir, '');
  if (system("$script -b -c -d " . $tmp_dir . " -r $jobId > nonmem_bat_stdout")) {
    croak("UD submit script failed.\nSystem error message:$!" );
  }
  
  if ($Config{osname} ne 'MSWin32') {
    cp($tmp_dir . '/psn.LST', $tmp_dir . '/psn.lst');
    unlink($tmp_dir . '/psn.LST');
  }
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
