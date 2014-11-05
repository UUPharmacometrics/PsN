package rplots;

use Config;
use include_modules;
use Cwd;
use File::Copy 'cp';
use Moose;
use MooseX::Params::Validate;
use PsN;
use model;
use OSspecific;

has 'directory' => (is => 'rw', isa => 'Str', required => 1 );
has 'level' => ( is => 'rw', isa => 'Int', required => 1 );
has 'toolname' => (is => 'rw', isa => 'Str', required => 1);
has 'raw_results_file' => (is => 'rw', isa => 'Str', required => 1);
has 'tool_results_file' => (is => 'rw', isa => 'Str');
has 'filename' => (is => 'rw', isa => 'Str');
has '_R_executable' => (is => 'rw', isa => 'Str' );
has 'pdf_title' => (is => 'rw', isa => 'Str' );
has 'indent' => (is => 'rw', isa => 'Str', default => "    " ); #4spaces
has 'standard_preamble' => ( is => 'rw', isa => 'ArrayRef[Str]',default => sub{ [] } );
has 'extra_preamble' => ( is => 'rw', isa => 'ArrayRef[Str]',default => sub{ [] } );
has 'plotcode' => ( is => 'rw', isa => 'ArrayRef[Str]', required => 1);
has 'libraries' => ( is => 'rw', isa => 'ArrayRef[Str]',default => sub{ [] } );

our $preambleline = '#WHEN THIS FILE IS USED AS A TEMPLATE THIS LINE MUST LOOK EXACTLY LIKE THIS';

sub BUILD
{
	my $self = shift;
	my $params = shift; #model argument used in tool rplots->new
	$self->set_R_executable();
	$self->setup(model => $params->{'model'});
}

sub setup
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  model => { isa => 'model', optional => 0 }
		);
	my $model = $parm{'model'};

	unless (defined $self->filename){
		$self->filename('PsN_'.$self->toolname.'_plots.R');
	}

	my ( $ldir, $rawname ) = OSspecific::absolute_path('', $self->raw_results_file);
	$self->raw_results_file($rawname);

	my ($dir, $modelfile) = OSspecific::absolute_path($model-> directory,
												 $model-> filename );

	#figure out table suffix and xpose runno
	my @xpose_names=("sdtab","mutab","patab","catab","cotab","mytab","xptab","cwtab");
	my @tables = @{$model->table_names}; #array of arrays without path
	my $runno;
	my $tabSuffix='';
	for (my $i=0; $i<scalar(@tables); $i++){
		if (defined $tables[$i]){
			foreach my $name (@{$tables[$i]}){
				foreach my $tab (@xpose_names){
					if (index( $name, $tab) == 0){
						#table file names starts like an xpose table
						#figure out number and suffix
						if ($name =~ /^..tab([^.]+)(.*)/){
							$runno=$1;
							if (length($2)>0){
								$tabSuffix = $2;
							}
							last;
						} 
					}
				}
				last if (defined $runno);
			}
		}
		last if (defined $runno);
	}


	#model prefix and suffix
	my $modSuffix='.mod'; 
	my $modPrefix='run'; 
	if (defined $runno and ($modelfile =~ /$runno/)){
		($modPrefix,$modSuffix)=split(/$runno/,$modelfile);
	}else{
		my $tmp = $modelfile; 
		if(	$tmp =~ s/(\.[^.]+)$//){
			$modSuffix = $1;
		}
		if ($tmp =~ s/^([^0-9]+)//){
			$modPrefix = $1;
			$runno = $tmp;
		}
	}

	$runno = '' unless (defined $runno and length($runno)>0);
	unless (defined $self->pdf_title){
		my $runstr = '';
		if (defined $runno and length($runno)>0){
			$runstr = " run $runno";
		}
		$self->pdf_title($self->toolname().' diagnostic plots'.$runstr);
	}

	my @arr =(
		 'rplots.level <- '.$self->level(),
		 "xpose.runno <- '".$runno."'",
		 "toolname <- '".$self->toolname()."'",
		 "pdf.filename <- paste0('PsN_',toolname,'_plots.pdf')",
		 "pdf.title <- '".$self->pdf_title."'",
		 "working.directory<-'".$self->directory."'",
		 "raw.results.file <- '".$self->raw_results_file."'",
		 "model.directory<-'".$dir."'",
		 "model.filename<-'".$modelfile."'",
		 "mod.suffix <- '".$modSuffix."'",
		 "mod.prefix <- '".$modPrefix."'",
		 "tab.suffix <- '".$tabSuffix."'"
		);
	if (-e $self->directory.$self->tool_results_file){
		push(@arr,
			 "tool.results.file <- '".$self->tool_results_file."'");
	}

	#parameter names and numbers, fixed
	unless ($model->is_dummy){
		foreach my $param ('theta','omega','sigma'){
			my $labels = $model->labels(parameter_type => $param,
										problem_numbers => [1],
										generic => 0);
			my $fixed = $model->fixed(parameter_type => $param,
									  problem_numbers => [1]);
			my $labelstring = '';
			my $fixstring = '';
			if (scalar(@{$labels->[0]})>0){
				$labelstring = "'".join("','",@{$labels->[0]})."'";
				my @temp= ('FALSE') x scalar(@{$fixed->[0]}) ;
				for (my$i=0; $i< scalar(@{$fixed->[0]}); $i++){
					$temp[$i] = 'TRUE' if ($fixed->[0]->[$i] >0);
				}
				$fixstring = join(',',@temp);
			}
			push(@arr,
				 $param.'.labels <- c('.$labelstring.')',
				 $param.'.fixed <- c('.$fixstring.')'
				);
		}
		my $nomegas = $model->nomegas(problem_numbers=>[1], with_same => 1, with_correlations => 0);
		my $nsigmas = $model->nsigmas(problem_numbers=>[1], with_same => 1, with_correlations => 0);
		push(@arr,
			 'n.eta <- '.$nomegas->[0],
			 'n.eps <- '.$nsigmas->[0]
			);
	}


	$self->standard_preamble(\@arr);


}

sub set_R_executable
{
	my $self = shift;
	#check in PsN config, or try R --version
	if ( defined $PsN::config -> {'_'} -> {'R'} ) {
		$self->_R_executable($PsN::config -> {'_'} -> {'R'});
	}else{
		my $null = '/dev/null';
		if ($Config{osname} eq 'MSWin32'){
			$null = 'NUL';
		}
		my $rc = system('R --version >'.$null.' 2>&1');
		$rc = $rc >> 8;
		if ($rc == 0){
			$self->_R_executable('R');
		}
	}
}

sub get_preamble()
{
	my $self=shift;

	my $pdfname = $self->filename();
	$pdfname =~ s/\.[^.]*$//;
	$pdfname .= '.pdf';

	my @datearr=localtime;
	my $theDate=sprintf "%4.4d-%2.2d-%2.2d",($datearr[5]+1900),($datearr[4]+1),($datearr[3]);
	my $theTime=sprintf "%2.2d:%2.2d",($datearr[2]),($datearr[1]);
	my @arr=();
	push(@arr,
		 "#START OF AUTO-GENERATED PREAMBLE, WILL BE OVERWRITTEN WHEN THIS FILE IS USED AS A TEMPLATE",
		 "#Created $theDate at $theTime");
	
	foreach my $lib (@{$self->libraries}){
		push(@arr,'require('.$lib.')');
	}
	push(@arr,'');
	push(@arr,@{$self->standard_preamble});
	push(@arr,'');
	push(@arr,@{$self->extra_preamble}); #generic per tool
	push(@arr,"\n"."setwd(working.directory)",
		 "\n############################################################################",
		 "#END OF AUTO-GENERATED PREAMBLE",
		 "$preambleline\n");
	
	
	return \@arr;
}


sub make_plots{
	my $self = shift;

	my $basedir = getcwd();
	chdir($self->directory);
	#always print R script to disk
	$self->print_R_script();
	#sometimes run script - need high enough level and defined R executable
	if ($self->level > 0 and defined ($self->_R_executable)){
		system($self->_R_executable." CMD BATCH ".$self->filename);
	}
	chdir($basedir);
}

sub print_R_script
{
	my $self = shift;

	#filter preamble from plotcode, if any
	my @printcode=();
	foreach my $line (@{$self->plotcode}){
		if ($line =~ /$preambleline/){
			#reset
			#print "\nfound preline\n";
			@printcode=();
		}else{
			push(@printcode,$line);
		}
	}

	open ( SCRIPT, ">" . $self->filename ); #local filename
	print SCRIPT join("\n",@{$self->get_preamble})."\n";
	print SCRIPT "\n";
	print SCRIPT join("\n",@printcode)."\n";
	print SCRIPT "\n";

	my @final =('if (rplots.level > 0){',
				$self->indent().'dev.off()',
				'}');
	print SCRIPT join("\n",@final)."\n";
	close SCRIPT;
}

sub add_preamble
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  code => { isa => 'ArrayRef[Str]', optional => 0 }
		);
	my $code = $parm{'code'};

	push(@{$self->extra_preamble},@{$code});

}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
