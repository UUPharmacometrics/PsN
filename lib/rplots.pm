package rplots;

use Config;
use include_modules;
use Cwd;
use File::Copy 'cp';
use File::Spec;
use Moose;
use MooseX::Params::Validate;
use PsN;
use model;
use OSspecific;

has 'directory' => (is => 'rw', isa => 'Str', required => 1 );
has 'level' => ( is => 'rw', isa => 'Int', required => 1 );
has 'toolname' => (is => 'rw', isa => 'Str', required => 1);
has 'raw_results_file' => (is => 'rw', isa => 'Str');
has 'tool_results_file' => (is => 'rw', isa => 'Str');
has 'filename' => (is => 'rw', isa => 'Str');
has '_R_executable' => (is => 'rw', isa => 'Str' );
has 'pdf_title' => (is => 'rw', isa => 'Str' );
has 'indent' => (is => 'rw', isa => 'Str', default => "    " ); #4spaces
has 'standard_preamble' => ( is => 'rw', isa => 'ArrayRef[Str]',default => sub{ [] } );
has 'extra_preamble' => ( is => 'rw', isa => 'ArrayRef[Str]',default => sub{ [] } );
has 'plotcode' => ( is => 'rw', isa => 'ArrayRef[Str]', required => 1);
has 'subset_variable' => (is => 'rw', isa => 'Maybe[Str]' );
has 'R_markdown' => (is => 'rw', isa => 'Bool', default => 0);
has 'rmarkdown_installed' => (is => 'rw', isa => 'Bool', default => 0);

our $preambleline = '#WHEN THIS FILE IS USED AS A TEMPLATE THIS LINE MUST LOOK EXACTLY LIKE THIS';

sub BUILD
{
	my $self = shift;
	my $params = shift; 
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
		if($self->R_markdown && $self->rmarkdown_installed) {
			$self->filename('PsN_'.$self->toolname.'_plots.Rmd');
		} else {
			$self->filename('PsN_'.$self->toolname.'_plots.R');
		}	
	}
	
	if (defined $self->raw_results_file){
		my ( $ldir, $rawname ) = OSspecific::absolute_path('', $self->raw_results_file);
		$self->raw_results_file($rawname);
	}
	my ($modeldir, $modelfile) = OSspecific::absolute_path($model-> directory,
												 $model-> filename );

	#figure out table suffix and xpose runno
	my @xpose_names=("sdtab","mutab","patab","catab","cotab","mytab","xptab","cwtab");
	my @tables = @{$model->table_names}; #array of arrays without path
	my $runno;
	my $is_sim=0;
	my $tabSuffix='';
	for (my $i=0; $i<scalar(@tables); $i++){
		if (defined $tables[$i]){
			foreach my $name (@{$tables[$i]}){
				my $tmp = model::get_xpose_runno_and_suffix(filename => $name);
				if (defined $tmp->[0]){
					#have runno
					$runno = $tmp->[0];
					#remove trailing sim if there
					$is_sim=1 if ($runno =~ /sim$/);
					$runno =~ s/sim$//;

					$tabSuffix = $tmp->[1];
					last;
				}
			}
		}
		last if (defined $runno);
	}


	#model prefix and suffix
	my $modSuffix='.mod'; 
	my $modPrefix='run'; 
	if (defined $runno and ($modelfile =~ /$runno/)){
		($modPrefix,$modSuffix)=split(/$runno/,$modelfile);
		if ($is_sim){
			$modSuffix =~ s/^sim//;
		}
	}else{
		my $tmp = $modelfile; 
		if(	$tmp =~ s/(\.[^.]+)$//){
			$modSuffix = $1;
		}
		if ($tmp =~ s/^([^0-9]+)//){
			$modPrefix = $1;
			$runno = $tmp unless (defined $runno);
		}
	}

	$runno = '' unless (defined $runno and length($runno)>0);
	unless (defined $self->pdf_title){
		my $runstr = '';
		if (defined $runno and length($runno)>0){
			$runstr = " run $runno";
		}else{
			$runstr = " $modelfile";
		}
		$self->pdf_title($self->toolname().' diagnostic plots'.$runstr);
	}

	my $subsetstring = 'NULL';
	$subsetstring = "'".$self->subset_variable."'" if (defined $self->subset_variable and length($self->subset_variable)>0);
	my $levelstring=1;
	if ($self->level() > 1){
		$levelstring = $self->level();
	}

	my $workingdirectory = $self->directory;
    my $rscripts_path = $PsN::Rscripts_dir;
	#Replace single backslash with double, assume windows, but do not change if already double
	$workingdirectory = double_backslashes(string => $workingdirectory);
	$modeldir = double_backslashes(string => $modeldir);
	$rscripts_path = double_backslashes(string =>$rscripts_path);

	my @arr =(
		 'rplots.level <- '.$levelstring,
		 "xpose.runno <- '".$runno."'",
		 "toolname <- '".$self->toolname()."'",
		 "pdf.filename <- paste0('PsN_',toolname,'_plots.pdf')",
		 "pdf.title <- '".$self->pdf_title."'",
		 "working.directory<-'".$workingdirectory."'",
		 "model.directory<-'".$modeldir."'",
		 "model.filename<-'".$modelfile."'",
		 "subset.variable<-".$subsetstring,
		 "mod.suffix <- '".$modSuffix."'",
		 "mod.prefix <- '".$modPrefix."'",
		 "tab.suffix <- '".$tabSuffix."'",
         "rscripts.directory <- '".$rscripts_path."'",
		);
	if (defined $self->tool_results_file and (-e $self->directory.$self->tool_results_file)){
		push(@arr,
			 "tool.results.file <- '".$self->tool_results_file."'");
	}
	if (defined $self->raw_results_file and (-e $self->directory.$self->raw_results_file)){
		push(@arr,
			 "raw.results.file <- '".$self->raw_results_file."'");
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
				for (my $i=0; $i< scalar(@{$fixed->[0]}); $i++){
					$temp[$i] = 'TRUE' if (defined $fixed->[0]->[$i] and ($fixed->[0]->[$i] >0));
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
		my $neta=0;
		my $neps=0;
		$neta = $nomegas->[0] if (defined $nomegas->[0]);
		$neps = $nsigmas->[0] if (defined $nsigmas->[0]);
		push(@arr,
			 'n.eta <- '.$neta,
			 'n.eps <- '.$neps
			);
	}


	$self->standard_preamble(\@arr);


}

sub double_backslashes{
	my %parm = validated_hash(\@_,
							  string => { isa => 'Str', optional => 0 }
		);
	my $string = $parm{'string'};

	#lookbehind and lookahead. Replace single backslash with double, but do not change if already double
	$string =~ s/(?<!\\)\\(?!\\)/\\\\/g;
	return $string;

}

sub set_R_executable
{
	my $self = shift;
	#check in PsN config, or try R --version
	my $R = PsN::get_R_exec();
	$self->_R_executable($R) if (defined $R);
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
	
	push(@arr,'');
	push(@arr,@{$self->standard_preamble}) if (scalar(@{$self->standard_preamble})>0);
	push(@arr,'');
	push(@arr,@{$self->extra_preamble}) if (scalar(@{$self->extra_preamble})>0); #generic per tool
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
		ui->print(category=> 'all',message => "\nRunning ".$self->filename."...\n");
		if($self->R_markdown && $self->rmarkdown_installed) {
			system($self->_R_executable."script -e \"rmarkdown::render(input='".$self->filename."',output_format='pdf_document',output_file='PsN_".$self->toolname()."_plots.pdf')\" > PsN_".$self->toolname()."_plots.Rout 2>&1");
		} else {
			system($self->_R_executable." CMD BATCH ".$self->filename);
		}
		unlink('.RData');
	}
	chdir($basedir);
}

sub print_R_script
{
	my $self = shift;
		
	my @printcode_first=();
	my @printcode_second=();	
	my @printcode=();
	if($self->R_markdown) {
		if($self->rmarkdown_installed) {
			# R markdown code has to be separated in two parts
			my $value = 0;
			my $first_line = 1; #TRUE
			foreach my $line (@{$self->plotcode}){
				if($value == 1 && $line =~ /^--- *$/) {
					$value = ++$value;
				}
				if ($first_line && $line =~ /^--- *$/){
					$value = ++$value;
					$first_line = 0; #FALSE
				}
				if($value == 0) {
					push(@printcode_second,$line);
				}
				if($value == 1 || $value == 2) {
					push(@printcode_first,$line);
				}
				if ($value == 2) {
					$value = 0;
				}			
			}
		} else {
			# get only R script parts and save it in an array
			my $code_chank=0;
			my $start_pdf_line= "pdf(file=pdf.filename,width=10,height=7)";
			push(@printcode,$start_pdf_line);
			foreach my $line (@{$self->plotcode}) {
				if ($line =~ /^ *``` *$/) {
					$code_chank=0;
				}
				if ($code_chank==1) {
					push(@printcode,$line);
				}
				if ($line =~ /^ *``` *{r( )+/) {
					$code_chank=1;
				}
			}
			my $end_pdf_line= "dev.off()";
			push(@printcode,$end_pdf_line);
		}
	} else {
		#filter preamble from plotcode, if any
		foreach my $line (@{$self->plotcode}){
			if ($line =~ /$preambleline/){
				reset
				print "\nfound preline\n";
				@printcode=();
			}else{
				push(@printcode,$line);
			}
		}
	}


	open ( SCRIPT, ">" . $self->filename ); #local filename
	no warnings qw(uninitialized);
	if($self->R_markdown && $self->rmarkdown_installed) {
		if(@printcode_first ne '') {
			print SCRIPT join("\n",@printcode_first)."\n";
			print SCRIPT "\n";
		}
		print SCRIPT join("\n",'```{r input_from_PsN,include=FALSE}')."\n"; # begin R markdown code chunk
		print SCRIPT "\n";
	}
	print SCRIPT join("\n",@{$self->get_preamble})."\n";
	print SCRIPT "\n";
	if($self->R_markdown && $self->rmarkdown_installed) {
		print SCRIPT join("\n",'```')."\n"; # end R markdown code chunk
		print SCRIPT "\n";
		print SCRIPT join("\n",@printcode_second)."\n";
		print SCRIPT "\n";
	} else {
		print SCRIPT join("\n",@printcode)."\n";
		print SCRIPT "\n";
	}
	close SCRIPT;
}

sub add_preamble
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  code => { isa => 'ArrayRef', optional => 0 }
		);
	my $code = $parm{'code'};

	push(@{$self->extra_preamble},@{$code});

}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
