package rplots;

use Config;
use include_modules;
use Cwd;
use File::Copy 'copy';
use File::Spec;
use File::Basename;
use Mouse;
use MouseX::Params::Validate;
use PsN;
use model;
use OSspecific;
use utils::file;


has 'directory' => (is => 'rw', isa => 'Str', required => 1 );
has 'level' => ( is => 'rw', isa => 'Int', required => 1 );
has 'toolname' => (is => 'rw', isa => 'Str', required => 1);
has 'raw_results_file' => (is => 'rw', isa => 'Str');
has 'tool_results_file' => (is => 'rw', isa => 'Str');
has 'filename' => (is => 'rw', isa => 'Str');
has '_R_executable' => (is => 'rw', isa => 'Str' );
has 'indent' => (is => 'rw', isa => 'Str', default => "    " ); #4spaces
has 'standard_preamble' => ( is => 'rw', isa => 'ArrayRef[Str]',default => sub{ [] } );
has 'extra_preamble' => ( is => 'rw', isa => 'ArrayRef[Str]',default => sub{ [] } );
has 'plotcode' => ( is => 'rw', isa => 'ArrayRef[Str]', required => 1);
has 'subset_variable' => (is => 'rw', isa => 'Maybe[Str]' );
has 'R_markdown' => (is => 'rw', isa => 'Bool', default => 0);
has 'rmarkdown_installed' => (is => 'rw', isa => 'Bool', default => 0);
has 'model' => (is => 'rw', isa => 'model');
has 'model_subdir' => (is => 'rw', isa => 'Bool', default => 0 );
has 'R_lib_path' => (is => 'rw', isa => 'Str' );
has 'file_type' => (is => 'rw', isa => 'Str' );
has 'debug_rmd' => (is => 'rw', isa => 'Bool', default => 0 );
has 'html' => (is => 'rw', isa => 'Bool', default => 0 );
has 'pdf' => ( is => 'rw', isa => 'Bool', default => 1 );

our $preambleline = '#WHEN THIS FILE IS USED AS A TEMPLATE THIS LINE MUST LOOK EXACTLY LIKE THIS';

sub BUILD
{
    my $self = shift;
    $self->set_R_executable();
    $self->set_R_library_path();
    if ($Config{osname} eq 'MSWin32') {
        my $path = PsN::get_R_lib_path();
        $ENV{'RSTUDIO_PANDOC'} = $path . '\..\pandoc';
    }
    $self->setup();
}

sub setup
{
    my $self = shift;

    if (not defined $self->filename) {
        if ($self->toolname eq "qa" and $self->file_type ne 'R') { # tools that need rmarkdown file to get a report
            $self->R_markdown(1);
            $self->rmarkdown_installed(1);
        }
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
    my ($modeldir, $modelfile) = OSspecific::absolute_path($self->model-> directory,
                                                 $self->model-> filename );

    #figure out table suffix
    my @xpose_names=("sdtab","mutab","patab","catab","cotab","mytab","xptab","cwtab");
    my @tables = @{$self->model->table_names}; #array of arrays without path
    my $runno;
    my $tabSuffix='';
    for (my $i=0; $i<scalar(@tables); $i++){
        if (defined $tables[$i]){
            foreach my $name (@{$tables[$i]}){
                my $tmp = model::get_xpose_runno_and_suffix(filename => $name);
                if (defined $tmp->[0]){
                    #have runno
                    $runno = $tmp->[0];
                    $runno =~ s/sim$//;

                    $tabSuffix = $tmp->[1];
                    last;
                }
            }
        }
        last if (defined $runno);
    }

    if (not (defined $runno and ($modelfile =~ /$runno/))) {
        my $tmp = $modelfile;
        if ($tmp =~ s/^([^0-9]+)//){
            $runno = $tmp unless (defined $runno);
        }
    }

    $runno = '' unless (defined $runno and length($runno)>0);

    my $subsetstring = 'NULL';
    $subsetstring = "'".$self->subset_variable."'" if (defined $self->subset_variable and length($self->subset_variable)>0);

    my $workingdirectory = $self->directory;
    my $results_dir;
    if ($self->model_subdir) {
        $results_dir = dirname($workingdirectory) . File::Spec->catfile('', '');    # Hack to add path separator
    } else {
        $results_dir = $modeldir;
    }
    $results_dir = File::Spec->abs2rel($results_dir, $workingdirectory);

    my $rscripts_path = $PsN::Rscripts_dir;
    if (not defined $rscripts_path) {
        croak "The PsN R-scripts directory could not be found. Check your PsN installation\n";
    }
    #Replace single backslash with double, assume windows, but do not change if already double
    $workingdirectory = double_backslashes(string => $workingdirectory);
    $rscripts_path = double_backslashes(string =>$rscripts_path);
    $results_dir = double_backslashes(string => $results_dir);

    my @arr =(
         "xpose.runno <- '".$runno."'",
         "toolname <- '".$self->toolname()."'",
         "pdf.filename <- paste0('PsN_',toolname,'_plots.pdf')",
         "working.directory<-'".$workingdirectory."'",
         "results.directory <- '" . $results_dir . "'",
         "subset.variable<-".$subsetstring,
         "tab.suffix <- '".$tabSuffix."' ",
         "rscripts.directory <- '" . $rscripts_path . "' # This is not used",
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
    unless ($self->model->is_dummy){
        foreach my $param ('theta','omega','sigma'){
            my $labels = $self->model->labels(parameter_type => $param,
                                        problem_numbers => [1],
                                        generic => 0);
            my $fixed = $self->model->fixed(parameter_type => $param,
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
        my $nomegas = $self->model->nomegas(problem_numbers=>[1], with_same => 1, with_correlations => 0);
        my $nsigmas = $self->model->nsigmas(problem_numbers=>[1], with_same => 1, with_correlations => 0);
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

sub double_backslashes
{
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

sub check_R_executable
{
    my $self = shift;
    my $executable = shift;

    my $null = '/dev/null';
    if ($Config{osname} eq 'MSWin32') {
        $null = 'NUL';
    }

    my $rc = system("$executable --version >$null 2>&1");
    $rc = $rc >> 8;
    if ($rc == 0) {
        return 1;
    } else {
        return 0;
    }
}

sub set_R_library_path
{
    my $self = shift;
    #check in PsN config
    my $R_lib_path = PsN::get_R_lib_path();
    $self->R_lib_path(double_backslashes(string => $R_lib_path));
}

sub get_preamble
{
    my $self=shift;

    my $pdfname = $self->filename;
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

sub make_plots
{
    my $self = shift;

    my $basedir = getcwd();
    chdir($self->directory);
    #always print R script to disk
    $self->print_R_script();
    #sometimes run script - need high enough level and defined R executable
    if ($self->level > 0 and defined ($self->_R_executable)){
        ui->print(category=> 'all',message => "\nRunning ".$self->filename."...\n");
        my $executable;
        if ($self->R_markdown && $self->rmarkdown_installed) {
            if (substr($self->_R_executable, -1) eq '"') {      # Special case when executable contains spaces and therefore is quoted
                $executable = $self->_R_executable;
                chop $executable;
                $executable .= 'script"';
            } else {
                $executable = $self->_R_executable . "script";
            }
        } else {
            $executable = $self->_R_executable;
        }
        my $available = $self->check_R_executable($executable);
        if (not $available) {
            chdir($basedir);
            print "Unable to find R executable: " . $executable . "  not in path?";
            return;
        }

        my $debug_option = "";
        if ($self->debug_rmd) {
            $debug_option = ", clean=FALSE";
            if ($self->pdf) {
                $debug_option .= ", output_options=list(keep_tex=TRUE)";
            }
        }

        my @output_formats;
        if ($self->html) {
            push @output_formats, 'html_document';
        }
        if ($self->pdf) {
            push @output_formats, 'pdf_document';
        }
       
        if($self->R_markdown && $self->rmarkdown_installed) {

            for my $format (@output_formats) {
                system($executable . " -e \"rmarkdown::render(input='" . $self->filename . "'$debug_option, output_format='$format')\" > PsN_".$self->toolname()."_plots.Rout 2>&1");
            }
        } else {
            @output_formats = ('pdf_document');
            system($executable . " CMD BATCH " . $self->filename);
        }
        unlink('.RData');
        my $stem = utils::file::get_file_stem($self->filename);
        for my $format (@output_formats) {
            my $output_file;
            if ($format eq 'html_document') {
                $output_file = "$stem.html";
            } else {
                $output_file = "$stem.pdf";
            }
            if (-e $output_file) {
                my $output_path = File::Spec->rel2abs($output_file);
                print "Successfully generated $output_path\n";
            } else {
                if ($self->level > 0) {
                    my $rout_path = File::Spec->rel2abs("$stem.Rout");
                    print "Error: could not generate report/plots. See $rout_path for R error messages\n";
                }
            }
        }
    }
    chdir($basedir);
}

sub print_R_script
{
    my $self = shift;

    my ($modeldir, $modelfile) = OSspecific::absolute_path($self->model-> directory,
                                                        $self->model-> filename );
    my $datafile = $self->model -> model::datafiles->[0];

    my @printcode_first=();
    my @printcode_second=();
    my @printcode=();
    if($self->R_markdown) {
        if($self->rmarkdown_installed) {
            # R markdown code has to be separated in two parts
            my $value = 0;
            my $first_line = 1; #TRUE
            #my $add_subtitle = 1;
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

sub create_r_vector
{
    # Creates the rcode for a vector given an array
    my %parm = validated_hash(\@_,
        array => { isa => 'ArrayRef', optional => 0 },
        quoted => { isa => 'Bool', default => 1 },
    );
    my $array = $parm{'array'};
    my $quoted = $parm{'quoted'};

    my $quote = "";
    if ($quoted) {
        $quote = "'";
    }

    my $str = "c(";
    if (@$array) {
        $str .= $quote . join("$quote, $quote", @{$array}) . $quote;
    }
    $str .= ')';

    return $str;
}

1;
