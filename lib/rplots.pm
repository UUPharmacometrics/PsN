package rplots;

use Config;
use include_modules;
use Cwd;
use File::Copy 'cp';
use Moose;
use MooseX::Params::Validate;
use PsN;

has 'directory' => (is => 'rw', isa => 'Str', required => 1 );
has 'level' => ( is => 'rw', isa => 'Int', required => 1 );
has 'filename' => (is => 'rw', isa => 'Str', default => 'PsNplots.R' );
has '_R_executable' => (is => 'rw', isa => 'Str' );
has 'indent' => (is => 'rw', isa => 'Str', default => "    " ); #4spaces
has 'extra_preamble' => ( is => 'rw', isa => 'ArrayRef[Str]',default => sub{ [] } );
has 'level_code' => ( is => 'rw', isa => 'HashRef', default => sub{ {} });
has 'libraries' => ( is => 'rw', isa => 'ArrayRef[Str]',default => sub{ [] } );

sub BUILD
{
	my $self = shift;
	$self->set_R_executable();
}

sub set_R_executable
{
	my $self = shift;
	#check in PsN config, or try R --version
	if ( defined $PsN::config -> {'_'} -> {'R'} ) {
		$self->_R_executable($PsN::config -> {'_'} -> {'R'});
	}else{
		my $rc = system('R --version');
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
		 "#Created $theDate at $theTime",
		 '#Change the variable level to make the script create more/less output');
	foreach my $lib (@{$self->libraries}){
		push(@arr,'library('.$lib.')');
	}
	push(@arr,
		 "setwd('".$self->directory."')",
		 'level <- '.$self->level(),
		 'if (level > 0){',
		 $self->indent().'pdf(file="'.$pdfname.'")',
		 '}'."\n"
		);
	
	push(@arr,@{$self->extra_preamble});

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

	open ( SCRIPT, ">" . $self->filename ); #local filename
	print SCRIPT join("\n",@{$self->get_preamble})."\n";
	foreach my $level (sort {$a <=> $b} keys(%{$self->level_code})){
		print SCRIPT "\n";
		print SCRIPT 'if (level > '.($level-1).'){'."\n";
		print SCRIPT $self->indent().join("\n".$self->indent(),@{$self->level_code->{$level}})."\n";
		print SCRIPT "}\n";
	}

	print SCRIPT "\n";
	my @final =('if (level > 0){',
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

sub add_plot
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  level => { isa => 'Int', optional => 0 },
							  code => { isa => 'ArrayRef[Str]', optional => 0 }
		);
	my $level = $parm{'level'};
	my $code  = $parm{'code'};

	if ($level <1){
		croak("Illegal level $level to add_plot");
	}

	unless (defined $self->level_code->{$level}){
		$self->level_code->{$level} = [];
	}

	push(@{$self->level_code->{$level}},@{$code});

}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
