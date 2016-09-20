package tool::nonparametric;

use strict;
use Moose;
use MooseX::Params::Validate;
use File::Copy 'cp';
use include_modules;
use data;
use log;
use filter_data;
use tool::modelfit;

extends 'tool';

has 'npsupp' => ( is => 'rw', isa => 'ArrayRef');

sub BUILD
{
    my $self = shift;

	
}

sub modelfit_setup
{
	my $self = shift;
	my $modelfit;
	my $input_model = $self->models->[0];

my $filestem = $input_model ->filename();
#this regex must be the same as used in modelfit.pm, for consistency
$filestem =~ s/\.[^.]+$//; #last dot and extension

$input_model -> set_records(type => 'nonparametric',
							record_strings => ['UNCONDITIONAL']);
														
my @models_array=();
foreach my $value (@{$self->npsupp}){
	push(@models_array,$input_model -> copy( filename => $filestem.'_'.$value.'.mod',
											   copy_datafile => 0,
											   copy_output => 0,
											   write_copy => 0,
											   output_same_directory => 1,
											   directory => $self->directory.'/m1'));
	
	$models_array[-1] -> add_option(record_name => 'nonparametric',
									option_name => 'NPSUPP',
									option_value => $value);
	$models_array[-1] -> _write;
}

#basedirect $main_directory
$modelfit = 
	tool::modelfit->new( eval( $common_options::parameters ),
	  prepend_model_file_name => 1,
	  directory => undef,
	  min_retries => 0,
	  retries => 0,
	  copy_data => 0,
	  top_tool => 0,
	  raw_results_file => [$self -> directory.$self->raw_results_file->[0]],
	  raw_nonp_file => [$self -> directory.$self->raw_nonp_file->[0]],
	  models => \@models_array );
    
    
	$self->tools([]) unless (defined $self->tools);
	push(@{$self->tools}, $modelfit);
}

sub modelfit_analyze
{
    # Collect all tables into one results table

    my $self = shift;
    my $added_column = add_column(filename => $self->raw_nonp_file->[0], npsupp => $self->npsupp);
	overwrite_csv(filename => $self->raw_nonp_file->[0], rows => $added_column);
	

}

sub add_column
{
	my %parm = validated_hash(\@_,
							  filename => { isa => 'Str', optional => 0 },
							  npsupp => {isa => 'ArrayRef', optional => 0},
		);
	my $filename = $parm{'filename'};
	my $npsupp = $parm{'npsupp'};
	
	# Read in a csv file
	open( CSV, '<'."$filename" ) || die "Could not open $filename for reading";
	my @lines;
	my @exp;
	my $amount;
	while (my $line = <CSV> ) {
		chomp($line);
		my @tmp = ();
		if ($line =~ /^\"/) { 
			@tmp = split(/","/,$line);  # split columns where row consist of headers
			$tmp[0] =~ s/['\"']//g; 
			$tmp[scalar(@tmp)-1] =~ s/['\"']//g; 
		} else {
			@tmp = split(/,/,$line); # split columns where row consist of numbers
		}
	push(@lines,\@tmp);
		push(@exp,$line);
	}
	close( CSV );

	# search position of the column "npofv"
	my $element = 'npofv';
	my $position;
	for (my $i=0; $i < scalar(@{$lines[0]}); $i++) {
		if ($lines[0][$i] eq $element) {
			$position = $i;
		}
	}
	
	# add column
	my @rows = ();
	unless ( defined $npsupp && (scalar(@{$npsupp}) > 0) ) {
		die "Error: Npsupp values are not defined or there are no npsupp values.";
	} else {
		splice @{$lines[0]}, $position+1 , 0, 'npsupp';
		$rows[0] = '"'.join('","',@{$lines[0]}).'"'."\n";
		for (my $n=0; $n < scalar(@{$npsupp}); $n++) {
			splice @{$lines[$n+1]}, $position+1 , 0, $npsupp->[$n];
			$rows[$n+1] = join(',',@{$lines[$n+1]})."\n";
		}
	}
	
	return(\@rows);
}

sub overwrite_csv
{
	my %parm = validated_hash(\@_,
							  filename => { isa => 'Str', optional => 0 },
							  rows => {isa => 'ArrayRef', optional => 0},
		);
	my $filename = $parm{'filename'};
	my $rows = $parm{'rows'};

	#write a csv file
	open (my $fh, '>' ."$filename");
	for (my $n=0; $n < (scalar(@{$rows})); $n++) {
		print $fh $rows->[$n];	# write each row in the csv file. 
	}
	close $fh;	
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
