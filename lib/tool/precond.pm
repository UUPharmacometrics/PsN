package tool::precond;

use include_modules;
use Math::Random;
use File::Spec;
use Moose;
use MooseX::Params::Validate;

use array qw(:all);
use tool::modelfit;
use linear_algebra;

extends 'tool';

has 'precond_matrix' => (is => 'rw', isa => 'ArrayRef[ArrayRef]');
has 'precond_model' => (is => 'rw', isa => 'model');
has 'nodec' => (is => 'rw', isa => 'Bool', default => 0);
has 'cholesky' => (is => 'rw', isa => 'Bool', default => 0);

sub BUILD
{
	my $self = shift;


}

sub modelfit_setup
{
	my $self = shift;

	my $model_filename = $self->precond_model->filename;
	$model_filename =~ s/(\.ctl|\.mod)$//;
	$model_filename .= '_repara.mod';
	$model_filename = File::Spec->catfile(($self->directory, "m1"), $model_filename);

	my $model = create_reparametrized_model(
		filename => $model_filename,
		model => $self->precond_model,
		precond_matrix => $self->precond_matrix,
	);

	my $modelfit = tool::modelfit->new(
		%{common_options::restore_options(@common_options::tool_options)},
		models => [ $model ], 
		base_dir => $self->directory,
		directory => undef,
		top_tool => 0,
	);

	$self->tools([]) unless defined $self->tools;
	push(@{$self->tools}, $modelfit);
}

sub modelfit_analyze
{
	my $self = shift;

	my $cov_filename = $self->tools->[0]->directory . 'NM_run1/psn.cov';

	convert_reparametrized_cov(
		cov_filename => $cov_filename,
		model => $self->precond_model,
		precond_matrix => $self->precond_matrix,
		output_filename => "result.cov",
	);
}

sub create_reparametrized_model
{	
	my %parm = validated_hash(\@_,
		filename => { isa => 'Str', optional => 0 },
		model => { isa => 'model' },
	  precond_matrix => { isa => 'ArrayRef[ArrayRef]' },
		nodec => { isa => 'Bool', default => 0 },
		cholesky => { isa => 'Bool', default => 0 },
	);
	my $filename = $parm{'filename'};
	my $model = $parm{'model'};
	my @precond_matrix = @{$parm{'precond_matrix'}};
	my $nodec = $parm{'nodec'};
	my $cholesky = $parm{'cholesky'};

	my @precMatrix = @{$parm{'precond_matrix'}};		# FIXME 

	my $model = $model->copy(
		output_same_directory => 1,
		filename => $filename,
		copy_datafile => 0,
		write_copy => 0,
		copy_output => 0
	);

	my @code;
	@code = @{$model->pk(problem_number => 1)};
	my $use_pred = 0;
	if (not scalar(@code) > 0) {
		@code = @{$model->pred(problem_number => 1)};
		$use_pred = 1;
	}
	if (not scalar(@code) > 0) {
		croak("Neither PK nor PRED defined in " . $model->filename . "\n");
	}

	for (my $i = 0; $i < scalar(@code); $i++) {
		for (my $j = 0; $j < $model->nthetas + 1; $j++) {
			my $find = "THETA($j)";
			my $replace = "THE_$j";
			$find = quotemeta $find;
			@code[$i] =~ s/$find/$replace/g;
		}
	}

	my $tempString;
	my $tempTempSt;

	for (my $i = 0; $i < $model->nthetas; $i++) {
		$tempString = 'THE_' . ($i + 1) . '=';
		my @temp=@{$precMatrix[$i]};
		for (my $j = 0; $j < scalar(@temp); $j++) {
			if ($j == 0 or $precMatrix[$i][$j] < 0) {
				if (length($tempString) + length($precMatrix[$i][$j] . '*THETA(' . ($j + 1) . ')') > 70) {
					$tempString = $tempString . "\n";
					$tempTempSt = $tempTempSt . $tempString;
					$tempString = 'THE_'.($i + 1) . '=THE_' . ($i + 1) . $precMatrix[$i][$j] . '*THETA(' . ($j + 1) . ')';
				} else {
					$tempString = $tempString . $precMatrix[$i][$j] . '*THETA(' . ($j + 1) . ')';
				}
			} elsif ($precMatrix[$i][$j] > 0) {
				if (length($tempString) + length($precMatrix[$i][$j] . '*THETA(' . ($j + 1) . ')') > 70) {
					$tempString = $tempString . "\n";
					$tempTempSt = $tempTempSt . $tempString;
					$tempString = 'THE_' . ($i + 1) . '=THE_' . ($i + 1) . '+' . $precMatrix[$i][$j] . '*THETA(' . ($j + 1) . ')';
				} else {
					$tempString = $tempString . '+' . $precMatrix[$i][$j] . '*THETA(' . ($j + 1) . ')';
				}
			}
		}
		$tempString = $tempString . "\n";
		$tempTempSt = $tempTempSt . $tempString;
	}

	unshift @code, $tempTempSt;

	if ($use_pred) {
		$model->pred(problem_number => 1, new_pred => \@code);
	} else {
		$model->pk(problem_number => 1, new_pk => \@code);
	}

	my @parameter_initial = @{$model->initial_values(parameter_type => 'theta')->[0]};

	if (not $nodec) {
		if (not $cholesky) {
			my $dummy = linear_algebra::LU_factorization(\@precMatrix);
		} else {
			my $error = linear_algebra::cholesky(\@precMatrix);
			if ($error) {
				die "Unable to perform cholesky decomposition";
			}
		}
	}

	for (my $i = 1; $i < scalar(@precMatrix); $i++) {
		for (my $j = 0; $j < $i; $j++) {
			$parameter_initial[$i] = $parameter_initial[$i] - $parameter_initial[$j] * $precMatrix[$i][$j];
		}
	}

	for (my $i = $#precMatrix; $i >= 0; $i--) {
		for (my $j = $#precMatrix; $j > $i; $j--) {
			$parameter_initial[$i] = $parameter_initial[$i] - $parameter_initial[$j] * $precMatrix[$i][$j];
		}
		$parameter_initial[$i] = $parameter_initial[$i] / $precMatrix[$i][$i];
	}

	$model->initial_values(parameter_type => 'theta', new_values => [[@parameter_initial]]);

	$model->_write;

	return $model;
}

sub convert_reparametrized_cov
{
	my %parm = validated_hash(\@_,
		cov_filename => { isa => 'Str' },
		model => { isa => 'model' },
		precond_matrix => { isa => 'ArrayRef[ArrayRef]' },
		output_filename => { isa => 'Str' },
	);
	my $cov_filename = $parm{'cov_filename'};
	my $model = $parm{'model'};
	my @precond_matrix = @{$parm{'precond_matrix'}};
	my $output_filename = $parm{'output_filename'};

	my @precMatrix = @{$parm{'precond_matrix'}};		# FIXME

	my @cov_lines;
	open(my $fh, '<', $cov_filename) or croak("Cannot find the .cov file '$cov_filename' [$!]\n");
	while (my $tline = <$fh>) {
		chomp $tline;
		push @cov_lines, $tline;
	}

	my @reparaCov;
	my $rowCount = 0;
	my $found_table;
	my @header_labels;
	my $header_ok;
	my $given_header_warning;

	for (my $k = 0; $k < scalar(@cov_lines); $k++) {
		my $line = $cov_lines[$k]; 
		if ($line =~ /^\s*TABLE NO.\s+(\d+):/) {
			croak("two tables found where 1 expected") if $found_table;
			$found_table = 1;
		} elsif ($line =~ /^\s*NAME/ ) {
			$line =~ s/^\s*//; #get rid of leading spaces
			@header_labels = split /\s+/, $line;
			$header_ok = 1 if ($header_labels[0] eq 'NAME');
		} else {
			unless ((scalar(@header_labels > 2)) or $given_header_warning or $header_ok) {
				my $mes = "\n\n\***Warning***\n".
				"Too few elements in parameter label array in additional output file. ".
				"Is label row missing, or is the ".
				"delimiter something other than spaces (default)? ".
				"Parsing is likely to fail".
				"\n*************\n";
				print $mes;
				$given_header_warning = 1;
			}
			$line =~ s/^\s*//; #get rid of leading spaces
			my @line_values = split /\s+/,$line;
			my $max_column;

			my @new_line;
			$max_column = scalar(@header_labels) ; #store full matrix
			for (my $j = 0; $j < $max_column; $j++) {
				my $i = $j + 1; #must permute omega-sigma
				if ($line_values[$i] eq 'NaN') {
					push(@new_line, undef);
					$reparaCov[$rowCount][$j]=undef;
				} else {
					push(@new_line, eval($line_values[$i]));
					$reparaCov[$rowCount][$j]=eval($line_values[$i]);
				}

			}
			$rowCount++;
		}
	}


	my @temp_varcovMatrix;

	for (my $i = 0; $i < $model->nthetas; $i++) {
		for (my $j = 0; $j < $model->nthetas; $j++) {
			$temp_varcovMatrix[$i][$j] = 0;
			for (my $k = 0; $k < $model->nthetas; $k++) {
				$temp_varcovMatrix[$i][$j] = $temp_varcovMatrix[$i][$j] + $precMatrix[$i][$k] * $reparaCov[$k][$j];
			}
		}
	}

	my @varcovMatrix;

	for (my $i = 0; $i < $model->nthetas; $i++) {
		for (my $j = 0; $j < $model->nthetas; $j++) {
			$varcovMatrix[$i][$j] = 0;
			for (my $k = 0; $k < $model->nthetas; $k++) {
				$varcovMatrix[$i][$j] = $varcovMatrix[$i][$j] + $temp_varcovMatrix[$i][$k] * $precMatrix[$j][$k];
			}
		}
	}

	my $line;
	for (my $i = 0; $i < $model->nthetas; $i++) {
		$line = $cov_lines[$i + 2]; 
		$line =~ s/^\s*//; #get rid of leading spaces
		my @line_values = split /\s+/, $line;
		for (my $j = 0; $j < $model->nthetas; $j++) {
			@line_values[$j + 1] = $varcovMatrix[$i][$j];
		}
		$cov_lines[$i + 2] = join "   ", @line_values;
		$cov_lines[$i + 2] = " ".$cov_lines[$i + 2];
	}

	open (my $MYFILE, '>', $output_filename);
	for (my $i = 0; $i < scalar(@cov_lines); $i++) {
		print $MYFILE $cov_lines[$i] . "\n";
	}

	close $MYFILE; 
}

no Moose;
__PACKAGE__->meta->make_immutable;
