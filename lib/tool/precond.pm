package tool::precond;

use include_modules;
use Math::Random;
use File::Spec;
use Moose;
use MooseX::Params::Validate;
use File::Copy qw(copy);

use array qw(:all);
use tool::modelfit;
use linear_algebra;
use output;

extends 'tool';

has 'precond_matrix' => (is => 'rw', isa => 'ArrayRef[ArrayRef]');
has 'precond_model' => (is => 'rw', isa => 'model');
has 'update_inits' => (is => 'rw', isa => 'Maybe[Str]');
has '_repara_model' => (is => 'rw', isa => 'model');

sub BUILD
{
	my $self = shift;

	my $records = $self->precond_model->problems->[0]->covariances;
	unless (defined $records and defined $records->[0]) {
		croak("No \$COVARIANCE defined in " . $self->precond_model->full_name . "\nPlease add it to the model and run again.");
	}
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

	$self->_repara_model($model);

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

    if ($self->_repara_model->is_run) {
        my $output = $self->_repara_model->outputs->[0];

        my $copy = $self->precond_model->copy(
            filename => 'updated_model.mod',
            copy_datafile => 0,
            write_copy => 0,
            copy_output => 0
        );

        my $theta = $output->get_single_value(attribute => 'thetas');
        my $new_theta = [];

        # Multiply precond_matrix with theta to obtain original theta
        for (my $row = 0; $row < scalar(@$theta); $row++) {
            my $sum = 0;
            for (my $col = 0; $col < scalar(@$theta); $col++) {
                $sum += $self->precond_matrix->[$row]->[$col] * $theta->[$col];
            }
            $new_theta->[$row] = $sum;
        }

        for my $theta (@{$copy->problems->[0]->thetas}) {
            for (my $i = 0; $i < scalar(@{$theta->options}); $i++) {
                $theta->options->[$i]->init($new_theta->[$i]);
            }
        }

        $copy->_write;
        if (defined $self->update_inits) {
            copy $copy->full_name, '../' . $self->update_inits;
        }
    } else {
        print "Unable to update_inits: model was not run";
    }
}

sub _reparametrize
{
	my $code = shift;
	my $nthetas = shift;

	for (my $i = 0; $i < scalar(@$code); $i++) {
		for (my $j = 0; $j < $nthetas + 1; $j++) {
			my $find = "THETA($j)";
			my $replace = "THE_$j";
			$find = quotemeta $find;
			$code->[$i] =~ s/$find/$replace/g;
		}
	}
}

sub create_reparametrized_model
{	
	my %parm = validated_hash(\@_,
		filename => { isa => 'Str', optional => 0 },
		model => { isa => 'model' },
	  precond_matrix => { isa => 'ArrayRef[ArrayRef]' },
	);
	my $filename = $parm{'filename'};
	my $model = $parm{'model'};
	my @precond_matrix = @{$parm{'precond_matrix'}};

	my $model = $model->copy(
		output_same_directory => 1,
		filename => $filename,
		copy_datafile => 0,
		write_copy => 0,
		copy_output => 0
	);

	my @code;
	my $code_record;
	if ($model->has_code(record => 'pk')) {
		@code = @{$model->get_code(record => 'pk')};
		$code_record = 'pk';
	} elsif ($model->has_code(record => 'pred')) {
		@code = @{$model->get_code(record => 'pred')};
		$code_record = 'pred';
	} else {
		croak("Neither PK nor PRED defined in " . $model->filename . "\n");
	}

	_reparametrize(\@code, $model->nthetas);

	my $tempString;
	my $tempTempSt;

	for (my $i = 0; $i < $model->nthetas; $i++) {
		$tempString = 'THE_' . ($i + 1) . '=';
		my @temp=@{$precond_matrix[$i]};
		for (my $j = 0; $j < scalar(@temp); $j++) {
			if ($j == 0 or $precond_matrix[$i][$j] < 0) {
				if (length($tempString) + length($precond_matrix[$i][$j] . '*THETA(' . ($j + 1) . ')') > 70) {
					$tempString = $tempString . "\n";
					$tempTempSt = $tempTempSt . $tempString;
					$tempString = 'THE_'.($i + 1) . '=THE_' . ($i + 1) . $precond_matrix[$i][$j] . '*THETA(' . ($j + 1) . ')';
				} else {
					$tempString = $tempString . $precond_matrix[$i][$j] . '*THETA(' . ($j + 1) . ')';
				}
			} elsif ($precond_matrix[$i][$j] > 0) {
				if (length($tempString) + length($precond_matrix[$i][$j] . '*THETA(' . ($j + 1) . ')') > 70) {
					$tempString = $tempString . "\n";
					$tempTempSt = $tempTempSt . $tempString;
					$tempString = 'THE_' . ($i + 1) . '=THE_' . ($i + 1) . '+' . $precond_matrix[$i][$j] . '*THETA(' . ($j + 1) . ')';
				} else {
					$tempString = $tempString . '+' . $precond_matrix[$i][$j] . '*THETA(' . ($j + 1) . ')';
				}
			}
		}
		$tempString = $tempString . "\n";
		$tempTempSt = $tempTempSt . $tempString;
	}

	unshift @code, $tempTempSt;

	$model->set_code(record => $code_record, code => \@code);

	# Reparametrize other blocks of abbreviated code
	for my $record (('error', 'des', 'aes', 'aesinitial', 'mix', 'infn')) {
		if ($model->has_code(record => $record)) {  
			my $code = $model->get_code(record => $record);
			_reparametrize($code, $model->nthetas);
			$model->set_code(record => $record, code => $code);
		}
	}

	# Set new initial values for thetas
	my @parameter_initial = @{$model->initial_values(parameter_type => 'theta')->[0]};

	my $dummy = linear_algebra::LU_factorization(\@precond_matrix);

	for (my $i = 1; $i < scalar(@precond_matrix); $i++) {
		for (my $j = 0; $j < $i; $j++) {
			$parameter_initial[$i] = $parameter_initial[$i] - $parameter_initial[$j] * $precond_matrix[$i][$j];
		}
	}

	for (my $i = $#precond_matrix; $i >= 0; $i--) {
		for (my $j = $#precond_matrix; $j > $i; $j--) {
			$parameter_initial[$i] = $parameter_initial[$i] - $parameter_initial[$j] * $precond_matrix[$i][$j];
		}
		$parameter_initial[$i] = $parameter_initial[$i] / $precond_matrix[$i][$i];
	}

	$model->initial_values(parameter_type => 'theta', new_values => [[@parameter_initial]]);


	# Remove limits for thetas
	foreach my $theta (@{$model->problems->[0]->thetas}) {
		foreach my $option (@{$theta->options}) {
			$option->clear_upbnd;
			$option->clear_lobnd
		}
	}

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
				$temp_varcovMatrix[$i][$j] = $temp_varcovMatrix[$i][$j] + $precond_matrix[$i][$k] * $reparaCov[$k][$j];
			}
		}
	}

	my @varcovMatrix;

	for (my $i = 0; $i < $model->nthetas; $i++) {
		for (my $j = 0; $j < $model->nthetas; $j++) {
			$varcovMatrix[$i][$j] = 0;
			for (my $k = 0; $k < $model->nthetas; $k++) {
				$varcovMatrix[$i][$j] = $varcovMatrix[$i][$j] + $temp_varcovMatrix[$i][$k] * $precond_matrix[$j][$k];
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
