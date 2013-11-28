#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use lib ".."; #location of includes.pm
use includes; #file with paths to PsN packages

use model;

sub is_array{
    my $func=shift;
    my $facit=shift;
    my $label=shift;

    is (scalar(@{$func}),scalar(@{$facit}),"$label, equal length");

    my $min = scalar(@{$func});
    $min = scalar(@{$facit}) if (scalar(@{$facit})< $min);
    for (my $i=0; $i<$min; $i++){
    	is ($func->[$i],$facit->[$i],"$label, index $i");
    }		
	
}

my $modeldir = "../test_files";

my $model = model->new(filename => "$modeldir/pheno.mod");

is (scalar(@{$model->problems}), 1, "Check number of problems");
my $problem = $model->problems->[0];
is (scalar(@{$problem->inputs}), 1, "Check number of inputs");
my $options = $model->problems->[0]->inputs->[0]->options;
is (scalar(@{$options}), 6, "Check number of options for inputs");

# Names of input options
my @input_option_names = qw(ID TIME AMT WGT APGR DV);

for (my $i = 0; $i < @input_option_names; $i++) {
	is ($options->[$i]->name, $input_option_names[$i], "Check \$INPUT options name $i");
	is ($options->[$i]->value, '', "Check \$INPUT options value $i");
}

# ouput_files method
my @output_files = qw(pheno.lst pheno.ext pheno.cov pheno.cor pheno.coi pheno.phi pheno.phm pheno.shk pheno.grd pheno.xml pheno.smt pheno.rmt patab1 phenomsf);

my $files = $model->output_files;

for (my $i = 0; $i < @output_files; $i++) {
	is ($$files[$i], $output_files[$i], "output_files method $i");
}

# get_coordslabels method
my $coordslabels = $model->get_coordslabels(parameter_type => 'theta');
is ((keys %{$$coordslabels[0]})[0], 'THETA1', 'get_coordslabels theta 1');
is ((keys %{$$coordslabels[0]})[1], 'THETA2', 'get_coordslabels theta 2');
is ((values %{$$coordslabels[0]})[0], 'CL', 'get_coordslabels theta 3');
is ((values %{$$coordslabels[0]})[1], 'V', 'get_coordslabels theta 4');

$coordslabels = $model->get_coordslabels(parameter_type => 'omega');
is ((keys %{$$coordslabels[0]})[0], 'OMEGA(2,2)', 'get_coordslabels omega 1');
is ((keys %{$$coordslabels[0]})[1], 'OMEGA(1,1)', 'get_coordslabels omega 2');
is ((values %{$$coordslabels[0]})[0], 'IVV', 'get_coordslabels omega 3');
is ((values %{$$coordslabels[0]})[1], 'IVCL', 'get_coordslabels omega 4');

# idcolumns method
my $columns = $model->idcolumns(problem_numbers => [0]);

is ($$columns[0], 1, "idcolumns method");

# is_option_set method
ok ($model->is_option_set(record => 'input', name => 'APGR'), "is_option_set \$INPUT");
ok ($model->is_option_set(record => 'estimation', name => 'MAXEVALS'), "is_option_set \$INPUT");
ok (!$model->is_option_set(record => 'input', name => 'OPEL'), "is_option_set \$INPUT");

#setup_filter method
my @header = ('method','model','problem','significant_digits','minimization_successful','covariance_step_successful','ofv');
my @filter = ('minimization_successful.eq.1','significant_digits.gt.4','problem.lt.2','covariance_step_successful.ne.0');
my ($indices,$relations,$value) = $model->setup_filter(filter => \@filter, header => \@header);
is_array($indices,[4,3,2,5], "setup_filter method, finding columns");
is_array($relations,['==','>','<','!='], "setup_filter method, finding relations");
is_array($value,[1,4,2,0], "setup_filter method, finding values");

@filter = ('method.eq.bootstrap');
($indices,$relations,$value) = $model->setup_filter(filter => \@filter, header => \@header, string_filter => 1);
is ($indices->[0],0,"setup_filter method, method index");
is ($relations->[0],'eq',"setup_filter method, method relation");
is ($value->[0],'bootstrap',"setup_filter method, method value");


#get_rawres_params method
#here $model must still refer to pheno.mod, otherwise test will fail

my $arr = $model -> get_rawres_params(filename => $modeldir.'/rawres_for_get_rawres_params.csv',
				      string_filter => ['method.eq.bootstrap'],
				      filter => ['significant_digits.gt.4'],
				      require_numeric_ofv => 1,
				      offset => 1);
is (scalar(@{$arr}),3,'method get_rawres_params, number of lines returned ');
is($arr->[0]->{'theta'}->{'CL'},1.1,'method get_rawres_params, theta 0');
is($arr->[0]->{'theta'}->{'V'},1.2,'method get_rawres_params, theta 0');
is($arr->[1]->{'theta'}->{'CL'},2.1,'method get_rawres_params, theta 1');
is($arr->[1]->{'theta'}->{'V'},2.2,'method get_rawres_params, theta 1');
is($arr->[2]->{'theta'}->{'CL'},3.1,'method get_rawres_params, theta 2');
is($arr->[2]->{'theta'}->{'V'},3.2,'method get_rawres_params, theta 2');
is($arr->[0]->{'omega'}->{'IVCL'},1.3,'method get_rawres_params, omega 0');
is($arr->[0]->{'omega'}->{'IVV'},1.4,'method get_rawres_params, omega 0');
is($arr->[1]->{'omega'}->{'IVCL'},2.3,'method get_rawres_params, omega 1');
is($arr->[1]->{'omega'}->{'IVV'},2.4,'method get_rawres_params, omega 1');
is($arr->[2]->{'omega'}->{'IVCL'},3.3,'method get_rawres_params, omega 2');
is($arr->[2]->{'omega'}->{'IVV'},3.4,'method get_rawres_params, omega 2');
is($arr->[0]->{'sigma'}->{"SIGMA(1,1)"},1.5,'method get_rawres_params, sigma 0');
is($arr->[1]->{'sigma'}->{"SIGMA(1,1)"},2.5,'method get_rawres_params, sigma 1');
is($arr->[2]->{'sigma'}->{"SIGMA(1,1)"},3.5,'method get_rawres_params, sigma 2');
is($arr->[0]->{'ofv'},752.0400284856,'method get_rawres_params, ofv 0');
is($arr->[1]->{'ofv'},761.1429095868,'method get_rawres_params, ofv 1');
is($arr->[2]->{'ofv'},674.1097668967,'method get_rawres_params, ofv 2');
is($arr->[0]->{'model'},1,'method get_rawres_params, model 0');
is($arr->[1]->{'model'},2,'method get_rawres_params, model 1');
is($arr->[2]->{'model'},5,'method get_rawres_params, model 2');

# set_maxeval_zero

use PsN;			# Need to set PsN version as this is a global variable
$PsN::nm_major_version = 7;
my $ver = $PsN::nm_major_version;

is ($model->get_option_value(record_name => 'estimation', option_name => 'MAXEVALS'), 9997, "before set_maxeval_zero");
$model->set_maxeval_zero;
is ($model->get_option_value(record_name => 'estimation', option_name => 'MAXEVALS'), 0, "before set_maxeval_zero");

$model->add_records( type  => 'estimation', record_strings => ['METHOD=IMP'] );
is ($model->get_option_value(record_name => 'estimation', option_name => 'METHOD', record_index => 1), 'IMP', "added estimation record");

$model->set_maxeval_zero;

ok ($model->is_option_set(record => 'estimation', name => 'POSTHOC'), "set_maxeval_zero: transferred option");
is ($model->problems->[0]->estimations->[1], undef, "set_maxeval_zero: Removed estimation");

# update_inits
$model = model->new(filename => "$modeldir/pheno.mod");

$model->update_inits(from_output_file => "$modeldir/pheno_test.lst");

my $lines = $model->record(record_name => 'sigma');
ok ($lines->[0]->[0] =~ 0.0164, "update_inits: new sigma");

is ($model->problems->[0]->thetas->[0]->options->[0]->lobnd, 0, "update_inits: new theta lobnd");
is ($model->problems->[0]->thetas->[0]->options->[0]->init, 0.00555, "update_inits: new theta init");
is ($model->problems->[0]->thetas->[1]->options->[0]->lobnd, 0, "update_inits: new theta lobnd");
is ($model->problems->[0]->thetas->[1]->options->[0]->init, 1.34, "update_inits: new theta init");

done_testing();
