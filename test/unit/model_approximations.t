#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages

use model_approximations;

my $modeldir = $includes::testfiledir;

# linearize_error_terms
my $expected_1_1 = [ "ERR1_0 = D_EPS_1", "ERR1_1 = D_EPSETA1_1 * (ETA(1) - OETA1)"];
my $expected_1_2 = [ "ERR1_0 = D_EPS_1", "ERR1_1 = D_EPSETA1_1 * (ETA(1) - OETA1)", "ERR1_2 = D_EPSETA1_2 * (ETA(2) - OETA2)" ];
my $expected_2_1 = [ "ERR1_0 = D_EPS_1", "ERR1_1 = D_EPSETA1_1 * (ETA(1) - OETA1)", "ERR2_0 = D_EPS_2", "ERR2_1 = D_EPSETA2_1 * (ETA(1) - OETA1)" ];
my $expected_2_2 = [ "ERR1_0 = D_EPS_1", "ERR1_1 = D_EPSETA1_1 * (ETA(1) - OETA1)", "ERR1_2 = D_EPSETA1_2 * (ETA(2) - OETA2)", "ERR2_0 = D_EPS_2", "ERR2_1 = D_EPSETA2_1 * (ETA(1) - OETA1)", "ERR2_2 = D_EPSETA2_2 * (ETA(2) - OETA2)" ];

is_deeply (model_approximations::linearize_error_terms(neps => 1, neta => 1),  $expected_1_1, "linearized_error_terms 1");
is_deeply (model_approximations::linearize_error_terms(neps => 1, neta => 2),  $expected_1_2, "linerized_error_terms 2");
is_deeply (model_approximations::linearize_error_terms(neps => 2, neta => 1),  $expected_2_1, "linerized_error_terms 3");
is_deeply (model_approximations::linearize_error_terms(neps => 2, neta => 2),  $expected_2_2, "linerized_error_terms 4");

# linearize_covariate_error_terms
my $expected_cov_1 = [ "ERRC1_1 = D_EPSETA1_1 * OGK_CL * (GZ_CL - OGZ_CL)" ];
my $expected_cov_2 = [ "ERRC1_1 = D_EPSETA1_1 * OGK_CL * (GZ_CL - OGZ_CL)", "ERRC2_1 = D_EPSETA2_1 * OGK_CL * (GZ_CL - OGZ_CL)" ];
my $expected_cov_3 = [ "ERRC1_1 = D_EPSETA1_1 * OGK_CL * (GZ_CL - OGZ_CL)", "ERRC1_3 = D_EPSETA1_3 * OGK_KA * (GZ_KA - OGZ_KA)" ];
my %parameters1 = ( 1 => "CL" );
my %parameters2 = ( 1 => "CL", 3 => "KA" );
is_deeply (model_approximations::linearize_covariate_error_terms(neps => 1, eta_parameter => \%parameters1), $expected_cov_1, "linearize_covariate_error_terms 1"); 
is_deeply (model_approximations::linearize_covariate_error_terms(neps => 2, eta_parameter => \%parameters1), $expected_cov_2, "linearize_covariate_error_terms 2"); 
is_deeply (model_approximations::linearize_covariate_error_terms(neps => 1, eta_parameter => \%parameters2), $expected_cov_3, "linearize_covariate_error_terms 3"); 

# linearize_error_sums
is_deeply (model_approximations::linearize_error_sums(neps => 1, neta => 1, eta_parameter => {}), [ "ESUM1 = ERR1_0 + ERR1_1" ] , "linearize_error_sums");



my $model = model->new(filename => "$modeldir/pheno.mod", ignore_missing_data => 1);

my $derivatives_model = model_approximations::second_order_derivatives_model(model => $model);

is (scalar(@{$derivatives_model->problems->[0]->pks->[0]->code}), 15, "2nd order Derivatives model reset");
is (scalar(@{$derivatives_model->problems->[0]->errors->[0]->code}), 19, "2nd order Derivatives model error");

my $approximation_model = model_approximations::second_order_approximation_model(model => $model);



done_testing();
