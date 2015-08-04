package answers;

sub read_answers
{

	my @answer_hashes = ();
	my $index=0;
	$answer_hashes[$index]={};


    $answer_hashes[$index]->{file} = 'special_mod/special_inverse.lst';
    $answer_hashes[$index]->{parsed_successfully} = 1;
    $answer_hashes[$index]->{answers} = {};
	$answer_hashes[$index]->{near_bounds_names}=['CRCL_on_CL']; 
	$answer_hashes[$index]->{near_bounds_values}=[7.70E-03];
	$answer_hashes[$index]->{near_bounds_bounds}=[0];
    $answer_hashes[$index]->{answers}->{0}->{0}->{'raw_invcovmatrix'} = [
		3.88E+00,
		-4.64E+01,  1.46E+03,
		1.52E+00, -4.87E+01,  2.38E+01,
		1.59E+01,  2.83E+03, -3.74E+01,  1.91E+05,
		3.60E+01, -1.24E+03,  1.54E+02,  2.50E+03,  4.61E+03,
		8.56E+02, -1.20E+04,  5.00E+02, -9.66E+04,  4.03E+04,  3.09E+06,
		-1.26E+01, -5.59E+01, -1.80E+02, -4.00E+03, -1.01E+01,  4.85E+04,  6.14E+03,
		-1.55E+00,  2.21E+01, -3.37E+00, -1.35E+02, -8.44E+01, -2.45E+03, -1.16E+01,  4.57E+00,
		2.07E+02, -3.38E+03,  7.97E+01, -4.75E+03,  5.86E+03,  2.25E+05,  2.59E+03, -1.92E+02,  4.79E+04,
		-2.99E+02,  5.71E+03,  7.67E+01,  2.40E+04, -7.83E+03, -2.86E+05, -5.85E+03,  2.13E+02, -6.13E+04,  1.31E+05,
		1.23E+02, -2.78E+03, -1.45E+02, -2.39E+04,  2.41E+03,  8.33E+04,  3.04E+03, -3.55E+01,  2.20E+04, -6.59E+04,  5.24E+04,
		-1.63E-01, -1.75E-01, -1.04E+00, -8.09E+01,  1.37E+00,  1.45E+02,  3.27E+01,  2.18E-01,  9.89E+00, -2.28E+01, 1.38E+01, 2.95E-01,
		1.48E+02, -1.07E+03,  3.89E+01,  1.69E+04,  2.78E+03,  6.33E+04, -1.10E+03, -8.33E+01,  2.28E+04, -2.42E+04,  5.50E+03, -1.63E+01,  5.81E+04,
		-2.15E+00,  2.98E+01, -2.55E+01, -5.19E+02, -2.32E+02, -1.43E+03,  2.86E+02,  4.29E+00, -2.12E+02, -2.26E+02, 3.67E+02,  8.08E-01, -3.14E+02,  5.57E+01
		];


    $index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='nm73/anneal2_V7_30_beta.lst';
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{problem_count}=1;
	$answer_hashes[$index]->{subproblem_count}=[1];
	$answer_hashes[$index]->{answers}={};
#problem index  0 subproblem index 0
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=859.85332322386887;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[3.49590E+00,6.27891E+00,1.05572E+00,1.52839E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[8.07614E-01,6.74746E-01,9.41826E-01,0.00000E+00,0.00000E+00,2.28296E-01,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[2.64841E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sethetas'}=[2.01673E-01,1.80612E-01,8.08876E-02,6.94340E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'seomegas'}=[2.33550E-01,2.57089E-01,2.93394E-01,0.00000E+00,0.00000E+00,6.04961E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sesigmas'}=[6.99733E-01];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/multEST/withSIM/max100_max100_V7_10_g_reg.lst';
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{problem_count}=2;
	$answer_hashes[$index]->{subproblem_count}=[3,2];
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-276.07618174617875;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=0; 
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[2.79788E+01,1.14076E+02,2.66196E+00,2.22218E-01,3.63246E-01,4.84541E-01,5.83322E-01,7.09159E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[1.00000E+00,0.00000E+00,1.00000E+00,0.00000E+00,0.00000E+00,3.62740E-02,0.00000E+00,0.00000E+00,0.00000E+00,1.52350E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.52350E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.04911E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.04911E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[1.00000E+00];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'ofv'}=-247.31262299241806;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'thetas'}=[3.26422E+01,1.66082E+02,1.64039E+00,2.14195E-01,3.54482E-01,5.04495E-01,6.74452E-01,5.65794E-01];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'omegas'}=[1.00000E+00,0.00000E+00,1.00000E+00,0.00000E+00,0.00000E+00,7.27444E-01,0.00000E+00,0.00000E+00,0.00000E+00,4.26494E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,4.26494E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.01796E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.01796E+00];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'sigmas'}=[1.00000E+00];
	$answer_hashes[$index]->{answers}->{0}->{2}->{'ofv'}=-340.22860825268862;
	$answer_hashes[$index]->{answers}->{0}->{2}->{'covariance_step_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{2}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{2}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{2}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{2}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{2}->{'thetas'}=[2.73151E+01,1.27301E+02,2.58121E+00,1.97931E-01,3.33668E-01,3.74253E-01,5.06795E-01,6.33804E-01];
	$answer_hashes[$index]->{answers}->{0}->{2}->{'omegas'}=[1.00000E+00,0.00000E+00,1.00000E+00,0.00000E+00,0.00000E+00,1.94999E-01,0.00000E+00,0.00000E+00,0.00000E+00,3.66361E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,3.66361E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.04774E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.04774E+00];
	$answer_hashes[$index]->{answers}->{0}->{2}->{'sigmas'}=[1.00000E+00];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=-376.58703571678359;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'thetas'}=[2.82982E+01,1.28296E+02,2.35878E+00,2.01751E-01,3.36372E-01,3.49941E-01,5.36288E-01,6.89272E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'omegas'}=[1.00000E+00,0.00000E+00,1.00000E+00,0.00000E+00,0.00000E+00,5.67801E-01,0.00000E+00,0.00000E+00,0.00000E+00,2.04461E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,2.04461E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.00565E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.00565E+00];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sigmas'}=[1.00000E+00];
	$answer_hashes[$index]->{answers}->{1}->{1}->{'ofv'}=-471.95093926904735;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'thetas'}=[2.93105E+01,1.41436E+02,2.34032E+00,1.76322E-01,3.41641E-01,4.07305E-01,4.63707E-01,6.90731E-01];
	$answer_hashes[$index]->{answers}->{1}->{1}->{'omegas'}=[1.00000E+00,0.00000E+00,1.00000E+00,0.00000E+00,0.00000E+00,3.99152E-01,0.00000E+00,0.00000E+00,0.00000E+00,6.77925E-03,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,6.77925E-03,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,3.77667E-01,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,3.77667E-01];
	$answer_hashes[$index]->{answers}->{1}->{1}->{'sigmas'}=[1.00000E+00];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/multEST/withSIM/max100_max100_V7_30_beta.lst';
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{problem_count}=2;
	$answer_hashes[$index]->{subproblem_count}=[3,2];
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-293.36139632157551;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[2.79788E+01,1.14077E+02,2.66195E+00,2.22218E-01,3.63246E-01,4.84540E-01,5.83322E-01,7.09159E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[1.00000E+00,0.00000E+00,1.00000E+00,0.00000E+00,0.00000E+00,3.62738E-02,0.00000E+00,0.00000E+00,0.00000E+00,1.52350E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.52350E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.04911E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.04911E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[1.00000E+00];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'ofv'}=-189.62114864658128;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'thetas'}=[3.26422E+01,1.66082E+02,1.64039E+00,2.14195E-01,3.54482E-01,5.04495E-01,6.74452E-01,5.65794E-01];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'omegas'}=[1.00000E+00,0.00000E+00,1.00000E+00,0.00000E+00,0.00000E+00,7.27444E-01,0.00000E+00,0.00000E+00,0.00000E+00,4.26493E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,4.26493E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.01796E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.01796E+00];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'sigmas'}=[1.00000E+00];
	$answer_hashes[$index]->{answers}->{0}->{2}->{'ofv'}=-334.37351004160899;
	$answer_hashes[$index]->{answers}->{0}->{2}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{2}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{2}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{2}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{2}->{'thetas'}=[2.73151E+01,1.27301E+02,2.58121E+00,1.97931E-01,3.33668E-01,3.74253E-01,5.06795E-01,6.33804E-01];
	$answer_hashes[$index]->{answers}->{0}->{2}->{'omegas'}=[1.00000E+00,0.00000E+00,1.00000E+00,0.00000E+00,0.00000E+00,1.94999E-01,0.00000E+00,0.00000E+00,0.00000E+00,3.66361E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,3.66361E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.04774E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.04774E+00];
	$answer_hashes[$index]->{answers}->{0}->{2}->{'sigmas'}=[1.00000E+00];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=-363.56081918335855;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'thetas'}=[3.24143E+01,1.54543E+02,1.99931E+00,1.70044E-01,3.57692E-01,3.72399E-01,5.83395E-01,6.23866E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'omegas'}=[1.00000E+00,0.00000E+00,1.00000E+00,0.00000E+00,0.00000E+00,9.18690E-01,0.00000E+00,0.00000E+00,0.00000E+00,1.03453E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.03453E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,8.14881E-01,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,8.14881E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sigmas'}=[1.00000E+00];
	$answer_hashes[$index]->{answers}->{1}->{1}->{'ofv'}=-458.54859642114030;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'thetas'}=[3.23465E+01,1.35970E+02,1.96257E+00,1.86058E-01,3.54610E-01,3.92045E-01,5.08687E-01,6.11332E-01];
	$answer_hashes[$index]->{answers}->{1}->{1}->{'omegas'}=[1.00000E+00,0.00000E+00,1.00000E+00,0.00000E+00,0.00000E+00,4.33823E-01,0.00000E+00,0.00000E+00,0.00000E+00,4.22423E-03,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,4.22423E-03,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,4.24353E-01,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,4.24353E-01];
	$answer_hashes[$index]->{answers}->{1}->{1}->{'sigmas'}=[1.00000E+00];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/oneEST/withSIM/p1_cov_p2_sim_noest_V7_10_g_reg.lst';
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{problem_count}=2;
	$answer_hashes[$index]->{subproblem_count}=[1,20];
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=742.05104625255387;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[5.55363E-03,1.33638E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[2.47074E-01,0.00000E+00,1.41581E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[1.64153E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sethetas'}=[3.94763E-04,7.99018E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'seomegas'}=[1.55530E-01,0.00000E+00,3.48941E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sesigmas'}=[3.39466E-03];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'estimate_near_boundary'}=0;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/oneEST/noSIM/p1_p2_tab_noest_Vvi_20.lst';
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{problem_count}=2;
	$answer_hashes[$index]->{subproblem_count}=[1,1];
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=742.051;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[5.55000E-03,1.34000E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[2.47000E-01,0.00000E+00,1.42000E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[1.64000E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=742.051;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'thetas'}=[5.55000E-03,1.34000E+00];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'omegas'}=[2.47000E-01,0.00000E+00,1.42000E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sigmas'}=[1.64000E-02];


	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/oneEST/withSIM/p1_cov_p2_sim_noest_V7_20_g.lst';
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=742.051;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[5.55000E-03,1.34000E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[2.47000E-01,0.00000E+00,1.42000E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[1.64000E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sethetas'}=[3.95000E-04,7.99000E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'seomegas'}=[1.56000E-01,0.00000E+00,3.49000E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sesigmas'}=[3.39000E-03];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=0;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='onePROB/oneEST/withSIM/cov_nsub2_Vvi_10.lst';
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{problem_count}=1;
	$answer_hashes[$index]->{subproblem_count}=[2];
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=934.044;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[9.03000E-03,9.30000E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[3.99000E-01,0.00000E+00,2.32000E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[5.67000E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sethetas'}=[7.30000E-04,5.53000E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'seomegas'}=[8.81000E-02,0.00000E+00,6.33000E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sesigmas'}=[1.32000E-02];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'ofv'}=901.694;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'thetas'}=[9.05000E-03,1.01000E+00];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'omegas'}=[2.89000E-01,0.00000E+00,1.77000E-01];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'sigmas'}=[6.47000E-02];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'sethetas'}=[7.97000E-04,6.50000E-02];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'seomegas'}=[7.24000E-02,0.00000E+00,4.99000E-02];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'sesigmas'}=[7.82000E-03];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='onePROB/oneEST/withSIM/cov_nsub2_V7_30_beta.lst';
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=934.044;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[9.03000E-03,9.30000E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[3.99000E-01,0.00000E+00,2.32000E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[5.67000E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sethetas'}=[7.30000E-04,5.53000E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'seomegas'}=[8.81000E-02,0.00000E+00,6.33000E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sesigmas'}=[1.32000E-02];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'ofv'}=901.694;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'thetas'}=[9.05000E-03,1.01000E+00];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'omegas'}=[2.89000E-01,0.00000E+00,1.77000E-01];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'sigmas'}=[6.47000E-02];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'sethetas'}=[7.97000E-04,6.50000E-02];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'seomegas'}=[7.24000E-02,0.00000E+00,4.99000E-02];
	$answer_hashes[$index]->{answers}->{0}->{1}->{'sesigmas'}=[7.82000E-03];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='nm73/superid2_6_V7_30_beta.lst';
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-41192.390884414948;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[1.99439E+00,3.63042E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[9.76602E-03,2.92096E-05,1.07136E-02,0.00000E+00,0.00000E+00,2.76589E-02,0.00000E+00,0.00000E+00,1.30038E-03,3.19452E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.01345E-01,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,-1.82207E-02,8.06941E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[9.90272E-03];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sethetas'}=[2.22792E-03,2.32011E-03];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'seomegas'}=[2.97557E-04,2.31969E-04,3.29679E-04,0.00000E+00,0.00000E+00,2.32662E-03,0.00000E+00,0.00000E+00,1.85648E-03,2.72959E-03,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,3.27528E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,2.46370E-02,2.71274E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sesigmas'}=[1.23859E-04];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='nm73/example6b_V7_30_beta.lst';
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-4711.4060414857186;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[3.90747E+00,-2.20658E+00,5.61946E-01,-1.83556E-01,2.26636E+00,2.18132E-01,3.71109E+00,-7.04241E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[2.48377E-01,-3.87328E-02,1.70706E-01,4.74358E-02,-1.73692E-02,9.37211E-02,3.16692E-02,5.29769E-02,-1.53826E-02,2.27852E-01,2.80747E-02,2.63515E-02,-1.44630E-03,-3.27241E-02,1.69928E-01,-2.60946E-02,1.08877E-02,2.43980E-02,1.74737E-02,-7.93908E-02,1.92119E-01,3.02589E-02,-4.29579E-02,3.13619E-02,-7.30079E-02,2.43874E-02,3.38764E-03,2.13970E-01,9.72365E-02,8.43240E-02,4.09195E-02,4.65969E-02,1.49755E-03,-5.47501E-02,5.53562E-02,1.99631E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[9.21793E-03,0.00000E+00,2.22112E-02];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='special_mod/tnpri.lst';
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=2;
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=2514.2081585470546;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'thetas'}=[9.87078E+01,2.27590E+01,7.45253E-01,5.56133E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'omegas'}=[2.32539E-01,-2.13450E-02,1.13703E-01,0.00000E+00,0.00000E+00,1.07531E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sigmas'}=[1.27891E+02];
	
	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='special_mod/nwpri.lst';
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=741.51927686677084;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[5.55363E-03,1.33638E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[2.47074E-01,0.00000E+00,1.41581E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[1.64153E-02];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='msfi/cov_V7.lst';
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=-1;
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=742.051;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'condition_number'}=3.90191897654584;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[5.55000E-03,1.34000E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[2.47000E-01,0.00000E+00,1.42000E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[1.64000E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sethetas'}=[3.95000E-04,7.99000E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'seomegas'}=[1.56000E-01,0.00000E+00,3.49000E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sesigmas'}=[3.39000E-03];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='msfi/cov_V7_30_beta.lst';
	$answer_hashes[$index]->{parsed_successfully}=0;
	$answer_hashes[$index]->{answers}={};

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='msfi/sim_noest_Vvi_20.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=-1;
	$answer_hashes[$index]->{problem_count}=2;
	$answer_hashes[$index]->{subproblem_count}=[0,1];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=0;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='msfi/tab_Vvi_20.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=2;
	$answer_hashes[$index]->{problem_count}=2;
	$answer_hashes[$index]->{subproblem_count}=[0,1];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=742.051;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'thetas'}=[5.55000E-03,1.34000E+00];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'omegas'}=[2.47000E-01,0.00000E+00,1.42000E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sigmas'}=[1.64000E-02];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/multEST/noSIM/p1_p2_cov_V7_10_g_reg.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=833.76948783528667;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[1.05000E-02,1.05000E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[4.00000E-01,0.00000E+00,2.50000E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[4.00000E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=742.05104625255387;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'thetas'}=[5.55363E-03,1.33638E+00];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'omegas'}=[2.47074E-01,0.00000E+00,1.41581E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sigmas'}=[1.64153E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sethetas'}=[3.94763E-04,7.99018E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'seomegas'}=[1.55530E-01,0.00000E+00,3.48941E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sesigmas'}=[3.39466E-03];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/multEST/noSIM/p1_p2_cov_V7_20_g.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=833.76948783528667;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[1.05000E-02,1.05000E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[4.00000E-01,0.00000E+00,2.50000E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[4.00000E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sdcorrform_omegas'}=[6.32456E-01,0.00000E+00,5.00000E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sdcorrform_sigmas'}=[2.00000E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=742.05104625255387;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'thetas'}=[5.55363E-03,1.33638E+00];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'omegas'}=[2.47074E-01,0.00000E+00,1.41581E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sigmas'}=[1.64153E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sdcorrform_omegas'}=[4.97065E-01,0.00000E+00,3.76273E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sdcorrform_sigmas'}=[1.28122E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sethetas'}=[3.94763E-04,7.99018E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'seomegas'}=[1.55530E-01,0.00000E+00,3.48941E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sesigmas'}=[3.39466E-03];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sdcorrform_seomegas'}=[1.56448E-01,0.00000E+00,4.63682E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sdcorrform_sesigmas'}=[1.32477E-02];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/multEST/noSIM/p1_p2_cov_V7_12_g_reg.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=833.769;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[1.05000E-02,1.05000E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[4.00000E-01,0.00000E+00,2.50000E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[4.00000E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=742.051;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'thetas'}=[5.55000E-03,1.34000E+00];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'omegas'}=[2.47000E-01,0.00000E+00,1.42000E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sigmas'}=[1.64000E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sethetas'}=[3.95000E-04,7.99000E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'seomegas'}=[1.56000E-01,0.00000E+00,3.49000E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sesigmas'}=[3.39000E-03];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/multEST/noSIM/p1_p2_cov_V7_30_beta.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=833.769;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[1.05000E-02,1.05000E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[4.00000E-01,0.00000E+00,2.50000E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[4.00000E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=742.051;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'thetas'}=[5.55000E-03,1.34000E+00];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'omegas'}=[2.47000E-01,0.00000E+00,1.42000E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sigmas'}=[1.64000E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sethetas'}=[3.95000E-04,7.99000E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'seomegas'}=[1.56000E-01,0.00000E+00,3.49000E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sesigmas'}=[3.39000E-03];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/multEST/noSIM/p1_p2_cov_V7.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=833.76948783528667;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[1.05000E-02,1.05000E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[4.00000E-01,0.00000E+00,2.50000E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[4.00000E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=742.05104625255387;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'thetas'}=[5.55363E-03,1.33638E+00];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'omegas'}=[2.47074E-01,0.00000E+00,1.41581E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sigmas'}=[1.64153E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sethetas'}=[3.94763E-04,7.99018E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'seomegas'}=[1.55530E-01,0.00000E+00,3.48941E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sesigmas'}=[3.39466E-03];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/multEST/noSIM/p1_p2_tab_noest_V7_10_g_reg.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{problem_count}=2;
	$answer_hashes[$index]->{subproblem_count}=[1,1];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=738.19441029848952;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[5.16312E-03,1.41204E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[1.49555E-01,0.00000E+00,1.83409E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[1.82999E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=740.639;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'thetas'}=[5.16000E-03,1.41000E+00];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'omegas'}=[1.50000E-01,0.00000E+00,1.83000E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sigmas'}=[1.83000E-02];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/multEST/noSIM/p1_p2_tab_noest_V7_12_g_reg.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=738.19441029848952;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[5.16312E-03,1.41204E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[1.49555E-01,0.00000E+00,1.83409E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[1.82999E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=740.63872559432298;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'thetas'}=[5.16312E-03,1.41204E+00];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'omegas'}=[1.49555E-01,0.00000E+00,1.83409E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sigmas'}=[1.82999E-02];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/multEST/noSIM/p1_p2_tab_noest_V7_20_g.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=738.61874386531338;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[5.06122E-03,1.43078E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[1.48299E-01,0.00000E+00,1.82469E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[1.78285E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=740.88436134763037;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'thetas'}=[5.06122E-03,1.43078E+00];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'omegas'}=[1.48299E-01,0.00000E+00,1.82469E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sigmas'}=[1.78285E-02];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/multEST/noSIM/p1_p2_tab_noest_V7.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=738.19441029848952;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[5.16312E-03,1.41204E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[1.49555E-01,0.00000E+00,1.83409E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[1.82999E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=740.63872559432298;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'thetas'}=[5.16312E-03,1.41204E+00];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'omegas'}=[1.49555E-01,0.00000E+00,1.83409E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sigmas'}=[1.82999E-02];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/multEST/noSIM/p1_p2_tab_noest_V7_30_beta.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=738.61874385235967;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[5.06122E-03,1.43078E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[1.48299E-01,0.00000E+00,1.82469E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[1.78285E-02];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=740.88436014117190;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'thetas'}=[5.06122E-03,1.43078E+00];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'omegas'}=[1.48299E-01,0.00000E+00,1.82469E-01];
	$answer_hashes[$index]->{answers}->{1}->{0}->{'sigmas'}=[1.78285E-02];


	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='special_mod/near_bounds.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{near_bounds_names}=['TVV','TVV','IOV CL','IOV KA']; 
	$answer_hashes[$index]->{near_bounds_values}=[1.15339E+02,1.15339E+02,1.47000E-06,5.06000E-05];
	$answer_hashes[$index]->{near_bounds_bounds}=[115,115.5,0,0];
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{problem_count}=1;
	$answer_hashes[$index]->{subproblem_count}=[1];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-631.50031282584780;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[3.28941E+01,1.15339E+02,1.45712E+00,8.18601E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[4.16124E-01,3.89690E-01,5.79038E-01,0.00000E+00,0.00000E+00,2.58361E-01,0.00000E+00,0.00000E+00,0.00000E+00,1.47000E-06,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.47000E-06,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,5.06000E-05,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,5.06000E-05];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[1.12380E-01];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='special_mod/rounding_errors.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{problem_count}=1;
	$answer_hashes[$index]->{subproblem_count}=[1];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-585.76675297099121;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[2.62886E+01,1.07708E+02,3.90804E+00,2.42283E-01,3.30242E-01,4.71680E-02,-1.49650E-05,-6.67081E-06,-2.97796E-05,1.99760E-06,-1.59788E-05,3.91623E-06,2.77498E-05,2.38591E-05,1.02152E-01,1.50000E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[6.48598E-02,3.54825E-02,2.86177E-02,0.00000E+00,0.00000E+00,2.98700E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.58236E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.58236E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,4.41612E-01,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,4.41612E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[1.00000E+00];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='special_mod/s_matrix_singular.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{near_bounds_names}=['CL','SIGMA(1,1)']; 
	$answer_hashes[$index]->{near_bounds_values}=[4.84185E-03,7.93076E-03];
	$answer_hashes[$index]->{near_bounds_bounds}=[0,0];
	$answer_hashes[$index]->{problem_count}=1;
	$answer_hashes[$index]->{subproblem_count}=[1];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=45.143655409090925;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_warnings'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[4.84185E-03,1.41308E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[2.25889E-01,0.00000E+00,1.01966E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[7.93076E-03];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sethetas'}=[9.37700E-04,1.95031E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'seomegas'}=[1.21766E-01,0.00000E+00,4.69149E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sesigmas'}=[3.18393E-03];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='special_mod/objv_infinity.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=undef;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[1.86950E+01,6.82496E+02,1.97082E-12,9.99990E+05];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[9.44170E-01,0.00000E+00,1.43831E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[2.60706E-02];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='nm6/nm61_1.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-5975.486;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[1.04000E+02,2.65000E+03,6.65000E+01,4.72000E+03,7.48000E-01,-3.01000E-01,-2.79000E-01,-3.26000E-01,-5.78000E-01,1.60000E+03,6.82000E-01,4.94000E-01,6.08000E-01,6.75000E-01,2.08000E-01,2.64000E-01,0.00000E+00,2.31000E-01,3.15000E-01,7.50000E-01,1.00000E+00,3.64000E-01,2.75000E-02,3.14000E-01,-1.86000E-01,-1.43000E-01,-3.16000E-01,-2.55000E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[2.45000E-01,1.99000E-01,1.90000E-01,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,2.22000E-01,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[1.12000E-01,0.00000E+00,9.26000E-02,0.00000E+00,0.00000E+00,1.59000E-01,0.00000E+00,0.00000E+00,0.00000E+00,5.39000E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.00000E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sethetas'}=[6.88000E+00,1.65000E+02,1.71000E+00,7.94000E+01,5.60000E-02,5.97000E-02,1.22000E-01,5.83000E-02,4.22000E-02,7.88000E+01,3.00000E-02,1.32000E-01,3.17000E-02,4.06000E-02,8.48000E-02,2.84000E-02,0.00000E+00,1.23000E-01,1.15000E-01,0.00000E+00,0.00000E+00,1.47000E-01,1.24000E-01,7.59000E-02,5.20000E-02,8.83000E-02,1.01000E-01,1.50000E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'seomegas'}=[1.68000E-02,1.66000E-02,2.04000E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.78000E-02,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sesigmas'}=[1.65000E-03,0.00000E+00,1.31000E-03,0.00000E+00,0.00000E+00,8.49000E-03,0.00000E+00,0.00000E+00,0.00000E+00,2.55000E-03,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.73000E-03];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='nm74/icon_examples/delayed.res';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{near_bounds_names}=[]; 
	$answer_hashes[$index]->{near_bounds_values}=[];
	$answer_hashes[$index]->{near_bounds_bounds}=[];
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-85.0795013007985;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[2.34316E+01,2.50000E-01,2.41679E-01,5.04213E-02,1.07754E+00,9.13190E+01,1.00000E+00,5.00000E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[1.22387E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sethetas'}=[3.37642E-01,0.00000E+00,2.50121E-02,2.56658E-03,5.44639E-01,1.41546E+01,0.00000E+00,0.00000E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'seomegas'}=[3.46169E-03];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='nm74/icon_examples/control3boot.res';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=39.4268320987164;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[9.84184E-01,1.05444E-01,3.30321E+01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[2.01423E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sethetas'}=[1.60086E-01,9.07980E-03,1.64052E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'seomegas'}=[1.01252E-01];
	$answer_hashes[$index]->{answers}->{0}->{44}->{'ofv'}=20.0445708743240;
	$answer_hashes[$index]->{answers}->{0}->{44}->{'covariance_step_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{44}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{44}->{'covariance_step_warnings'}=1;
	$answer_hashes[$index]->{answers}->{0}->{44}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{44}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{44}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{44}->{'thetas'}=[1.04705E+01,8.47473E-02,3.37695E+01];
	$answer_hashes[$index]->{answers}->{0}->{44}->{'omegas'}=[3.45876E-02];
	$answer_hashes[$index]->{answers}->{0}->{69}->{'ofv'}=-72.8267401581814;
	$answer_hashes[$index]->{answers}->{0}->{69}->{'covariance_step_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{69}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{69}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{0}->{69}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{69}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{69}->{'estimate_near_boundary'}=1;
	$answer_hashes[$index]->{answers}->{0}->{69}->{'thetas'}=[2.52330E+00,8.66996E-02,3.33685E+01];
	$answer_hashes[$index]->{answers}->{0}->{69}->{'omegas'}=[2.00000E-05];
	$answer_hashes[$index]->{answers}->{0}->{99}->{'ofv'}=47.0800325822182;
	$answer_hashes[$index]->{answers}->{0}->{99}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{99}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{99}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{0}->{99}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{99}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{99}->{'estimate_near_boundary'}=0;
	$answer_hashes[$index]->{answers}->{0}->{99}->{'thetas'}=[1.51417E+00,9.99757E-02,3.26092E+01];
	$answer_hashes[$index]->{answers}->{0}->{99}->{'omegas'}=[4.03929E-01];
	$answer_hashes[$index]->{answers}->{0}->{99}->{'sethetas'}=[3.71889E-01,8.02271E-03,1.28137E+00];
	$answer_hashes[$index]->{answers}->{0}->{99}->{'seomegas'}=[2.12480E-01];

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='special_mod/license_dummy.lst';
	$answer_hashes[$index]->{parsed_successfully}=0;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='special_mod/license_missing.lst';
	$answer_hashes[$index]->{parsed_successfully}=0;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='special_mod/license_expired.lst';
	$answer_hashes[$index]->{parsed_successfully}=0;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='special_mod/data_missing.lst';
	$answer_hashes[$index]->{parsed_successfully}=0;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='special_mod/missingmodel.lst';
	$answer_hashes[$index]->{parsed_successfully}=0;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='special_mod/psnmissingmodel.out';
	$answer_hashes[$index]->{parsed_successfully}=0;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='special_mod/psnmissingdata.out';
	$answer_hashes[$index]->{parsed_successfully}=0;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='special_mod/empty.lst';
	$answer_hashes[$index]->{parsed_successfully}=0;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='special_mod/empty_lines.lst';
	$answer_hashes[$index]->{parsed_successfully}=0;

	$index++;
	$answer_hashes[$index]->{file}='onePROB/multEST/firstEstTerm.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-2510.8137852064342;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[4.47434E+01,2.23257E-01,3.27935E+00,1.42116E+02,2.32998E+00,2.43090E-02,4.69452E-01,1.79948E-01,1.07469E+01,1.00000E+00,7.44648E+00,2.10000E-02,4.42944E-01,1.24226E+02,5.00000E-01,6.41308E-02,1.12344E-02,5.48695E-01,1.96984E+00,1.01710E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[1.24200E-03,0.00000E+00,5.16470E-02,0.00000E+00,0.00000E+00,9.75276E-02,0.00000E+00,0.00000E+00,0.00000E+00,1.80301E-03,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,9.00406E-06,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,0.00000E+00,1.23912E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[1.00000E+00];

	$index++;
	$answer_hashes[$index]->{file}='special_mod/covcrash.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=742.05104506946304;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[5.55363E-03,1.33638E+00];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'}=[2.47074E-01,0.00000E+00,1.41581E-01];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'}=[1.64153E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=1;

	$index++;
	$answer_hashes[$index]->{file}='special_mod/covcrash_noext.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=0;

	$index++;
	$answer_hashes[$index]->{file}='multPROB/noEST/withSIM/sim_noest_V7_30.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=-2;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=850.2920;


	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='special_mod/minimterm_cov_unconditional.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-126.07168897227753;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_run'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_step_warnings'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'rounding_errors'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=0;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'condition_number'}=64.3499;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'}=[1.07027E+00,1.01106E+00,1.62179E+00,9.94810E-01,9.91695E-01,9.99383E-01,9.66497E-01,2.96531E+02,1.00943E+00,8.55659E-01,5.80170E-01,0.00000E+00,3.93536E+00,2.94797E-01,6.75100E-03,1.23423E-01,0.00000E+00,9.74910E-02];
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sethetas'}=[5.86046E-01,5.03596E-01,1.43049E-01,5.11541E-01,5.42919E-01,5.40343E-01,2.45831E-01,4.12643E+01,9.77086E-02,2.67354E-02,1.29819E-01,0.00000E+00,5.80841E-01,4.07894E-02,0.00000E+00,4.18430E-02,0.00000E+00,5.58527E-02];

	$index++;
	$answer_hashes[$index]->{file}='special_mod/high_correlations.lst';
	$answer_hashes[$index]->{parsed_successfully}=1; #omega(2,1)-omega(1,1)  omega(2,2)-omega(1,1)  omega(2,2)-omega(2-1)
	$answer_hashes[$index]->{high_correlations_names}=['OMEGA(2,1) - IIV CL','IIV V - IIV CL','IIV V - OMEGA(2,1)']; 
	$answer_hashes[$index]->{high_correlations_values}=[0.991,0.964,0.99];
	$answer_hashes[$index]->{near_bounds_names}=['OMEGA(2,1)']; 
	$answer_hashes[$index]->{near_bounds_values}=[2.10E-01/sqrt(1.17E-01*(3.79E-01))];
	$answer_hashes[$index]->{near_bounds_bounds}=[1];
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'condition_number'}=(3.03E+00/1.62E-04);

	$index++;
	$answer_hashes[$index]->{file}='special_mod/large_standard_errors.lst';
	$answer_hashes[$index]->{parsed_successfully}=1;#    4-2             5-2           5-4        22-2             22-4            22-5         33-3 
	$answer_hashes[$index]->{high_correlations_names}=['KD0 - ln(KL)','L0 - ln(KL)','L0 - KD0','ETA(2) - ln(KL)','ETA(2) - KD0','ETA(2) - L0','ETA(3) - ln(IC50)']; 
	$answer_hashes[$index]->{high_correlations_values}=[0.998,          -0.996,        -0.990,   -0.986,         -0.989,            0.973,       -0.988];
	$answer_hashes[$index]->{large_standard_errors_names}=['L0','ETA(2)','OMEGA(3,1)','OMEGA(3,2)','ETA(3)'];
	$answer_hashes[$index]->{large_standard_errors_values}=[2.36E-02/abs(2.39E-02),1.75E-02/abs(1.32E-02),2.12E-01/abs(-1.26E-01),3.21E-01/abs(-1.87E-01),1.37E+01/abs(4.37E+00)];
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{answers}->{0}->{0}->{'minimization_successful'}=0;

	$index++;
	$answer_hashes[$index]->{file} = 'nm73/theo.lst';
	$answer_hashes[$index]->{parsed_successfully} = 1;
    $answer_hashes[$index]->{answers} = {};
    $answer_hashes[$index]->{answers}->{0}->{0}->{'est_thetanames'} = [ 'THETA1', 'THETA2', 'THETA3' ];
    $answer_hashes[$index]->{answers}->{0}->{0}->{'est_omeganames'} = [ 'OMEGA(1,1)', 'OMEGA(2,1)', 'OMEGA(2,2)', 'OMEGA(3,1)', 'OMEGA(3,2)', 'OMEGA(3,3)' ];
    $answer_hashes[$index]->{answers}->{0}->{0}->{'est_sigmanames'} = [ 'SIGMA(1,1)' ]; 
    $answer_hashes[$index]->{answers}->{0}->{0}->{'thetas'} = [ 2.81E+00, 7.79E-02, 3.62E-02 ];
    $answer_hashes[$index]->{answers}->{0}->{0}->{'omegas'} = [ 5.71E+00, 7.56E-03, 2.42E-04, -8.48E-02, 9.08E-03, 5.12E-01 ];
    $answer_hashes[$index]->{answers}->{0}->{0}->{'sigmas'} = [ 3.86E-01 ];
    $answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_matrix'} = [
        5.08E-01,   # THETA1
        1.31E-03, 5.28E-05,  #THETA2
        9.70E-05, 3.08E-05, 2.02E-05,   #THETA3
        3.45E+00, 7.30E-03, -2.76E-05, 2.48E+01,    #OMEGA(1,1)
        4.73E-03, 7.53E-05, 3.80E-05, 2.59E-02, 1.96E-04,   #OMEGA(2,1)
        1.70E-05, 5.34E-07, 2.64E-07, 5.75E-05, 1.57E-06, 1.72E-08, # OMEGA(2,2)
        1.12E-02, 1.65E-03, 1.03E-03, -2.45E-01, 5.11E-03, 4.42E-05,  2.31E-01, #OMEGA(3,1)
        2.05E-04,  1.55E-06, 6.05E-07, -1.18E-03, 2.69E-05, 3.50E-07,  1.27E-03, 1.43E-05,  #OMEGA(3,2)
        1.03E-02, -2.75E-04, -1.23E-04, -6.10E-02,  6.73E-04,  9.27E-06,  5.43E-02, 7.12E-04,  4.48E-02,    #OMEGA(3,3)
        -1.54E-02,  2.01E-04,  6.26E-05, -1.56E-01,  6.21E-04,  7.84E-06,  1.66E-02,  1.01E-04, -1.41E-03,  1.13E-02    #SIGMA(1,1)
    ];

    $index++;
    $answer_hashes[$index]->{file} = 'nm73/theo_withcov.lst';
    $answer_hashes[$index]->{parsed_successfully} = 1;
    $answer_hashes[$index]->{answers} = {};
    $answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_matrix'} = [
        5.07786E-01, 
        0.00131282,  5.27984E-05,
        9.69754E-05,  3.08185E-05,  2.02063E-05,
        3.44688E+00,  0.00729906, -2.76244E-05,   2.48296E+01,
        4.72920E-03,  7.53160E-05,  3.80490E-05,    2.59374E-02,  1.96274E-04,
        1.69976E-05,  5.33531E-07,  2.63998E-07,    5.74721E-05,  1.57408E-06,  1.71934E-08,
        1.12427E-02,  0.00164902,  0.00103047,   -2.45394E-01,  5.11372E-03,  4.41910E-05,  2.31016E-01,
        0.0002047,  1.54755E-06,  6.05472E-07,   -1.17764E-03,  2.68966E-05,  3.50019E-07,  1.27464E-03,  1.43318E-05,
        1.02500E-02, -2.75206E-04, -1.22964E-04,  -6.09693E-02,  6.72757E-04,  9.26956E-06,  0.0542811,  7.12239E-04,  0.0447782,
        -1.54293E-02,  2.00946E-04,  6.25539E-05,   -1.55937E-01,  0.000621036,  7.83792E-06,  1.65946E-02,  1.00560E-04, -1.41069E-03,  1.12882E-02 
    ];

    $index++;
    $answer_hashes[$index]->{file} = 'special_mod/theo_t_matrix.lst';
    $answer_hashes[$index]->{parsed_successfully} = 1;
    $answer_hashes[$index]->{answers} = {};
    $answer_hashes[$index]->{answers}->{0}->{0}->{'t_matrix'} = [
		0.00E+00,
		0.00E+00,  2.38E+03,
		0.00E+00, -1.09E+04,  5.04E+04,
		0.00E+00, -2.42E-16,  1.11E-15,  2.46E-35,
		0.00E+00, -2.01E-11,  9.27E-11,  2.05E-30,  1.71E-25,
		0.00E+00,  8.21E-09, -3.78E-08, -8.34E-28, -6.95E-23,  2.83E-20,
		0.00E+00, -1.39E+03,  6.40E+03,  1.41E-16,  1.18E-11, -4.80E-09, 8.13E+02, 
		0.00E+00,  2.78E-08, -1.28E-07, -2.83E-27, -2.36E-22,  9.60E-20, -1.63E-08, 3.25E-19, 
		0.00E+00,  6.97E+01, -3.21E+02, -7.08E-18, -5.90E-13,  2.40E-10, -4.07E+01, 8.15E-10,  2.04E+00,
		0.00E+00,  1.75E+01, -8.07E+01, -1.78E-18, -1.48E-13,  6.05E-11, -1.02E+01, 2.05E-10, 5.13E-01, 1.29E-01,
		0.00E+00,  1.18E+01, -5.42E+01, -1.20E-18, -9.97E-14,  4.06E-11, -6.88E+00, 1.38E-10, 3.45E-01, 8.67E-02, 5.82E-02
		];

    $index++;
    $answer_hashes[$index]->{file} = 'special_mod/two_digit_cov_index.lst';
    $answer_hashes[$index]->{parsed_successfully} = 1;
    $answer_hashes[$index]->{answers} = {};
    $answer_hashes[$index]->{answers}->{0}->{0}->{'raw_invcovmatrix'} = [
		3.08E+08,
		-1.07E+08,3.75E+07,
		-5.30E+07,1.85E+07,9.12E+06,
		2.58E+07,-9.01E+06,-4.44E+06,2.17E+06,
		9.89E+07,-3.45E+07,-1.70E+07,8.30E+06,3.18E+07,
		-1.59E+07,5.55E+06,2.73E+06,-1.33E+06,-5.11E+06,8.21E+05,
		-3.62E+09,1.26E+09,6.22E+08,-3.03E+08,-1.16E+09,1.87E+08,4.25E+10,
		-6.08E+08,2.12E+08,1.05E+08,-5.10E+07,-1.95E+08,3.14E+07,7.14E+09,1.20E+09,
		7.46E+06,-2.60E+06,-1.28E+06,6.26E+05,2.40E+06,-3.85E+05,-8.77E+07,-1.47E+07,1.81E+05,
		5.07E+08,-1.77E+08,-8.72E+07,4.25E+07,1.63E+08,-2.62E+07,-5.96E+09,-1.00E+09,1.23E+07,8.35E+08,
		5.45E+08,-1.90E+08,-9.37E+07,4.57E+07,1.75E+08,-2.81E+07,-6.40E+09,-1.08E+09,1.32E+07,8.97E+08,9.65E+08,
		-5.80E+07,2.02E+07,9.98E+06,-4.87E+06,-1.86E+07,3.00E+06,6.82E+08,1.15E+08,-1.41E+06,-9.56E+07,-1.03E+08,1.09E+07,
		2.64E+08,-9.20E+07,-4.54E+07,2.21E+07,8.47E+07,-1.36E+07,-3.10E+09,-5.21E+08,6.40E+06,4.35E+08,4.67E+08,-4.97E+07,2.26E+08,
		9.91E+09,-3.46E+09,-1.70E+09,8.31E+08,3.18E+09,-5.12E+08,-1.16E+11,-1.96E+10,2.40E+08,1.63E+10,1.75E+10,-1.87E+09,8.49E+09,3.19E+11,
		2.19E+08,-7.64E+07,-3.77E+07,1.84E+07,7.03E+07,-1.13E+07,-2.57E+09,-4.32E+08,5.31E+06,3.61E+08,3.88E+08,-4.13E+07,1.88E+08,7.05E+09,1.56E+08,
		-9.51E+09,3.32E+09,1.64E+09,-7.98E+08,-3.06E+09,4.91E+08,1.12E+11,1.88E+10,-2.31E+08,-1.57E+10,-1.68E+10,1.79E+09,-8.15E+09,-3.06E+11,-6.77E+09,2.94E+11,
		-1.37E+08,4.80E+07,2.36E+07,-1.15E+07,-4.42E+07,7.10E+06,1.62E+09,2.71E+08,-3.33E+06,-2.26E+08,-2.43E+08,2.59E+07,-1.18E+08,-4.42E+09,-9.78E+07,4.25E+09,6.15E+07,
		4.40E+08,-1.54E+08,-7.58E+07,3.70E+07,1.42E+08,-2.27E+07,-5.17E+09,-8.70E+08,1.07E+07,7.25E+08,7.80E+08,-8.30E+07,3.77E+08,1.42E+10,3.13E+08,-1.36E+10,-1.97E+08,6.30E+08,
		-1.80E+09,6.26E+08,3.09E+08,-1.51E+08,-5.77E+08,9.27E+07,2.11E+10,3.54E+09,-4.35E+07,-2.96E+09,-3.18E+09,3.38E+08,-1.54E+09,-5.78E+10,-1.28E+09,5.55E+10,8.02E+08,-2.57E+09,1.05E+10,
		1.10E+09,-3.84E+08,-1.89E+08,9.24E+07,3.54E+08,-5.69E+07,-1.29E+10,-2.17E+09,2.67E+07,1.81E+09,1.95E+09,-2.08E+08,9.44E+08,3.54E+10,7.83E+08,-3.40E+10,-4.92E+08,1.58E+09,-6.42E+09,3.94E+09,
		1.16E+08,-4.05E+07,-2.00E+07,9.75E+06,3.73E+07,-6.00E+06,-1.36E+09,-2.29E+08,2.82E+06,1.91E+08,2.06E+08,-2.19E+07,9.96E+07,3.74E+09,8.26E+07,-3.59E+09,-5.19E+07,1.66E+08,-6.78E+08,4.16E+08,4.38E+07];


    $index++;
    $answer_hashes[$index]->{file} = 'special_mod/sparse_format_cov_matrix.lst';
    $answer_hashes[$index]->{parsed_successfully} = 1;
    $answer_hashes[$index]->{answers} = {};
    $answer_hashes[$index]->{answers}->{0}->{0}->{'covariance_matrix'} = [
    2.13E-03,         
	3.70E-04, 2.40E-03,         
	3.77E-04 ,2.21E-04 ,4.26E-03,        
	-1.52E-05,1.67E-04 ,1.61E-03, 2.91E-03,
	-2.51E-04, 9.51E-05, 5.83E-05, -9.67E-05,8.40E-04, 
   1.09E-05,  1.74E-04, 2.54E-05, -1.43E-04,3.14E-04, 5.35E-04,       
	-3.90E-05, -2.28E-05,-2.18E-04,-1.24E-04,1.40E-04, 2.40E-04,1.22E-03,
   -1.72E-04 ,  -8.86E-06,-2.67E-04,2.17E-04  , 1.44E-04, 2.38E-04,8.20E-05 ,1.06E-03,         
    -8.29E-05,  -8.04E-05, 6.23E-05,1.21E-04,  1.88E-04   ,4.06E-05,2.80E-04,-6.25E-05,1.47E-03,       
    -3.31E-04 ,-3.25E-04 , -2.61E-04,4.94E-04, 2.20E-04 , 1.45E-04, 5.41E-04,8.13E-04 ,4.36E-04,4.20E-03,   
   -1.84E-04 , -2.18E-04 , 1.34E-04,1.63E-04 ,  2.24E-04, 1.82E-04,1.37E-04 ,5.47E-04, -2.93E-05,3.81E-04,7.62E-04 ,
    -2.62E-04,-1.01E-04  ,1.32E-04 ,1.46E-04 ,1.14E-04 ,8.62E-05  ,3.78E-04,2.40E-05 ,5.88E-04,4.02E-04,9.07E-05,7.47E-04,
    -2.35E-05,-8.77E-05, 2.28E-04  , 2.47E-04, 2.49E-04, 1.15E-04 ,3.75E-04,2.89E-04 ,3.82E-04,2.18E-03,2.11E-04,3.30E-04,2.03E-03,       
    2.93E-05 ,-1.97E-05, 1.74E-04 ,-3.14E-04 , 2.71E-04, 1.26E-04 ,3.24E-04  ,6.07E-05,2.03E-04, 1.10E-03,1.49E-04,2.25E-04,1.34E-03,1.65E-03,     
    4.47E-05 ,  6.18E-05, -1.18E-06, 3.01E-05 ,-5.92E-05,-4.22E-05,-7.90E-05,-5.65E-05,7.95E-06,-9.36E-05,-7.69E-05,-3.98E-05,-1.06E-04,-1.16E-04,5.79E-05 
		];


    $index++;
    $answer_hashes[$index]->{file} = 'special_mod/zeroruntime.lst';
    $answer_hashes[$index]->{parsed_successfully} = 1;
    $answer_hashes[$index]->{runtime} = '00:00:00';
    $answer_hashes[$index]->{answers} = {};

    $index++;
    $answer_hashes[$index]->{file} = 'special_mod/postprocesstime.lst';
    $answer_hashes[$index]->{parsed_successfully} = 1;
    $answer_hashes[$index]->{runtime} = '0:08:02';
    $answer_hashes[$index]->{answers} = {};
	$answer_hashes[$index]->{answers}->{0}->{0}->{'sum_estimation_time'}=478.93;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/oneEST/noSIM/multprob_nm712.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-559.89313598683509;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=-553.46988748710919;
	$answer_hashes[$index]->{answers}->{2}->{0}->{'ofv'}=-558.53874243878795;
	$answer_hashes[$index]->{answers}->{3}->{0}->{'ofv'}=-560.84030244582539;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/oneEST/noSIM/multprob_nm712_noext.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-559.893;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=-553.470;
	$answer_hashes[$index]->{answers}->{2}->{0}->{'ofv'}=-558.539;
	$answer_hashes[$index]->{answers}->{3}->{0}->{'ofv'}=-560.840;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/oneEST/noSIM/multprob_nm710.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-555.688;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=-547.911;
	$answer_hashes[$index]->{answers}->{2}->{0}->{'ofv'}=-563.021;
	$answer_hashes[$index]->{answers}->{3}->{0}->{'ofv'}=-560.777;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/oneEST/noSIM/multprob_nm720.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-555.04235144492679;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=-559.67920118052928;
	$answer_hashes[$index]->{answers}->{2}->{0}->{'ofv'}=-563.81626251355419;
	$answer_hashes[$index]->{answers}->{3}->{0}->{'ofv'}=-560.40149791248427;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/oneEST/noSIM/multprob_nm730.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-558.25489893903068;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=-566.20617959163906;
	$answer_hashes[$index]->{answers}->{2}->{0}->{'ofv'}=-554.14690890054476;
	$answer_hashes[$index]->{answers}->{3}->{0}->{'ofv'}=-552.94094141583696;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/oneEST/noSIM/multprob_nm740beta.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-559.24199356206861;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=-555.55731999512977;
	$answer_hashes[$index]->{answers}->{2}->{0}->{'ofv'}=-551.85341632298309;
	$answer_hashes[$index]->{answers}->{3}->{0}->{'ofv'}=-562.36483954451865;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/multEST/withSIM/multprobmix_nm730.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-560.94860931564199;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=undef;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'ofv'}=undef;
	$answer_hashes[$index]->{answers}->{2}->{0}->{'ofv'}=-496.32222094253308;
	$answer_hashes[$index]->{answers}->{3}->{0}->{'ofv'}=-475.21574178971809;
	$answer_hashes[$index]->{answers}->{4}->{0}->{'ofv'}=-722.46794253879420;
	$answer_hashes[$index]->{answers}->{4}->{1}->{'ofv'}=-712.67215984695213;
	$answer_hashes[$index]->{answers}->{4}->{2}->{'ofv'}=-705.52435900849582;
	$answer_hashes[$index]->{answers}->{5}->{0}->{'ofv'}=-569.40860646604642;
	$answer_hashes[$index]->{answers}->{6}->{0}->{'ofv'}=-551.47787941243621;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/multEST/withSIM/multprobmix_nm710.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-560.94860366519333;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=undef;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'ofv'}=undef;
	$answer_hashes[$index]->{answers}->{2}->{0}->{'ofv'}=-496.322;
	$answer_hashes[$index]->{answers}->{3}->{0}->{'ofv'}=-475.216;
	$answer_hashes[$index]->{answers}->{4}->{0}->{'ofv'}=-722.468;
	$answer_hashes[$index]->{answers}->{4}->{1}->{'ofv'}=-712.672;
	$answer_hashes[$index]->{answers}->{4}->{2}->{'ofv'}=-705.524;
	$answer_hashes[$index]->{answers}->{5}->{0}->{'ofv'}=-569.409;
	$answer_hashes[$index]->{answers}->{6}->{0}->{'ofv'}=-551.478;



	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/multEST/withSIM/multprobmix_nm720.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-560.94860364775752;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=undef;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'ofv'}=undef;
	$answer_hashes[$index]->{answers}->{2}->{0}->{'ofv'}=-496.32221630076214;
	$answer_hashes[$index]->{answers}->{3}->{0}->{'ofv'}=-475.21573581031379;
	$answer_hashes[$index]->{answers}->{4}->{0}->{'ofv'}=-722.46793724366478;
	$answer_hashes[$index]->{answers}->{4}->{1}->{'ofv'}=-712.67215413061240;
	$answer_hashes[$index]->{answers}->{4}->{2}->{'ofv'}=-705.52435331302206;
	$answer_hashes[$index]->{answers}->{5}->{0}->{'ofv'}=-569.40859223812595;
	$answer_hashes[$index]->{answers}->{6}->{0}->{'ofv'}=-551.47787396093008;

	$index++;
	$answer_hashes[$index]={};
	$answer_hashes[$index]->{file}='multPROB/multEST/withSIM/multprobmix_3_nm730.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-560.94860931564199;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=undef;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'ofv'}=undef;
	$answer_hashes[$index]->{answers}->{2}->{0}->{'ofv'}=-496.32222094253308;
	$answer_hashes[$index]->{answers}->{3}->{0}->{'ofv'}=-475.21574178971809;
	$answer_hashes[$index]->{answers}->{4}->{0}->{'ofv'}=-722.46794253879420;
	$answer_hashes[$index]->{answers}->{4}->{1}->{'ofv'}=-712.67215984695213;
	$answer_hashes[$index]->{answers}->{4}->{2}->{'ofv'}=-705.52435900849582;
	$answer_hashes[$index]->{answers}->{5}->{0}->{'ofv'}=-569.40860646604642;
	$answer_hashes[$index]->{answers}->{6}->{0}->{'ofv'}=-551.47787941243621;

	$index++;

	$answer_hashes[$index]->{file}='multPROB/multEST/withSIM/multprobmix_nm712.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-560.94860354934463;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=undef;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'ofv'}=undef;
	$answer_hashes[$index]->{answers}->{2}->{0}->{'ofv'}=-496.32221641683088;
	$answer_hashes[$index]->{answers}->{3}->{0}->{'ofv'}=-475.21573581031379;
	$answer_hashes[$index]->{answers}->{4}->{0}->{'ofv'}=-722.46793726016938;
	$answer_hashes[$index]->{answers}->{4}->{1}->{'ofv'}=-712.67215435439618;
	$answer_hashes[$index]->{answers}->{4}->{2}->{'ofv'}=-705.52435339164538;
	$answer_hashes[$index]->{answers}->{5}->{0}->{'ofv'}=-569.40859691303922;
	$answer_hashes[$index]->{answers}->{6}->{0}->{'ofv'}=-551.47787398117805;

	$index++;

	$answer_hashes[$index]->{file}='onePROB/oneEST/withSIM/evaluation_nm730.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{estimation_evaluation_problem_number}=-1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=782.60689655390649;
	$answer_hashes[$index]->{answers}->{0}->{1}->{'ofv'}=729.02713435169971;

	$index++;
	$answer_hashes[$index]->{file}='multPROB/multEST/withSIM/multprobmix_2_nm710.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-560.94860354934463;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=undef;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'ofv'}=undef;
	$answer_hashes[$index]->{answers}->{2}->{0}->{'ofv'}=-496.322;
	$answer_hashes[$index]->{answers}->{3}->{0}->{'ofv'}=-654.386;
	$answer_hashes[$index]->{answers}->{3}->{1}->{'ofv'}=-691.184;
	$answer_hashes[$index]->{answers}->{4}->{0}->{'ofv'}=-722.468;
	$answer_hashes[$index]->{answers}->{4}->{1}->{'ofv'}=-712.672;
	$answer_hashes[$index]->{answers}->{4}->{2}->{'ofv'}=-705.524;
	$answer_hashes[$index]->{answers}->{5}->{0}->{'ofv'}=-569.409;
	$answer_hashes[$index]->{answers}->{6}->{0}->{'ofv'}=-551.478;

	$index++;
	$answer_hashes[$index]->{file}='multPROB/multEST/withSIM/multprobmix_2_nm712.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-560.94860354934463;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=undef;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'ofv'}=undef;
	$answer_hashes[$index]->{answers}->{2}->{0}->{'ofv'}=-496.32221641683088;
	$answer_hashes[$index]->{answers}->{3}->{0}->{'ofv'}=-654.38616869426903;
	$answer_hashes[$index]->{answers}->{3}->{1}->{'ofv'}=-691.18369732098222;
	$answer_hashes[$index]->{answers}->{4}->{0}->{'ofv'}=-722.46793726016938;
	$answer_hashes[$index]->{answers}->{4}->{1}->{'ofv'}=-712.67215435439618;
	$answer_hashes[$index]->{answers}->{4}->{2}->{'ofv'}=-705.52435339164538;
	$answer_hashes[$index]->{answers}->{5}->{0}->{'ofv'}=-569.40859691303922;
	$answer_hashes[$index]->{answers}->{6}->{0}->{'ofv'}=-551.47787398117805;

	$index++;
	$answer_hashes[$index]->{file}='multPROB/multEST/withSIM/multprobmix_2_nm720.lst';
	$answer_hashes[$index]->{answers}={};
	$answer_hashes[$index]->{parsed_successfully}=1;
	$answer_hashes[$index]->{answers}->{0}->{0}->{'ofv'}=-560.94860364775752;
	$answer_hashes[$index]->{answers}->{1}->{0}->{'ofv'}=undef;
	$answer_hashes[$index]->{answers}->{1}->{1}->{'ofv'}=undef;
	$answer_hashes[$index]->{answers}->{2}->{0}->{'ofv'}=-496.32221630076214;
	$answer_hashes[$index]->{answers}->{3}->{0}->{'ofv'}=-654.38616874343393;
	$answer_hashes[$index]->{answers}->{3}->{1}->{'ofv'}=-691.18369737154046;
	$answer_hashes[$index]->{answers}->{4}->{0}->{'ofv'}=-722.46793724366478;
	$answer_hashes[$index]->{answers}->{4}->{1}->{'ofv'}=-712.67215413061240;
	$answer_hashes[$index]->{answers}->{4}->{2}->{'ofv'}=-705.52435331302206;
	$answer_hashes[$index]->{answers}->{5}->{0}->{'ofv'}=-569.40859223812595;
	$answer_hashes[$index]->{answers}->{6}->{0}->{'ofv'}=-551.47787396093008;

	return \@answer_hashes;
}

1;
