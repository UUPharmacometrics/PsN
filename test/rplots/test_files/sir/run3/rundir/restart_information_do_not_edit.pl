%parameter_hash = (
                    'coords' => [
                                  '1',
                                  '2',
                                  '3',
                                  '1,1',
                                  '2,2',
                                  '1,1'
                                ],
                    'labels' => [
                                  'CL',
                                  'V',
                                  'CLWGT',
                                  'IVCL',
                                  'IVV',
                                  'SIGMA(1,1)'
                                ],
                    'choleskyform' => [
                                        0,
                                        0,
                                        0,
                                        0,
                                        0,
                                        0
                                      ],
                    'block_number' => [
                                        0,
                                        0,
                                        0,
                                        0,
                                        0,
                                        0
                                      ],
                    'coordinate_strings' => [
                                              'THETA1',
                                              'THETA2',
                                              'THETA3',
                                              'OMEGA(1,1)',
                                              'OMEGA(2,2)',
                                              'SIGMA(1,1)'
                                            ],
                    'off_diagonal' => [
                                        0,
                                        0,
                                        0,
                                        0,
                                        0,
                                        0
                                      ],
                    'inits' => [
                                 '0.00563997',
                                 '1.35134',
                                 '1.13',
                                 '0.0487546',
                                 '0.129957',
                                 '0.0174894'
                               ],
                    'param' => [
                                 'theta',
                                 'theta',
                                 'theta',
                                 'omega',
                                 'omega',
                                 'sigma'
                               ],
                    'lower_bounds' => [
                                        0,
                                        0,
                                        '-0.435',
                                        0,
                                        0,
                                        0
                                      ],
                    'sdcorrform' => [
                                      0,
                                      0,
                                      0,
                                      0,
                                      0,
                                      0
                                    ],
                    'values' => [
                                  '0.00618958',
                                  '1.3538',
                                  '1.13452',
                                  '0.046732',
                                  '0.130531',
                                  '0.0166369'
                                ],
                    'upper_bounds' => [
                                        1000000,
                                        1000000,
                                        '1.429',
                                        1000000,
                                        1000000,
                                        1000000
                                      ]
                  );
@center_rawresults_vector = (
                              'input',
                              1,
                              1,
                              1,
                              1,
                              1,
                              0,
                              0,
                              0,
                              0,
                              0,
                              0,
                              0,
                              '4.2',
                              '9.22986',
                              'FOCE',
                              '0:00:02',
                              '0.5',
                              '0.41',
                              '687.763925240872',
                              '0.00618958',
                              '1.3538',
                              '1.13452',
                              '0.046732',
                              '0.130531',
                              '0.0166369',
                              '0.000306852',
                              '0.0729223',
                              '0.21504',
                              '0.0216478',
                              '0.0402793',
                              '0.00342753',
                              undef,
                              undef,
                              undef,
                              '0.224065',
                              '0.366914',
                              '0.658204',
                              '1.27746',
                              '1.40527',
                              '2.06809'
                            );
@negative_dofv = (
                   0,
                   0,
                   0,
                   0,
                   0
                 );
@intermediate_raw_results_files = (
                                    'raw_results_sir_iteration1.csv',
                                    'raw_results_sir_iteration2.csv',
                                    'raw_results_sir_iteration3.csv',
                                    'raw_results_sir_iteration4.csv',
                                    'raw_results_run3.csv'
                                  );
@samples = (
             1000,
             1000,
             1000,
             2000,
             2000
           );
@resamples = (
               200,
               400,
               500,
               1000,
               1000
             );
@attempted_samples = (
                       1000,
                       1000,
                       1114,
                       2000,
                       2000
                     );
@successful_samples = (
                        999,
                        898,
                        1114,
                        2000,
                        1999
                      );
@actual_resamples = (
                      200,
                      359,
                      557,
                      1000,
                      1000
                    );
@seed_array = (
                1041341892,
                201427763
              );
$with_replacement = 0;
$mceta = 0;
$problems_per_file = 1;
@minimum_ofv = (
                 '688.240546781661',
                 '688.075615629348',
                 '687.961642812041',
                 '687.962621259161',
                 '688.078658678241'
               );
$reference_ofv = '687.763925240872';
$done = 1;
$iteration = 5;
$recenter = 1;
$copy_data = 1;
$boxcox = 1;
$nm_version = 'default';
$model_filename = 'run3.mod';
$cap_resampling = 1;
$cap_correlation = '0.8';
$adjust_blocks = 0;
$check_cholesky_reparameterization = 0;
$subjects = '59';
