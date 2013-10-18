model=pheno_with_cov.mod
search_direction=forward
p_forward=0.05
parallel_states=1
continuous_covariates=WGT,APGR,CV1,CV2,CV3
categorical_covariates=CVD1,CVD2,CVD3

[test_relations]
CL=WGT


[valid_states]
continuous = 1,2,3
categorical = 1,2

[code]
CL:WGT-2=PARCOV=THETA(1)*COV/(THETA(2)+COV)
CL:WGT-3=PARCOV=THETA(1)*(COV**THETA(2))/((THETA(3)**THETA(2))+(COV**THETA(2)))

[lower_bounds]
CL:WGT-2=-1000,0.01
CL:WGT-3=-1000,-10,0.01

[inits]
CL:WGT-2=0.01,median
CL:WGT-3=0.01,0.01,median

[upper_bounds]
CL:WGT-2=1000,maximum
CL:WGT-3=1000,10,maximum
