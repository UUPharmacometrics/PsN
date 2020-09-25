model=pheno_localmin.mod
search_direction=both
p_backward=0.01
p_forward=0.05

continuous_covariates=WGT,CV1,CV2
categorical_covariates=APGR,CVD1

[test_relations]
CL=WGT,APGR,CV1
V=WGT,CV2,CVD1

[valid_states]
continuous = 1,2
categorical = 1,2


