model=pheno_with_cov.mod
search_direction=both
logfile=scmlog1.txt
p_forward=0.05
p_backward=0.01
abort_on_fail=0
retries=3

continuous_covariates=WGT,APGR,CV1
;,CV2,CV3
categorical_covariates=CVD1
;,CVD2,CVD3

[test_relations]
CL=WGT,CVD1,APGR
V=WGT,CVD1

[valid_states]
continuous = 1,5
categorical = 1

