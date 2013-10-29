search_direction=both
model=pheno_with_cov.mod
logfile=scmlog1.txt
p_forward=0.05
p_backward=0.01
abort_on_fail=0
retries=5

continuous_covariates=WGT,APGR,CV1,CV2,CV3
categorical_covariates=CVD1,CVD2,CVD3

[test_relations]
CL=WGT,APGR
;CV1,CV2,CV3,CVD1,CVD2,CVD3
V=WGT,APGR
;CV1,CV2,CV3,CVD1,CVD2,CVD3

[valid_states]
continuous = 1,2,4
categorical = 1,2

