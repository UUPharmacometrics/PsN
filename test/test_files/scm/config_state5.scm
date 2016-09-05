model=pheno_with_cov.mod
search_direction=both
logfile=scmlog1.txt
p_forward=0.05
p_backward=0.01
abort_on_fail=0
retries=3

continuous_covariates=WGT,APGR,CV1
;,CV2,CV3

;,CVD2,CVD3

[test_relations]
CL=WGT,APGR

[valid_states]
continuous = 1,5


