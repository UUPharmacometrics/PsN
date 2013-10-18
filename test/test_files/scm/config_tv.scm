model=pheno_with_tv.mod
linearize=1
search_direction=backward
logfile=scmlog1.txt
p_forward=1
p_backward=0.01
abort_on_fail=0
retries=3
update_derivatives=1

continuous_covariates=WGT,APGR,CV1,CV2,CV3
categorical_covariates=CVD1,CVD2,CVD3

[test_relations]
CL=WGT
V=WGT
;ETA1=WGT,APGR,CV1,CV2,CV3,CVD1,CVD2,CVD3
;ETA2=WGT,APGR,CV1,CV2,CV3,CVD1,CVD2,CVD3

[valid_states]
continuous = 1,2,4
categorical = 1,2,4

[included_relations]
CL=WGT-4
V=WGT-4
