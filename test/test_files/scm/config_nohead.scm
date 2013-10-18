model=pheno_nohead.mod
linearize=1
search_direction=both
logfile=scmlog1.txt
p_forward=0.05
p_backward=0.01
abort_on_fail=0
retries=3
update_derivatives=1

continuous_covariates=WGT,APGR


[test_relations]
CL=WGT,APGR
V=WGT
;ETA1=WGT,APGR,CV1,CV2,CV3,CVD1,CVD2,CVD3
;ETA2=WGT,APGR,CV1,CV2,CV3,CVD1,CVD2,CVD3

[valid_states]
continuous = 1,3
categorical = 1,2

