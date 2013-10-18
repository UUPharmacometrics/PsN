model=pheno_logit.mod
foce=1
linearize=1
search_direction=forward
p_forward=0.05
p_backward=0.01
abort_on_fail=0
retries=3

continuous_covariates=WGT,APGR,CV1,CV2,CV3
categorical_covariates=CVD1,CVD2,CVD3
logit=LG1

[test_relations]
;ETA1
LG1=WGT,CVD1
;ETA2
V=WGT,CVD1

[valid_states]
continuous = 1,2
categorical = 1,2

[included_relations]
LG1=WGT-2
V=CVD1-2