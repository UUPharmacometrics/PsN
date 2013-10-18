model=pheno_with_cov.mod
linearize=1
search_direction=forward
p_forward=0.05
p_backward=0.01
abort_on_fail=0
retries=3


categorical_covariates=CVD1,CVD2


[test_relations]
;ETA1
CL=CVD1,CVD2
;ETA2
V=CVD1,CVD2

[valid_states]
continuous = 1,2
categorical = 1,2


