model=pheno_with_cov.mod
linearize=1
search_direction=forward
p_forward=0.05
update_derivatives=1

continuous_covariates=WGT,APGR
categorical_covariates=CVD1,CVD2,CVD3

[test_relations]
;ETA1
CL=WGT,CVD1
;ETA2
V=WGT,CVD1

[valid_states]
continuous = 1,2
categorical = 1,2

