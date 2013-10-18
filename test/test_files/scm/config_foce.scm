model=pheno_with_cov.mod
linearize=1
search_direction=forward
p_forward=0.05
p_backward=0.01
abort_on_fail=0
retries=3

continuous_covariates=WGT,APGR


[test_relations]
;ETA1
CL=WGT
;ETA2
V=APGR

[valid_states]
continuous = 1,2
categorical = 1,2

[code]
V:*-2=PARCOV= ( 1 + THETA(1)*(COV - median))
CL:WGT-2=PARCOV= ( 1 + THETA(1)*(COV - median))

[lower_bounds]
*:APGR-2=1/(median-maximum)

[upper_bounds]

*:WGT-2=1/(median-minimum)

[inits]
*:APGR-2=0.001/(median-minimum)
