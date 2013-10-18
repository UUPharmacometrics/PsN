linearize=1
search_direction=forward
p_forward=0

continuous_covariates=WGT,APGR,CV1,CV2,CV3
categorical_covariates=CVD1,CVD2,CVD3

[test_relations]
;ETA1
CL=WGT,CVD1
;ETA2
V=WGT,CVD1

[valid_states]
continuous = 1
categorical = 1

