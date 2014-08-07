model = run100.mod
logfile=run100.log
search_direction = both
p_forward=0.05
p_backward=0.01
continuous_covariates= WT,APGR

[test_relations]
CL=WT,APGR
V=WT,APGR

[valid_states]
continuous = 1,2