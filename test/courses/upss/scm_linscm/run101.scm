model = run101.mod
logfile=run101.log
search_direction = both
p_forward=0.05
p_backward=0.01
continuous_covariates= WT,APGR
foce=1
linearize=1
error=add
epsilon=0
[test_relations]
CL=WT,APGR
V=WT,APGR

[valid_states]
continuous = 1,2