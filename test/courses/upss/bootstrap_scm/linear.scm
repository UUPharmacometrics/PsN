model = run4.mod
;logfile=run4.log
search_direction = forward
nm_version=default
p_forward=0.05
;p_backward=0.001
linearize=1
foce=1
epsilon=0
error=add

continuous_covariates=AGE,CRCL,WT
categorical_covariates=ACE,DIG,DIU,SEX,NYHA

[test_relations]
CL=AGE,CRCL,ACE,DIG,DIU,SEX,WT
V=AGE,WT,ACE,DIG,DIU,SEX

[valid_states]
continuous = 1,2 
categorical= 1,2
