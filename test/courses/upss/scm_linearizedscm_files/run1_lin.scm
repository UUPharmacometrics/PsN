model = run1.mod
logfile=run1.log
search_direction=both
p_forward=0.05
p_backward=0.01
linearize=1
foce=1
epsilon=0
error=add

continuous_covariates=AGE
categorical_covariates=ACE,DIG,DIU,SEX,NYHA

[test_relations]
CL=AGE,ACE,DIG,DIU,SEX,NYHA
V= AGE,ACE,DIG,DIU,SEX,NYHA

[valid_states]
continuous = 1,2
categorical= 1,2