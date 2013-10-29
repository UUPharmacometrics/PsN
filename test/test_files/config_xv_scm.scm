linearize=1
model=mox1.mod
foce=1
epsilon=0
error=add

continuous_covariates=AGE
categorical_covariates=ACE,DIG,DIU,SEX

[test_relations]
CL=AGE,DIU,SEX
V= ACE,DIG

[valid_states]
continuous = 1,2
categorical= 1,2

