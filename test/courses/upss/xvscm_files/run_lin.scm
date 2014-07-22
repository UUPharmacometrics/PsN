model = run.mod
logfile=run.log
linearize=1
foce=1
epsilon=0
error=add

continuous_covariates=AGE,CRCL,WT
categorical_covariates=ACE,DIG,DIU,SEX,NYHA2

[test_relations]
CL=AGE,CRCL,ACE,DIG,DIU,SEX,WT
V= AGE,WT,ACE,DIG,DIU,SEX

[valid_states]
continuous = 1,2
categorical= 1,2

[inits]
CL:CRCL-2=0.0001