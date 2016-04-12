model=pheno_with_tv.mod
search_direction=forward
logfile=scmlog1.txt
p_forward=0.05

continuous_covariates=WGT,APGR,CV1,CV2,CV3
categorical_covariates=CVD1,CVD2,CVD3

[test_relations]
CL=WGT,CV1,CV2,CV3,CVD1
V=APGR

[valid_states]
continuous = 1,2,3,4,5
categorical = 1,2

[included_relations]
CL=WGT-2,CVD1-1
V=APGR-4

[code]
V:*-1=none
V:*-2=none
V:*-3=none
V:*-4=none
V:*-5=linear
