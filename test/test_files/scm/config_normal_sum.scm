model=pheno_with_cov.mod
search_direction=both
logfile=scmlog1.txt
p_forward=0.05
p_backward=0.01
abort_on_fail=0
retries=3


continuous_covariates=WGT,APGR,CV1,CV2,CV3
categorical_covariates=CVD1,CVD2,CVD3
logit=CL,V

[test_relations]
CL=WGT,CVD1
V=WGT,CVD1

[valid_states]
continuous = 1,2
categorical = 1,2

[code]
*:WGT-2=PARWGT=(THETA(1)*(WGT-1.30))
*:CVD1-2=IF(CVD1.EQ.1) PARCVD1 = 0\ 
IF(CVD1.NE.1) PARCVD1 = THETA(1)



