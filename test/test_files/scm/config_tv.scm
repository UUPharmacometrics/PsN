model=pheno_with_tv.mod
linearize=1
search_direction=backward
gof=ofv
abort_on_fail=0
retries=3
update_derivatives=1

continuous_covariates=WGT,APGR,CV1,CV2,CV3
categorical_covariates=CVD1,CVD2,CVD3

[ofv_backward]
1=10
2=20
3=30
4=40
5=50
6=51
7=52
8=53
9=54
10=55

[test_relations]
CL=WGT
V=WGT
;ETA1=WGT,APGR,CV1,CV2,CV3,CVD1,CVD2,CVD3
;ETA2=WGT,APGR,CV1,CV2,CV3,CVD1,CVD2,CVD3

[valid_states]
continuous = 1,2,4
categorical = 1,2,4

[included_relations]
CL=WGT-4
V=WGT-4
