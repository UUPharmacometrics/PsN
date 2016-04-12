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
1=100
2=200
3=300
4=400
5=500
6=510
7=520
8=530
9=540
10=550

[test_relations]
CL=WGT
V=WGT
;ETA1=WGT,APGR,CV1,CV2,CV3,CVD1,CVD2,CVD3
;ETA2=WGT,APGR,CV1,CV2,CV3,CVD1,CVD2,CVD3

[valid_states]
continuous = 1,2
categorical = 1,2

[included_relations]
CL=WGT-2
V=WGT-2
