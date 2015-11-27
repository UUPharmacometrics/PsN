model=pheno_with_cov_foce_prop.mod
foce=1
linearize=1
search_direction=forward
gof=ofv
abort_on_fail=0
retries=3
error=prop
epsilon=0


continuous_covariates=WGT,APGR,CV1,CV2,CV3
categorical_covariates=CVD1,CVD2,CVD3

[ofv_forward]
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
;ETA1
CL=WGT,CVD1
;ETA2
V=WGT,CVD1

[valid_states]
continuous = 1,2,4
categorical = 1,2

