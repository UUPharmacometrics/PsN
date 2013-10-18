model=pheno_ignore.mod
search_direction=forward
p_forward=0.05
p_backward=0.01
abort_on_fail=0
gof=ofv
max_steps=1
continuous_covariates=WGT,APGR
time_varying=WGT,APGR

[test_relations]
CL=WGT,APGR
V=WGT,APGR

[valid_states]
continuous = 1,2
categorical = 1,2

;[included_relations]
;CL=WGT-2
;V=WGT-2

[code]
V:APGR-2=VAPGR=(THETA(1)*(APGR-mean))


