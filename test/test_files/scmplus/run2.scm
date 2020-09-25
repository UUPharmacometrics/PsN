model=run1.mod
search_direction=both
;fix=0
;logfile=scmlog.txt
p_value=0.05
;picky=1
;abort_on_fail=0
linearize=1

continuous_covariates=WGT,APGR

;[extra_data_files]
;pheno_extra.dta=ID,WGT,APGR

[valid_states]
categorical=1,2
continuous=1,2,3,4

[test_relations]
CL=WGT,APGR
V=WGT,APGR

[inits]
CL:WGT-2 = 0.01
CL:WGT-3 = 0.01,0.01
CL:WGT-4 = 0.01

[lower_bounds]
CL:WGT-2 = -2
CL:WGT-3 = -2,-2
CL:WGT-4 = -2

[upper_bounds]
CL:WGT-2 = 2
CL:WGT-3 = 2,2
CL:WGT-4 = 2

;[ofv_change]
;1=5.86
;3=10
