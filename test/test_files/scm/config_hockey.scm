model=pheno_with_cov.mod
search_direction=forward
p_forward=0.05
parallel_states=1
continuous_covariates=WGT,APGR,CV1,CV2,CV3
categorical_covariates=CVD1,CVD2,CVD3
parallel_states=1

[test_relations]
CL=WGT


[valid_states]
continuous = 1,3,4
categorical = 1,2

[code]
CL:WGT-4=BP=THETA(1)\
PARCOV=1+(THETA(2)*BP**99/(BP**99+COV**99)+THETA(3)*COV**99/(BP**99+COV**99))*(COV-median)

[lower_bounds]
CL:WGT-4=minimum,-100000,1/(median-maximum)


[upper_bounds]
CL:WGT-4=maximum,1/(median-minimum),100000


[inits]
CL:WGT-4=median,0.001*(1/(median-minimum)),0.001*(1/(median-maximum))

