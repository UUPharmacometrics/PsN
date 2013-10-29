;config_centering_bivariate_covariate.scm
;This configuration file illustrates how categorical covariates with
;two categories can be centered using the [code] section.
;CVD1 is used in the example.

model=pheno_with_cov.mod
search_direction=forward
p_forward=0.05

continuous_covariates=WGT
categorical_covariates=CVD1

[test_relations]
CL=WGT
V=CVD1

[valid_states]
continuous = 1,2,5
categorical = 1,2

[code]
;Note that all but the last line must have an \ at the end
;Here we assume 0.37 is the fraction of individual with CVD1 equal to 0

*:CVD1-2=IF(COV.EQ.1) PARCOV = ( 1 + THETA(1)*0.37)\
IF(COV.EQ.0) PARCOV = ( 1 - THETA(1)*(1-0.37))

;bounds must be defined so that V cannot become negative,
;i.e. the covariate functions must not become negative
[lower_bounds]

*:CVD1-2=-1/0.37

[upper_bounds]

*:CVD1-2=1/(1-0.37)

[inits]
*:CVD1-2=0.01



