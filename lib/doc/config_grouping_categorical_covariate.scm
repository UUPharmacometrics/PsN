;config_grouping_categorical_covariate.scm
;This configuration file illustrates how categorical covariates with
;many categories can be grouped using the [code] section, merging
;groups with few individuals. APGR is used in the example.

model=pheno_with_cov.mod
search_direction=forward
p_forward=0.05

continuous_covariates=WGT,CV1
categorical_covariates=CVD1,APGR

[test_relations]
CL=WGT,CV1
V=CVD1,APGR

[valid_states]
continuous = 1,2,5
categorical = 1,2

[code]
;PsN will renumber the THETAs. The THETA that appears first will get
;the lowest number, the next will get the second lowest, and so on. 
;Note that all but the last line must have an \ at the end

*:APGR-2=IF(COV.EQ.1) PARCOV = ( 1 + THETA(1))\
IF(COV.EQ.2) PARCOV = ( 1 + THETA(1))\
IF(COV.EQ.3) PARCOV = ( 1 + THETA(1))\
IF(COV.EQ.4) PARCOV = ( 1 + THETA(1))\
IF(COV.EQ.5) PARCOV = ( 1 + THETA(2))\
IF(COV.EQ.6) PARCOV = ( 1 + THETA(3))\
IF(COV.EQ.7) PARCOV = 1  ; Most common\
IF(COV.EQ.8) PARCOV = ( 1 + THETA(4))\
IF(COV.EQ.9) PARCOV = ( 1 + THETA(5))\
IF(COV.EQ.10) PARCOV = ( 1 + THETA(5))

;lower bounds must be defined so that parameters cannot become negative,
;i.e. the covariate functions must not become negative
[lower_bounds]
;Need 5 estimates for *:APGR since 5 THETAs are used. The values
;must be defined in the same order as the THETAs
;appear in the NONMEM code above. PsN will ignore the order of the original 
;THETA numbers, it is only the order in which THETAs appear that is taken 
;into account. In this particular example the bounds and initial values are 
;the same for all THETAs.

*:APGR-2=-1,-1,-1,-1,-1

[upper_bounds]

*:APGR-2=5,5,5,5,5

[inits]
*:APGR-2=0.01,0.01,0.01,0.01,0.01



