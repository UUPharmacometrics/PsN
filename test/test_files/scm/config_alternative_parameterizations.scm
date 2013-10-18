;;config_alternative_parameterizations.scm
;;lines starting with ; are comments

model=pheno_with_cov.mod
search_direction=forward
p_forward=0.05

continuous_covariates=WGT,APGR,CV1,CV2,CV3
categorical_covariates=CVD1,CVD2,CVD3

[included_relations]
CL=CVD1-2,APGR-2,WGT-2
V=APGR-2,WGT-3

[test_relations]
CL=WGT,CVD1,APGR
V=APGR,WGT

;there are no default parameterizations for states 7,8
;so if listed here they must be defined in the code section
[valid_states]
continuous = 1,2,3
categorical = 1,2


;When parameterising alwyas use theta numbering starting from 1 in all 
;definitions, scm will renumber properly
;Remember that scm always multiplies TV<parameter>, 
;with the covariate function, e.g. TVCL=TVCL*CLCVD1

[code]


;Example 4
;redefining state 2 for all parameter with covariate WGT
;Use wildcard * for all parameters. In the code use PAR, 
;and then scm will replace PAR with CL, V etc
;scm will replace median, maximum and minimum with
;the computed statistics for WGT from the datafile
CL:WGT-2=PARWGT=(1+THETA(1)*(WGT-median)/((maximum-minimum)/2))

;Example 5
;Redefine state 3 for all parameters with covariate WGT
;Hockey-stick with estimated breakpoint instead of break at median
V:WGT-3=BP=THETA(1)\
PARWGT=1+(THETA(2)*BP**99/(BP**99+WGT**99)+THETA(3)*WGT**99/(BP**99+WGT**99))*(WGT-median)


;must define bounds so that CL etc do not get unphysiological values
;can use text minimum maximum median which scm will replace with the actual
;computed values for the covariates
[lower_bounds]
;Example 4
CL:WGT-2=((maximum-minimum)/2)/(median-maximum)

;Example 5. Bounds must be defined for THETAs in the order they appear in the definition
;BP,TH2,TH3
;BP cannot be lower than minimum WGT, but cannot define perfect expression for
;TH3 since BP estimated. Needs investigation case by case, ideally 
;the bound should be 1/(BP-maximum), but we must approximate here
V:WGT-3=minimum,-100000,1/(median-maximum)


;Example 6. Interaction terms
;Since scm only adds

[upper_bounds]
;Example 4
CL:WGT-2=((maximum-minimum)/2)/(median-minimum)
;Example 5.
;BP cannot be higher than maximum WGT
;TH2 difficult since BP estimated, bound should ideally be 1/(BP-minimum). We approximate here
V:WGT-3=maximum,1/(median-minimum),100000


[inits]
;Example 4
CL:WGT-2=0.01

;Example 5
;appropiate init for BP is median.
V:WGT-3=median,0.001*(1/(median-minimum)),0.001*(1/(median-maximum))
