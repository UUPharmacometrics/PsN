;;config_emax_and_other_parameterizations.scm
;;lines starting with ; are comments
;;there is no file run34.mod in the extra material, this example config file is for a hypothetical model file

model=run34.mod
directory=scm_run34
search_direction=forward
p_forward=0.05

continuous_covariates=WGT,AGE
categorical_covariates=SEX,RACE,APGR,XA,XB

[test_relations]
CL=WGT,SEX,RACE,APGR,AGE
V=XA,XB

;;Since there are no default parameterizations for state 7 and 8 those 
;states must be defined in [code] section below
[valid_states]
continuous = 1,2,3,7,8
categorical = 1,2


;When parameterising alwyas use theta numbering starting from 1 in all 
;definitions, scm will renumber properly
;Remember that scm always multiplies TV<parameter>, 
;with the covariate function, e.g. TVCL=TVCL*CLSEX

[code]

;Example 1
;redefining state 2 for all parameter with covariate WGT
;A linear function normalized with range
;Use wildcard * for all parameters. In the code use PAR, 
;and then scm will replace PAR with CL, V etc
;scm will replace median, maximum and minimum with
;the computed statistics for WGT from the datafile
*:WGT-2=PARWGT=(1+THETA(1)*(WGT-median)/((maximum-minimum)/2))

;Example 2
;Redefine state 3 for all parameters with covariate AGE
;Hockey-stick with estimated breakpoint instead of break at median
*:AGE-3=BP=THETA(1)\
PARAGE=1+(THETA(2)*BP**99/(BP**99+AGE**99)+THETA(3)*AGE**99/(BP**99+AGE**99))*(AGE-BP)

;Example 3
;Define new state 7 as Emax model for all parameter-covariate pairs
;Use wildcard for both parameter and covariate to defined for all combinations at once
;PAR will be replaced with CL, V... and COV with WGT, AGE... as appropriate 
*:*-7=PARCOV=THETA(1)*COV/(THETA(2)+COV)

;Example 4
;Define new state 8 as sigmoidal Emax for all parameter-covariate pairs
*:*-8=PARCOV=THETA(1)*(COV**THETA(2))/((THETA(3)**THETA(2))+(COV**THETA(2)))


;must define bounds so that CL etc do not get unphysiological values
;can use text minimum maximum median which scm will replace with the actual
;computed values for the covariates
[lower_bounds]
;Example 1
*:WGT-2=((maximum-minimum)/2)/(median-maximum)

;Example 2. Bounds must be defined for THETAs in the order they appear in the definition
;BP,TH2,TH3
;BP cannot be lower than minimum AGE, but cannot define perfect expression for
;TH3 since BP estimated. Needs investigation case by case, ideally 
;the bound should be 1/(BP-maximum), but we must approximate here
*:AGE-3=minimum,-100000,1/(median-maximum)

;Example 3,4
;Boundaries and inits must be defined individually since allowed values of THETAs 
;depend on the covariate values
;Important: When the boundaries are set the order of the values must match the order in which
;THETAs appear in *:*-7=... or *:*-8=... above. The **numbering** of THETAs **does not** decide the
;order in which inits and bounds should be defined. 
;These example do not have appropriate numerical values, they only demonstrate syntax
CL:WGT-7=1,0
CL:WGT-8=1,-20,0
CL:AGE-7=1,2
CL:AGE-8=1,-10,2


[upper_bounds]
;Example 1
*:WGT-2=((maximum-minimum)/2)/(median-minimum)
;Example 2.
;BP cannot be higher than maximum AGE
;TH2 difficult since BP estimated, bound should ideally be 1/(BP-minimum). We approximate here
*:AGE-3=maximum,1/(median-minimum),100000

;Example 3,4
;Boundaries and inits must be defined individually since allowed values depend on the covariate values
;If the boundaries are set then the order of the values must match the order in which
;THETAs appear in [code] Example 3 and 4 above, the **numbering** of THETAs **does not** decide the
;order in which inits and bounds should be defined 
;Default upper boundary for all THETAs is 100000

[inits]
;Example 1
*:WGT-2=0.01

;Example 2
;appropriate init for BP is median of AGE.
*:AGE-3=median,0.001*(1/(median-minimum)),0.001*(1/(median-maximum))

;Example 3,4
;Boundaries and inits must be defined individually since allowed values depend on the covariate values
;If the boundaries are set then the order of the values must match the order in which
;THETAs appear in [code] Example 3 and 4 above, the **numbering** of THETAs **does not** decide the
;order in which inits and bounds should be defined 
;Default init is 0.001 (or global_init if that option is set) for all THETAs

