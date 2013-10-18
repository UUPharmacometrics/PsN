; config_all_default_parameterizations_explicitly.scm
; This file shows how the covariate function forms 
; none/not included, linear, hockey-stick, exponential and power
; could be defined using the [code] section of the scm configuration file.
; The definitions in this file are shown as examples and illustration on
; how covariate function code can be defined.
; Never use exactly the definitions given in the [code] section below, 
; because all these covariate function forms are predefined in scm including 
; handling of some special cases that are not mentioned here. 
; These function forms also have shortcut definitions none, linear, 
; hockey-stick, exponential and power, if the user wants to change the 
; default numbering of the functions.

model=pheno_with_cov.mod
search_direction=forward
logfile=scmlog1.txt
p_forward=0.05

continuous_covariates=WGT,CV1,CV2,CV3
categorical_covariates=CVD1

[test_relations]
CL=WGT,CV1,CV2,CV3
V=CVD1,WGT

[valid_states]
continuous = 1,2,3,4,5
categorical = 1,2

[included_relations]
CL=WGT-2,CV1-3,CV2-4,CV3-5
V=CVD1-2,WGT-1

; Function definitions in the [code] section are essentially NONMEM code. 
; PsN will replace 
; PAR with the parameter name, for example CL,
; COV with the covariate name, for example WGT, 
; median with the median of the covariate, for example 42.33
; maximum with the maximum of the covariate, for example 78.22
; and minimum with the minimum of the covariate, for example 12.09
; If the code spans multiple lines, all lines except the last on must have 
; an \ at the end. Never set \ at the end of a configuration file comment, 
; (i.e. a line in this file that starts with ;) because that
; might cause very strange errors.
; Each definition starts with 
; parameter:covariate-state_number=
; for example
; CL:WGT-2=
; Everything to the right of the equal sign is used as NONMEM code,
; after PsN:s replacements.
; * is a wildcard meaning 'all parameters' or 'all covariates'
; Indentation will not be preserved
; In the definitions below minimum, median and maximum are assumed positive,
; otherwise the definitions would be slightly different. PsN will 
; automatically take care of negative values.


;*****************************************************************
; The [code] line must only appear once in the configuration file. 
;*****************************************************************
[code]

;----------------------------------------------------------------------
; none/not included
;----------------------------------------------------------------------
; Setting the covariate function to 1 gives no effect, since the typical value
; of the parameter is multiplied with the covariate function
*:*-1=PARCOV=1

;----------------------------------------------------------------------
;Linear functions
;----------------------------------------------------------------------
;For the linear relation we need to separate between the continuous
;and the categorical covariates
;It is ***NOT*** possible to first define state 2 for all using *:*-2=
;and then write new code for some covariates to override the *:*-2
;for the exceptions. 

;----------------------------------------------------------------------
;Linear function for a continuous covariate with -99 as missing data token.
;----------------------------------------------------------------------
;If we have missing data must handle that too
;note that a \ is required at the end of all but the last code line. 
;Never put \ on the last line
*:WGT-2=IF(COV.EQ.-99) THEN\
     PARCOV=1\
ELSE\
     PARCOV= ( 1 + THETA(1)*(COV - median))\
ENDIF

;----------------------------------------------------------------------
;Linear function for a continuous covariate without missing data
;----------------------------------------------------------------------
*:CV1-2=PARCOV= ( 1 + THETA(1)*(COV - median))


;----------------------------------------------------------------------
;Linear function for a bivariate categorical covariate
;----------------------------------------------------------------------
;PsN checks automatically how many categories there are and which category 
;is the most common. When writing code one must check manually how many
;categories there are and which is the most common.
;Note the inclusion of a comment which will appear in the modelfile
*:CVD1-2=IF(COV.EQ.1) PARCOV = 1  ; Most common \
IF(COV.EQ.0) PARCOV = ( 1 + THETA(1))

;----------------------------------------------------------------------
;Hockey-stick, or piece-wise linear, function for continuous covariates.
;----------------------------------------------------------------------
;Note that numbering of thetas start at 1 for each individual code
;definition, scm will take care of renumbering the thetas.
;Note that two thetas are needed for the hockey-stick function, THETA 1 and 2, 
;for the two different slopes.
;PsN will not preserve the original numbering of the THETAs. The THETA 
;that appears first will get the lowest number.
;
;*:*-3=... will define state 3 to the hockey-stick function for all covariates,
;including the categorical, so it is important to noe use state 3 
;for categorical covariates by mistake

*:*-3=IF(COV.LE.median) PARCOV = ( 1 + THETA(1)*(COV - median))\
IF(COV.GT.median) PARCOV = ( 1 + THETA(2)*(COV - median))

;----------------------------------------------------------------------
;Exponential function for continuous covariates.
;----------------------------------------------------------------------

*:*-4=PARCOV= EXP(THETA(1)*(COV - median))

;----------------------------------------------------------------------
;Power function for continuous covariates.
;----------------------------------------------------------------------

*:*-5=PARCOV= ((COV/median)**THETA(1))


;----------------------------------------------------------------------
;Defining bounds and initial values for THETAs 
;----------------------------------------------------------------------

;When the [code] section is used it is often also necessary to define
;lower and upper bounds for the THETAs in the code, so that parameters
;cannot reach unphysiological values. For example, TVCL must not be multiplied
;with a negative covariate function value so that CL can become negative. 
;The default lower, initial, upper values are -1000000,0.001,1000000 for newly defined states
;which means it is not necessary to set values in the configuration file
;if the default values are good enough. In the default parameterizations
;the boundaries are set so that the covariate function cannot become negative. This
;is appropriate e.g. for parameters CL and V, but other parameters should perhaps be allowed
;to become negative. In that case the boundaries might have to be reset.
;In this example file all values are set as an illustration of the syntax.
 
;As in the code section, each definition starts with
;parameter:covariate-state_number=
;for example
;CL:WGT-2=
;After the equal sign follows a numerical value, or a mathematical expression
;that can be evaluated by Perl. Before evaluating the expression, PsN will
;replace 
;median with the median of the covariate, for example 42.33
;maximum with the maximum of the covariate, for example 78.22
;and minimum with the minimum of the covariate, for example 12.09

;*****************************************************************
;the line [lower_bounds] must appear only once in the configuration file
;*****************************************************************
[lower_bounds]

;----------------------------------------------------------------------
; none/not included
;----------------------------------------------------------------------
; The 'none' function has no thetas, so no bounds or inits are needed


;As in the [code] section it is important to defined bounds
;for thetas for categorical and continuous covariates separately

;----------------------------------------------------------------------
;continuous linear theta lower bound, make sure function cannot become negative 
;----------------------------------------------------------------------
*:WGT-2=1/(median-maximum)
*:CV1-2=1/(median-maximum)
;----------------------------------------------------------------------
;categorical linear theta lower bound 
;----------------------------------------------------------------------

*:CVD1-2=-1

;----------------------------------------------------------------------
;Hockey-stick, or piece-wise linear, function for continuous covariates.
;----------------------------------------------------------------------
;Need two thetas in hockey-stick function -> need two lower bounds.
;The order in which the bounds are defined is important. The theta that
;appears first in the code must have its boundary defined first.
;The two lower boundaries must be separated by a comma
*:*-3=-1000000,1/(median-maximum)

;----------------------------------------------------------------------
;Exponential function for continuous covariates.
;----------------------------------------------------------------------

*:*-4=-1000000
;----------------------------------------------------------------------
;Power function for continuous covariates.
;----------------------------------------------------------------------

*:*-5=-1000000

;*****************************************************************
;*****************************************************************
[upper_bounds]
;----------------------------------------------------------------------
;continuous linear theta upper bound 
;----------------------------------------------------------------------

*:WGT-2=1/(median-minimum)
*:CV1-2=1/(median-minimum)
;----------------------------------------------------------------------
;categorical linear theta upper bound 
;----------------------------------------------------------------------

*:CVD1-2=5

;----------------------------------------------------------------------
;Hockey-stick, or piece-wise linear, function for continuous covariates.
;----------------------------------------------------------------------
;Need two values again, the order is important
*:*-3=1/(median-minimum),1000000

;----------------------------------------------------------------------
;Exponential function for continuous covariates.
;----------------------------------------------------------------------
*:*-4=1000000

;----------------------------------------------------------------------
;Power function for continuous covariates.
;----------------------------------------------------------------------
*:*-5=1000000

;*****************************************************************
;initial values are set in the section [inits]
;*****************************************************************

[inits]
;----------------------------------------------------------------------
;continuous linear theta initial value 
;----------------------------------------------------------------------
*:WGT-2=0.001/(median-minimum)
*:CV1-2=0.001/(median-minimum)

;----------------------------------------------------------------------
;categorical covariate linear theta initial value 
;----------------------------------------------------------------------
*:CVD1-2=-0.001

;----------------------------------------------------------------------
;Hockey-stick, or piece-wise linear, function for continuous covariates.
;----------------------------------------------------------------------
;need two values, order is important
*:*-3=0.001*(1/(median-minimum)),0.001*(1/(median-maximum))

;----------------------------------------------------------------------
;Exponential function for continuous covariates.
;----------------------------------------------------------------------
*:*-4=0.001

;----------------------------------------------------------------------
;Power function for continuous covariates.
;----------------------------------------------------------------------
*:*-5=0.001



