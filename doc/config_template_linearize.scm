;config_template_linearize.scm
;lines starting with ; are comments
;Most of the options in this file are optional, but this file is 
;a good check-list. Not all scm options
;are listed here, see the userguide for a complete list.

;;nonlinear model without any of the covariate combinations in test_relations. 
;;Other covariates may be included
;;There is no file run34.mod in the extra material
model=run34.mod 

;;it is helpful, but not necessary, to set the name of the run directory
directory=scm_run34_2

;---------------------------
;;options for linearization
linearize=1 ;must be set
;;foce=1 requires that nonlinear input model has METHOD=1. If nonlinear model 
;;is run with FO set foce=0
foce=1 

;;set update_derivatives=1 to increase accuracy by recalculating 
;;derivatives after each covariate addition
update_derivatives=0 

;;Set option lst_file if you want to use estimates from a lst-file
;;as initial estimates when running the nonlinear model to obtain
;;the derivatives needed as input to the linear model. Using
;;this option can save time.
lst_file=run34.lst


;;If the derivatives data has been generated previously, it is possible to
;;save time by giving this data file as input to the scm. Then there is
;;no need to run the nonlinear model before starting the forward search with
;;the linearized model. The user must check carefully that the derivatives 
;;data file matches what is needed for this particular run, there is
;;no checking by the scm program.
;derivatives_data=run34_1_derivatives_covariates.dta

;;option epsilon is set by default, meaning that scm linearizes with respect 
;;to epsilons in addition to with respect to etas. 
;;try setting epsilon=0 if you run out of columns in $INPUT (NONMEM only 
;;allows 50)
;epsilon=0

;;option error is only allowed if epsilon=0, then it is required. 
;;This option tells scm the type of the error function.
;;The possible alternatives are add for additive, prop for proportional, 
;;propadd for proportional plus additive or user for user-specified. 
;error=add 

;;if error=user then option error_code is required. Give exact NONMEM code 
;;that spcifies Y using IPRED
;;If the code spans multiple lines all lines except the last must end 
;;with \ (a backslash)
;error_code=Y=IPRED+IPRED*EPS(1)*EXP(ETA(4))+EPS(2)*EXP(ETA(5))

;;If any item in $INPUT is needed to run the model even for the base model 
;;when no covariates are included it must be listed in do_not_drop. 
;;Needed for all parameters used in IGNORE/ACCEPT statements and
;;for all parameters (except IPRED, ETA() and EPS()) in the user specified 
;;error code, if option error_code is used.
;;Note: option do_not_drop has a slightly different functionality for 
;;linearized and regular (linearize=0) scm. With linearize=1 do not list 
;;parameters that are in $PK/$PRED
;;but not in IGNORE/ACCEPT or the error code.
;do_not_drop=AGE

;end options for linearization
;--------------------------------------------


;;search direction can be forward, backward or both
search_direction=forward

;;provided that the perl module Math::CDF is installed, any p-values can 
;;be used. p_backward should be smaller than p_forward 
p_forward=0.05
;p_backward=0.01

;;it is required to list the covariates to test
continuous_covariates=WGT,AGE
categorical_covariates=CVD1


;;if the covariates have missing data values, it is essential to 
;;tell scm how missing values are coded
;missing_data_token=-99

;;if a parameter in test_relations is a logit transformation, it is 
;;important to tell scm so that it can adjust the covariate 
;;relation function properly
;logit=BIO,PHI

;;if parallel_states is set to 1, scm will test all possible relation forms 
;;for a parameter-covariate pair simultaneously,
;;instead of trying them one at time, in the order set in valid_states
;parallel_states=1

;These general PsN options can be set in the configuration file. 
;Most general PsN options must however 
;be set on the command-line
nm_version=7 ;the linearize option only works together with NONMEM7
retries=1
threads=6
tweak_inits=1
picky=0

[test_relations]
CL=WGT,AGE
V=WGT,CVD1
;BIO=WGT
;PHI=WGT

;;valid_states (possibly in combination with [code]) tells scm which 
;;parameterizations should be tested for the covariates
;;There are default meanings to numbers 1-5, but by adding a [code] section 
;;new functions can be defined, and numbers can be set to mean different 
;;parameterizations for different parameter-covariate pairs 
;;The first valid state must always be 1
[valid_states]
continuous = 1,2,5
categorical = 1,2



