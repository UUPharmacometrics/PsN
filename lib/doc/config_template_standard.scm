;;config_template_standard.scm
;;lines starting with ; are comments
;;if a line starts with ; it must not end with \ because that will cause 
;;strange errors. 
;;Some of the options in this file can also be given on the command-line, 
;;but it is convenient to set them in the config file
;;Most of the options in this file are optional, but this file is a good 
;;check-list. Not all scm options are listed here, see the userguide for 
;;a complete list.
;;Edit as needed, comment/uncomment options to suit your run

;;model file without any of the covariate combinations in test_relations. 
;;Other covariates may be included
;;There is no run34.mod in the extra material
model=run34.mod

;;it is helpful, but not necessary, to set the name of the run directory
directory=scm_run34

;;search direction can be forward, backward or both
search_direction=forward

;;provided that the perl module Math::CDF is installed, any p-values can 
;;be used. p_backward should be smaller than p_forward 
p_forward=0.05
;p_backward=0.01

;;it is required to list the covariates to test
continuous_covariates=WGT,AGE,CV1
categorical_covariates=CVD1

;;If a covariate listed in continuous_covariates or categorical_covariates 
;;is needed to run the model even when the covariate is not included in a 
;;covariate relation, it must be listed in do_not_drop (otherwise scm will 
;;DROP it in $INPUT for models where it is not in a covariate relation). 
;;Needed e.g. when covariate used in IGNORE/ACCEPT or used in $PK/$PRED of 
;;base model
;do_not_drop=WGT

;;if the covariates have missing data values, it is essential to tell scm 
;;how missing values are coded in the datafile
;missing_data_token=-99

;;if a parameter in test_relations has been logit transformed, it is essential 
;;to tell scm so that it can adjust the parameterization properly
;logit=BIO,PHI

;;if [included_relations] is set, PsN will run the base model with those 
;;relations to obtain a reference ofv value to be used when computing 
;;p-values for the candidate models. If the user already knows the ofv 
;;value for the base model with included relations, time can be saved by 
;;giving scm that ofv-value 
;base_criteria_values={ofv=>-1209}

;;By default option parallel_states=0 and scm tries parameterizations
;;one at a time, in the order set in valid_states. Only if the covariate
;;is included in the model with the first parameterization is the next
;;parameterization tried.
;;if parallel_states is set to 1, scm will test all possible relation 
;;forms for a parameter-covariate pair simultaneously.
;parallel_states=1

;;These general PsN options that can be set in the configuration file. 
;;Most general PsN options must however be set on the command-line
nm_version=7
retries=1
threads=6
tweak_inits=1
picky=0

;;In the configuration file all single-line options must come BEFORE the 
;;first bracket-header section, otherwise the options will be ignored by scm
;;Each bracket-header section can have many lines, but each header must 
;;appear at most one time

[test_relations]
CL=WGT,AGE,CV1
V=WGT,CVD1
;BIO=WGT
;PHI=WGT

;;valid_states (possibly in combination with [code]) tells scm which 
;;parameterizations should be tested for the covariates
;;There are default meanings to numbers 1-5, but by adding a [code] section 
;;new parameterizations can be defined, and numbers can be set to mean 
;;different parameterizations for different parameter-covariate pairs 
;;The first valid state must always be 1
[valid_states]
continuous = 1,2,5
categorical = 1,2




