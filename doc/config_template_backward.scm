;;config_template_backward.scm
;;In backward search it is normally only interesting to have one parameterization
;;for each parameter-covariate pair, since in a previous forward search it has already been
;;investigated which parameterization is the best. The backward search should decide if
;;the covariate should be included in this best form or not at all, period.
;;The scm was not originally designed to handle different valid states for different 
;;parameter-covariate pairs, so some tricks must be used, 
;;see sections [valid_states], [code] and [included_relations] below

;;lines starting with ; are comments
;;if a line starts with ; it must not end with \ because that will cause 
;;strange errors. 
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

search_direction=backward

;;provided that the perl module Math::CDF is installed, any p-values can 
;;be used.
p_backward=0.01

;;it is required to list the covariates to test
continuous_covariates=WGT,AGE
categorical_covariates=CVD1

;do_not_drop=WGT
;missing_data_token=-99

;;if [included_relations] is set, PsN will run the base model with those 
;;relations to obtain a reference ofv value to be used when computing 
;;p-values for the candidate models. If the user already knows the ofv 
;;value for the base model with included relations, time can be saved by 
;;giving scm that ofv-value on the command-line (not in the config file)
;;using option -base_ofv

;;These general PsN options that can be set in the configuration file. 
;;Most general PsN options must however be set on the command-line
nm_version=7

[test_relations]
CL=WGT,AGE
V=WGT,CVD1

;;Set valid states to 1,2 for both continuous and categorical and
;;then use [code] to redefine state 2 to the best parameterization found for each 
;;parameter-covariate pair
[valid_states]
continuous = 1,2
categorical = 1,2

;assume that in the forward search we ended up with WGT in linear relation on CL,
;AGE in power relation on CL, WGT in exponential relation on V and CVD1 in linear relation on V
;Then we can leave CL:WGT-2 and V:CVD1-2 to the default linear parameterizations
;Redefine AGE on CL and  and WGT on V
[code]
CL:AGE-2=power 
V:WGT-2=exponential

;;included relations must be defined if search_direction is backward, otherwise there
;;are no relations for scm to remove
;;Here we include all relations that were found significant in a previous forward search
[included_relations]
CL=WGT-2,AGE-2
V=WGT-2,CVD1-2

