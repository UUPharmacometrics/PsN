;;config_different_parameterizations_different_covariates.scm
;;lines starting with ; are comments
;;there is no file run34.mod in the extra material, this example config file is for a hypothetical model file

;model=run34.mod
;directory=scm_run34
model=pheno_with_cov.mod
search_direction=forward
p_forward=0.05

continuous_covariates=WGT,APGR
categorical_covariates=CVD1

logit=CL

;;Let's assume that we want the following
;;         	Start at	To be tested
;; CL-WGT	none		linear (since CL is a logit we only want to test linear inclusion)
;; CL-APGR 	none		linear (since CL is a logit we only want to test linear inclusion)
;; V-APGR	none		linear and then power if linear is found significant
;; V-WGT	linear		exponential
;; V-CVD1	none		linear
;;Below is how to achieve this

[test_relations]
CL=WGT,APGR
V=WGT,APGR,CVD1

;;The first valid state must always be 1
[valid_states]
continuous = 1,2,5
categorical = 1,2

[code]
;;Never redefine state 1
;;only linearly included covariates should be considered for logit 
;;transformed parameters, not any other function form.
;;We redefine state 2 to be 'none/not included' for any covariate on CL, and
;;state 5 to be linear. 
;;We redefined state 5 to be exponential instead of the default power for V-WGT
CL:*-2=none
CL:*-5=hockey-stick
V:WGT-5=exponential

;;Setting included_relations forces inclusion of some relations. 
;;These relations should never be manually added to 
;;the input model, scm will add them.
;;[included_relations] is required when search_direction=backward, 
;;otherwise there are no relations to remove
;;Since in this example we have redefined state 2 to 'none' 
;;for CL-APGR and CL-WGT it is essential to set included_relations
;;to 2. Otherwise scm would test and compute p-value for changing the state 
;;from 1 to 2, i.e. from 'none' to 'none', which will of course never give
;;a significant improvement of the model fit.
;;In this example we also want include V-WGT in state 2. Since state 2 
;;for V-WGT 
;;still has the default meaning, this means we 
;;are including WGT linearly on V 

[included_relations]
CL=APGR-2,WGT-2
V=WGT-2


