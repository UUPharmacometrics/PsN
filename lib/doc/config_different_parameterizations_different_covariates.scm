;;config_different_parameterizations_different_covariates.scm
;;lines starting with ; are comments
;;there is no file run34.mod in the extra material, this example config file is for a hypothetical model file

model=run34.mod
directory=scm_run34
search_direction=forward
p_forward=0.05

continuous_covariates=WGT,APGR
categorical_covariates=SEX

logit=BIO

;;scm was not originally designed to allow different parameterizations for different
;;covariates, but this is possible to work around.

;;The general strategy is as follows:
;;In [valid_states] set the list for continuous and categorical to suit the parameter-covariate pair
;;with *the largest number* of parameterizations you want to test. Always start with 1 for not included.
;;State 1 must never be redefined.
;;Then go through each parameter-covariate pair. If the number of parameterizations equals the
;;number in [valid_states], only change the meaning of the state digits if necessary,
;;like below where the meaning of state 5 for V:WGT is changed to exponential.
;;If the number of parameterizations is smaller than in [valid_states], set
;;the N superfluous leading states, after the initial state 1, in [valid_states] to 'none', 
;;as below where there is one state
;;too many for BIO so therefore the second state is changed to none. The remaining states
;;are redefined as desired, like below where state 5 is redefined to linear for all covariates
;;on BIO.
;;If a number of leading states have been set to none, set [included_relations] to the last
;;of the states that is 'none', as below where included_states is BIO=APGR-2,WGT-2

;;The example above only uses the pre-defined parameterizations, but it is of course 
;;possible to define and use new parameterizations.

;;An example:
;;Let's assume that we want the following
;;         	Start at	To be tested
;; BIO-WGT	none		linear (since BIO is a logit we only want to test linear inclusion)
;; BIO-APGR 	none		linear (since BIO is a logit we only want to test linear inclusion)
;; V-APGR	none		linear and then power if linear is found significant
;; V-WGT	none		linear and then exponential if linear is found significant
;; V-SEX	none		linear
;;Below is how to achieve this

[test_relations]
BIO=WGT,APGR
V=WGT,APGR,SEX

;;The first valid state must always be 1
[valid_states]
continuous = 1,2,5
categorical = 1,2

[code]
;;Never redefine state 1
;;only linearly included covariates should be considered for logit 
;;transformed parameters, not any other function form.
;;We redefine state 2 to be 'none/not included' for any covariate on BIO, and
;;state 5 to be linear. 
;;We redefined state 5 to be exponential instead of the default power for V-WGT
BIO:*-2=none
BIO:*-5=linear
V:WGT-5=exponential

;;Setting included_relations forces inclusion of some relations. 
;;These relations should never be manually added to 
;;the input model, scm will add them.
;;[included_relations] is required when search_direction=backward, 
;;otherwise there are no relations to remove
;;Since in this example we have redefined state 2 to 'none' 
;;for BIO-APGR and BIO-WGT it is essential to set included_relations
;;to 2. Otherwise scm would test and compute p-value for changing the state 
;;from 1 to 2, i.e. from 'none' to 'none', which will of course never give
;;a significant improvement of the model fit.

[included_relations]
BIO=APGR-2,WGT-2


