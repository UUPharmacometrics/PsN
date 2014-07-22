$PROBLEM    PHENOBARB additive model
;$ABBREVIATED COMRES=2
$INPUT      ID TIME AMT WT APGR DV
$DATA       PHENO.dta IGNORE=@
$SUBROUTINE ADVAN1 TRANS2
$OMEGA  0.228  ; variance for ETA(1), initial estimate
$OMEGA  0.146  ; variance for ETA(2), initial estimate
$PK


  TVCL = THETA(1) ; typical value of CL
  TVV  = THETA(2)
  CL   = TVCL*EXP(ETA(1)) ; individual value of CL
  V    = TVV*EXP(ETA(2))  ; individual value of V
  S1   = V

$ERROR


  IPRED  = F       ; individual predicion
  IRES   = DV - F   ; individual residual
  W      = THETA(3)       ; additive residual error
  IF(W.EQ.0) W = 1
  IWRES  = IRES/W   ; individula weighed residual
  Y      = IPRED + W*EPS(1)

$THETA  (0,.005) ; 1. TVCL (lower bound,initial estimate)
$THETA  (0,1.45) ; 2. TVV  (lower bound,initial estimate)
$THETA (0, 5)
$SIGMA  1 FIX  ; initial estimate
$ESTIMATION METHOD=1 MAXEVAL=9999 ; FOCE calculation method
;$TABLE      ID DV MDV IPRED=OPRED W G11 G21 ETA1 ETA2 APGR WT NOPRINT
;            NOAPPEND ONEHEADER FILE=derivatives_covariates.dta

