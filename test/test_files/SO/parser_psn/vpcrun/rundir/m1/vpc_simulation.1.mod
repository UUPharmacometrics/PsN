$PROBLEM    PHENOBARB SIMPLE MODEL
$INPUT      ID TIME AMT WGT APGR DV
$DATA      ../../pheno5.dta IGNORE=@
$SUBROUTINE ADVAN1 TRANS2
$PK 

      TVCL=THETA(1)
      TVV=THETA(2)
      CL=TVCL*EXP(ETA(1))
      V=TVV*EXP(ETA(2))
      S1=V
$ERROR 

      W=F
      Y=F+W*EPS(1)

      IPRED=F         ;  individual-specific prediction
      IRES=DV-IPRED   ;  individual-specific residual
      IWRES=IRES/W    ;  individual-specific weighted residual

$THETA  (0,0.00478) ; CL
$THETA  (0,1.42) ; V
$OMEGA  0.183  ;       IVCL
 0.0825  ;        IVV
$SIGMA  0.00738
$SIMULATION (932139285) ONLYSIMULATION NSUBPROBLEMS=20
$TABLE      ID MDV TIME DV ONEHEADER NOPRINT NOAPPEND FILE=npctab.dta

