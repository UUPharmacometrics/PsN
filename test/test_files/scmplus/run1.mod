$PROBLEM    PHENOBARB SIMPLE MODEL
$INPUT      ID TIME AMT WGT APGR DV
$DATA      pheno.dta IGNORE=@ IGNORE(WGT.EQ.23)
$SUBROUTINE ADVAN1 TRANS2
$PK

    ETA_CL = ETA(1)
    ETA_V = ETA(2)

      TVCL=THETA(1)
      TVV=THETA(2)
      CL=TVCL*EXP(ETA(1))
      V=TVV*EXP(ETA(2))
      S1=V
      CENSOR=0
$ERROR

      W=F
      Y=F+W*EPS(1)

      IPRED=F         ;  individual-specific prediction
      IRES=DV-IPRED   ;  individual-specific residual
      IWRES=IRES/W    ;  individual-specific weighted residual

$THETA  (0,0.01050101010101) ; TVCL
$THETA  (0,1.0500) ; TVV
$OMEGA  .4  ;       IVCL
 .25  ;        IVV
$SIGMA  .04
$ESTIMATION METHOD=1 MAX=9999 SIGDIGITS=4 POSTHOC MSFO=phenomsf
$COVARIANCE PRINT=E
$TABLE      NOPRINT ID CL V ETA_CL ETA_V NOAPPEND FILE=patab
$TABLE      ID TIME CPRED CL DV MDV PRED IPRED RES IRES WRES IWRES CWRES NPDE
            NOPRINT NOAPPEND FILE=sdtab

