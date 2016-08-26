;; 1. Based on: 2
$PROBLEM    PHENOBARB SIMPLE MODEL
$INPUT      ID TIME AMT WGT APGR DV
$DATA      ../pheno.dta IGNORE=@
$SUBROUTINE ADVAN1 TRANS2
$PK   

      CLWGT = ( 1 + THETA(3)*(WGT - 1.3))
      TVCL=THETA(1)
      TVCL = CLWGT*TVCL
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

$THETA  (0,0.00563997) ; CL
$THETA  (0,1.35134) ; V
$THETA  (-0.435,1.13,1.429) ; CLWGT
$OMEGA  0.0487546  ;       IVCL
 0.129957  ;        IVV
$SIGMA  0.0174894
$ESTIMATION METHOD=COND MAXEVALS=9997
$COVARIANCE PRINT=E
$TABLE      ID CL V ETA1 ETA2 NOPRINT ONEHEADER FILE=patab3
$TABLE      ID TIME IPRED IWRES CWRES NOPRINT ONEHEADER FILE=sdtab3
$TABLE      ID WGT APGR NOPRINT ONEHEADER FILE=cotab3

