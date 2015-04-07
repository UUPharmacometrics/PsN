
$PROBLEM  PHENOBARB SIMPLE MODEL
$INPUT  ID TIME AMT WGT APGR DV
$DATA  pheno.dta IGNORE=@
$SUBROUTINE  ADVAN1 TRANS2
$PK

      TVCL=THETA(1)
      TVV=THETA(2)
      CL=TVCL*EXP(ETA(1))* ( 1 + THETA(3)*(WGT - 1.30))
      V=TVV*EXP(ETA(2))
      S1=V
$ERROR

      W=F
      Y=F+W*EPS(1)

      IPRED=F         ;  individual-specific prediction
      IRES=DV-IPRED   ;  individual-specific residual
      IWRES=IRES/W    ;  individual-specific weighted residual

$THETA  (0,0.0105)    ; 

$THETA  (0,1.0500)    ; 

$THETA  (-0.434,1.12,1.428) ; CLWGT

$OMEGA            .4  ; IVCL
                  .25 ; IVV

$SIGMA            .04                         

$ESTIMATION  MAXEVALS=9997 SIGDIGITS=4 POSTHOC MSFO=phenomsf
$COVARIANCE  PRINT=E
$TABLE ID TVCL TVV ETA1 ETA2 NOPRINT NOAPPEND ONEHEADER FILE=patab1
$TABLE ID TIME IPRED IWRES NOPRINT ONEHEADER FILE=sdtab1
$TABLE Id WGT APGR NOPRINT NOAPPEND ONEHEADER FILE=cotab1

