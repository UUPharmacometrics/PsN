
$PROBLEM  PHENOBARB SIMPLE MODEL
$INPUT  ID TIME AMT WGT APGR DV FA1 FA2
$DATA  ../pheno.dta IGNORE=@
$SUBROUTINE  ADVAN1 TRANS2
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

$THETA  (0,0.0105)    ; CL ; unit

$THETA  (0,1.0500)    ; V ; unit

$OMEGA            .4  ; IVCL
                  .25 ; IVV

$SIGMA            .04                         

$ESTIMATION  MAXEVALS=9997 SIGDIGITS=4 POSTHOC MSFO=phenomsf
$COVARIANCE  

;$TABLE ID TIME IPRED IWRES NOPRINT ONEHEADER FILE=sdtab5.dat
;$TABLE ID CL ETA1 ETA2 NOPRINT NOAPPEND ONEHEADER FILE=patab5.dat
;$TABLE ID WGT APGR NOPRINT NOAPPEND ONEHEADER FILE=cotab5.dat



