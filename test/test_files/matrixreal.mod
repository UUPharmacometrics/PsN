
$PROBLEM  PHENOBARB SIMPLE MODEL
$INPUT  ID TIME AMT WGT APGR DV FA1=DROP FA2=DROP
$DATA  pheno.dta IGNORE=@
$SUBROUTINE  ADVAN1 TRANS2
$PK

      TVCL=THETA(1)
      TVV=THETA(2)
      CL=TVCL*EXP(THETA(3)*ETA(1))
      V=TVV*EXP(THETA(4)*ETA(2))
      S1=V
$ERROR

      W=F
      Y=F+W*EPS(1)*THETA(5)

      IPRED=F         ;  individual-specific prediction
      IRES=DV-IPRED   ;  individual-specific residual
      IWRES=IRES/W    ;  individual-specific weighted residual

$THETA  (0,0.0105)    ; CL 

$THETA  (0,1.0500)    ; V
$THETA  (0,0.65)    
$THETA  (0,0.5)   
$THETA  (0,0.2)   

$OMEGA            1 FIX
$OMEGA            1 FIX

$SIGMA            1 FIX                         

$ESTIMATION  MAXEVALS=9997 SIGDIGITS=4 POSTHOC
$COVARIANCE  PRINT=E


