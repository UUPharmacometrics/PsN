$PROBLEM    PHENOBARB SIMPLE MODEL
$INPUT      ID TIME AMT WGT APGR DV CV1 CV2 CV3 CVD1 CVD2 CVD3
$DATA       pheno_ch.csv IGNORE=@
$SUBROUTINE ADVAN1 TRANS2
$PK
      TVCL  = THETA(1)
	  CL = TVCL*EXP(ETA(1))

      TVV   = THETA(2)
      TVLG1 = LOG(TVV/(1-TVV))
	LG1 = TVLG1+ETA(2)
      V    = 0.001+2*EXP(LG1)/(1+EXP(LG1))

      S1    = V
$ERROR      
      IPRED = F          ;  individual-specific prediction
      IRES  = DV-IPRED   ;  individual-specific residual

      W     = THETA(3)
      IWRES = IRES/W     ;  individual-specific weighted residual

      Y     = IPRED+W*EPS(1)


$THETA  (0,0.0105)     
$THETA  (0.00001,0.99999)     
$THETA  (0,0.4)
$OMEGA  .4
        .25
$SIGMA  1 FIX
$ESTIMATION MAXEVAL=9999 SIGDIGITS=4 METHOD=ZERO
;$COVARIANCE PRINT=E
;$TABLE ID TIME AMT WGT APGR DV NOPRINT FILE=sdtab2

