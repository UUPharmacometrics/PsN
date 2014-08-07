$PROB Repeated Time To Event (RTTE) + Ordered Categorical Data
$INPUT ID TIME ODV DOSE ICL IV IKA TYPE SMAX SMXH THR CAV CAVH CON
       CNT=DROP CNT2=DROP CNT3=DROP DV=HC HC2=DROP HC3=DROP FE EVID

$DATA data.csv IGNORE=@ ACCEPT=(THR.GT.0)

$PRED
  ;Baseline
 BASE=THETA(1)*EXP(ETA(1))
 C50=THETA(2)
 ;Lambda
 LAMB=BASE*(1-CAVH/(CAVH+C50))

 TP00=THETA(3)                         ;probability of having zero count
 PHI=LOG(TP00/(1-TP00))                ;logit transformation
 P0=EXP(PHI+ETA(2))/(1+EXP(PHI+ETA(2)))
 P1=1-P0

  ;Approximation of the factorial (log scale)
 LFAC=DV*LOG(DV)-DV+LOG(DV*(1+4*DV*(1+2*DV)))/6+LOG(3.1415)/2
 IF(DV.EQ.0) LFAC=0

            Y=-2*LOG(P1*EXP(-LAMB+DV*LOG(LAMB)-LFAC)) ;Poisson model
IF(DV.EQ.0) Y=-2*LOG(P0+P1*EXP(-LAMB))                  ;zero inflation

$THETA
 (0,7)  ;BASE
 (0,6)        ; D50
 (0.00001,0.1,1)  ; P0
$OMEGA 0.09 0.1

$ESTIM MAXEVAL=9999 METHOD=COND LAPLACE -2LL PRINT=1 MSFO=msf70
$COV PRINT=E

$TABLE ID TIME NOPRINT ONEHEADER FILE=sdtab70
$TABLE ID CAV CAVH CON NOPRINT ONEHEADER FILE=cotab70
$TABLE ID DOSE NOPRINT ONEHEADER FILE=catab70
$TABLE ID ICL IV IKA NOPRINT ONEHEADER FILE=patab70
