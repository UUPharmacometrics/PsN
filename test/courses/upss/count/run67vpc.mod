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

    IF (ICALL.EQ.4) THEN
         T=0
         N=0
         DO WHILE (T.LT.1)
         CALL RANDOM (2,R)
                     T=T-LOG(1-R)/LAMB
                     IF (T.LT.1) N=N+1
         END DO
         DV=N
    ENDIF

$THETA
 (0,7)  ;BASE
 (0,6)        ; D50
$OMEGA
 0.9

$SIM (12345) (678910 UNI) 
$ESTIM MAXEVAL=0 METHOD=COND LAPLACE -2LL PRINT=1
