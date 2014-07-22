$PROB Repeated Time To Event (RTTE) + Ordered Categorical Data
$INPUT ID TIME ODV DOSE ICL IV IKA TYPE SMAX SMXH THR CAV CAVH CON
       CNT=DROP CNT2=DROP CNT3=DROP DV=HC HC2=DROP HC3=DROP FE EVID

$DATA data.csv IGNORE=@ ACCEPT=(THR.GT.0)

$MSFI msf67 NPOPETAS=1

$PRED
  ;Baseline
 BASE=THETA(1)*EXP(ETA(1))
 C50=THETA(2)
 ;Lambda
 LAMB=BASE*(1-CAVH/(CAVH+C50))

  ;Simulation block
    IF (ICALL.EQ.4) THEN
         T=0
         N=0
         DO WHILE (T.LT.1)              ;Loop
         CALL RANDOM (2,R)              ;Random number in a uniform distribution
                     T=T-LOG(1-R)/LAMB
                     IF (T.LT.1) N=N+1
         END DO
         DV=N                            ;Incrementation of one integer to the DV
    ENDIF

$SIM (12345) (678910 UNI) ONLYSIM NOPRED NSUB=100 TRUE=FINAL

$TABLE ID TIME NOPRINT ONEHEADER FILE=sdtab67sim
$TABLE ID CAV CAVH CON NOPRINT ONEHEADER FILE=cotab67sim
$TABLE ID DOSE NOPRINT ONEHEADER FILE=catab67sim
$TABLE ID ICL IV IKA NOPRINT ONEHEADER FILE=patab67sim
