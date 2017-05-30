$PROB Count Data
$INPUT ID TIME ODV DOSE ICL IV IKA TYPE SMAX SMXH THR CAV CAVH CON
       CNT=DROP CNT2=DROP CNT3=DROP DV=HC HC2=DROP HC3=DROP FE EVID

$DATA data.csv IGNORE=@ ACCEPT=(THR.GT.0)

$PRED
  ;Baseline
 BASE=THETA(1)*EXP(ETA(1))
  ;Lambda
 LAMB=BASE

;Sim_start
;  ;Approximation of the factorial (log scale)
 LFAC=DV*LOG(DV)-DV+LOG(DV*(1+4*DV*(1+2*DV)))/6+LOG(3.1415)/2
 IF(DV.EQ.0) LFAC=0

;  ;Logarithm of the Poisson distribution
 LPOI = -LAMB+DV*LOG(LAMB)-LFAC
;  ;-2 Log Likelihood
 Y=-2*LPOI

;  ;Simulation block
;    IF (ICALL.EQ.4) THEN
;         T=0
;         N=0
;         DO WHILE (T.LT.1)              ;Loop
;         CALL RANDOM (2,R)              ;Random number in a uniform distribution
;                     T=T-LOG(1-R)/LAMB
;                     IF (T.LT.1) N=N+1
;         END DO
;         DV=N                            ;Incrementation of one integer to the DV
;    ENDIF

;Sim_end

$THETA
 (0,7)  ;BASE
$OMEGA
 0.9

;Sim_start
;$SIM (12345) (678910 UNI) ONLYSIM NOPRED NSUB=100
$ESTIM MAXEVAL=9999 METHOD=COND LAPLACE -2LL PRINT=1 MSFO=msf65
$COV PRINT=E
;Sim_end

$TABLE ID TIME NOPRINT ONEHEADER FILE=sdtab65
$TABLE ID CAV CAVH CON NOPRINT ONEHEADER FILE=cotab65
$TABLE ID DOSE NOPRINT ONEHEADER FILE=catab65
$TABLE ID ICL IV IKA NOPRINT ONEHEADER FILE=patab65
