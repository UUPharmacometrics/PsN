$PROB MEM Binary Data analysis, Baseline model

$INPUT ID TIME ODV DOSE ICL IV IKA 
       TYPE SMAX DV=SMXH THR CAV CAVH CON   
       ;CNT CNT2 CNT3 HC HC2 HC3 FE EVID 
       ;ETA1 ETA2 ETA3 ETA4 

$DATA data.csv IGNORE=@ ACCEPT=(THR.GT.0)
  ; THR to filter the data to be used for binary data 
  ; CAVH is average concentration during time interval
  ; SMXH is the data varaible to be used here, merged to 0 and 1 in code

$PRED
  TVBASE = THETA(1)                      ; TVBASE is a proportion
  PHI    = LOG(TVBASE/(1-TVBASE)) + ETA(1)  
  BASE   = EXP(PHI)/(1+EXP(PHI))   ; Logit transformation, 0<BASE<1

  IF(DV.GT.1) THEN   ; A reflux event for binary data
    Y=BASE
    RDV = 1     ; the "real" DV
  ENDIF
  IF(DV.LE.1) THEN   ; No reflux event for binary data
    Y=1-BASE
    RDV = 0     ; the "real" DV
  ENDIF

$THETA (0,.8)    ; BASE
$OMEGA 0.1       ; BSV BASE

$ESTIM MAXEVAL=9990 METHOD=COND LAPLACE LIKE PRINT=1 MSFO=msfb36
$COV PRINT=E

$TABLE ID TIME RDV NOPRINT ONEHEADER FILE=sdtab36
$TABLE ID CAV CAVH CON NOPRINT ONEHEADER FILE=cotab36
$TABLE ID DOSE NOPRINT ONEHEADER FILE=catab36
$TABLE ID ICL IV IKA NOPRINT ONEHEADER FILE=patab36


