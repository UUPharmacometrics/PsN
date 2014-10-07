$PROBLEM    STUDY DESIGN COURSE - Design of dose finding study
$INPUT      ID DOSE TIME ODV ARM CRCL MDV MDV2 DV
;ID    - 300 subjects (60 per arm)
;DOSE  - 5 doses: 0, 5, 15, 30 and 60 mg daily doses 
;TIME  - 4 observation times: 0,1,2,3
;DV    - response variable (continuous)
$DATA       data11.csv IGNORE=@ 
$PRED

  BASELINE    = THETA(1) 
  DRUG        = 0
  IF(DOSE.GT.0) DRUG=THETA(2)
  IPRED       = BASELINE +DRUG
  Y           = IPRED + ERR(1)  

$THETA  (0,20)       ;1_BASELINE
$THETA  (0,10)       ;1_BASELINE

$OMEGA  100           ;1_SIGMA

$EST METH=0 MAX=99999
$COV
$TABLE ID ARM DOSE TIME NOPRINT FILE=mytab16


