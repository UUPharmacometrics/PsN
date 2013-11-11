$PROBLEM    STUDY DESIGN COURSE - Design of dose finding study
$INPUT      ID DOSE TIME DV ARM CRCL MDV1 MDV2 CFB
;ID    - 90 subjects (30 per arm)
;DOSE  - 3 doses: 0, 5 and 30 mg daily doses 
;TIME  - 4 observation times: 0,1,2,3
;DV    - response variable (continuous)
$DATA     data1.csv IGNORE=@
$PRED

  BASELINE    = THETA(1)*EXP(ETA(1))  
  PLACEBO_MAX = THETA(2)* (1+ETA(2))     
  PLACEBO_HL  = THETA(3)
  EMAX        = THETA(4)*EXP(ETA(3))
  ED50        = THETA(5)
  PLACEBO     = PLACEBO_MAX*(1-EXP(-LOG(2)/PLACEBO_HL*TIME)) 
  IPRED       = BASELINE + PLACEBO + EMAX*DOSE/(DOSE+ED50)
  Y           = IPRED + EPS(1)  

;---to be used in later exercise----
IF(ICALL.EQ.4) THEN 
IF(TIME.EQ.0) BDV=Y
CFB=Y-BDV
ENDIF
;------------------------------------
$THETA  (0,100)       ;1_BASELINE
$THETA   25          ;2_PLACEBO_MAX
$THETA    (0,1)      ;3_PLACEBO_HL
$THETA   (0,25)      ;4_EMAX
$THETA   (0,20,1000) ;5_ED50

$OMEGA  BLOCK(2)
 0.25                ;1_OM_BASELINE
-0.10  0.25          ;2_OM_PLC_MAX
$OMEGA 0.25          ;4_OM_EMAX
$SIGMA 100           ;1_SIGMA
$EST METH=1 MAX=99999
$COV
$TABLE ID ARM DOSE TIME CFB NOPRINT FILE=mytab10


