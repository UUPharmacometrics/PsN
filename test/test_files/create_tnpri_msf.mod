$PROBLEM    STUDY DESIGN COURSE
$INPUT      ID DOSE TIME DV ARM CRCL
;ID    - 90 subjects (30 per arm)
;DOSE  - 3 doses: 0, 5 and 30 mg daily doses 
;TIME  - 4 observation times: 0,1,2,3 
;DV    - response variable 
;CRCL  - not used in model - for later discussion
;MDV1  - not used in model - for later diagnostic use
;MDV2  - not used in model - for later diagnostic use
$DATA       data_tnpri.csv IGNORE=@
$PRED

  BASELINE    = THETA(1)*EXP(ETA(1))  
  PLACEBO_MAX = THETA(2)* (1+ETA(2)) ;pos or neg on ind. level
  PLACEBO_HL  = THETA(3) 
  SLP         = THETA(4)*EXP(ETA(3))
  PLACEBO     = PLACEBO_MAX*(1-EXP(-LOG(2)/PLACEBO_HL*TIME)) 
  IPRED       = BASELINE + PLACEBO + SLP*DOSE 
  Y           = IPRED + EPS(1)

; Change from baseline (CFB) for diagnostics 
   IF(TIME.EQ.0) BASE_DV=DV
   CFB         = DV - BASE_DV

 IF(ICALL.EQ.4) THEN
    IF(TIME.EQ.0) BASE_DV=IPRED + EPS(1)
   CFB         = IPRED + EPS(1) - BASE_DV
 ENDIF
  
$THETA  100 ;1_BASELINE
$THETA   25 ;2_PLACEBO_MAX
$THETA    1 ;3_PLACEBO_HL
$THETA    1 ;4_SLP


$OMEGA  BLOCK(2)
 0.25                ;1_OM_BASELINE
-0.10  0.25          ;2_OM_PLC_MAX
$OMEGA 0.25          ;3_OM_SLP
$SIGMA 100           ;1_SIGMA

$EST METH=1 MSFO=msf_tnpri MAX=99999
$COVARIANCE MATRIX=R


