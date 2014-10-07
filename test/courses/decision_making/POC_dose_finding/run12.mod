$PROBLEM    STUDY DESIGN COURSE - Design of dose finding study
$INPUT      ID DOSE TIME DV ARM CRCL MDV1 MDV2
;ID    - 300 subjects (60 per arm)
;DOSE  - 5 doses: 0, 5, 15, 30 and 60 mg daily doses 
;TIME  - 4 observation times: 0,1,2,3
;DV    - response variable (continuous)
$DATA       data11.csv IGNORE=@
$PRED

  BASELINE    = THETA(1)*EXP(ETA(1))  
  PLACEBO_MAX = THETA(2)* (1+ETA(2))     
  PLACEBO_HL  = THETA(3)
  SLP        = THETA(4)*EXP(ETA(3))
  PLACEBO     = PLACEBO_MAX*(1-EXP(-LOG(2)/PLACEBO_HL*TIME)) 
  IPRED       = BASELINE + PLACEBO + SLP*DOSE
  Y           = IPRED + EPS(1)  

$THETA  (0,100)      ;1_BASELINE
$THETA   25          ;2_PLACEBO_MAX
$THETA    (0,1)      ;3_PLACEBO_HL
$THETA   (0,.5)      ;4_SLP


$OMEGA  BLOCK(2)
 0.25                ;1_OM_BASELINE
-0.10  0.25          ;2_OM_PLC_MAX
$OMEGA 0.25          ;4_OM_SLP
$SIGMA 100           ;1_SIGMA

$EST METH=1 MAX=99999
;$COV
;$TABLE ID ARM DOSE TIME NOPRINT FILE=mytab12


