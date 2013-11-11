$PROBLEM    STUDY DESIGN COURSE
$INPUT      ID DOSE TIME DV ARM CRCL XMDV=DROP XMDV2=DROP
;ID    - 90 subjects (30 per arm)
;DOSE  - 3 doses: 0, 5 and 30 mg daily doses 
;TIME  - 4 observation times: 0,1,2,3 
;DV    - response variable 
;CRCL  - not used in model - for later discussion
;XMDV1  - not used in model
;XMDV2  - not used in model
$DATA       data5.csv IGNORE=@ 
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
$THETA  97.7078 ; 1_BASELINE
$THETA  22.759 ; 2_PLACEBO_MAX
$THETA  0.745253 ; 3_PLACEBO_HL
$THETA  0.556133 ; 4_SLP
$OMEGA  BLOCK(2)
 0.232539  ; 1_OM_BASELINE
 -0.021345 0.113703  ; 2_OM_PLC_MAX
$OMEGA  0.10753  ;   3_OM_SLP
$SIGMA  127.891  ;    1_SIGMA
$EST METH=1 MSFO=msfb5 MAX=99999
$COV MATRIX=R
$TABLE ID DOSE TIME DV CFB BASE_DV CWRES NPDE NOAPPEND NOPRINT FILE=mytab5
