$PROBLEM    STUDY DESIGN COURSE
$INPUT      ID DOSE TIME DV ARM CRCL MDV1 MDV2
;ID    - 90 subjects (30 per arm)
;DOSE  - 3 dose s: 0, 5 and 30 mg daily doses 
;TIME  - 4 observation times: 0,1,2,3
;DV    - response variable 
$DATA       data1.csv IGNORE=@ IGNORE(ID.GE.4)
$PRED
;Sim_start
;  BASELINE    = THETA(1) +ETA(1)*0 ;note ETAs are needed
;  PLACEBO_MAX = THETA(2)    
;  PLACEBO_HL  = THETA(3) 
;  SLP         = THETA(4)
;  PLACEBO     = PLACEBO_MAX*(1-EXP(-LOG(2)/PLACEBO_HL*TIME)) 
;  IPRED       = BASELINE + PLACEBO + SLP*DOSE 
;  Y           = IPRED + EPS(1)*1E-16 ;some error is needed
;$SIML (987987)
  BASELINE    = THETA(1)*EXP(ETA(1))  
  PLACEBO_MAX = THETA(2)* (1+ETA(2))    
  PLACEBO_HL  = THETA(3) 
  SLP         = THETA(4)*EXP(ETA(3))
  PLACEBO     = PLACEBO_MAX*(1-EXP(-LOG(2)/PLACEBO_HL*TIME)) 
  IPRED       = BASELINE + PLACEBO + SLP*DOSE 
  Y           = IPRED + EPS(1)
$EST METH=1 MSFO=msfb3 MAX=0

;Sim_end  
  

$THETA  97.7078 ; 1_BASELINE
$THETA  22.759 ; 2_PLACEBO_MAX
$THETA  0.745253 ; 3_PLACEBO_HL
$THETA  0.556133 ; 4_SLP
$OMEGA  BLOCK(2)
 0.232539  ; 1_OM_BASELINE
 -0.021345 0.113703  ; 2_OM_PLC_MAX
$OMEGA  0.107531  ;   3_OM_SLP
$SIGMA  127.891  ;    1_SIGMA
