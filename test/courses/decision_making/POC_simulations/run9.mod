$PROBLEM    STUDY DESIGN COURSE
$INPUT      ID DOSE TIME DV ARM CRCL MDV1 MDV2
;ID    - 3000 subjects (1000 per arm)
;DOSE  - 3 doses: 0, 5 and 30 mg daily doses 
;TIME  - 4 observation times: 0,1,2,3
;DV    - response variable ;note output will be MEAN response
;ARM   - treatment arm (0, 5, 30 mg)
;CRCL  - not used
;MDV1  - not used
;MDV   - 0 final record last ID for 5 and 30 mg ARM, 1 otherwise
$DATA       data5.csv IGNORE=@ 
$PRIOR TNPRI (PROBLEM 2) PLEV=.9999 IVAR=0
;PLEV=.9999 means that the 0.01% most extreme parameter vectors 
;     are rejected
;IVAR=0 means that the msfb1 file contains the R matrix
$MSFI msfb1 ONLYREAD
$PRED

  BASELINE    = THETA(1)*EXP(ETA(1))  
  PLACEBO_MAX = THETA(2)* (1+ETA(2))     
  PLACEBO_HL  = THETA(3) 
  SLP         = THETA(4)*EXP(ETA(3))
  PLACEBO     = PLACEBO_MAX*(1-EXP(-LOG(2)/PLACEBO_HL*TIME)) 
  IPRED       = BASELINE + PLACEBO + SLP*DOSE 
  Y           = IPRED+EPS(1)

;Note code below not necessary for producing a VPC with uncertainty
IF(ICALL.EQ.4) THEN 
IF(NEWIND.EQ.0) WRITE(50,*) THETA(1),THETA(2),THETA(3),THETA(4),OMEGA(1,1),OMEGA(1,2),OMEGA(2,2),OMEGA(3,3),SIGMA(1,1)  

  IF(NEWIND.EQ.0) THEN
   M_0_0=0
   M_0_3=0
   M_5_0=0
   M_5_3=0
   M_30_0=0
   M_30_3=0
   NFLG=1000   ;no of subjects per ARM
  ENDIF

   IF(ARM.EQ.0.AND.TIME.EQ.0) M_0_0= Y+M_0_0  ;sum of responses
   IF(ARM.EQ.0.AND.TIME.EQ.3) M_0_3= Y+M_0_3;sum of responses

   IF(ARM.EQ.5.AND.TIME.EQ.0) M_5_0= Y+M_5_0
   IF(ARM.EQ.5.AND.TIME.EQ.3) M_5_3= Y+M_5_3
   IF(ARM.EQ.30.AND.TIME.EQ.0) M_30_0= Y+M_30_0
   IF(ARM.EQ.30.AND.TIME.EQ.3) M_30_3= Y+M_30_3
   IF(ARM.EQ.5.AND.TIME.EQ.3)  Y5=(M_5_3-M_5_0-(M_0_3-M_0_0))/NFLG
   IF(ARM.EQ.30.AND.TIME.EQ.3)  Y30=(M_30_3-M_30_0-(M_0_3-M_0_0))/NFLG
IF(MDV2.EQ.0.AND.ARM.EQ.30.AND.TIME.EQ.3) WRITE(60,*) Y5,Y30

ENDIF

$PROBLEM 2 
$INPUT      ID DOSE TIME DV ARM CRCL MDV1 MDV2
;ID    - 3000 subjects (1000 per arm)
;DOSE  - 3 doses: 0, 5 and 30 mg daily doses 
;TIME  - 4 observation times: 0,1,2,3
;DV    - response variable 
;ARM   - treatment arm (0, 5, 30 mg)
;CRCL  - not used
;MDV1  - not used
;MDV   - 0 for final record for last ID for 5&30 mg ARM, else 1
$DATA       data5.csv IGNORE=@ REWIND 
$THETA  98.7078  ; 1_BASELINE
$THETA  22.759   ; 2_PLACEBO_MAX
$THETA  0.745253 ; 3_PLACEBO_HL
$THETA  0.556133 ; 4_SLP
$OMEGA  BLOCK(2)
 0.232539  ; 1_OM_BASELINE
 -0.021345 0.113703  ; 2_OM_PLC_MAX
$OMEGA  0.107531  ;   3_OM_SLP
$SIGMA  127.891  ;    1_SIGMA
$SIML (98798) NSUB=100 ONLY TRUE=PRIOR
; NSUB=100 - 100 studies to be simulated
; TRUE=PRIOR necessary to specify for performed with varying
;            population parameter vector for each study

