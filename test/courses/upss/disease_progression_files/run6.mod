$PROB MOXONIDINE NA ANALYSIS
$INPUT ID              ; Consecutive 1-97
VISI                   ; Visit 3 (Week 0), 6 (Week 4) or 8 (Week 12)
DGRP                   ; Dose arm (0, 1, 2, 3) 
DOSE                   ; Actual daily dose (0, 200, 400, 600)
TIME                   ; Time (h) since first dose in trial
CLOC                   ; CLOC=24-h decimal clock time
TVIS                   ; Time for plots, visit 3, 6, 8 starts at TVIS=0, 12, 24
TAD                    ; Time after latest dose
DV AMT SS II XCMT EVID ; Standard input
ICL IV IKA ILAG        ; Individual PK parameters

$DATA data1.csv IGNORE=# IGN=(DOSE.LE.0) ;only treatment data are used
$ABBREVIATED DERIV2=NO
$SUBROUTINES ADVAN2 TRANS1

$PK
CL    =ICL           ; Individual PK parameters in model file
V     =IV
KA    =IKA
ALAG1 =ILAG
K     =CL/V
S2    =V

$ERROR
;---------------- UNDERLYING DISEASE -------------
    IF(VISI.EQ.3) IOV=ETA(1)     ; Week 0
    IF(VISI.EQ.6) IOV=ETA(2)     ; Week 4
    IF(VISI.EQ.8) IOV=ETA(3)     ; Week 12
 BASE = THETA(1)*EXP(IOV+ETA(4)) ; Baseline
 ALPHA = THETA(3)                ; Disease progression rate constant
;---------------- DRUG CONC & EFFECT -------------------------
  CP    =A(2)/S2  
  EMAX  = THETA(4)
  EC50  = THETA(5)*EXP(ETA(5))
  EFF   = EMAX*CP/(EC50+CP)               ; Predicted drug concentration
;---------------- MODEL & PREDICTION NA ----------------------
 DS    = BASE*(1+ALPHA*TIME/24/7)*(1-EFF) ; Disease status

 IPRED = LOG(DS)                  ; Log-transformed DV used
 IRES  = IPRED-DV
 W     = THETA(2)
 IWRES = IRES/W
 Y     = IPRED+ERR(1)*W

;--------------- OUTPUT FOR VPC ------------------------------
 STRT=1                         ;Treatment arm
   IF(DGRP.EQ.0) STRT=2         ;Placebo arm

;----------------- INITIAL ESTIMATES --------------------------
$THETA  (426 FIX)       ; 1 BASELINE
$THETA  (0.204 FIX)     ; 2 RV
$THETA  (0.0218 FIX)    ; 3 ALPHA
$THETA  (0,.5,1)        ; 4 EMAX
$THETA  (0,1,10)        ; 5 EC50
$OMEGA  BLOCK(1) 0.0285 FIX ; 1 IOV BASE
$OMEGA  BLOCK(1) SAME   ; 2 IOV BASE
$OMEGA  BLOCK(1) SAME   ; 3 IOV BASE
$OMEGA  0.115           ; 4 IIV BASE
$OMEGA  0.09            ; 5 IIV EC50
$SIGMA  1.000000  FIX
$EST MAXEVALS=9990 METH=0 PRINT=2 POSTHOC MSFO=msfb5
$COV PRINT=E
$TAB ID TVIS IPRED IWRES              ONEHEADER NOPRINT FILE=sdtab6
$TAB ID BASE IOV                      ONEHEADER NOPRINT FILE=patab6
$TAB ID DOSE ICL                      ONEHEADER NOPRINT FILE=cotab6
$TAB ID TIME TAD DS                   ONEHEADER NOPRINT FILE=mytab6
