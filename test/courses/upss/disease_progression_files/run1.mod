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

$DATA data1.csv IGNORE=# IGN=(DOSE.GT.0) ;only placebo data are used
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
 BASE  =THETA(1)*EXP(IOV+ETA(4)) ; Baseline

;---------------- DRUG CONC --------------------------------
  CP    =A(2)/S2                ; Predicted drug concentration
                                ; ADVAN2 -> Central comp is CMT=2

;---------------- MODEL & PREDICTION NA ----------------------
 DS = BASE                      ; Disease status   

 IPRED =LOG(DS)                 ; Log-transformed DV used
 IRES  =IPRED-DV
 W     =THETA(2)
 IWRES =IRES/W
 Y     =IPRED+ERR(1)*W

;--------------- OUTPUT FOR VPC ------------------------------
 STRT=1                         ;Treatment arm
   IF(DGRP.EQ.0) STRT=2         ;Placebo arm

;----------------- INITIAL ESTIMATES --------------------------
$THETA  (30,396.000)    ; 1 BASELINE
$THETA  (0,0.196000)    ; 2 RV
$OMEGA  BLOCK(1) 0.031  ; 1 IOV BASE
$OMEGA  BLOCK(1) SAME   ; 2 IOV BASE
$OMEGA  BLOCK(1) SAME   ; 3 IOV BASE
$OMEGA  0.138000        ; 4 IIV BASE
$SIGMA  1.000000  FIX
$EST MAXEVALS=9990 METH=0 PRINT=2 POSTHOC MSFO=msfb1
$COV PRINT=E
$TAB ID TVIS IPRED IWRES              ONEHEADER NOPRINT FILE=sdtab1
$TAB ID BASE IOV                      ONEHEADER NOPRINT FILE=patab1
$TAB ID DOSE ICL                      ONEHEADER NOPRINT FILE=cotab1
$TAB ID TIME TAD DS                   ONEHEADER NOPRINT FILE=mytab1
