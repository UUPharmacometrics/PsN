$PROBLEM    Tumor growth inhibition and overall survival model
$INPUT      ID DAY WEEK=TIME CMT DV EVID AMT ADDL II ARM CAPD FLG NTTE
            NEVENT NEVID NECOG NMETA NTSR6 NBASE TEST2 TEST3 TEST4 TEST5 DID

      ; FLG, NTTE, NEVENT, and NEVID are placeholders and flag for TTE "alternate" SSE model...N is "New"
      ; idea is for TTE model to filter by FLG, and NTTE, NEVENT, and NEVID are TIME, DV, and EVID, respectively,
      ; in $INPUT of alt model

; TIME: in weeks (units are given in /week)
; CMT:  compartment
; DV:   sum of longest diameters
; EVID: event identification variable
; AMT:  drug amount, adjusted to BSA
; ADDL: additional doses
; II:   dosing interval
; ARM:  either cohort 1 or 2 --for TTE model; cohort 2 is reference arm for hazard ratio
; CAPD: capecitabine dose

$DATA data_20131014.csv IGNORE=#

$SUBROUTINE ADVAN6 TOL=4
$MODEL      NCOMPS=3 COMP=('DOCE') COMP=('CAPE') COMP=('TUMOR' DEFOBS)

$PK 

; ~.~.~.~. Parameters for the tumor model .~.~.~.~
BASE = THETA(9) * EXP(ETA(6)) ; tumor size at baseline
KL   = THETA(1) * EXP(ETA(1)) ; tumor growth rate
KD1  = THETA(2) * EXP(ETA(2)) ; constant cell kill rate for docetaxel
KD2  = THETA(3) * EXP(ETA(3)) ; constant cell kill rate for capecitabine
DM1  = THETA(4) * EXP(ETA(4)) ; decrease rate constant for docetaxel
DM2  = THETA(5) * EXP(ETA(5)) ; decrease rate constant for capecitabine
REE  = SQRT(THETA(6))         ; SD for residual error
KP1  = THETA(7)               ; elimination rate constant from virtual biophase compartment for docetaxel
KP2  = THETA(8)               ; elimination rate constant from virtual biophase compartment for capecitabine

; ~.~.~.~. Initial conditions .~.~.~.~
A_0(1) = 0                    ; Docetaxel compartment
A_0(2) = 0                    ; Capecitabine compartment
A_0(3) = BASE                 ; Tumor compartment

; ~.~.~.~. Overall survival model .~.~.~.~
B0   =  THETA(10)             ; Intercept
B1   =  THETA(11)             ; Baseline tumor size
B2   =  THETA(12)             ; Fractional change un tumor size at week 6
B3   =  THETA(13)             ; ECOG
B4   =  THETA(14)             ; Number of metastasis


$DES 

DADT(1)= - KP1 * A(1)         ; Docetaxel compartment
DADT(2)= - KP2 * A(2)         ; Capecitabine compartment

A1=A(1)
A2=A(2)

;TTD = TFD
;IF (TFD.LT.0) TTD = 0

DR1 = A(1) * KP1 * EXP(-DM1*TIME)
DR2 = A(2)/1000 * KP2 * EXP(-DM2*TIME)

DADT(3) = KL*A(3) - KD1*DR1*A(3) - KD2*DR2*A(3)       ; Tumor compartment

A3 =A(3)

$ERROR 

; ~.~.~.~. Tumor prediction .~.~.~.~
Y = F + REE *  ERR(1)
IPRE=F
IRES = DV - IPRE
IWRES=(DV-IPRE)/REE

; ~.~.~.~. Tumor size ratio at week 6 .~.~.~.~

IF(TIME.EQ.0)THEN
TS0 = A(3)
ELSE
TS0=TS0
ENDIF

IF(TIME.EQ.6) TS6 = A(3)
IF(TIME.EQ.6) TSR6 = TS6/TS0

; ~.~.~.~. Simulation of ECOG score and nb of metastasis .~.~.~.~

PECOG0 = 0.66             ; proportion of ECOG=0 in NO16853 is 312/470=0.66                     cf Budzar et al. Anal Oncol 2012
PMETA0 = 0.23             ; proportion of META=0 (max 1 metastasis) in NO16853 is 107/470=0.23  cf Budzar et al. Anal Oncol 2012

IF(ICALL.EQ.4.AND.NEWIND.NE.2) THEN
CALL RANDOM(2,R)
IF(R.GT.PECOG0) ECOG = 1
IF(R.LE.PECOG0) ECOG = 0
ENDIF

IF(ICALL.EQ.4.AND.NEWIND.NE.2) THEN
CALL RANDOM(3,R)
IF(R.GT.PMETA0) META = 1  ; More than 1 metastasis
IF(R.LE.PMETA0) META = 0
ENDIF

; ~.~.~.~. Survival model .~.~.~.~

IF(TIME.EQ.6) LOGT =  B0 + B1*BASE + B2*TSR6 + B3*ECOG + B4*META + EPS(2) ; Log of survival time
TTE  =  EXP(LOGT)                                                         ; Survival time (months)
IF(TTE.LT.1.377)TTE=1.377                                                 ; An individual have to survive 6 weeks (=1.377 months) to get a TSR6, needed in the TTE function.

IF(TTE.LT.48) EVENT = 1
IF(TTE.GE.48)THEN
TTE   = 48
EVENT = 0    ; censored event
ENDIF

IF(TIME.NE.6) TTE = 0
IF(TIME.EQ.0) EVENT = 0

IF(ICALL.EQ.4) THEN
NTTE=TTE
NEVENT=EVENT
NECOG=ECOG
NMETA=META
NTSR6=TSR6
NBASE=BASE
ENDIF



$THETA  (0,0.00437) ; 1. KL  /week
$THETA  (0,0.00128) ; 2. KD1 /(week2*mg)
$THETA  (0,0.00470) ; 3. KD2 /(week2*g)
$THETA  (0,0.045)   ; 4. DM1 /week
$THETA  (0,0.24)    ; 5. DM2 /week
$THETA  (0,112)     ; 6. RUV mm2
$THETA  0.1 FIX     ; 7. KPD /week
$THETA  3 FIX       ; 8. KPC /week
$THETA  (0,72.11)   ; 9. BASE mm --> calculated from a 99% CI of a lognormal distribution with a 0.5 percentile of 10 mm and a 99.5 percentile of 520 mm
$THETA  3.02        ; 10. B0 = Intercept
$THETA  -0.00231    ; 11. B1 --> Baseline tumor size
$THETA  -0.801      ; 12. B2 --> Fractional change in tumor size at week 6
$THETA  -0.352      ; 13. B3 --> ECOG performance status
$THETA  -0.200      ; 14. B4 --> Number of metasteses

$OMEGA  1.6         ; 1. KL
$OMEGA  1.31        ; 2. KD1
$OMEGA  1.30        ; 3. KD2
$OMEGA  0  FIX      ; 4. DM1
$OMEGA  0  FIX      ; 5. DM2
$OMEGA  0.585       ; 6. BASE --> calculated from a 99% CI of a lognormal distribution with a 0.5 percentile of 10 mm and a 99.5 percentile of 520 mm

$SIGMA  1  FIX      ; Tumor model
$SIGMA  0.597529    ; lognormal distribution for TTE

$SIMULATION (822826) (179919 UNIFORM) (148453 UNIFORM) ONLYSIMULATION NSUB=1

$TABLE ID DAY WEEK CMT DV EVID AMT ADDL II ARM CAPD FLG NTTE NEVENT NEVID
       NECOG NMETA NTSR6 NBASE TEST2 TEST3 TEST4 TEST5 DID NOPRINT ONEHEADER
       FILE=part1.csv
