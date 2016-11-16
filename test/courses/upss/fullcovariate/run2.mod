$PROBLEM MOXONIDINE PK, FINAL ESTIMATES, ALL DATA + covariate insertion
;;
;;
$INPUT      ID VISI XAT2=DROP DGRP=DROP DOSE=DROP FLAG=DROP ONO=DROP
            XIME=DROP DVO=DROP NEUY SCR AGE SEX NYHA WT
            DROP ACE DIG DIU NUMB=DROP TAD TIME VIDD=DROP CRCL AMT SS
            II DROP CMT=DROP CONO=DROP DV EVID=DROP OVID=DROP
            DROP SHR2=DROP NYHDI=DROP
$DATA       mx19.csv IGNORE=@

$SUBROUTINE ADVAN2 TRANS1
$PK

;-----------OCCASIONS----------
   VIS3               = 0
   IF(VISI.EQ.3) VIS3 = 1
   VIS8               = 0
   IF(VISI.EQ.8) VIS8 = 1

;----------IOV--------------------
   
   KPCL  = VIS3*ETA(4)+VIS8*ETA(5)
   KPKA  = VIS3*ETA(6)+VIS8*ETA(7)

;---------- PK model ------------------

   TVCL  = THETA(1)*(CRCL/65)**THETA(6)*THETA(8)**ACE*THETA(9)**DIG
   TVV   = THETA(2)*(WT/70)**THETA(7)

   CL    = TVCL*EXP(ETA(1)+KPCL)
   V     = TVV*EXP(ETA(2))
   KA    = THETA(3)*EXP(ETA(3)+KPKA)
   ALAG1 = THETA(4)
   K     = CL/V
   S2    = V

$ERROR

     IPRED = LOG(.025)
     WA     = THETA(5)
     W      = WA
     IF(F.GT.0) IPRED = LOG(F)
     IRES  = IPRED-DV
     IWRES = IRES/W
     Y     = IPRED+ERR(1)*W

$THETA  (0,27.6)           ;TVCL
$THETA  (0,85)             ;TVV
$THETA  (0,2.45)           ;TVKA
$THETA  (0,.140)           ;LAG
$THETA  (0,.33)            ;RES ERR
$THETA  (0, 0.75)          ;CRCL on CL
$THETA  (0, 1)             ;WT on V
$THETA  (0, 1)             ;ACE on CL
$THETA  (0, 1)             ;DIG on CL



$OMEGA  BLOCK(2) 0.0444 0.027 0.0241    ; IIV (CL-V)
$OMEGA  BLOCK(1) 3.0           ; IIV KA

$OMEGA  BLOCK(1) 0.0165           ; IOV CL
$OMEGA  BLOCK(1)  SAME         ; IOV CL

$OMEGA  BLOCK(1)  0.495           ; IOV KA
$OMEGA  BLOCK(1)  SAME         ; IOV KA

$SIGMA  1  FIX

$ESTIMATION METHOD=1  MAXEVALS=9999
$COV MATRIX=R PRINT=E
