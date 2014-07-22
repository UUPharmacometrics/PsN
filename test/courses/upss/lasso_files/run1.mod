$PROBLEM MOXONIDINE PK, FINAL ESTIMATES, ALL DATA
;;
;; run 1, FOCEI, Lag model, all data
;;
$INPUT      ID VISI XAT2=DROP DGRP=DROP DOSE=DROP FLAG=DROP ONO=DROP
            XIME=DROP DVO=DROP NEUY=DROP SCR=DROP AGE SEX NYH=DROP WT
            DROP ACE DIG DIU NUMB=DROP TAD TIME VIDD=DROP CRCL AMT SS
            II DROP CMT=DROP CONO=DROP DV EVID=DROP OVID=DROP
            DROP SHR2=DROP  NYHA
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

   TVCL  = THETA(1)*(1+THETA(6)*(CRCL-65))
   TVV   = THETA(2)*WT

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

$THETA  (0,26.6)              ;TVCL
$THETA  (0,1.43)             ;TVV
$THETA  (0,4.45)               ;TVKA
$THETA  (0,.240)            ;LAG
$THETA  (0,.33)               ;RES ERR
$THETA  (0,.00758,.02941)       ;CRCL on CL

$OMEGA  BLOCK(2) 0.0404 0.027 0.0270    ; IIV (CL-V)
$OMEGA  BLOCK(1) 2.56           ; IIV KA

$OMEGA  BLOCK(1) 0.017           ; IOV CL
$OMEGA  BLOCK(1)  SAME         ; IOV CL

$OMEGA  BLOCK(1)  0.435           ; IOV KA
$OMEGA  BLOCK(1)  SAME         ; IOV KA

$SIGMA  1  FIX

$ESTIMATION METHOD=1  MAXEVALS=9999
;$COVARIANCE PRINT=E


