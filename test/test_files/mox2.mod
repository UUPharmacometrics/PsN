$PROBLEM MOXONIDINE PK, FINAL ESTIMATES, SIMULATED DATA
;;
;; run 1, FOCEI, Lag model, all data
;;
$INPUT      ID VISI XAT2=DROP DGRP DOSE FLAG=DROP ONO=DROP
            XIME=DROP DVO=DROP NEUY SCR AGE SEX NYH WT DROP ACE
            DIG DIU NUMB=DROP TAD TIME VIDD=DROP CRCL AMT SS II DROP
            CMT=DROP CONO=DROP DV EVID=DROP OVID=DROP
$DATA       mox_simulated.csv IGNORE=@

$SUBROUTINE ADVAN2 TRANS1
$PK

;---------- PK model ------------------

   TVCL  = THETA(1)
   TVV   = THETA(2)

   CL    = TVCL*EXP(ETA(1))
   V     = TVV*EXP(ETA(2))
   KA    = THETA(3)*EXP(ETA(3))
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

$THETA  (0,26.6)            ; TVCL
$THETA  (0,1.43)            ; TVV
$THETA  (0,4.45)            ; TVKA
$THETA  (0,.240)            ; LAG
$THETA  (0,.33)             ; RES ERR


$OMEGA  BLOCK(1) 0.0404 	; IIV CL
$OMEGA  BLOCK(1) 0.0270    	; IIV V
$OMEGA  BLOCK(1) 2.56       ; IIV KA

$SIGMA  1  FIX

$ESTIMATION METHOD=1  MAXEVALS=9999
;$COVARIANCE PRINT=E




