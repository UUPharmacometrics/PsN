$PROBLEM    MOXONIDINE PK,FINAL ESTIMATES,simulated data
$INPUT      ID VISI XAT2=DROP DGRP DOSE FLAG=DROP ONO=DROP XIME=DROP
            DVO=DROP NEUY SCR AGE SEX NYHA WT DROP ACE DIG DIU
            NUMB=DROP TAD TIME VIDD=DROP CRCL AMT SS II DROP CMT=DROP
            CONO=DROP DV EVID=DROP OVID=DROP
$DATA      mox_simulated.csv IGNORE=@
$SUBROUTINE ADVAN2 TRANS1
$PK  
;-----------OCCASIONS----------
   VIS3               = 0
   IF(VISI.EQ.3) VIS3 = 1
   VIS8               = 0
   IF(VISI.EQ.8) VIS8 = 1

;----------IOV--------------------
   
   KPLAG  = VIS3*ETA(1)+VIS8*ETA(2)

;---------- PK model ------------------

   TVCL  = THETA(1)*30
   TVV   = THETA(2)*100
   TVKA  = THETA(3)

   CL    = TVCL*EXP(ETA(3))
   V     = TVV*EXP(ETA(4))
   KA    = TVKA*EXP(ETA(5))
   LAG   = THETA(4)/20
   PHI   = LOG(LAG/(1-LAG))
   ALAG1 = EXP(PHI+KPLAG)/(1+EXP(PHI+KPLAG))
   K     = CL/V
   S2    = V

$ERROR  

     IPRED = LOG(.025)
     WA     = 1
     W      = WA
     IF(F.GT.0) IPRED = LOG(F)
     IRES  = IPRED-DV
     IWRES = IRES/W
     Y     = IPRED+ERR(1)*W

$THETA  (0,1.1) ; TVCL/30
$THETA  (0,1.15) ; TVV/100
$THETA  (0,1.41) ; TVKA
$THETA  (0,1.37) ; LAG*20
$OMEGA  BLOCK(1)
 0.394  ;    IOV LAG
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(2)
 0.416  ;     IIV CL
 0.392 0.584  ;      IIV V
$OMEGA  BLOCK(1)
 0.224  ;     IIV KA
$SIGMA  0.112
$ESTIMATION METHOD=1 MAXEVAL=9999
$COVARIANCE PRINT=E

