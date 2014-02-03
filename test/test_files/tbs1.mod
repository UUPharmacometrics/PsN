$PROBLEM lambda=0 FIX, zeta=0 FIX
;;
;;
$INPUT      ID VISI DV NEUY SCR AGE SEX NYHA WT ACE DIG DIU TAD TIME CRCL AMT SS II LNDV
$DATA       mox_sim_tbs.dta  IGNORE=@


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

   TVCL  = THETA(1)
   TVV   = THETA(2)

   CL    = TVCL*EXP(ETA(1)+KPCL)
   V     = TVV*EXP(ETA(2))
   KA    = THETA(3)*EXP(ETA(3)+KPKA)
   ALAG1 = THETA(4)
   K     = CL/V
   S2    = V

$ERROR

     IPRED = F
     IF(IPRED.LE.0) IPRED=0.025
     W     = THETA(5)*IPRED

     IRES  = IPRED-DV
     IWRES = IRES/W
     Y     = IPRED+ERR(1)*W

$THETA  (0,27.6)             ;TVCL
$THETA  (0,85)             ;TVV
$THETA  (0,2.45)             ;TVKA
$THETA  (0,.140)             ;LAG
$THETA  (0,.33)              ;RES ERR


$OMEGA  BLOCK(2) 0.0444 0.027 0.0241    ; IIV (CL-V)
$OMEGA  BLOCK(1) 3.0           ; IIV KA

$OMEGA  BLOCK(1) 0.0165           ; IOV CL
$OMEGA  BLOCK(1)  SAME         ; IOV CL

$OMEGA  BLOCK(1)  0.495           ; IOV KA
$OMEGA  BLOCK(1)  SAME         ; IOV KA

$SIGMA  1  FIX

$ESTIMATION METHOD=1 INTER MAXEVALS=9999 NOABORT PRINT=5


