$PROBLEM  SIMULATION ESTIMATION MODEL

;; MODEL 1: one comp linear abs

$INPUT ID TIME DV EVID COV
$DATA data30.dat IGNORE=@ IGNORE=(EVID.EQ.1)

$PRED
  DOSE = 1000 
 
  TVCL = THETA(2)
  TVV = THETA(3)
  TVKE = TVCL/TVV  
  TVKA = THETA(1)
  
     
  CL = TVCL*EXP(ETA(2))
  V = TVV*EXP(ETA(3))
  KE = CL/V  
  KA = TVKA*EXP(ETA(1)) + KE
    

  A = EXP(-KE*TIME)
  B = EXP(-KA*TIME)
  C = KA-KE

  I = EXP(-TVKE*TIME)
  J = EXP(-(TVKA+TVKE)*TIME)
  K = TVKA
 
     
  IPRED = ((DOSE/V)*(KA/C))*(A-B)

  F = IPRED
  W = SQRT(THETA(4)**2*IPRED**2)
  DEL = 0.00001

IWRES = (DV-IPRED)/W

  IW = W
  Y = IPRED+W*EPS(1)

  PIP = IPRED
;  IF(COMACT.EQ.1) QPRE=F

$THETA  (0,1)                ;1 KA
$THETA  (0,10)               ;2 CL
$THETA  (0,100)              ;3 V  
$THETA  (0,0.3)              ;4 PROP  

$OMEGA  0.25                  ;1 IIV_KA 50%
$OMEGA  0.25                  ;2 IIV_CL 30%
$OMEGA  0.25                  ;2 IIV_V 30%

$SIGMA  1 FIX                ;1  

$ESTIMATION MAXEVAL=9990 POSTHOC NSIGDIG=3 METHOD = 1 INTER
;gls-final $ESTIMATION MAXEVAL=9990 METHOD = 0
;gls-sim $ESTIMATION MAXEVAL=9990 METHOD = 0


$TABLE ID TIME DV FILE=gls4tab NOAPPEND NOPRINT ONEHEADER
;$COVARIANCE PRINT=E

