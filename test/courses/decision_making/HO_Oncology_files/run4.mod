$PROBLEM Log-normal - no covariates
$INPUT ID DAY WEEK OCMT ODV OEVID OAMT OADDL OII ARM CAPD
       FLG TIME DV EVID ECOG META TSR6 BASET TEST2 TEST3
       TEST4 TEST5 DID

$DATA part1.csv IGN=@ IGNORE=(FLG.LT.1) IGN(ARM.GT.1)

$SUBR ADVAN=6 TOL=5
$MODEL COMP=(HAZARD)

$PK

   SIGM = THETA(1)*EXP(ETA(1))   ; SD for lognormal distribution (ETA is a placeholder)

$DES

DEL  = 1E-12
PI   = 3.14159265
TIM  = T+DEL
LTIM = LOG(TIM)
X1   = LTIM/SIGM
PDF  = EXP(-(1/2)*(X1**2))/SQRT(2*PI)

LOGNORM = ((1/(TIM*SIGM))*PDF/(1-PHI(X1)))
DADT(1)= LOGNORM

$ERROR

;--------------------------TTE Model ------------------------------

  IF(NEWIND.NE.2) OLDCHZ=0  ; reset the cumulative hazard
  CHZ = A(1)-OLDCHZ         ; cumulative hazard
                            ; from previous time point
                            ; in data set
  OLDCHZ = A(1)             ; rename old cumulative hazard

  SUR = EXP(-CHZ)                                   ; survival probability

  DELX  = 1E-12
  PIX   = 3.14159265
  TIMX  = TIME+DELX
  LTIMX = LOG(TIMX)
  X1X   = LTIMX/SIGM
  PDFX  = EXP(-(1/2)*(X1X**2))/SQRT(2*PIX)
  
  LOGNORMX = ((1/(TIMX*SIGM))*PDFX/(1-PHI(X1X)))
  HAZNOW = LOGNORMX                                ; rate of event
                                                   ; each time pt
                                                   ; NB: update with each new model

  IF(DV.EQ.0)   Y=SUR          ; censored event (prob of survival)
  IF(DV.NE.0)   Y=SUR*HAZNOW   ; prob density function of event

$THETA  0.773                  ; SD for log-normal distribution
$OMEGA  0  FIX

$ESTIM MAXEVAL=9999 METHOD=0 LIKE PRINT=1 MSFO=msfb4 SIGL=9 NSIG=3
$COV PRINT=E         


