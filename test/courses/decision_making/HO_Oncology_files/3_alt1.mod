$PROBLEM Weibull - ref:1250 test:900 N:500
$INPUT ID DAY WEEK OCMT ODV OEVID OAMT OADDL OII ARM CAPD
       FLG TIME DV EVID ECOG META TSR6 BASET TEST2 TEST3
       TEST4 TEST5 DID

$DATA data_20131014.csv IGN=@ IGNORE=(FLG.LT.1) IGNORE=(TEST4.LT.1) IGNORE=(DID.GT.500)

$SUBR ADVAN=6 TOL=6
$MODEL COMP=(HAZARD)

$PK

 LAM   = THETA(1)*EXP(ETA(1))    ; scale factor in arm 1
 SHP   = THETA(2)                ; shape factor
 HR    = EXP(THETA(3))           ; log HR

 BETA1 = THETA(4)                ; coefficient BASE(Tumor)
 BETA2 = THETA(5)                ; coefficient TSR6
 BETA3 = THETA(6)                ; coefficient ECOG
 BETA4 = THETA(7)                ; coefficient META
 
 COV = BETA1*BASET+BETA2*TSR6+BETA3*ECOG+BETA4*META

$DES

  DEL = 1E-6
  WB  = (LAM*SHP*(LAM*(T+DEL))**(SHP-1))
  IF(ARM.GT.1) WB = HR*WB

  DADT(1)= WB*EXP(COV)           ; hazard


$ERROR

;--------------------------TTE Model ------------------------------

  IF(NEWIND.NE.2) OLDCHZ=0  ; reset the cumulative hazard
  CHZ = A(1)-OLDCHZ         ; cumulative hazard
                            ; from previous time point
                            ; in data set
  OLDCHZ = A(1)             ; rename old cumulative hazard

  SUR = EXP(-CHZ)                                ; survival probability

  DELX = 1E-6
  WBX  = (LAM*SHP*(LAM*(TIME+DELX))**(SHP-1))
  IF(ARM.GT.1) WBX = HR*WBX

  COVX = BETA1*BASET+BETA2*TSR6+BETA3*ECOG+BETA4*META
  HAZNOW = WBX*EXP(COVX)                         ; rate of event
                                                 ; each time pt
                                                 ; NB: update with each new model

  IF(DV.EQ.0)   Y=SUR          ; censored event (prob of survival)
  IF(DV.NE.0)   Y=SUR*HAZNOW   ; prob density function of event

$THETA   (0,0.025) ; 1. LAM
$THETA   (0,1)     ; 2. SHP
$THETA   (0.025)   ; 3. LOG HR
$THETA   (0.002)   ; 3. coefficient BASE(Tumor)
$THETA   0 FIX     ; 4. coefficient TSR6
$THETA   (0.3)     ; 5. coefficient ECOG
$THETA   (0.2)     ; 6. coefficient META
$OMEGA  0  FIX

$ESTIM MAXEVAL=9999 METHOD=0 LIKE PRINT=1 MSFO=msfb3_alt1 SIGL=9 NSIG=3
$COV PRINT=E



