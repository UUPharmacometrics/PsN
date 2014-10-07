$PROBLEM Constant Hazard - no covariates
$INPUT ID DAY WEEK OCMT ODV OEVID OAMT OADDL OII ARM CAPD 
       FLG TIME DV EVID ECOG META TSR6 BASET TEST2 TEST3
       TEST4 TEST5 DID

;Sim_start : remove from simulation model
$DATA part1.csv IGN=@ IGNORE=(FLG.LT.1) IGN(ARM.GT.1)
;Sim_end

;Sim_start : add for simulation
;$DATA part1_sim.csv IGNORE=#
;Sim_end

$SUBR ADVAN=6 TOL=9
$MODEL COMP=(HAZARD)

$PK

  BASE  = THETA(1)*EXP(ETA(1)) ; hazard ref. (the ETA is a placeholder here)

$DES

  DADT(1)=BASE                 ; hazard

$ERROR

;--------------------------TTE Model ------------------------------

  IF(NEWIND.NE.2) OLDCHZ=0  ; reset the cumulative hazard
  CHZ = A(1)-OLDCHZ         ; cumulative hazard
                            ; from previous time point
                            ; in data set
  OLDCHZ = A(1)             ; rename old cumulative hazard

  SUR = EXP(-CHZ)           ; survival probability
  HAZNOW=BASE               ; rate of event
                            ; each time pt
                            ; NB: update with each new model

  IF(DV.EQ.0)   Y=SUR          ; censored event (prob of survival)
  IF(DV.NE.0)   Y=SUR*HAZNOW   ; prob density function
  
  IF(ICALL.EQ.4)THEN ; for simulation
    CALL RANDOM (2,R)
    DV=0
    RTTE=0
    IF(TIME.EQ.48) RTTE=1
    IF(R.GT.SUR)THEN
    DV=1
    RTTE=1
    ENDIF
  ENDIF

$THETA  (0,0.025) ; 1. BASE
$OMEGA  0  FIX

;Sim_start : add for simulation
;$SIMULATION (123456789) (12345 UNIFORM) ONLYSIM NOPRED
$ESTIM MAXEVAL=9999 METHOD=0 LIKE PRINT=1 MSFO=msfb2 SIGL=9 NSIG=3
$COV PRINT=E
;Sim_end





