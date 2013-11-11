;; 1. Based on: sse_2
;; 2. Description: est-true model - sse - uncertainty
;; x1. Author: Salim Bouchene
$PROB  FULLMODEL_INH=MAV
$INPUT ID TIME DV CMT TREAT DOSE AMT EVID
$DATA sse002.csv IGNORE=#
$SUBROUTINE ADVAN6 TOL=9
$MODEL NCOMP=4

COMP=TCELL
COMP=ACTIVE
COMP=LATENT
COMP=VIRUS

$PK

IF(NEWIND.NE.2) DBAS=0               ; Set these values to 0 if a new individual
IF(NEWIND.NE.2) BASE=0
IF(NEWIND.NE.2) ENDHIV=0

RR0=THETA(1)*EXP(ETA(1))           ; REPROD RATIO
IF (RR0.LE.1) EXIT 1
LAMBDA=THETA(2)*EXP(ETA(2))        ;UNINFECTED CELL ACTIVATION
DA0=THETA(3)*EXP(ETA(3))           ;ACTIVE INFECTED CELLS DEATH RATE
D=0.006                            ;UNINFECTED CELL DEATH RATE
QA=0.96                            ;FRACTION OF INFECTED CELLS ACTIVELY VS LATENTLY
DL=0.0132                          ;LATENTLY INFECTED CELLS DEATH RATE
AL=0.037                           ;RATE OF CONVERSION FROM LAETENTLY TO ACTIVELY
POVC=35.4                          ;RATIO OF BIRTH TO DEATH RATE OF VIRUS
BETA=RR0*D*DA0 / (POVC*LAMBDA*(QA+(1-QA)*AL/(DL+AL))); INFECT RATE CD4    ;HUR beräkna detta?



F1X=LAMBDA/(D*RR0)
F2X=(QA+(1-QA)*AL/(DL+AL)) * (LAMBDA-D*F1X)/DA0
F3X=(1-QA)*(LAMBDA-D*F1X)/(DL+AL)
F4X=F2X*POVC

A_0(1)=F1X
A_0(2)=F2X
A_0(3)=F3X
A_0(4)=F4X

EFF=0                        ; 10 DAGARS BEHANDLING
INH=0
ED50=THETA(4)*EXP(ETA(4))

IF (TREAT.EQ.1)EFF=1
IF (DOSE.NE.0) THEN           ; DRUG
INH=(DOSE/(ED50+DOSE))*EFF
ENDIF

$DES
VI=POVC*A(2)
DADT(1)=LAMBDA - D*A(1) - (1-INH)*BETA*VI*A(1)
DADT(2)=QA*(1-INH)*BETA*VI*A(1)-DA0*A(2)+AL*A(3)
DADT(3)=(1-QA)*(1-INH)*BETA*VI*A(1)-DL*A(3)-AL*A(3)

CD4=A(1)
ACT=A(2)
LAT=A(3)

VIR=VI*1000
VIRB=F4X*1000

$ERROR
IPRED = LOG10((POVC*A(2))*1000+0.00001)
Y     = IPRED+EPS(1)

;------------------------------
;CALCULATIONS
;------------------------------
HIV=Y
BASE=BASE
IF(TIME.EQ.0.AND.CMT.EQ.4) BASE = HIV   ; Define simulated baseline PANSS for each simulated individual
DBAS = (BASE-HIV)*(-1)               	; Calculated difference from baseline for last simulate PANSS

ENDHIV=ENDHIV
IF(TIME.EQ.10.AND.CMT.EQ.4) ENDHIV=HIV
;------------------------------
;END OF CALCKULATIONS
;------------------------------
$THETA   ; slitly altered initial cond
(2.46 FIX)            ;1 RR0
(0.5,3.59, 6)         ;2 LAMBDA
(0.1,0.676,3)         ;3 DA0
(0,4.4,10)              ;4 ED50      ; MG/L

$OMEGA
0 FIX
0.82                   ; 1.82
0.01                 ;0.0729
0.2                    ;0.1

$SIGMA 0.042            ;0.04
$ESTIMATION MAXEVAL=99999 METHOD=1 INTER PRINT=5 SIGL=9 NSIG=3 NOABORT MSFO=msf24.4
$COV
;$SIMULATION (12344) ONLYSIMULATION SUBPROBLEMS=100    ;TRUE=INITIAL
$TABLE ID TIME DV PRED IPRED VIRB VIR NOPRINT ONEHEADER FILE=ssetab002u

