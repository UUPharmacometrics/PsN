;; 1. Based on: 360
;; 2. Description: Full WT range, -  D&E+PL, No PPG, DRUG switch
;; x1. Author: Steve Choy
$PROB  Cov Matrix(11), no RIS, not logit BC0, MTT 3
$INPUT ID TIME BSL CMT DV TRT 
$DATA Sim_WHIG_369m.csv IGNORE=@   IGNORE = (CMT.LT.4,TIME.GT.253)
$SUB ADVAN6 TOL=5;
$MODEL
COMP = (WEIGHT)                        ; CMT1 for BODY WEIGHT
COMP = (INSULIN)                       ; CMT2 for INSULIN
COMP = (FPG)                           ; CMT3 for FPG
COMP = (HBA1C)                         ; CMT4 for HBA1c
COMP = (HBA2C)                         ; CMT5 for HBA1c
COMP = (HBA3C)                         ; CMT6 for HBA1c
;
$PK
;;=================Drug effect (increasing BC)======
DRUG  = THETA(20)**TRT  ; Drug effect is switched on if TRT = 1

;;=================Weight parameters================
Kout  = LOG(2)/THETA(1)
BLWT  = THETA(2)* EXP(ETA(8))
Kin   = Kout * BLWT

;;=================Beta-cell parameters=============
BC0     =  THETA(3) + ETA(2)
BCE0    =  1/(1 + EXP(BC0))                ; scale beta-cell function between 0 and 1
RBCL     =  THETA(4)+ETA(3) ; rate of beta cell function loss

;;=================Insulin parameters===============
IS0     =  THETA(5) + ETA(4)
ISS0    =  1/(1 + EXP(IS0))    ; scale insulin sensitivity between 0 and 1
KIOI    =  7.8                   ; kinI/koutI set to correspond to typical steady-state fasting FSI 5uU/mL

;;=================Glucose parameters===============
T50     =  THETA(6) * EXP(ETA(5))         ; kout FPG
KIOG     =  35.1    ; kinG/koutG set to correspond to typical ss FPG level in healthy subjects of 4.5 mmol/L given a ss FSI level of 7.8 uU/mL

;;=================HbA1c parameters=================
MTT     =  THETA(8)		; mean transit time
NC      =  3			; no. of total compartments for HbA1c
KIHB    =  THETA(7)
KOHB    =  NC/MTT		; K transit
INTCPT  =  THETA(9) * EXP(ETA(6))
PPG     =  INTCPT
IF(TIME.GT.0) PPG = INTCPT * THETA(19)

;;=================Baseline functions===============
B       =  3.5 * BCE0 * KIOI
C       =  -BCE0 * KIOI * KIOG / (ISS0)
BLI     =  (-B + SQRT(B**2 -4*C)) / (2)   ; baseline insulin
BLG     =  KIOG / (ISS0 * BLI)       ; baseline FPG
BLHB    =  (INTCPT + KIHB*BLG)/KOHB *NC  ; baseline HbA1c

;;=================Treatment effect=================
EFP     =  THETA(10) *EXP(ETA(7)) ; effect of placebo
EFB     =  THETA(11) + (ETA(1)) ; effect on beta cell function

OCC1=0
OCC2=0
IF(TIME.GT.1)   OCC1 = 1    ; Effect of treatment 1
;IF(TIME.GT.BSL) OCC2 = 1	; Effect placebo, administered at baseline
IF(TIME.GT.1)  OCC2 = 1	; Effect placebo, administered at baseline

EFDE = THETA(12)  + ETA(9)  ; Effect parameter of Diet&Exercise: counseling administered at screening
EFPL = THETA(18)  + ETA(10)  ; Effect placebo, administered at baseline

EF   = EFDE * OCC1 + (EFPL * OCC2)

;;=================Disease progression functions====
RDPR  = THETA(13)  + ETA(11) ; rate of disease progression

;;=================Residual error parameters========
RESWT   = THETA(14)		; proportional weight residual error
RESFSI	= THETA(15)	* EXP(ETA(12))	; proportional FSI residual error
RESFPG	= THETA(16)	* EXP(ETA(13))	; proportional FPG residual error
RESHBA	= THETA(17)	* EXP(ETA(14))	; proportional HbA1c residual error

;;=================Time parameters==================
BF      =  1/(1 + EXP(BC0 + RBCL*TIME/365)) ;rate of loss beta cell function per year
DPR     = (100 + RDPR * TIME/365) / 100 ; disease progression rate / effect of loss d&e and placebo
EFFB = DRUG * ((1 - EFB * (1 - EXP(-(LOG(2)/T50)*( TIME )))))
EFFW = DPR * (100 + EF) / 100 ; effect of weight based on disease progression, effect of D&E and effect of placebo

BNET = EFFB * BF
;;=================Initialization of compartments===
A_0(1)      =  BLWT
A_0(2)      =  BLI
A_0(3)      =  BLG
A_0(4)      =  BLHB/NC
A_0(5)      =  BLHB/NC
A_0(6)      =  BLHB/NC

;;================= First Weight record switch======
IF(NEWIND.NE.2) IND = 0
IF(CMT.EQ.1)    IND = 1
$DES
AA1=A(1)
IF(IND.EQ.0) DWT = 0
IF(IND.EQ.1) DWT = BLWT - A(1)
IF(IND.EQ.0) DWTP = 1
IF(IND.EQ.1) DWTP = A(1)/BLWT

EFFS = 1 + EFP * DWT      ;EFFECT ON INSULIN SENSITIVITY IS PROPORTIONAL TO CHANGE IN BODY WEIGHT FROM BASELINE
;
B1 =  3.5 * BF * EFFB * KIOI ; 3.5 is the FPG threshold conc
C1 =  -BF * EFFB * KIOI * KIOG / (ISS0 * EFFS)
FSI =  (-B1 + SQRT(B1*B1 -4*C1)) / (2)   ; FSI production stimulated by FPG (linearized)
IF (FSI.LT.0)  FSI = 1
;
FPG =  KIOG / (EFFS * ISS0 * FSI)       ; baseline FPG
;
DADT(1) = Kin * EFFW - Kout * A(1)            ; turn-over model for body weight
DADT(2) = 0                                   ; short-term dynamics assumed to be at steady-state
DADT(3) = 0                                   ; short-term dynamics assumed to be at steady-state
DADT(4) = INTCPT + KIHB * FPG - KOHB * A(4)      ; HbA1c production driven by FPG
DADT(5) = KOHB*A(4)-KOHB*A(5)      			  ; HbA1c transit
DADT(6) = KOHB*A(5)-KOHB*A(6)                 ; HbA1c transit
;

$ERROR
;;=================Re-defining Time parameters outisde $DES===
;
AA4=A(4)
AA5=A(5)
AA6=A(6)
EWT  = A(1)
IF(EWT.LE.0) EWT = 0.00001
EHB  = A(4) + A(5) + A(6)
IF(EHB.LE.0) EHB = 0.00001
;
IF(IND.EQ.0) DWTE = 0
IF(IND.EQ.1) DWTE = BLWT - A(1)
IF(IND.EQ.0) DWTPE = 1
IF(IND.EQ.1) DWTPE = A(1)/BLWT

EEFS = 1 + EFP * DWTE
;
B2 =  3.5 * BF * EFFB * KIOI
C2 =  -BF * EFFB * KIOI * KIOG / (ISS0 * EEFS)
EFSI =  (-B2 + SQRT(B2**2 -4*C2)) / (2)   ; FSI production stimulated by FPG (linearized)
IF (EFSI.LT.1)  EFSI = 1
EFPG =  KIOG / (EEFS * ISS0 * EFSI)       ; baseline FPG
IF (EFPG.LE.0) EFPG = 0.00001
EDEN = (EEFS * ISS0 * EFSI)
;
;
;;=================On/off switch for each compartment========
E1 = 0
E2 = 0
E3 = 0
E4 = 0
IF (CMT.EQ.1) E1 = 1
IF (CMT.EQ.2) E2 = 1
IF (CMT.EQ.3) E3 = 1
IF (CMT.EQ.4) E4 = 1

IPRED  =   LOG(0.00001)
IF(F.GT.0) IPRED = E1*LOG(EWT) + E2*LOG(EFSI) + E3*LOG(EFPG) + E4*LOG(EHB)
IF(F.GT.0) W = RESWT*E1 + RESFSI*E2 + RESFPG*E3 + RESHBA*E4
IRES  =   DV - IPRED
IWRES  = IRES
IF(W.NE.0) IWRES =   IRES/W
Y     =   IPRED + W*ERR(1)

IF(ICALL.EQ.4.AND.CMT.EQ.3) THEN
Y = LOG((INT(10*( EXP(IPRED + W*ERR(1) ) )))/10)
ENDIF

IF(ICALL.EQ.4.AND.CMT.EQ.4) THEN
Y = LOG((INT(10*( EXP(IPRED + W*ERR(1) ) )))/10)
ENDIF
;;=================Change from baseline===================
;IF(NEWIND.NE.2) THEN
;  WTFLG4=0
;ENDIF

;IF(WTFLG4.EQ.0) THEN
;  HBAO = EHB * (1 + ERR(1)* RESHBA)
;  WTFLG4=1
;ENDIF

;IF(TIME.EQ.0.AND.CMT.EQ.4) HBAO = EHB * (1 + ERR(1)* RESHBA)

IF(TIME.EQ.0) THEN
	HBAO = EHB * (1 + ERR(1)* RESHBA)
	HBAOL = HBAO
	ELSE
	HBAO = HBAOL
ENDIF

HBA = EHB * (1 + ERR(1)* RESHBA)

CFB =  HBA - HBAOL

;CFB = E1*CFB1 + E2*CFB2 + E3*CFB3 + E4*CFB4
;IF(TIME.EQ.0) CFB = 0
;
$THETA  (0,89.5583) ; TH1 Kout
 (0,104.403) ; TH2 BLWT
 (-5,-0.461812,2) ; TH3 BC
 0.71803 ; TH4 RBCL
 (0,1.11039) ; TH5 IS
 (1,74.799) ; TH6 T50
 (0,0.0130831) ; TH7 KIHB
 (0,36.4087) ; TH8 MTT
 (0,0.081699) ; TH9 INT
 0.0460469 ; TH10 EFP
 (0,0.312451) ; TH11 EFB
 3.99825 ; TH12 EFDE PLAC
 2.66071 ; TH13 RDPR
 (0,0.0091604) ; TH14 RESWT
 (0,0.265931) ; TH15 RESFSI
 (0,0.0684208) ; TH16 RESFPG
 (0,0.0254438) ; TH17 RESHBA
 2.13616 ; TH18 EFPL PLAC
 0.95814 ; TH19 PPG
 1.25 FIX ; TH20 DRUG
$OMEGA  BLOCK(11)
 0.0681127  ;    ET1 EFB
 -0.00690965 1.27314  ;     ET2 BC
 0.0656651 -0.40139 0.295331  ;    ET3 RBC
 -0.0291804 -0.357006 0.139839 0.321029  ;     ET4 IS
 0.0562389 0.756092 -0.183369 -0.430168 0.970904  ;    ET5 T50
 0.00201925 -0.0100005 -0.00105561 -0.00974081 0.0267237 0.0198738  ;    ET6 INT
 -0.009544 -0.13697 0.125997 0.266393 -0.152373 0.0112107 0.506651  ;    ET7 EFP
 0.00261448 -0.0364231 0.0231808 0.0362905 -0.0680202 -0.00394876 0.0221903 0.0214367  ;   ET8 BLWT
 0.447639 2.60235 -0.782191 -0.873613 1.78148 0.0131188 -0.75395 -0.192044 30.7064  ; ET9 EFP1 DIET
 -0.243595 -1.74171 -0.247023 0.812703 -3.60273 0.00661366 0.330241 0.0918769 -14.407 36.915  ; ET10 EFP3 BSL PLAC
 0.356115 0.453734 -0.560722 0.00952051 -1.32943 -0.0370975 -1.2696 -0.12185 7.26478 26.3611 63.1507  ;  ET11 RDPR
$OMEGA  BLOCK(2)
 0.0707989  ; ET12 RESFSI
 0.0303287 0.0755153  ; ET13 RESFPG
$OMEGA  0.0277395  ; ET14 RESHBA
$SIGMA  1  FIX
$SIML (12345) ONLYSIM NSUB=1
;$EST MSFO=msfb355 METHOD=1 INTERACTION MAX=19990 PRINT=5 POSTHOC NOABORT
;$COVARIANCE MATRIX=S PRINT=E UNCONDITIONAL 
$TABLE ID TIME CMT DV PRED IPRED CFB EFFW EFFB BF EHB  
EEFS DWTE TRT NOAPP NOPRINT ONEHEAD FILE=simtab54