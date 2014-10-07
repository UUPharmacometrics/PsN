;; 1. Based on: 
;; 2. Description: Cov Matrix(11), no RIS, not logit BC0, MTT 3, PPG, EFP exp ETA, RESFSI RESFPG cor ETA, RESHBA ETA, discrete FPG&HBA sim, $COV, HOMA2 correct math 4.5, cleaned up code
;; x1. Author: Steve Choy
$PROB  Cov Matrix(11), no RIS, not logit BC0, MTT 3
$INPUT ID TIME STDY BSL ODV DV MDV PCHG=DROP CMT fvar=DROP CMTA=DROP TRT=DROP DIP PHAS AGE RACE SEX BMI BLBMI TBLWT BLWT1 TBLINS BLINS TBLFPG BLFPG TBLHBA BLHBA FLAGA=DROP FLAGB=DROP FLAGC=DROP FLAG
$DATA obdm3plac.JN.BMI.FLAGFPG.csv IGNORE=@   IGNORE = (CMT.EQ.0,DIP.EQ.1,FLAG.GT.0)
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
IF(TIME.GT.BSL) OCC2 = 1	; Effect placebo, administered at baseline

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
EFFB = 1 + EFB * (1 - EXP(-(LOG(2)/T50)*( TIME )))
EFFW = DPR * (100 - EF) / 100 ; effect of weight based on disease progression, effect of D&E and effect of placebo

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
DADT(4) = PPG + KIHB * FPG - KOHB * A(4)      ; HbA1c production driven by FPG
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
IF(NEWIND.NE.2) THEN
  WTFLG1=0
  WTFLG2=0
  WTFLG3=0
  WTFLG4=0
ENDIF

IF(WTFLG1.EQ.0.AND.CMT.EQ.1) THEN
   BASE1=EXP( DV )
   WTFLG1=1
ENDIF

IF(WTFLG2.EQ.0.AND.CMT.EQ.2) THEN
   BASE2=EXP( DV )
   WTFLG2=1
ENDIF

IF(WTFLG3.EQ.0.AND.CMT.EQ.3) THEN
   BASE3=EXP( DV )
   WTFLG3=1
ENDIF

IF(WTFLG4.EQ.0.AND.CMT.EQ.4) THEN
   BASE4=EXP( DV )
   WTFLG4=1
ENDIF

IF(CMT.EQ.1) CFB1 = (EXP(DV)-BASE1)/BASE1
IF(CMT.EQ.2) CFB2 = (EXP(DV)-BASE2)/BASE2
IF(CMT.EQ.3) CFB3 = (EXP(DV)-BASE3)/BASE3
IF(CMT.EQ.4) CFB4 = (EXP(DV)-BASE4)/BASE4

CFB = E1*CFB1 + E2*CFB2 + E3*CFB3 + E4*CFB4 
IF(TIME.EQ.0) CFB = 0
;
$THETA  (0,89.5131) ; TH1 Kout
 (0,104.405) ; TH2 BLWT
 (-5,-0.462066,2) ; TH3 BC
 0.718179 ; TH4 RBCL
 (0,1.11055) ; TH5 IS
 (1,74.7782) ; TH6 T50
 (0,0.0130832) ; TH7 KIHB
 (0,36.4085) ; TH8 MTT
 (0,0.0816999) ; TH9 INT
 0.0460701 ; TH10 EFP
 (0,0.312403) ; TH11 EFB
 3.99431 ; TH12 EFDE PLAC
 2.65807 ; TH13 RDPR
 (0,0.00916042) ; TH14 RESWT
 (0,0.265923) ; TH15 RESFSI
 (0,0.0684201) ; TH16 RESFPG
 (0,0.0254434) ; TH17 RESHBA
 2.13792 ; TH18 EFPL PLAC
 0.958143 ; TH19 PPG
$OMEGA  BLOCK(11)
 0.0681142  ;    ET1 EFB
 -0.00694845 1.27322  ;     ET2 BC
 0.0656894 -0.401436 0.295355  ;    ET3 RBC
 -0.0291842 -0.356978 0.13982 0.320989  ;     ET4 IS
 0.0562604 0.756522 -0.183467 -0.430277 0.971574  ;    ET5 T50
 0.00201561 -0.00999291 -0.0010588 -0.00974419 0.026728 0.0198728  ;    ET6 INT
 -0.00953879 -0.136956 0.125966 0.266251 -0.15222 0.0112097 0.506327  ;    ET7 EFP
 0.00261666 -0.0364239 0.0231839 0.0362867 -0.0680401 -0.00395047 0.0221807 0.0214374  ;   ET8 BLWT
 0.447129 2.60079 -0.781796 -0.872974 1.78152 0.0131066 -0.753038 -0.191984 30.6715  ; ET9 EFP1 DIET
 -0.243414 -1.74033 -0.247247 0.811916 -3.60189 0.00660404 0.328743 0.0918214 -14.3845 36.878  ; ET10 EFP3 BSL PLAC
 0.355828 0.453127 -0.560557 0.00937272 -1.32844 -0.0372012 -1.26968 -0.121772 7.25755 26.3326 63.0963  ;  ET11 RDPR
$OMEGA  BLOCK(2)
 0.0708115  ; ET12 RESFSI
 0.0303269 0.0755274  ; ET13 RESFPG
$OMEGA  0.0277395  ; ET14 RESHBA
$SIGMA  1  FIX
$EST MSFO=msfb355 METHOD=1 INTERACTION MAX=19990 PRINT=5 POSTHOC NOABORT
$COVARIANCE MATRIX=S PRINT=E UNCONDITIONAL 
$TABLE ID TIME STDY ODV BF ISS0 DPR EFFW AA4 AA5 AA6 CMT
CFB DV PRED IPRED RES WRES IRES IWRES CWRES NOAPP NOPRINT ONEHEAD FILE=sdtab360_clean
$TABLE ID TIME STDY BSL CMT DV CFB EF EFFW KOHB BLWT BLI BLG EFFB BF BNET 
AGE RACE SEX EFSI EFPG EDEN EWT EHB EEFS BMI BC0 BCE0 IS0 ISS0 RBCL T50 DWTE DWTPE
TBLWT BLWT1 TBLINS BLINS TBLFPG BLFPG TBLHBA BLHBA NOAPP NOPRINT ONEHEAD FILE=catab360_clean
$TABLE ID BC0 IS0 ETA1 ETA2 ETA3 ETA4 ETA5 ETA6
ETA7 ETA8 ETA9 ETA10 ETA11 NOAPP NOPRINT ONEHEAD FILE=patab360_clean
