;$SIZES NO = 1000 LIM6=600
$PROBLEM 
;; 
;; 1. Based on: 
;; 2. Description:  simulation + drugx instead of rbv
; 3. Label HCV
; 4. Structural model TC, IC, VL_I, VL_NI
; 5. Covariate model:  NA
; 6. Inter-individual variability: Yes
; 7. Inter-occasion variability: No
; 8. Residual variability: Additive on log
; 9. Simulation no bql handling


$INPUT ID TIME DV MDV GENO BW PEG DRBV T_END CENS TRM DX


$DATA Sim_mlx07_2.csv IGNORE=@

$SUBS  ADVAN8 TOL=5


$MODEL
NCOMP=4             ;Total number of compartments
COMPARTMENT=(TC)      ;Compartment Target Cells, suspectible
COMPARTMENT=(IC)      ;Compartment Infected Cells
COMPARTMENT=(VL_I)    ;Compartment Virions Infective
COMPARTMENT=(VL_NI)     ;Compartment Virions Non-Infective




$PK
; dos RBV drugx
DRUGX = DX/BW
RBV = DRBV/BW


;Some constants
TC_max = 18500000                    ; cells.mL-1
d      = 1/300                       ; day-1
IC_min = 1/13500                     ; cells.mL-1
My_s      = TC_max*d                 ; cells.mL-1.day-1


;Parameters, typical values
TV_RR0               =THETA(1)
TV_p                 =THETA(2)
TV_c                 =THETA(3)
IF (GENO==0)  TV_delta           =THETA(4)
IF (GENO==1)  TV_delta           =THETA(5)
IF (GENO==0)  TV_ED50peg         =THETA(6)
IF (GENO==1)  TV_ED50peg         =THETA(7)
TV_ED50rbv           =THETA(8)
TV_ED50drugx           =THETA(12)
TV_K                 =THETA(9)
TV_r                 =THETA(10)

;Parameters, individual values
IV_RR0   = ETA(1)
IV_c     = ETA(2)
IV_delta = ETA(3)
IV_EC50  = ETA(4)


;Parameters, combined
RR0     = TV_RR0*EXP(IV_RR0)
TV_p    = TV_p
MY_c    = TV_c*EXP(IV_c)
delta   = TV_delta*EXP(IV_delta)
ED50peg = TV_ED50peg*EXP(IV_EC50)
ED50rbv = TV_ED50rbv*EXP(IV_EC50)
ED50drugx = TV_ED50drugx*EXP(IV_EC50)
K       = TV_K
Tv_r    = TV_r


;$REG PEG RBV T_End

my_eps = PEG/(PEG+ED50peg)
rho = RBV/(RBV+ED50rbv)
gamma = (DRUGX)/(DRUGX+ED50drugx)


RR0T = 1 + RR0

beta   = RR0T*delta*MY_c/(TC_max*TV_p)  ; virions.mL-1.day-1

; compute the initial values
E = delta*RR0T/TV_r
B = delta*TC_max/TV_r
my_D = (TC_max + (d*B)/(delta*E) - B/E - B) / E

;Initialize system
A_0(1)    = 0.5*(-my_D + SQRT(my_D**2 + (4*TC_max*My_s)/(TV_r*(E**2))))
A_0(2)    = A_0(1)*(E-1) + TC_max - B
A_0(3)  = TV_p*A_0(2)/MY_c
A_0(4) = 0


$DES

; condition on the number of infected cells
pCond = TV_p
IF  ( A(2) < IC_min ) pCond = 0


; inhibition before and after the end of treatment
coeff = 1
IF (T>T_END) coeff = EXP(-K*(T-T_END))

rhoCond = coeff*rho
epsCond = coeff*my_eps
gammaCond=coeff*gamma


; proliferation rate of the cells
r_prolif = TV_r*(1-(A(1)+A(2))/TC_max)

; ODE system
DADT(1)    = My_s + r_prolif*A(1) - d*A(1) - beta*A(1)*A(3)  ;Compartment Target Cells, suspectible [cell/ml/day]
DADT(2)    = beta*A(1)*A(3) + r_prolif*A(2) - delta*A(2)     ;Compartment Infected Cells [cell/ml/day]
DADT(3)  = (1-gammaCond -rhoCond)*(1-epsCond)*pCond*A(2) - MY_c*A(3)    ;Compartment Virions Infective [IU/ml/day]
DADT(4) = (gammaCond+rhoCond)*(1-epsCond)*pCond*A(2) - MY_c*A(4)         ;Compartment Virions Non-Infective [IU/ml/day]

$ERROR

LLOQ=LOG10(50)  ;LLOQ=50   (LOG=LN)   ; anna: log10

OTC   =A(1)
OIC   =A(2)
OVL_I =A(3)
OVL_NI=A(4)


VLTOT=A(3)+A(4)

;Guard against log(0)

IF(VLTOT.LE.1) VLTOT=1.0001  ; anna: (prevent log(x)<0) (VLTOT and F are not on logscale)

IPRED=LOG10(VLTOT)

IRES = DV-IPRED

W    = THETA(11);

IWRES = IRES/W

;DUM = (LLOQ-IPRED)/W    ;anna: not variance but std
;DUM2= PHI(DUM)


;IF(CENS.EQ.0) THEN  ;DV>LLOQ
;F_FLAG=0
Y=IPRED+EPS(1)*W
;ENDIF

;IF(CENS.EQ.1) THEN  ;DV<LLOQ
;F_FLAG=1
;Y=DUM2
;ENDIF

;Model deifined cure if < 1 infected hapatocyte in system  (13500 ml)
CURE = 0
IF(OIC.LT.(1/13500)) CURE = 1

OLOQ=0
IF((IPRED+EPS(1)*W)<LLOQ) OLOQ=1

;IF no virons detected 24 w after end of treatment --> SVR
SVR = 0
IF(TIME.GE.(T_END+24*7)) THEN
SVR = OLOQ
ENDIF


;LLOQ observation
IF(ICALL.EQ.4) THEN
CENS = OLOQ
ENDIF




$THETA  (0,7.15)    ; 1. RR0
$THETA  (0,25.1)    ; 2. p                       Virions·hepatocyte/day
$THETA  (0,4.53)    ; 3. c                       /day
$THETA  (0,0.192)   ; 4. delta_n1                /day
$THETA  (0,0.139)   ; 5. delta_1                 /day
$THETA  (0,1.19)    ; 6. ED50peg_n1              µg/week
$THETA  (0,20.9)    ; 7. ED50peg_1               µg/week
$THETA  (0,14.4)    ; 8. ED50rbv                 mg/kg/day
$THETA  (0,0.0238)  ; 9. K                      /day
$THETA  (0,0.00562) ; 10. r                      /day
$THETA  (0,0.51)    ; 11. std error
$THETA  (0,4)      ; 12. ED50drugx               mg/kg/day

$OMEGA  1.879   ;RR0
$OMEGA  1.44    ;c
$OMEGA  0.3364  ;delta
$OMEGA  7.8961  ;EC50


$SIGMA  1 FIX  ;variance of add. on log residual error

$SIMULATION (2142240) ONLYSIMULATION
; $ESTIMATION METHOD=1 LAPLACIAN NUMERICAL SIGDIG=3  MAXEVAL=0
; POSTHOC  PRINT=1   NOABORT

;$COV

$TABLE ID TIME PRED IPRED IWRES NOPRINT ONEHEADER FILE=sdtab53
$TABLE ID TIME TRM OTC OIC OVL_I OVL_NI VLTOT T_END SVR CURE CENS IPRED DV RR0 TV_c MY_c delta ED50peg ED50rbv ED50drugx K Tv_r NOPRINT ONEHEADER FILE=patab53