$PROB Digoxin PK-HR-CONVERSION DATA
$INPUT ID TRET SEX AGE WT CRCL TIME DV AMT RATE FLG
; TRET is treatment, 1=Digoxin, 0=placebo
; FLG is FLAG, 0=HR data, 1=Digoxin conc, 3=Conversion, 4=Censored
$DATA dig_all1.csv IGNORE=@ 
$SUBROUTINES ADVAN6 TRANS1 TOL=5
$MODEL COMP = CENT
       COMP = PERI
       COMP = EFF
       COMP = HAZ
$PK
;---------PK (parameters hardcoded)---------------
       CRC  = CRCL
         IF(CRCL.EQ.-99) CRC = 70     ;missing in treatment group
         IF(CRCL.EQ.0)   CRC = 70     ;missing in placebo group
       FCRC = 1+.01985*(CRC-70)
       CL   = 9.87*FCRC*EXP(ETA(3))
       V1   = 27.8
       Q    = 71.8
       V2   = 444
       S1   = V1
       K12  = Q/V1
       K21  = Q/V2
       K10  = CL/V1
;--------- Effect compartment--------------
       K13  = K10*.001
       K31  = THETA(5)
       S3   = S1*K13/K31

;---------Heart rate (HR)------------------
       BASE = THETA(1)*EXP(ETA(2))
       TEFF = THETA(6)*EXP(ETA(1))
       SLOP = THETA(4)
;---------Sinus conversion-----------------
       BHAZ = THETA(7)


$DES
 DADT(1) = K21*A(2) - (K12+K10) * A(1) ; 2-comp kinetic model
 DADT(2) =-K21*A(2) +  K12      * A(1)
 DADT(3) =-K31*A(3) +  K13      * A(1) ; Effect compartment

    HR   = BASE*(1+TEFF*T)*(1+SLOP*A(3)/S3)  ; Model for heart rate
                       ; Heart rate depends on time from start of study 
                       ; and digoxin concentration in effect compartment

 DADT(4) = BHAZ*EXP(-SEX*THETA(8))     ; Integrate hazard

$ERROR
;-------------PK and HR-----------------
 CONC = A(1)/S1  ; Conc in central compartment
 CE   = A(3)/S3  ; Conc in effect compartment

 IHR   = BASE*(1+TEFF*TIME)*(1+SLOP*CE)  ; HR prediction
 IPRED = IHR       ; For FLG=0  (HR)
 W = THETA(3)*IPRED
  
 IF(FLG.EQ.1) IPRED = CONC  ; For FLG=1, i.e. digoxin conc
 IF(FLG.EQ.1) W = THETA(2)

 IRES = DV-IPRED
 IWRES= IRES/W

 IF(FLG.LT.2) THEN          ; Digoxin conc and HR cont variables
    F_FLAG= 0
    Y  = IPRED+EPS(1)*W
 ENDIF

;------------TIME TO EVENT--------------     

  CHZ   = A(4)              ; Cumulative hazard
  SUR   = EXP(-CHZ)         ; Survival

IF(FLG.EQ.3)    IPRED = SUR*BHAZ*EXP(-THETA(8)*SEX)  ; Survival * hazard (S*h), conversion
IF(FLG.EQ.4)    IPRED = SUR       ; Censored, no conversion during study interval
IF(FLG.GT.2) THEN
                F_FLAG= 1      ; Conversion and censoring events are categorical 
                Y     = IPRED  ; Y is probability for TTE data
ENDIF

$THETA  (0,117.0000) FIX ; 1 BASE HR
$THETA  (0,1.420000) FIX ; 2 W CONC
$THETA  (0,0.100000) FIX ; 3 W HR
$THETA  -0.10300     FIX ; 4 SLOP
$THETA  (0,0.174000) FIX ; 5 KEO
$THETA  -0.00175     FIX ; 6 TIME-EFFECT
$THETA  (0,0.0400)       ; 7 BHAZ FOR CONVERSION
$THETA  .001             ; 8 CONV DEP on SEX
$OMEGA  1.510000  FIX    ; 1 TIME ON BASE IIV
$OMEGA  0.031300  FIX    ; 2 BASE IIV
$OMEGA  0.970000  FIX    ; 3 CL IIV
$SIGMA  1.000000  FIX

$EST MAX=9990 LAPLACIAN MET=1 PRINT=1 MSFO=msfb 
  ;$COV
$TAB ID TIME IPRED IWRES IPRED FLG CHZ SUR ONEHEADER 
     NOPRINT FILE=sdtab810
