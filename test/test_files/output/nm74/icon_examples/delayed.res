Thu 05/01/2014 
06:29 PM
$PROB delayed time example
$INPUT WWX TIME  DV AMT RATE CMT EVID MDV
$DATA delayed.csv IGNORE=C

$SUBROUTINES ADVAN13 TRANS1 TOL=8
$MODEL NCOMPARTMENTS=5

$PK
TDELAY=THETA(1)
ALAG3=TDELAY
KEL=THETA(2)
K0=THETA(3)
K1=THETA(4)
SC50=THETA(5)
SMAX=THETA(6)
V=THETA(7)
KS=THETA(8)
A_0(2)=K0/K1
A_0(4)=K0/K1
PI=3.141592654E+00

$DES
DADT(1) = -KEL*A(1)
DADT(2) = K0*(1.0 + SMAX*A(1)/(SC50*V + A(1))) - K1*A(2) &
        + KS*A(2)*SIN(2.0d+00*PI*T/TDELAY)*TDELAY/(2.0d+00*PI)
DADT(3) = -KEL*A(3)
DADT(4) = K0*(1.0 + SMAX*A(3)/(SC50*V + A(3))) - K1*A(4) &
        + KS*A(4)*SIN(2.0d+00*PI*T/TDELAY)*TDELAY/(2.0d+00*PI)
; use this conditional statement to "prime the system", and
; facilitate evaluating initial condition of A(3)
IF(T.lt.TDELAY) THEN
DADT(5)=K1*A(2)
ELSE
DADT(5)=K1*A(2) - K1*A(4)
ENDIF

$ERROR
IPRED=LOG(ABS(A(5))+1.0E-04)
Y=IPRED+ERR(1)

$THETA
;(0.001,23.4155) (0.25 FIXED) (0.0,0.309388) (0.0,0.0497557) (0.0,1.15534) (0.0,72.72273) (1.0 FIXED) (0.05 FIXED)
(0.001,15) (0.25 FIXED) (0.0,0.7) (0.0,0.08) (0.0,2) (0.0,45) (1.0 FIXED) (0.05 FIXED)

$OMEGA 0.01

$EST METHOD=0 PRINT=10 MAXEVAL=1000 NSIG=4
$COV UNCONDITIONAL MATRIX=R
$TABLE TIME DV IPRED NOAPPEND NOPRINT FILE=delayed.tab
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  1) NM-TRAN INFERS THAT THE DATA ARE SINGLE-SUBJECT.
  NONMEM RUN CANNOT BE PARALLELIZED.

 (DATA WARNING   5) RECORD         1, DATA ITEM   3, CONTENTS: 2.35
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:        1 MAY 2014
Days until program expires :5871
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.4.0 alpha3 (nm74a3)
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 delayed time example
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:       28
 NO. OF DATA ITEMS IN DATA SET:   9
 ID DATA ITEM IS DATA ITEM NO.:   9
 DEP VARIABLE IS DATA ITEM NO.:   3
 MDV DATA ITEM IS DATA ITEM NO.:  8
0INDICES PASSED TO SUBROUTINE PRED:
   7   2   4   5   0   0   6   0   0   0   0
0LABELS FOR DATA ITEMS:
 WWX TIME DV AMT RATE CMT EVID MDV .ID.
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 IPRED
0FORMAT FOR DATA:
 (E2.0,3E10.0,E9.0,3E2.0,1F2.0)

 TOT. NO. OF OBS RECS:       25
 TOT. NO. OF INDIVIDUALS:     25
0LENGTH OF THETA:   8
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-02     0.1500E+02     0.1000E+07
  0.2500E+00     0.2500E+00     0.2500E+00
  0.0000E+00     0.7000E+00     0.1000E+07
  0.0000E+00     0.8000E-01     0.1000E+07
  0.0000E+00     0.2000E+01     0.1000E+07
  0.0000E+00     0.4500E+02     0.1000E+07
  0.1000E+01     0.1000E+01     0.1000E+01
  0.5000E-01     0.5000E-01     0.5000E-01
0INITIAL ESTIMATE OF OMEGA:
 0.1000E-01
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:              NO
 COMPRESSED FORMAT:              NO
 SIGDIGITS ETAHAT (SIGLO):                  -1
 SIGDIGITS GRADIENTS (SIGL):                -1
 RELATIVE TOLERANCE (TOL):                  -1
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY (ATOL): -1
 EXCLUDE COV FOR FOCE (NOFCOV):              NO
 RESUME COV ANALYSIS (RESUME):               NO
0TABLES STEP OMITTED:    NO
 NO. OF TABLES:           1
 SEED NUMBER (SEED):    11456
 RANMETHOD:             3
 MC SAMPLES (ESEED):    300
 WRES SQUARE ROOT TYPE (WRESCHOL): EIGENVALUE
0-- TABLE   1 --
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                YES
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 TIME DV IPRED
1DOUBLE PRECISION PREDPP VERSION 7.4.0 alpha3 (nm74a3)

 GENERAL NONLINEAR KINETICS MODEL USING LSODA (ADVAN13)
0MODEL SUBROUTINE USER-SUPPLIED - ID NO. 9999
0MAXIMUM NO. OF BASIC PK PARAMETERS:   9
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         COMP 1       ON         YES        YES        YES        YES
    2         COMP 2       ON         YES        YES        NO         NO
    3         COMP 3       ON         YES        YES        NO         NO
    4         COMP 4       ON         YES        YES        NO         NO
    5         COMP 5       ON         YES        YES        NO         NO
    6         OUTPUT       OFF        YES        NO         NO         NO
0NRD VALUE(S) FROM SUBROUTINE TOL:   8
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            *           *           *           *           *
    2            *           *           *           *           *
    3            *           *           *           *          10
    4            *           *           *           *           *
    5            *           *           *           *           *
    6            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      7
   TIME DATA ITEM IS DATA ITEM NO.:          2
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   4
   DOSE RATE DATA ITEM IS DATA ITEM NO.:     5
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:    6

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0PK SUBROUTINE INDICATES THAT COMPARTMENT AMOUNTS ARE INITIALIZED.
0ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
0DES SUBROUTINE USES FULL STORAGE MODE.
1
 
 
 #TBLN:      1
 #METH: First Order
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           SINGLE-SUBJECT
 SLOW GRADIENT METHOD USED:               YES
 EPS-ETA INTERACTION:                     NO
 NO. OF FUNCT. EVALS. ALLOWED:            1000
 NO. OF SIG. FIGURES REQUIRED:            4
 INTERMEDIATE PRINTOUT:                   YES
 ESTIMATE OUTPUT TO MSF:                  NO
 IND. OBJ. FUNC. VALUES SORTED:           NO
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0
 ETA HESSIAN EVALUATION METHOD (ETADER):  0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    100
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):   100
 NOPRIOR SETTING (NOPRIOR):               OFF
 NOCOV SETTING (NOCOV):                   OFF
 DERCONT SETTING (DERCONT):               OFF
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):-100
 FINAL ETA RE-EVALUATION (FNLETA):        ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):           NO
 NON-INFL. ETA CORRECTION (NONINFETA):    OFF
 RAW OUTPUT FILE (FILE): delayed.ext
 EXCLUDE TITLE (NOTITLE):                 NO
 EXCLUDE COLUMN LABELS (NOLABEL):         NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):    S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):     TSOL
 ADDITIONAL CONVERGENCE TEST (CTYPE=4)?:  NO
 EM OR BAYESIAN METHOD USED:                NONE

 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=NPRED
 RES=NRES
 WRES=NWRES
 IWRS=NIWRES
 IPRD=NIPRED
 IRS=NIRES
 
 MONITORING OF SEARCH:

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   1486.01652487750        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:        7
 NPARAMETR:  1.5000E+01  7.0000E-01  8.0000E-02  2.0000E+00  4.5000E+01  1.0000E-02
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:  -4.3336E+03 -2.3979E+03  3.6762E+03  6.3185E+02 -1.3917E+03 -3.1523E+03
 
0ITERATION NO.:   10    OBJECTIVE VALUE:  -51.3084770996619        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       92
 NPARAMETR:  2.5996E+01  2.5675E-01  7.2700E-02  8.0230E-02  4.6919E+01  6.7749E-02
 PARAMETER:  6.4991E-01 -9.0296E-01  4.3123E-03 -3.1160E+00  1.4176E-01  1.0566E+00
 GRADIENT:   1.6264E+02  3.0966E+01  2.1436E+01 -2.3352E+00  3.1056E+01  1.8023E+01
 
0ITERATION NO.:   20    OBJECTIVE VALUE:  -84.9901097822355        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      179
 NPARAMETR:  2.3467E+01  2.4156E-01  5.0339E-02  1.0703E+00  9.0162E+01  1.2175E-02
 PARAMETER:  5.4757E-01 -9.6395E-01 -3.6324E-01 -5.2517E-01  7.9494E-01  1.9840E-01
 GRADIENT:  -5.8282E+01 -2.2908E+01 -7.2740E-01  3.0125E+00 -2.1496E+01 -4.3746E-01
 
0ITERATION NO.:   29    OBJECTIVE VALUE:  -85.0795013007985        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      263
 NPARAMETR:  2.3432E+01  2.4168E-01  5.0421E-02  1.0775E+00  9.1319E+01  1.2239E-02
 PARAMETER:  5.4606E-01 -9.6347E-01 -3.6161E-01 -5.1846E-01  8.0770E-01  2.0101E-01
 GRADIENT:  -4.2571E-03 -1.3918E-03  3.8955E-03  2.8902E-04 -2.0356E-03  1.5041E-03
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      263
 NO. OF SIG. DIGITS IN FINAL EST.:  4.0
 #TERE:
 Elapsed estimation  time in seconds:     0.69
 Elapsed covariance  time in seconds:     0.13
 Elapsed postprocess time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************      -85.080       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         2.34E+01  2.50E-01  2.42E-01  5.04E-02  1.08E+00  9.13E+01  1.00E+00  5.00E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1     
 
 ETA1
+        1.22E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1     
 
 ETA1
+        1.11E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.38E-01 .........  2.50E-02  2.57E-03  5.45E-01  1.42E+01 ......... .........
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1     
 
 ETA1
+        3.46E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1     
 
 ETA1
+        1.56E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11  
 
 TH 1
+        1.14E-01
 
 TH 2
+       ......... .........
 
 TH 3
+       -1.50E-03 .........  6.26E-04
 
 TH 4
+        1.50E-04 .........  1.91E-05  6.59E-06
 
 TH 5
+       -6.05E-02 ......... -2.70E-03 -1.16E-03  2.97E-01
 
 TH 6
+       -1.64E+00 ......... -2.68E-01 -2.27E-02  5.37E+00  2.00E+02
 
 TH 7
+       ......... ......... ......... ......... ......... ......... .........
 
 TH 8
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM11
+        9.58E-09 .........  1.66E-09  2.78E-10 -5.05E-08 -1.44E-06 ......... .........  1.20E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11  
 
 TH 1
+        3.38E-01
 
 TH 2
+       ......... .........
 
 TH 3
+       -1.78E-01 .........  2.50E-02
 
 TH 4
+        1.73E-01 .........  2.97E-01  2.57E-03
 
 TH 5
+       -3.29E-01 ......... -1.98E-01 -8.28E-01  5.45E-01
 
 TH 6
+       -3.43E-01 ......... -7.58E-01 -6.24E-01  6.96E-01  1.42E+01
 
 TH 7
+       ......... ......... ......... ......... ......... ......... .........
 
 TH 8
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM11
+        8.19E-06 .........  1.92E-05  3.13E-05 -2.68E-05 -2.93E-05 ......... .........  3.46E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11  
 
 TH 1
+        4.56E+01
 
 TH 2
+       ......... .........
 
 TH 3
+        1.11E+03 .........  3.50E+04
 
 TH 4
+       -5.25E+02 ......... -3.22E+04  5.43E+05
 
 TH 5
+       -2.94E+01 ......... -1.02E+03  2.84E+03  4.37E+01
 
 TH 6
+        2.58E+00 .........  7.96E+01 -6.23E+01 -2.46E+00  1.92E-01
 
 TH 7
+       ......... ......... ......... ......... ......... ......... .........
 
 TH 8
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM11
+        7.56E-03 .........  2.36E-01 -3.16E+00 -1.09E-02  9.11E-04 ......... .........  8.34E+04
 
 #CPUT: Total CPU Time in Seconds,        0.780
Stop Time: 
Thu 05/01/2014 
06:29 PM
