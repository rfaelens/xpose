;; 1. Based on:          036
;; 2. Description:       Used step function instead of Lag to improve fit in bile release
;; 3. Label:             037
;; 4. Structural model:  Para: 2CMT,1TR | Exclude Meal 7
;; 5. Covariate model:   SEXSLP, T2DMSLP
;; 6. IIV:               CL, VC, BA, KG0, SLP, KDJ
;; 7. IOV:               na
;; 8. Residual Var:      Combined-LLOQ/2
;; 9. Estimation:        FOCE-I
$PROBLEM    New Gastric Emptying Model
$INPUT      EXLD PID ID2 ID STUDY T2DM MEAL TIME FLG DV AMT RATE BQL
            LLOQ CMT MDV EVID SEX AGE HT WT BMI
$DATA      ../../ProducedData/POOL_8MEALS_v5_COV.csv IGNORE=@
            IGNORE=(EXLD>0) IGNORE=(FLG==1) IGNORE=(BQL==1)
            IGNORE=(MEAL==7)
$ABBREVIATED DERIV2=NO
$SUBROUTINE ADVAN13 TOL=6 ; Fine
$MODEL      COMP=(DEPOT) ; 1. Paracetamol in Stomach
            COMP=(TRANS) ; 2. Paracetamol Small Intestine/Duodenum
            COMP=(CENTRAL,DEFOBS) ; 3. Paracetamol Central Compartment
            COMP=(PERIPH) ; 4. Paracetamol Peripheral Compartment
            COMP=(GLUC) ; 5. Calories in intestine
$PRIOR      NWPRI
$PK       
;_________________________________ Covariates _________________________________;     
WOMAN = 0
IF(SEX.EQ.2) WOMAN = 1

DIAB = 0
IF(T2DM.EQ.2) DIAB = 1

;______________________________ Meals Composition _____________________________;     
GLUC = 0                 ; Amount of carbohydrates (g)
IF (MEAL==4) GLUC = 107
IF (MEAL==5) GLUC = 93
IF (MEAL==6) GLUC = 32

PROT = 0                 ; Amount of proteins (g)
IF (MEAL==4) PROT = 13
IF (MEAL==5) PROT = 11
IF (MEAL==6) PROT = 3

FAT  = 0                 ; Amount of fat (g)
IF (MEAL==4) FAT  = 2.5
IF (MEAL==5) FAT  = 10
IF (MEAL==6) FAT  = 40
                                     
;______________________ Parameters with prior information______________________;  
 HLKA   = THETA(1)                                   ; Absorption half-life (Clements et al 1978)
 KA     = LOG(2)/HLKA

;________________________ Disposition related Parameters ______________________;  
 TVCL   = THETA(2)
 CL     = TVCL*EXP(ETA(1))                           ; Clearance total
 
 TVVC   = THETA(3)
 VC     = TVVC*EXP(ETA(2))                           ; Volume of central compartment
 
 VP     = THETA(4)                                   ; Volume of peripheral compartment
 
 Q      = THETA(5)                                   ; Distribution Clearance
 
;________________________ Absorption related Parameters _______________________;    
 F1     = 1*EXP(ETA(3))                              ; Relative Bioavailability
 
 IF(STUDY.EQ.3) THEN
  D1    = 1                                          ; The whole dose in 1 min
 ENDIF

 IF(STUDY.EQ.2) THEN
  D1    = 0.001                                      ; No infusions with OGTT meals
  T50   = THETA(6)                                   ; Short delay with OGTT meals
ENDIF
 
 IF(STUDY.EQ.1) THEN
  D1    = 0.001                                      ; No infusions with Fat meals
  T50   = THETA(7)                                   ; Longer delay with Fat meals
ENDIF
 
 TVKG0  = LOG(2)/THETA(8)
 KG0    = TVKG0*EXP(ETA(4))                          ; Maximal rate of Gastric Emptying

 TVSLP  = THETA(9)*(1+THETA(11)*WOMAN)*(1+THETA(12)*DIAB)

 SLP    = TVSLP*EXP(ETA(5))                          ; Linear slope of calories inhibition on KG

 TVKDJ  = LOG(2)/THETA(10)                           ; K duodenum jejunum (min-1)
 KDJ    = TVKDJ*EXP(ETA(6)) 

 STEEP  = THETA(13)                                  ; Steepness of the onset function
 
;___________________________ Calories Parameters ______________________________; 
 RAMAXD = 0.573*4.1                                  ; Maximum rate of absorption Kcal in intestine
 KM     = 6.28*4.1                                   ; 50 % of absorption rate (Kcal)

;_____________________________ Meal Parameters ________________________________; 
 KCAL  = 0                                           ; Control calories
 IF(MEAL==1) KCAL = 25*4.1                           ; OGTT 25g calories
 IF(MEAL==2) KCAL = 75*4.1                           ; OGTT 75g calories
 IF(MEAL==3) KCAL = 125*4.1                          ; OGTT 125g calories
 IF(MEAL>=4) KCAL = GLUC*4.1 + PROT*4.1 + FAT*8.8    ; Equivalence calories

;__________________________ Paracetamol Parameters ____________________________; 
 IF(NEWIND.NE.2) FLAG = 0                            ; Reset flag for each new individuals
 IF(AMT.GT.0.AND.FLAG.EQ.0) FLAG = 1                 ; Flag of the first Dose
 IF(EVID.EQ.1.OR.EVID.EQ.4) TDOS = TIME              ; Compile time after dose (TAD)
 DOZ    = 9.9231                                     ; Theoretical dose of paracetamol (mMol)
 DPARA  = F1*DOZ*FLAG                                ; 'Real' dose of paracetamol

$DES                                                  
; Compartment Amounts (Safety)
 X1 = A(1)                                           ; Amount of Paracetamol in the stomach (mMol)
 X2 = A(2)                                           ; Amount of Paracetamol in the Intestine (mMol)
 X3 = A(3)                                           ; Amount of Paracetamol in the Central Compartment (mMol)
 X4 = A(4)                                           ; Amount of Paracetamol in the Peripheral Compartment (mMol)
 X5 = A(5)                                           ; Calories in the intestine (Kcal)
 IF (X5.LE.0) X5 = 1E-10

; Gastric delay
 TAD1    = T - TDOS                                  ; Time after dose (TAD)
 LAG1    = 1                                         ; No onset water
 IF(MEAL.GT.0) THEN
  LAG1    = 1/(1+EXP(-STEEP*(TAD1-T50)))             ; Onset Time stomach (min)
 ENDIF

;______________________________ Paracetamol Model _____________________________; 
 KG1     =  KG0*(1+SLP*X5)

; Eq Diff
 DADT(1) = -KG1*X1*LAG1                              ; Stomach Paracetamol (mMol)
 DADT(2) =  KG1*X1*LAG1 - KA*X2                      ; Duodenum/Small Intestine Paracetamol (mMol)
 DADT(3) =  KA*X2 - ((CL+Q)/VC)*X3 + (Q/VP)*X4       ; Central Paracetamol (mMol)
 DADT(4) =  (Q/VC)*X3 - (Q/VP)*X4                    ; Peripheral Paracetamol (mMol)

;_______________________________ Calories Model _______________________________;
; Glucose Stomach
 AGS1    = 0                                         ; Safety to avoid dividing by 0
 IF(FLAG==1) AGS1 = (X1/DPARA)*KCAL                  ; Calories in the stomach (Kcal)
 
; Eq Diff
 RAD     = RAMAXD*X5/(KM+X5)                         ; Rate of Absorption in intestine
 DADT(5) = KG1*AGS1*LAG1 -RAD -KDJ*X5                ; Calories in intestine (Kcal)

$ERROR        
; Compartment Amounts
 A1 = A(1)                                           ; Amount of Paracetamol in the stomach (mMol)
 A2 = A(2)                                           ; Amount of Paracetamol in the Intestine (mMol)
 A3 = A(3)                                           ; Amount of Paracetamol in the Central Compartment (mMol)
 A4 = A(4)                                           ; Amount of Paracetamol in the Peripheral Compartment (mMol)
 A5 = A(5)                                           ; Calories in the intestine (Kcal)
 IF (A5.LE.0) A5 = 1E-10

; Glucose Stomach
 AGS    = 0                                          ; Safety to avoid dividing by 0
 IF(FLAG==1) AGS = (A1/DPARA)*KCAL                   ; Calories in the stomach (Kcal)

; Gastric delay
 TAD    = TIME - TDOS                                ; Time after dose (TAD)
 LAG    = 1                                          ; No onset water
 IF(MEAL.GT.0) THEN
  LAG    = 1/(1+EXP(-STEEP*(TAD-T50)))               ; Onset Time stomach (min)
 ENDIF

; Paracetamol Fit
 CP    = A3/VC                                       ; Predicted Plasma concentration (mMol/L)
 IPRED = CP
 Y     = IPRED * (1+EPS(1)) + EPS(2)*(LLOQ/2)**2
 W     = SQRT((IPRED*SQRT(SIGMA(1,1)))**2 + (LLOQ/2)**2) ; Weighting
 IF (W<=0) W = 1E-5                                  ; Safety for NaN
 IRES  = DV-IPRED                                    ; Individual residual
 IWRES = IRES/W                                      ; Individual Weighted residual

$THETAP  6.779 FIX    ; HLKA prior
$THETAPV  0.808  FIX  ; HLKA prior uncertainty
$THETA  
 (0,8.314860) ; Th1. HLKA (min-1)
 (0,0.437501) ; Th2. CL (L/min)
 (0,11.56370) ; Th3. Vc (L)
 (0,54.62910) ; Th4. Vp (L)
 (0,1.788680) ; Th5. Q  (L/min)
 (0,8.967270) ; Th6. Lag Ogtt (min)
 (0,14.53210) ; Th7. Lag Fat (min)
 (0,1.487240) ; Th8. KG Half-life (min)
 -0.01937     ; Th9. Slp calories feedback (na)
 (0,23.88510) ; Th10. HL Kdj (min)
 0.284149     ; Th11. SEX on Slp
 -0.12693     ; Th12. T2DM on Slp
 (0,0.659120) ; Th13. Onset steepness

$OMEGA  
 0.029528     ; Om1.1. CL
 0.922095     ; Om2.2. Vc
 0.031843     ; Om3.3. BA
 1.415040     ; Om4.4. KG0
 0.039306     ; Om5.5. Slp
 0.272711     ; Om6.6. KDJ

$SIGMA  
 0.022582     ; Sig1.1. Proportional Error
 1  FIX       ; Sig2.2. Additive Error

;$ESTIMATION METHOD=1 SIGL=3 NSIG=1 INTER MAXEVAL=9999 PRINT=1 NOABORT ; Fast
$ESTIMATION METHOD=1 SIGL=6 NSIG=2 INTER MAXEVAL=9999 PRINT=1 NOABORT ; Fine
;$ESTIMATION METHOD=IMP EONLY=1 ISAMPLE=1000 NITER=5
;$COVARIANCE UNCONDITIONAL MATRIX=R
$TABLE      ID STUDY T2DM MEAL TIME DV AMT CMT MDV EVID IPRED CWRES
            IWRES NOPRINT ONEHEADER FILE=sdtab037
$TABLE      ID TIME MDV A1 A2 A3 A4 A5 ETAS(1:7) CL VC KG0 KG1 KA F1
            T50 STEEP LAG D1 SLP KDJ WOMAN DIAB CP DPARA AGS KCAL
            NOAPPEND NOPRINT ONEHEADER FILE=patab037

