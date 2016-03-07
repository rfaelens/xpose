$PROBLEM    ggxpose test run
$INPUT      ID OCC DOSE AMT SS II TAD TIME DV CLCR AGE SEX CLASS WT
            ACE MED1 MED2 EVID
$DATA      ggxpose_test_dataset.csv IGNORE=@
$SUBROUTINE ADVAN2 TRANS1
$PK  
   TVCL  = THETA(1) * (1 + THETA(6) * (CLCR - 65))
   CL    = TVCL * EXP(ETA(1))

   TVV   = THETA(2) * WT
   V     = TVV * EXP(ETA(2))  

   TVKA  = THETA(3)
   KA    = TVKA * EXP(ETA(3))

   ALAG1 = THETA(4)

   K     = CL/V
   S2    = V

$ERROR   
 A1    = A(1)
 A2    = A(2)
 
 IPRED = LOG(0.025)
 IF(F.GT.0) IPRED = LOG(F)

 W     = THETA(5)
 IRES  = IPRED - DV
 IWRES = IRES/W
 
 Y     = IPRED + ERR(1) * W
     
$THETA  
 (0,26.5055)            ; Th1. CL
 (0,1.42569)            ; Th2. V
 (0,3.51068)            ; Th3. KA
 (0,0.232223)           ; Th4. LAG
 (0,0.371944)           ; Th5. Add ERR
 (0,0.00651728,0.02941) ; Th6. CRCL on CL

$OMEGA  
 0.0419341              ; Om1.1. CL
 0.0135816              ; Om2.2. V
 2.55683                ; Om3.3. KA

$SIGMA  
 1  FIX                 ; Sig1.1. Err

$ESTIMATION METHOD=1 INTER MAXEVALS=9999 PRINT=5 NOABORT
$TABLE      ID OCC DOSE AMT SS II TIME TAD IPRED CWRES IWRES 
            EVID ONEHEADER NOPRINT FILE=sdtab001
$TABLE      ID SEX CLASS ACE MED1 MED2 ONEHEADER NOPRINT FILE=catab001
$TABLE      ID CLCR AGE WT ONEHEADER NOPRINT FILE=cotab001
$TABLE      ID KA CL V ALAG1 ETAS(1:LAST) ONEHEADER NOPRINT FILE=patab001
