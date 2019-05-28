        !COMPILER-GENERATED INTERFACE MODULE: Tue May 09 21:12:24 2017
        MODULE CALCU_MODEL__genmod
          INTERFACE 
            SUBROUTINE CALCU_MODEL(DELT_X,DELT_Z,NX,NZ,C11,C13,C33,C44, &
     &C66,P11,P12,P22,R,Q1,Q3,B11,B22,B33,OUTPUT_FILE_MODELPAR)
              REAL(KIND=4) :: DELT_X
              REAL(KIND=4) :: DELT_Z
              INTEGER(KIND=4) :: NX
              INTEGER(KIND=4) :: NZ
              REAL(KIND=4) :: C11
              REAL(KIND=4) :: C13
              REAL(KIND=4) :: C33
              REAL(KIND=4) :: C44
              REAL(KIND=4) :: C66
              REAL(KIND=4) :: P11
              REAL(KIND=4) :: P12
              REAL(KIND=4) :: P22
              REAL(KIND=4) :: R
              REAL(KIND=4) :: Q1
              REAL(KIND=4) :: Q3
              REAL(KIND=4) :: B11
              REAL(KIND=4) :: B22
              REAL(KIND=4) :: B33
              CHARACTER(*) :: OUTPUT_FILE_MODELPAR
            END SUBROUTINE CALCU_MODEL
          END INTERFACE 
        END MODULE CALCU_MODEL__genmod
