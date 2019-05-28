        !COMPILER-GENERATED INTERFACE MODULE: Thu May 11 11:20:39 2017
        MODULE CALCU_MODEL__genmod
          INTERFACE 
            SUBROUTINE CALCU_MODEL(DELT_X,DELT_Z,NX,NZ,P11,P12,P22,R,B11&
     &,B22,B33,Q1,Q3,C,MODELPAR)
              REAL(KIND=4) :: DELT_X
              REAL(KIND=4) :: DELT_Z
              INTEGER(KIND=4) :: NX
              INTEGER(KIND=4) :: NZ
              REAL(KIND=4) :: P11
              REAL(KIND=4) :: P12
              REAL(KIND=4) :: P22
              REAL(KIND=4) :: R
              REAL(KIND=4) :: B11
              REAL(KIND=4) :: B22
              REAL(KIND=4) :: B33
              REAL(KIND=4) :: Q1
              REAL(KIND=4) :: Q3
              REAL(KIND=4) :: C(1:6,1:6)
              CHARACTER(*) :: MODELPAR
            END SUBROUTINE CALCU_MODEL
          END INTERFACE 
        END MODULE CALCU_MODEL__genmod
