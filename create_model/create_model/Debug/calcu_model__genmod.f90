        !COMPILER-GENERATED INTERFACE MODULE: Sun May 28 22:20:46 2017
        MODULE CALCU_MODEL__genmod
          INTERFACE 
            SUBROUTINE CALCU_MODEL(DELT_X,DELT_Z,NX,NZ,C11,C13,C33,C44, &
     &C66,DENSITY,OUTPUT_FILE_MODELPAR)
              INTEGER(KIND=4) :: NZ
              INTEGER(KIND=4) :: NX
              REAL(KIND=4) :: DELT_X
              REAL(KIND=4) :: DELT_Z
              REAL(KIND=4) :: C11(NX,NZ)
              REAL(KIND=4) :: C13(NX,NZ)
              REAL(KIND=4) :: C33(NX,NZ)
              REAL(KIND=4) :: C44(NX,NZ)
              REAL(KIND=4) :: C66(NX,NZ)
              REAL(KIND=4) :: DENSITY(NX,NZ)
              CHARACTER(*) :: OUTPUT_FILE_MODELPAR
            END SUBROUTINE CALCU_MODEL
          END INTERFACE 
        END MODULE CALCU_MODEL__genmod
