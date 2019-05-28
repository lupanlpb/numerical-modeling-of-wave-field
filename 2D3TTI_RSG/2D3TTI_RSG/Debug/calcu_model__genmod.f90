        !COMPILER-GENERATED INTERFACE MODULE: Thu Apr 20 08:46:01 2017
        MODULE CALCU_MODEL__genmod
          INTERFACE 
            SUBROUTINE CALCU_MODEL(DELT_X,DELT_Z,NX,NZ,C,DENSITY,       &
     &OUTPUT_FILE_MODELPAR)
              INTEGER(KIND=4) :: NZ
              INTEGER(KIND=4) :: NX
              REAL(KIND=4) :: DELT_X
              REAL(KIND=4) :: DELT_Z
              REAL(KIND=4) :: C(6,6)
              REAL(KIND=4) :: DENSITY(NX,NZ)
              CHARACTER(*) :: OUTPUT_FILE_MODELPAR
            END SUBROUTINE CALCU_MODEL
          END INTERFACE 
        END MODULE CALCU_MODEL__genmod
