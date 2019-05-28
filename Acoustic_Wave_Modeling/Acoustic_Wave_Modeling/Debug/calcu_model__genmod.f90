        !COMPILER-GENERATED INTERFACE MODULE: Thu May 04 10:22:52 2017
        MODULE CALCU_MODEL__genmod
          INTERFACE 
            SUBROUTINE CALCU_MODEL(DELT_X,DELT_Z,NX,NZ,VP,VS,DENSITY,   &
     &OUTPUT_FILE_MODELPAR)
              REAL(KIND=4) :: DELT_X
              REAL(KIND=4) :: DELT_Z
              INTEGER(KIND=4) :: NX
              INTEGER(KIND=4) :: NZ
              REAL(KIND=4) :: VP
              REAL(KIND=4) :: VS
              REAL(KIND=4) :: DENSITY
              CHARACTER(*) :: OUTPUT_FILE_MODELPAR
            END SUBROUTINE CALCU_MODEL
          END INTERFACE 
        END MODULE CALCU_MODEL__genmod
