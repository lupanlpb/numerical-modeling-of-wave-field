        !COMPILER-GENERATED INTERFACE MODULE: Thu May 04 10:22:52 2017
        MODULE READ_PAR__genmod
          INTERFACE 
            SUBROUTINE READ_PAR(CMDFILE,XMIN,XMAX,ZMIN,ZMAX,DELT_X,     &
     &DELT_Z,VP,VS,DENSITY,OUTPUT_FILE_MODELPAR)
              CHARACTER(*) :: CMDFILE
              REAL(KIND=4) :: XMIN
              REAL(KIND=4) :: XMAX
              REAL(KIND=4) :: ZMIN
              REAL(KIND=4) :: ZMAX
              REAL(KIND=4) :: DELT_X
              REAL(KIND=4) :: DELT_Z
              REAL(KIND=4) :: VP
              REAL(KIND=4) :: VS
              REAL(KIND=4) :: DENSITY
              CHARACTER(*) :: OUTPUT_FILE_MODELPAR
            END SUBROUTINE READ_PAR
          END INTERFACE 
        END MODULE READ_PAR__genmod
