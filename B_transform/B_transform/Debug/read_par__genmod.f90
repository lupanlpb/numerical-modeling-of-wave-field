        !COMPILER-GENERATED INTERFACE MODULE: Tue May 09 21:12:24 2017
        MODULE READ_PAR__genmod
          INTERFACE 
            SUBROUTINE READ_PAR(CMDFILE,XMIN,XMAX,ZMIN,ZMAX,DELT_X,     &
     &DELT_Z,THETA,PHI,C11,C13,C33,C44,C66,P11,P12,P22,R,Q1,Q3,B11,B33, &
     &OUTPUT_FILE_MODELPAR)
              CHARACTER(*) :: CMDFILE
              REAL(KIND=4) :: XMIN
              REAL(KIND=4) :: XMAX
              REAL(KIND=4) :: ZMIN
              REAL(KIND=4) :: ZMAX
              REAL(KIND=4) :: DELT_X
              REAL(KIND=4) :: DELT_Z
              REAL(KIND=4) :: THETA
              REAL(KIND=4) :: PHI
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
              REAL(KIND=4) :: B33
              CHARACTER(LEN=80) :: OUTPUT_FILE_MODELPAR
            END SUBROUTINE READ_PAR
          END INTERFACE 
        END MODULE READ_PAR__genmod
