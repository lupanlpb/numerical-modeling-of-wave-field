        !COMPILER-GENERATED INTERFACE MODULE: Thu May 11 11:20:39 2017
        MODULE READ_PAR__genmod
          INTERFACE 
            SUBROUTINE READ_PAR(CMDFILE,XMIN,XMAX,ZMIN,ZMAX,DELT_X,     &
     &DELT_Z,P11,P12,P22,R,B11,B22,B33,Q1,Q3,C,MODELPAR)
              CHARACTER(*) :: CMDFILE
              REAL(KIND=4) :: XMIN
              REAL(KIND=4) :: XMAX
              REAL(KIND=4) :: ZMIN
              REAL(KIND=4) :: ZMAX
              REAL(KIND=4) :: DELT_X
              REAL(KIND=4) :: DELT_Z
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
            END SUBROUTINE READ_PAR
          END INTERFACE 
        END MODULE READ_PAR__genmod
