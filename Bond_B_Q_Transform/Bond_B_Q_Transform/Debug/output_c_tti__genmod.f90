        !COMPILER-GENERATED INTERFACE MODULE: Wed May 10 11:14:51 2017
        MODULE OUTPUT_C_TTI__genmod
          INTERFACE 
            SUBROUTINE OUTPUT_C_TTI(XMIN,XMAX,ZMIN,ZMAX,DELT_X,DELT_Z,  &
     &P11,P12,P22,R,B11,B22,B33,Q1,Q3,C,MODEL_PARA_FILE,MODELPAR)
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
              CHARACTER(*) :: MODEL_PARA_FILE
              CHARACTER(*) :: MODELPAR
            END SUBROUTINE OUTPUT_C_TTI
          END INTERFACE 
        END MODULE OUTPUT_C_TTI__genmod
