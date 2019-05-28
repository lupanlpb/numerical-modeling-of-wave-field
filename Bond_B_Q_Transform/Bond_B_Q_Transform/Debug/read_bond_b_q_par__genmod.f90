        !COMPILER-GENERATED INTERFACE MODULE: Wed May 10 11:14:51 2017
        MODULE READ_BOND_B_Q_PAR__genmod
          INTERFACE 
            SUBROUTINE READ_BOND_B_Q_PAR(PATH,CMDFILE,THETA,PHI,C11,C13,&
     &C33,C55,C66,XMIN,XMAX,ZMIN,ZMAX,DELT_X,DELT_Z,P11,P12,P22,R,Q1,Q3,&
     &B11,B33,C_VTI_FILE,MODEL_PARA_FILE,MODELPAR)
              CHARACTER(*) :: PATH
              CHARACTER(*) :: CMDFILE
              REAL(KIND=4) :: THETA
              REAL(KIND=4) :: PHI
              REAL(KIND=4) :: C11
              REAL(KIND=4) :: C13
              REAL(KIND=4) :: C33
              REAL(KIND=4) :: C55
              REAL(KIND=4) :: C66
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
              REAL(KIND=4) :: Q1
              REAL(KIND=4) :: Q3
              REAL(KIND=4) :: B11
              REAL(KIND=4) :: B33
              CHARACTER(*) :: C_VTI_FILE
              CHARACTER(*) :: MODEL_PARA_FILE
              CHARACTER(*) :: MODELPAR
            END SUBROUTINE READ_BOND_B_Q_PAR
          END INTERFACE 
        END MODULE READ_BOND_B_Q_PAR__genmod
