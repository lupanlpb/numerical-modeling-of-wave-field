        !COMPILER-GENERATED INTERFACE MODULE: Wed May 10 11:14:51 2017
        MODULE BOND_TRANSFORM__genmod
          INTERFACE 
            SUBROUTINE BOND_TRANSFORM(PATH,D,C,THETA,PHI,MODEL_PARA_FILE&
     &)
              CHARACTER(*) :: PATH
              REAL(KIND=4) :: D(1:6,1:6)
              REAL(KIND=4) :: C(1:6,1:6)
              REAL(KIND=4) :: THETA
              REAL(KIND=4) :: PHI
              CHARACTER(*) :: MODEL_PARA_FILE
            END SUBROUTINE BOND_TRANSFORM
          END INTERFACE 
        END MODULE BOND_TRANSFORM__genmod
