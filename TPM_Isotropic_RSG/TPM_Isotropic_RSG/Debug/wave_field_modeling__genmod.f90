        !COMPILER-GENERATED INTERFACE MODULE: Fri Apr 28 17:49:39 2017
        MODULE WAVE_FIELD_MODELING__genmod
          INTERFACE 
            SUBROUTINE WAVE_FIELD_MODELING(DELT_X,DELT_Z,DELT_H,        &
     &REFLECT_COEFFICIENT,FRE_WAVELET,DELAY_SOURCE_T0,NT,DELT_T,N_SOURCE&
     &,IGE,IG1,IG2,NX,NZ,ML,IS,JS,INPUT_FILE_MODELPAR,OUTPUT_FILE_SNAPX,&
     &OUTPUT_FILE_SNAPZ,OUTPUT_FILE_RECORDX,OUTPUT_FILE_RECORDZ)
              INTEGER(KIND=4) :: ML
              INTEGER(KIND=4) :: NZ
              INTEGER(KIND=4) :: NX
              INTEGER(KIND=4) :: NT
              REAL(KIND=4) :: DELT_X
              REAL(KIND=4) :: DELT_Z
              REAL(KIND=4) :: DELT_H
              REAL(KIND=4) :: REFLECT_COEFFICIENT
              REAL(KIND=4) :: FRE_WAVELET
              REAL(KIND=4) :: DELAY_SOURCE_T0
              REAL(KIND=4) :: DELT_T
              INTEGER(KIND=4) :: N_SOURCE
              REAL(KIND=4) :: IGE
              REAL(KIND=4) :: IG1
              REAL(KIND=4) :: IG2
              INTEGER(KIND=4) :: IS
              INTEGER(KIND=4) :: JS
              CHARACTER(*) :: INPUT_FILE_MODELPAR
              CHARACTER(*) :: OUTPUT_FILE_SNAPX
              CHARACTER(*) :: OUTPUT_FILE_SNAPZ
              CHARACTER(*) :: OUTPUT_FILE_RECORDX
              CHARACTER(*) :: OUTPUT_FILE_RECORDZ
            END SUBROUTINE WAVE_FIELD_MODELING
          END INTERFACE 
        END MODULE WAVE_FIELD_MODELING__genmod
