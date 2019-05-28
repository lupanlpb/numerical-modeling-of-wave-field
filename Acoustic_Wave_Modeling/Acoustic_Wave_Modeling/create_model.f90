Subroutine create_model()
  Implicit None
  Character *(80) :: cmdfile
  Real :: xmin,xmax,zmin,zmax
  Real :: delt_x,delt_z
  Real :: C
  Character *(80) :: Output_file_modelpar
  Integer :: nx,nz
  Integer :: ml
  
  cmdfile='model_parameter.par'
  Call Read_par(cmdfile,xmin,xmax,zmin,zmax,delt_x,delt_z,C,Output_file_modelpar)
  nx=int((xmax-xmin)/delt_x)
  nz=int((zmax-zmin)/delt_z)
  Call Calcu_model(delt_x,delt_z,nx,nz,C,Output_file_modelpar)
  
End Subroutine create_model
  

Subroutine Read_par(cmdfile,xmin,xmax,zmin,zmax,delt_x,delt_z,C,Output_file_modelpar)
  Implicit None
  Character *(*) :: cmdfile
  Real :: xmin,xmax,zmin,zmax
  Real :: delt_x,delt_z
  Real :: C
  Character *(*) :: Output_file_modelpar
  
  Open(11,file=cmdfile,status='old') 
  Read(11,*) xmin,xmax,zmin,zmax
  Read(11,*) delt_x,delt_z
  Read(11,*) C
  Read(11,*) Output_file_modelpar
  Close(11)
  
End Subroutine Read_par

  
Subroutine Calcu_model(delt_x,delt_z,nx,nz,C,Output_file_modelpar)
  Implicit None
  Real :: delt_x,delt_z
  Integer :: nx,nz
  Real :: C
  Character *(*) :: Output_file_modelpar
  Integer :: i,j
  
  Open(11,file=Output_file_modelpar,status='replace')
  Write(11,'(A)') '---------x-----------z---------C------'
  Do j=1,nz
    Do i=1,nx
      Write(11,'(5F12.3/)') (i-1)*delt_x,(j-1)*delt_z,C
    End Do
  End Do
  Close(11)
  
End Subroutine Calcu_model
