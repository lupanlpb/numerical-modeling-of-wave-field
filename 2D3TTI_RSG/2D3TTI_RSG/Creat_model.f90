Subroutine Create_model()
  Implicit None
  Character *(80) :: cmdfile
  Real :: xmin,xmax,zmin,zmax
  Real :: delt_x,delt_z
  Character *(80) :: Output_file_modelpar
  Integer :: nx,nz
  Integer :: ml
  Real :: c(6,6)
  Real,Allocatable :: density(:,:)
  
  cmdfile='model_parameter.par'
  Call Read_par(cmdfile,xmin,xmax,zmin,zmax,delt_x,delt_z,Output_file_modelpar)
  nx=int((xmax-xmin)/delt_x)
  nz=int((zmax-zmin)/delt_z)
  Allocate(density(nx,nz))
  Call Calcu_model(delt_x,delt_z,nx,nz,c,density,Output_file_modelpar)
  Deallocate(density)
  
End Subroutine create_model
  

Subroutine Read_par(cmdfile,xmin,xmax,zmin,zmax,delt_x,delt_z,Output_file_modelpar)
  Implicit None
  Character *(*) :: cmdfile
  Real :: xmin,xmax,zmin,zmax
  Real :: delt_x,delt_z
  Character *(*) :: Output_file_modelpar
  
  Open(11,file=cmdfile,status='old') 
  Read(11,*) xmin,xmax,zmin,zmax
  Read(11,*) delt_x,delt_z
  Read(11,*) Output_file_modelpar
  Close(11)
  
End Subroutine Read_par

  
Subroutine Calcu_model(delt_x,delt_z,nx,nz,c,density,Output_file_modelpar)
  Implicit None
  Real :: delt_x,delt_z
  Integer :: nx,nz
  Real :: c(6,6)
  Real :: density(nx,nz)
  Character(len=80) :: Input_c_TTI
  Character *(*) :: Output_file_modelpar
  Integer :: i,j
  
  Input_c_TTI='output_c_TTI.dat'
  density=2400
  
  Open(12,file=Input_c_TTI,status='old')
  Do i=1,6
    Read(12,*) c(i,:)
  End Do
  Close(12)
  
  Open(11,file=Output_file_modelpar,status='replace')
  Write(11,'(A)') '------------------------------c_TTI---------------------------'
  Do i=1,6
    Write(11,'(6E12.3/)') c(i,:)
  End Do
  Write(11,'(A)') '-------x-----------z---------density------'
  Do j=1,nz
    Do i=1,nx
      Write(11,'(3E12.3/)') (i-1)*delt_x,(j-1)*delt_z,density(i,j)
    End Do
  End Do
  Close(11)
  
End Subroutine Calcu_model
