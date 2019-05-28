Program create_model
  Implicit None
  Character *(80) :: cmdfile
  Real :: xmin,xmax,zmin,zmax
  Real :: delt_x,delt_z
  Character *(80) :: Output_file_modelpar
  Integer :: nx,nz
  Integer :: ml
  Real,Allocatable :: c11(:,:),c13(:,:),c33(:,:),c44(:,:),c66(:,:)
  Real,Allocatable :: density(:,:)
  
  cmdfile='model_parameter.par'
  Call Read_par(cmdfile,xmin,xmax,zmin,zmax,delt_x,delt_z,Output_file_modelpar)
  nx=int((xmax-xmin)/delt_x)
  nz=int((zmax-zmin)/delt_z)
  Allocate(c11(nx,nz),c13(nx,nz),c33(nx,nz),c44(nx,nz),c66(nx,nz),density(nx,nz))
  Call Calcu_model(delt_x,delt_z,nx,nz,c11,c13,c33,c44,c66,density,Output_file_modelpar)
  Deallocate(c11,c13,c33,c44,c66,density)
  
End Program create_model
  

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

  
Subroutine Calcu_model(delt_x,delt_z,nx,nz,c11,c13,c33,c44,c66,density,Output_file_modelpar)
  Implicit None
  Real :: delt_x,delt_z
  Integer :: nx,nz
  Real :: c11(nx,nz),c13(nx,nz),c33(nx,nz),c44(nx,nz),c66(nx,nz)
  Real :: density(nx,nz)
  Character *(*) :: Output_file_modelpar
  Integer :: i,j
  
  Open(11,file=Output_file_modelpar,status='replace')
  Write(11,'(A)') '-------x-----------z---------c11---------c13---------c33----------c44-------density------'
  Do j=1,nz
    Do i=1,nx
      IF(j<176) Then
        c11(i,j)=26.4E9
        c13(i,j)=6.11E9
        c33(i,j)=15.6E9
        c44(i,j)=4.38E9
        c66(i,j)=6.84E9
        density(i,j)=2000
      Else
        c11(i,j)=52.2E9
        c13(i,j)=12.3E9
        c33(i,j)=30.9E9
        c44(i,j)=9.83E9
        c66(i,j)=15.1E9
        density(i,j)=2400
      End If
    End Do
  End Do
  Do j=1,nz
    Do i=1,nx
      Write(11,'(7E12.3/)') (i-1)*delt_x,(j-1)*delt_z,c11(i,j),c13(i,j),c33(i,j),c44(i,j),density(i,j)
    End Do
  End Do
  Close(11)
  
End Subroutine Calcu_model
