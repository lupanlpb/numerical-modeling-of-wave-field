Subroutine Create_model()
  Implicit None
  Character *(80) :: cmdfile
  Character *(80) :: modelpar
  Real :: C(1:6,1:6)
  Real :: xmin,xmax,zmin,zmax
  Real :: delt_x,delt_z
  Integer :: nx,nz
  Real :: p11,p12,p22
  Real :: b11,b22,b33
  Real :: Q1,Q3
  Real :: R
    
  cmdfile='model_parameter.par'
  Call Read_par(cmdfile,xmin,xmax,zmin,zmax,delt_x,delt_z,p11,p12,p22,R,b11,b22,b33,Q1,Q3,C,modelpar)
  nx=int((xmax-xmin)/delt_x)
  nz=int((zmax-zmin)/delt_z)
  Call Calcu_model(delt_x,delt_z,nx,nz,p11,p12,p22,R,b11,b22,b33,Q1,Q3,C,modelpar)
  
End Subroutine create_model
  

Subroutine Read_par(cmdfile,xmin,xmax,zmin,zmax,delt_x,delt_z,p11,p12,p22,R,b11,b22,b33,Q1,Q3,C,modelpar)
  Implicit None
  Character *(*) :: cmdfile
  Character *(*) :: modelpar
  Real :: C(1:6,1:6)
  Real :: xmin,xmax,zmin,zmax
  Real :: delt_x,delt_z
  Integer :: nx,nz
  Real :: p11,p12,p22
  Real :: b11,b22,b33
  Real :: Q1,Q3
  Real :: R
  Integer :: i
  
  Open(11,file=cmdfile,status='old')
  Read(11,*)
  
  Do i=1,6
    Read(11,*) C(i,:)
  End Do
  
  Read(11,*)
  Read(11,*)
  Read(11,*) p11,p12,p22,R,Q1,Q3,b11,b22,b33
  Read(11,*)
  Read(11,*)
  Read(11,*) xmin,xmax,zmin,zmax
  Read(11,*)
  Read(11,*)
  Read(11,*) delt_x,delt_z
  Read(11,*)
  Read(11,*)
  Read(11,*) modelpar
  Close(11)
  
End Subroutine Read_par

  
Subroutine Calcu_model(delt_x,delt_z,nx,nz,p11,p12,p22,R,b11,b22,b33,Q1,Q3,C,modelpar)
  Implicit None
  Character *(*) :: modelpar
  Real :: C(1:6,1:6)
  Real :: delt_x,delt_z
  Integer :: nx,nz
  Real :: p11,p12,p22
  Real :: b11,b22,b33
  Real :: Q1,Q3
  Real :: R
  Integer :: i,j
    
  Open(12,file=modelpar,status='replace')
  Write(12,'(A)') '------------------------------c_TTI---------------------------'
  Do i=1,6
    Write(12,'(6E12.3/)') C(i,:)
  End Do
  Write(12,'(A)') '-------x-----------z---------p11---------p12--------p22-----------R-----------Q1--------Q3-----------b11-----------b22--------b33-----'
  Do j=1,nz
    Do i=1,nx
      Write(12,'(11E12.3/)') (i-1)*delt_x,(j-1)*delt_z,p11,p12,p22,R,Q1,Q3,b11,b22,b33
    End Do
  End Do
  Close(12)
  
End Subroutine Calcu_model
