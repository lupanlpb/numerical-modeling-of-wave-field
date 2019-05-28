Subroutine create_model()
  Implicit None
  Character *(80) :: cmdfile
  Real :: xmin,xmax,zmin,zmax
  Real :: delt_x,delt_z
  Real :: Lamda_2Miu,Lamda,Miu    !Lamda_2Miu,Lamda,Miu为刚度系数
  Real :: p11,p12,p22             !p11,p12,p22分别为固相密度、流相密度、耦合密度
  Real :: R                       !R为描述孔隙流体的弹性参数
  Real :: Q                       !Q为耦合参数
  Real :: b                       !b为耗散参数
  Character *(80) :: Output_file_modelpar
  Integer :: nx,nz
  
  cmdfile='model_parameter.par'
  Call Read_par(cmdfile,xmin,xmax,zmin,zmax,delt_x,delt_z,Lamda_2Miu,&
                Lamda,Miu,p11,p12,p22,R,Q,b,Output_file_modelpar)
  nx=int((xmax-xmin)/delt_x)
  nz=int((zmax-zmin)/delt_z)
  Call Calcu_model(delt_x,delt_z,nx,nz,Lamda_2Miu,Lamda,Miu,p11,p12,p22,R,Q,b,Output_file_modelpar)
  
End Subroutine create_model
  

Subroutine Read_par(cmdfile,xmin,xmax,zmin,zmax,delt_x,delt_z,Lamda_2Miu,&
                    Lamda,Miu,p11,p12,p22,R,Q,b,Output_file_modelpar)
  Implicit None
  Character *(*) :: cmdfile
  Real :: xmin,xmax,zmin,zmax
  Real :: delt_x,delt_z
    Real :: Lamda_2Miu,Lamda,Miu    !Lamda_2Miu,Lamda,Miu为刚度系数
  Real :: p11,p12,p22             !p11,p12,p22分别为固相密度、流相密度、耦合密度
  Real :: R                       !R为描述孔隙流体的弹性参数
  Real :: Q                       !Q为耦合参数
  Real :: b                       !b为耗散参数
  Character *(80) :: Output_file_modelpar
  Character(len=18) :: path
  
  path='create_model_file\'
  
  Open(11,file=path//cmdfile,status='old') 
  Read(11,*) xmin,xmax,zmin,zmax
  Read(11,*) delt_x,delt_z
  Read(11,*) Lamda_2Miu,Lamda,Miu
  Read(11,*) p11,p12,p22
  Read(11,*) R
  Read(11,*) Q
  Read(11,*) b
  Read(11,*) Output_file_modelpar
  Close(11)
  
End Subroutine Read_par

  
Subroutine Calcu_model(delt_x,delt_z,nx,nz,Lamda_2Miu,Lamda,Miu,p11,p12,p22,R,Q,b,Output_file_modelpar)
  Implicit None
  Real :: delt_x,delt_z
  Integer :: nx,nz
  Real :: Lamda_2Miu,Lamda,Miu    !Lamda_2Miu,Lamda,Miu为刚度系数
  Real :: p11,p12,p22             !p11,p12,p22分别为固相密度、流相密度、耦合密度
  Real :: R                       !R为描述孔隙流体的弹性参数
  Real :: Q                       !Q为耦合参数
  Real :: b                       !b为耗散参数
  Character *(*) :: Output_file_modelpar
  Integer :: i,j
  
  Open(11,file=Output_file_modelpar,status='replace')
  Write(11,'(A)') '-------x-----------z-------Lamda_2Miu----Lamda--------Miu---------p11---------p12--------p22-----------R-----------Q-----------b-----'
  Do j=1,nz
    Do i=1,nx
      Write(11,'(11E12.3/)') (i-1)*delt_x,(j-1)*delt_z,Lamda_2Miu,Lamda,Miu,p11,p12,p22,R,Q,b
    End Do
  End Do
  Close(11)
  
End Subroutine Calcu_model
