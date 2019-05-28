Subroutine create_model()
  Implicit None
  Character *(80) :: cmdfile
  Real :: xmin,xmax,zmin,zmax
  Real :: delt_x,delt_z
  Real :: c11(2),c13(2),c33(2),c44(2),c66(2)     !c11,c13,c33,c44,c66为刚度系数
  Real :: p11,p12,p22                            !p11,p12,p22分别为固相密度、流相密度、耦合密度
  Real :: R                                      !R为描述孔隙流体的弹性参数
  Real :: Q1,Q3                                  !Q1,Q3为耦合参数
  Real :: b11,b33                                !!b11,b33为耗散参数
  Character *(80) :: Output_file_modelpar
  Integer :: nx,nz
  
  cmdfile='model_parameter.par'
  Call Read_par(cmdfile,xmin,xmax,zmin,zmax,delt_x,delt_z,c11,c13,c33,c44,&
                c66,p11,p12,p22,R,Q1,Q3,b11,b33,Output_file_modelpar)
  nx=int((xmax-xmin)/delt_x)
  nz=int((zmax-zmin)/delt_z)
  Call Calcu_model(delt_x,delt_z,nx,nz,c11,c13,c33,c44,c66,p11,p12,p22,R,Q1,Q3,b11,b33,Output_file_modelpar)
  
End Subroutine create_model
  

Subroutine Read_par(cmdfile,xmin,xmax,zmin,zmax,delt_x,delt_z,c11,c13,c33,c44,&
                c66,p11,p12,p22,R,Q1,Q3,b11,b33,Output_file_modelpar)
  Implicit None
  Character *(*) :: cmdfile
  Real :: xmin,xmax,zmin,zmax
  Real :: delt_x,delt_z
  Real :: c11(2),c13(2),c33(2),c44(2),c66(2)     
  Real :: p11,p12,p22             
  Real :: R                       
  Real :: Q1,Q3
  Real :: b11,b33
  Character *(80) :: Output_file_modelpar
  Character(len=18) :: path
  
  path='create_model_file\'
  
  Open(11,file=path//cmdfile,status='old') 
  Read(11,*) xmin,xmax,zmin,zmax
  Read(11,*) delt_x,delt_z
  Read(11,*) c11(1),c13(1),c33(1),c44(1),c66(1)
  Read(11,*) c11(2),c13(2),c33(2),c44(2),c66(2)
  Read(11,*) p11,p12,p22
  Read(11,*) R
  Read(11,*) Q1,Q3
  Read(11,*) b11,b33
  Read(11,*) Output_file_modelpar
  Close(11)
  
End Subroutine Read_par

  
Subroutine Calcu_model(delt_x,delt_z,nx,nz,c11,c13,c33,c44,c66,p11,p12,p22,R,Q1,Q3,b11,b33,Output_file_modelpar)
  Implicit None
  Real :: delt_x,delt_z
  Integer :: nx,nz
  Real :: c11(2),c13(2),c33(2),c44(2),c66(2)
  Real :: p11,p12,p22             
  Real :: R                       
  Real :: Q1,Q3
  Real :: b11,b33
  Character *(*) :: Output_file_modelpar
  Integer :: i,j
  
  Open(11,file=Output_file_modelpar,status='replace')
  Write(11,'(A)') '-------x-----------z---------c11---------c13---------c33----------c44---------p11---------p12--------p22-----------R-----------Q1--------Q3-----------b11--------b33-----'
  Do j=1,nz
    Do i=1,nx
      If(j<=151) Then
        Write(11,'(14E12.3/)') (i-1)*delt_x,(j-1)*delt_z,c11(1),c13(1),c33(1),c44(1),p11,p12,p22,R,Q1,Q3,b11,b33
      Else If(151<j.and.j<=181) Then  !断距为300m
        If(i<=151) Then
          Write(11,'(14E12.3/)') (i-1)*delt_x,(j-1)*delt_z,c11(2),c13(2),c33(2),c44(2),p11,p12,p22,R,Q1,Q3,b11,b33
        Else
          Write(11,'(14E12.3/)') (i-1)*delt_x,(j-1)*delt_z,c11(1),c13(1),c33(1),c44(1),p11,p12,p22,R,Q1,Q3,b11,b33
        End If
      Else
        Write(11,'(14E12.3/)') (i-1)*delt_x,(j-1)*delt_z,c11(2),c13(2),c33(2),c44(2),p11,p12,p22,R,Q1,Q3,b11,b33
      End If
    End Do
  End Do
  Close(11)
  
End Subroutine Calcu_model
