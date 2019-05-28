Program Main
  Implicit None
  
  Call BQ_transform()
  
End Program Main
  
  
Subroutine BQ_transform()
  Implicit None
  Character *(80) :: cmdfile
  Real :: xmin,xmax,zmin,zmax
  Real :: delt_x,delt_z
  Real :: Theta,Phi
  Real :: c11,c13,c33,c44,c66     !c11,c13,c33,c44,c66为刚度系数
  Real :: p11,p12,p22             !p11,p12,p22分别为固相密度、流相密度、耦合密度
  Real :: R                       !R为描述孔隙流体的弹性参数
  Real :: Q1,Q2,Q3                !Q1,Q3为耦合参数
  Real :: b11,b22,b33             !!b11,b33为耗散参数
  Character *(80) :: Output_file_modelpar
  Integer :: nx,nz
  
  cmdfile='model_parameter.par'
  Call Read_par(cmdfile,xmin,xmax,zmin,zmax,delt_x,delt_z,Theta,Phi,c11,c13,c33,c44,&
                c66,p11,p12,p22,R,Q1,Q3,b11,b33,Output_file_modelpar)
  nx=int((xmax-xmin)/delt_x)
  nz=int((zmax-zmin)/delt_z)
  Call B_trans(b11,b22,b33,Theta,Phi)
  Call Q_trans(Q1,Q2,Q3,Theta,Phi)
  Call Calcu_model(delt_x,delt_z,nx,nz,c11,c13,c33,c44,c66,p11,p12,p22,R,Q1,Q3,b11,b22,b33,Output_file_modelpar)
  
End Subroutine BQ_transform
  

Subroutine Read_par(cmdfile,xmin,xmax,zmin,zmax,delt_x,delt_z,Theta,Phi,c11,c13,c33,c44,&
                c66,p11,p12,p22,R,Q1,Q3,b11,b33,Output_file_modelpar)
  Implicit None
  Character *(*) :: cmdfile
  Real :: xmin,xmax,zmin,zmax
  Real :: delt_x,delt_z
  Real :: Theta,Phi
  Real :: c11,c13,c33,c44,c66     
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
  Read(11,*) Theta,Phi
  Read(11,*) c11,c13,c33,c44,c66
  Read(11,*) p11,p12,p22
  Read(11,*) R
  Read(11,*) Q1,Q3
  Read(11,*) b11,b33
  Read(11,*) Output_file_modelpar
  Close(11)
  
End Subroutine Read_par
  
  
Subroutine B_trans(b11,b22,b33,Theta,Phi)
  Implicit None
  Real :: Theta,Phi
  Real :: b11,b22,b33
  Real :: B1(1:3,1:3),B2(1:3,1:3)
  Real :: A(1:3,1:3),At(1:3,1:3)
  Real :: temp(1:3,1:3)
  Integer :: i,j,k
  
  b22=b11
  B1=0.0
  B2=0.0
  B1(1,1)=b11
  B1(2,2)=b22
  B1(3,3)=b33
  
  
  A(1,1)=cosd(Theta)*sind(Phi)
  A(1,2)=cosd(Phi)
  A(1,3)=sind(Theta)*sind(Phi)
  A(2,1)=-cosd(Theta)*cosd(Phi)
  A(2,2)=sind(Phi)
  A(2,3)=-sind(Theta)*cosd(Phi)
  A(3,1)=-sind(Theta)
  A(3,2)=0.0
  A(3,3)=cosd(Theta)
  
  Open(11,file='A.dat')
  Do i=1,3
    Write(11,*) A(i,:)
  End Do
  Close(11)
  
  Do i=1,3
    Do j=1,3
      At(j,i)=A(i,j)                    !计算A矩阵转置At
    End Do
  End Do
  
  Do i=1,3
    Do j=1,3
      temp(i,j)=0.0
      Do k=1,3
        temp(i,j)=temp(i,j)+A(i,k)*B1(k,j)   !计算A*B1
      End Do
    End Do
  End Do
  
  Open(11,file='AB1.dat')
  Do i=1,3
    Write(11,*) temp(i,:)
  End Do
  Close(11)
  
  Do i=1,3
    Do j=1,3
      B2(i,j)=0.0
      Do k=1,3
        B2(i,j)=B2(i,j)+temp(i,k)*At(k,j)     !计算A*B1*At
      End Do
    End Do
  End Do
  
  Open(11,file='AB1At.dat')
  Do i=1,3
    Write(11,*) B2(i,:)
  End Do
  Close(11)
  
  b11=B2(1,1)
  b22=B2(2,2)
  b33=B2(3,3)

End Subroutine B_trans


Subroutine Q_trans(Q1,Q2,Q3,Theta,Phi)
  Implicit None
  Real :: Theta,Phi
  Real :: Q1,Q2,Q3
  Real :: Q11(1:6),Q22(1:6)
  Real :: R(1:6,1:6)
  Real :: a1,a2,a3
  Real :: b1,b2,b3
  Real :: e1,e2,e3
  Integer :: i,j
  
  Q2=Q1
  Q11=0.0
  Q22=0.0
  Q11(1)=Q1
  Q11(2)=Q2
  Q11(3)=Q3
  
  a1=cosd(Theta)*sind(Phi)
  a2=-cosd(Theta)*cosd(Phi)
  a3=-sind(Theta)
  b1=cosd(Phi)
  b2=sind(Phi)
  b3=0
  e1=sind(Theta)*sind(Phi)
  e2=-sind(Theta)*cosd(Phi)
  e3=cosd(Theta)
  
  R(1,1)=a1**2
  R(1,2)=b1**2
  R(1,3)=e1**2
  R(1,4)=2*b1*e1
  R(1,5)=2*a1*e1
  R(1,6)=2*a1*b1
  R(2,1)=a2**2
  R(2,2)=b2**2
  R(2,3)=e2**2
  R(2,4)=2*b2*e2
  R(2,5)=2*a2*e2
  R(2,6)=2*a2*b2
  R(3,1)=a3**2
  R(3,2)=b3**2
  R(3,3)=e3**2
  R(3,4)=2*b3*e3
  R(3,5)=2*a3*e3
  R(3,6)=2*a3*b3
  R(4,1)=a2*a3
  R(4,2)=b2*b3
  R(4,3)=e2*e3
  R(4,4)=b2*e3+b3*e2
  R(4,5)=e2*a3+e3*a2
  R(4,6)=a2*b3+a3*b2
  R(5,1)=a3*a1
  R(5,2)=b3*b1
  R(5,3)=e3*e1
  R(5,4)=b3*e1+b1*e3
  R(5,5)=e3*a1+e1*a3
  R(5,6)=a3*b1+a1*b3
  R(6,1)=a1*a2
  R(6,2)=b1*b2
  R(6,3)=e1*e2
  R(6,4)=b1*e2+b2*e1
  R(6,5)=e1*a2+e2*a1
  R(6,6)=a1*b2+a2*b1
  
  Do i=1,6
    Q22(i)=0.0
    Do j=1,6
        Q22(i)=Q22(i)+R(i,j)*Q11(j)     !计算R*Q11
    End Do
  End Do
  
  Q1=Q22(1)
  Q2=Q22(2)
  Q3=Q22(3)  
  
End Subroutine Q_trans
  
  
Subroutine Calcu_model(delt_x,delt_z,nx,nz,c11,c13,c33,c44,c66,p11,p12,p22,R,Q1,Q3,b11,b22,b33,Output_file_modelpar)
  Implicit None
  Real :: delt_x,delt_z
  Integer :: nx,nz
  Real :: c11,c13,c33,c44,c66
  Real :: p11,p12,p22             
  Real :: R                       
  Real :: Q1,Q3
  Real :: b11,b22,b33
  Character *(*) :: Output_file_modelpar
  Integer :: i,j
  
  Open(11,file=Output_file_modelpar,status='replace')
  Write(11,'(A)') '-------x-----------z---------c11---------c13---------c33----------c44---------p11---------p12--------p22-----------R-----------Q1--------Q3-----------b11-----------b22--------b33-----'
  Do j=1,nz
    Do i=1,nx
      Write(11,'(15E12.3/)') (i-1)*delt_x,(j-1)*delt_z,c11,c13,c33,c44,p11,p12,p22,R,Q1,Q3,b11,b22,b33
    End Do
  End Do
  Close(11)
  
End Subroutine Calcu_model
