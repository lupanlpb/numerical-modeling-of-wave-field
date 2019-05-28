Subroutine Bond_B_Q_Transform()
  Implicit None
  Character(len=24) :: path
  Character(len=80) :: cmdfile
  Character(len=80) :: c_VTI_file
  Character(len=80) :: model_para_file
  Character(len=80) :: modelpar
  Real :: c11,c13,c33,c55,c66     !c11,c13,c33,c55,c66为刚度系数
  Real :: xmin,xmax,zmin,zmax
  Real :: delt_x,delt_z
  Real :: Theta,Phi
  Real :: p11,p12,p22             !p11,p12,p22分别为固相密度、流相密度、耦合密度
  Real :: R                       !R为描述孔隙流体的弹性参数
  Real :: Q1,Q2,Q3                !Q1,Q2,Q3为耦合参数
  Real :: b11,b22,b33             !b11,b22,b33为耗散参数
  Real :: D(1:6,1:6)      !VTI刚度矩阵
  Real :: C(1:6,1:6)      !TTI刚度矩阵
  Integer :: i,j,k
  
  
  path='Bond_B_Q_Transform_file\'
  cmdfile='Bond_B_Q_Transform.par'
  Call Read_Bond_B_Q_par(path,cmdfile,Theta,Phi,c11,c13,c33,c55,c66,xmin,xmax,zmin,zmax,&
                         delt_x,delt_z,p11,p12,p22,R,Q1,Q3,b11,b33,c_VTI_file,model_para_file,modelpar)
  
  C=0.0
  D=0.0
  D(1,1)=c11
  D(1,3)=c13
  D(3,3)=c33
  D(5,5)=c55
  D(6,6)=c66
  D(2,2)=D(1,1)
  D(1,2)=D(1,1)-2*D(6,6)
  D(2,1)=D(1,2)
  D(3,1)=D(1,3)
  D(2,3)=D(1,3)
  D(3,2)=D(2,3)
  D(4,4)=D(5,5)
  
  Call Output_c_VTI(path,D,c_VTI_file)  
  Call B_trans(b11,b22,b33,Theta,Phi)
  Call Q_trans(Q1,Q2,Q3,Theta,Phi)
  Call Bond_Transform(path,D,C,Theta,Phi,model_para_file)
  Call Output_c_TTI(xmin,xmax,zmin,zmax,delt_x,delt_z,p11,p12,p22,R,b11,b22,b33,Q1,Q3,C,model_para_file,modelpar)
  
  
End Subroutine Bond_B_Q_Transform
  

Subroutine Read_Bond_B_Q_par(path,cmdfile,Theta,Phi,c11,c13,c33,c55,c66,xmin,xmax,zmin,zmax,&
                             delt_x,delt_z,p11,p12,p22,R,Q1,Q3,b11,b33,c_VTI_file,model_para_file,modelpar)
  Implicit None
  Character *(*) :: path
  Character *(*) :: cmdfile
  Character *(*) :: c_VTI_file
  Character *(*) :: model_para_file
  Character *(*) :: modelpar
  Real :: c11,c13,c33,c55,c66
  Real :: xmin,xmax,zmin,zmax
  Real :: delt_x,delt_z
  Real :: Theta,Phi     
  Real :: p11,p12,p22             
  Real :: R                       
  Real :: Q1,Q3
  Real :: b11,b33
  
  Open(11,file=path//cmdfile,status='old') 
  Read(11,*) 
  Read(11,*) c11,c13,c33,c55,c66
  Read(11,*) xmin,xmax,zmin,zmax
  Read(11,*) delt_x,delt_z
  Read(11,*) Theta,Phi
  Read(11,*) p11,p12,p22
  Read(11,*) R
  Read(11,*) Q1,Q3
  Read(11,*) b11,b33
  Read(11,*) c_VTI_file
  Read(11,*) model_para_file
  Read(11,*) modelpar
  Close(11)
  
End Subroutine Read_Bond_B_Q_par
  
  
Subroutine Output_c_VTI(path,D,c_VTI_file)
  Implicit None
  Character *(*) :: path
  Character *(*) :: c_VTI_file
  Real :: D(1:6,1:6)      !VTI刚度矩阵
  Integer :: i,j
  
  Open(12,file=path//c_VTI_file,status='replace') !输出VTI刚度矩阵和Thomsen系数
  Write(12,'(A)') '---------------------------------c_VTI--------------------------------'
  Do i=1,6
    Write(12,'(6E12.5/)') D(i,:)
  End Do
  Close(12)
  
End Subroutine Output_c_VTI
  
  
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
    
  Do i=1,3
    Do j=1,3
      B2(i,j)=0.0
      Do k=1,3
        B2(i,j)=B2(i,j)+temp(i,k)*At(k,j)     !计算A*B1*At
      End Do
    End Do
  End Do
    
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

  
Subroutine Bond_Transform(path,D,C,Theta,Phi,model_para_file)
  Implicit None
  Character *(*) :: path
  Character *(*) :: model_para_file
  Real :: D(1:6,1:6)      !VTI刚度矩阵
  Real :: C(1:6,1:6)      !TTI刚度矩阵
  Real :: R(1:6,1:6)
  Real :: Rt(1:6,1:6)
  Real :: temp(1:6,1:6)
  Real :: a1,a2,a3
  Real :: b1,b2,b3
  Real :: e1,e2,e3
  Real :: Theta,Phi
  Integer :: i,j,k
    
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
  
  Open(14,file=path//'R.dat',status='replace')
  Do i=1,6
    Write(14,'(6E12.3/)') R(i,:)        !输出Bond变换矩阵R
  End Do
  Close(14)
  
  Do i=1,6
    Do j=1,6
      Rt(j,i)=R(i,j)                    !计算Bond变换矩阵R转置Rt
    End Do
  End Do
  
  Open(15,file=path//'Rt.dat',status='replace')
  Do i=1,6
    Write(15,'(6E12.3/)') Rt(i,:)
  End Do
  Close(15)
  
  Do i=1,6
    Do j=1,6
      temp(i,j)=0.0
      Do k=1,6
        temp(i,j)=temp(i,j)+R(i,k)*D(k,j)   !计算R*D
      End Do
    End Do
  End Do
  
  Do i=1,6
    Do j=1,6
      C(i,j)=0.0
      Do k=1,6
        C(i,j)=C(i,j)+temp(i,k)*Rt(k,j)     !计算R*D*Rt
      End Do
    End Do
  End Do
  
End Subroutine Bond_Transform
  
  
Subroutine Output_c_TTI(xmin,xmax,zmin,zmax,delt_x,delt_z,p11,p12,p22,R,b11,b22,b33,Q1,Q3,C,model_para_file,modelpar)
  Implicit None
  Character *(*) :: model_para_file
  Character *(*) :: modelpar
  Real :: xmin,xmax,zmin,zmax
  Real :: delt_x,delt_z
  Real :: p11,p12,p22
  Real :: b11,b22,b33
  Real :: Q1,Q3
  Real :: R
  Real :: C(1:6,1:6)
  Integer :: i
  
  Open(13,file=model_para_file,status='replace')
  Write(13,'(A)') '---------------------------------c_TTI--------------------------------'
  Do i=1,6
    Write(13,'(6E12.3/)') C(i,:)            !输出TTI刚度矩阵
  End Do
  Write(13,'(A)') '------p11---------p12---------p22----------R-----------Q1---------Q3----------b11---------b22---------b33-----'
  Write(13,'(9E12.3/)') p11,p12,p22,R,Q1,Q3,b11,b22,b33
  Write(13,'(A)') '------xmin--------xmax--------zmin--------zmax---------------'
  Write(13,'(4E12.3/)') xmin,xmax,zmin,zmax
  Write(13,'(A)') '-----delt_x-----delt_z-------'
  Write(13,'(2E12.3/)') delt_x,delt_z
  Write(13,'(A)') '---模型文件---'
  Write(13,*) modelpar
  Close(13)
  
End Subroutine Output_c_TTI