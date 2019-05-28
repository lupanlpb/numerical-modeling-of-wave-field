Program BondTransform
  Implicit None
  Character(Len=80) :: cmdfile
  Character(Len=80) :: c_VTI_file
  Character(Len=80) :: c_TTI_file
  Real :: D(1:6,1:6)      !VTI刚度矩阵
  Real :: R(1:6,1:6)
  Real :: Rt(1:6,1:6)
  Real :: C(1:6,1:6)      !TTI刚度矩阵
  Real :: temp(1:6,1:6)
  Real :: a1,a2,a3
  Real :: b1,b2,b3
  Real :: e1,e2,e3
  Real :: c11,c13,c33,c55,c66
  Real :: density
  Real :: Alpha,Beta,Ebsl,Gama,Sigma
  Real :: Theta,Phi
  Integer :: i,j,k
  
  cmdfile='cmd.par'
  C=0.0
  D=0.0
  
  Open(11,file=cmdfile,status='old')    !读取Theta角，Phi角
  Read(11,*)
  Read(11,*) D(1,1),D(1,3),D(3,3),D(5,5),D(6,6)
  Read(11,*) density
  Read(11,*) Theta
  Read(11,*) Phi
  Read(11,*) c_VTI_file
  Read(11,*) c_TTI_file
  Close(11)
  
  D(2,2)=D(1,1)
  D(1,2)=D(1,1)-2*D(6,6)
  D(2,1)=D(1,2)
  D(3,1)=D(1,3)
  D(2,3)=D(1,3)
  D(3,2)=D(2,3)
  D(4,4)=D(5,5)
  
  Alpha=sqrt(D(3,3)/density)
  Beta=sqrt(D(4,4)/density)
  Ebsl=(D(1,1)-D(3,3))/(2*D(3,3))
  Gama=(D(6,6)-D(4,4))/(2*D(4,4))
  Sigma=((D(1,3)+D(4,4))**2-(D(3,3)-D(4,4))**2)/(2*D(3,3)**2-2*D(3,3)*D(4,4))
  
  
  Open(12,file=c_VTI_file,status='replace') !输出VTI刚度矩阵和Thomsen系数
  Do i=1,6
    Write(12,'(6E12.5/)') D(i,:)
  End Do
  Write(12,*) '---Alpha---------Beta-----------Ebsl-------------Gama---------Sigma-----'
  Write(12,*) Alpha,Beta,Ebsl,Gama,Sigma
  Close(12)
  
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
  
  Open(14,file='R.dat',status='replace')
  Do i=1,6
    Write(14,'(6E12.3/)') R(i,:)        !输出Bond变换矩阵R
  End Do
  Close(14)
  
  Do i=1,6
    Do j=1,6
      Rt(j,i)=R(i,j)                    !计算Bond变换矩阵R转置Rt
    End Do
  End Do
  
  Open(15,file='Rt.dat',status='replace')
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
  
  Open(16,file=c_TTI_file,status='replace')
  Do i=1,6
    Write(16,'(6E12.3/)') C(i,:)            !输出TTI刚度矩阵
  End Do
  Close(16)
  
End Program BondTransform