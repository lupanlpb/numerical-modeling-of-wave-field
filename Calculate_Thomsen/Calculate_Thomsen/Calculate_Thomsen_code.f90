Program Calculate_Thomsen
  Implicit None
  Real :: c11,c13,c33,c44,c55,c66
  Real :: Ebsl,Gama,Sigma
  
  Open(11,file='parameter.par',status='old')
  Read(11,*)
  Read(11,*) c11,c13,c33,c44,c55,c66
  Close(11)
  
  Ebsl=(c11-c33)/(2*c33)
  Gama=(c66-c44)/(2*c44)
  Sigma=((c13+c44)**2-(c33-c44)**2)/(2*c33**2-2*c33*c44)
  
  Open(12,file='result.dat',status='replace')
  Write(12,'(A)') '--Ebsl-------------Gama----------Sigma------'
  Write(12,*) Ebsl,Gama,Sigma
  Close(12)
  
End Program Calculate_Thomsen