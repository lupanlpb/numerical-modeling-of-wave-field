Program Main
  Implicit None
  Integer :: row,column
  Character(len=6),Allocatable :: name(:) 
  Real,Allocatable :: dat(:,:)
  Character(len=8) :: Readfile
  
  Readfile='test.txt'
  
  Call Get_col_row(row,column)
  Write(*,"(A5,I6,5X,A7,I2)") "Row=",row,"Column=",column
  
  Allocate(name(1:column),dat(1:row,1:column))
  
  Call Read_data(Readfile,row,column,name,dat)
  Print*,name
  
End Program
  
Subroutine Get_col_row(row,column)
  Integer,Parameter :: length=1000
  Character(len=length) A,B,Filename
  Integer(kind=4)::column, row, K,L
  
  column=0;row=0;K=0;L=1;filename="test.txt"
  
  Open (55,file=Filename)
  Read(55,'(A)') A
  Do I=1, length
    If(A(I:I)==','.or.A(I:I)==' ') Then
      If(I-1>=L)  column=column+1
      L=I+1
    End If
  End Do
  Close(55)
  
  Open(55,file=Filename)
  Do
    Read(55,*,iostat=k) B
     If( k /= 0 ) Exit
         row = row + 1
  End Do
  Close(55)

End Subroutine Get_col_row

  
Subroutine Read_data(Readfile,row,column,name,dat)
  Implicit None
  Integer :: row,column
  Character(len=6) :: name(1:column) 
  Real :: dat(1:row,1:column)
  Character *(*) :: Readfile
  Integer :: i,j
  

  Open(11,file=Readfile,status='old')
  Read(11,*) name(:)
  Do i=2,row
      Read(11,*) dat(i,:)
  End Do
  Close(11)
  Close(12)
  
End Subroutine Read_data