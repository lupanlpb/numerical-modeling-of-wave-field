Subroutine Create_file(n_file,filename,file_extension,file_name,Output_filename)
  Implicit none
  Integer :: n_file   !需要产生的文件个数(需小于999)
  Character *(*) :: filename
  Character *(*) :: file_extension
  Character(len=3) :: name_serial
  Character *(*) :: file_name(1:n_file)   !所产生的多个文件名
  Character *(*) :: Output_filename
  Integer :: k1,k2,k3
  Integer :: i
  Character(len=14) :: Outpath
  
  Outpath='Output_Result\'
  
  k1=1
  k2=1
  k3=2
  
  Open(11,file=Outpath//Output_filename,status='replace')
  Do i=1,n_file
    name_serial(1:1)=Char(k1+47)
    name_serial(2:2)=Char(k2+47)
    name_serial(3:3)=Char(k3+47)
    file_name(i)=filename//name_serial//file_extension
    k3=k3+1
    If(k3>10) Then
      k3=1
      k2=k2+1
      If(k2>10) Then
        k2=1
        k1=k1+1
        If(k1>10) k1=1
      End If
    End If
    Write(11,*) file_name(i)
  End Do
  Close(11)
  
End Subroutine create_file