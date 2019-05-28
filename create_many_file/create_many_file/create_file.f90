Program create_file
  Implicit none
  Integer :: n_file   !需要产生的文件个数(需小于999)
  Character(len=4) :: file_name
  Character(len=3) :: serial_number
  Character(len=4) :: file_extension
  Character(len=11),Allocatable :: filename(:)   !所产生的多个文件名
  Integer :: k1,k2,k3
  Integer :: i
  
  Write(*,*) '请输入需要产生的文件的个数(须小于999):'
  Read(*,*) n_file
  Allocate(filename(1:n_file))
    
  k1=1
  k2=1
  k3=2
  file_name='附件'
  file_extension='.doc'
  
  Open(11,file='filename.txt',status='replace')
  Do i=1,n_file
    serial_number(1:1)=Char(k1+47)
    serial_number(2:2)=Char(k2+47)
    serial_number(3:3)=Char(k3+47)
    filename(i)=file_name//serial_number//file_extension
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
    Write(11,*) filename(i)
  End Do
  Close(11)
  
  Deallocate(filename)
  
End Program create_file