!-----------------------------------------------
!功能：
!    读取震源坐标
!输入参数：
!    Input_file_source   ：震源坐标文件source.dat文件名变量
!    x_coordinate_source ：震源x坐标
!    z_coordinate_source ：震源z坐标
!-----------------------------------------------
Subroutine Read_coordinate_source(Input_file_source,x_coordinate_source,z_coordinate_source)
  Implicit None
  Character *(*) Input_file_source
  Real :: x_coordinate_source,z_coordinate_source
  Open(12,file=Input_file_source,status='old')
  Read(12,*) x_coordinate_source,z_coordinate_source
  Close(12)
End Subroutine Read_coordinate_source