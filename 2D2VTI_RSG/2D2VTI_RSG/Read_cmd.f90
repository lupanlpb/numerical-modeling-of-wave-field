!------------------------------------------------------------
!功能：读取控制文件参数    
!    cmdfile            : 控制文件FDM_SSG.cmd文件名变量
!    xmin               : x坐标最小值
!    xmax               : x坐标最大值
!    zmin               : z坐标最小值
!    zmax               : z坐标最大值
!    thickness_PML      : 最佳匹配层厚度
!    delt_x             : x方向步长
!    delt_z             : z方向步长
!    delt_h             : 最佳匹配层步长
!    geophone_x         : 检波器x坐标
!    geophone_z1        : 检波器z1坐标
!    geophone_z2        : 检波器z2坐标
!    fre_wavelet        : 震源子波主频
!    delay_source_t0    : 震源加载延迟时间
!    nt                 : 采样次数
!    delt_t             : 采样时间间隔
!    n_source           : 震源个数
!    Input_file_source  : 震源文件source.dat文件名变量
!-----------------------------------------------------
Subroutine Read_cmd(cmdfile,xmin,xmax,zmin,zmax,thickness_PML,delt_x,delt_z,delt_h,reflect_coefficient,&
                geophone_z,geophone_x1,geophone_x2,fre_wavelet,delay_source_t0,nt,delt_t,n_source,&
                Input_file_source,Input_file_modelpar,Output_file_snapx,Output_file_snapz,&
                Output_file_recordx,Output_file_recordz)
  Implicit None
  Character *(*) :: cmdfile
  Real :: xmin,xmax,zmin,zmax
  Real :: thickness_PML,delt_x,delt_z,delt_h
  Real :: reflect_coefficient
  Real :: geophone_z,geophone_x1,geophone_x2
  Real :: fre_wavelet,delay_source_t0
  Integer :: nt
  Real :: delt_t
  Integer :: n_source
  Character *(*) :: Input_file_source
  Character *(*) :: Input_file_modelpar
  Character *(*) :: Output_file_snapx,Output_file_snapz
  Character *(*) :: Output_file_recordx,Output_file_recordz
  Open(11,file=cmdfile,status='old') 
  Read(11,*) xmin,xmax,zmin,zmax
  Read(11,*) thickness_PML,delt_x,delt_z,delt_h
  Read(11,*) reflect_coefficient
  Read(11,*) geophone_z,geophone_x1,geophone_x2
  Read(11,*) fre_wavelet,delay_source_t0
  Read(11,*) nt
  Read(11,*) delt_t
  Read(11,*) n_source
  Read(11,*) Input_file_source
  Read(11,*) Input_file_modelpar
  Read(11,*) Output_file_snapx
  Read(11,*) Output_file_snapz
  Read(11,*) Output_file_recordx
  Read(11,*) Output_file_recordz
  Close(11)
End Subroutine Read_cmd