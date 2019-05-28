!---- p-sv wave modeling with finite-difference method using staggered grid
!---- reference:Jean Virieux,1986,P-SV......method,Geophysics,v.51,no.4,p.889-901.
!---- first-order in time ,eight-order in space
!---- using PML boundary condition  2006,12,18 
Program FDM_SSG
  Implicit None
  Character(len=80) :: cmdfile
  Real :: xmin,xmax,zmin,zmax
  Real :: thickness_PML,delt_x,delt_z,delt_h
  Real :: reflect_coefficient
  Real :: geophone_z,geophone_x1,geophone_x2
  Real :: fre_wavelet,delay_source_t0
  Integer :: nt
  Real :: delt_t
  Integer :: n_source
  Character(len=80) :: Input_file_source
  Character(len=80) :: Input_file_modelpar
  Character(len=80) :: Output_file_snapx,Output_file_snapz
  Character(len=80) :: Output_file_recordx,Output_file_recordz
  Real :: ige,ig1,ig2
  Integer :: nx,nz,ml
  Real :: x_coordinate_source,z_coordinate_source
  Integer :: is,js
  
  cmdfile = 'FDM_SSG.par'
  
  Call Read_cmd(cmdfile,xmin,xmax,zmin,zmax,thickness_PML,delt_x,delt_z,delt_h,reflect_coefficient,&
                geophone_z,geophone_x1,geophone_x2,fre_wavelet,delay_source_t0,nt,delt_t,n_source,&
                Input_file_source,Input_file_modelpar,Output_file_snapx,Output_file_snapz,&
                Output_file_recordx,Output_file_recordz)
  
  ige=(geophone_z-zmin)/delt_h+1  !网格节点横坐标
  ig1=(geophone_x1-xmin)/delt_h+1 !网格节点纵坐标
  ig2=(geophone_x2-xmin)/delt_h   !纵向第几个网格
  nx=(xmax-xmin)/delt_x
  nz=(zmax-zmin)/delt_z
  ml=thickness_PML/delt_h
  
  Call Read_coordinate_source(Input_file_source,x_coordinate_source,z_coordinate_source)
  
  is=(x_coordinate_source-xmin)/delt_h+1
  js=(z_coordinate_source-zmin)/delt_h+1
  
  Call create_model()
  Call Wave_Field_Modeling(delt_x,delt_z,delt_h,reflect_coefficient,fre_wavelet,delay_source_t0,nt,delt_t,&
                           n_source,ige,ig1,ig2,nx,nz,ml,is,js,Input_file_modelpar,Output_file_snapx,&
                           Output_file_snapz,Output_file_recordx,Output_file_recordz)
  
End Program FDM_SSG
