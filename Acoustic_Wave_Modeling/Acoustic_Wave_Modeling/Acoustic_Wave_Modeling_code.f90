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
  ige=(geophone_z-zmin)/delt_h+1  !网格节点横坐标？？？
  ig1=(geophone_x1-xmin)/delt_h+1 !网格节点纵坐标？？？
  ig2=(geophone_x2-xmin)/delt_h   !纵向第几个网格？？？
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
    
    
Subroutine Wave_Field_Modeling(delt_x,delt_z,delt_h,reflect_coefficient,fre_wavelet,delay_source_t0,nt,delt_t,&
                           n_source,ige,ig1,ig2,nx,nz,ml,is,js,Input_file_modelpar,Output_file_snapx,&
                           Output_file_snapz,Output_file_recordx,Output_file_recordz)
  Implicit None
  Real :: delt_x,delt_z,delt_h
  Real :: reflect_coefficient
  Real :: fre_wavelet,delay_source_t0
  Integer :: nt
  Real :: delt_t
  Integer :: n_source
  Real :: ige,ig1,ig2
  Integer :: nx,nz,ml
  Integer :: is,js
  Character *(*) :: Input_file_modelpar
  Character *(*) :: Output_file_snapx,Output_file_snapz
  Character *(*) :: Output_file_recordx,Output_file_recordz
  Real :: Cv(-ml:nx+ml,-ml:nz+ml)                                  !Cv为波速
  Real :: vx(-ml:nx+ml,-ml:nz+ml,2),vz(-ml:nx+ml,-ml:nz+ml,2)     !vx,vz分别是x方向速度分量和z方向速度分量
  Real :: P(-ml:nx+ml,-ml:nz+ml,2)                                !P是压强
  Real :: vxxi1(ml,-ml:nz+ml,2),vxxi2(ml,-ml:nz+ml,2),&	          !左、右 边界 
          vxxj1(-ml:nx+ml,ml,2),vxxj2(-ml:nx+ml,ml,2)	            !上、下 边界 
	Real :: vxzi1(ml,-ml:nz+ml,2),vxzi2(ml,-ml:nz+ml,2),&	          !左、右 边界 
          vxzj1(-ml:nx+ml,ml,2),vxzj2(-ml:nx+ml,ml,2)	            !上、下 边界 
	Real :: vzxi1(ml,-ml:nz+ml,2),vzxi2(ml,-ml:nz+ml,2),&	          !左、右 边界 
          vzxj1(-ml:nx+ml,ml,2),vzxj2(-ml:nx+ml,ml,2)	            !上、下 边界 
	Real :: vzzi1(ml,-ml:nz+ml,2),vzzi2(ml,-ml:nz+ml,2),&	          !左、右 边界 
          vzzj1(-ml:nx+ml,ml,2),vzzj2(-ml:nx+ml,ml,2)	            !上、下 边界 
	Real :: Pxi1(ml,-ml:nz+ml,2),Pxi2(ml,-ml:nz+ml,2),&             !左、右 边界 
          Pxj1(-ml:nx+ml,ml,2),Pxj2(-ml:nx+ml,ml,2)	              !上、下 边界
	Real :: Pzi1(ml,-ml:nz+ml,2),Pzi2(ml,-ml:nz+ml,2),&             !左、右 边界
          Pzj1(-ml:nx+ml,ml,2),Pzj2(-ml:nx+ml,ml,2)	              !上、下 边界
  Real :: vxt(nx,nt),vzt(nx,nt)                                   !vxt,vzt分别是地震记录x分量和z分量
  Real :: xx,zz
  Real :: vum,vdm,vlm,vrm
  Real :: dx,dz
  Real :: qx(1:ml),qz(1:ml)
  Real :: qxd(1:ml),qzd(1:ml)
  Real :: r1,r2
  Integer(Kind=2) :: head(120)
  Integer(Kind=2) :: irecl1,irecl2                                !写入的一笔记录的长度
  Real :: ef1,ef2,ef3,ef4                                         !差分方程系数
  Integer :: nt2,it,iit
  Integer :: n_file,n_file2                                       !需要产生的文件个数(需小于999)
  Character(len=9) :: filename1_x,filename1_z
  Character(len=11) :: filename2_x,filename2_z
  Character(len=4) :: file_extension
  Character *(80) :: Output_filename_x,Output_filename_z
  Character(len=16),Allocatable :: snapshot_x(:),snapshot_z(:)    !所产生的多个文件的文件名
  Character(len=18),Allocatable :: seismogram_x(:),seismogram_z(:)
  Real :: time
  Real :: pi
  Real :: f0
  Real :: dxz
  Real :: fx,fz
  Real :: s1,s2
  Integer :: Radius                                               !加载震源半径
  Integer :: nx1,nz1
  Integer :: count
  Integer :: irec
  Integer :: ii
  Real :: a11,a12,a13,a14,a21,a22,a23,a24
  Real :: b11,b12,b13,b14,b21,b22,b23,b24
  Real :: d11,d12,d13,d14,d21,d22,d23,d24
  Character(len=12) :: Outpath
  Integer :: i,j,k
  
  Outpath='Output_file\'
  r1=delt_t/(delt_x*2)
  r2=delt_t/(delt_z*2)
  
  Open(13,file=Input_file_modelpar,status='old')
  Read(13,*)
  Do j=1,nz
    Do i=1,nx
      Read(13,*) xx,zz,Cv(i,j)
    End Do
  End Do
  Close(13)
  
  vum=0.0
  Do i=-ml,nx+ml
    Do j=-ml,0
      Cv(i,j)=Cv(i,1)
      vum=max(vum,Cv(i,j))
    End Do
  End Do
  
  vdm=0.0
  Do i=-ml,nx+ml
    Do j=nz+1,nz+ml
      Cv(i,j)=Cv(i,nz)
      vdm=max(vdm,Cv(i,j))
    End Do
  End Do
  
  vlm=0.0
  Do j=-ml,nz+ml
    Do i=-ml,0
      Cv(i,j)=Cv(1,j)
      vlm=max(vlm,Cv(i,j))
    End Do
  End Do
  
  vrm=0.0
  Do j=-ml,nz+ml
    Do i=nx+1,ml+nx
      Cv(i,j)=Cv(nx,j)
      vrm=max(vrm,Cv(i,j))
    End Do
  End Do
  
  dx=3.0*log(1.0/reflect_coefficient)*vlm/(2.0*ml*delt_x)
  dz=3.0*log(1.0/reflect_coefficient)*vlm/(2.0*ml*delt_z)
  Do i=1,ml
    qx(i)=delt_t*dx*((ml-i)*delt_x/(ml*delt_x))**2
    qz(i)=delt_t*dz*((ml-i)*delt_z/(ml*delt_z))**2
    qxd(i)=1.0/(1.0+qx(i))
    qzd(i)=1.0/(1.0+qz(i))
  End Do
  
  
  vx=0.0;vz=0.0	  
  P=0.0
  vxxi1=0.0;vxxi2=0.0;vxxj1=0.0;vxxj2=0.0
	vxzi1=0.0;vxzi2=0.0;vxzj1=0.0;vxzj2=0.0
	vzxi1=0.0;vzxi2=0.0;vzxj1=0.0;vzxj2=0.0
	vzzi1=0.0;vzzi2=0.0;vzzj1=0.0;vzzj2=0.0
	Pxi1=0.0;Pxi2=0.0;Pxj1=0.0;Pxj2=0.0
	Pzi1=0.0;Pzi2=0.0;Pzj1=0.0;Pzj2=0.0

!----------------生成波场快照文件名开始-------------------------
  n_file=int(nt/400)   !用于输出波场快照个数的计数
  Allocate(snapshot_x(1:n_file),snapshot_z(1:n_file))
  filename1_x='snapshotx'
  filename1_z='snapshotz'
  file_extension='.sgy'
  Output_filename_x='snapshots_x.txt'
  Output_filename_z='snapshots_z.txt'
  
  Call Create_file(n_file,filename1_x,file_extension,snapshot_x,Output_filename_x,Outpath)
  Call Create_file(n_file,filename1_z,file_extension,snapshot_z,Output_filename_z,Outpath)
  
  n_file2=int(ig2/50.0)
  Allocate(seismogram_x(1:n_file2),seismogram_z(1:n_file2))
  filename2_x='seismogramx'
  filename2_z='seismogramz'
  file_extension='.dat'
  Output_filename_x='seismogram_x.txt'
  Output_filename_z='seismogram_z.txt'
  
  Call Create_file(n_file2,filename2_x,file_extension,seismogram_x(:),Output_filename_x,Outpath)
  Call Create_file(n_file2,filename2_z,file_extension,seismogram_z(:),Output_filename_z,Outpath)
!----------------生成波场快照文件名结束-------------------------

  head(58)=nz+2*ml+1
  head(59)=2000
  irecl1=(nz+2*ml+1)*4+240
  
  Do i=1,n_file
    Open(i+100,file=Outpath//snapshot_x(i),form='binary',access='direct',status='replace',recl=irecl1)
    Open(i+200,file=Outpath//snapshot_z(i),form='binary',access='direct',status='replace',recl=irecl1)
  End Do
 
  Do i=1,n_file2
    Open(i+300,file=Outpath//seismogram_x(i),status='replace')
    Open(i+400,file=Outpath//seismogram_z(i),status='replace')
  End Do
  
  nt2=nt*2
  ef1=1225.0/1024.0      
  ef2=245.0/3072.0       
  ef3=441.0/46080.0      
  ef4=5.0/7168.0
  pi=4.0*atan(1.0)
  Radius=4
  
  Do it=1,nt2
    If(mod(it,2).ne.0) Then   !若是奇数则进行下列操作
      iit=(it+1)/2.0
      time=(iit-1)*delt_t
      f0=pi*fre_wavelet*(time-delay_source_t0)
      
      Do j=1,nz
        Do i=1,nx
          fx=0.0
          
            If((i-is)**2+(j-js)**2<=Radius**2) Then  
              dxz=((i-is)**2+(j-js)**2)*0.1               
              fx=(1-8*f0**2)*exp(-4*f0**2)*exp(-dxz)
            End If
          
	        a11=P(i,j,2)-P(i-1,j-1,2)
	        a12=P(i+1,j+1,2)-P(i-2,j-2,2)
	        a13=P(i+2,j+2,2)-P(i-3,j-3,2)
	        a14=P(i+3,j+3,2)-P(i-4,j-4,2)
          a21=P(i,j-1,2)-P(i-1,j,2)
          a22=P(i+1,j-2,2)-P(i-2,j+1,2)
          a23=P(i+2,j-3,2)-P(i-3,j+2,2)
          a24=P(i+3,j-4,2)-P(i-4,j+3,2)
	        vx(i,j,2)=vx(i,j,1)+r1*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+fx
	        vz(i,j,2)=vz(i,j,1)+r2*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)-(ef1*a21-ef2*a22+ef3*a23-ef4*a24))
        End Do
      End Do
      
      Do j=1,nz          !i=1--->-(ml-1), i=ml--->0	 左边界
        Do i=4,ml
	        a11=P(-ml+i,j,2)-P(-ml+i-1,j-1,2)
	        a12=P(-ml+i+1,j+1,2)-P(-ml+i-2,j-2,2)
	        a13=P(-ml+i+2,j+2,2)-P(-ml+i-3,j-3,2)
	        a14=P(-ml+i+3,j+3,2)-P(-ml+i-4,j-4,2)
          a21=P(-ml+i,j-1,2)-P(-ml+i-1,j,2)
          a22=P(-ml+i+1,j-2,2)-P(-ml+i-2,j+1,2)
          a23=P(-ml+i+2,j-3,2)-P(-ml+i-3,j+2,2)
          a24=P(-ml+i+3,j-4,2)-P(-ml+i-4,j+3,2)
	        s1=r1*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))
	        s2=(1.0-0.5*qx(i))*vxxi1(i,j,1)			
	        vxxi1(i,j,2)=(s1+s2)*qxd(i)
	        s1=0.0
	        s2=0.0
	        vxzi1(i,j,2)=vxzi1(i,j,1)+(s1+s2)
	        vx(-ml+i,j,2)=vxxi1(i,j,2)+vxzi1(i,j,2)
          
          s1=0.0
          s2=(1.0-0.5*qx(i))*vzxi1(i,j,1)
          vzxi1(i,j,2)=(s1+s2)*qxd(i)
	        s1=r2*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)-(ef1*a21-ef2*a22+ef3*a23-ef4*a24))
	        s2=0.0
	        vzzi1(i,j,2)=vzzi1(i,j,1)+(s1+s2)
	        vz(-ml+i,j,2)=vzxi1(i,j,2)+vzzi1(i,j,2)            
        End Do
      End Do
      
      Do j=1,nz
        Do i=1,ml
          vxxi1(i,j,1)=vxxi1(i,j,2)
          vxzi1(i,j,1)=vxzi1(i,j,2)
          vzxi1(i,j,1)=vzxi1(i,j,2)
          vzzi1(i,j,1)=vzzi1(i,j,2)
        End Do
      End Do
      
      nx1=nx+ml+1
      Do j=1,nz      !i=1--->-(ml-1), i=ml--->nx+1	  右边界
        Do i=ml,5,-1
          a11=P(nx1-i,j,2)-P(nx1-i-1,j-1,2)
	        a12=P(nx1-i+1,j+1,2)-P(nx1-i-2,j-2,2)
	        a13=P(nx1-i+2,j+2,2)-P(nx1-i-3,j-3,2)
	        a14=P(nx1-i+3,j+3,2)-P(nx1-i-4,j-4,2)
          a21=P(nx1-i,j-1,2)-P(nx1-i-1,j,2)
          a22=P(nx1-i+1,j-2,2)-P(nx1-i-2,j+1,2)
          a23=P(nx1-i+2,j-3,2)-P(nx1-i-3,j+2,2)
          a24=P(nx1-i+3,j-4,2)-P(nx1-i-4,j+3,2)
	        s1=r1*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))
	        s2=(1.0-0.5*qx(i))*vxxi2(i,j,1)
	        vxxi2(i,j,2)=(s1+s2)*qxd(i)
	        s1=0.0
	        s2=0.0
	        vxzi2(i,j,2)=vxzi2(i,j,1)+(s1+s2)
	        vx(nx1-i,j,2)=vxxi2(i,j,2)+vxzi2(i,j,2)
          
          s1=0.0
          s2=(1.0-0.5*qx(i))*vzxi2(i,j,1)
	        vzxi2(i,j,2)=(s1+s2)*qxd(i)
	        s1=r2*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)-(ef1*a21-ef2*a22+ef3*a23-ef4*a24))
	        s2=0.0
	        vzzi2(i,j,2)=vzzi2(i,j,1)+(s1+s2)
	        vz(nx1-i,j,2)=vzxi2(i,j,2)+vzzi2(i,j,2)            
        End Do
      End Do
      
      Do j=1,nz
        Do i=1,ml
	        vxxi2(i,j,1)=vxxi2(i,j,2)
	        vxzi2(i,j,1)=vxzi2(i,j,2)
	        vzxi2(i,j,1)=vzxi2(i,j,2)	    
	        vzzi2(i,j,1)=vzzi2(i,j,2)
        End Do
      End Do
      
      Do j=4,ml                   ! j=ml--->0	 上边界
        Do i=-ml+4,nx+ml-4
	        k=ml
	        if(i.le.0) k=ml+i       !处理区域1、3、7、9  
	        if(i.gt.nx) k=nx+ml-i+1
	        a11=P(i,-ml+j,2)-P(i-1,-ml+j-1,2)
	        a12=P(i+1,-ml+j+1,2)-P(i-2,-ml+j-2,2)
	        a13=P(i+2,-ml+j+2,2)-P(i-3,-ml+j-3,2)
	        a14=P(i+3,-ml+j+3,2)-P(i-4,-ml+j-4,2)
          a21=P(i,-ml+j-1,2)-P(i-1,-ml+j,2)
          a22=P(i+1,-ml+j-2,2)-P(i-2,-ml+j+1,2)
          a23=P(i+2,-ml+j-3,2)-P(i-3,-ml+j+2,2)
          a24=P(i+3,-ml+j-4,2)-P(i-4,-ml+j+3,2)
	        s1=r1*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))
	        s2=(1.0-0.5*qx(k))*vxxj1(i,j,1)			
	        vxxj1(i,j,2)=(s1+s2)*qxd(k)
	        s1=0.0
          s2=(1.0-0.5*qx(j))*vxzj1(i,j,1)
	        vxzj1(i,j,2)=(s1+s2)*qxd(j)
	        vx(i,-ml+j,2)=vxxj1(i,j,2)+vxzj1(i,j,2)
            
	        s1=0.0
	        s2=(1.0-0.5*qx(k))*vzxj1(i,j,1)
	        vzxj1(i,j,2)=(s1+s2)*qxd(k)
	        s1=r2*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)-(ef1*a21-ef2*a22+ef3*a23-ef4*a24))
          s2=(1.0-0.5*qx(j))*vzzj1(i,j,1)
	        vzzj1(i,j,2)=(s1+s2)*qxd(j)
	        vz(i,-ml+j,2)=vzxj1(i,j,2)+vzzj1(i,j,2)
        End Do
      End Do
      
      Do j=1,ml
        Do i=-ml,nx+ml
	      vxxj1(i,j,1)=vxxj1(i,j,2)   	    
	      vxzj1(i,j,1)=vxzj1(i,j,2)
	      vzxj1(i,j,1)=vzxj1(i,j,2)   	    
	      vzzj1(i,j,1)=vzzj1(i,j,2)
        End Do
      End Do
      
      nz1=nz+ml+1
      Do j=ml,5,-1                !i=1--->-(ml-1), i=ml--->nx+1	  下边界
        Do i=-ml+4,nx+ml-4
	        k=ml
	        if(i.le.0) k=ml+i         !处理区域1、3、7、9  
	        if(i.gt.nx) k=nx+ml-i+1
	        a11=P(i,nz1-j,2)-P(i-1,nz1-j-1,2)
	        a12=P(i+1,nz1-j+1,2)-P(i-2,nz1-j-2,2)
	        a13=P(i+2,nz1-j+2,2)-P(i-3,nz1-j-3,2)
	        a14=P(i+3,nz1-j+3,2)-P(i-4,nz1-j-4,2)
          a21=P(i,nz1-j-1,2)-P(i-1,nz1-j,2)
          a22=P(i+1,nz1-j-2,2)-P(i-2,nz1-j+1,2)
          a23=P(i+2,nz1-j-3,2)-P(i-3,nz1-j+2,2)
          a24=P(i+3,nz1-j-4,2)-P(i-4,nz1-j+3,2)
	        s1=r1*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))
	        s2=(1.0-0.5*qx(k))*vxxj2(i,j,1)
	        vxxj2(i,j,2)=(s1+s2)*qxd(k)
	        s1=0.0
	        s2=(1.0-0.5*qx(j))*vxzj2(i,j,1)
	        vxzj2(i,j,2)=(s1+s2)*qxd(j)
	        vx(i,nz1-j,2)=vxxj2(i,j,2)+vxzj2(i,j,2)
            
	        s1=0.0
	        s2=(1.0-0.5*qx(k))*vzxj2(i,j,1)
	        vzxj2(i,j,2)=(s1+s2)*qxd(k)
	        s1=r2*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)-(ef1*a21-ef2*a22+ef3*a23-ef4*a24))
	        s2=(1.0-0.5*qx(j))*vzzj2(i,j,1)
	        vzzj2(i,j,2)=(s1+s2)*qxd(j)
	        vz(i,nz1-j,2)=vzxj2(i,j,2)+vzzj2(i,j,2)
        End Do
      End Do
      
      Do j=1,ml
        Do i=-ml,nx+ml
	        vxxj2(i,j,1)=vxxj2(i,j,2)    	    
	        vxzj2(i,j,1)=vxzj2(i,j,2)
	        vzxj2(i,j,1)=vzxj2(i,j,2)  
	        vzzj2(i,j,1)=vzzj2(i,j,2)
        End Do
      End Do
      
      Do j=-ml,nz+ml     
        Do i=-ml,nx+ml   
          vx(i,j,1)=vx(i,j,2)   !将前一时刻计算的波场值vx(i,j,2)赋值给vx(i,j,1)
          vz(i,j,1)=vz(i,j,2)  
        End Do
      End Do
    
      Do i=1,nx
        vxt(i,iit)=vx(i,ige,2)    !将横坐标为0的前一时刻的波场值赋值给横坐标为0的检波器
        vzt(i,iit)=vz(i,ige,2)
      End Do
      
      Do i=1,n_file2              !输出合成地震图
        j=26+(i-1)*50
        Write(i+300,*) time,vxt(j,iit)
        Write(i+400,*) time,vzt(j,iit)
      End Do
        
      If(mod(iit,400).eq.0) Then
        k=0
        Do i=-ml,nx+ml     
          k=k+1
          count=int(iit/400)
          Write(count+100,rec=k) head,(vx(i,j,2),j=-ml,nz+ml) !输出波场快照 
          Write(count+200,rec=k) head,(vz(i,j,2),j=-ml,nz+ml) 
        End Do
      End If
      
      If(mod(iit,10).eq.0) Write(*,*) iit,P(is,js,2)
      
    Else
      iit=it/2.0
      Do j=1,nz
        Do i=1,nx
          b11=vx(i+1,j+1,2)-vx(i,j,2)
          b12=vx(i+2,j+2,2)-vx(i-1,j-1,2)
          b13=vx(i+3,j+3,2)-vx(i-2,j-2,2)
          b14=vx(i+4,j+4,2)-vx(i-3,j-3,2)
          b21=vx(i+1,j,2)-vx(i,j+1,2)
          b22=vx(i+2,j-1,2)-vx(i-1,j+2,2)
          b23=vx(i+3,j-2,2)-vx(i-2,j+3,2)
          b24=vx(i+4,j-3,2)-vx(i-3,j+4,2)
          d11=vz(i+1,j+1,2)-vz(i,j,2)
          d12=vz(i+2,j+2,2)-vz(i-1,j-1,2)
          d13=vz(i+3,j+3,2)-vz(i-2,j-2,2)
          d14=vz(i+4,j+4,2)-vz(i-3,j-3,2)
          d21=vz(i+1,j,2)-vz(i,j+1,2)
          d22=vz(i+2,j-1,2)-vz(i-1,j+2,2)
          d23=vz(i+3,j-2,2)-vz(i-2,j+3,2)
          d24=vz(i+4,j-3,2)-vz(i-3,j+4,2)
          P(i,j,2)=P(i,j,1)+Cv(i,j)**2*r1*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
                            Cv(i,j)**2*r2*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)-(ef1*d21-ef2*d22+ef3*d23-ef4*d24))
        End Do
      End Do
      
      Do j=1,nz                   !i=1--->-(ml-1), i=ml--->0	  左边界
        Do i=4,ml
	        b11=vx(-ml+i+1,j+1,2)-vx(-ml+i,j,2)
          b12=vx(-ml+i+2,j+2,2)-vx(-ml+i-1,j-1,2)
          b13=vx(-ml+i+3,j+3,2)-vx(-ml+i-2,j-2,2)
          b14=vx(-ml+i+4,j+4,2)-vx(-ml+i-3,j-3,2)
          b21=vx(-ml+i+1,j,2)-vx(-ml+i,j+1,2)
          b22=vx(-ml+i+2,j-1,2)-vx(-ml+i-1,j+2,2)
          b23=vx(-ml+i+3,j-2,2)-vx(-ml+i-2,j+3,2)
          b24=vx(-ml+i+4,j-3,2)-vx(-ml+i-3,j+4,2)
          d11=vz(-ml+i+1,j+1,2)-vz(-ml+i,j,2)
          d12=vz(-ml+i+2,j+2,2)-vz(-ml+i-1,j-1,2)
          d13=vz(-ml+i+3,j+3,2)-vz(-ml+i-2,j-2,2)
          d14=vz(-ml+i+4,j+4,2)-vz(-ml+i-3,j-3,2)
          d21=vz(-ml+i+1,j,2)-vz(-ml+i,j+1,2)
          d22=vz(-ml+i+2,j-1,2)-vz(-ml+i-1,j+2,2)
          d23=vz(-ml+i+3,j-2,2)-vz(-ml+i-2,j+3,2)
          d24=vz(-ml+i+4,j-3,2)-vz(-ml+i-3,j+4,2)
	        s1=r1*Cv(-ml+i,j)**2*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
	        s2=(1.0-0.5*qx(i))*Pxi1(i,j,1)
	        Pxi1(i,j,2)=(s1+s2)*qxd(i)
	        s1=r2*Cv(-ml+i,j)**2*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)-(ef1*d21-ef2*d22+ef3*d23-ef4*d24))
	        s2=0.0 
	        Pzi1(i,j,2)=Pzi1(i,j,1)+s1+s2
	        P(-ml+i,j,2)=Pxi1(i,j,2)+Pzi1(i,j,2)
        End Do
      End Do
      
      Do j=1,nz
        Do i=1,ml
	      Pxi1(i,j,1)=Pxi1(i,j,2)
	      Pzi1(i,j,1)=Pzi1(i,j,2)
        End Do
      End Do
      
      nx1=nx+ml+1
      Do j=1,nz                !i=1--->-(ml-1), i=ml--->nx+1 右边界
        Do i=ml,5,-1
          b11=vx(nx1-i+1,j+1,2)-vx(nx1-i,j,2)
          b12=vx(nx1-i+2,j+2,2)-vx(nx1-i-1,j-1,2)
          b13=vx(nx1-i+3,j+3,2)-vx(nx1-i-2,j-2,2)
          b14=vx(nx1-i+4,j+4,2)-vx(nx1-i-3,j-3,2)
          b21=vx(nx1-i+1,j,2)-vx(nx1-i,j+1,2)
          b22=vx(nx1-i+2,j-1,2)-vx(nx1-i-1,j+2,2)
          b23=vx(nx1-i+3,j-2,2)-vx(nx1-i-2,j+3,2)
          b24=vx(nx1-i+4,j-3,2)-vx(nx1-i-3,j+4,2)
          d11=vz(nx1-i+1,j+1,2)-vz(nx1-i,j,2)
          d12=vz(nx1-i+2,j+2,2)-vz(nx1-i-1,j-1,2)
          d13=vz(nx1-i+3,j+3,2)-vz(nx1-i-2,j-2,2)
          d14=vz(nx1-i+4,j+4,2)-vz(nx1-i-3,j-3,2)
          d21=vz(nx1-i+1,j,2)-vz(nx1-i,j+1,2)
          d22=vz(nx1-i+2,j-1,2)-vz(nx1-i-1,j+2,2)
          d23=vz(nx1-i+3,j-2,2)-vz(nx1-i-2,j+3,2)
          d24=vz(nx1-i+4,j-3,2)-vz(nx1-i-3,j+4,2)
	        s1=r1*Cv(nx1-i,j)**2*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
	        s2=(1.0-0.5*qx(i))*Pxi2(i,j,1)
	        Pxi2(i,j,2)=(s1+s2)*qxd(i)
	        s1=r2*Cv(nx1-i,j)**2*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)-(ef1*d21-ef2*d22+ef3*d23-ef4*d24))
	        s2=0.0 
	        Pzi2(i,j,2)=Pzi2(i,j,1)+s1+s2
	        P(nx1-i,j,2)=Pxi2(i,j,2)+Pzi2(i,j,2)
        End Do
      End Do
      
      Do j=1,nz
        Do i=1,ml
	        Pxi2(i,j,1)=Pxi2(i,j,2)
	        Pzi2(i,j,1)=Pzi2(i,j,2)
        End Do
      End Do
      
      Do j=4,ml                   ! j=ml--->0	  上边界
        Do i=-ml+4,nx+ml-4
	        k=ml
	        if(i.le.0) k=ml+i
	        if(i.gt.nx) k=nx+ml-i+1
	        b11=vx(i+1,-ml+j+1,2)-vx(i,-ml+j,2)
          b12=vx(i+2,-ml+j+2,2)-vx(i-1,-ml+j-1,2)
          b13=vx(i+3,-ml+j+3,2)-vx(i-2,-ml+j-2,2)
          b14=vx(i+4,-ml+j+4,2)-vx(i-3,-ml+j-3,2)
          b21=vx(i+1,-ml+j,2)-vx(i,-ml+j+1,2)
          b22=vx(i+2,-ml+j-1,2)-vx(i-1,-ml+j+2,2)
          b23=vx(i+3,-ml+j-2,2)-vx(i-2,-ml+j+3,2)
          b24=vx(i+4,-ml+j-3,2)-vx(i-3,-ml+j+4,2)
          d11=vz(i+1,-ml+j+1,2)-vz(i,-ml+j,2)
          d12=vz(i+2,-ml+j+2,2)-vz(i-1,-ml+j-1,2)
          d13=vz(i+3,-ml+j+3,2)-vz(i-2,-ml+j-2,2)
          d14=vz(i+4,-ml+j+4,2)-vz(i-3,-ml+j-3,2)
          d21=vz(i+1,-ml+j,2)-vz(i,-ml+j+1,2)
          d22=vz(i+2,-ml+j-1,2)-vz(i-1,-ml+j+2,2)
          d23=vz(i+3,-ml+j-2,2)-vz(i-2,-ml+j+3,2)
          d24=vz(i+4,-ml+j-3,2)-vz(i-3,-ml+j+4,2)
	        s1=r1*Cv(i,-ml+j)**2*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
	        s2=(1.0-0.5*qx(k))*Pxj1(i,j,1)
	        Pxj1(i,j,2)=(s1+s2)*qxd(k)
	        s1=r2*Cv(i,-ml+j)**2*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)-(ef1*d21-ef2*d22+ef3*d23-ef4*d24))
          s2=(1.0-0.5*qx(j))*Pzj1(i,j,1) 
	        Pzj1(i,j,2)=(s1+s2)*qxd(j)
	        P(i,-ml+j,2)=Pxj1(i,j,2)+Pzj1(i,j,2)
        End Do
      End Do
      
      Do j=1,ml
        Do i=-ml,nx+ml
	        Pxj1(i,j,1)=Pxj1(i,j,2)
	        Pzj1(i,j,1)=Pzj1(i,j,2)
        End Do
      End Do
      
      nz1=nz+ml+1
      Do j=ml,5,-1                ! j=ml--->nz+1 下边界
        Do i=-ml+4,nx+ml-4
	        k=ml
	        if(i.le.0) k=ml+i
	        if(i.gt.nx) k=nx+ml-i+1
	        b11=vx(i+1,nz1-j+1,2)-vx(i,nz1-j,2)
          b12=vx(i+2,nz1-j+2,2)-vx(i-1,nz1-j-1,2)
          b13=vx(i+3,nz1-j+3,2)-vx(i-2,nz1-j-2,2)
          b14=vx(i+4,nz1-j+4,2)-vx(i-3,nz1-j-3,2)
          b21=vx(i+1,nz1-j,2)-vx(i,nz1-j+1,2)
          b22=vx(i+2,nz1-j-1,2)-vx(i-1,nz1-j+2,2)
          b23=vx(i+3,nz1-j-2,2)-vx(i-2,nz1-j+3,2)
          b24=vx(i+4,nz1-j-3,2)-vx(i-3,nz1-j+4,2)
          d11=vz(i+1,nz1-j+1,2)-vz(i,nz1-j,2)
          d12=vz(i+2,nz1-j+2,2)-vz(i-1,nz1-j-1,2)
          d13=vz(i+3,nz1-j+3,2)-vz(i-2,nz1-j-2,2)
          d14=vz(i+4,nz1-j+4,2)-vz(i-3,nz1-j-3,2)
          d21=vz(i+1,nz1-j,2)-vz(i,nz1-j+1,2)
          d22=vz(i+2,nz1-j-1,2)-vz(i-1,nz1-j+2,2)
          d23=vz(i+3,nz1-j-2,2)-vz(i-2,nz1-j+3,2)
          d24=vz(i+4,nz1-j-3,2)-vz(i-3,nz1-j+4,2)
	        s1=r1*Cv(i,nz1-j)**2*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
	        s2=(1.0-0.5*qx(k))*Pxj2(i,j,1)
	        Pxj2(i,j,2)=(s1+s2)*qxd(k)
	        s1=r2*Cv(i,nz1-j)**2*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)-(ef1*d21-ef2*d22+ef3*d23-ef4*d24))
	        s2=(1.0-0.5*qx(j))*Pzj2(i,j,2) 
	        Pzj2(i,j,2)=(s1+s2)*qxd(j)
	        P(i,nz1-j,2)=Pxj2(i,j,2)+Pzj2(i,j,2)
        End Do
      End Do
      
      Do j=1,ml
        Do i=-ml,nx+ml
	        Pxj2(i,j,1)=Pxj2(i,j,2)
	        Pzj2(i,j,1)=Pzj2(i,j,2)
        End Do
      End Do

      Do j=-ml,nz+ml   
        Do i=-ml,nx+ml 
          P(i,j,1)=P(i,j,2)
        End Do
      End Do 
      
    End If
  End Do
  
  Do i=1,n_file
    Close(i+100)
    Close(i+200)
  End Do
  
  Do i=1,n_file2
    Close(i+300)
    Close(i+400)
  End Do
  
  irecl2=nt*4+240
  head(58)=nt
  head(59)=delt_t*1000000
  
  Open(16,file=Outpath//'synthetic_seismic_record_x.sgy',form='binary',access='direct',status='replace',recl=irecl2)
  Open(17,file=Outpath//'synthetic_seismic_record_z.sgy',form='binary',access='direct',status='replace',recl=irecl2)     
  Do i=ig1,ig2
    ii=i-ig1+1
    Write(16,rec=ii) head,(vxt(i,j),j=1,nt)
    Write(17,rec=ii) head,(vzt(i,j),j=1,nt)
  End Do
  Close(16)
  Close(17)
  
End Subroutine Wave_Field_Modeling