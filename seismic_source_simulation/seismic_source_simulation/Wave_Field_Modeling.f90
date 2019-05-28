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
  Call Create_model()
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
    

Subroutine Create_model()
  Implicit None
  Character(len=80) :: model_parameter
  Character(len=80) :: modelpar
  Real :: xmin,xmax,zmin,zmax
  Real :: vp1,vp2,vs1,vs2
  Real :: density1,density2
  Real :: delt_x,delt_z
  Integer :: nx,nz
  Integer :: i,j
  
  model_parameter='model_parameter.par'
  Open(11,file=model_parameter,status='old')
  Read(11,*) xmin,xmax,zmin,zmax
  Read(11,*) vp1,vp2,vs1,vs2
  Read(11,*) density1,density2
  Read(11,*) delt_x,delt_z
  Read(11,*) modelpar
  Close(11)
  
  nx=int((xmax-xmin)/delt_x)
  nz=int((zmax-zmin)/delt_z)
  
  Open(12,file=modelpar,status='replace')
  Write(12,*) '-------x--------------z------------vp-------------vs-----------density------'
  Do j=1,nz
    Do i=1,nx
      If(j<=151) Then
        Write(12,'(5E12.3/)') (i-1)*delt_x,(j-1)*delt_z,vp1,vs1,density1
      Else If(151<j.and.j<=176) Then  !断距为50m
        If(i<=126) Then
          Write(12,'(5E12.3/)') (i-1)*delt_x,(j-1)*delt_z,vp2,vs2,density2
        Else
          Write(12,'(5E12.3/)') (i-1)*delt_x,(j-1)*delt_z,vp1,vs1,density1
        End If
      Else
        Write(12,'(5E12.3/)') (i-1)*delt_x,(j-1)*delt_z,vp2,vs2,density2
      End If
    End Do
  End Do
  Close(12)

End Subroutine Create_model
  
  
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
  Real :: density(-ml:nx+ml,-ml:nz+ml)
  Real :: vp(-ml:nx+ml,-ml:nz+ml),vs(-ml:nx+ml,-ml:nz+ml)         !vp,vs分别是纵波波速和横波波速
  Real :: vx(-ml:nx+ml,-ml:nz+ml,2),vz(-ml:nx+ml,-ml:nz+ml,2)     !vx,vz分别是x方向速度分量和z方向速度分量
  Real :: txx(-ml:nx+ml,-ml:nz+ml,2),tzz(-ml:nx+ml,-ml:nz+ml,2),&
          txz(-ml:nx+ml,-ml:nz+ml,2)                              !txx,tzz,txz分别是三个应力分量
  Real :: Lamda_2Miu(-ml:nx+ml,-ml:nz+ml),Lamda(-ml:nx+ml,-ml:nz+ml),&
          Miu(-ml:nx+ml,-ml:nz+ml)
  Real :: vxxi1(ml,-ml:nz+ml,2),vxxi2(ml,-ml:nz+ml,2),&	          !左、右 边界 
          vxxj1(-ml:nx+ml,ml,2),vxxj2(-ml:nx+ml,ml,2)	            !上、下 边界 
	Real :: vxzi1(ml,-ml:nz+ml,2),vxzi2(ml,-ml:nz+ml,2),&	          !左、右 边界 
          vxzj1(-ml:nx+ml,ml,2),vxzj2(-ml:nx+ml,ml,2)	            !上、下 边界 
	Real :: vzxi1(ml,-ml:nz+ml,2),vzxi2(ml,-ml:nz+ml,2),&	          !左、右 边界 
          vzxj1(-ml:nx+ml,ml,2),vzxj2(-ml:nx+ml,ml,2)	            !上、下 边界 
	Real :: vzzi1(ml,-ml:nz+ml,2),vzzi2(ml,-ml:nz+ml,2),&	          !左、右 边界 
          vzzj1(-ml:nx+ml,ml,2),vzzj2(-ml:nx+ml,ml,2)	            !上、下 边界 
	Real :: t1xi1(ml,-ml:nz+ml,2),t1xi2(ml,-ml:nz+ml,2),&           !左、右 边界 
          t1xj1(-ml:nx+ml,ml,2),t1xj2(-ml:nx+ml,ml,2)	            !上、下 边界      t1=txx
	Real :: t1zi1(ml,-ml:nz+ml,2),t1zi2(ml,-ml:nz+ml,2),&           !左、右 边界
          t1zj1(-ml:nx+ml,ml,2),t1zj2(-ml:nx+ml,ml,2)	            !上、下 边界
  Real :: t2xi1(ml,-ml:nz+ml,2),t2xi2(ml,-ml:nz+ml,2),&           !左、右 边界	    t2=tzz  
          t2xj1(-ml:nx+ml,ml,2),t2xj2(-ml:nx+ml,ml,2)             !上、下 边界
  Real :: t2zi1(ml,-ml:nz+ml,2),t2zi2(ml,-ml:nz+ml,2),&           !左、右 边界
          t2zj1(-ml:nx+ml,ml,2),t2zj2(-ml:nx+ml,ml,2)	            !上、下 边界        
	Real :: t3xi1(ml,-ml:nz+ml,2),t3xi2(ml,-ml:nz+ml,2),&           !左、右 边界	    t3=txz
          t3xj1(-ml:nx+ml,ml,2),t3xj2(-ml:nx+ml,ml,2)             !上、下 边界
	Real :: t3zi1(ml,-ml:nz+ml,2),t3zi2(ml,-ml:nz+ml,2),&           !左、右 边界
          t3zj1(-ml:nx+ml,ml,2),t3zj2(-ml:nx+ml,ml,2)	            !上、下 边界
  Real :: vxt(nx,nt),vzt(nx,nt)                                   !vxt,vzt分别是地震记录x分量和z分量
  Real :: xx,zz
  Real :: vum,vdm,vlm,vrm
  Real :: dx,dz
  Real :: qx(1:ml),qz(1:ml)
  Real :: qxd(1:ml),qzd(1:ml)
  Real :: r1,r2
  Integer(Kind=2) :: head(120)
  Integer(Kind=2) :: irecl1,irecl2                                !写入的一笔记录的长度
  Integer :: ii1,ii2,jj1,jj2                                      !震源区域范围
  Real :: ef1,ef2,ef3,ef4                                         !差分方程系数
  Integer :: nt2,it,iit
  Integer :: n_file                                               !需要产生的文件个数(需小于999)
  Character(len=9) :: filename1_x,filename1_z
  Character(len=4) :: file_extension
  Character *(80) :: Output_filename_x,Output_filename_z
  Character(len=16),Allocatable :: snapshot_x(:),snapshot_z(:)    !所产生的多个文件的文件名
  Real :: time
  Real :: fx,fz
  Real :: dxz
  Real :: fc0
  Real :: a1,a2,a3,a4
  Real :: b1,b2,b3,b4
  Real :: c1,c2,c3,c4
  Real :: d1,d2,d3,d4
  Real :: s1,s2
  Integer :: nx1,nz1
  Integer :: count
  Integer :: irec
  Integer :: ii
  Real :: a11,a12,a13,a14,a21,a22,a23,a24
  Real :: b11,b12,b13,b14,b21,b22,b23,b24
  Real :: c11,c12,c13,c14,c21,c22,c23,c24
  Integer :: i,j,k
  
  r1=delt_t/delt_x
  r2=delt_t/delt_z
  
  Open(13,file=Input_file_modelpar,status='old')
  Read(13,*)
  Do j=1,nz
    Do i=1,nx
      Read(13,*) xx,zz,vp(i,j),vs(i,j),density(i,j)
    End Do
  End Do
  Close(13)
  
  vum=0.0
  Do i=-ml,nx+ml
    Do j=-ml,0
      vp(i,j)=vp(i,1)
      vs(i,j)=vs(i,1)
      density(i,j)=density(i,1)
      vum=max(vum,vp(i,j))
    End Do
  End Do
  
  vdm=0.0
  Do i=-ml,nx+ml
    Do j=nz+1,nz+ml
      vp(i,j)=vp(i,nz)
      vs(i,j)=vs(i,nz)
      density(i,j)=density(i,nz)
      vdm=max(vdm,vp(i,j))
    End Do
  End Do
  
  vlm=0.0
  Do j=-ml,nz+ml
    Do i=-ml,0
      vp(i,j)=vp(1,j)
      vs(i,j)=vs(1,j)
      density(i,j)=density(1,j)
      vlm=max(vlm,vp(i,j))
    End Do
  End Do
  
  vrm=0.0
  Do j=-ml,nz+ml
    Do i=nx+1,ml+nx
      vp(i,j)=vp(nx,j)
      vs(i,j)=vs(nx,j)
      density(i,j)=density(nx,j)
      vrm=max(vrm,vp(i,j))
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
  
  Do i=-ml,nx+ml
    Do j=-ml,nz+ml
      Lamda_2Miu(i,j)=vp(i,j)*vp(i,j)*density(i,j)
      Miu(i,j)=vs(i,j)*vs(i,j)*density(i,j)
      Lamda(i,j)=Lamda_2Miu(i,j)-2*Miu(i,j)
    End Do
  End Do
  
  vx=0.0  
  vz=0.0	  
  txx=0.0	  
  tzz=0.0	  
  txz=0.0
  vxxi1=0.0
	vxxi2=0.0
	vxxj1=0.0
	vxxj2=0.0
	vxzi1=0.0
	vxzi2=0.0
	vxzj1=0.0
	vxzj2=0.0
	vzxi1=0.0
	vzxi2=0.0
	vzxj1=0.0
	vzxj2=0.0
	vzzi1=0.0
	vzzi2=0.0
	vzzj1=0.0
	vzzj2=0.0
	t1xi1=0.0
	t1xi2=0.0
	t1xj1=0.0
	t1xj2=0.0
	t1zi1=0.0
	t1zi2=0.0
	t1zj1=0.0
	t1zj2=0.0
  t2xi1=0.0
  t2xi2=0.0
  t2xj1=0.0
  t2xj2=0.0
  t2zi1=0.0
  t2zi2=0.0
  t2zj1=0.0
  t2zj2=0.0
	t3xi1=0.0
	t3xi2=0.0
	t3xj1=0.0
	t3xj2=0.0
	t3zi1=0.0
	t3zi2=0.0
	t3zj1=0.0
	t3zj2=0.0

!----------------生成波场快照文件名开始-------------------------
  n_file=int(nt/400)   !用于输出波场快照个数的计数
  Allocate(snapshot_x(1:n_file),snapshot_z(1:n_file))
  filename1_x='snapshotx'
  filename1_z='snapshotz'
  file_extension='.sgy'
  Output_filename_x='snapshots_x.txt'
  Output_filename_z='snapshots_z.txt'
  
  Call Create_file(n_file,filename1_x,file_extension,snapshot_x,Output_filename_x)
  Call Create_file(n_file,filename1_z,file_extension,snapshot_z,Output_filename_z)
  
!----------------生成波场快照文件名结束-------------------------

  head(58)=nz+2*ml+1
  head(59)=2000
  irecl1=(nz+2*ml+1)*4+240
  
  Do i=1,n_file
    Open(i+100,file=snapshot_x(i),form='binary',access='direct',status='replace',recl=irecl1)
    Open(i+200,file=snapshot_z(i),form='binary',access='direct',status='replace',recl=irecl1)
  End Do
   
  nt2=nt*2
  ii1=max(1,is-10)       
  ii2=min(nx,is+10)      
  jj1=max(1,js-10)      
  jj2=min(nz,js+10)      
  ef1=1225.0/1024.0      
  ef2=245.0/3072.0       
  ef3=441.0/46080.0      
  ef4=5.0/7168.0
  fx=0.0
  fz=0.0
  
  Do it=1,nt2
    If(mod(it,2).ne.0) Then   !若是奇数则进行下列操作
      iit=(it+1)/2.0
      time=(iit-1)*delt_t          
      Call source_force(fre_wavelet,delay_source_t0,time,fc0)	!调用震源函数 
      Do j=1,nz
        Do i=1,nx
          
	        If(iit.le.300) Then
            If(i.ge.ii1.and.i.le.ii2.and.j.ge.jj1.and.j.le.jj2) Then	  
	            dxz=((i-is)**2+(j-js)**2)*0.1              
              fx=fc0*exp(-dxz)
              fz=fc0*exp(-dxz)
            End If
          End If
          
	        a1=txx(i,j,2)-txx(i-1,j,2)
	        a2=txx(i+1,j,2)-txx(i-2,j,2)
	        a3=txx(i+2,j,2)-txx(i-3,j,2)
	        a4=txx(i+3,j,2)-txx(i-4,j,2)
          b1=txz(i,j,2)-txz(i,j-1,2)
          b2=txz(i,j+1,2)-txz(i,j-2,2)
          b3=txz(i,j+2,2)-txz(i,j-3,2)
          b4=txz(i,j+3,2)-txz(i,j-4,2)
	        c1=tzz(i,j+1,2)-tzz(i,j,2)
	        c2=tzz(i,j+2,2)-tzz(i,j-1,2)
	        c3=tzz(i,j+3,2)-tzz(i,j-2,2)
	        c4=tzz(i,j+4,2)-tzz(i,j-3,2)
          d1=txz(i+1,j,2)-txz(i,j,2)
          d2=txz(i+2,j,2)-txz(i-1,j,2)
          d3=txz(i+3,j,2)-txz(i-2,j,2)
          d4=txz(i+4,j,2)-txz(i-3,j,2)
	        vx(i,j,2)=vx(i,j,1)+1/density(i,j)*(r1*(ef1*a1-ef2*a2+ef3*a3-ef4*a4)+r2*(ef1*b1-ef2*b2+ef3*b3-ef4*b4))
	        vz(i,j,2)=vz(i,j,1)+1/density(i,j)*(r2*(ef1*c1-ef2*c2+ef3*c3-ef4*c4)+r1*(ef1*d1-ef2*d2+ef3*d3-ef4*d4))
        End Do
      End Do
      
      Do j=1,nz          !i=1--->-(ml-1), i=ml--->0	 左边界
	      Do i=4,ml
	        a1=txx(-ml+i,j,2)-txx(-ml+i-1,j,2)    !vxx部分
	        a2=txx(-ml+i+1,j,2)-txx(-ml+i-2,j,2)
	        a3=txx(-ml+i+2,j,2)-txx(-ml+i-3,j,2)
	        a4=txx(-ml+i+3,j,2)-txx(-ml+i-4,j,2)
	        s1=1/density(-ml+i,j)*r1*(ef1*a1-ef2*a2+ef3*a3-ef4*a4)
	        s2=(1.0-0.5*qx(i))*vxxi1(i,j,1)			
	        vxxi1(i,j,2)=(s1+s2)*qxd(i)           !？？？
          
	        a1=txz(-ml+i,j,2)-txz(-ml+i,j-1,2)    !vxz部分
	        a2=txz(-ml+i,j+1,2)-txz(-ml+i,j-2,2)
	        a3=txz(-ml+i,j+2,2)-txz(-ml+i,j-3,2)
	        a4=txz(-ml+i,j+3,2)-txz(-ml+i,j-4,2)
	        s1=1/density(-ml+i,j)*r1*(ef1*a1-ef2*a2+ef3*a3-ef4*a4)
	        s2=0.0
	        vxzi1(i,j,2)=vxzi1(i,j,1)+(s1+s2)     !？？？
	        vx(-ml+i,j,2)=vxxi1(i,j,2)+vxzi1(i,j,2)
          
	        b1=txz(-ml+i+1,j,2)-txz(-ml+i,j,2)
	        b2=txz(-ml+i+2,j,2)-txz(-ml+i-1,j,2)
	        b3=txz(-ml+i+3,j,2)-txz(-ml+i-3,j,2)
	        b4=txz(-ml+i+4,j,2)-txz(-ml+i-4,j,2)
	        s1=1/density(-ml+i,j)*r2*(ef1*b1-ef2*b2+ef3*b3-ef4*b4)
	        s2=(1.0-0.5*qx(i))*vzxi1(i,j,1)
	        vzxi1(i,j,2)=(s1+s2)*qxd(i)
          
	        b1=tzz(-ml+i,j+1,2)-tzz(-ml+i,j,2)
	        b2=tzz(-ml+i,j+2,2)-tzz(-ml+i,j-1,2)
	        b3=tzz(-ml+i,j+3,2)-tzz(-ml+i,j-2,2)
	        b4=tzz(-ml+i,j+4,2)-tzz(-ml+i,j-3,2)
	        s1=1/density(-ml+i,j)*r2*(ef1*b1-ef2*b2+ef3*b3-ef4*b4)
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
          a1=txx(nx1-i,j,2)-txx(nx1-i-1,j,2)
	        a2=txx(nx1-i+1,j,2)-txx(nx1-i-2,j,2)
	        a3=txx(nx1-i+2,j,2)-txx(nx1-i-3,j,2)
	        a4=txx(nx1-i+3,j,2)-txx(nx1-i-4,j,2)
	        s1=1/density(nx1-i,j)*r1*(ef1*a1-ef2*a2+ef3*a3-ef4*a4)
	        s2=(1.0-0.5*qx(i))*vxxi2(i,j,1)			!
	        vxxi2(i,j,2)=(s1+s2)*qxd(i)
          
	        a1=txz(nx1-i,j,2)-txz(nx1-i,j-1,2)
	        a2=txz(nx1-i,j+1,2)-txz(nx1-i,j-2,2)
	        a3=txz(nx1-i,j+2,2)-txz(nx1-i,j-3,2)
	        a4=txz(nx1-i,j+3,2)-txz(nx1-i,j-4,2)
	        s1=1/density(nx1-i,j)*r1*(ef1*a1-ef2*a2+ef3*a3-ef4*a4)
	        s2=0.0
	        vxzi2(i,j,2)=vxzi2(i,j,1)+(s1+s2)
	        vx(nx1-i,j,2)=vxxi2(i,j,2)+vxzi2(i,j,2)
          
	        b1=txz(nx1-i+1,j,2)-txz(nx1-i,j,2)
	        b2=txz(nx1-i+2,j,2)-txz(nx1-i-1,j,2)
	        b3=txz(nx1-i+3,j,2)-txz(nx1-i-3,j,2)
	        b4=txz(nx1-i+4,j,2)-txz(nx1-i-4,j,2)
	        s1=1/density(nx1-i,j)*r2*(ef1*b1-ef2*b2+ef3*b3-ef4*b4)
	        s2=(1.0-0.5*qx(i))*vzxi2(i,j,1)
	        vzxi2(i,j,2)=(s1+s2)*qxd(i)
          
	        b1=tzz(nx1-i,j+1,2)-tzz(nx1-i,j,2)
	        b2=tzz(nx1-i,j+2,2)-tzz(nx1-i,j-1,2)
	        b3=tzz(nx1-i,j+3,2)-tzz(nx1-i,j-2,2)
	        b4=tzz(nx1-i,j+4,2)-tzz(nx1-i,j-3,2)
	        s1=1/density(nx1-i,j)*r2*(ef1*b1-ef2*b2+ef3*b3-ef4*b4)
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
	        a1=txx(i,-ml+j,2)-txx(i-1,-ml+j,2)
	        a2=txx(i+1,-ml+j,2)-txx(i-2,-ml+j,2)
	        a3=txx(i+2,-ml+j,2)-txx(i-3,-ml+j,2)
	        a4=txx(i+3,-ml+j,2)-txx(i-4,-ml+j,2)
	        s1=1/density(i,-ml+j)*r1*(ef1*a1-ef2*a2+ef3*a3-ef4*a4)
	        s2=(1.0-0.5*qx(k))*vxxj1(i,j,1)			
	        vxxj1(i,j,2)=(s1+s2)*qxd(k)
          
	        a1=txz(i,-ml+j,2)-txz(i,-ml+j-1,2)
	        a2=txz(i,-ml+j+1,2)-txz(i,-ml+j-2,2)
	        a3=txz(i,-ml+j+2,2)-txz(i,-ml+j-3,2)
	        a4=txz(i,-ml+j+3,2)-txz(i,-ml+j-4,2)
	        s1=1/density(i,-ml+j)*r1*(ef1*a1-ef2*a2+ef3*a3-ef4*a4)
          s2=(1.0-0.5*qx(j))*vxzj1(i,j,1)
	        vxzj1(i,j,2)=(s1+s2)*qxd(j)
	        vx(i,-ml+j,2)=vxxj1(i,j,2)+vxzj1(i,j,2)
          
	        b1=txz(i+1,-ml+j,2)-txz(i,-ml+j,2)
	        b2=txz(i+2,-ml+j,2)-txz(i-1,-ml+j,2)
	        b3=txz(i+3,-ml+j,2)-txz(i-3,-ml+j,2)
	        b4=txz(i+4,-ml+j,2)-txz(i-4,-ml+j,2)
	        s1=1/density(i,-ml+j)*r2*(ef1*b1-ef2*b2+ef3*b3-ef4*b4)
	        s2=(1.0-0.5*qx(k))*vzxj1(i,j,1)
	        vzxj1(i,j,2)=(s1+s2)*qxd(k)
          
	        b1=tzz(i,-ml+j+1,2)-tzz(i,-ml+j,2)
	        b2=tzz(i,-ml+j+2,2)-tzz(i,-ml+j-1,2)
	        b3=tzz(i,-ml+j+3,2)-tzz(i,-ml+j-2,2)
	        b4=tzz(i,-ml+j+4,2)-tzz(i,-ml+j-3,2)
	        s1=1/density(i,-ml+j)*r2*(ef1*b1-ef2*b2+ef3*b3-ef4*b4)
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
	        a1=txx(i,nz1-j,2)-txx(i-1,nz1-j,2)
	        a2=txx(i+1,nz1-j,2)-txx(i-2,nz1-j,2)
	        a3=txx(i+2,nz1-j,2)-txx(i-3,nz1-j,2)
	        a4=txx(i+3,nz1-j,2)-txx(i-4,nz1-j,2)
	        s1=1/density(i,nz1-j)*r1*(ef1*a1-ef2*a2+ef3*a3-ef4*a4)
	        s2=(1.0-0.5*qx(k))*vxxj2(i,j,1)			!
	        vxxj2(i,j,2)=(s1+s2)*qxd(k)
          
	        a1=txz(i,nz1-j,2)-txz(i,nz1-j-1,2)
	        a2=txz(i,nz1-j+1,2)-txz(i,nz1-j-2,2)
	        a3=txz(i,nz1-j+2,2)-txz(i,nz1-j-3,2)
	        a4=txz(i,nz1-j+3,2)-txz(i,nz1-j-4,2)
	        s1=1/density(i,nz1-j)*r1*(ef1*a1-ef2*a2+ef3*a3-ef4*a4)
	        s2=(1.0-0.5*qx(j))*vxzj2(i,j,1)
	        vxzj2(i,j,2)=(s1+s2)*qxd(j)
	        vx(i,nz1-j,2)=vxxj2(i,j,2)+vxzj2(i,j,2)
          
	        b1=txz(i+1,nz1-j,2)-txz(i,nz1-j,2)
	        b2=txz(i+2,nz1-j,2)-txz(i-1,nz1-j,2)
	        b3=txz(i+3,nz1-j,2)-txz(i-3,nz1-j,2)
	        b4=txz(i+4,nz1-j,2)-txz(i-4,nz1-j,2)
	        s1=1/density(i,nz1-j)*r2*(ef1*b1-ef2*b2+ef3*b3-ef4*b4)
	        s2=(1.0-0.5*qx(k))*vzxj2(i,j,1)
	        vzxj2(i,j,2)=(s1+s2)*qxd(k)
          
	        b1=tzz(i,nz1-j+1,2)-tzz(i,nz1-j,2)
	        b2=tzz(i,nz1-j+2,2)-tzz(i,nz1-j-1,2)
	        b3=tzz(i,nz1-j+3,2)-tzz(i,nz1-j-2,2)
	        b4=tzz(i,nz1-j+4,2)-tzz(i,nz1-j-3,2)
	        s1=1/density(i,nz1-j)*r2*(ef1*b1-ef2*b2+ef3*b3-ef4*b4)
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
              
      If(mod(iit,400).eq.0) Then
        k=0
        Do i=-ml,nx+ml     
          k=k+1
          count=int(iit/400)
          Write(count+100,rec=k) head,(vx(i,j,2),j=-ml,nz+ml) !输出波场快照 
          Write(count+200,rec=k) head,(vz(i,j,2),j=-ml,nz+ml) 
        End Do
      End If
      
      If(mod(iit,10).eq.0) Write(*,*) iit,vx(is,js,2)
      
    Else
      iit=it/2.0
      
      Do j=1,nz
        Do i=1,nx
          
          If(iit.le.300) Then
            If(i.ge.ii1.and.i.le.ii2.and.j.ge.jj1.and.j.le.jj2) Then	  
	            dxz=((i-is)**2+(j-js)**2)*0.1               
              fx=fc0*exp(-dxz)
              fz=fc0*exp(-dxz)
            End If
          End If

          a11=vx(i+1,j,2)-vx(i,j,2)                               
          a12=vx(i+2,j,2)-vx(i-1,j,2)                                 
          a13=vx(i+3,j,2)-vx(i-2,j,2)                                  
          a14=vx(i+4,j,2)-vx(i-3,j,2)                                  
          a21=vz(i,j,2)-vz(i,j-1,2)                                    
          a22=vz(i,j+1,2)-vz(i,j-2,2)                                  
          a23=vz(i,j+2,2)-vz(i,j-3,2)                                  
          a24=vz(i,j+3,2)-vz(i,j-4,2)                                  
          b11=vz(i,j,2)-vz(i,j-1,2)                                    
          b12=vz(i,j+1,2)-vz(i,j-2,2)                                  
          b13=vz(i,j+2,2)-vz(i,j-3,2)                                  
          b14=vz(i,j+3,2)-vz(i,j-4,2)                                  
          b21=vx(i+1,j,2)-vx(i,j,2)                                    
          b22=vx(i+2,j,2)-vx(i-1,j,2)                                  
          b23=vx(i+3,j,2)-vx(i-2,j,2)                                  
          b24=vx(i+4,j,2)-vx(i-3,j,2)                                  
          c11=vx(i,j+1,2)-vx(i,j,2)
          c12=vx(i,j+2,2)-vx(i,j-1,2)
          c13=vx(i,j+3,2)-vx(i,j-2,2)
          c14=vx(i,j+4,2)-vx(i,j-3,2)
          c21=vz(i,j,2)-vz(i-1,j,2)
          c22=vz(i+1,j,2)-vz(i-2,j,2)
          c23=vz(i+2,j,2)-vz(i-3,j,2)
          c24=vz(i+3,j,2)-vz(i-4,j,2)
          txx(i,j,2)=txx(i,j,1)+r1*Lamda_2Miu(i,j)*(ef1*a11-ef2*a12+ef3*a13-ef4*a14)+r2*Lamda(i,j)*(ef1*a21-ef2*a22+ef3*a23-ef4*a24)+fx
          tzz(i,j,2)=tzz(i,j,1)+r2*Lamda_2Miu(i,j)*(ef1*b11-ef2*b12+ef3*b13-ef4*b14)+r1*Lamda(i,j)*(ef1*b21-ef2*b22+ef3*b23-ef4*b24)+fz
          txz(i,j,2)=txz(i,j,1)+r2*Miu(i,j)*(ef1*c11-ef2*c12+ef3*c13-ef4*c14)+r1*Miu(i,j)*(ef1*c21-ef2*c22+ef3*c23-ef4*c24)
        End Do
      End Do
      
      Do j=1,nz                   !i=1--->-(ml-1), i=ml--->0	  左边界
        Do i=4,ml
	        a11=vx(-ml+i+1,j,2)-vx(-ml+i,j,2)
		      a12=vx(-ml+i+2,j,2)-vx(-ml+i-1,j,2)
	        a13=vx(-ml+i+3,j,2)-vx(-ml+i-2,j,2)
	        a14=vx(-ml+i+4,j,2)-vx(-ml+i-3,j,2)
	        s1=r1*Lamda_2Miu(-ml+i,j)*(ef1*a11-ef2*a12+ef3*a13-ef4*a14)
	        s2=(1.0-0.5*qx(i))*t1xi1(i,j,1)
	        t1xi1(i,j,2)=(s1+s2)*qxd(i)
          
	        a21=vz(-ml+i,j,2)-vz(-ml+i,j-1,2)
		      a22=vz(-ml+i,j+1,2)-vz(-ml+i,j-2,2)
	        a23=vz(-ml+i,j+2,2)-vz(-ml+i,j-3,2)
	        a24=vz(-ml+i,j+3,2)-vz(-ml+i,j-4,2)
	        s1=r2*Lamda(-ml+i,j)*(ef1*a21-ef2*a22+ef3*a23-ef4*a24)
	        s2=0.0 
	        t1zi1(i,j,2)=t1zi1(i,j,1)+s1+s2
	        txx(-ml+i,j,2)=t1xi1(i,j,2)+t1zi1(i,j,2)
          
	        b21=vx(-ml+i+1,j,2)-vx(-ml+i,j,2)
		      b22=vx(-ml+i+2,j,2)-vx(-ml+i-1,j,2)
	        b23=vx(-ml+i+3,j,2)-vx(-ml+i-2,j,2)
	        b24=vx(-ml+i+4,j,2)-vx(-ml+i-3,j,2)
	        s1=r1*Lamda(-ml+i,j)*(ef1*b21-ef2*b22+ef3*b23-ef4*b24)
		      s2=(1.0-0.5*qx(i))*t2xi1(i,j,1)
	        t2xi1(i,j,2)=(s1+s2)*qxd(i)
          
	        b11=vz(-ml+i,j,2)-vz(-ml+i,j-1,2)
		      b12=vz(-ml+i,j+1,2)-vz(-ml+i,j-2,2)
	        b13=vz(-ml+i,j+2,2)-vz(-ml+i,j-3,2)
	        b14=vz(-ml+i,j+3,2)-vz(-ml+i,j-4,2)
	        s1=r2*Lamda_2Miu(-ml+i,j)*(ef1*b11-ef2*b12+ef3*b13-ef4*b14)
	        s2=0.0
	        t2zi1(i,j,2)=t2zi1(i,j,1)+s1+s2
	        tzz(-ml+i,j,2)=t2xi1(i,j,2)+t2zi1(i,j,2)
          
	        c11=vz(-ml+i,j,2)-vz(-ml+i-1,j,2)
		      c12=vz(-ml+i+1,j,2)-vz(-ml+i-2,j,2)
	        c13=vz(-ml+i+2,j,2)-vz(-ml+i-3,j,2)
	        c14=vz(-ml+i+3,j,2)-vz(-ml+i-4,j,2)
	        s1=r2*Miu(-ml+i,j)*(ef1*c11-ef2*c12+ef3*c13-ef4*c14)
	        s2=(1.0-0.5*qx(i))*t3xi1(i,j,1)
	        t3xi1(i,j,2)=(s1+s2)*qxd(i)
	        
	        c21=vx(-ml+i,j+1,2)-vx(-ml+i,j,2)
		      c22=vx(-ml+i,j+2,2)-vx(-ml+i,j-1,2)
	        c23=vx(-ml+i,j+3,2)-vx(-ml+i,j-2,2)
	        c24=vx(-ml+i,j+4,2)-vx(-ml+i,j-3,2)
	        s1=r1*Miu(-ml+i,j)*(ef1*c21-ef2*c22+ef3*c23-ef4*c24)
	        s2=0.0
	        t3zi1(i,j,2)=t3zi1(i,j,1)+s1+s2
	        txz(-ml+i,j,2)=t3xi1(i,j,2)+t3zi1(i,j,2)
        End Do
      End Do
      
      Do j=1,nz
        Do i=1,ml
	        t1xi1(i,j,1)=t1xi1(i,j,2)
	        t1zi1(i,j,1)=t1zi1(i,j,2)
	        t2xi1(i,j,1)=t2xi1(i,j,2)
	        t2zi1(i,j,1)=t2zi1(i,j,2)
	        t3xi1(i,j,1)=t3xi1(i,j,2)
	        t3zi1(i,j,1)=t3zi1(i,j,2)
        End Do
      End Do
      
      nx1=nx+ml+1
      Do j=1,nz                !i=1--->-(ml-1), i=ml--->nx+1 右边界
        Do i=ml,5,-1
	        a11=vx(nx1-i+1,j,2)-vx(nx1-i,j,2)
		      a12=vx(nx1-i+2,j,2)-vx(nx1-i-1,j,2)
	        a13=vx(nx1-i+3,j,2)-vx(nx1-i-2,j,2)
	        a14=vx(nx1-i+4,j,2)-vx(nx1-i-3,j,2)
	        s1=r1*Lamda_2Miu(nx1-i,j)*(ef1*a11-ef2*a12+ef3*a13-ef4*a14)
	        s2=(1.0-0.5*qx(i))*t1xi2(i,j,1)
	        t1xi2(i,j,2)=(s1+s2)*qxd(i)
          
	        a21=vz(nx1-i,j,2)-vz(nx1-i,j-1,2)
		      a22=vz(nx1-i,j+1,2)-vz(nx1-i,j-2,2)
	        a23=vz(nx1-i,j+2,2)-vz(nx1-i,j-3,2)
	        a24=vz(nx1-i,j+3,2)-vz(nx1-i,j-4,2)
	        s1=r2*Lamda(nx1-i,j)*(ef1*a21-ef2*a22+ef3*a23-ef4*a24)
	        s2=0.0 
	        t1zi2(i,j,2)=t1zi2(i,j,1)+s1+s2
	        txx(nx1-i,j,2)=t1xi2(i,j,2)+t1zi2(i,j,2)
          
	        b21=vx(nx1-i+1,j,2)-vx(nx1-i,j,2)
		      b22=vx(nx1-i+2,j,2)-vx(nx1-i-1,j,2)
	        b23=vx(nx1-i+3,j,2)-vx(nx1-i-2,j,2)
	        b24=vx(nx1-i+4,j,2)-vx(nx1-i-3,j,2)
	        s1=r1*Lamda(nx1-i,j)*(ef1*b21-ef2*b22+ef3*b23-ef4*b24)
		      s2=(1.0-0.5*qx(i))*t2xi2(i,j,1)
	        t2xi2(i,j,2)=(s1+s2)*qxd(i)
          
	        b11=vz(nx1-i,j,2)-vz(nx1-i,j-1,2)
		      b12=vz(nx1-i,j+1,2)-vz(nx1-i,j-2,2)
	        b13=vz(nx1-i,j+2,2)-vz(nx1-i,j-3,2)
	        b14=vz(nx1-i,j+3,2)-vz(nx1-i,j-4,2)
	        s1=r2*Lamda_2Miu(nx1-i,j)*(ef1*b11-ef2*b12+ef3*b13-ef4*b14)
	        s2=0.0
	        t2zi2(i,j,2)=t2zi2(i,j,1)+s1+s2
	        tzz(nx1-i,j,2)=t2xi2(i,j,2)+t2zi2(i,j,2)
          
	        c11=vz(nx1-i,j,2)-vz(nx1-i-1,j,2)
		      c12=vz(nx1-i+1,j,2)-vz(nx1-i-2,j,2)
	        c13=vz(nx1-i+2,j,2)-vz(nx1-i-3,j,2)
	        c14=vz(nx1-i+3,j,2)-vz(nx1-i-4,j,2)
	        s1=r2*Miu(nx1-i,j)*(ef1*c11-ef2*c12+ef3*c13-ef4*c14)
	        s2=(1.0-0.5*qx(i))*t3xi2(i,j,1)
	        t3xi2(i,j,2)=(s1+s2)*qxd(i)
	        
	        c21=vx(nx1-i,j+1,2)-vx(nx1-i,j,2)
		      c22=vx(nx1-i,j+2,2)-vx(nx1-i,j-1,2)
	        c23=vx(nx1-i,j+3,2)-vx(nx1-i,j-2,2)
	        c24=vx(nx1-i,j+4,2)-vx(nx1-i,j-3,2)
	        s1=r1*Miu(nx1-i,j)*(ef1*c21-ef2*c22+ef3*c23-ef4*c24)
	        s2=0.0
	        t3zi2(i,j,2)=t3zi2(i,j,1)+s1+s2
	        txz(nx1-i,j,2)=t3xi2(i,j,2)+t3zi2(i,j,2)
        End Do
      End Do
      
      Do j=1,nz
        Do i=1,ml
	        t1xi2(i,j,1)=t1xi2(i,j,2)
	        t1zi2(i,j,1)=t1zi2(i,j,2)
	        t2xi2(i,j,1)=t2xi2(i,j,2)
	        t2zi2(i,j,1)=t2zi2(i,j,2)
	        t3xi2(i,j,1)=t3xi2(i,j,2)
	        t3zi2(i,j,1)=t3zi2(i,j,2)
        End Do
      End Do
      
      Do j=4,ml                   ! j=ml--->0	  上边界
        Do i=-ml+4,nx+ml-4
	        k=ml
	        if(i.le.0) k=ml+i
	        if(i.gt.nx) k=nx+ml-i+1
	        a11=vx(i+1,-ml+j,2)-vx(i,-ml+j,2)
		      a12=vx(i+2,-ml+j,2)-vx(i-1,-ml+j,2)
	        a13=vx(i+3,-ml+j,2)-vx(i-2,-ml+j,2)
	        a14=vx(i+4,-ml+j,2)-vx(i-3,-ml+j,2)
	        s1=r1*Lamda_2Miu(i,-ml+j)*(ef1*a11-ef2*a12+ef3*a13-ef4*a14)
	        s2=(1.0-0.5*qx(k))*t1xj1(i,j,1)
	        t1xj1(i,j,2)=(s1+s2)*qxd(k)
          
	        a21=vz(i,-ml+j,2)-vz(i,-ml+j-1,2)
		      a22=vz(i,-ml+j+1,2)-vz(i,-ml+j-2,2)
	        a23=vz(i,-ml+j+2,2)-vz(i,-ml+j-3,2)
	        a24=vz(i,-ml+j+3,2)-vz(i,-ml+j-4,2)
	        s1=r2*Lamda(i,-ml+j)*(ef1*a21-ef2*a22+ef3*a23-ef4*a24)
          s2=(1.0-0.5*qx(j))*t1zj1(i,j,1) 
	        t1zj1(i,j,2)=(s1+s2)*qxd(j)
	        txx(i,-ml+j,2)=t1xj1(i,j,2)+t1zj1(i,j,2)
          
	        b21=vx(i+1,-ml+j,2)-vx(i,-ml+j,2)
		      b22=vx(i+2,-ml+j,2)-vx(i-1,-ml+j,2)
	        b23=vx(i+3,-ml+j,2)-vx(i-2,-ml+j,2)
	        b24=vx(i+4,-ml+j,2)-vx(i-3,-ml+j,2)
	        s1=r1*Lamda(i,-ml+j)*(ef1*b21-ef2*b22+ef3*b23-ef4*b24)
		      s2=(1.0-0.5*qx(k))*t2xj1(i,j,1)
	        t2xj1(i,j,2)=(s1+s2)*qxd(k)
          
	        b11=vz(i,-ml+j,2)-vz(i,-ml+j-1,2)
		      b12=vz(i,-ml+j+1,2)-vz(i,-ml+j-2,2)
	        b13=vz(i,-ml+j+2,2)-vz(i,-ml+j-3,2)
	        b14=vz(i,-ml+j+3,2)-vz(i,-ml+j-4,2)
	        s1=r2*Lamda_2Miu(i,-ml+j)*(ef1*b11-ef2*b12+ef3*b13-ef4*b14)
          s2=(1.0-0.5*qx(j))*t2zj1(i,j,1)
	        t2zj1(i,j,2)=(s1+s2)*qxd(j)
	        tzz(i,-ml+j,2)=t2xj1(i,j,2)+t2zj1(i,j,2)
          
	        c11=vz(i,-ml+j,2)-vz(i-1,-ml+j,2)
		      c12=vz(i+1,-ml+j,2)-vz(i-2,-ml+j,2)
	        c13=vz(i+2,-ml+j,2)-vz(i-3,-ml+j,2)
	        c14=vz(i+3,-ml+j,2)-vz(i-4,-ml+j,2)
	        s1=r2*Miu(i,-ml+j)*(ef1*c11-ef2*c12+ef3*c13-ef4*c14)
	        s2=(1.0-0.5*qx(k))*t3xj1(i,j,1)
	        t3xj1(i,j,2)=(s1+s2)*qxd(k)
	        
	        c21=vx(i,-ml+j+1,2)-vx(i,-ml+j,2)
		      c22=vx(i,-ml+j+2,2)-vx(i,-ml+j-1,2)
	        c23=vx(i,-ml+j+3,2)-vx(i,-ml+j-2,2)
	        c24=vx(i,-ml+j+4,2)-vx(i,-ml+j-3,2)
	        s1=r1*Miu(i,-ml+j)*(ef1*c21-ef2*c22+ef3*c23-ef4*c24)
          s2=(1.0-0.5*qx(j))*t3zj1(i,j,1)
	        t3zj1(i,j,2)=(s1+s2)*qxd(j)
	        txz(i,-ml+j,2)=t3xj1(i,j,2)+t3zj1(i,j,2)
        End Do
      End Do
      
      Do j=1,ml
        Do i=-ml,nx+ml
	        t1xj1(i,j,1)=t1xj1(i,j,2)
	        t1zj1(i,j,1)=t1zj1(i,j,2)
	        t2xj1(i,j,1)=t2xj1(i,j,2)
	        t2zj1(i,j,1)=t2zj1(i,j,2)
	        t3xj1(i,j,1)=t3xj1(i,j,2)
	        t3zj1(i,j,1)=t3zj1(i,j,2)
        End Do
      End Do
      
      nz1=nz+ml+1
      Do j=ml,5,-1                ! j=ml--->nz+1 下边界
        Do i=-ml+4,nx+ml-4
	        k=ml
	        if(i.le.0) k=ml+i
	        if(i.gt.nx) k=nx+ml-i+1
	        a11=vx(i+1,nz1-j,2)-vx(i,nz1-j,2)
		      a12=vx(i+2,nz1-j,2)-vx(i-1,nz1-j,2)
	        a13=vx(i+3,nz1-j,2)-vx(i-2,nz1-j,2)
	        a14=vx(i+4,nz1-j,2)-vx(i-3,nz1-j,2)
	        s1=r1*Lamda_2Miu(i,nz1-j)*(ef1*a11-ef2*a12+ef3*a13-ef4*a14)
	        s2=(1.0-0.5*qx(k))*t1xj2(i,j,1)
	        t1xj2(i,j,2)=(s1+s2)*qxd(k)
         
	        a21=vz(i,nz1-j,2)-vz(i,nz1-j-1,2)
		      a22=vz(i,nz1-j+1,2)-vz(i,nz1-j-2,2)
	        a23=vz(i,nz1-j+2,2)-vz(i,nz1-j-3,2)
	        a24=vz(i,nz1-j+3,2)-vz(i,nz1-j-4,2)
	        s1=r2*Lamda(i,nz1-j)*(ef1*a21-ef2*a22+ef3*a23-ef4*a24)
	        s2=(1.0-0.5*qx(j))*t1zj2(i,j,2) 
	        t1zj2(i,j,2)=(s1+s2)*qxd(j)
	        txx(i,nz1-j,2)=t1xj2(i,j,2)+t1zj2(i,j,2)
         
	        b21=vx(i+1,nz1-j,2)-vx(i,nz1-j,2)
		      b22=vx(i+2,nz1-j,2)-vx(i-1,nz1-j,2)
	        b23=vx(i+3,nz1-j,2)-vx(i-2,nz1-j,2)
	        b24=vx(i+4,nz1-j,2)-vx(i-3,nz1-j,2)
	        s1=r1*Lamda(i,nz1-j)*(ef1*b21-ef2*b22+ef3*b23-ef4*b24)
		      s2=(1.0-0.5*qx(k))*t2xj2(i,j,1)
	        t2xj2(i,j,2)=(s1+s2)*qxd(k)
         
	        b11=vz(i,nz1-j,2)-vz(i,nz1-j-1,2)
		      b12=vz(i,nz1-j+1,2)-vz(i,nz1-j-2,2)
	        b13=vz(i,nz1-j+2,2)-vz(i,nz1-j-3,2)
	        b14=vz(i,nz1-j+3,2)-vz(i,nz1-j-4,2)
	        s1=r2*Lamda_2Miu(i,nz1-j)*(ef1*b11-ef2*b12+ef3*b13-ef4*b14)
	        s2=(1.0-0.5*qx(j))*t2zj2(i,j,1)
	        t2zj2(i,j,2)=(s1+s2)*qxd(j)
	        tzz(i,nz1-j,2)=t2xj2(i,j,2)+t2zj2(i,j,2)
         
	        c11=vz(i,nz1-j,2)-vz(i-1,nz1-j,2)
		      c12=vz(i+1,nz1-j,2)-vz(i-2,nz1-j,2)
	        c13=vz(i+2,nz1-j,2)-vz(i-3,nz1-j,2)
	        c14=vz(i+3,nz1-j,2)-vz(i-4,nz1-j,2)
	        s1=r2*Miu(i,nz1-j)*(ef1*c11-ef2*c12+ef3*c13-ef4*c14)
	        s2=(1.0-0.5*qx(k))*t3xj2(i,j,1)
	        t3xj2(i,j,2)=(s1+s2)*qxd(k)
	       
	        c21=vx(i,nz1-j+1,2)-vx(i,nz1-j,2)
		      c22=vx(i,nz1-j+2,2)-vx(i,nz1-j-1,2)
	        c23=vx(i,nz1-j+3,2)-vx(i,nz1-j-2,2)
	        c24=vx(i,nz1-j+4,2)-vx(i,nz1-j-3,2)
	        s1=r1*Miu(i,nz1-j)*(ef1*c21-ef2*c22+ef3*c23-ef4*c24)
	        s2=(1.0-0.5*qx(j))*t3zj2(i,j,1)
	        t3zj2(i,j,2)=(s1+s2)*qxd(j)
	        txz(i,nz1-j,2)=t3xj2(i,j,2)+t3zj2(i,j,2)
        End Do
      End Do
      
      Do j=1,ml
        Do i=-ml,nx+ml
	        t1xj2(i,j,1)=t1xj2(i,j,2)
	        t1zj2(i,j,1)=t1zj2(i,j,2)
	        t2xj2(i,j,1)=t2xj2(i,j,2)
	        t2zj2(i,j,1)=t2zj2(i,j,2)
	        t3xj2(i,j,1)=t3xj2(i,j,2)
	        t3zj2(i,j,1)=t3zj2(i,j,2)
        End Do
      End Do

      Do i=-ml,nx+ml   
        Do j=-ml,nz+ml 
          txx(i,j,1)=txx(i,j,2)
          tzz(i,j,1)=tzz(i,j,2)
          txz(i,j,1)=txz(i,j,2)
        End Do
      End Do 
      
    End If
  End Do
  
  Do i=1,n_file
    Close(i+100)
    Close(i+200)
  End Do
  
  irecl2=nt*4+240
  head(58)=nt
  head(59)=delt_t*1000000
  
  Open(16,file='synthetic_seismic_record_x.sgy',form='binary',access='direct',status='replace',recl=irecl2)
  Open(17,file='synthetic_seismic_record_z.sgy',form='binary',access='direct',status='replace',recl=irecl2)     
  Do i=ig1,ig2
    ii=i-ig1+1
    Write(16,rec=ii) head,(vxt(i,j),j=1,nt)
    Write(17,rec=ii) head,(vzt(i,j),j=1,nt)
  End Do
  Close(16)
  Close(17)
  
End Subroutine Wave_Field_Modeling


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
  
  k1=1
  k2=1
  k3=2
  
  Open(11,file=Output_filename,status='replace')
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
  
Subroutine source_force(fre_wavelet,delay_source_t0,time,fc0)
  Implicit None
  Real :: fre_wavelet,delay_source_t0
  Real :: time
  Real :: fc0
  Real :: pi
  Real :: g1,g2,g3
  
  pi=4.0*atan(1.0)	
  g1=2*pi*fre_wavelet*(time-delay_source_t0)                 !问题：这里的震源函数似乎有问题？
  g2=2*g1
  g3=g1*g1
  fc0=0.0
  If(g3.lt.10.0) Then                !问题：g3为什么要大于10？
    fc0=(1.0-0.5*g2*g2)*exp(-g3)   
  End If
End Subroutine source_force
