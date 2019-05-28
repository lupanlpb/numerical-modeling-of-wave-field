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
  Real :: c11(-ml:nx+ml,-ml:nz+ml),c13(-ml:nx+ml,-ml:nz+ml),&
          c33(-ml:nx+ml,-ml:nz+ml),c44(-ml:nx+ml,-ml:nz+ml)       !c11,c13,c33,c44,c66为刚度系数
  Real :: p11(-ml:nx+ml,-ml:nz+ml),p12(-ml:nx+ml,-ml:nz+ml),&
          p22(-ml:nx+ml,-ml:nz+ml)                                !p11,p12,p22分别为固相密度、流相密度、耦合密度
  Real :: R(-ml:nx+ml,-ml:nz+ml)                                  !R为描述孔隙流体的弹性参数
  Real :: Q1(-ml:nx+ml,-ml:nz+ml),Q3(-ml:nx+ml,-ml:nz+ml)         !Q1,Q3为耦合参数
  Real :: w11(-ml:nx+ml,-ml:nz+ml),w33(-ml:nx+ml,-ml:nz+ml)       !w11,w33为耗散参数   
  Real :: vx(-ml:nx+ml,-ml:nz+ml,2),vz(-ml:nx+ml,-ml:nz+ml,2)     !vx,vz分别是固相x方向速度分量和z方向速度分量
  Real :: vxf(-ml:nx+ml,-ml:nz+ml,2),vzf(-ml:nx+ml,-ml:nz+ml,2)   !vxf,vzf分别是流相x方向速度分量和z方向速度分量
  Real :: txx(-ml:nx+ml,-ml:nz+ml,2),tzz(-ml:nx+ml,-ml:nz+ml,2),&
          txz(-ml:nx+ml,-ml:nz+ml,2)                             !txx,tzz,txz分别是三个应力分量
  Real :: ts(-ml:nx+ml,-ml:nz+ml,2)                               !ts为流相有效应力
  Real :: vxxi1(ml,-ml:nz+ml,2),vxxi2(ml,-ml:nz+ml,2),&	          !左、右 边界 
          vxxj1(-ml:nx+ml,ml,2),vxxj2(-ml:nx+ml,ml,2)             !上、下 边界 
  Real :: vxzi1(ml,-ml:nz+ml,2),vxzi2(ml,-ml:nz+ml,2),&	          !左、右 边界 
          vxzj1(-ml:nx+ml,ml,2),vxzj2(-ml:nx+ml,ml,2)             !上、下 边界 
  Real :: vzxi1(ml,-ml:nz+ml,2),vzxi2(ml,-ml:nz+ml,2),&	          !左、右 边界 
          vzxj1(-ml:nx+ml,ml,2),vzxj2(-ml:nx+ml,ml,2)             !上、下 边界 
  Real :: vzzi1(ml,-ml:nz+ml,2),vzzi2(ml,-ml:nz+ml,2),&	          !左、右 边界 
          vzzj1(-ml:nx+ml,ml,2),vzzj2(-ml:nx+ml,ml,2)             !上、下 边界
  Real :: vxfxi1(ml,-ml:nz+ml,2),vxfxi2(ml,-ml:nz+ml,2),&	        !左、右 边界 
          vxfxj1(-ml:nx+ml,ml,2),vxfxj2(-ml:nx+ml,ml,2)           !上、下 边界 
  Real :: vxfzi1(ml,-ml:nz+ml,2),vxfzi2(ml,-ml:nz+ml,2),&	        !左、右 边界 
          vxfzj1(-ml:nx+ml,ml,2),vxfzj2(-ml:nx+ml,ml,2)           !上、下 边界
  Real :: vzfxi1(ml,-ml:nz+ml,2),vzfxi2(ml,-ml:nz+ml,2),&	        !左、右 边界 
          vzfxj1(-ml:nx+ml,ml,2),vzfxj2(-ml:nx+ml,ml,2)           !上、下 边界 
  Real :: vzfzi1(ml,-ml:nz+ml,2),vzfzi2(ml,-ml:nz+ml,2),&	        !左、右 边界 
          vzfzj1(-ml:nx+ml,ml,2),vzfzj2(-ml:nx+ml,ml,2)           !上、下 边界
  Real :: t1xi1(ml,-ml:nz+ml,2),t1xi2(ml,-ml:nz+ml,2),&           !左、右 边界 
          t1xj1(-ml:nx+ml,ml,2),t1xj2(-ml:nx+ml,ml,2)             !上、下 边界      t1=txx
  Real :: t1zi1(ml,-ml:nz+ml,2),t1zi2(ml,-ml:nz+ml,2),&           !左、右 边界
          t1zj1(-ml:nx+ml,ml,2),t1zj2(-ml:nx+ml,ml,2)             !上、下 边界
  Real :: t2xi1(ml,-ml:nz+ml,2),t2xi2(ml,-ml:nz+ml,2),&           !左、右 边界	    t2=tzz  
          t2xj1(-ml:nx+ml,ml,2),t2xj2(-ml:nx+ml,ml,2)             !上、下 边界
  Real :: t2zi1(ml,-ml:nz+ml,2),t2zi2(ml,-ml:nz+ml,2),&           !左、右 边界
          t2zj1(-ml:nx+ml,ml,2),t2zj2(-ml:nx+ml,ml,2)             !上、下 边界        
  Real :: t3xi1(ml,-ml:nz+ml,2),t3xi2(ml,-ml:nz+ml,2),&           !左、右 边界	    t3=txz
          t3xj1(-ml:nx+ml,ml,2),t3xj2(-ml:nx+ml,ml,2)             !上、下 边界
  Real :: t3zi1(ml,-ml:nz+ml,2),t3zi2(ml,-ml:nz+ml,2),&           !左、右 边界
          t3zj1(-ml:nx+ml,ml,2),t3zj2(-ml:nx+ml,ml,2)             !上、下 边界
  Real :: tsxi1(ml,-ml:nz+ml,2),tsxi2(ml,-ml:nz+ml,2),&           !左、右 边界
          tsxj1(-ml:nx+ml,ml,2),tsxj2(-ml:nx+ml,ml,2)             !上、下 边界
  Real :: tszi1(ml,-ml:nz+ml,2),tszi2(ml,-ml:nz+ml,2),&           !左、右 边界
          tszj1(-ml:nx+ml,ml,2),tszj2(-ml:nx+ml,ml,2)             !上、下 边界
  Real :: vxt(nx,nt),vzt(nx,nt)                                   !vxt,vzt分别是固相地震记录x分量和z分量
  Real :: vxft(nx,nt),vzft(nx,nt)
  Real :: xx,zz
  Real :: vum,vdm,vlm,vrm
  Real :: vp(-ml:nx+ml,-ml:nz+ml)                                 !Vp为固相纵波速度
  Real :: dx,dz
  Real :: qx(1:ml),qz(1:ml),qxf(1:ml),qzf(1:ml)                  !**************************************
  Real :: qxd(1:ml),qzd(1:ml),qxdf(1:ml),qzdf(1:ml)
  Real :: r1,r2
  Real :: temp(-ml:nx+ml,-ml:nz+ml)
  Real :: D1(-ml:nx+ml,-ml:nz+ml),D2(-ml:nx+ml,-ml:nz+ml),&
          D3(-ml:nx+ml,-ml:nz+ml)
  Integer(Kind=2) :: head(120)
  Integer(Kind=2) :: irecl1,irecl2                                !写入的一笔记录的长度
  Integer :: ii1,ii2,jj1,jj2                                      !震源区域范围
  Real :: ef1,ef2,ef3,ef4                                         !差分方程系数
  Integer :: nt2,it,iit
  Integer :: n_file,n_file2                                       !需要产生的文件个数(需小于999)
  Character(len=9) :: filename1_x,filename1_z
  Character(len=11) :: filename2_x,filename2_z
  Character(len=4) :: file_extension
  Character(len=80) :: Output_filename_x,Output_filename_z
  Character(len=16) :: vx_file,vz_file
  Character(len=16),Allocatable :: snapshot_x(:),snapshot_z(:)    !所产生的多个文件的文件名
  Character(len=18),Allocatable :: seismogram_x(:),seismogram_z(:)
  Real :: time
  Real :: pi
  Real :: f0
  Real :: dxz
  Real :: fx,fz
  Real :: s1,s2
  Integer :: Radius
  Integer :: nx1,nz1
  Integer :: count
  Integer :: irec
  Integer :: ii
  Real :: a11,a12,a13,a14,a21,a22,a23,a24
  Real :: b11,b12,b13,b14,b21,b22,b23,b24
  Real :: d11,d12,d13,d14,d21,d22,d23,d24
  Real :: e11,e12,e13,e14,e21,e22,e23,e24
  Integer :: i,j,k
  Character(len=19) :: Outpath1,Outpath2
  Character(len=26) :: Outpath3,Outpath4
  
  Outpath1='Output_Result_固相\'
  Outpath2='Output_Result_流相\'
  Outpath3='Output_Normalization_固相\'
  Outpath4='Output_Normalization_流相\'
  
  r1=delt_t/(delt_x*2.0)
  r2=delt_t/(delt_z*2.0)
  
  Open(13,file=Input_file_modelpar,status='old')
  Read(13,*)
  Do j=1,nz
    Do i=1,nx
      Read(13,*) xx,zz,c11(i,j),c13(i,j),c33(i,j),c44(i,j),p11(i,j),p12(i,j),&
                 p22(i,j),R(i,j),Q1(i,j),Q3(i,j),w11(i,j),w33(i,j)
    End Do
  End Do
  Close(13)
  
  Do j=1,nz
    Do i=1,nx
      temp(i,j)=p12(i,j)**2-p11(i,j)*p22(i,j)
    End Do
  End Do
  
  Do j=1,nz
    Do i=1,nx
      D1(i,j)=p11(i,j)/temp(i,j)
      D2(i,j)=p12(i,j)/temp(i,j)
      D3(i,j)=p22(i,j)/temp(i,j)
    End Do
  End Do
!***此处在处理边界时需注意：固相最大速度与流相最大速度不同，因为密度不同*******
  Do j=1,nz
    Do i=1,nx
      vp(i,j)=sqrt(c33(i,j)/p11(i,j))
    End Do
  End Do
  
  vum=0.0
  Do i=-ml,nx+ml
    Do j=-ml,0
      D1(i,j)=D1(i,1)
      D2(i,j)=D2(i,1)
      D3(i,j)=D3(i,1)
      w11(i,j)=w11(i,1)
      w33(i,j)=w33(i,1)
      c11(i,j)=c11(i,1)
      c13(i,j)=c13(i,1)
      c33(i,j)=c33(i,1)
      c44(i,j)=c44(i,1)
      Q1(i,j)=Q1(i,1)
      Q3(i,j)=Q3(i,1)
      R(i,j)=R(i,1)
      vp(i,j)=vp(i,1)
      vum=max(vum,vp(i,j))
    End Do
  End Do
  
  vdm=0.0
  Do i=-ml,nx+ml
    Do j=nz+1,nz+ml
      D1(i,j)=D1(i,nz)
      D2(i,j)=D2(i,nz)
      D3(i,j)=D3(i,nz)
      w11(i,j)=w11(i,nz)
      w33(i,j)=w33(i,nz)
      c11(i,j)=c11(i,nz)
      c13(i,j)=c13(i,nz)
      c33(i,j)=c33(i,nz)
      c44(i,j)=c44(i,nz)
      Q1(i,j)=Q1(i,nz)
      Q3(i,j)=Q3(i,nz)
      R(i,j)=R(i,nz)
      vp(i,j)=vp(i,nz)
    End Do
  End Do
  
  vlm=0.0
  Do j=-ml,nz+ml
    Do i=-ml,0
      D1(i,j)=D1(1,j)
      D2(i,j)=D2(1,j)
      D3(i,j)=D3(1,j)
      w11(i,j)=w11(1,j)
      w33(i,j)=w33(1,j)
      c11(i,j)=c11(1,j)
      c13(i,j)=c13(1,j)
      c33(i,j)=c33(1,j)
      c44(i,j)=c44(1,j)
      Q1(i,j)=Q1(1,j)
      Q3(i,j)=Q3(1,j)
      R(i,j)=R(1,j)
      vp(i,j)=vp(1,j)
      vlm=max(vlm,vp(1,j))
    End Do
  End Do
  
  vrm=0.0
  Do j=-ml,nz+ml
    Do i=nx+1,ml+nx
      D1(i,j)=D1(nx,j)
      D2(i,j)=D2(nx,j)
      D3(i,j)=D3(nx,j)
      w11(i,j)=w11(nx,j)
      w33(i,j)=w33(nx,j)
      c11(i,j)=c11(nx,j)
      c13(i,j)=c13(nx,j)
      c33(i,j)=c33(nx,j)
      c44(i,j)=c44(nx,j)
      Q1(i,j)=Q1(nx,j)
      Q3(i,j)=Q3(nx,j)
      R(i,j)=R(nx,j)
      vp(i,j)=vp(nx,j)
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
    
  vx=0.0;vz=0.0;vxf=0.0;vzf=0.0 
  txx=0.0;tzz=0.0;txz=0.0;ts=0.0
  vxxi1=0.0;vxxi2=0.0;vxxj1=0.0;vxxj2=0.0
  vxzi1=0.0;vxzi2=0.0;vxzj1=0.0;vxzj2=0.0
  vzxi1=0.0;vzxi2=0.0;vzxj1=0.0;vzxj2=0.0
  vzzi1=0.0;vzzi2=0.0;vzzj1=0.0;vzzj2=0.0
  vxfxi1=0.0;vxfxi2=0.0;vxfxj1=0.0;vxfxj2=0.0
  vxfzi1=0.0;vxfzi2=0.0;vxfzj1=0.0;vxfzj2=0.0
  vzfxi1=0.0;vzfxi2=0.0;vzfxj1=0.0;vzfxj2=0.0
  vzfzi1=0.0;vzfzi2=0.0;vzfzj1=0.0;vzfzj2=0.0
  t1xi1=0.0;t1xi2=0.0;t1xj1=0.0;t1xj2=0.0
  t1zi1=0.0;t1zi2=0.0;t1zj1=0.0;t1zj2=0.0
  t2xi1=0.0;t2xi2=0.0;t2xj1=0.0;t2xj2=0.0
  t2zi1=0.0;t2zi2=0.0;t2zj1=0.0;t2zj2=0.0
  t3xi1=0.0;t3xi2=0.0;t3xj1=0.0;t3xj2=0.0
  t3zi1=0.0;t3zi2=0.0;t3zj1=0.0;t3zj2=0.0
  tsxi1=0.0;tsxi2=0.0;tsxj1=0.0;tsxj2=0.0
  tszi1=0.0;tszi2=0.0;tszj1=0.0;tszj2=0.0

!----------------生成波场快照文件名开始-------------------------
  n_file=int(nt/200)   !用于输出固相
  Allocate(snapshot_x(1:n_file),snapshot_z(1:n_file))
  filename1_x='snapshotx'
  filename1_z='snapshotz'
  file_extension='.sgy'
  Output_filename_x='snapshots_x.txt'
  Output_filename_z='snapshots_z.txt'
  
  Call Create_file(n_file,filename1_x,file_extension,snapshot_x,Output_filename_x,Outpath1)
  Call Create_file(n_file,filename1_z,file_extension,snapshot_z,Output_filename_z,Outpath1)
  
  n_file2=int(ig2/20.0+0.5)
  Allocate(seismogram_x(1:n_file2),seismogram_z(1:n_file2))
  filename2_x='seismogramx'
  filename2_z='seismogramz'
  file_extension='.dat'
  Output_filename_x='seismogram_x.txt'
  Output_filename_z='seismogram_z.txt'
  
  Call Create_file(n_file2,filename2_x,file_extension,seismogram_x(:),Output_filename_x,Outpath1)
  Call Create_file(n_file2,filename2_z,file_extension,seismogram_z(:),Output_filename_z,Outpath1)
  
  n_file=int(nt/200)   !用于输出流相
  filename1_x='snapshotx'
  filename1_z='snapshotz'
  file_extension='.sgy'
  Output_filename_x='snapshots_x.txt'
  Output_filename_z='snapshots_z.txt'
  
  Call Create_file(n_file,filename1_x,file_extension,snapshot_x,Output_filename_x,Outpath2)
  Call Create_file(n_file,filename1_z,file_extension,snapshot_z,Output_filename_z,Outpath2)
  
  n_file2=int(ig2/20.0+0.5)
  filename2_x='seismogramx'
  filename2_z='seismogramz'
  file_extension='.dat'
  Output_filename_x='seismogram_x.txt'
  Output_filename_z='seismogram_z.txt'
  
  Call Create_file(n_file2,filename2_x,file_extension,seismogram_x(:),Output_filename_x,Outpath2)
  Call Create_file(n_file2,filename2_z,file_extension,seismogram_z(:),Output_filename_z,Outpath2)
  
!----------------生成波场快照文件名结束-------------------------

  head(58)=nz+2*ml+1
  head(59)=2000
  irecl1=(nz+2*ml+1)*4+240
  
  Do i=1,n_file
    Open(i+100,file=Outpath1//snapshot_x(i),form='binary',access='direct',status='replace',recl=irecl1)
    Open(i+200,file=Outpath1//snapshot_z(i),form='binary',access='direct',status='replace',recl=irecl1)
  End Do
  
  Do i=1,n_file2
    Open(i+300,file=Outpath1//seismogram_x(i),status='replace')
    Open(i+400,file=Outpath1//seismogram_z(i),status='replace')
  End Do
  
  Do i=1,n_file
    Open(i+500,file=Outpath2//snapshot_x(i),form='binary',access='direct',status='replace',recl=irecl1)
    Open(i+600,file=Outpath2//snapshot_z(i),form='binary',access='direct',status='replace',recl=irecl1)
  End Do
  
  Do i=1,n_file2
    Open(i+700,file=Outpath2//seismogram_x(i),status='replace')
    Open(i+800,file=Outpath2//seismogram_z(i),status='replace')
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
  pi=4.0*atan(1.0)
  Radius=10
      
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
          
	        a11=txx(i,j,2)-txx(i-1,j-1,2)
	        a12=txx(i+1,j+1,2)-txx(i-2,j-2,2)
	        a13=txx(i+2,j+2,2)-txx(i-3,j-3,2)
	        a14=txx(i+3,j+3,2)-txx(i-4,j-4,2)
          a21=txx(i,j-1,2)-txx(i-1,j,2)
          a22=txx(i+1,j-2,2)-txx(i-2,j+1,2)
          a23=txx(i+2,j-3,2)-txx(i-3,j+2,2)
          a24=txx(i+3,j-4,2)-txx(i-4,j+3,2)
          b11=txz(i,j,2)-txz(i-1,j-1,2)
          b12=txz(i+1,j+1,2)-txz(i-2,j-2,2)
          b13=txz(i+2,j+2,2)-txz(i-3,j-3,2)
          b14=txz(i+3,j+3,2)-txz(i-4,j-4,2)
          b21=txz(i,j-1,2)-txz(i-1,j,2)
          b22=txz(i+1,j-2,2)-txz(i-2,j+1,2)
          b23=txz(i+2,j-3,2)-txz(i-3,j+2,2)
          b24=txz(i+3,j-4,2)-txz(i-4,j+3,2)
	        d11=tzz(i,j,2)-tzz(i-1,j-1,2)
	        d12=tzz(i+1,j+1,2)-tzz(i-2,j-2,2)
	        d13=tzz(i+2,j+2,2)-tzz(i-3,j-3,2)
	        d14=tzz(i+3,j+3,2)-tzz(i-4,j-4,2)
          d21=tzz(i,j-1,2)-tzz(i-1,j,2)
          d22=tzz(i+1,j-2,2)-tzz(i-2,j+1,2)
          d23=tzz(i+2,j-3,2)-tzz(i-3,j+2,2)
          d24=tzz(i+3,j-4,2)-tzz(i-4,j+3,2)
          e11=ts(i,j,2)-ts(i-1,j-1,2)
	        e12=ts(i+1,j+1,2)-ts(i-2,j-2,2)
	        e13=ts(i+2,j+2,2)-ts(i-3,j-3,2)
	        e14=ts(i+3,j+3,2)-ts(i-4,j-4,2)
          e21=ts(i,j-1,2)-ts(i-1,j,2)
          e22=ts(i+1,j-2,2)-ts(i-2,j+1,2)
          e23=ts(i+2,j-3,2)-ts(i-3,j+2,2)
          e24=ts(i+3,j-4,2)-ts(i-4,j+3,2)
          vx(i,j,2)=vx(i,j,1)+delt_t*(D2(i,j)+D3(i,j))*w11(i,j)*(vx(i,j,1)-vxf(i,j,1))-&
                    D3(i,j)*r1*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))-&
                    D3(i,j)*r2*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
                    D2(i,j)*r1*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)+(ef1*e21-ef2*e22+ef3*e23-ef4*e24))+fx
          vz(i,j,2)=vz(i,j,1)+delt_t*(D2(i,j)+D3(i,j))*w33(i,j)*(vz(i,j,1)-vzf(i,j,1))-&
                    D3(i,j)*r1*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))-&
                    D3(i,j)*r2*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)-(ef1*d21-ef2*d22+ef3*d23-ef4*d24))+&
                    D2(i,j)*r2*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
          vxf(i,j,2)=vxf(i,j,1)-delt_t*(D1(i,j)+D2(i,j))*w11(i,j)*(vx(i,j,1)-vxf(i,j,1))+&
                     D2(i,j)*r1*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
                     D2(i,j)*r2*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))-&
                     D1(i,j)*r1*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)+(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
          vzf(i,j,2)=vzf(i,j,1)-delt_t*(D1(i,j)+D2(i,j))*w33(i,j)*(vz(i,j,1)-vzf(i,j,1))+&
                     D2(i,j)*r1*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
                     D2(i,j)*r2*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)-(ef1*d21-ef2*d22+ef3*d23-ef4*d24))-&
                     D1(i,j)*r2*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
        End Do
      End Do
      
      Do j=1,nz          !i=1--->-(ml-1), i=ml--->0	 左边界
        Do i=4,ml
	        a11=txx(-ml+i,j,2)-txx(-ml+i-1,j-1,2)         !txx
	        a12=txx(-ml+i+1,j+1,2)-txx(-ml+i-2,j-2,2)
	        a13=txx(-ml+i+2,j+2,2)-txx(-ml+i-3,j-3,2)
	        a14=txx(-ml+i+3,j+3,2)-txx(-ml+i-4,j-4,2)
          a21=txx(-ml+i,j-1,2)-txx(-ml+i-1,j,2)
          a22=txx(-ml+i+1,j-2,2)-txx(-ml+i-2,j+1,2)
          a23=txx(-ml+i+2,j-3,2)-txx(-ml+i-3,j+2,2)
          a24=txx(-ml+i+3,j-4,2)-txx(-ml+i-4,j+3,2)
          b11=txz(-ml+i,j,2)-txz(-ml+i-1,j-1,2)         !txz
          b12=txz(-ml+i+1,j+1,2)-txz(-ml+i-2,j-2,2)
          b13=txz(-ml+i+2,j+2,2)-txz(-ml+i-3,j-3,2)
          b14=txz(-ml+i+3,j+3,2)-txz(-ml+i-4,j-4,2)
          b21=txz(-ml+i,j-1,2)-txz(-ml+i-1,j,2)
          b22=txz(-ml+i+1,j-2,2)-txz(-ml+i-2,j+1,2)
          b23=txz(-ml+i+2,j-3,2)-txz(-ml+i-3,j+2,2)
          b24=txz(-ml+i+3,j-4,2)-txz(-ml+i-4,j+3,2)
          d11=tzz(-ml+i,j,2)-tzz(-ml+i-1,j-1,2)         !tzz
	        d12=tzz(-ml+i+1,j+1,2)-tzz(-ml+i-2,j-2,2)
	        d13=tzz(-ml+i+2,j+2,2)-tzz(-ml+i-3,j-3,2)
	        d14=tzz(-ml+i+3,j+3,2)-tzz(-ml+i-4,j-4,2)
          d21=tzz(-ml+i,j-1,2)-tzz(-ml+i-1,j,2)
          d22=tzz(-ml+i+1,j-2,2)-tzz(-ml+i-2,j+1,2)
          d23=tzz(-ml+i+2,j-3,2)-tzz(-ml+i-3,j+2,2)
          d24=tzz(-ml+i+3,j-4,2)-tzz(-ml+i-4,j+3,2)
          e11=ts(-ml+i,j,2)-ts(-ml+i-1,j-1,2)           !ts
	        e12=ts(-ml+i+1,j+1,2)-ts(-ml+i-2,j-2,2)
	        e13=ts(-ml+i+2,j+2,2)-ts(-ml+i-3,j-3,2)
	        e14=ts(-ml+i+3,j+3,2)-ts(-ml+i-4,j-4,2)
          e21=ts(-ml+i,j-1,2)-ts(-ml+i-1,j,2)
          e22=ts(-ml+i+1,j-2,2)-ts(-ml+i-2,j+1,2)
          e23=ts(-ml+i+2,j-3,2)-ts(-ml+i-3,j+2,2)
          e24=ts(-ml+i+3,j-4,2)-ts(-ml+i-4,j+3,2)
          !计算vx
	        s1=delt_t*(D2(-ml+i,j)+D3(-ml+i,j))*w11(-ml+i,j)*(vx(i,j,1)-vxf(i,j,1))-&
             D3(-ml+i,j)*r1*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
             D2(-ml+i,j)*r1*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)+(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(i))*vxxi1(i,j,1)			
	        vxxi1(i,j,2)=(s1+s2)*qxd(i)           
	        s1=-D3(-ml+i,j)*r2*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
	        s2=0.0
	        vxzi1(i,j,2)=vxzi1(i,j,1)+(s1+s2)
	        vx(-ml+i,j,2)=vxxi1(i,j,2)+vxzi1(i,j,2)
          !计算vz
          s1=-D3(-ml+i,j)*r1*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
          s2=(1.0-0.5*qx(i))*vzxi1(i,j,1)
          vzxi1(i,j,2)=(s1+s2)*qxd(i)
	        s1=delt_t*(D2(-ml+i,j)+D3(-ml+i,j))*w33(-ml+i,j)*(vz(i,j,1)-vzf(i,j,1))-&
             D3(-ml+i,j)*r2*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)-(ef1*d21-ef2*d22+ef3*d23-ef4*d24))+&
             D2(-ml+i,j)*r2*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=0.0
	        vzzi1(i,j,2)=vzzi1(i,j,1)+(s1+s2)
	        vz(-ml+i,j,2)=vzxi1(i,j,2)+vzzi1(i,j,2)
          !计算vxf
          s1=-delt_t*(D1(-ml+i,j)+D2(-ml+i,j))*w11(-ml+i,j)*(vx(i,j,1)-vxf(i,j,1))+&
             D2(-ml+i,j)*r1*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))-&
             D1(-ml+i,j)*r1*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)+(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(i))*vxfxi1(i,j,1)			
	        vxfxi1(i,j,2)=(s1+s2)*qxd(i)           
	        s1=D2(-ml+i,j)*r2*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
	        s2=0.0
	        vxfzi1(i,j,2)=vxfzi1(i,j,1)+(s1+s2)
	        vxf(-ml+i,j,2)=vxfxi1(i,j,2)+vxfzi1(i,j,2)
          !计算vzf
          s1=D2(-ml+i,j)*r1*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
          s2=(1.0-0.5*qx(i))*vzfxi1(i,j,1)
          vzfxi1(i,j,2)=(s1+s2)*qxd(i)
	        s1=-delt_t*(D1(-ml+i,j)+D2(-ml+i,j))*w33(-ml+i,j)*(vz(i,j,1)-vzf(i,j,1))+&
             D2(-ml+i,j)*r2*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)-(ef1*d21-ef2*d22+ef3*d23-ef4*d24))-&
             D1(-ml+i,j)*r2*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=0.0
	        vzfzi1(i,j,2)=vzfzi1(i,j,1)+(s1+s2)
	        vzf(-ml+i,j,2)=vzfxi1(i,j,2)+vzfzi1(i,j,2)
        End Do
      End Do
      
      Do j=1,nz
        Do i=1,ml
          vxxi1(i,j,1)=vxxi1(i,j,2)
          vxzi1(i,j,1)=vxzi1(i,j,2)
          vzxi1(i,j,1)=vzxi1(i,j,2)
          vzzi1(i,j,1)=vzzi1(i,j,2)
          vxfxi1(i,j,1)=vxfxi1(i,j,2)
          vxfzi1(i,j,1)=vxfzi1(i,j,2)
          vzfxi1(i,j,1)=vzfxi1(i,j,2)
          vzfzi1(i,j,1)=vzfzi1(i,j,2)
        End Do
      End Do
      
      nx1=nx+ml+1
      Do j=1,nz      !i=1--->-(ml-1), i=ml--->nx+1	  右边界
        Do i=ml,5,-1
          a11=txx(nx1-i,j,2)-txx(nx1-i-1,j-1,2)         !txx
	        a12=txx(nx1-i+1,j+1,2)-txx(nx1-i-2,j-2,2)
	        a13=txx(nx1-i+2,j+2,2)-txx(nx1-i-3,j-3,2)
	        a14=txx(nx1-i+3,j+3,2)-txx(nx1-i-4,j-4,2)
          a21=txx(nx1-i,j-1,2)-txx(nx1-i-1,j,2)
          a22=txx(nx1-i+1,j-2,2)-txx(nx1-i-2,j+1,2)
          a23=txx(nx1-i+2,j-3,2)-txx(nx1-i-3,j+2,2)
          a24=txx(nx1-i+3,j-4,2)-txx(nx1-i-4,j+3,2)
          b11=txz(nx1-i,j,2)-txz(nx1-i-1,j-1,2)         !txz
          b12=txz(nx1-i+1,j+1,2)-txz(nx1-i-2,j-2,2)
          b13=txz(nx1-i+2,j+2,2)-txz(nx1-i-3,j-3,2)
          b14=txz(nx1-i+3,j+3,2)-txz(nx1-i-4,j-4,2)
          b21=txz(nx1-i,j-1,2)-txz(nx1-i-1,j,2)
          b22=txz(nx1-i+1,j-2,2)-txz(nx1-i-2,j+1,2)
          b23=txz(nx1-i+2,j-3,2)-txz(nx1-i-3,j+2,2)
          b24=txz(nx1-i+3,j-4,2)-txz(nx1-i-4,j+3,2)
          d11=tzz(nx1-i,j,2)-tzz(nx1-i-1,j-1,2)         !tzz
	        d12=tzz(nx1-i+1,j+1,2)-tzz(nx1-i-2,j-2,2)
	        d13=tzz(nx1-i+2,j+2,2)-tzz(nx1-i-3,j-3,2)
	        d14=tzz(nx1-i+3,j+3,2)-tzz(nx1-i-4,j-4,2)
          d21=tzz(nx1-i,j-1,2)-tzz(nx1-i-1,j,2)
          d22=tzz(nx1-i+1,j-2,2)-tzz(nx1-i-2,j+1,2)
          d23=tzz(nx1-i+2,j-3,2)-tzz(nx1-i-3,j+2,2)
          d24=tzz(nx1-i+3,j-4,2)-tzz(nx1-i-4,j+3,2)
          e11=ts(nx1-i,j,2)-ts(nx1-i-1,j-1,2)           !ts
	        e12=ts(nx1-i+1,j+1,2)-ts(nx1-i-2,j-2,2)
	        e13=ts(nx1-i+2,j+2,2)-ts(nx1-i-3,j-3,2)
	        e14=ts(nx1-i+3,j+3,2)-ts(nx1-i-4,j-4,2)
          e21=ts(nx1-i,j-1,2)-ts(nx1-i-1,j,2)
          e22=ts(nx1-i+1,j-2,2)-ts(nx1-i-2,j+1,2)
          e23=ts(nx1-i+2,j-3,2)-ts(nx1-i-3,j+2,2)
          e24=ts(nx1-i+3,j-4,2)-ts(nx1-i-4,j+3,2)
          !计算vx
	        s1=delt_t*(D2(nx1-i,j)+D3(nx1-i,j))*w11(nx1-i,j)*(vx(i,j,1)-vxf(i,j,1))-&
             D3(nx1-i,j)*r1*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
             D2(nx1-i,j)*r1*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)+(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(i))*vxxi2(i,j,1)			
	        vxxi2(i,j,2)=(s1+s2)*qxd(i)
	        s1=-D3(nx1-i,j)*r2*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
	        s2=0.0
	        vxzi2(i,j,2)=vxzi2(i,j,1)+(s1+s2)
	        vx(nx1-i,j,2)=vxxi2(i,j,2)+vxzi2(i,j,2)
          !计算vz
          s1=-D3(nx1-i,j)*r1*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
          s2=(1.0-0.5*qx(i))*vzxi2(i,j,1)
	        vzxi2(i,j,2)=(s1+s2)*qxd(i)
	        s1=delt_t*(D2(nx1-i,j)+D3(nx1-i,j))*w33(nx1-i,j)*(vz(i,j,1)-vzf(i,j,1))-&
             D3(nx1-i,j)*r2*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)-(ef1*d21-ef2*d22+ef3*d23-ef4*d24))+&
             D2(nx1-i,j)*r2*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=0.0
	        vzzi2(i,j,2)=vzzi2(i,j,1)+(s1+s2)
	        vz(nx1-i,j,2)=vzxi2(i,j,2)+vzzi2(i,j,2)
          !计算vxf
          s1=-delt_t*(D1(nx1-i,j)+D2(nx1-i,j))*w11(nx1-i,j)*(vx(i,j,1)-vxf(i,j,1))+&
             D2(nx1-i,j)*r1*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))-&
             D1(nx1-i,j)*r1*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)+(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(i))*vxfxi2(i,j,1)
	        vxfxi2(i,j,2)=(s1+s2)*qxd(i)           
	        s1=D2(nx1-i,j)*r2*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
	        s2=0.0
	        vxfzi2(i,j,2)=vxfzi2(i,j,1)+(s1+s2)
	        vxf(nx1-i,j,2)=vxfxi2(i,j,2)+vxfzi2(i,j,2)
          !计算vzf
          s1=D2(nx1-i,j)*r1*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
          s2=(1.0-0.5*qx(i))*vzfxi2(i,j,1)
          vzfxi2(i,j,2)=(s1+s2)*qxd(i)
	        s1=-delt_t*(D1(nx1-i,j)+D2(nx1-i,j))*w33(nx1-i,j)*(vz(i,j,1)-vzf(i,j,1))+&
             D2(nx1-i,j)*r2*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)-(ef1*d21-ef2*d22+ef3*d23-ef4*d24))-&
             D1(nx1-i,j)*r2*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=0.0
	        vzfzi2(i,j,2)=vzfzi2(i,j,1)+(s1+s2)
	        vzf(nx1-i,j,2)=vzfxi2(i,j,2)+vzfzi2(i,j,2)
        End Do
      End Do
      
      Do j=1,nz
        Do i=1,ml
	        vxxi2(i,j,1)=vxxi2(i,j,2)
	        vxzi2(i,j,1)=vxzi2(i,j,2)
	        vzxi2(i,j,1)=vzxi2(i,j,2)
	        vzzi2(i,j,1)=vzzi2(i,j,2)
          vxfxi2(i,j,1)=vxfxi2(i,j,2)
	        vxfzi2(i,j,1)=vxfzi2(i,j,2)
	        vzfxi2(i,j,1)=vzfxi2(i,j,2)
	        vzfzi2(i,j,1)=vzfzi2(i,j,2)
        End Do
      End Do
      
      Do j=4,ml                   ! j=ml--->0	 上边界
        Do i=-ml+4,nx+ml-4
	        k=ml
	        if(i.le.0) k=ml+i       !处理区域1、3、7、9  
	        if(i.gt.nx) k=nx+ml-i+1
	        a11=txx(i,-ml+j,2)-txx(i-1,-ml+j-1,2)         !txx
	        a12=txx(i+1,-ml+j+1,2)-txx(i-2,-ml+j-2,2)
	        a13=txx(i+2,-ml+j+2,2)-txx(i-3,-ml+j-3,2)
	        a14=txx(i+3,-ml+j+3,2)-txx(i-4,-ml+j-4,2)
          a21=txx(i,-ml+j-1,2)-txx(i-1,-ml+j,2)
          a22=txx(i+1,-ml+j-2,2)-txx(i-2,-ml+j+1,2)
          a23=txx(i+2,-ml+j-3,2)-txx(i-3,-ml+j+2,2)
          a24=txx(i+3,-ml+j-4,2)-txx(i-4,-ml+j+3,2)
          b11=txz(i,-ml+j,2)-txz(i-1,-ml+j-1,2)         !txz
          b12=txz(i+1,-ml+j+1,2)-txz(i-2,-ml+j-2,2)
          b13=txz(i+2,-ml+j+2,2)-txz(i-3,-ml+j-3,2)
          b14=txz(i+3,-ml+j+3,2)-txz(i-4,-ml+j-4,2)
          b21=txz(i,-ml+j-1,2)-txz(i-1,-ml+j,2)
          b22=txz(i+1,-ml+j-2,2)-txz(i-2,-ml+j+1,2)
          b23=txz(i+2,-ml+j-3,2)-txz(i-3,-ml+j+2,2)
          b24=txz(i+3,-ml+j-4,2)-txz(i-4,-ml+j+3,2)
          d11=tzz(i,-ml+j,2)-tzz(i-1,-ml+j-1,2)         !tzz
	        d12=tzz(i+1,-ml+j+1,2)-tzz(i-2,-ml+j-2,2)
	        d13=tzz(i+2,-ml+j+2,2)-tzz(i-3,-ml+j-3,2)
	        d14=tzz(i+3,-ml+j+3,2)-tzz(i-4,-ml+j-4,2)
          d21=tzz(i,-ml+j-1,2)-tzz(i-1,-ml+j,2)
          d22=tzz(i+1,-ml+j-2,2)-tzz(i-2,-ml+j+1,2)
          d23=tzz(i+2,-ml+j-3,2)-tzz(i-3,-ml+j+2,2)
          d24=tzz(i+3,-ml+j-4,2)-tzz(i-4,-ml+j+3,2)
          e11=ts(i,-ml+j,2)-ts(i-1,-ml+j-1,2)           !ts
	        e12=ts(i+1,-ml+j+1,2)-ts(i-2,-ml+j-2,2)
	        e13=ts(i+2,-ml+j+2,2)-ts(i-3,-ml+j-3,2)
	        e14=ts(i+3,-ml+j+3,2)-ts(i-4,-ml+j-4,2)
          e21=ts(i,-ml+j-1,2)-ts(i-1,-ml+j,2)
          e22=ts(i+1,-ml+j-2,2)-ts(i-2,-ml+j+1,2)
          e23=ts(i+2,-ml+j-3,2)-ts(i-3,-ml+j+2,2)
          e24=ts(i+3,-ml+j-4,2)-ts(i-4,-ml+j+3,2)
          !计算vx
	        s1=delt_t*(D2(i,-ml+j)+D3(i,-ml+j))*w11(i,-ml+j)*(vx(i,j,1)-vxf(i,j,1))-&
             D3(i,-ml+j)*r1*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
             D2(i,-ml+j)*r1*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)+(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(k))*vxxj1(i,j,1)			
	        vxxj1(i,j,2)=(s1+s2)*qxd(k)
	        s1=-D3(i,-ml+j)*r2*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
	        s2=(1.0-0.5*qx(j))*vxzj1(i,j,1)
	        vxzj1(i,j,2)=(s1+s2)*qxd(j)
	        vx(i,-ml+j,2)=vxxj1(i,j,2)+vxzj1(i,j,2)
          !计算vz
          s1=-D3(i,-ml+j)*r1*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
          s2=(1.0-0.5*qx(k))*vzxj1(i,j,1)
	        vzxj1(i,j,2)=(s1+s2)*qxd(k)
	        s1=delt_t*(D2(i,-ml+j)+D3(i,-ml+j))*w33(i,-ml+j)*(vz(i,j,1)-vzf(i,j,1))-&
             D3(i,-ml+j)*r2*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)-(ef1*d21-ef2*d22+ef3*d23-ef4*d24))+&
             D2(i,-ml+j)*r2*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(j))*vzzj1(i,j,1)
	        vzzj1(i,j,2)=(s1+s2)*qxd(j)
	        vz(i,-ml+j,2)=vzxj1(i,j,2)+vzzj1(i,j,2)
          !计算vxf
          s1=-delt_t*(D1(i,-ml+j)+D2(i,-ml+j))*w11(i,-ml+j)*(vx(i,j,1)-vxf(i,j,1))+&
             D2(i,-ml+j)*r1*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))-&
             D1(i,-ml+j)*r1*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)+(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(k))*vxfxj1(i,j,1)
	        vxfxj1(i,j,2)=(s1+s2)*qxd(k)           
	        s1=D2(i,-ml+j)*r2*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
	        s2=(1.0-0.5*qx(j))*vxfzj1(i,j,1)
	        vxfzj1(i,j,2)=(s1+s2)*qxd(j)
	        vxf(i,-ml+j,2)=vxfxj1(i,j,2)+vxfzj1(i,j,2)
          !计算vzf
          s1=D2(i,-ml+j)*r1*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
          s2=(1.0-0.5*qx(k))*vzfxj1(i,j,1)
          vzfxj1(i,j,2)=(s1+s2)*qxd(k)
	        s1=-delt_t*(D1(i,-ml+j)+D2(i,-ml+j))*w33(i,-ml+j)*(vz(i,j,1)-vzf(i,j,1))+&
             D2(i,-ml+j)*r2*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)-(ef1*d21-ef2*d22+ef3*d23-ef4*d24))-&
             D1(i,-ml+j)*r2*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(j))*vzfzj1(i,j,1)
	        vzfzj1(i,j,2)=(s1+s2)*qxd(j)
	        vzf(i,-ml+j,2)=vzfxj1(i,j,2)+vzfzj1(i,j,2)
        End Do
      End Do
      
      Do j=1,ml
        Do i=-ml,nx+ml
	        vxxj1(i,j,1)=vxxj1(i,j,2)   	    
	        vxzj1(i,j,1)=vxzj1(i,j,2)
	        vzxj1(i,j,1)=vzxj1(i,j,2)   	    
	        vzzj1(i,j,1)=vzzj1(i,j,2)
          vxfxj1(i,j,1)=vxfxj1(i,j,2)   	    
	        vxfzj1(i,j,1)=vxfzj1(i,j,2)
	        vzfxj1(i,j,1)=vzfxj1(i,j,2)   	    
	        vzfzj1(i,j,1)=vzfzj1(i,j,2)
        End Do
      End Do
      
      nz1=nz+ml+1
      Do j=ml,5,-1                !i=1--->-(ml-1), i=ml--->nx+1	  下边界
        Do i=-ml+4,nx+ml-4
	        k=ml
	        if(i.le.0) k=ml+i         !处理区域1、3、7、9  
	        if(i.gt.nx) k=nx+ml-i+1
	        a11=txx(i,nz1-j,2)-txx(i-1,nz1-j-1,2)         !txx
	        a12=txx(i+1,nz1-j+1,2)-txx(i-2,nz1-j-2,2)
	        a13=txx(i+2,nz1-j+2,2)-txx(i-3,nz1-j-3,2)
	        a14=txx(i+3,nz1-j+3,2)-txx(i-4,nz1-j-4,2)
          a21=txx(i,nz1-j-1,2)-txx(i-1,nz1-j,2)
          a22=txx(i+1,nz1-j-2,2)-txx(i-2,nz1-j+1,2)
          a23=txx(i+2,nz1-j-3,2)-txx(i-3,nz1-j+2,2)
          a24=txx(i+3,nz1-j-4,2)-txx(i-4,nz1-j+3,2)
          b11=txz(i,nz1-j,2)-txz(i-1,nz1-j-1,2)         !txz
          b12=txz(i+1,nz1-j+1,2)-txz(i-2,nz1-j-2,2)
          b13=txz(i+2,nz1-j+2,2)-txz(i-3,nz1-j-3,2)
          b14=txz(i+3,nz1-j+3,2)-txz(i-4,nz1-j-4,2)
          b21=txz(i,nz1-j-1,2)-txz(i-1,nz1-j,2)
          b22=txz(i+1,nz1-j-2,2)-txz(i-2,nz1-j+1,2)
          b23=txz(i+2,nz1-j-3,2)-txz(i-3,nz1-j+2,2)
          b24=txz(i+3,nz1-j-4,2)-txz(i-4,nz1-j+3,2)
          d11=tzz(i,nz1-j,2)-tzz(i-1,nz1-j-1,2)         !tzz
	        d12=tzz(i+1,nz1-j+1,2)-tzz(i-2,nz1-j-2,2)
	        d13=tzz(i+2,nz1-j+2,2)-tzz(i-3,nz1-j-3,2)
	        d14=tzz(i+3,nz1-j+3,2)-tzz(i-4,nz1-j-4,2)
          d21=tzz(i,nz1-j-1,2)-tzz(i-1,nz1-j,2)
          d22=tzz(i+1,nz1-j-2,2)-tzz(i-2,nz1-j+1,2)
          d23=tzz(i+2,nz1-j-3,2)-tzz(i-3,nz1-j+2,2)
          d24=tzz(i+3,nz1-j-4,2)-tzz(i-4,nz1-j+3,2)
          e11=ts(i,nz1-j,2)-ts(i-1,nz1-j-1,2)           !ts
	        e12=ts(i+1,nz1-j+1,2)-ts(i-2,nz1-j-2,2)
	        e13=ts(i+2,nz1-j+2,2)-ts(i-3,nz1-j-3,2)
	        e14=ts(i+3,nz1-j+3,2)-ts(i-4,nz1-j-4,2)
          e21=ts(i,nz1-j-1,2)-ts(i-1,nz1-j,2)
          e22=ts(i+1,nz1-j-2,2)-ts(i-2,nz1-j+1,2)
          e23=ts(i+2,nz1-j-3,2)-ts(i-3,nz1-j+2,2)
          e24=ts(i+3,nz1-j-4,2)-ts(i-4,nz1-j+3,2)
          !计算vx
	        s1=delt_t*(D2(i,nz1-j)+D3(i,nz1-j))*w11(i,nz1-j)*(vx(i,j,1)-vxf(i,j,1))-&
             D3(i,nz1-j)*r1*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
             D2(i,nz1-j)*r1*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)+(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(k))*vxxj2(i,j,1)			
	        vxxj2(i,j,2)=(s1+s2)*qxd(k)
	        s1=-D3(i,nz1-j)*r2*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
	        s2=(1.0-0.5*qx(j))*vxzj2(i,j,1)
	        vxzj2(i,j,2)=(s1+s2)*qxd(j)
	        vx(i,nz1-j,2)=vxxj2(i,j,2)+vxzj2(i,j,2)
          !计算vz
          s1=-D3(i,nz1-j)*r1*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
          s2=(1.0-0.5*qx(k))*vzxj2(i,j,1)
	        vzxj2(i,j,2)=(s1+s2)*qxd(k)
	        s1=delt_t*(D2(i,nz1-j)+D3(i,nz1-j))*w33(i,nz1-j)*(vz(i,j,1)-vzf(i,j,1))-&
             D3(i,nz1-j)*r2*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)-(ef1*d21-ef2*d22+ef3*d23-ef4*d24))+&
             D2(i,nz1-j)*r2*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(j))*vzzj2(i,j,1)
	        vzzj2(i,j,2)=(s1+s2)*qxd(j)
	        vz(i,nz1-j,2)=vzxj2(i,j,2)+vzzj2(i,j,2)
          !计算vxf
          s1=-delt_t*(D1(i,nz1-j)+D2(i,nz1-j))*w11(i,nz1-j)*(vx(i,j,1)-vxf(i,j,1))+&
             D2(i,nz1-j)*r1*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))-&
             D1(i,nz1-j)*r1*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)+(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(k))*vxfxj2(i,j,1)
	        vxfxj2(i,j,2)=(s1+s2)*qxd(k)           
	        s1=D2(i,nz1-j)*r2*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
	        s2=(1.0-0.5*qx(j))*vxfzj2(i,j,1)
	        vxfzj2(i,j,2)=(s1+s2)*qxd(j)
	        vxf(i,nz1-j,2)=vxfxj2(i,j,2)+vxfzj2(i,j,2)
          !计算vzf
          s1=D2(i,nz1-j)*r1*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
          s2=(1.0-0.5*qx(k))*vzfxj2(i,j,1)
          vzfxj2(i,j,2)=(s1+s2)*qxd(k)
	        s1=-delt_t*(D1(i,nz1-j)+D2(i,nz1-j))*w33(i,nz1-j)*(vz(i,j,1)-vzf(i,j,1))+&
             D2(i,nz1-j)*r2*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)-(ef1*d21-ef2*d22+ef3*d23-ef4*d24))-&
             D1(i,nz1-j)*r2*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(j))*vzfzj2(i,j,1)
	        vzfzj2(i,j,2)=(s1+s2)*qxd(j)
	        vzf(i,nz1-j,2)=vzfxj2(i,j,2)+vzfzj2(i,j,2)
        End Do
      End Do
      
      Do j=1,ml
        Do i=-ml,nx+ml
	        vxxj2(i,j,1)=vxxj2(i,j,2)
	        vxzj2(i,j,1)=vxzj2(i,j,2)
	        vzxj2(i,j,1)=vzxj2(i,j,2)
	        vzzj2(i,j,1)=vzzj2(i,j,2)
          vxfxj2(i,j,1)=vxfxj2(i,j,2)
	        vxfzj2(i,j,1)=vxfzj2(i,j,2)
	        vzfxj2(i,j,1)=vzfxj2(i,j,2)
	        vzfzj2(i,j,1)=vzfzj2(i,j,2)
        End Do
      End Do

      Do j=-ml,nz+ml     
        Do i=-ml,nx+ml   
          vx(i,j,1)=vx(i,j,2)   !将前一时刻计算的波场值vx(i,j,2)赋值给vx(i,j,1)
          vz(i,j,1)=vz(i,j,2)
          vxf(i,j,1)=vxf(i,j,2)
          vzf(i,j,1)=vzf(i,j,2)
        End Do
      End Do
    
      Do i=1,nx
        vxt(i,iit)=vx(i,ige,2)    !将横坐标为0的前一时刻的波场值赋值给横坐标为0的检波器
        vzt(i,iit)=vz(i,ige,2)
        vxft(i,iit)=vxf(i,ige,2)
        vzft(i,iit)=vzf(i,ige,2)
      End Do
      
      Do i=1,n_file2              !输出合成地震图
        j=6+(i-1)*20
        Write(i+300,*) time,vxt(j,iit)
        Write(i+400,*) time,vzt(j,iit)
      End Do
      
      Do i=1,n_file2              !输出合成地震图
        j=6+(i-1)*20
        Write(i+700,*) time,vxft(j,iit)
        Write(i+800,*) time,vzft(j,iit)
      End Do
              
      If(mod(iit,200).eq.0) Then
        k=0        
        Do i=-ml,nx+ml     
          k=k+1
          count=int(iit/200)
          Write(count+100,rec=k) head,(vx(i,j,2),j=-ml,nz+ml) !输出波场快照 
          Write(count+200,rec=k) head,(vz(i,j,2),j=-ml,nz+ml) 
        End Do
      End If
      
      If(mod(iit,200).eq.0) Then
        k=0
        Do i=-ml,nx+ml     
          k=k+1
          count=int(iit/200)
          Write(count+500,rec=k) head,(vxf(i,j,2),j=-ml,nz+ml) !输出波场快照 
          Write(count+600,rec=k) head,(vzf(i,j,2),j=-ml,nz+ml) 
        End Do
      End If
      
      If(mod(iit,10).eq.0) Write(*,*) iit,vx(is,js,2)
      
    Else
      iit=it/2.0
      
      Do j=1,nz
        Do i=1,nx
          a11=vx(i+1,j+1,2)-vx(i,j,2)
          a12=vx(i+2,j+2,2)-vx(i-1,j-1,2)
          a13=vx(i+3,j+3,2)-vx(i-2,j-2,2)
          a14=vx(i+4,j+4,2)-vx(i-3,j-3,2)
          a21=vx(i+1,j,2)-vx(i,j+1,2)
          a22=vx(i+2,j-1,2)-vx(i-1,j+2,2)
          a23=vx(i+3,j-2,2)-vx(i-2,j+3,2)
          a24=vx(i+4,j-3,2)-vx(i-3,j+4,2)
          b11=vz(i+1,j+1,2)-vz(i,j,2)
          b12=vz(i+2,j+2,2)-vz(i-1,j-1,2)
          b13=vz(i+3,j+3,2)-vz(i-2,j-2,2)
          b14=vz(i+4,j+4,2)-vz(i-3,j-3,2)
          b21=vz(i+1,j,2)-vz(i,j+1,2)
          b22=vz(i+2,j-1,2)-vz(i-1,j+2,2)
          b23=vz(i+3,j-2,2)-vz(i-2,j+3,2)
          b24=vz(i+4,j-3,2)-vz(i-3,j+4,2)
          d11=vxf(i+1,j+1,2)-vxf(i,j,2)
          d12=vxf(i+2,j+2,2)-vxf(i-1,j-1,2)
          d13=vxf(i+3,j+3,2)-vxf(i-2,j-2,2)
          d14=vxf(i+4,j+4,2)-vxf(i-3,j-3,2)
          d21=vxf(i+1,j,2)-vxf(i,j+1,2)
          d22=vxf(i+2,j-1,2)-vxf(i-1,j+2,2)
          d23=vxf(i+3,j-2,2)-vxf(i-2,j+3,2)
          d24=vxf(i+4,j-3,2)-vxf(i-3,j+4,2)
          e11=vzf(i+1,j+1,2)-vzf(i,j,2)
          e12=vzf(i+2,j+2,2)-vzf(i-1,j-1,2)
          e13=vzf(i+3,j+3,2)-vzf(i-2,j-2,2)
          e14=vzf(i+4,j+4,2)-vzf(i-3,j-3,2)
          e21=vzf(i+1,j,2)-vzf(i,j+1,2)
          e22=vzf(i+2,j-1,2)-vzf(i-1,j+2,2)
          e23=vzf(i+3,j-2,2)-vzf(i-2,j+3,2)
          e24=vzf(i+4,j-3,2)-vzf(i-3,j+4,2)
          txx(i,j,2)=txx(i,j,1)+r1*c11(i,j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
                                r2*c13(i,j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
                                r1*Q1(i,j)*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)+(ef1*d21-ef2*d22+ef3*d23-ef4*d24))+&
                                r2*Q1(i,j)*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
          tzz(i,j,2)=tzz(i,j,1)+r1*c13(i,j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
                                r2*c33(i,j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
                                r1*Q3(i,j)*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)+(ef1*d21-ef2*d22+ef3*d23-ef4*d24))+&
                                r2*Q3(i,j)*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
          txz(i,j,2)=txz(i,j,1)+r1*c44(i,j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
                                r2*c44(i,j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)-(ef1*a21-ef2*a22+ef3*a23-ef4*a24))
          ts(i,j,2) = ts(i,j,1)+r1*Q1(i,j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
                                r2*Q3(i,j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
                                r1*R(i,j)*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)+(ef1*d21-ef2*d22+ef3*d23-ef4*d24))+&
                                r2*R(i,j)*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
        End Do
      End Do
      
      Do j=1,nz                   !i=1--->-(ml-1), i=ml--->0	  左边界
        Do i=4,ml
	        a11=vx(-ml+i+1,j+1,2)-vx(-ml+i,j,2)           !vx
          a12=vx(-ml+i+2,j+2,2)-vx(-ml+i-1,j-1,2)
          a13=vx(-ml+i+3,j+3,2)-vx(-ml+i-2,j-2,2)
          a14=vx(-ml+i+4,j+4,2)-vx(-ml+i-3,j-3,2)
          a21=vx(-ml+i+1,j,2)-vx(-ml+i,j+1,2)
          a22=vx(-ml+i+2,j-1,2)-vx(-ml+i-1,j+2,2)
          a23=vx(-ml+i+3,j-2,2)-vx(-ml+i-2,j+3,2)
          a24=vx(-ml+i+4,j-3,2)-vx(-ml+i-3,j+4,2)
          b11=vz(-ml+i+1,j+1,2)-vz(-ml+i,j,2)           !vz
          b12=vz(-ml+i+2,j+2,2)-vz(-ml+i-1,j-1,2)
          b13=vz(-ml+i+3,j+3,2)-vz(-ml+i-2,j-2,2)
          b14=vz(-ml+i+4,j+4,2)-vz(-ml+i-3,j-3,2)
          b21=vz(-ml+i+1,j,2)-vz(-ml+i,j+1,2)
          b22=vz(-ml+i+2,j-1,2)-vz(-ml+i-1,j+2,2)
          b23=vz(-ml+i+3,j-2,2)-vz(-ml+i-2,j+3,2)
          b24=vz(-ml+i+4,j-3,2)-vz(-ml+i-3,j+4,2)
          d11=vxf(-ml+i+1,j+1,2)-vxf(-ml+i,j,2)         !vxf
          d12=vxf(-ml+i+2,j+2,2)-vxf(-ml+i-1,j-1,2)
          d13=vxf(-ml+i+3,j+3,2)-vxf(-ml+i-2,j-2,2)
          d14=vxf(-ml+i+4,j+4,2)-vxf(-ml+i-3,j-3,2)
          d21=vxf(-ml+i+1,j,2)-vxf(-ml+i,j+1,2)
          d22=vxf(-ml+i+2,j-1,2)-vxf(-ml+i-1,j+2,2)
          d23=vxf(-ml+i+3,j-2,2)-vxf(-ml+i-2,j+3,2)
          d24=vxf(-ml+i+4,j-3,2)-vxf(-ml+i-3,j+4,2)
          e11=vzf(-ml+i+1,j+1,2)-vzf(-ml+i,j,2)         !vzf
          e12=vzf(-ml+i+2,j+2,2)-vzf(-ml+i-1,j-1,2)
          e13=vzf(-ml+i+3,j+3,2)-vzf(-ml+i-2,j-2,2)
          e14=vzf(-ml+i+4,j+4,2)-vzf(-ml+i-3,j-3,2)
          e21=vzf(-ml+i+1,j,2)-vzf(-ml+i,j+1,2)
          e22=vzf(-ml+i+2,j-1,2)-vzf(-ml+i-1,j+2,2)
          e23=vzf(-ml+i+3,j-2,2)-vzf(-ml+i-2,j+3,2)
          e24=vzf(-ml+i+4,j-3,2)-vzf(-ml+i-3,j+4,2)
          !计算txx
	        s1=r1*c11(-ml+i,j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
             r1*Q1(-ml+i,j)*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)+(ef1*d21-ef2*d22+ef3*d23-ef4*d24))
	        s2=(1.0-0.5*qx(i))*t1xi1(i,j,1)
	        t1xi1(i,j,2)=(s1+s2)*qxd(i)
	        s1=r2*c13(-ml+i,j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
             r2*Q1(-ml+i,j)*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=0.0 
	        t1zi1(i,j,2)=t1zi1(i,j,1)+s1+s2
	        txx(-ml+i,j,2)=t1xi1(i,j,2)+t1zi1(i,j,2)
          !计算tzz
	        s1=r1*c13(-ml+i,j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
             r1*Q3(-ml+i,j)*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)+(ef1*d21-ef2*d22+ef3*d23-ef4*d24))
          s2=(1.0-0.5*qx(i))*t2xi1(i,j,1)
	        t2xi1(i,j,2)=(s1+s2)*qxd(i)
	        s1=r2*c33(-ml+i,j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
             r2*Q3(-ml+i,j)*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=0.0
	        t2zi1(i,j,2)=t2zi1(i,j,1)+s1+s2
	        tzz(-ml+i,j,2)=t2xi1(i,j,2)+t2zi1(i,j,2)
          !计算txz
	        s1=r1*c44(-ml+i,j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
	        s2=(1.0-0.5*qx(i))*t3xi1(i,j,1)
	        t3xi1(i,j,2)=(s1+s2)*qxd(i)
	        s1=r2*c44(-ml+i,j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)-(ef1*a21-ef2*a22+ef3*a23-ef4*a24))
	        s2=0.0
	        t3zi1(i,j,2)=t3zi1(i,j,1)+s1+s2
	        txz(-ml+i,j,2)=t3xi1(i,j,2)+t3zi1(i,j,2)
          !计算ts
	        s1=r1*Q1(-ml+i,j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
             r1*R(-ml+i,j)*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)+(ef1*d21-ef2*d22+ef3*d23-ef4*d24))
	        s2=(1.0-0.5*qx(i))*tsxi1(i,j,1)
	        tsxi1(i,j,2)=(s1+s2)*qxd(i)
	        s1=r2*Q3(-ml+i,j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
             r2*R(-ml+i,j)*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=0.0
	        tszi1(i,j,2)=tszi1(i,j,1)+s1+s2
	        ts(-ml+i,j,2)=tsxi1(i,j,2)+tszi1(i,j,2) 
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
          tsxi1(i,j,1)=tsxi1(i,j,2)
	        tszi1(i,j,1)=tszi1(i,j,2)
        End Do
      End Do
      
      nx1=nx+ml+1
      Do j=1,nz                !i=1--->-(ml-1), i=ml--->nx+1 右边界
        Do i=ml,5,-1
          a11=vx(nx1-i+1,j+1,2)-vx(nx1-i,j,2)           !vx
          a12=vx(nx1-i+2,j+2,2)-vx(nx1-i-1,j-1,2)
          a13=vx(nx1-i+3,j+3,2)-vx(nx1-i-2,j-2,2)
          a14=vx(nx1-i+4,j+4,2)-vx(nx1-i-3,j-3,2)
          a21=vx(nx1-i+1,j,2)-vx(nx1-i,j+1,2)
          a22=vx(nx1-i+2,j-1,2)-vx(nx1-i-1,j+2,2)
          a23=vx(nx1-i+3,j-2,2)-vx(nx1-i-2,j+3,2)
          a24=vx(nx1-i+4,j-3,2)-vx(nx1-i-3,j+4,2)
          b11=vz(nx1-i+1,j+1,2)-vz(nx1-i,j,2)           !vz
          b12=vz(nx1-i+2,j+2,2)-vz(nx1-i-1,j-1,2)
          b13=vz(nx1-i+3,j+3,2)-vz(nx1-i-2,j-2,2)
          b14=vz(nx1-i+4,j+4,2)-vz(nx1-i-3,j-3,2)
          b21=vz(nx1-i+1,j,2)-vz(nx1-i,j+1,2)
          b22=vz(nx1-i+2,j-1,2)-vz(nx1-i-1,j+2,2)
          b23=vz(nx1-i+3,j-2,2)-vz(nx1-i-2,j+3,2)
          b24=vz(nx1-i+4,j-3,2)-vz(nx1-i-3,j+4,2)
          d11=vxf(nx1-i+1,j+1,2)-vxf(nx1-i,j,2)         !vxf
          d12=vxf(nx1-i+2,j+2,2)-vxf(nx1-i-1,j-1,2)
          d13=vxf(nx1-i+3,j+3,2)-vxf(nx1-i-2,j-2,2)
          d14=vxf(nx1-i+4,j+4,2)-vxf(nx1-i-3,j-3,2)
          d21=vxf(nx1-i+1,j,2)-vxf(nx1-i,j+1,2)
          d22=vxf(nx1-i+2,j-1,2)-vxf(nx1-i-1,j+2,2)
          d23=vxf(nx1-i+3,j-2,2)-vxf(nx1-i-2,j+3,2)
          d24=vxf(nx1-i+4,j-3,2)-vxf(nx1-i-3,j+4,2)
          e11=vzf(nx1-i+1,j+1,2)-vzf(nx1-i,j,2)         !vzf
          e12=vzf(nx1-i+2,j+2,2)-vzf(nx1-i-1,j-1,2)
          e13=vzf(nx1-i+3,j+3,2)-vzf(nx1-i-2,j-2,2)
          e14=vzf(nx1-i+4,j+4,2)-vzf(nx1-i-3,j-3,2)
          e21=vzf(nx1-i+1,j,2)-vzf(nx1-i,j+1,2)
          e22=vzf(nx1-i+2,j-1,2)-vzf(nx1-i-1,j+2,2)
          e23=vzf(nx1-i+3,j-2,2)-vzf(nx1-i-2,j+3,2)
          e24=vzf(nx1-i+4,j-3,2)-vzf(nx1-i-3,j+4,2)
          !计算txx
	        s1=r1*c11(nx1-i,j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
             r1*Q1(nx1-i,j)*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)+(ef1*d21-ef2*d22+ef3*d23-ef4*d24))
	        s2=(1.0-0.5*qx(i))*t1xi2(i,j,1)
	        t1xi2(i,j,2)=(s1+s2)*qxd(i)
	        s1=r2*c13(nx1-i,j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
             r2*Q1(nx1-i,j)*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=0.0 
	        t1zi2(i,j,2)=t1zi2(i,j,1)+s1+s2
	        txx(nx1-i,j,2)=t1xi2(i,j,2)+t1zi2(i,j,2)
          !计算tzz
	        s1=r1*c13(nx1-i,j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
             r1*Q3(nx1-i,j)*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)+(ef1*d21-ef2*d22+ef3*d23-ef4*d24))
          s2=(1.0-0.5*qx(i))*t2xi2(i,j,1)
	        t2xi2(i,j,2)=(s1+s2)*qxd(i)
	        s1=r2*c33(nx1-i,j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
             r2*Q3(nx1-i,j)*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=0.0
	        t2zi2(i,j,2)=t2zi2(i,j,1)+s1+s2
	        tzz(nx1-i,j,2)=t2xi2(i,j,2)+t2zi2(i,j,2)
          !计算txz
	        s1=r1*c44(nx1-i,j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
	        s2=(1.0-0.5*qx(i))*t3xi2(i,j,1)
	        t3xi2(i,j,2)=(s1+s2)*qxd(i)
	        s1=r2*c44(nx1-i,j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)-(ef1*a21-ef2*a22+ef3*a23-ef4*a24))
	        s2=0.0
	        t3zi2(i,j,2)=t3zi2(i,j,1)+s1+s2
	        txz(nx1-i,j,2)=t3xi2(i,j,2)+t3zi2(i,j,2)
          !计算ts
	        s1=r1*Q1(nx1-i,j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
             r1*R(nx1-i,j)*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)+(ef1*d21-ef2*d22+ef3*d23-ef4*d24))
	        s2=(1.0-0.5*qx(i))*tsxi2(i,j,1)
	        tsxi2(i,j,2)=(s1+s2)*qxd(i)
	        s1=r2*Q3(nx1-i,j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
             r2*R(nx1-i,j)*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=0.0
	        tszi2(i,j,2)=tszi2(i,j,1)+s1+s2
	        ts(nx1-i,j,2)=tsxi2(i,j,2)+tszi2(i,j,2)
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
          tsxi2(i,j,1)=tsxi2(i,j,2)
	        tszi2(i,j,1)=tszi2(i,j,2)
        End Do
      End Do
      
      Do j=4,ml                   ! j=ml--->0	  上边界
        Do i=-ml+4,nx+ml-4
	        k=ml
	        if(i.le.0) k=ml+i
	        if(i.gt.nx) k=nx+ml-i+1
	        a11=vx(i+1,-ml+j+1,2)-vx(i,-ml+j,2)
          a12=vx(i+2,-ml+j+2,2)-vx(i-1,-ml+j-1,2)
          a13=vx(i+3,-ml+j+3,2)-vx(i-2,-ml+j-2,2)
          a14=vx(i+4,-ml+j+4,2)-vx(i-3,-ml+j-3,2)
          a21=vx(i+1,-ml+j,2)-vx(i,-ml+j+1,2)
          a22=vx(i+2,-ml+j-1,2)-vx(i-1,-ml+j+2,2)
          a23=vx(i+3,-ml+j-2,2)-vx(i-2,-ml+j+3,2)
          a24=vx(i+4,-ml+j-3,2)-vx(i-3,-ml+j+4,2)
          b11=vz(i+1,-ml+j+1,2)-vz(i,-ml+j,2)
          b12=vz(i+2,-ml+j+2,2)-vz(i-1,-ml+j-1,2)
          b13=vz(i+3,-ml+j+3,2)-vz(i-2,-ml+j-2,2)
          b14=vz(i+4,-ml+j+4,2)-vz(i-3,-ml+j-3,2)
          b21=vz(i+1,-ml+j,2)-vz(i,-ml+j+1,2)
          b22=vz(i+2,-ml+j-1,2)-vz(i-1,-ml+j+2,2)
          b23=vz(i+3,-ml+j-2,2)-vz(i-2,-ml+j+3,2)
          b24=vz(i+4,-ml+j-3,2)-vz(i-3,-ml+j+4,2)
          d11=vxf(i+1,-ml+j+1,2)-vxf(i,-ml+j,2)
          d12=vxf(i+2,-ml+j+2,2)-vxf(i-1,-ml+j-1,2)
          d13=vxf(i+3,-ml+j+3,2)-vxf(i-2,-ml+j-2,2)
          d14=vxf(i+4,-ml+j+4,2)-vxf(i-3,-ml+j-3,2)
          d21=vxf(i+1,-ml+j,2)-vxf(i,-ml+j+1,2)
          d22=vxf(i+2,-ml+j-1,2)-vxf(i-1,-ml+j+2,2)
          d23=vxf(i+3,-ml+j-2,2)-vxf(i-2,-ml+j+3,2)
          d24=vxf(i+4,-ml+j-3,2)-vxf(i-3,-ml+j+4,2)
          e11=vzf(i+1,-ml+j+1,2)-vzf(i,-ml+j,2)
          e12=vzf(i+2,-ml+j+2,2)-vzf(i-1,-ml+j-1,2)
          e13=vzf(i+3,-ml+j+3,2)-vzf(i-2,-ml+j-2,2)
          e14=vzf(i+4,-ml+j+4,2)-vzf(i-3,-ml+j-3,2)
          e21=vzf(i+1,-ml+j,2)-vzf(i,-ml+j+1,2)
          e22=vzf(i+2,-ml+j-1,2)-vzf(i-1,-ml+j+2,2)
          e23=vzf(i+3,-ml+j-2,2)-vzf(i-2,-ml+j+3,2)
          e24=vzf(i+4,-ml+j-3,2)-vzf(i-3,-ml+j+4,2)
          !计算txx
	        s1=r1*c11(i,-ml+j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
             r1*Q1(i,-ml+j)*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)+(ef1*d21-ef2*d22+ef3*d23-ef4*d24))
	        s2=(1.0-0.5*qx(k))*t1xj1(i,j,1)
	        t1xj1(i,j,2)=(s1+s2)*qxd(k)
	        s1=r2*c13(i,-ml+j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
             r2*Q1(i,-ml+j)*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(j))*t1zj1(i,j,1) 
	        t1zj1(i,j,2)=(s1+s2)*qxd(j)
	        txx(i,-ml+j,2)=t1xj1(i,j,2)+t1zj1(i,j,2)
          !计算tzz
	        s1=r1*c13(i,-ml+j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
             r1*Q3(i,-ml+j)*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)+(ef1*d21-ef2*d22+ef3*d23-ef4*d24))
          s2=(1.0-0.5*qx(k))*t2xj1(i,j,1)
	        t2xj1(i,j,2)=(s1+s2)*qxd(k)
	        s1=r2*c33(i,-ml+j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
             r2*Q3(i,-ml+j)*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(j))*t2zj1(i,j,1)
	        t2zj1(i,j,2)=(s1+s2)*qxd(j)
	        tzz(i,-ml+j,2)=t2xj1(i,j,2)+t2zj1(i,j,2)
          !计算txz
	        s1=r1*c44(i,-ml+j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
	        s2=(1.0-0.5*qx(k))*t3xj1(i,j,1)
	        t3xj1(i,j,2)=(s1+s2)*qxd(k)
	        s1=r2*c44(i,-ml+j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)-(ef1*a21-ef2*a22+ef3*a23-ef4*a24))
	        s2=(1.0-0.5*qx(j))*t3zj1(i,j,1)
	        t3zj1(i,j,2)=(s1+s2)*qxd(j)
	        txz(i,-ml+j,2)=t3xj1(i,j,2)+t3zj1(i,j,2)
          !计算ts
	        s1=r1*Q1(i,-ml+j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
             r1*R(i,-ml+j)*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)+(ef1*d21-ef2*d22+ef3*d23-ef4*d24))
	        s2=(1.0-0.5*qx(k))*tsxj1(i,j,1)
	        tsxj1(i,j,2)=(s1+s2)*qxd(k)
	        s1=r2*Q3(i,-ml+j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
             r2*R(i,-ml+j)*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(j))*tszj1(i,j,1)
	        tszj1(i,j,2)=(s1+s2)*qxd(j)
	        ts(i,-ml+j,2)=tsxj1(i,j,2)+tszj1(i,j,2)
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
          tsxj1(i,j,1)=tsxj1(i,j,2)
	        tszj1(i,j,1)=tszj1(i,j,2)
        End Do
      End Do
      
      nz1=nz+ml+1
      Do j=ml,5,-1                ! j=ml--->nz+1 下边界
        Do i=-ml+4,nx+ml-4
	        k=ml
	        if(i.le.0) k=ml+i
	        if(i.gt.nx) k=nx+ml-i+1
	        a11=vx(i+1,nz1-j+1,2)-vx(i,nz1-j,2)
          a12=vx(i+2,nz1-j+2,2)-vx(i-1,nz1-j-1,2)
          a13=vx(i+3,nz1-j+3,2)-vx(i-2,nz1-j-2,2)
          a14=vx(i+4,nz1-j+4,2)-vx(i-3,nz1-j-3,2)
          a21=vx(i+1,nz1-j,2)-vx(i,nz1-j+1,2)
          a22=vx(i+2,nz1-j-1,2)-vx(i-1,nz1-j+2,2)
          a23=vx(i+3,nz1-j-2,2)-vx(i-2,nz1-j+3,2)
          a24=vx(i+4,nz1-j-3,2)-vx(i-3,nz1-j+4,2)
          b11=vz(i+1,nz1-j+1,2)-vz(i,nz1-j,2)
          b12=vz(i+2,nz1-j+2,2)-vz(i-1,nz1-j-1,2)
          b13=vz(i+3,nz1-j+3,2)-vz(i-2,nz1-j-2,2)
          b14=vz(i+4,nz1-j+4,2)-vz(i-3,nz1-j-3,2)
          b21=vz(i+1,nz1-j,2)-vz(i,nz1-j+1,2)
          b22=vz(i+2,nz1-j-1,2)-vz(i-1,nz1-j+2,2)
          b23=vz(i+3,nz1-j-2,2)-vz(i-2,nz1-j+3,2)
          b24=vz(i+4,nz1-j-3,2)-vz(i-3,nz1-j+4,2)
          d11=vxf(i+1,nz1-j+1,2)-vxf(i,nz1-j,2)
          d12=vxf(i+2,nz1-j+2,2)-vxf(i-1,nz1-j-1,2)
          d13=vxf(i+3,nz1-j+3,2)-vxf(i-2,nz1-j-2,2)
          d14=vxf(i+4,nz1-j+4,2)-vxf(i-3,nz1-j-3,2)
          d21=vxf(i+1,nz1-j,2)-vxf(i,nz1-j+1,2)
          d22=vxf(i+2,nz1-j-1,2)-vxf(i-1,nz1-j+2,2)
          d23=vxf(i+3,nz1-j-2,2)-vxf(i-2,nz1-j+3,2)
          d24=vxf(i+4,nz1-j-3,2)-vxf(i-3,nz1-j+4,2)
          e11=vzf(i+1,nz1-j+1,2)-vzf(i,nz1-j,2)
          e12=vzf(i+2,nz1-j+2,2)-vzf(i-1,nz1-j-1,2)
          e13=vzf(i+3,nz1-j+3,2)-vzf(i-2,nz1-j-2,2)
          e14=vzf(i+4,nz1-j+4,2)-vzf(i-3,nz1-j-3,2)
          e21=vzf(i+1,nz1-j,2)-vzf(i,nz1-j+1,2)
          e22=vzf(i+2,nz1-j-1,2)-vzf(i-1,nz1-j+2,2)
          e23=vzf(i+3,nz1-j-2,2)-vzf(i-2,nz1-j+3,2)
          e24=vzf(i+4,nz1-j-3,2)-vzf(i-3,nz1-j+4,2)
          !计算txx
	        s1=r1*c11(i,nz1-j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
             r1*Q1(i,nz1-j)*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)+(ef1*d21-ef2*d22+ef3*d23-ef4*d24))
	        s2=(1.0-0.5*qx(k))*t1xj2(i,j,1)
	        t1xj2(i,j,2)=(s1+s2)*qxd(k)
	        s1=r2*c13(i,nz1-j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
             r2*Q1(i,nz1-j)*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(j))*t1zj2(i,j,1) 
	        t1zj2(i,j,2)=(s1+s2)*qxd(j)
	        txx(i,nz1-j,2)=t1xj2(i,j,2)+t1zj2(i,j,2)
          !计算tzz
	        s1=r1*c13(i,nz1-j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
             r1*Q3(i,nz1-j)*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)+(ef1*d21-ef2*d22+ef3*d23-ef4*d24))
          s2=(1.0-0.5*qx(k))*t2xj2(i,j,1)
	        t2xj2(i,j,2)=(s1+s2)*qxd(k)
	        s1=r2*c33(i,nz1-j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
             r2*Q3(i,nz1-j)*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(j))*t2zj2(i,j,1)
	        t2zj2(i,j,2)=(s1+s2)*qxd(j)
	        tzz(i,nz1-j,2)=t2xj2(i,j,2)+t2zj2(i,j,2)
          !计算txz
	        s1=r1*c44(i,nz1-j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)+(ef1*b21-ef2*b22+ef3*b23-ef4*b24))
	        s2=(1.0-0.5*qx(k))*t3xj2(i,j,1)
	        t3xj2(i,j,2)=(s1+s2)*qxd(k)
	        s1=r2*c44(i,nz1-j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)-(ef1*a21-ef2*a22+ef3*a23-ef4*a24))
	        s2=(1.0-0.5*qx(j))*t3zj2(i,j,1)
	        t3zj2(i,j,2)=(s1+s2)*qxd(j)
	        txz(i,nz1-j,2)=t3xj2(i,j,2)+t3zj2(i,j,2)
          !计算ts
	        s1=r1*Q1(i,nz1-j)*((ef1*a11-ef2*a12+ef3*a13-ef4*a14)+(ef1*a21-ef2*a22+ef3*a23-ef4*a24))+&
             r1*R(i,nz1-j)*((ef1*d11-ef2*d12+ef3*d13-ef4*d14)+(ef1*d21-ef2*d22+ef3*d23-ef4*d24))
	        s2=(1.0-0.5*qx(k))*tsxj2(i,j,1)
	        tsxj2(i,j,2)=(s1+s2)*qxd(k)
	        s1=r2*Q3(i,nz1-j)*((ef1*b11-ef2*b12+ef3*b13-ef4*b14)-(ef1*b21-ef2*b22+ef3*b23-ef4*b24))+&
             r2*R(i,nz1-j)*((ef1*e11-ef2*e12+ef3*e13-ef4*e14)-(ef1*e21-ef2*e22+ef3*e23-ef4*e24))
	        s2=(1.0-0.5*qx(j))*tszj2(i,j,1)
	        tszj2(i,j,2)=(s1+s2)*qxd(j)
	        ts(i,nz1-j,2)=tsxj2(i,j,2)+tszj2(i,j,2)
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
          tsxj2(i,j,1)=tsxj2(i,j,2)
	        tszj2(i,j,1)=tszj2(i,j,2)
        End Do
      End Do

      Do j=-ml,nz+ml   
        Do i=-ml,nx+ml 
          txx(i,j,1)=txx(i,j,2)
          tzz(i,j,1)=tzz(i,j,2)
          txz(i,j,1)=txz(i,j,2)
          ts(i,j,1)=ts(i,j,2)
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
  
  Do i=1,n_file
    Close(i+500)
    Close(i+600)
  End Do
  
  Do i=1,n_file2
    Close(i+700)
    Close(i+800)
  End Do
  
  irecl2=nt*4+240
  head(58)=nt
  head(59)=delt_t*1000000
  
  Open(16,file=Outpath1//'synthetic_seismic_record_x.sgy',form='binary',access='direct',status='replace',recl=irecl2)
  Open(17,file=Outpath1//'synthetic_seismic_record_z.sgy',form='binary',access='direct',status='replace',recl=irecl2)
  Do i=ig1,ig2
    ii=i-ig1+1
    Write(16,rec=ii) head,(vxt(i,j),j=1,nt)
    Write(17,rec=ii) head,(vzt(i,j),j=1,nt)
  End Do
  Close(16)
  Close(17)
  
  Open(18,file=Outpath2//'synthetic_seismic_record_x.sgy',form='binary',access='direct',status='replace',recl=irecl2)
  Open(19,file=Outpath2//'synthetic_seismic_record_z.sgy',form='binary',access='direct',status='replace',recl=irecl2)
  Do i=ig1,ig2
    ii=i-ig1+1
    Write(18,rec=ii) head,(vxft(i,j),j=1,nt)
    Write(19,rec=ii) head,(vzft(i,j),j=1,nt)
  End Do
  Close(18)
  Close(19)
  
  Deallocate(snapshot_x,snapshot_z)
  Deallocate(seismogram_x,seismogram_z)
  
  vx_file='seismogram_x.txt'
  vz_file='seismogram_z.txt'
  Call grapher_seismograms(n_file2,nt,vx_file,vz_file,Outpath1,Outpath3)
  
  vx_file='seismogram_x.txt'
  vz_file='seismogram_z.txt'
  Call grapher_seismograms(n_file2,nt,vx_file,vz_file,Outpath2,Outpath4)
  
    
End Subroutine Wave_Field_Modeling