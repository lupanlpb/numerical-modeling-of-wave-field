Program Read_Write_sgy
  Implicit None
  Character(len=80) :: cmdfile
  Character(len=80) :: MULIN1_DAT,MULIN2_DAT
  Character(len=80) :: output_sgy_file
  Integer(Kind=2) :: sample_point1,sample_point2
  Integer(Kind=2) :: sample_rate1,sample_rate2
  Integer(Kind=4) :: file_size1,file_size2
  Integer(Kind=2) :: n_trace1,n_trace2
  Real,Allocatable :: seismic_data1(:,:),seismic_data2(:,:)
  cmdfile='parameter.cmd'
  Call Read_cmdfile(cmdfile,MULIN1_DAT,MULIN2_DAT,output_sgy_file)
  Call Read_sgy_parameter(MULIN1_DAT,sample_point1,sample_rate1,file_size1,n_trace1)
  Call Read_sgy_parameter(MULIN2_DAT,sample_point2,sample_rate2,file_size2,n_trace2)
  Allocate(seismic_data1(1:n_trace1,1:sample_point1),seismic_data2(1:n_trace2,1:sample_point2))
  Call Read_sgy_data(MULIN1_DAT,sample_point1,n_trace1,seismic_data1)
  Call Read_sgy_data(MULIN2_DAT,sample_point2,n_trace2,seismic_data2)
  Open(17,file='readme.txt')
  Write(17,*)'采样点数',sample_point1
  Write(17,*)'采样率',sample_rate1
  Write(17,*)'总道数',n_trace1+n_trace2
  Close(17)
  Call Write_sgy_file(output_sgy_file,sample_point1,sample_rate1,n_trace1,seismic_data1,&
                          sample_point2,sample_rate2,n_trace2,seismic_data2)
  
End Program Read_Write_sgy
    
Subroutine Read_cmdfile(cmdfile,MULIN1_DAT,MULIN2_DAT,output_sgy_file)
  Implicit None
  Character *(*) :: cmdfile
  Character *(*) :: MULIN1_DAT,MULIN2_DAT
  Character *(*) :: output_sgy_file
  Open(11,file=cmdfile,status='old')
    Read(11,*) MULIN1_DAT
    Read(11,*) MULIN2_DAT
    Read(11,*) output_sgy_file
  Close(11)
End Subroutine Read_cmdfile

Subroutine Read_sgy_parameter(MULIN_DAT,sample_point,sample_rate,file_size,n_trace)
  Implicit None
  Character *(*) :: MULIN_DAT
  Integer(Kind=2) :: sample_point
  Integer(Kind=2) :: sample_rate
  Integer(Kind=4) :: file_size
  Integer(Kind=2) :: n_trace
  Integer :: K
  Open(12,file=MULIN_DAT,access='direct',form='binary',status='old',recl=2)!读取采样点数与采样率
    Read(12,rec=58) sample_point
    Read(12,rec=59) sample_rate
  Close(12)
  Open(13,file=MULIN_DAT,access='direct',form='binary',status='old',recl=1)!读取文件大小
    file_size=0
    K=0
    Do While(.TRUE.)
      file_size=file_size+1
	  Read(13,rec=file_size,iostat=K)
      If(K/=0) exit
	End Do
	file_size=file_size-1
  Close(13)
  Open(14,file=MULIN_DAT,access='direct',form='binary',status='old',recl=2)!读取文件总道数
    Do K=1,9999
      If(file_size==K*(240+4*sample_point)) Then
        n_trace=K
      End If
	End Do
  Close(14)
End Subroutine Read_sgy_parameter


Subroutine Read_sgy_data(MULIN_DAT,sample_point,n_trace,seismic_data)!读取地震数据
  Implicit None
  Character *(*) :: MULIN_DAT
  Integer(Kind=2) :: sample_point
  Integer(Kind=2) :: n_trace
  Real :: seismic_data(1:n_trace,1:sample_point)
  Integer :: i,j
  Open(15,file=MULIN_DAT,access='direct',form='binary',status='old',recl=4)
    Do i=1,n_trace
      Read(15,rec=i*60+(i-1)*sample_point) (seismic_data(i,j),j=1,sample_point)
	End Do
  Close(15)
End Subroutine Read_sgy_data
    
Subroutine Write_sgy_file(output_sgy_file,sample_point1,sample_rate1,n_trace1,seismic_data1,&
                          sample_point2,sample_rate2,n_trace2,seismic_data2)
  Implicit None
  Character *(*) :: output_sgy_file
  Integer(Kind=2) :: sample_point1,sample_point2
  Integer(Kind=2) :: sample_rate1,sample_rate2
  Integer(Kind=4) :: file_size1,file_size2
  Integer(Kind=2) :: n_trace1,n_trace2
  Real :: seismic_data1(1:n_trace1,1:sample_point1),seismic_data2(1:n_trace2,1:sample_point2)
  Integer(Kind=2) :: head_infor(1:120)=0
  Integer(Kind=2) :: lsyn
  Integer :: i,j
  head_infor(58)=sample_point1
  head_infor(59)=sample_rate1
  lsyn=240+4*sample_point1
  Open(16,file=output_sgy_file,access='direct',recl=lsyn,form='binary',status='unknown')  
  Do i=1,n_trace1+n_trace2
    If(i<=n_trace1) Then
      Write(16,rec=i) head_infor,(seismic_data1(i,j),j=1,sample_point1)
    Else
      Write(16,rec=i) head_infor,(seismic_data2(i-n_trace1,j),j=1,sample_point2)
    End If
  End Do
  Close(16)
End Subroutine Write_sgy_file