!--------------------------------------------------------------
!功能：
!  1.本程序用于读写sgy文件
!	 2.判断待读取文件是否有卷头，若读入文件无卷头，
!    则加入卷头，转换成标准格式
!	 3.有卷头数据去卷头,并转换成标准格式
!--------------------------------------------------------------
Program sgy_operation
  Implicit None
  Character(len=80) :: cmdfile
  Character(len=80) :: Input_sgy_file,Output_sgy_file
  cmdfile='cmdfile.par'
  Open(11,file=cmdfile,status='old')
  Read(11,*) Input_sgy_file
  Read(11,*) Output_sgy_file
  Close(11)
  CAll operation_sgy(Input_sgy_file,Output_sgy_file)
End Program


Subroutine operation_sgy(Input_sgy_file,Output_sgy_file)
  Implicit None
  Character *(*) :: Input_sgy_file,Output_sgy_file
  Real,Parameter :: e = 0.000001
  Integer(Kind=2) :: i,j,k
  Integer(Kind=2) :: flag,flag1,flag2
  Integer(Kind=2) :: trace_amount
  Integer(Kind=2) :: sample_rate1
  Integer(Kind=2) :: trace_length1
  Integer(Kind=2) :: sample_rate2
  Integer(Kind=2) :: trace_length2
  Integer(Kind=2) :: sample_rate
  Integer(Kind=2) :: trace_length
  Integer(Kind=2) :: trace_code
  Integer(Kind=2) :: shot_amount
  Integer(Kind=2) :: shot_no
  Integer(Kind=2) :: trace_amount_pershot
  Integer(Kind=2) :: trace_head(120)
  Integer(Kind=4) :: filesize
  Integer(Kind=4) :: sequence_no
  Character *80 reelhead
  Real(Kind=4),Allocatable :: seismic_data(:,:)
  Open(12,file=Input_sgy_file,access='direct',form='binary',recl=2)
  Read(12,rec=1609) sample_rate1
  Read(12,rec=1611) trace_length1
  Close(12)
  If (sample_rate1<=0.or.mod(sample_rate1,1)/=0) Then
	flag1 = 0
  Else
	flag1 = 1
  End If
  Open(13,file=Input_sgy_file,access='direct',form='binary',recl=240)
  Read(13,rec=16) trace_head
  trace_length2 = trace_head(58)
  sample_rate2 = trace_head(59)
  Close (13)
  If(sample_rate2<=0.or.mod(sample_rate2,1)/=0) Then
    flag2 = 0
  Else
    flag2 = 1
  End If
  If(flag1==1.and.flag2==1.and.abs(sample_rate1-sample_rate2)<e.and.abs(TRACE_LENGTH1-TRACE_LENGTH2)<e) Then
    flag=1 !SGY数据含有卷头
    Write(*,*) 'SGY数据含有卷头'
  Else
    flag=0 !SGY数据不含卷头
    Write(*,*) 'SGY数据不含有卷头'
  End If
!-----------------------------------------------------------------------------------------------------
  Write(*,*) '获得SGY数据采样率、采样长度'
  Open(14,file=Input_sgy_file,access='direct',form='binary',recl=2)
  If(flag==1) Then
    Read(14,rec=1859) sample_rate
    Read(14,rec=1858) trace_length
    Write(*,*) '采样率=', sample_rate, '采样长度=', trace_length
  End If
  If(flag==0) Then
    Read(14,rec=58) trace_length
    Read(14,rec=59) sample_rate
    Write(*,*) '采样率=', sample_rate, '采样长度=', trace_length
  End If
  Close(14)
!------------------------------------------------------------------------------------------------------
  Write(*,*) '读取地震数据总道数'
  Open(15,file=Input_sgy_file,access='direct',form='binary',recl=1)
  filesize=0
  k=0
  Do While(.True.)
    filesize=filesize+1
    Read(15,rec=filesize,iostat=k)
    If(k/=0) Exit
  End Do
  filesize=filesize-1
  Close(15)
  Write(*,*) 'SGY文件大小:', filesize
  If(flag==1) Then
    Do i=1,9999
      If(filesize==3600+i*(240+4*trace_length)) Then
        trace_amount=i
        Write(*,*) '数据含有卷头:总道数=',trace_amount
      End If
    End Do
  End If
  If(flag==0) Then
    Do j=1,9999
      If(filesize==j*(240+4*trace_length)) Then
        trace_amount=j
        Write(*,*) '数据不含有卷头:总道数=',trace_amount
      End If
    End Do
  End If
  
!  shot_amount=trace_amount/trace_amount_pershot
  Allocate(seismic_data(trace_amount,trace_length))
!-----------------------------------------------------------------------------
  If(flag==1) Then
    Write(*,*) '读取有卷头SGY地震数据'
    Open(16,file=Input_sgy_file,access='direct',form='binary',recl=4)
    Do i=1,trace_amount
      Read(16,rec=900+i*60+(i-1)*trace_length)(seismic_data(i,j),j=1,trace_length)
    End Do
    Close(16)
  End If
  If(flag==0) Then
    Write(*,*) '读取无卷头SGY地震数据'
    Open(17,file=Input_sgy_file,Form='BINARY',Access='DIRECT',Recl=4)
    Do i=1, trace_amount
      Read(17,rec=i*60+(i-1)*trace_length)(seismic_data(i,j),j=1,trace_length)
    End Do
    Close(17)
  End If
  
  sequence_no=0
  trace_code=1
!------------------------------------------------------------------------------------------
!  open(45,file=outputfilename,form='BINARY',access='DIRECT',recl=80)
!  do i=1, 40
!	 write(45,rec=i) reelhead
!  end do
!  close(45)
!------------------------------------------------------------------------------------------
  If(flag==0) Then
    Write(*,*) '给无卷头SGY数据加卷头'
    Open(18,file=Output_sgy_file,access='direct',form='binary',status='replace',recl=4)
    Write(18,rec=804) trace_amount,0_2
    Write(18,rec=805) sample_rate,0_2
    Write(18,rec=806) trace_length,0_2
    Write(18,rec=807) 1_2,0_2
    Write(18,rec=814) 0_2,1_2
    Do i=1,trace_amount
!shot_no=1+int((i-1)/trace_amount_pershot)
	  sequence_no=sequence_no+1
	  Write(18,rec=900+(i-1)*(60+trace_length)+1) sequence_no
!write(18,rec=900+(i-1)*(60+trace_length)+2) sequence_no
!write(18,rec=900+(i-1)*(60+trace_length)+3) shot_no
	  Write(18,rec=900+(i-1)*(60+trace_length)+8) trace_code,0_2
	  Write(18,rec=900+(i-1)*(60+trace_length)+29) 0_2,trace_length
	  Write(18,rec=900+(i-1)*(60+trace_length)+30) sample_rate,0_2
	  Do j=1,trace_length
		Write(18,rec=900+(i-1)*(60+trace_length)+60+j) seismic_data(i,j)
	  End Do
	End Do
    Close(18)
  End If
  If(flag==1) Then
	Write(*,*) '给有卷头SGY数据去卷头'
	Open(19,file=Output_sgy_file,form='binary',access='direct',status='replace',recl=4)
	Do i=1,trace_amount
!shot_no=1+int((i-1)/trace_amount_pershot)
	  sequence_no=sequence_no+1
	  Write(19,rec=(i-1)*(60+trace_length)+1) sequence_no
!write(19,rec=(i-1)*(60+trace_length)+2) sequence_no
!write(19,rec=(i-1)*(60+trace_length)+3) shot_no
      Write(19,rec=(i-1)*(60+trace_length)+8) trace_code,0_2
      Write(19,rec=(i-1)*(60+trace_length)+29) 0_2,trace_length
      Write(19,rec=(i-1)*(60+trace_length)+30) sample_rate,0_2
      Do j=1,trace_length
        Write(19,rec=(i-1)*(60+trace_length)+60+j) seismic_data(i,j)
      End Do
    End Do
    Close(19)
  End If
End Subroutine operation_sgy