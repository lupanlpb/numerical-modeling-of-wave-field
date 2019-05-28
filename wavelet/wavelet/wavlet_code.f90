Program Wavelet
  Implicit None
  Real :: fre_wavelet=60,delay_source_t0=0.006
  Real :: time
  Real :: fc1,fc2,fc3,fc4
  Real :: pi
  Real :: g1,g2,g3,g4
  Integer :: i
  
  Open(11,file='wavelet111_2.dat',status='replace')
  Open(12,file='wavelet222_4.dat',status='replace')
  Open(13,file='wavelet333_8.dat',status='replace')
  Open(14,file='wavelet_no_delay.dat',status='replace')
  Do i=1,4000
    time=(i-1)*0.00005
    pi=4.0*atan(1.0)
    g1=pi*fre_wavelet*(time-delay_source_t0)
    g2=pi*fre_wavelet*time
    g3=g1**2
    g4=g2**2
    fc1=(1.0-2*g1**2)*exp(-g3)
    fc2=(1.0-4*g1**2)*exp(-2*g3)
    fc3=(1.0-8*g1**2)*exp(-4*g3)
    fc4=(1.0-2*g2**2)*exp(-g4)
    Write(11,*) time,fc1
    Write(12,*) time,fc2
    Write(13,*) time,fc3
    Write(14,*) time,fc4
  End Do
  Close(11)
  Close(12)
  Close(13)
  Close(14)
  
End Program