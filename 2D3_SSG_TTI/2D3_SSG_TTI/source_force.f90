Subroutine source_force(fre_wavelet,delay_source_t0,time,fc0)
  Implicit None
  Real :: fre_wavelet,delay_source_t0
  Real :: time
  Real :: fc0
  Real :: pi
  Real :: g1,g2,g3
  
  pi=4.0*atan(1.0)	
  g1=2*pi*fre_wavelet*(time-delay_source_t0)
  g2=2*g1
  g3=g1*g1
  fc0=0.0
  If(g3.lt.10.0) Then                !问题：g3为什么要大于10？
    fc0=(1.0-0.5*g2*g2)*exp(-g3)   
  End If
End Subroutine source_force