Program directory_operate
  Implicit None
  Integer(kind=4) :: getcwd,changedirQQ,status,istat     !此处对getcwd,changedirQQ进行了声明，
  Character(len=64) :: dirname                           !也可直接使用Use IFPORT而不用声明getcwd,changedirQQ
  
  status = getcwd( dirname )                             !获取当前工作目录
  If ( status .ne. 0 ) stop 'getcwd: error'
  
  istat=changedirQQ('dir_test')                          !改变当前工作目录到'dir_test'
                                                         !注意：此处若找不到'dir_test'文件夹，则会将'print.txt'文件
                                                         !输出到当前工作目录
  
  Open(11,file='print.txt',status='replace')             !将获取的当前工作目录字符串输出到&
  Write(11,*) dirname                                    !改变当前工作目录后的目录'dir_test'中的'print.txt'文件
  Close(11)
  
End Program directory_operate