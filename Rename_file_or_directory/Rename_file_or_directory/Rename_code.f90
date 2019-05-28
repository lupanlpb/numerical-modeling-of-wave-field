Program Main
  Integer :: i, n
  Integer :: rename, status
  Character(len=18) file1,file2
  
  file1='hello'         !如果file2指定的文件已存在，则file2和file1必须属于相同的文件类型，
  file2='hello world'   !并且必须位于相同的文件系统中。如果file2已存在，应先将其删除。
    
  status = rename(file1,file2)
  if ( status .ne. 0 ) stop 'rename: error'

End Program Main