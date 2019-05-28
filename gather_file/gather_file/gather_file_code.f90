Program Main
  Integer iT
  External WriteFileName
  Call DoWithWildcard("*.txt",WriteFileName,iT)
End Program Main

Subroutine WriteFileName( cFileName , iFile )
  Character*(*),Intent(In)::cFileName
  Integer , Intent(In)::iFile
  Write(*,*) '第',iFile,'个文件是：',cFileName
End Subroutine WriteFileName

Subroutine DoWithWildcard(cWildcard,CallBack,iTotal)
  Use DFLib,only:GetFileInfoQQ,GetLastErrorQQ,FILE$INFO,FILE$LAST,FILE$ERROR,FILE$FIRST,ERR$NOMEM,ERR$NOENT,FILE$DIR
  Implicit None
  
  Interface 
    Subroutine CallBack( FileName , loop )
      Character(*),Intent(In) :: FileName
      Integer,Intent(In) :: loop
    End Subroutine CallBack
  End Interface
  
  Character*(*),Intent(In)::cWildcard
  Integer,Intent(Out)::iTotal
  TYPE (FILE$INFO) info
  INTEGER(4)::Wildhandle,length,retInt
  Wildhandle = FILE$FIRST
  iTotal = 0
  DO WHILE (.TRUE.)
      length = GetFileInfoQQ(cWildCard,info,Wildhandle)
      IF ((Wildhandle .EQ. FILE$LAST) .OR.(Wildhandle .EQ. FILE$ERROR)) THEN
        SELECT CASE (GetLastErrorQQ())
        CASE (ERR$NOMEM)  !//内存不足
          iTotal = - 1
          Return
        CASE (ERR$NOENT)  !//碰到通配符序列尾
          Return
        CASE DEFAULT
          iTotal = 0
          Return
        END SELECT
      END IF
      If ((info%permit.AND.FILE$DIR).Eq.0) then
        Call CallBack( Trim(info.Name) , iTotal + 1 )
        iTotal = iTotal + 1
      End If
  END DO
  
End Subroutine DoWithWildcard