      program zerodirs
c
c      Spots and lists directories with no files
c
c Directory of C:\data\Zipfiles\RHEL
c
c30/08/2007  18:37    <DIR>          .
c30/08/2007  18:37    <DIR>          ..
c               0 File(s)              0 bytes
c
      character(1)  text(215)
      character(9)  dirtext
      character(24) zerotxt
      character(200)out_text
c      
      equivalence (dirtext,text(2:10))
      equivalence (text(15:215),out_text)
c      
      open(8,file='clear_zero_dirs.bat',status='unknown')
  1   continue
      out_text=' '
      read(*,100,end=2,err=2)text
      if(dirtext.eq.'Directory'
     *  )then
             read(*,101)zerotxt
c            write(*,*)zerotxt
             if(zerotxt.eq.'0 File(s)              0'
     *         )then
                    write(*,102)trim(out_text)
                    write(8,102)trim(out_text)
             endif        
      endif       
      go to 1
c
  2   continue
      stop
100   format(215a1)
101   format(///,15x,a24)
102   format('rmdir "',a,'"')
c
      end      
