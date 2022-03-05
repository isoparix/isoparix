      program jpegtime
c
c      Reads and displays JPG shooting time
c
      character(1) picdata(4096),pipedata(512)
      character(4096) picdatb
      character(512)currdir,filename,pathname
      character(6) lead_junk
      character(14) lead_junkb
      character(19) linkname
      character(12) filesize
      character(4) exifida,exifidb
c      
      logical found_colon
c      
      line=0
      open(4,file='err.log',status='unknown')
      open(8,file='detail.log',status='unknown')
c
   1  continue
      read(5,114,end=2,err=2)pipedata !End or Err = end of data/pipe
c
      if(pipedata(2).eq.'D'
     *  )then
             write(currdir,114)(pipedata(mx),mx=15,512)
             write(4,117)trim(currdir)
      endif
c
      if(pipedata(15).eq.':'
     *  )then
             found_colon=.false.
             write(filesize,114)(pipedata(mx),mx=24,35)
             write(filename,114)(pipedata(mx),mx=37,512)
             pathname=trim(currdir)//'\'//trim(filename)
c
             open(3,file=trim(pathname),err=200,status='old')
             read(3,112,end=4)lead_junk,exifida,lead_junkb,exifidb
             if(exifida.eq.'Exif'.or.
     *          exifidb.eq.'Exif'
     *         )then
c
c      Now hunt the colon....
c
                    rewind(3)
                    line=0
c
                    nfirst=1
  3                 continue
                    read(3,114,end=4)picdata(nfirst:)
                    line=line+1
                    write(picdatb,114)picdata
                    nbytes=len(trim(picdatb))
c                   if(nbytes.gt.3
c    *                )then
c                          nend=min0(4096,nfirst+3)
c                          write(*,107)nbytes,line
c    *                          ,(ichar(picdata(mx)),mx=nfirst,nend)
c    *                          ,(ichar(picdata(mx)),mx=nbytes-3,nbytes)
c                      else
c                          write(*,107)nbytes,line
c    *                          ,(ichar(picdata(mx)),mx=nfirst,nfirst+3)
c                   endif
c
                    if(line.eq.1
     *                 )then
                            do n=1,10
                               picdata(n)=' '
                            enddo
                    endif
c
                    do i=nbytes,nfirst,-1
                       if(picdata(i   ).eq.':'.and.
     *                    picdata(i- 3).eq.':'.and.
     *                    picdata(i- 6).eq.' '.and.
     *                    picdata(i- 9).eq.':'.and.
     *                    picdata(i-12).eq.':'
     *                   )then
c                             write(*,*)nfirst,nbytes,i
                              found_colon=.true.
                              ntxta=i-16
                              if(ntxta.lt.1
     *                          )then
                                     write(4,105)i,line,trim(pathname)
                                     exit
                              endif
                              ntxtb=i+2
                              do k=ntxta+4,ntxtb-2
                                 if(picdata(k).eq.':')picdata(k)='-'
                                 if(picdata(k).eq.' ')picdata(k)='_'
                              enddo
                              write(linkname,114)picdata(ntxta:ntxtb)
                              write(8,108)linkname,filesize
     *                                   ,trim(pathname)
                              write(*,103)linkname
     *                                   ,trim(pathname)
                              exit
                       endif      
                    enddo
                    nfirst=nbytes+1
c      
                    if(found_colon
     *                )then
                           close(3)
                       else
c                          if(line.lt.3
                           if(nbytes.lt.4096
     *                       )then
                                  go to 3
                           endif
                    endif
                else
                    write(4,118)trim(pathname)  ! Not Exif
                    go to 1
             endif
         else
             go to 1 ! Not a file line in the dir listing
      endif
c      
   4  continue            ! Arrive here when all the jpg data is exhausted
      if(.not.found_colon
     *  )then
             write(4,106)trim(pathname),line
      endif
      go to 1             ! See if there's another file to read
c
   2  continue
      stop
c
 200  continue
      write(4,104)trim(pathname)
      go to 1
c
101   format(/'Line:',i4)      
102   format(a)
103   format('xxmklink /q \TEMP_LINKS\',a19,' "',a,'"')
104   format(17x,'     JPEGTIME: Opening file ',a)
105   format('JPEGTIME: Bad colon position at',i8,' in line',i3,' ',a)
106   format(17x,'JPEGTIME: No colon found in ',a,' after',i3,' reads')
107   format(i8,' bytes read by line',i3
     *      ,': First four -',4z3,' Last four - ',4z3)
108   format(2a20,' "',a,'"')
112   format(a6,a4,a14,a4)
114   format(4096a1)      
115   format(32(16z3,/))      
116   format(2z3,2a3)
117   format('Scanning/indexing ',a)
118   format(44x,'"',a,'" is not an Exif file')
c
      end      

