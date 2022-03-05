      subroutine remblanks(outarray,nchars)
c
c      Remove blanks from outarray, and writes it to STDOUT
c
      character *1 outarray(nchars)
c
      m=0 
      do n=1,nchars
c
c      Remove forward slashes....
c
          if(outarray(n).eq.'/'
     *      )then
c               outarray(n)='\'	!Removed \ for AIX
                outarray(n)='|'
         endif
c
          if(outarray(n).eq.' '
     *      )then
                nblanks=nblanks+1
            else
                nblanks=0 
         endif
c
         if(nblanks.lt.2
     *     )then
                m=m+1
                outarray(m)=outarray(n)
         endif
c
      enddo
      write(6,100)outarray(:m)
      call isoflush(6)
c
      return
c
100   format(50000a)
c
      end
