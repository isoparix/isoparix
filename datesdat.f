      program gpfsdat
c
      implicit real (8) (a-h,o-z)
c
c      Generates geometric series of block sizes
c
      open(8,file='datestinp.data',status='unknown',form='formatted')
      write(8,100)
c
      sqrt2=dsqrt(2.0d+00)
      nblock=512
      a=nblock
c
      do i=1,31
         write(8,*)nblock
         a=a*sqrt2
         nblock=.5+a
      enddo
c
      write(8,101)
      stop
c
100   format('F                              ;Unmount FS (T/F)'
     *     ,/'.                                                 '
     *     , ';50-char file system name'
     *     ,/'10000000                       ;Size of test file'
     *      ) 
101   format('-1                             ;End of data',/)
c
      end
