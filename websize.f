      program websize
c
c      Calculates the numbers to use ImamgeMagick 'crop'; maintain the
c      aspect ratio; and give a vertical height of 126 pixels.
c
      character (1)answer
      character (4)pictype
c
c      Get picture dimensions
c
      call picdims(pictype,ixdim,iydim)
      write(*,110)pictype,ixdim,iydim
c
  2   continue
      write(*,102)
      read(*,*)answer
      if(answer.eq.'q')stop
      if(answer.ne.'x'.and.answer.ne.'y')go to 2
c
      if(answer.eq.'x')
     *   then
  3          continue
             write(*,103)
             read(*,*)ixnew
             if(ixnew.le.0)go to 3
             iynew=(float(iydim)*float(ixnew)/float(ixdim))+.5
      endif
c
      if(answer.eq.'y')
     *   then
  4          continue
             write(*,104)
             read(*,*)iynew
             if(iynew.le.0)go to 4
             ixnew=(float(ixdim)*float(iynew)/float(iydim))+.5
      endif
c
      open(4,file="webcrop.bat",form='formatted',status='unknown')
      write(4,107)ixnew,iynew,ixnew,iynew
      stop
c
 98   continue
      write(*,101)
      stop
c
101   format('ERROR in read')
102   format('Resize the width (x)? or the height (y)? or quit (q)')
103   format('New horizontal size of image, in pixels?')
104   format('New vertical size of image, in pixels?')
107   format('convert -resize ',i0,'x',i0
     *      ,' -density 80x80 %1 %~n1_',i4.4,'x',i4.4,'.jpg') ! Density pels/cm
110   format('Current',a5,' image dimensions are ',i0,'x',i0)
115   format('ERROR in stream identity text: ',80a1)
c
      end
