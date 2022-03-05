      program textin
c
c      What's a null input?
c
      character (1) alpha
c
  1   continue
      read(*,*)alpha
      if(alpha.eq.'x'
     *  )then
             stop
         else
             write(*,*)ichar(alpha),alpha
             go to 1
      endif
c
      end
