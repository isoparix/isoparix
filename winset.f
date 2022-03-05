      subroutine winset
c
      use allcomms
c
      real(8)phired,phigreen,phiblue
     *        ,ared,  agreen,  ablue
     *        ,pred,  pgreen,  pblue
c
      if(open_window
     *  )then
C      
C         We have a window - let's resize it!
C      
             call x11resize(%val(ixm),%val(iym))
             write(0,103)ixm,iym
         else
c
c      Read in the details of the colour map
c
c0.0 0.0 0.5      ;Distance through sin**2(x) wave cycle (x/pi) at start
c0.0 0.4 0.0      ;Minimum level of this colour (0-1)
c  1   4  1       ;Number of periods of this colour
c  R   G  B
c
             read(7,101,err=97)phired,phigreen,phiblue
             read(7,101,err=97)ared,  agreen,  ablue
             read(7,102,err=97)ired,  igreen,  iblue
             rewind(7)
c
             write(0,101)phired,phigreen,phiblue
             write(0,101)ared,  agreen,  ablue
             write(0,102)ired,  igreen,  iblue
c
             pred=  ired
             pgreen=igreen
             pblue= iblue
c
             pi_math=355./113.
             phired=  phired  *pi_math
             phigreen=phigreen*pi_math
             phiblue= phiblue *pi_math
c
c      Open up the display window
c
             call x11winope
     *        (%val(-ixm),%val(iym),maxcols,isocols,irc
     *        ,%val(phired), %val(phigreen), %val(phiblue)
     *        ,%val(  ared), %val(  agreen), %val(  ablue)
     *        ,%val(  pred), %val(  pgreen), %val(  pblue)
     *        )
c
             open_window=.true.
      endif
c
      ruler=.false.
      call x11clearpixmap()
      call buttons(.false.)
      call x11flush()
      call isoflush(8)
c
      return 
c
97    continue
      write(0,100)phired,phigreen,phiblue,ared,agreen,ablue
     *             ,ired,  igreen,  iblue
      stop
c
100   format('***** ERROR READING COLOUR MAP IN WINSET *****'
     *     ,/2(3e17.10,/),3i17)
101   format(3f4.1)
102   format(3i4)
103   format('WINSET:  Window resized to',i6,' by',i6)      
      end
