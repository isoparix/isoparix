      program svn_calib
c
      character(15) text
c
      real(8)phired,phigreen,phiblue
     *        ,ared,  agreen,  ablue
     *        ,pred,  pgreen,  pblue
c
c      Set the details of the colour map
c
      phired  =0.0
      phigreen=0.0
      phiblue =0.5
c
      ared  =0.0
      agreen=0.4
      ablue =0.0
c
      pred=  1.0
      pgreen=4.0
      pblue= 1.0
c
      pi_math=355./113.
      phired=  phired  *pi_math
      phigreen=phigreen*pi_math
      phiblue= phiblue *pi_math
c
      ixm=800
      iym=600
      write(*,100)
c
c      Open up the display window
c
      call x11winope
     * (%val(-ixm)
     * ,%val(iym)
     *             ,%ref(maxcols),%ref(isocols),%ref(irc)
     *             ,%val(phired), %val(phigreen), %val(phiblue)
     *             ,%val(  ared), %val(  agreen), %val(  ablue)
     *             ,%val(  pred), %val(  pgreen), %val(  pblue)
     *               )
c
      if(irc.ne.0
     *  )then
             write(*,101)irc
             stop
      endif       
c      
      call x11title('   SVN calibration window   ')
      call system('sleep 10')
      stop
c
100   format('SVN_CALIB: About to open window')
101   format('SVN_CALIB: Bad return code',i6)
c
      end
