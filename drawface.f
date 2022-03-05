      subroutine drawface(idcube,nf,ncol,ifull)
c
c      Draws cube face NF of cube IDCUBE
c
      use isocomm
c
c      This next check is Bad News on the performance front.
c      Avoid it!!
c
c     do n=1,4
c        if(ixc(nvf(nf,n),idcube).le.0.or.
c    *      iyc(nvf(nf,n),idcube).le.0
c    *     )then
c               write(0,104)idcube,n,nf
c               call cubecheck(idcube)
c        endif
c     enddo
c
c     write(*,1001)idcube,nf,lsc(nf,idcube)
c     write(*,100)ifull,number(1,ifull),number(2,ifull),idcube
c    *                                          ,lsc(nf,idcube)
      if(number(2,ifull).eq.1
     *  )then
c
c      Draw the face coloured in...
c
             call x11quad(
     *                    %val(ixc(nvf(nf,1),idcube))
     *                   ,%val(iyc(nvf(nf,1),idcube))
     *                   ,%val(ixc(nvf(nf,2),idcube))
     *                   ,%val(iyc(nvf(nf,2),idcube))
     *                   ,%val(ixc(nvf(nf,4),idcube))
     *                   ,%val(iyc(nvf(nf,4),idcube))
     *                   ,%val(ixc(nvf(nf,3),idcube))
     *                   ,%val(iyc(nvf(nf,3),idcube))
     *                   ,%val(ncol)
     *                    )
c            write(32,107)nf
c    *                   ,ixc(nvf(nf,1),idcube),ixc(nvf(nf,2),idcube)
c    *                   ,ixc(nvf(nf,4),idcube),ixc(nvf(nf,3),idcube)
c    *                   ,iyc(nvf(nf,1),idcube),iyc(nvf(nf,2),idcube)
c    *                   ,iyc(nvf(nf,4),idcube),iyc(nvf(nf,3),idcube)
         else
c            write(32,106)idcube,nf,ncol,ifull
      endif
c
c      Draw its surrounding lines - the frame
c
      if(number(1,ifull).eq.1
     *  )then
             do ls=1,lsc(nf,idcube)
c
 
                nxa=ixc(lsa(ls,nf,idcube),idcube)
                nya=iyc(lsa(ls,nf,idcube),idcube)
                nxb=ixc(lsb(ls,nf,idcube),idcube)
                nyb=iyc(lsb(ls,nf,idcube),idcube)
c
                if(nxa.gt.ixcmax)ixcmax=nxa
                if(nxb.gt.ixcmax)ixcmax=nxb
                if(nya.gt.iycmax)iycmax=nya
                if(nyb.gt.iycmax)iycmax=nyb
                if(nxa.lt.ixcmin)ixcmin=nxa
                if(nxb.lt.ixcmin)ixcmin=nxb
                if(nya.lt.iycmin)iycmin=nya
                if(nyb.lt.iycmin)iycmin=nyb
c
c               write(*,108)nxa,nya,nxb,nyb
                call x11whiteline(
     *                         %val(nxa)
     *                        ,%val(nya)
     *                        ,%val(nxb)
     *                        ,%val(nyb)
     *                        )
             enddo   
      endif
c
      return
c
100   format('DRAWFACE: IFULL=',i5,', NUMBER(1/2,IFULL)=',2i5
     *      ,', IDCUBE=',i5,', LSC(NF,IDCUBE)=',i5)
1001  format(/'Cube',i4,', face',i4,', LineSegmentCount',i3,':')
101   format('Face',i2,' (',4i5,'), Verts:',6i5)
102   format('Face',i2,' (',4i5,') will not be displayed')
103   format('Line',i2,' on face',i2,' joins vertices',i2,' and',i2)
104   format(/'***** ERROR: Cube',i5,' has vertex',i3,' of face',i3
     *       ,' off screen',/)
105   format('Drawing LineSegment',i3,' in cube',i3)
106   format('##### DRAWFACE: No face drawn - idcube=',i4,', nf=',i4
     *      ,', ncol=',i4,'ifull=',i4)
107   format('DRAWFACE: Face vertex:',7x,'1',7x,'2',7x,'4',7x,'3 (Face'
     *      ,i3,')'
     *     ,/'             Screen X:',4i8  
     *     ,/'             Screen Y:',4i8)
108   format('DRAWFACE: Drawing white lines around',4i4)
      end
