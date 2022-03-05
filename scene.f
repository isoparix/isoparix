      subroutine scene
c
      use isocomm
c
      ixcmax=0
      ixcmin=1000000
      iycmax=0
      iycmin=1000000
c
c     write(*,105)nlong,(longact(mx),mx=1,nlong)
      if(ye.gt.0)call floorshow
c
c      Set up hidden-line removal
c
      call hidrem
c
      nclim=0
      ndraw=0
      ntotface=0
c
      do nla=1,nlong
         nclim=nclim+1
         notviewed(longact(nla))=.true.
      enddo
c
c     write(*,105)nlong
  1   continue
c
      idraw=0
      do nla=1,nlong
         idc=longact(nla)
         if(notviewed(idc)
     *     )then
c
c      ...we haven't drawn cube IDC yet...
c
                mx=0
                do nlb=1,nlong
                   if(cmasks(idc,longact(nlb))
     *               )then
c
c      Cube IDC is masking cube LONGACT(NLB)...
c
c                write(*,111)idc,sqrt(vdmin(idc)),sqrt(vdmax(idc))
c    *             ,min_x(idc),min_y(idc),max_x(idc),max_y(idc)
c    *             ,longact(nlb),sqrt(vdmin(longact(nlb)))
c    *                          ,sqrt(vdmax(longact(nlb)))
c    *             ,min_x(longact(nlb)),min_y(longact(nlb))
c    *             ,max_x(longact(nlb)),max_y(longact(nlb))
                          mx=mx+1
                   endif
                enddo
c
                if(mx.eq.0
     *            )then
                       call drawcube(idc)
                       notviewed(idc)=.false.
                       ndraw=ndraw+1
                       idraw=idraw+1
c                      write(*,112)idc,ndraw
c
c      Now that this 'covering nothing' cube has been drawn, it can't be
c      hidden any more by any other cubes.
c      Who covers what? If cmasks is true, then the second 
c      element of cmasks is covered by the first.
c
                       do nlc=1,nlong
                          cmasks(longact(nlc),idc)=.false.
                       enddo
c
                endif	! mx=0
         endif	! notviewed(idc)
c
      enddo
c
c      Did we draw anything?   If not, leave this alone...
c
      if(idraw.eq.0)then
                        if(ye.lt.0)call floorshow
                        go to 2
      endif
c
      if(ndraw.lt.nclim)then
                        go to 1
      endif
c
  2   continue
c
      if(ye.lt.0)call floorshow
      call outline
      return
c
100   format(/'***** ERROR IN SCENE - nothing drawn, with active cubes'
     *     ,//i13,100i3)
101   format(i3,2l3,':',100l3)
102   format(8(2i4,' :'))
103   format('***** Entering subroutine SCENE *****'
     *     ,/'                          Number of active cubes is',i4)
104   format('LineSegmentCount',i3,' on face',i2,' of inactive cube',i3)
105   format('SCENE:   NLONG=',i8
     *     ,/'       LONGACT=',64i4)
111   format('SCENE: Cube',i4,2f7.1,4i5,' masks cube',i4,2f7.1,4i5)
112   format('Cube',i3,' covers nothing and has been drawn. Total',i3)
      end
