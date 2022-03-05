      subroutine edge(ixout,iyout,kdy,mcycle)
c
c      Delivers a new picture centre and dimensions
c      An edge centre from MSET, or a zoom-out
c
      use isocomm
c
      parameter (nedges=50000)
c
      integer(4),dimension(nedges) :: ixcen,iycen
c
      real(8) x
c
c      Check mcycle...
c
      if(mcycle.ne.1.and.
     *   mcycle.ne.2
     *  )then
             kdy=iym
             ixout=ixm/2
             iyout=iym/2
             write(0,102)mcycle
             iso_mpi_term=.true.
             return
      endif
c
      if(mcycle.eq.2
     *  )then
c
c      ...we're zooming out!
c
             percent=7.5
             kdy=real(iym)*(1.+(.01*percent))
             ixout=-1
             iyout=-1
             write(*,103)kdy
             return
      endif
c
      kdy=10
      idx=ixm/kdy
      idy=iym/kdy
c
      msetmax=0
      nc=0
      ncen=0
      do iy=idy,iym-idy
         do ix=idx,ixm-idx
c
            if(mset(ix,iy).gt.limit
     *        )then
                   if(mset(ix-1,iy).lt.limit.or.
     *                mset(ix+1,iy).lt.limit
     *               )then
                          nc=nc+1
                          if(nc.le.nedges
     *                      )then
                                 ncen=ncen+1
                                 ixcen(ncen)=ix
                                 iycen(ncen)=iy
                          endif
                   endif
               else
                   if(mset(ix,iy).gt.msetmax
     *               )then
                          msetmax=mset(ix,iy)
                          ixout=ix
                          iyout=iy
                   endif
            endif
         enddo
      enddo
c
      if(ncen.gt.0
     *  )then
c
c      Pick a new centre based on time-of-day...
c
             call tim(x)
             i=113.*(x-real(int(x)))*real(ncen)
             i=1+mod(i,ncen)
             ixout=ixcen(i)
             iyout=iycen(i)
             write(*,100)i,ixcen(i),iycen(i),nc,nedges,mset(ixout,iyout)
         else
             write(*,101)ixout,iyout,msetmax,mset(ixout,iyout)
      endif
c
      if(screen_graphics
     *  )then
c
c      Calculate new boundaries
c
             idxa=ixm/(2*kdy)
             idya=iym/(2*kdy)
             kx1=ixout-idxa
             kx2=ixout+idxa
             ky1=iyout-idya
             ky2=iyout+idya
             kdy=2*idya
c
c      Flash the proposed area
c
             do i=10,0,-1
                write(txtout,112)i
                if(i.le.4
     *            )then
                       call x11bound(%val(kx1),%val(ky1),
     *                               %val(kx2),%val(ky2))
                       call x11flush()
                endif

c
c      Display what's going on, and check for exposures...
c
                call title
                do mx=1,10
                   call microsleep(100000)
                   call x11spotbutton(ja,jb,jc)
                enddo
             enddo
      endif
c
      return
c
100   format('EDGE: Centre',i7,' at (',2i6,') chosen out of',i8
     *      ,' edges found.  Highest/actual iteration',2i6)
101   format('EDGE: Centre placed at (',2i6
     *      ,'). No edges found.',45x,'Highest/actual iteration',2i6)
102   format('***** ERROR IN EDGE: MCYCLE has value of',i10)
103   format('EDGE: zooming out to IYM=',i6)
112   format('Viewing - new picture starting in',i3,' seconds') 
c
      end
