      subroutine gridmap
c
c      Study the grid and end up with a list of where particular grid
c      values (the 'depth') (x,y)start and (x,y)end in each row of the grid.   
c      The 'number of run-line elements', nrle, is the size of the 
c      collection of all of these elements.
c
      use venn_comms
      use bmp_comms
c
      implicit real(8) (a-h,o-z)
c
      logical found_depth,new_poly,suspend
      logical,dimension(20000) :: el_view
c
      character(1),dimension(0:1023,200) :: colourchart
c
      real(8),allocatable,dimension(:) :: local_xstart,local_xend
     *                                   ,local_ystart,local_yend
c
      integer(4),allocatable,dimension(:) :: local_depth
c
      integer(4),dimension(0:20) :: depth_count
c
      ncolstep=int(254./float(maxdepth))
      if(check.or.bitmap
     *  )then
             bmname=trim(title)
             call gridbmp
c
             do n=0,255
                m=n*4
                colourchart(m:m+3,:)=char(n)
             enddo
             bmname='colourchart'
cPROB            call array2bmp(1024,200,colourchart)
      endif
c
      lgrid=.true.
c
c      Count and clear the zeros
c
      depth_count=0
      ncount=0
      do iy=nylow,nyhi
         do ix=nxlow,nxhi
            if(grid(ix,iy).eq.0
     *        )then
                   ncount=ncount+1
                   lgrid(ix,iy)=.false.
            endif
            if(check
     *        )then
                   depth_count(grid(ix,iy))=1+depth_count(grid(ix,iy))
            endif
         enddo
      enddo
      if(check)write(*,104)(mx,depth_count(mx),mx=0,maxdepth)
c
      line_view=.true.
      nrle=0
  3   continue
      do ndepth=1,maxdepth
         line_start=nxlow
  4      continue
         ndcount=0
         do iy=nylow,nyhi
            if(line_view(iy)
     *        )then
c
c      Find the start, in this line, of a 'true' value on lgrid
c
                    found_depth=.false.
                    do ix=line_start(iy),nxhi
c      Find the start (recorded as idx) of this depth in this row
c
                      if(grid(ix,iy).eq.ndepth.and.lgrid(ix,iy)
     *                  )then
                             if(agrid(ix,iy).gt.10000.
     *                         )then
                                    write(0,107)line_start(iy),idx,ix,iy
     *                                         ,agrid(ix-2:ix+2,iy)
     *                                         , grid(ix-2:ix+2,iy)
                                    stop
                             endif
                             nrle=nrle+1  !  Start a new line segment pair
                             idx=ix       !  Got the start point in this line
                             depth (nrle)  =ndepth
                             xstart(nrle)  =agrid(ix,iy)
                             ystart(nrle)  =real(iy)
                             yend  (nrle)  =real(iy)
                             found_depth=.true.
                             exit
                      endif
                   enddo
c
c      Find the end, in this row, of this depth value on the grid
c
                   if(found_depth
     *               )then
                          do ix=idx,nxhi
                             if(grid(ix,iy).eq.ndepth.and.lgrid(ix,iy)
     *                         )then
                                    xend(nrle)  =agrid(ix,iy)
                                    ncount = ncount+1
                                    ndcount=ndcount+1
                                    lgrid(ix,iy)=.false. ! Clear this element
                                    itx=ix   !  Where do we exit?
                                else
                                    exit
                             endif
                          enddo                   ! End of line
c
                          if(check
     *                       )then
                                  write(12,101)idx,itx,iy
     *                      ,grid(idx,iy),agrid(idx,iy)
     *                      ,grid(itx,iy),agrid(itx,iy)
                                 if(itx.eq.idx
     *                             )then !  This is a single square
                                        write(18,110)ix,iy,grid(ix,iy)
     *                                                   ,agrid(ix,iy)
                                 endif
                          endif
c
                          line_start(iy)=itx+1  ! Start here next time...
                          if(xend(nrle  ).gt.10000.
     *                      )then
                                 write(0,108)line_start(iy),idx,itx,iy
     *                                         ,agrid(itx-2:itx+2,iy)
     *                                         , grid(itx-2:itx+2,iy)
                              stop
                          endif
                   endif   !  found_depth
            endif
         enddo
         if(check)write(*,103)ncount,nelements,ndcount,ndepth
         if(ndcount.gt.0)go to 4
      enddo
c
      if(ncount.gt.nelements)stop
c
c      Allocate local space
c
      allocate(local_xstart(nrle))
      allocate(local_ystart(nrle))
      allocate(local_xend  (nrle))
      allocate(local_yend  (nrle))
      allocate(local_depth (nrle))
c
      if(ncount.eq.nelements
     *  )then
             if(check
     *         )then
                    write(16,102)
             endif
c
  8          continue
             npoly=0
             el_view=.true.
             nplist=0
             nplistold=0
c
c      Replicate last line
c
             xstart(nrle+1)=xstart(nrle)
             ystart(nrle+1)=ystart(nrle)
             xend  (nrle+1)=xend  (nrle)
             yend  (nrle+1)=yend  (nrle)
c
             nfirst=1
  7          continue
             suspend=.false.
             nviewed=0
             nqx=0
             nlines=0
c
             do n=nfirst,nrle
                if(el_view(n)
     *            )then
                       if(check
     *                   )then
                              write(16,100)n,nrle,depth(n)
     *                                         ,xstart(n),ystart(n)
     *                                         ,xend(n)  ,yend(n)
                       endif
                       nviewed=nviewed+1
c
                       if(.not.suspend
     *                   )then  !  Collect this line
                              el_view(n)=.false.
                              nplist=nplist+1
                              local_xstart(nplist)=xstart(n)
                              local_ystart(nplist)=ystart(n)
                              local_xend  (nplist)=xend(n)
                              local_yend  (nplist)=yend(n)
                              local_depth (nplist)=depth(n)
                              nlines=nlines+1
                              if(check)write(18,100)n,nrle,depth(n)
     *                                          ,xstart(n),ystart(n)
     *                                          ,xend(n)  ,yend(n)
                       endif
c
c      Test for new polygon
c
                       new_poly=.false.
                       if(depth(n).ne.depth(n+1)
     *                   )then
                              if(check)write(16,200)
                              new_poly=.true.
                       endif
c
                       if(xstart(n).ne.xend(n)
     *                   )then
                              if(xstart(n).gt.xend(n+1)
     *                          )then
                                     if(check)write(16,201)
                                     new_poly=.true.
                              endif
c
                              if(xend(n).lt.xstart(n+1)
     *                          )then
                                     if(check)write(16,202)
                                     new_poly=.true.
                              endif
                       endif
c
                       if((sqrt((yend(n)-yend(n+1))**2)).gt.1.1
     *                   )then
                              if(check)write(16,203)
                              new_poly=.true.
                       endif
c
                       if(new_poly
     *                   )then
                              if(suspend
     *                          )then
c
c      Can we un-suspend?  Does the unrecorded next, first, line of the new
c      poly match the last line of the last-recorded poly?
c
                                     if(xzs.eq.xstart(n+1).and.
     *                                  yzs.eq.ystart(n+1).and.
     *                                  xze.eq.xend  (n+1).and.
     *                                  izd.eq.depth (n+1)
     *                                 )then
                                            suspend=.false.
                                            if(check)write(18,206)
     *                                               depth(n+1)
     *                                          ,xstart(n+1),ystart(n+1)
     *                                          ,xend(n+1),yend(n+1),n+1
                                     endif
                                 else
c
c      Record the details of the last line of the last polygon
c
                                     xzs=xstart(n)
                                     yzs=ystart(n)
                                     xze=xend(n)
                                     izd=depth(n)
                                     suspend=.true. ! Need to find these again
                                     nlines=0
c
                                     if(check)write(18,205)depth(n)
     *                                       ,xstart(n),ystart(n)
     *                                       ,xend(n),yend(n)
                               endif
                               npoly=npoly+1
                               new_poly=.false.
                               if(check
     *                           )then
                                      write(16,1001)npoly,depth(n)
     *                               ,n,nlines
     *                               ,xstart(n),xend(n)
     *                               ,ystart(n),yend(n)
                                endif
                       endif  !  ...from new_poly
                endif  !  ...from el_view
             enddo
c            return      !  TEST TO HERE!
c
             if(check)write(*,109)nplist,nplistold,nrle,nplist-nplistold
             if(suspend
     *         )then
                    if(check)write(18,207)izd,xzs,yzs,xze
             endif
             call flush(12)
             call flush(16)
             call flush(18)
             nplistold=nplist
c
             if(nplist.eq.nrle
     *         )then
                    xstart(1:nplist)=local_xstart(1:nplist)
                    ystart(1:nplist)=local_ystart(1:nplist)
                    xend  (1:nplist)=local_xend(1:nplist)
                    yend  (1:nplist)=local_yend(1:nplist)
                    depth (1:nplist)=local_depth(1:nplist)
c                   read(*,*)junk
c                   if(junk.eq.0
c    *                )then
                           return
c                      else
c                          go to 8
c                    endif
                else
                    if(check)write(16,111)
                    go to 7
             endif
c
             return
         else
             go to 3
      endif
c
100   format(i10,2i6,4f15.7)
1001  format('POLY',4i6,6f15.7,/)
101   format('GRIDMAP: idx,itx,iy,grid,agrid:',3i6,2(i6,f15.7))
102   format('POLY',i4,' n  nrle depth',19x,'xstart',19x,'ystart',20x
     *      , 'xend',22x,'yend')
103   format('GRIDMAP: Count is:',i8,' of',i8,' elements.'
     *       ,i9,' found at depth',i3)
104   format('GRIDMAP: Depth',i3,', count',i8)
107   format('GRIDMAP ERROR (A): line_start(iy),idx,ix,iy,ax-/+2,depth'
     *      ,4i5,5e10.3
     *      ,/72x,5i10)
108   format('GRIDMAP ERROR (B): line_start(iy),idx,itx,iy,ax-/+2,depth'
     *      ,4i5,5e10.3
     *      ,/72x,5i10)
109   format('GRIDMAP: NPLIST is:',i8,' (was',i8,') of',i8,' elements.'
     *      ,' Delta=',i6)
110   format('GRIDMAP: Single square - ix,iy,grid,agrid:'
     *      ,3i5,2f15.7)
111   format('GRIDMAP: End of joining scan')
200   format(/'GRIDMAP: Change of depth')
201   format(/'GRIDMAP: XSTART > previous XEND')
202   format(/'GRIDMAP: XEND < previous XSTART')
203   format(/'GRIDMAP: Line break')
204   format(/'GRIDMAP: Last element')
205   format(/'GRIDMAP:   Seeking',i4,4f15.7)
206   format( 'GRIDMAP:     Found',i4,4f15.7,', N=',i6,/)
207   format( 'GRIDMAP: Not found',i4,3f15.7,/)
c
      end
