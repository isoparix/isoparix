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
      logical found_depth,new_poly,singleton,polytest
     *       ,unsorted,unmatched,bracketed
c
      character(1),dimension(0:1023,200) :: colourchart
      character(1) :: ssq
      character(50) data_file
      character(30) graph_name
      character(45)outcode
c
      real(8),allocatable,dimension(:) :: local_xstart,local_xend
     *                                   ,local_ystart,local_yend
c
      integer(4),allocatable,dimension(:) :: local_depth
c
      integer(4),dimension(-1:16,0:2000) :: depth_count
      integer(4),dimension(     2000) :: tl,tr
c
      integer(4),dimension(6,2000) :: keyline_a,keyline_b
c
      real(8),dimension(0:2000) :: tg
c
      integer(4)tzc,tyfinal,trfinal,tlfinal
c
c
c
      maxdepth=0
      ntplist=0
c
      do iyz=nylow,nyhi
         if(zcount(iyz).gt.0
     *     )then
c
                if(check
     *            )then
                       write(18,220)
     *                 iyz,(zal(mx,iyz),agrid(mx,iyz),zar(mx,iyz)
     *                     ,zbl(mx,iyz),bgrid(mx,iyz),zbr(mx,iyz)
     *                     ,mx=1,zcount(iyz))
                endif
c
                nticks=0
                do nt=1,zcount(iyz)
                   nticks=nticks+1
                   tg(nticks)=agrid(nt,iyz)
                   nticks=nticks+1
                   tg(nticks)=bgrid(nt,iyz)
                enddo  ! 1,zcount(iyz)
c
c      Bubble sort array tg
c
c        write(18,*)nylow,iyz,nyhi,zcount(iyz)
                unsorted=.true.
                do while(unsorted)
                   nx=0
                   do ns=1,nticks-1
                      if(tg(ns).gt.tg(ns+1)
     *                  )then
                             t=tg(ns)
                             tg(ns)=tg(ns+1)
                             tg(ns+1)=t
                             nx=nx+1
                      endif
                   enddo !  ns=1,nticks-1
                   if(nx.eq.0)unsorted=.false.
                enddo ! while
c
                if(check
     *            )then
                       write(18,2201)
     *                 iyz,(tg(mx),mx=1,nticks)
                endif
c
c      Remove duplicates
c
                nx=1
                do ns=2,nticks
                   if(tg(ns).ne.tg(nx)
     *               )then
                          nx=nx+1
                          tg(nx)=tg(ns)
                   endif
                enddo  !  2,nticks
                nticks=nx
c
c
c      Define probe points as the mid-way points between adjacent
c      elements of tg
c
                do ns=1,nticks
                   tl(ns)=-1
                   tr(ns)=-1
                enddo  ! 1,nticks
c
                do ns=1,nticks-1
                   probe=.5*(tg(ns)+tg(ns+1))  !   Probe at mid-way point
c
                   do mx=-1,maxlayer
                      depth_count(mx,ns)=0
                   enddo
c
                   do ic=1,zcount(iyz)
                      if(probe.gt.agrid(ic,iyz).and.
     *                   probe.lt.bgrid(ic,iyz)
     *                  )then   !  Probe lies between an agrid/bgrid pair...
                             depth_count(zar(ic,iyz),ns)
     *                      =depth_count(zar(ic,iyz),ns)+1
                      endif
                   enddo  !  1,zcount(iyz)
c
                   if(depth_count(0,ns).eq.0
     *               )then   !  No exclusion zone here
                          ndcount=0
                          kburns=0
                          do mx=1,maxlayer
                             if(depth_count(mx,ns).gt.0
     *                         )then  !  Probe has hit a real vlue
                                    if(mx.eq.1
     *                                )then   !   A 'burn' layer
                                           kburns=kburns+1
                                       else
                                           ndcount=ndcount+1
                                           if(ndcount.gt.maxdepth
     *                                       )then
                                                  maxdepth=ndcount
                                           endif
                                    endif
                             endif
                          enddo  !  mx=1,maxlayer
                          if(ndcount.gt.0.and.
     *                       kburns.ge.nburns
     *                      )then
                                 tr(ns  )=ndcount
                                 tl(ns+1)=ndcount
                          endif
                      else
                          tr(ns  )=0
                          tl(ns+1)=0
                   endif  ! depth_count(0,ns).eq.0
                   write(18,2204)iyz,ndcount,probe,nticks,zcount(iyz)
                enddo
c
                if(check.and.zcount(iyz).gt.0
     *            )then
                       write(18,2203)
     *                 iyz,(tl(mx),tg(mx),tr(mx)
     *                 ,mx=1,zcount(iyz))
                endif
c
c      Gather all the edges into one line
c
                nt=0
                do ic=1,nticks
                   if(tl(ic).ne.tr(ic)
     *               )then  ! This is an edge
                          nt=nt+1
                          zg(nt,iyz)=tg(ic)
                          zl(nt,iyz)=tl(ic)
                          zr(nt,iyz)=tr(ic)
                   endif
                   nd=max0(tl(ic),tr(ic))
                enddo
                zcount(iyz)=nt
                if(check.and.zcount(iyz).gt.0
     *            )then
                       write(18,2202) ! Gathered
     *                 iyz,(zl(mx,iyz),zg(mx,iyz),zr(mx,iyz)
     *                 ,mx=1,zcount(iyz))
                endif
                endif ! zcount(iyz) > 0?
      enddo ! iyz=nylow,nyhi
c
c *****************************************************************
c *   We now have all the co-ordinates of where all these circles *
c *   cut this line...                                            *
c *****************************************************************
c
      grid=0
      do iyb=nylow,nyhi
         yg(iyb)=iyb   !   The y-label for this row - subject to change
c
c      Create a grid for a bitmap
c
         do ix=1,zcount(iyb)-1
            if(zr(ix,iyb).eq.zl(ix+1,iyb).and.
     *         zr(ix,iyb).gt.0 !  Avoid exclusion zones
     *        )then ! Next line does the bitmap array
                   grid(int(zg(ix,iyb)):int(zg(ix+1,iyb)),iyb)
     *                     =zr(ix,iyb)
            endif
         enddo  ! ix=1,zcount(iyb)-1
      enddo ! iyb=nylow,nyhi
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
      do ndepth=0,maxdepth
         nslots_old=0
         do iyq=nylow,nyhi
            nslots=0
            do ix=1,zcount(iyq)
               if(zr(ix,iyq).eq.zl(ix+1,iyq).and.
     *            zr(ix,iyq).eq.ndepth
     *           )then
                      nslots=nslots+1
               endif
            enddo
            if(nslots.ne.nslots_old.and.iyq.gt.1.and.
     *         zcount(iyq).eq.zcount(iyq-1)
     *        )then
                   write(20,123)iyq,yg(iyq),ndepth,nslots,nslots_old
     *                         ,zcount(iyq),zcount(iyq-1)
     *                         ,zcount(iyq)-zcount(iyq-1)
     *                         ,nslots-nslots_old
            endif
            nslots_old=nslots
         enddo
      enddo
c
      nytop=nyhi
c     go to 80
      iyb=nylow-1
      do iyline=nylow,nyhi
         iyb=iyb+1
         bracketed=.false.
c
         if(iyb.gt.nylow      .and.
     *      zcount(iyb-1).gt.0.and.
     *      zcount(iyb  ).gt.0.and.
     *      zcount(iyb-1).ne.zcount(iyb)
     *     )then
                if(zcount(iyb-1).gt.zcount(iyb)
     *            )then
                       iyp=iyb-1  ! 'P' for positive, greater than,...
                       iyq=iyb
                       write(18,122)iyb,iyp,zcount(iyp),iyq,zcount(iyq)
     *                             ,yg(iyp)
                   else
                       iyp=iyb    ! 'P' for positive, greater than,...
                       iyq=iyb-1
                       write(18,121)iyb,iyp,zcount(iyp),iyq,zcount(iyq)
     *                             ,yg(iyp)
                endif  ! zcounts not equal
                tzc=zcount(iyp)       !   The one with the biggest zcount
c
         do my=1,tzc    !   Copy line
            tl(my)=zl(my,iyp)
            tg(my)=zg(my,iyp)
            tr(my)=zr(my,iyp)
         enddo
c
         iz=zr(1,iyp)   !  Colour of this line segment (1:2,iyp)
         za=zg(1,iyp)
         do ix=2,zcount(iyp)
            zb=zg(ix,iyp)
            if(iz.eq.zl(ix,iyp)
     *        )then
                   do mx=2,zcount(iyq)    !  DEBUG Move 't's to 'e's..
                      if(za.ge.zg(mx-1,iyq).and.
     *                   zb.ge.zg(mx-1,iyq).and.
     *                   za.le.zg(mx,iyq).and.
     *                   zb.le.zg(mx,iyq).and.
     *                   iz.ne.zl(mx,iyq)
     *                  )then
                             bracketed=.true.
                             if(check
     *                         )then
                                    nposp=0
                                    do mq=1,ix-1
                                       if(zr(mq,iyp).eq.iz)nposp=nposp+1
                                    enddo
                                    nposq=0
                                    do mq=1,mx-1
                            if(zr(mq,iyq).eq.zr(mx-1,iyq))nposq=nposq+1
                                    enddo
                                    write(18,223)iyp,yg(iyp),za,zb,iz
     *                       ,nposp,zg(mx-1,iyq),zg(mx,iyq),zr(mx-1,iyq)
     *                       ,nposq,yg(iyq)
                             endif
c
c      Reflect the bracketing...
c
c                          za   zb
c            ix-2         ix-1  ix             ix+1
c      iyp     *------------*||||*--------------*   zcount(iyp)>zcount(iyq)
c      NEW     *------------*----*--------------*   New line segment has
c      iyq            *-----------------------*     same value as bracket
c                  zg(mx-1)                 zg(mx)  
c
                             ty=yg(iyp)
                             tr(ix-1)=zr(mx-1,iyq)
                             tl(ix  )=zr(mx-1,iyq)
                      endif
                   enddo
            endif
            za=zb
            iz=zr(ix,iyp)
         enddo
c
         if(bracketed
     *     )then
c
                write(18,2205)yg(iyp)  ! Current line
     *         ,(zl(my,iyp),zg(my,iyp),zr(my,iyp),my=1,zcount(iyp))
                write(18,2205)yg(iyq)  ! Current line
     *         ,(zl(my,iyq),zg(my,iyq),zr(my,iyq),my=1,zcount(iyq))
                write(18,2206)tyfinal,(tl(my),tg(my),tr(my),my=1,tzc)! Altered line
                iys=iyb
                nytop=nytop+1
                do n=nytop,iys+1,-1   !  Push all entries up the stack
                   do m=1,zcount(n-1)
                      zl(m,n)=zl(m,n-1)
                      zg(m,n)=zg(m,n-1)
                      zr(m,n)=zr(m,n-1)
                   enddo
                       yg(n)=yg(n-1)
                   zcount(n)=zcount(n-1)
                enddo
                m=0
                do n=1,tzc
                   if(tl(n).ne.tr(n)
     *               )then
                          m=m+1
                          zl(m,iys)=tl(n)
                          zg(m,iys)=tg(n)
                          zr(m,iys)=tr(n)
                   endif
                enddo
                    yg(iys)=ty
                zcount(iys)=m
                iyb=iyb+1   ! WAS 2 !!
                if(check
     *            )then
                       write(18,2191)int(yg(iys))
     *                   ,(zl(n,iys),zg(n,iys),zr(n,iys),n=1,m)
                       write(18,120)
                endif
            endif
         endif  ! iyb.gt.nylow, zcount(iyb-1).gt.0, zcount(iyb).gt.0
      enddo  ! nylow to nyhi
c
 80   continue
c
      if(check
     *  )then
             do n=nylow,nytop
                write(18,219)n,yg(n)  !  GM line
     *                     ,(zl(mx,n),zg(mx,n),zr(mx,n)
     *                      ,mx=1,zcount(n))
             enddo
      endif
c
      new_poly=.false.
      ameg=1000000.
      nkey=1
      line_view=.true.
      keyline_a(1,1)=1
      nrle=0
      kbmax=0
      do ndepth=maxdepth,1,-1
         nx=1
         do while (nx.gt.0)
            nx=0
            do iyq=nylow,nytop
               ixn=0
               do ix=1,zcount(iyq)
                  if(zr(ix,iyq).eq.zl(ix+1,iyq).and.
     *               zr(ix,iyq).eq.ndepth.and.   !  Get this depth
     *               ixn.eq.0               !  Capture the first valid zone
     *              )then
c
c      Capture the 'B'-line
c
                         nrle=nrle+1
                         xstart(nrle)=zg(ix,  iyq)
                         xend  (nrle)=zg(ix+1,iyq)
                         ystart(nrle)=yg(iyq)
                         yend  (nrle)=yg(iyq)
                         depth (nrle)=zr(ix,iyq)
                         if(nrle.gt.1
     *                     )then
                                if(polytest(
     *                             depth (nrle-1),depth (nrle)
     *                            ,xstart(nrle-1),xstart(nrle)
     *                            ,xend  (nrle-1),xend  (nrle)
     *                            ,yend  (nrle-1),yend  (nrle)
     *                            ,check   )
     *                            )then
                                   keyline_b(1,nkey)=nrle-1
                                   keyline_b(2,nkey)=xstart(nrle-1)*ameg
                                   keyline_b(3,nkey)=xend  (nrle-1)*ameg
                                   keyline_b(4,nkey)=yend  (nrle-1)
                                   keyline_b(5,nkey)=depth (nrle-1)
                                   keyline_b(6,nkey)=0
              write(18,1121)nkey,keyline_a(1:6,nkey),keyline_b(1:6,nkey)
                                       nkey=nkey+1
                                   keyline_a(1,nkey)=nrle
                                   keyline_a(2,nkey)=xstart(nrle)*ameg
                                   keyline_a(3,nkey)=xend  (nrle)*ameg
                                   keyline_a(4,nkey)=yend  (nrle)
                                   keyline_a(5,nkey)=depth (nrle)
                                   keyline_a(6,nkey)=0
                                endif
                         endif
c
                         if(check
     *                     )then
                                write(18,222)nrle
     *                           ,int(ystart(nrle))
     *                               ,xstart(nrle)
     *                               ,xend(  nrle)
     *                               ,depth (nrle)
     *                           ,.5*(xstart(nrle)+xend(nrle))
                         endif
c
                         zr(ix,  iyq)=0
                         zl(ix+1,iyq)=0
                         nx=nx+1
                         ixn=1
                  endif !  General conditions on zone
               enddo  ! IX...
               nslots_old=nslots
            enddo  ! IYQ
            write(18,104)ndepth,nx
         enddo   !  WHILE...
      enddo   !  NDEPTH...
c
      if(nrle.le.0
     *  )then
             write(0,118)nrle
             stop
      endif
c
c      We now have all start and end points for all instances of
c      all depths on all lines
c
      ybak2=yend(1)
      ybak1=yend(2)
      y    =yend(3)
      yfor1=yend(4)
      yfor2=yend(5)
c
      xsbak2=xstart(1)
      xsbak1=xstart(2)
      xs    =xstart(3)
      xsfor1=xstart(4)
      xsfor2=xstart(5)
c
      xebak2=xend(1)
      xebak1=xend(2)
      xe    =xend(3)
      xefor1=xend(4)
      xefor2=xend(5)
c
      do n=6,nrle
         xspbak=poly_x(xsbak2,ybak2,xsbak1,ybak1,y)
         xspfor=poly_x(xsfor1,yfor1,xsfor2,yfor2,y)
         xepbak=poly_x(xebak2,ybak2,xebak1,ybak1,y)
         xepfor=poly_x(xefor1,yfor1,xefor2,yfor2,y)
         write(20,119)n-3,int(y),xs,xspfor,xspbak,xs-xspfor,xs-xspbak
     *                          ,xe,xepfor,xepbak,xe-xepfor,xe-xepbak
         write(2,1191)n-3,int(y),xs,xs,depth(n-3)
         ybak2=ybak1
         ybak1=y
         y    =yfor1
         yfor1=yfor2
         yfor2=yend(n)
c
         xsbak2=xsbak1
         xsbak1=xs
         xs    =xsfor1
         xsfor1=xsfor2
         xsfor2=xstart(n)
c
         xebak2=xebak1
         xebak1=xe
         xe    =xefor1
         xefor1=xefor2
         xefor2=xend(n)
c
      enddo
c
      keyline_b(1,nkey)=nrle
      keyline_b(2,nkey)=xstart(nrle)*ameg
      keyline_b(3,nkey)=xend  (nrle)*ameg
      keyline_b(4,nkey)=ystart(nrle)
      keyline_b(5,nkey)=depth (nrle)
      keyline_b(6,nkey)=0
c
c
c
      if(check
     *  )then
             do nk=1,nkey
                write(18,1121)nk,keyline_a(1:6,nk),keyline_b(1:6,nk)
             enddo
             call flush(18)
      endif
c     stop    !  ********** STOP ************
c
      do n=1,nkey
         nxs=keyline_a(2,n)
         nxe=keyline_a(3,n)
         nxy=keyline_a(4,n)-1
         nxd=keyline_a(5,n)
         unmatched=.true.
         m=1
         do while (unmatched.and.m.le.nkey)
            if(keyline_b(2,m).le.nxe.and.
     *         keyline_b(3,m).ge.nxs.and.
     *         keyline_b(4,m).eq.nxy.and.
     *         keyline_b(5,m).eq.nxd.and.
     *         m.ne.n
     *        )then  ! keyline_a (top) has a match with keyline_b (bottom)
                   keyline_a(6,n)=m
                   keyline_b(6,m)=n
                   unmatched=.false.
            endif
            m=m+1
         enddo  !   While
      enddo  ! n=1,nkey
c
c      Allocate local space
c
      allocate(xleft (3*nrle))
      allocate(yleft (3*nrle))
      allocate(xright(3*nrle))
      allocate(yright(3*nrle))
c
c      Write Google headers
c
      write(outcode,1181)trim(title)  ! Start of KML Google Earth file
      open(2,file=outcode,form='formatted',status='unknown')
      write(2,1000)
c
      nzoom=12
      write(outcode,1182)trim(title)  ! Start of KML Google Maps file
      open(4,file=outcode,form='formatted',status='unknown')
      write(4,2000)trim(title),ctrlat,ctrlon,nzoom
c
      if(check.or.graph
     *  )then
             graph_name=trim(title)
             al=nxlow
             ah=nxhi
             bl=nylow
             bh=nyhi
             call grf_header(al,ah,bl,bh,graph_name)
      endif
c
      if(check
     *  )then
             do nk=1,nkey
                write(18,112)nk,keyline_a(1:6,nk),keyline_b(1:6,nk)
             enddo
      endif
c
c     Chain the polygons
c
      npolygon=0
      do nk=1,nkey
         if(keyline_a(6,nk).eq.0
     *     )then  !  This is the start of a chain
                nplist=0
                mk=nk
                npolygon=npolygon+1
                if(check)write(18,113)npolygon
c
                kk=1
                do while (kk.gt.0)
                   if(check)write(18,114)mk,keyline_a(1,mk)
     *                                     ,keyline_b(1,mk)
     *                                  ,npolygon
     *                                   ,1+keyline_b(1,mk)
     *                                     -keyline_a(1,mk)
     *                                     ,keyline_a(4,mk)
     *                                     ,keyline_b(4,mk)
                   do npl=keyline_a(1,mk),keyline_b(1,mk)
                      nplist=nplist+1
                      xleft (nplist)=xstart(npl)
                      xright(nplist)=xend  (npl)
                      yleft (nplist)=ystart(npl)
                      yright(nplist)=yend  (npl)
                   enddo
                   if(keyline_b(6,mk).ne.0
     *               )then
                          mk=keyline_b(6,mk)
                      else
                          kk=0
                   endif
                enddo ! While
c
                ndepth=keyline_b(5,mk)
                ntplist=ntplist+nplist
                if(check
     *            )then
                       write(18,116)nplist,ntplist,npolygon,ndepth,mk
                endif
                call kml_prep(nplist,npolygon,ndepth)
         endif ! keyline_a(6,nk).eq.0
      enddo ! nk=1,nkey
c
      if(check.or.graph)call grf_trailer(npolygon)
c
      write(2,1003)
      write(4,2004)
c
      return
c
100   format('SCAN: nrle,depth,xs,ys,xe,ye',2i7,2(f14.7,f8.1))
1011  format('GRIDMAP POLYGON_END:        ',2i7,2(f14.7,f8.1),/)
1012  format('GRIDMAP POLYGON START:      ',2i7,2(f14.7,f8.1))
1013  format('GRIDMAP: New polygon')
1001  format('POLY_END  ',2i6,23x,2i6,12x,4f15.7,//)
1002  format('POLY_START',2i6,23x,2i6,12x,4f15.7,//)
102   format('POLY',i4,' n  nrle depth',19x,'xstart',19x,'ystart',20x
     *      , 'xend',22x,'yend')
103   format('GRIDMAP: Count is:',i8,' of',i8,' elements.'
     *       ,i9,' found at depth',i3,'.  Starts/ends:',2i6)
104   format('GRIDMAP: Depth',i3,', count',i8)
107   format('GRIDMAP ERROR (A): line_start(iy),    ix,iy,ax-/+2'
     *      ,i10,2i5,5f12.5
     *      ,/44x,'bx-/+2',20x,5f12.5
     *      ,/44x,'depth ',16x,5i12)
108   format('GRIDMAP ERROR (B): line_start(iy),ix,iy,ax-/+2'
     *      ,3i5,5f12.5
     *      ,/44x,'bx-/+2',20x,5f12.5
     *      ,/44x,'depth ',16x,5i12)
109   format('GRIDMAP: NPLIST is:',i8,' (was',i8,') of',i8,' elements.'
     *      ,' Delta=',i6)
110   format('GRIDMAP: Single square - ix,iy,grid,agrid,bgrid:'
     *      ,3i5,2f15.7)
111   format('GRIDMAP: End of joining scan')
112   format('Post-chain key:',i5,2(' Runline',i5,2i12,i6,2i5))
1121  format(' Pre-chain key:',i5,2(' Runline',i5,2i12,i6,2i5),/)
113   format(/'GRIDMAP: Start of new polygon',i3)
114   format('GRIDMAP Using nkey',i3,': Sub-chain of runlines from'
     *      ,i5,' to',i5,' in polygon',i3,'.  Sub-chain length=',i5
     *      ,'.  Y-values (x10)',i6,' to',i6)
115   format('GRIDMAP: Polygons reduced from',i4,' to',i4)
116   format('GRIDMAP:  Data pairs',i6,' of',i6
     *      ,': Npolygon',i4,', ndepth',i4,'. Last key=',i4)
117   format('GRIDMAP ERROR: Duplicate at',i5,' and',i5,':',6i12)
118   format('GRIDMAP ERROR: NRUNLINES=',i5)
119   format('Runline',i5' Y=',i5,':'
     *    ,2('  Actual/predicted:',3f7.2,2f7.1))
1191  format('Runline',i5' Y=',i5,':  Start',f8.2
     *                             ,' End=',f8.2,' Depth=',i2)
120   format(/)
121   format('iyb=',i5
     *    ,', iyp=',i5,', zcount(iyp)=',i5
     *    ,', iyq=',i5,', zcount(iyq)=',i5,' IYB <==> IYP',i5,':')
122   format('iyb=',i5
     *    ,', iyp=',i5,', zcount(iyp)=',i5
     *    ,', iyq=',i5,', zcount(iyq)=',i5,' IYB <==> IYQ',i5,':')
123   format('Line=',i3,' Y=',i5,' ndepth=',i3
     *        ,': Current/old slots=',2i3
     *       ,' Current/old zcounts=',2i3
     * ,' Differences zcounts/slots=',2i3)
204   format(/'GRIDMAP: Last element')
205   format(/'GRIDMAP:   Seeking',i4,4f15.7)
206   format( 'GRIDMAP:     Found',i4,4f15.7,', N=',i6,/)
207   format( 'GRIDMAP: Not found',i4,3f15.7,/)
219   format('GM line',i5,' Y=',i5,':',100(i3,f7.2,i3))
2191  format( 'Extra new at Y=',i5,':',100(i3,f7.2,i3))
220   format(/'Ticks        Y=',i5,':',100(i3,f7.2,i3))
2201  format( 'Sorted       Y=',i5,':',100(  f10.2,3x))
2202  format( 'Gathered     Y=',i5,':',100(i3,f7.2,i3))
2203  format( 'Probed       Y=',i5,':',100(i3,f7.2,i3))
2204  format( 'Sample       Y=',i5,':',100(i3,f7.2,i3))
2205  format( 'Current line Y=',i5,':',100(i3,f7.2,i3))
2206  format( 'Altered line Y=',i5,':',100(i3,f7.2,i3))
221   format(1000i6)
222   format('Runline',i5,' Y=',i5,':',f10.2,f13.2,i3,f10.2)
223   format('Continuation at line',i5,', Y-value',i5
     *      ,' between',2f10.2
     *      ,' (level',i2,' pos',i2') bracketed by',2f10.2
     *      ,' (level',i2,' pos',i2') at Y=',i5)
224   format('ERROR in GRIDMAP: Right at tick',i2,' (',i2,')'
     *                        ,' Left at tick',i2,' (',i2,')'
     *      ,' not equal at line',i4)
1000  format(' <kml xmlns="http://www.opengis.net/kml/2.2">'
     *     ,/'  <Document>')
1003  format('   </Document>'
     *     ,/'</kml>'
     *      )
1181  format(a,'_poly_earth.kml')
1182  format(a,'_poly_maps.html')
2000   format('<!DOCTYPE html>'
     *      ,/'<html>'
     *      ,/'  <head>'
     *      ,/'  <meta name="viewport" content="initial-scale=1.0'
     *      ,                                ',user-scalable=no">'
     *      ,/'  <meta charset="utf-8">'
     *      ,/'  <title>Google Maps JavaScript API v3 ',a,'</title>'
     *      ,/'  <link href="http://code.google.com//apis/maps/'
     *      ,     'documentation/javascript/examples/default.css"'
     *      ,     ' rel="stylesheet" type="text/css" rel="stylesheet">'
     *      ,/'  <script type="text/javascript" '
     *      ,/      'src="https://maps.googleapis.com/maps/api/js?'
     *      ,       'key=AIzaSyBrt_zwpr1_uZXfGENhPDVQCIuNZIVKPk8'
     *      ,       '&sensor=false">'
     *      ,/'  </script>'
     *      ,/'  <script type="text/javascript">'
     *      ,/'    var map;'
     *      ,/'    var infoWindow;'
     *      ,/'    function initialize() {'
     *      ,/'    var myCentre = new google.maps.LatLng'
     *      ,                       '(',f0.6,',',f0.6');'
     *      ,/'    var mapOptions = {zoom: ',i3,', center: myCentre'
     *      ,       ',mapTypeId: google.maps.MapTypeId.ROADMAP};'
     *      ,/'      map = new google.maps.Map(document.getElementById'
     *      ,       "('map-canvas'),mapOptions);"
     *       )
2004  format(' }'
     *     ,/'</script>'
     *     ,/'   </head>'
     *     ,/'   <body onload="initialize()">'
     *     ,/'     <div id="map-canvas"></div>'
     *     ,/'   </body>'
     *     ,/'</html>'
     *      )
c
      end
