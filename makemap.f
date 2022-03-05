      subroutine makemap(winx1,winy1,winwidth,winheight)
c
c      Scrapes the window, and creates a bitmap
c
      use bmp_comms
c
      integer (4) winx1,winy1,winwidth,winheight,winwtmp,winhtmp
      integer (4) npixel, npixelold
c
      logical nuwinpel,allocate_needed
c
      integer (1) rgb(1024)
      equivalence(rgb,rgbq)
      character (1024) rgbq
c
      winwtmp=winwidth
c     winwidth=100
c
      write(*,108)trim(bmname)
c
      allocate_needed=.false.
      if(winwidth .ne.winwidth_old.or.
     *   winheight.ne.winheight_old
     *  )then
c
c      A new size...
c
c            write(*,1092)
             allocate_needed=.true.
             if(allocated(mapdata)
     *         )then
c                   write(*,1091)
                    deallocate(mapdata)
c                   write(*,1101)
             endif
      endif
c
      if(allocate_needed
     *  )then
c
c      Allocate mapdata
c
c            write(*,109)winwidth, winheight
             allocate (mapdata(winwidth,winheight))
c            write(*,110)winwidth, winheight
             winwidth_old =winwidth
             winheight_old=winheight
      endif
c
      call x11getimage(%val(winx1),   %val(winy1)
     *                ,%val(winwidth),%val(winheight))
      write(*,111)winx1,winy1,winwidth, winheight
c
      npixelold=-1
      nsame=0
c
      do i=0,winwidth-1
         do j=0,winheight-1
c           write(*,112)i,j
            call x11getpel(npixel,%val(i),%val(j))
c           write(8,113)npixel,i,j
c           call flush(8)
            if(npixel.gt.16777215.or.
     *         npixel.lt.0
     *        )then
                   write(22,101)npixel,i,j
                   write( 0,101)npixel,i,j
                   call isoflush(22)
               else
c
c      Is this pel the same as the last one?
c
                   if(npixel.ne.npixelold
     *               )then
c
c      ....it's not the same, so we have to see if we've
c      seen it before....
                          npixelold=npixel
c
c      Is this a valid pel?
c
                          nuwinpel=.true.
                          if(nwinpels.le.1024
     *                      )then
                                 nwp=nwinpels
                             else
                                 nwp=1024
                          endif
c
c      Have we seen it before?
c
                          do nw=1,nwp
                             if(winpels(nw).eq.npixel
     *                         )then
                                    n_pel_colour=nw
                                    nuwinpel=.false.
                                    exit
                             endif
                          enddo
c
c      If so, record it...
c
                           if(nuwinpel
     *                       )then
                                  nwinpels=nwinpels+1
                                  winpels(nwinpels)=npixel
                                  call isoflush(20)
                                  if(nwinpels.ge.256
     *                              )then
                                    write(20,1001)nw,npixel
                                     else
c
c      ...and grab its colour values for the colour map
c
                                         nw=nwinpels
                                         n_pel_colour=nw
                                         mx=4*nw
                                         call x11qcmap(%val(npixel)
     *                                         ,nred,ngreen,nblue)
                                         rgb(mx+1)=nblue/256
                                         rgb(mx+2)=ngreen/256
                                         rgb(mx+3)=nred/256
                                         rgb(mx+4)=0
                 write(20,100 )nw,npixel,(rgb(mx+nx),nx=1,3)
                                  endif
                           endif
                      else
                          nsame=nsame+1
                   endif
            endif
c
            mapdata(i+1,j+1)=char(n_pel_colour)
c
         enddo
      enddo 
c
c       Write out stats
c
      write(20,107)nwinpels,100.*real(nsame)/real(winheight*winwidth)
      write( *,107)nwinpels,100.*real(nsame)/real(winheight*winwidth)
      call isoflush(20)
c
c      Write the colour-map, and the bitmap
c
      rgbquad=rgbq
      call array2bmp(winwidth,winheight,mapdata)
c
      winwidth=winwtmp
      return
c
100   format('MAKEMAP: Colour',i5,': Pixel=',z8
     *      ,' Red=',z4,' Green=',z4,' Blue=',z4)
1001  format('MAKEMAP: Cannot capture ',i5,': Pixel=',z8)
101   format('MAKEMAP: Bad value pixel',z8,' found at',2i5)
102   format('MAKEMAP: About to get the pixel values')
103   format('MAKEMAP: About to get the colour map')
104   format('MAKEMAP: About to get the image (X,Y,dX,dY)'
     *      ,'from the screen',4i5)
105   format(a1024)
106   format('MAKEMAP: Look for details for pixel '
     *       ,z8,', NWINPELS=',i12)
1061  format('MAKEMAP: Cannot capture pixel ',i12)
107   format(/i4,' colours captured - '
     *      ,f5.1,'% of pixels were repeat values')
108   format('MAKEMAP: Writing bitmap ',a)
109   format('MAKEMAP:    Allocating memory:',2i8)
1091  format('MAKEMAP: De-allocating memory')
1092  format('MAKEMAP: New size for mapdata')
110   format('MAKEMAP:    Allocated....     ',2i8)
1101  format('MAKEMAP: De-allocated....     ')
111   format('MAKEMAP:  Got image',4i9)
112   format('MAKEMAP: Getting pixel at',2i6)
113   format('MAKEMAP: Pixel',z8,' found at',2i5)
c
      end
