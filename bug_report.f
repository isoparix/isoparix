      program bug_report
c
      real (8) zreal,zimag,cra,cia,zimaga,zt,zlim,zreal2,zimag2
c
c     zreal =sr(ix)
c     zimag =si(iy)
c     cra=cr(ix)
c     cia=ci(iy)
c
c     Do the minimum number of loops...
c
  1   continue
      read(86,end=2)ix,iy,miniter,zreal,zimag,cra,cia
      write(*,*)ix,iy,miniter,zreal,zimag,cra,cia
c
      do i=1,miniter
c
      write(*,*)i,miniter,zreal,zimag
         zimaga=(zimag*zimag)-cra
         zt =(zreal*zimag)+cia
         zimag =(zreal*zimag)+zt
         zreal =(zreal*zreal)-zimaga
c
      enddo
      go to 1
c
  2   continue
c
100   format('ERROR IN LOOPER: Unallocated mset',i10)
101   format('LOOPER: LIM=',i7,', IX=',i7,', IY=',i7,', MS=',i7)
102   format('LOOPER: MIN=',i7,', IX=',i7,', IY=',i7)
103   format('LOOPER: Pre ',4f10.6)
104   format('LOOPER: Post',i7,', IX=',i7,', IY=',i7)
      end
