      subroutine looper(ix,iy,miniter,iter)
c
      use isocomm
c
      real (8) zreal,zimag,cra,cia,zimaga,zt,zlim,zreal2,zimag2
c
      if(miniter.lt.0
     *  )then
             write(txtout,102)miniter,ix,iy
             call statout
      endif
c
      zreal =sr(ix)
      zimag =si(iy)
      cra=cr(ix)
      cia=ci(iy)
c
CXX   if(miniter.gt.0
CXX  *  )then
c
c     Do the minimum number of loops...
c
CXX           do i=1,miniter
c
CXX              zimaga=(zimag*zimag)-cra
CXX              zt =(zreal*zimag)+cia
CXX              zimag =(zreal*zimag)+zt
CXX              zreal =(zreal*zreal)-zimaga
c
CXX           enddo
CXX           iter=miniter
c
c      Now carry on until a point is found, or the limit is reached..
c
CXX   else
c
c      Or carry on until a point is found, or the limit is reached..
c
             iter=0
CXX   endif
c
      zlim=4.+cra
  2   continue
      zreal2=(zreal*zreal)+cra
      zimag2=zimag*zimag
      if(zreal2+zimag2.gt.zlim
     *  )then
             go to 3
      endif
      zt=(zreal*zimag)+cia
      zimag=(zreal*zimag)+zt
      zreal=zreal2-zimag2
      iter=iter+1
      if(iter.le.limit)go to 2
c
  3   continue
c
CXX   if(iter.lt.miniter.and.iter.gt.0
CXX  *  )then
CXX        write(txtout,102)subname,ix,iy,miniter,iter
CXX          call statout
CXX   endif
c
      mset(ix,iy)=iter
      if(mset(ix,iy).lt.0
     *  )then
             write(txtout,1011)limit,mset(ix,iy),ix,iy
             call statout
      endif
      iterxs=iterxs+iter-miniter
      return
c
100   format('ERROR IN LOOPER: Unallocated mset',i10)
101   format('MINITER=',i7,', ITER=',i7,', IX=',i7,', IY=',i7)
1011  format('MINITER=',i7,', MSET=',i7,', IX=',i7,', IY=',i7)
102   format('LOOPER:',a6,i5,i6,i14,i15)
103   format('LOOP:',2i5,4f10.6)
104   format('LOOPER: Post',i7,', IX=',i7,', IY=',i7)
      end
