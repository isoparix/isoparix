      program border2
c
c      Counts black pixels
c
c# ImageMagick pixel enumeration: 2304,3072,255,RGB
c75,0: (  0,  0,  0) black
c77,0: (  0,  0,  0) black
c78,0: (  0,  0,  0) black
c80,0: (  0,  0,  0) black
c100,0: (  0,  0,  0) black
c101,0: (  0,  0,  0) black
c2102,1098: (  0,  0,  0) black
c
      character (24) txtdata
      character (32) txtenum
      character (1) txt(24)
c
      equivalence(txt,txtdata)
c
      dimension ix(100000),iy(100000)
c
      logical landscape
c
      ix=0
      iy=0
c
      margin=20
c
c      Get picture dimensions
c
      read(*,109,iostat=ios,end=2,err=3)txtenum,txt
      do i=1,24
         nc=ichar(txt(i))
         if(nc.lt.48.or.nc.gt.57
     *     )then
                txt(i)=' '
         endif
      enddo
      read(txtdata,*)ixdim,iydim
      write(*,110)ixdim,iydim
c
      nxmax=ixdim-1
      nymax=iydim-1
      nxmin=0
      nymin=0
c
      npoints=0
  1   read(*,100,iostat=ios,end=2,err=3)txt
      do i=1,24
         nc=ichar(txt(i))
         if(nc.lt.48.or.nc.gt.57
     *     )then
                txt(i)=' '
         endif
      enddo
c
      read(txtdata,*)nx,ny,ir,ig,ib
      if(ir+ig+ib.gt.0
     *  )then
c
c      Colour of this pixel is not black
c
             go to 1
      endif
c
      npoints=npoints+1
      if(mod(npoints,5000).eq.0)write(*,111)npoints,ny
      ix(npoints)=nx
      iy(npoints)=ny
c
      go to 1
c
  3   continue
      write(*,101)
c
  2   continue
      write(*,112)npoints
      if((nxmax-nxmin).gt.(nymax-nymin)
     *  )then
             landscape=.true.
         else
             landscape=.false.
      endif
c
      nxleft =0
      nxright=nxmax
      nybot=0
      nytop=nymax
c
      nxmean=(nxmax+nxmin)/2
      nymean=(nymax+nymin)/2
c
      fz=float(nymax)
      am=fz/float(nxmax)
      ram=1./am
      bm=-am
      rbm=-ram
c
      nlr=0	! Number left-right
      ntb=0	! Number top-bottom
c
      do i=1,npoints
         fx=float(ix(i))
         fy=float(iy(i))
         ya= am*fx
         xa=ram*fy
         yb=(bm*fx)+fz
         xb=rbm*(fy-fz)
c
         if(landscape
     *     )then
                if(fx.lt.xa.and.fx.gt.xb.and.iy(i).gt.nymean
     *            )then
                       ntb=ntb+1
c                      write(*,1041)fx,fy,xa,ya,xb,yb
                       if(iy(i).lt.nytop)nytop=iy(i)
                endif
c
                if(fx.gt.xa.and.fx.lt.xb.and.iy(i).lt.nymean
     *            )then
                       ntb=ntb+1
c                      write(*,1042)fx,fy,xa,ya,xb,yb
                       if(iy(i).gt.nybot)nybot=iy(i)
                endif
            else
                if(fy.gt.ya.and.fy.lt.yb.and.ix(i).lt.nymean
     *            )then
                       nlr=nlr+1
c                      write(*,1043)fx,fy,xa,ya,xb,yb
                       if(ix(i).gt.nxleft )nxleft= ix(i)
                endif
c
                if(fy.lt.ya.and.fy.gt.yb.and.ix(i).gt.nymean
     *            )then
                       nlr=nlr+1
c                      write(*,1044)fx,fy,xa,ya,xb,yb
                       if(ix(i).lt.nxright)nxright=ix(i)
                endif
         endif
c
      enddo
c
      if(landscape
     *  )then
             nytm=nytop-margin
             nybm=nybot+margin
             do i=1,npoints
                if(iy(i).lt.nytm.and.iy(i).gt.nybm
     *            )then
                       nlr=nlr+1
                       if(ix(i).gt.nxmean
     *                   )then
                              if(ix(i).lt.nxright)nxright=ix(i)
                          else
                              if(ix(i).gt.nxleft)nxleft=ix(i)
                       endif
                endif
             enddo
         else
             nxrm=nxright-margin
             nxlm=nxleft+margin
             do i=1,npoints
                if(ix(i).lt.nxrm.and.ix(i).gt.nxlm
     *            )then
                       ntb=ntb+1
                       if(iy(i).gt.nymean
     *                   )then
                              if(iy(i).lt.nytop)nytop=iy(i)
                          else
                              if(iy(i).gt.nybot)nybot=iy(i)
                       endif
                endif
             enddo
      endif
c
      write(*,107)ntb,nlr,landscape
      write(*,105)nxmin, nxmax,  nymin,nymax
     *           ,float(nymax-nymin)/float(nxmax-nxmin)
      write(*,106)nxleft,nxright,nybot,nytop
     *           ,float(nytop-nybot)/float(nxright-nxleft)
     *           ,npoints
      ia=nxright-nxleft
      ib=nytop-nybot
      open(10,file='crop.bat',status='unknown',form='formatted')
      write(10,108)ia,ib,nxleft,nybot
      stop
c
100   format(24a1)
101   format('ERROR in read')
102   format('X,',i6,',',i6)
103   format('Y,',i6,',',i6)
1041  format('Top  : ',6f8.1)
1042  format('Bot  : ',6f8.1)
1043  format('Left : ',6f8.1)
1044  format('Right: ',6f8.1)
105   format('Original frame:',4i8,f6.3)
106   format('     New frame:',4i8,f6.3,i8)
107   format(/'Numbers: Top-bottom=',i8,', Left-right=',i8
     *      ,', Landscape=',l1)
108   format('convert -crop ',i0,'x',i0,'+',i0,'+',i0
     *      ,' %1.jpg %1_crop.jpg')
109   format(a32,24a1)
110   format('Picture dimensions are ',i0,'x',i0)
111   format(i8,' black pixels located - current line is',i8)
112   format(/i8,' black pixels located - all pixels read')
c
      end


