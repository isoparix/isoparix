      program border2
c
c      Counts black pixels
c
      character (80) txtdata
      character (32) txtenum
      character (1) txt(80)
c
      equivalence(txt,txtdata)
c
      character(1),allocatable,dimension(:,:) :: picdata
c
      dimension ix(2000000),iy(2000000),nval(0:255)
c
      logical landscape,clearline
c
      ix=0
      iy=0
c
      margin=20
      nxmax=0
      nxmin=1000000
      nymax=0
      nymin=1000000
c
c      Get picture dimensions
c
cIMG_2164.jpg JPEG 2304x3072 2304x3072+0+0 DirectClass 8-bit 3.23167mb 1.232u 0:02
      read(7,109,iostat=ios)txt
      n=0
      i=1
  1   continue
      if(txt(i).eq.'G'.and.txt(i+1).eq.' '
     *  )then 
             i=i+1
  4          continue
             i=i+1
             n=n+1
             txt(n)=txt(i)
             if(txt(n).eq.'x')txt(n)=' '
             if(txt(i).ne.' ')go to 4
         else
             i=i+1
             if(i.lt.81
     *         )then
                    go to 1
                else
                    write(*,115)txt
                    stop
             endif
      endif
c
c     write(*,*)txtdata
      read(txtdata,*)ixdim,iydim
      write(*,110)ixdim,iydim
      allocate(picdata(0:ixdim-1,0:iydim-1))
c
      open(1,file='pic.dat',form='formatted'
     *       ,access='stream',status='old')
      read(1,113,advance='NO',iostat=irc
     *          ,eor=6,err=98)picdata
  6   continue
c
      nxmax=ixdim-1
      nymax=iydim-1
      nxmin=0
      nymin=0
c
      npoints=0
c
c      condition borders...
c
      rxd=100./float(ixdim)
      ryd=100./float(iydim)
      nthresh=5
c
      do nx=0,200
         nval=0
         do ny=0,iydim-1
            nval(ichar(picdata(nx,ny)))=nval(ichar(picdata(nx,ny)))+1
         enddo
         call density(nval,average,nvalmax,nvalind,iydim
     *               ,clearline,fblack)
c        write(*,114)nx,nvalind,nvalmax,average
c    *              ,fblack,clearline,(nval(mx),mx=0,30)
         if(clearline)picdata(nx,:)=char(0)
      enddo
c
      do nx=ixdim-1,ixdim-201,-1
         nval=0
         do ny=0,iydim-1
            nval(ichar(picdata(nx,ny)))=nval(ichar(picdata(nx,ny)))+1
         enddo
         call density(nval,average,nvalmax,nvalind,iydim
     *               ,clearline,fblack)
c        write(*,114)nx,nvalind,nvalmax,average
c    *              ,fblack,clearline,(nval(mx),mx=0,30)
         if(clearline)picdata(nx,:)=char(0)
      enddo
c
      do ny=0,200
         nval=0
         do nx=0,ixdim-1
            nval(ichar(picdata(nx,ny)))=nval(ichar(picdata(nx,ny)))+1
         enddo
         call density(nval,average,nvalmax,nvalind,ixdim
     *               ,clearline,fblack)
c        write(*,116)ny,nvalind,nvalmax,average
c    *              ,fblack,clearline,(nval(mx),mx=0,30)
         if(clearline)picdata(:,ny)=char(0)
      enddo
c
      do ny=iydim-1,iydim-201,-1
         nval=0
         do nx=0,ixdim-1
            nval(ichar(picdata(nx,ny)))=nval(ichar(picdata(nx,ny)))+1
         enddo
         call density(nval,average,nvalmax,nvalind,ixdim
     *               ,clearline,fblack)
c        write(*,116)ny,nvalind,nvalmax,average
c    *              ,fblack,clearline,(nval(mx),mx=0,30)
         if(clearline)picdata(:,ny)=char(0)
      enddo
c
      do ny=0,iydim-1
         do nx=0,ixdim-1
c           write(*,*)nx,ny,ichar(picdata(nx,ny))
            if(ichar(picdata(nx,ny)).eq.0
     *        )then
                   npoints=npoints+1
                   ix(npoints)=nx
                   iy(npoints)=ny
            endif
         enddo
      enddo
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
      nleft=0	! Number left
      nright=0	! Number right
      nbottom=0	! Number bottom
      ntop=0	! Number top
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
                if(iy(i).gt.nymean
     *            )then
                       if(fx.lt.xa.and.fx.gt.xb
     *                   )then
                              ntop=ntop+1
c                             write(*,1041)fx,fy,xa,ya,xb,yb
                              if(iy(i).lt.nytop)nytop=iy(i)
                       endif
                   else
                       if(fx.gt.xa.and.fx.lt.xb
     *                   )then
                              nbottom=nbottom+1
c                             write(*,1042)fx,fy,xa,ya,xb,yb
                              if(iy(i).gt.nybot)nybot=iy(i)
                       endif
                endif
            else
                if(ix(i).lt.nxmean
     *            )then
                       if(fy.gt.ya.and.fy.lt.yb
     *                   )then
                              nleft=nleft+1
c                             write(*,1043)fx,fy,xa,ya,xb,yb
                              if(ix(i).gt.nxleft )nxleft= ix(i)
                       endif
                   else
                       if(fy.lt.ya.and.fy.gt.yb
     *                   )then
                              nright=nright+1
c                             write(*,1044)fx,fy,xa,ya,xb,yb
                              if(ix(i).lt.nxright)nxright=ix(i)
                       endif
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
                              nright=nright+1
                              if(ix(i).lt.nxright)nxright=ix(i)
                          else
                              nleft=nleft+1
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
                       if(iy(i).gt.nymean
     *                   )then
                              ntop=ntop+1
                              if(iy(i).lt.nytop)nytop=iy(i)
                          else
                              nbottom=nbottom+1
                              if(iy(i).gt.nybot)nybot=iy(i)
                       endif
                endif
             enddo
      endif
c
      write(*,107)nleft,nright,nbottom,ntop,landscape
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
 98   continue
      write(*,101)
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
107   format(/'Numbers: Left=',i8,', Right=',i8
     *       ,', Bottom=',i8,', Top=',i8,', Landscape=',L1)
108   format('convert -crop ',i0,'x',i0,'+',i0,'+',i0
     *      ,' %1.jpg %1_crop.jpg')
109   format(80a1)
110   format('Picture dimensions are ',i0,'x',i0)
111   format(i8,' black pixels located - current line is',i8)
112   format(/i8,' black pixels located - all pixels read')
113   format(10000000(a1$))
114   format('X=',i5,i4,i5,2f6.1,l2,' ',40z3)
115   format('ERROR in strem identity text: ',80a1)
116   format('Y=',i5,i4,i5,2f6.1,l2,' ',40z3)
c
      end
c
c
c
      subroutine density
     *             (nval,average,nvalmax,nvalind,izdim,clearline,fblack)
c23456789012345678901234567890123456789012345678901234567890123456789012
c        1         2         3         4         5         6         7         
c
c   
c
      dimension nval(0:255)
c
      logical clearline
c
      nvalmax=0
      average=0.
c
      nblack=0
      do i=0,5
         nblack=nblack+nval(i)
      enddo
      fblack=float(100*nblack)/float(izdim)
c
      do i=0,255
         average=average+(float(i*nval(i)))
         if(nval(i).gt.nvalmax
     *     )then
                nvalmax=nval(i)
                nvalind=i
         endif
      enddo
c
      average=average/float(izdim)
c
c      Normalise nval to FF
c
      a=255./float(nvalmax)
      do i=0,255
         nval(i)=.5+(a*float(nval(i)))
      enddo
c
      if(average.lt.60.0
     *  )then
             clearline=.true.
         else
             clearline=.false.
      endif
c
      return
c
      end
