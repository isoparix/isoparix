      program colcut
c
      use bmp_comms
      use ip_comms
c
c      Gets jpg pixels, modifies a column, and writes bitmap
c
      character(1)rgb(0:1023)
      character(1024)rgbq
      equivalence(rgb,rgbq)
c
      integer,dimension(30) :: ncolumn, nwidth,local
c
      nc=0
c     open(3,file='colwidths.dat',status='old',form='formatted')
  1   continue
      nc=nc+1
      read(3,*,end=2)ncolumn(nc),nwidth(nc)
      write(*,100) n,ncolumn(nc),nwidth(nc)
      go to 1
  2   continue
      nc=nc-1
c
      call getpic
c
      do nx=0,ixdim-1
         write(22,104)nx,sum(ipic(nx,:))
      enddo
c
      njump=0
      do nx=1,nc
         write(*,101)nx,nc,ncolumn(nx)
         ix =ncolumn(nx)-njump
         idx=nwidth(nx)
         grade=1./float(2*nwidth(nx))
         do iy=0,iydim-1
            local_min=1000
            do mx=ix-10,ix+10
               if(ipic(mx,iy).lt.local_min
     *           )then
                      local_min=ipic(mx,iy)
                      ixmin=mx
               endif
            enddo
            write(20,102)iy,ix,ixmin,ipic(ixmin-10:ixmin+10,iy)
            nv1=0.1*float(sum(ipic(ixmin-10:ixmin-1,iy)))
            nv2=0.1*float(sum(ipic(ixmin+1:ixmin+10,iy)))
            dav=float(nv2-nv1)*grade
            ax=0
            iadd=1
            do ixa=ixmin-idx,ixmin+idx
               newval=nv1+int(ax*dav)
               write(20,104)nv1,newval,nv2
c
               if(ixa.lt.ixmin
     *           )then
                      picdata(ixa,iy)=char(newval+iadd)
                      ax=ax+1.0
               endif
c
               iadd=-iadd
               if(ixa.gt.ixmin
     *           )then
                      picdata(ixa-1,iy)=char(newval+iadd)
                      ax=ax+1.0
               endif
c
            enddo
         enddo 
         njump=njump+1
      enddo 
c
      bmname='result'
      call array2bmp(ixdim,iydim,picdata)
c
      stop
c
100   format('Column',i3,': X-coord',i4,', width',i3)
101   format('Processing column',i3,' of',i3,i6)
102   format('IY=',i5,', IX (nom)=',i5
     *               ,', IX (min)=',i5,', Values:',30i4)
103   format(/)
104   format(12i10)
c
      end
