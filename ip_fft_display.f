      subroutine ip_fft_display(nxdim,nydim,c)
c
      use bmp_comms
      use ip_comms
c
c      Display a complex array (c) in the colour map established
c      elsewhere
c
      character(1)tpic(nxdim,nydim)
      character(30)title
c
      complex(8)c(nxdim,nydim)
      real(8)r,realmax,imagmax,absmax,realmin,imagmin,absmin
      real(8)topreal,topimag,topabs,toplevel
      real(8)real_scale,imag_scale,abs_scale,t
c
      integer ncount(0:255)
c
      title=bmname
c
      toplevel=254.0
c
      realmax=maxval(real (c))
      imagmax=maxval(dimag(c))
      absmax =maxval(cdabs(c))
c
      realmin=minval(real (c))
      imagmin=minval(dimag(c))
      absmin =minval(cdabs(c))
c
      topreal=dmax1(dabs(realmax),dabs(realmin))
      topimag=dmax1(dabs(imagmax),dabs(imagmin))
      topabs =dmax1(dabs(absmax) ,dabs(absmin))
c
      if(topreal.ne.0.0
     *  )then
             real_scale=toplevel/topreal
         else
             real_scale=1.0
      endif
c
      if(topimag.ne.0.0
     *  )then
             imag_scale=toplevel/topimag
         else
             imag_scale=1.0
      endif
c
      if(topabs.ne.0.0
     *  )then
             abs_scale=toplevel/topabs
         else
             abs_scale=1.0
      endif
c
      write(*,100)bmname,nxdim,nydim
      write(*,101)realmax,imagmax,absmax
      write(*,102)realmin,imagmin,absmin
      write(*,103)real_scale,imag_scale,abs_scale
c
c      Magnitude
c
      tpic=char(int(real_scale*(dabs(real(c)))))
      ncount=0
      bmname=trim(title)//'_mag'
      call array2bmp(nxdim,nydim,tpic)
c
c      Phase
c
      tpic=char(int(imag_scale*(dabs(dimag(c)))))
      ncount=0
      bmname=trim(title)//'_phase'
      call array2bmp(nxdim,nydim,tpic)
c
c      Absolute
c
c     write(*,*)minval(int(abs_scale*cdabs(c)))
c    *         ,maxval(int(abs_scale*cdabs(c)))
c
      ncount=0
      ntotal=0
      do j=1,nydim
         do i=1,nxdim
            npixel=0.5+int(abs_scale*cdabs(c(i,j)))
            ncount(npixel)=ncount(npixel)+1
            ntotal=ntotal+npixel
         enddo
      enddo
c
      nlim=0.5+int(float(ntotal)*0.99)
      do n=0,255
         write(90,104)n,ncount(n)
      enddo
c
      nsum=0
      npcount=0
      do n=0,255
         nsum=nsum+ncount(n)
         npcount=npcount+(ncount(n)*n)
         write(92,104)n,nsum
         if(npcount.gt.nlim
     *     )then
                ta=(255.0**2)/float(n)
c               write(*,*)n,npcount,nsum,nlim
                abs_scale=ta/topabs
                exit
         endif
      enddo
c
      do j=1,nydim
         do i=1,nxdim
            m=int(abs_scale*cdabs(c(i,j)))
            if(m.gt.255)m=255
            tpic(i,j)=char(m)
         enddo
      enddo
c
      bmname=trim(title)//'_abs'
      call array2bmp(nxdim,nydim,tpic)
c
100   format(/a30,2i8/47x,'Real',21x,'Imaginary',22x,'Absolute')
101   format(23x,'Maxima:',3e30.20)
102   format(23x,'Minima:',3e30.20)
103   format(17x,'Scale factor:',3e30.20,/)
104   format(i4,',',i8)
c
      return
c
      end
