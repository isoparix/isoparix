      program ip_tiler
c
      use bmp_comms
      use ip_comms
c
c      Gets jpg pixels, does stuff to them, and writes bitmap of tiles
c
c
      character(1),allocatable,dimension(:,:) :: picdatb
      real(8),     allocatable,dimension(:,:) :: aggregate
c
      character (80) txtdata
      character (1) txt(80),testchar(0:255)
c
      equivalence(txt,txtdata)
c
      character(1)rgb(0:1023)
      character(1024)rgbq
      equivalence(rgb,rgbq)
c
      do n=0,255
         m=n*4
         rgb(m  )=char(n)
         rgb(m+1)=char(n)
         rgb(m+2)=char(n)
         rgb(m+3)=char(0)
      enddo
      rgbquad=rgbq
      
c
c     do i=0,255
c        testchar(i)=char(i)
c        write(*,*)i,ichar(testchar(i))
c     enddo
c     stop
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
      npixels=(ixdim+1)*(iydim+1)
      ncell=float(npixels)/256.
      write(*,110)ixdim,iydim,npixels
      allocate(picdata   (0:ixdim-1,0:iydim-1))
      allocate(brightness(0:ixdim-1,0:iydim-1))
c
      picdata=char(0)
      ix=0
      iy=0
c
      levelmax=0
      levelmin=300
      bmax=0.0
      a=1./256.0
      open(1,file='pic.dat',form='unformatted'
     *       ,access='stream',status='old')
      read(1,iostat=irc,err=98)picdata
c
c     call ip_histo(ixdim,iydim,picdata,histarray,1
c    *              ,ncount0,levelmin,levelmax,level255,balance)
c
c      Create tiles
c
      brightness=ichar(picdata)
c
      read(*,*)lside
c     lside=3    ! Length of picked-out side
      ltilex=ixdim/lside
      ltiley=iydim/lside
      allocate(picdatb   (0:ltilex-1,0:ltiley-1))
      allocate(aggregate (0:ltilex-1,0:ltiley-1))
      aggregate=0.0
      do nyoffset=0,lside-1
         kyp=nyoffset*ltiley
         do nxoffset=0,lside-1
            kxp=nxoffset*ltilex
            write(*,117)kxp,kyp
            iyp=kyp
            do iy=0,ltiley-1
               iya=(iy*lside)+nyoffset
               ixp=kxp
               do ix=0,ltilex-1
                  ixa=(ix*lside)+nxoffset
                  picdata(ixp,iyp)=char(int(brightness(ixa,iya)+.5))
                  aggregate(ix,iy)=aggregate(ix,iy)+brightness(ixa,iya)
                  ixp=ixp+1
               enddo
               iyp=iyp+1
            enddo
         enddo
      enddo
c
      bmname='tiles_'//char(48+lside)
      call array2bmp(ixdim,iydim,picdata)
c
      aggregate=aggregate/float(lside**2)
      do iy=0,ltiley-1
         do ix=0,ltilex-1
            picdatb(ix,iy)=char(int(aggregate(ix,iy)+0.5))
         enddo
      enddo
      bmname='aggregate_'//char(48+lside)
      call array2bmp(ltilex,ltiley,picdatb)
c
      stop
 98   continue
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
110   format('Picture dimensions are ',i0,'x',i0,' - ',i10,' pixels')
111   format(i8,' black pixels located - current line is',i8)
112   format(/i8,' black pixels located - all pixels read')
113   format(a$)
114   format('X=',i5,i4,i5,2f6.1,l2,' ',40z3)
115   format('ERROR in strem identity text: ',80a1)
116   format('Y=',i5,i4,i5,2f6.1,l2,' ',40z3)
117   format(25i5)
118   format(3i6)
120   format(16(16i8,/))
c
      end
