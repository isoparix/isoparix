      program border4
c
c      Counts black pixels
c
      implicit real(8) (a-h,o-z)
c
      character (80) txtdata,slidename
      character (32) txtenum
      character (1) txt(80)
c
      equivalence(txt,txtdata)
c
      character(1),allocatable,dimension(:,:) :: picdata
c
      integer,allocatable,dimension(:) :: 
     *                                lineleft,lineright,linetop,linebot
c
      logical landscape
c
c      Allocate arrays
c
      open(12,file='border.log',status='unknown',form='formatted'
     *       ,position='append')
      nedge=200
      allocate(  lineright(0:nedge-1))
      allocate(  lineleft (0:nedge-1))
      allocate(  linetop  (0:nedge-1))
      allocate(  linebot  (0:nedge-1))
c
c      Get picture dimensions
c
cIMG_2164.jpg JPEG 2304x3072 2304x3072+0+0 DirectClass 8-bit 3.23167mb 1.232u 0:02
      read(7,109,iostat=ios,err=98)txt
      n=0
      i=1
  1   continue
      if(txt(i).eq.'G'.and.txt(i+1).eq.' '
     *  )then 
             write(12,100)(txt(mx),mx=1,i)
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
      if((nxmax-nxmin).gt.(nymax-nymin)
     *  )then
             landscape=.true.
         else
             landscape=.false.
      endif
c
c      Examine borders...
c
      rxd=1./float(ixdim)
      ryd=1./float(iydim)
c
      nline=0
      do nx=0,nedge-1
         linesum=0.
         do ny=0,iydim-1
            linesum=linesum+ichar(picdata(nx,ny))
         enddo
         lineleft(nline)=linesum
         nline=nline+1
      enddo
c
      nline=0
      do nx=nxmax,ixdim-nedge,-1
         linesum=0.
         do ny=0,iydim-1
            linesum=linesum+ichar(picdata(nx,ny))
         enddo
         lineright(nline)=linesum
         nline=nline+1
      enddo
c
      nline=0
      do ny=0,nedge-1
         linesum=0.
         do nx=0,ixdim-1
            linesum=linesum+ichar(picdata(nx,ny))
         enddo
         linetop(nline)=linesum
         nline=nline+1
      enddo
c
      nline=0
      do ny=nymax,iydim-nedge,-1
         linesum=0.
         do nx=0,ixdim-1
            linesum=linesum+ichar(picdata(nx,ny))
         enddo
         linebot(nline)=linesum
         nline=nline+1
      enddo
c
c      View the averages
c
      nroll=9
c
c23456789012345678901234567890123456789012345678901234567890123456789012
c        1         2         3         4         5         6         7         
      call view_avg('LEFT ',lineleft ,nedge,nroll,iydim,nxleft, al,bl)
      call view_avg('RIGHT',lineright,nedge,nroll,iydim,nright, ar,br)
      call view_avg('TOP  ',linetop  ,nedge,nroll,ixdim,nytop,  at,bt)
      call view_avg('BOT  ',linebot  ,nedge,nroll,ixdim,nbottom,ab,bb)
c
      nxright=nxmax-nright
      nybot  =nymax-nbottom
      ia=1+nxright-nxleft
      ib=1+nybot-nytop
c
      write(12,107)nxleft,nxright,nytop,nybot,landscape,nroll
      write(12,117)al,ar,at,ab
      write(12,118)bl,br,bt,bb
      write(12,105)nxmin, nxmax,  nymin,nymax
     *           ,float(1+nxmax-nxmin)/float(1+nymax-nymin)
      write(12,106)nxleft,nxright,nytop,nybot,ia,ib,float(ia)/float(ib)
c
      write(*,107)nxleft,nxright,nytop,nybot,landscape,nroll
      write(*,117)al,ar,at,ab
      write(*,118)bl,br,bt,bb
      write(*,105)nxmin, nxmax,  nymin,nymax
     *           ,float(1+nxmax-nxmin)/float(1+nymax-nymin)
      write( *,106)nxleft,nxright,nytop,nybot,ia,ib,float(ia)/float(ib)
c
      open(10,file='crop.bat',status='unknown',form='formatted')
      write(10,108)ia,ib,nxleft,nytop
c
      stop
c
 98   continue
      write(*,101)txt
      stop
c
100   format(/80a1)
101   format('border_f.exe ERROR in read:',80a1)
102   format('X,',i6,',',i6)
103   format('Y,',i6,',',i6)
1041  format('Top  : ',6f8.1)
1042  format('Bot  : ',6f8.1)
1043  format('Left : ',6f8.1)
1044  format('Right: ',6f8.1)
105   format('Original frame:',4i5,10x,f6.3)
106   format('     New frame:',6i5,f6.3,i8,/)
107   format(/'Maxima:     Left=',i8,',  Right=',i8
     *       ,',    Top=',i8,', Bottom=',i8,', Landscape=',L1
     *       ,', Roll=',i4)
108   format('convert -crop ',i0,'x',i0,'+',i0,'+',i0
     *      ,' %1.jpg %1.jpg')
c    *      ,' %1.jpg %1_crop.jpg')
109   format(80a1)
110   format('Picture dimensions are ',i0,'x',i0)
113   format(10000000(a1$))
114   format('X=',i5,i4,i5,2f6.1,l2,' ',40z3)
115   format('border_f.exe ERROR in stream identity text: ',80a1)
117   format('Slope:  ',4f17.4)
118   format('EdgeAvg:',4f17.4)
c
      end
c
c
c
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine view_avg(title,linesum,nedge,nroll,ldim,lmax
     *           ,speak,edge_avg)
c
c      Looks at all the averages, calcs first and second derivatives
c
      implicit real(8) (a-h,o-z)
c
      character(5) title
      dimension linesum(0:nedge-1)
c
      a=1./float(nroll*ldim)
      b=1./float(ldim)
      k=nroll/2
      z=-huge(z)
      speak=z
      avgold=z
      avgmax=z
      rnroll=1./float(nroll)
      lmax=0
c
      do i=0,nedge-1
         if(i.lt.nroll-1
     *     )then
c               write(*,100)title,i,linesum(i)
            else
                nrollsum=0
                do j=i,i-nroll+1,-1
                   nrollsum=nrollsum+linesum(j)
                enddo
                avg=a*float(nrollsum)
                if(avgold.gt.z
     *            )then
                       deriv1=avg-avgold
                       if(deriv1.gt.speak
     *                   )then
                              if(speak.gt.1.0
     *                          )then
                                     lmax=i-k
                                     edge_avg=b*float(linesum(lmax))
                              endif
                              speak=deriv1
c                             write(*,101)title,i,linesum(i),nrollsum
c    *                            ,lmax,avg,speak
                          else
c                             write(*,109)title,i,linesum(i),nrollsum
c    *                            ,i-k,avg,deriv1
                       endif
                   else
c                      write(*,109)title,i,linesum(i),nrollsum,i-k,avg
                endif
                avgold=avg
         endif
      enddo
c
      return
c
100   format(a5,i5,3i10,2f10.3)
101   format(a5,i5,3i10,2f10.3,' - max slope')
      end
c
c23456789012345678901234567890123456789012345678901234567890123456789012
c        1         2         3         4         5         6         7         
