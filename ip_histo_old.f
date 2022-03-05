      subroutine ip_histo
c
c     Displays the histograms (with and without the zero-count) 
c     of the data in a character array
c
c      The character array picdata has values between 0 and 255.
c
      use bmp_comms
c
      use ip_comms
c
      logical draw
      integer(4)icdf(0:255)
c
c      Count the numbers in each level
c
      draw=.true.
c
      nonzero_pels=0
      jval=0
      kval=0
      nval=0
      levelmax=0
      levelmin=1000000000
      moment=0
      max_nonzero=0
      do iy=0,iydim-1
         do ix=0,ixdim-1
            level=ichar(picdata(ix,iy))
            if(level.gt.levelmax)levelmax=level
            if(level.lt.levelmin)levelmin=level
            nval(level)=nval(level)+1
            if(level.ge.nthresh
     *        )then
                   moment=moment+level
                   nonzero_pels=nonzero_pels+1
            endif
         enddo
      enddo
      ncount0=(ixdim*iydim)-nonzero_pels
c
      npels=0
      mval=0
      nz=nonzero_pels/2
      do n=255,0,-1
         if(n.ge.nthresh
     *     )then
                npels=npels+nval(n)
                mval(n)=npels ! Cumulative distribution function above nthresh
                if(npels.lt.nz)median=n
         endif
      enddo
c
      balance=float(moment)/float(nonzero_pels)
c
c      Set percentages in each level
c
      a=255./float(maxval(nval))
c
      maxbar=maxval(nval(nthresh:255))
      if(maxbar.le.0
     *  )then
             b=0.0
         else
             b=255./float(maxbar)
      endif
c
      c=255./float(maxval(mval))
c
      do i=0,255
         jval(i)=0.5+(255.-(a*float(nval(i))))
      enddo
c
      kval=255
      lval=255
      do i=255,nthresh,-1
         kval(i)=0.5+(255.-(b*float(nval(i))))
      enddo
c
      maxnzp=0
      do i=255,nthresh,-1
         lval(i)=0.5+(255.-(c*float(mval(i))))
         if(lval(i).ne.255.and.
     *      maxnzp.eq.0
     *     )then
                maxnzp=i     ! latest zero cell in CDF
         endif
      enddo
      kval(0)=0
c
c      Write out the values
c
      histo_name=''
      write(histo_name,102)nhisto
      open(2,file=histo_name,form='formatted',status='unknown')
      write(*,119)nhisto,ncount0,levelmin,levelmax
     *            ,maxnzp,maxbar,balance,median
      write(2,119)nhisto,ncount0,levelmin,levelmax
     *            ,maxnzp,maxbar,balance,median
      do iy=0,15
         iya=iy*16
         iyb=iya+15
         write(2,101)iya,nval(iya:iyb)
         enddo
      write(2,100)nhisto,jval,kval,lval
      close(2)
      call isoflush(2)
c
      kval(0:nthresh-1)=255.
      nh=(nhisto*265)+3
c
c      Draw frame...
c
      if(draw
     *  )then
c
             histarray(  1:258,     nh-  2)=0
             histarray(  1:258,     nh+257)=0
             do n=2,258,10
                 histarray(n,nh+258:nh+259)=0 ! Tick marks at 0,10,...
             enddo
             histarray(      1,nh-2:nh+257)=0
             histarray(    258,nh-2:nh+257)=0
c
             histarray(265:522,     nh-  2)=0
             histarray(265:522,     nh+257)=0
             do n=266,522,10
                 histarray(n,nh+258:nh+259)=0 ! Tick marks at 0,10,...
             enddo
             histarray(    265,nh-2:nh+257)=0
             histarray(    522,nh-2:nh+257)=0
c
             histarray(529:786,     nh-  2)=0
             histarray(529:786,     nh+257)=0
             do n=530,786,10
                 histarray(n,nh+258:nh+259)=0 ! Tick marks at 0,10,...
             enddo
             histarray(    529,nh-2:nh+257)=0
             histarray(    786,nh-2:nh+257)=0
c
c      Place values in histogram array
c
             nh_current=nh
             do ix=0,255
c
                ixj=ix+2
                do iy=1+jval(ix),255
                   histarray(ixj,nh+iy)=0
                enddo
c
                ixk=ix+266
                do iy=1+kval(ix),255
                   histarray(ixk,nh+iy)=0
                enddo
c
                ixk=ix+530
                do iy=1+lval(ix),255
                   histarray(ixk,nh+iy)=0
                enddo
c
             enddo
      endif
c
      nhisto=nhisto+1
c
      return
c
100   format(i10,3(/16(16i8,/)))
101   format(i10,':',16i7)
102   format('histo_value_',i2.2)
103   format('Transfer function',16(/,16f8.4))
104   format(10i10)
119   format('NHISTO:',i3,', NCOUNT0:',i10,', Levelmin:',i4
     *      ,', Levelmax:',i4,', MAXNZP:',i4
     *      ,', MAXBAR:',i10,', Balance:',f7.2,', MEDIAN:',i4)
c
      end
