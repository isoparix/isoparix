      program timanal
c
c      Analyses time stanmps from taperep* programs
c
      implicit real (8) (a-h,o-z)
c
      parameter(ndmax=200)
c
      dimension nactive (0:86440),rate(0:86440)
     *          ,totrate(0:ndmax),ndcount(0:ndmax)
c
      do i=0,86440
         rate(i)   =0.
         nactive(i)=0
      enddo
      ixmin=100000
      ixmax=0
c
  1   continue
c
c      Read the input data from STDIN - ie cat *.times | ./timanal.exe
c
      read(*,102,end=2,err=98,iostat=ios)txfer1,txfer2,trew1,trew2
c
c      Get nearest integers
c
      ix1=txfer1+.5
      ix2=txfer2+.5
      ir1=trew1 +.5
      ir2=trew2 +.5
c
c      Check input
c
      if(ix1.eq.0)go to 1
c
c      What was the mean activity rate?
c
      ratej=1./(txfer2-txfer1)
c
c      Map active times and activity rates to say how many drives were 
c      active now, and how busy all the system was.
c
      do i=ix1,ix2
         nactive(i)=nactive(i)+1
         rate(i)=rate(i)+ratej
      enddo
c
      if(ix1.lt.ixmin)then
c                         write(*,105)ixmin,ix1,txfer1
                          ixmin=ix1
      endif
c
      if(ix2.gt.ixmax)then
c                         write(*,106)ixmax,ix2,txfer2
                          ixmax=ix2
      endif
c
      go to 1
c
  2   continue
c
      do n=0,ndmax
         ndcount(n)=0
         totrate(n)=0.
      enddo
c
      maxden=0
      minden=1000000
c
c      Average all the rates recorded against the densities
c
      do i=ixmin,ixmax
         if(nactive(i).gt.ndmax
     *     )then
                write(*,101)i,nactive(i)
            else
                if(nactive(i).gt.maxden)maxden=nactive(i)
                if(nactive(i).lt.minden)minden=nactive(i)
                ndcount(nactive(i))=ndcount(nactive(i))+1
                totrate(nactive(i))=totrate(nactive(i))+rate(i)
c               write(*,101)i,nactive(i),rate(i)
         endif
      enddo
c
c      Write out mean densities
c
      do n=minden,maxden
         totrate(n)=totrate(n)/float(ndcount(n))
         write(*,103)n,totrate(n),ndcount(n)
      enddo
c
      stop
c
c      Handle errors
c
 98   continue
      write(8,100)txfer1,txfer2,trew1,trew2
      go to 1
      
c
100   format('***** TIMANAL: ERROR reading data',4f11.3)
101   format(2i10,f10.3)
102   format(10x,f10.3,3x,f10.3,25x,f10.3,3x,f10.3)
103   format(i10,f10.4,i10)
105   format('Previous min=',i10,', new min=',i10,' from',f10.3)
106   format('Previous max=',i10,', new max=',i10,' from',f10.3)
c
      end
