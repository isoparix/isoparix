      program border
c
c      Counts black pixels
c
c75,0: (  0,  0,  0) black
c77,0: (  0,  0,  0) black
c78,0: (  0,  0,  0) black
c80,0: (  0,  0,  0) black
c100,0: (  0,  0,  0) black
c101,0: (  0,  0,  0) black
c102,0: (  0,  0,  0) black
c
      character (80) txtdata
      character (1) txt(80)
c
      equivalence(txt,txtdata)
c
      dimension ix(4000),iy(4000)
c
      ix=0
      iy=0
c
      nxmax=0
      nxmin=1000000
      nymax=0
      nymin=1000000
c
      nxbot=0
      nxtop=1000000
      nybot=0
      nytop=1000000
c
  1   read(*,100,iostat=ios,end=2,err=3)txt
      do i=1,80
         if(txt(i).eq.':'.or.
     *      txt(i).eq.'('.or.
     *      txt(i).eq.')'.or.
     *      txt(i).eq.','
     *     )then
                txt(i)=' '
         endif
      enddo
c     write(*,*)txt
      read(txtdata,*)nx,ny
c
      if(nx.gt.nxmax)nxmax=nx
      if(nx.lt.nxmin)nxmin=nx
      if(ny.gt.nymax)nymax=ny
      if(ny.lt.nymin)nymin=ny
c
c     write(*,*) nx,ny
      ix(nx)=ix(nx)+1
      iy(ny)=iy(ny)+1
      go to 1
c
  3   continue
      write(*,101)
c
  2   continue
c
      nxmean=(nxmax+nxmin)/2
      nymean=(nymax+nymin)/2
c
      write(*,*)nxmax,nxmean,nxmin,nymax,nymean,nymin
      do i= 1,4000
         if(ix(i).gt.0
     *      )then
                 write(8,102)i,ix(i)
                 if(i.gt.nxmean.and.i.lt.nxtop)nxtop=i
                 if(i.lt.nxmean.and.i.gt.nxbot)nxbot=i
         endif
      enddo
c
      do i= 1,4000
         if(iy(i).gt.0
     *      )then
                 write(10,103)i,iy(i)
                 if(i.gt.nymean.and.i.lt.nytop)nytop=i
                 if(i.lt.nymean.and.i.gt.nybot)nybot=i
         endif
      enddo
c
      write(*,*)nxtop,nxbot,nytop,nybot
c
      stop
c
100   format(80a1)
101   format('ERROR in read')
102   format('X,',i6,',',i6)
103   format('Y,',i6,',',i6)
c
      end


