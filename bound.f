      subroutine bound(nprocess)
c
c      Characterises all vertices of cubes of process NPROCESS
c      and says which ones lie on the boundary
c
      use isocomm
c
      if(check
     *  )then
c            write(     0,102)na,nprocess
             write(txtout,102)na,nprocess
             call statout
      endif
      if(nprocess.gt.nsources)stop
c
      do na=1,nactive(nprocess)
         nc=listact(na,nprocess)
c
c      Clear Line_Vertex_Values, and Number_Edge_Vertices values
c
         lvc(nc)=7
         do nv=1,max_line_vertices
            lvv(nv,nc)=0
         enddo
c
         do j=1,4
            nev(j,nc)=2
         enddo
c
         lvid(1,1,nc)=3
         lvid(2,1,nc)=7
         lvid(1,2,nc)=5
         lvid(2,2,nc)=7
         lvid(1,3,nc)=1
         lvid(2,3,nc)=5
         lvid(1,4,nc)=1
         lvid(2,4,nc)=3
c
         lvlist(1,1,nc)=nx1(nc)
         lvlist(2,1,nc)=nx2(nc)
         lvlist(1,2,nc)=nz1(nc)
         lvlist(2,2,nc)=nz2(nc)
         lvlist(1,3,nc)=nx1(nc)
         lvlist(2,3,nc)=nx2(nc)
         lvlist(1,4,nc)=nz1(nc)
         lvlist(2,4,nc)=nz2(nc)
      enddo
c
      do na=1,nactive(nprocess)
         nc=listact(na,nprocess)
             do nv=1,4
                if(lvv(nvf(2,nv),nc).eq.0)then
                                           call vcheck(ivx(nvf(2,nv),nc)
     *                                                ,ivz(nvf(2,nv),nc)
     *                                                ,nprocess)
                endif
             enddo
      enddo
c
c      Now define the line segments
c
      do na=1,nactive(nprocess)
         nc=listact(na,nprocess)
c
c      Zero linesegment count for each face
c
            do nf=1,6
               lsc(nf,nc)=0
            enddo
c
c      First, the verticals
c
            do n=1,4
               nv=nvf(2,n)
               if(mod(lvv(nv,nc),3).ne.0)then
                           do i=1,3
                              if(nfv(nv,i).ne.2.and.
     *                           nfv(nv,i).ne.5
     *                          )then 
                                     call linesegs(nv,nv+1,nfv(nv,i),nc)
                              endif
                           enddo
               endif
            enddo
c
c      Then, the edges 
c     subroutine makeline(jside,itest,nface1,nface2,nc)
c
            call makeline(1,3,3,nc)
            call makeline(2,3,6,nc)
            call makeline(3,4,4,nc)
            call makeline(4,2,1,nc)
c
c      Identify the screen positions of the new vertices
c
      do nv=9,lvc(nc)+1
         call photomap(nv,nc)
      enddo
c
c           call cubecheck(nc)
c
      enddo
c
100   format('Cube',i3,', process',i3,' has',i3,' line vertices '
     *       ,'(max_line_vertices=',i3,')')
101   format(/'    Cube',i3,', process',i3,', line vertices',i3,':',/) 
102   format('BOUND: na/nprocess',2i18)
104   format(/'***** Summary for this line *****',/)
105   format('***** ERROR: LVVs:',2i3,' at',2i3)
106   format('LineSegmentCount for face',i2,' of cube',i3,' is',i3)
107   format(45x,4i5)
108   format('***** OK-OK: LN:',i3,', LS',i3,', NF',i3,' NC',i3)
      end
