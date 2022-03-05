      program andrew_graph
c
c      Reads in data pairs and prepares a .grf file 
c
      implicit real(8) (a-h,o-z)
c
      parameter (lenpts=512)
c
           real(8),dimension(lenpts) :: points
c
           real(8),allocatable,dimension (:) :: x1,y1,x2,y2
      character(8),allocatable,dimension (:) :: oc1,oc2
      character(20) graph_name
           logical,allocatable,dimension (:) :: drawn
           integer,allocatable,dimension (:) :: list_drawn
c
      logical andrew_clash,clash
c
      character (8) ocx,octail
c
      link_limit=1000
c
      nseries=len_file(1)
      allocate(x1(nseries))
      allocate(x2(nseries))
      allocate(y1(nseries))
      allocate(y2(nseries))
      allocate(oc1(nseries))
      allocate(oc2(nseries))
      allocate(drawn(nseries))
      allocate(list_drawn(nseries))
      write(*,111)nseries
c
c      Calculate offsets for flat-earth based on lon=-1.5, lat=53.0
c      (near Derby....)
c
      xref=-1.5D+00
      yref=53.0D+00
      zero=0.0D+00
c     radius=6378.1D+00 ! Kilometres
      radius=3963.2D+00 ! Miles
c
      do ns=1,nseries
c        read(1,112       )oc1(ns),alon1,alat1
         read(1,112,err=98)oc1(ns),alon1,alat1
     *                    ,oc2(ns),alon2,alat2,dist
c
c      Calculate 'flat-earth' co-ordinates for longitude
c
         x1(ns)=sphere_dist(alon1,alat1,xref,alat1,radius)
         x2(ns)=sphere_dist(alon2,alat2,xref,alat2,radius)
         y1(ns)=sphere_dist(alon1,alat1,alon1,yref,radius)
         y2(ns)=sphere_dist(alon2,alat2,alon2,yref,radius)
c
         if(xref.gt.alon1)x1(ns)=-x1(ns)
         if(xref.gt.alon2)x2(ns)=-x2(ns)
         if(yref.gt.alat1)y1(ns)=-y1(ns)
         if(yref.gt.alat2)y2(ns)=-y2(ns)
c
      enddo
c
      xmin=dmin1(minval(x1(1:nseries)),minval(x2(1:nseries)))
      ymin=dmin1(minval(y1(1:nseries)),minval(y2(1:nseries)))
      xmax=dmax1(maxval(x1(1:nseries)),maxval(x2(1:nseries)))
      ymax=dmax1(maxval(y1(1:nseries)),maxval(y2(1:nseries)))
c
      xcen=.5*(xmax+xmin)
      ycen=.5*(ymax+ymin)
      semi_span=1.02*(.5*amax1(xmax-xmin,ymax-ymin))
c
      xmax=xcen+semi_span
      xmin=xcen-semi_span
      ymax=ycen+semi_span
      ymin=ycen-semi_span
c
c      Write opening stanza for these data pairs
c
      graph_name='outcode'
      call grf_header(xmin,xmax,ymin,ymax,graph_name)
c
c      Write a 'point-series' stanza for these data pairs
c
      nlinks=1
      list_drawn(1)=1
      drawn=.true.
      do i=2,nseries
         if(mod(i,10000).eq.0)write(*,107)i,nlinks
         do k=1,nlinks
            j=list_drawn(k)
            if(oc1(i).ne.oc1(j).and.
     *         oc1(i).ne.oc2(j).and.
     *         oc2(i).ne.oc1(j).and.
     *         oc2(i).ne.oc2(j)
     *        )then
                   if(andrew_clash(x1(i),y1(i),x2(i),y2(i)
     *                            ,x1(j),y1(j),x2(j),y2(j))
     *               )then
                          drawn(i)=.false.
                   endif
            endif
         enddo
         if(drawn(i)
     *      )then
                 nlinks=nlinks+1
                 list_drawn(nlinks)=i
         endif
         if(nlinks.ge.link_limit
     *     )then
                do l=i+1,nseries
                   drawn(l)=.false.
                enddo
                exit
         endif
      enddo
c
      write(*,110)nlinks
c
c      Try to write strings of points in 'points stanzas'.  NB We are
c      NOT trying to solve 'Travelling Salesman'....
c
      nwritten=0
      npstanza=0
      initd=nseries     ! Long links first...
      lastd=1
  4   continue
      initdraw=initd
      lastdraw=lastd
      octail='NULLTAIL'
      ncoord=0
      ncold=0
      initd=0
   5  continue
      do i=initdraw,lastdraw,-1
c
         if(ncoord.ge.lenpts
     *     )then
                write(*,109)lenpts,i
                exit
         endif
c
         if(drawn(i)
     *     )then
                if(initd.eq.0
     *            )then
                       initd=i
                endif
c
                if(octail.eq.'NULLTAIL'
     *            )then
                       ncoord=ncoord+1
                       points(ncoord)=x1(i)
                       ncoord=ncoord+1
                       points(ncoord)=y1(i)
                       ncoord=ncoord+1
                       points(ncoord)=x2(i)
                       ncoord=ncoord+1
                       points(ncoord)=y2(i)
                       drawn(i)=.false.
                       if(initd.eq.i
     *                   )then
                              initd=0
                       endif
                       ocx=oc2(i)
                       nwritten=nwritten+1
                endif
c
                if(oc1(i).eq.octail
     *            )then
                       ncoord=ncoord+1
                       points(ncoord)=x2(i)
                       ncoord=ncoord+1
                       points(ncoord)=y2(i)
                       drawn(i)=.false.
                       if(initd.eq.i
     *                   )then
                              initd=0
                       endif
                       ocx=oc2(i)
                       nwritten=nwritten+1
                endif
c
                if(oc2(i).eq.octail
     *            )then
                       ncoord=ncoord+1
                       points(ncoord)=x1(i)
                       ncoord=ncoord+1
                       points(ncoord)=y1(i)
                       drawn(i)=.false.
                       if(initd.eq.i
     *                   )then
                              initd=0
                       endif
                       ocx=oc1(i)
                       nwritten=nwritten+1
                endif
c
                octail=ocx
         endif
         if(drawn(i)
     *     )then
                lastd=i
         endif
      enddo
      initdraw=initd

      if(ncoord.gt.ncold
     *  )then
             ncold=ncoord
             go to 5     ! Keep looking..
      endif
c
      if(ncoord.gt.0
     *  )then
             call grf_points(0,npstanza,ncoord,points(1:ncoord)
     *                      ,'Fuchsia ','Lime    ')
             write(*,108)ncoord/2,nwritten,nlinks,npstanza,initd,lastd
             go to 4
      endif
c
c      Write concluding stanza
c
      call grf_trailer(npstanza)
c
c      Test section for intercepts....
c
c 3   read(*,*)xa1,ya1,xa2,ya2,xb1,yb1,xb2,yb2
c     clash=andrew_clash(xa1,ya1,xa2,ya2,xb1,yb1,xb2,yb2)
c     if(clash
c    *  )then
c            write(*,104)
c        else
c            write(*,105)
c     endif
c     go to 3
c
      stop
c
 98   continue
      write(*,113,err=98)(oc1(mx),alon1,alat1
     *                  , oc2(mx),alon2,alat2,dist
     *                  ,mx,mx=nseries-2,nseries)
      write(*,103)nseries
      stop
103   format('ERROR in read at nseries=',i4)
104   format('CLASH is true')
105   format('CLASH is false')
106   format('Writing link',i6,' of',i6)
107   format('Checking link',i6,' against',i6,' drawn links')
108   format('Add',i4,' nodes for',i8,' of',i8,' links in'
     *      ,i6,' series',2i8)
109   format('Number of co-ordinates at limit of',i6,' after link',i6)
110   format('Identified',i6,' links - writing graph')
111   format('Have allocated',i8,'-element arrays')
112   format(2(a10,2f12.7),f8.3)
113   format(2(a10,2f12.7),': Spherical surface distance=',f8.3,i8)
c
      end
