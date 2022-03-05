      subroutine polyprep(na,nb)
c
c      Compute segments characteristics, and write out a graph of the polygon
c
      use venn_comms
c
      implicit real(8) (a-h,o-z)
c
      character(50) :: graph_name
c
      logical unsorted
c
      layer=poly_layer(na)
      if(check)write(18,102)na,nb,layer
c
      np=0
      do n=na,nb
         np=np+1
          xp(np)= xpoly(n)
          yp(np)= ypoly(n)
         r1p(np)=r1poly(n)
         r2p(np)=r2poly(n)
      enddo
c
      pxp(1)=xp(1)
      pyp(1)=yp(1)
      prp(1,1)=r1p(1)
      prp(1,2)=r2p(1)
c
      if(np.eq.1
     *  )then
             call polyfill(1,1,1)
             call polyfill(1,1,2)
             return
         else
             call polysplit(np)   !  Split polygon segments
      endif
      
c
c
      if(allocated(segment_00)
     *  )then
             deallocate(segment_00)
      endif
      allocate(segment_00(4,np))
c
      if(allocated(segslope_00)
     *  )then
             deallocate(segslope_00)
      endif
      allocate(segslope_00(np))
c
      if(allocated(segcept_00)
     *  )then
             deallocate(segcept_00)
      endif
      allocate(segcept_00(np))
c
      if(np.gt.1
     *  )then
             if(graph
     *         )then
                    write(graph_name,100)trim(title),na,nb
                    call grf_header(xmin,xmax,ymin,ymax,graph_name)
             endif
c
             ns_00=0
             do n=2,np
                ns_00=ns_00+1
                segment_00(1,ns_00)=xp(n-1)
                segment_00(2,ns_00)=yp(n-1)
                segment_00(3,ns_00)=xp(n)
                segment_00(4,ns_00)=yp(n)
                call segsc(segment_00(:,ns_00),segslope_00(ns_00)
     *                                        , segcept_00(ns_00))
                if(graph)call grf_points(0,ns_00,4,segment_00(:,ns_00)
     *                         ,'Lime    ','Blue    ')
             enddo
             if(graph)call grf_trailer(1+ns_00)
c
c   Map exclusion zones (inner radii), the map outer zones (outer radii)
c
             do n=2,np
                call polyfill(loc_centres(n-1),loc_centres(n),1)
             enddo
             do n=2,np
                call polyfill(loc_centres(n-1),loc_centres(n),2)
             enddo
      endif
c
      return
c
100   format(a,'_',i4.4,'_',i4.4,'_base_poly')
101   format('POLYPREP: n=',i6,', d,miles_per_line:',2f10.3
     *      ,', ndivs=',i6
     *      ,' from',2f10.2,' to',2f10.2,', ncentres=',i6)
102   format('POLYPREP: na=',i3,', nb=',i3," Layer=",i3)
114   format('POLYPREP:','        Extra point:',i6,4f10.5)
119   format('POLYPREP',i5,':',100(i3,f7.2,i3))
120   format('   Ticks',i5,':',100(i3,f7.2,i3))
121   format(1000i1)
122   format('POLYPREP CO-ORDS',i5,':',4f10.3,i3)
      end
