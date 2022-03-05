      subroutine poly_alloc
c
c      Allocates space
c
      use venn_comms
c
      implicit real(8) (a-h,o-z)
c
      nxlow=xmin*lines_per_mile
      nxhi =xmax*lines_per_mile
      nylow=ymin*lines_per_mile
      nyhi =ymax*lines_per_mile
      nelements=(nxhi-nxlow+1)*(nyhi-nylow+1)
      nsegments=200*(nyhi-nylow)
      allocate(line_view (nylow:nyhi))
      allocate(line_start(nylow:nyhi))
      allocate(depth (nsegments))
      allocate( xleft(nsegments))
      allocate( yleft(nsegments))
      allocate(xright(nsegments))
      allocate(yright(nsegments))
      if(check)write(*,108)miles_per_line,nxlow,nxhi,nylow,nyhi,nelements
c
      return
108   format('POLY_ALLOC:   '
     *   ,'dgrid     nxlow      nxhi     nylow      nyhi nelements'
     *     ,/'         ',f10.5,5i10)
      end
