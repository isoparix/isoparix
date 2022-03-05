      module venn_comms
c
      real(8)     ,dimension(20000)   :: xp,yp,rp,r1p,r2p,pxp,pyp
      real(8)     ,dimension(20000,2) :: prp
      real(8)     ,dimension(8000)    :: xpoly,ypoly,r1poly,r2poly
      character(2),dimension(8000)    :: poly_type
      integer(4)  ,dimension(8000)    :: poly_layer
      character(30) title
      character(7) lcolour(0:6)
      character(7) pcolour(0:6)
      character(8) ge_colour
c
      real(8),allocatable,dimension(:,:) :: segment_00,agrid,bgrid,zg
      real(8),allocatable,dimension(:) :: segslope_00,segcept_00
     *                                   ,xstart,ystart,xend,yend
     *                                   ,xleft,yleft,xright,yright
     *                                   ,ygrid
c
      real(8) ctrlon,ctrlat,miles_per_deglon,miles_per_deglat
     *       ,miles_per_line,xmax,xmin,ymax,ymin,r1,r2
     *       ,lines_per_mile
c
      integer(4),dimension(2000) :: loc_centres
      integer(4),dimension(:,:),allocatable :: grid,zar,zal,zbr,zbl
     *                                         ,zl,zr
      integer(4),dimension(:)  ,allocatable :: depth,line_start,zcount
     *                                        ,yg
c
      integer(4) ns_00,ndata,nextra,ncentres,ncolstep,nstart,nend
     *          ,nxlow,nxhi,nylow,nyhi,nelements,nsegments,nrle,maxdepth
     *          ,nrunlines,nburns,maxlayer,layer
c
      logical(1),allocatable,dimension(:)   :: line_view
      logical(1) check,graph,bitmap,extend
      logical nostyle(0:255)   !  True if no style set for this depth
c
      end module venn_comms
