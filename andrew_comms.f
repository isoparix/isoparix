      module andrew_comms
c
      real(8)     ,dimension(20000) :: xp,yp,r1p,r2p
      real(8)     ,dimension(2000)  :: xpoly,ypoly,r1poly,r2poly
      character(2),dimension(2000)  :: poly_type
      real(8),allocatable,dimension(:,:) :: segment_00
      real(8),allocatable,dimension(:) :: speed,distance
     *                                   ,outlat,outlon,inlat,inlon
     *                                   ,segslope_00,segcept_00
     *                                   ,xstart,ystart,xend,yend
     *                                   ,xgrid,ygrid
c
      real(8) ctrlon,ctrlat,miles_per_deglon,miles_per_deglat
     *       ,dgrid,xmax,xmin,ymax,ymin,r1,r2,north,south,east,west
c
      integer(4),dimension(:,:),allocatable :: grid
      integer(4),dimension(:)  ,allocatable :: depth
c
      integer(4),dimension(:)  ,allocatable :: tt
c
      integer(4),dimension(1296) :: outcode_index,incode_index
     *                              ,ttcode_index
      integer(4) noutrecs,ninrecs,nttrecs,ns_00,ndata,nextra,ncentres
     *          ,nxlow,nxhi,nylow,nyhi,nelements,nsegments,nrle,maxdepth
     *          ,ncoldivs
c
      character (8),allocatable,dimension(:) :: incode,outcode
     *                                         ,ttcodea,ttcodeb
      character (6),allocatable,dimension(:) :: travel_pair
c
      logical(1),allocatable,dimension(:,:) :: lgrid
      logical(1),allocatable,dimension(:)   :: line_view
      logical(1) check
c
      end module andrew_comms
