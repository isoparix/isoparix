      module ip_comms
c
      character(1),allocatable,dimension(:,:) :: picdata
      character(10)datea,dateb
      character(34)histo_name
c
      real(4),allocatable,dimension(:,:)    :: brightness,conarray
      integer(4),allocatable,dimension(:,:) :: ipic,jpic
c
      integer(4),dimension(0:255)  :: jval,kval,lval,mval,nval,ixfer
      integer(2) histarray(800,4000)
      integer(4)ixdim,iydim,nhisto,ncount0,npixels,nonzero_pels
     *         ,median,mean,levelmin,levelmax,maxnzp,maxbar,nthresh
     *         ,nh_current,lo_contrast,hi_contrast,icogx,icogy
c
      real(8)balance,xfer(0:255),sigma_blur,empty,avail,fringe
c
      end
