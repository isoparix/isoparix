      module bmp_comms
c
      character (1024) rgbquad          !Blue/Green/Red/Zero
      character (30) bmname
c
      real(8)phired,phigreen,phiblue
     *        ,ared,  agreen,  ablue
     *        ,pred,  pgreen,  pblue,pi
c
      integer (4) winpels(1024),nwinpels
     *        ,ired,  igreen,  iblue
c
      character(1),allocatable,dimension(:,:) :: mapdata,maptile,canvas
     *                                          ,left3d,right3d,font_def
      character(1),allocatable,dimension(:)   :: bitmap_array
      character(1),allocatable,dimension(:)   :: tmparray
      real(4)     ,allocatable,dimension(:,:) :: distance_data
c
      integer (4) nbmp,winwidth_old,winheight_oldi
     *       ,nsambuckets,ndifbuckets
      end
