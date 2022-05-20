      module isocomm
c
      parameter (max_line_vertices=20,maxcubes=2000,line_segments=30)
      parameter (nsources=320,nsplits=100,len_ngrafout=1048576)
      parameter (lindet_entries=1000,len_perimsg=1000000)
      parameter (max_x_prim=20000000)
c
      real(4) xscale,yscale,scale,xoffset,yoffset
     *       ,vs,vc,vd,vx,vz,xnca,znca,phi_inv
     *       ,aplane,bplane,cplane,dplane,eyemul
     *       ,xn,yn,zn,xr,yr,zr,cdist2,r2c,slopex,cx,sp,cp
     *       ,xi,yi,zi,xmax,xmin,ymax,ymin
     *       ,dmin,dmax
     *       ,xe,ye,ze,xcentre,ycentre,zcentre
     *       ,rr2,rd2,d2
     *       ,viewangle,photomax
c
c
c     integer(4) maxpels,ntotpels,npels,mpels,kpels
c
c      integer (8) kill a message somewhere...
c
      integer(8) maxpels,ntotpels,npels,mpels,kpels
      integer(4) ixm,iym,ixmh,iymh,n_x_prim
      integer iretcode,nedgecol,x_prim(max_x_prim,7)
     *       ,nactive(0:nsources),listact(nsplits,nsources)
     *       ,nlong,nprocmax,ntotface,ngline
     *       ,maxcols,isocols
     *       ,irndcol(0:300),mycol(256),ncubes,ixwin,iywin
     *       ,nv1,nv2,nv3,nv4,minact,mline,nline
     *       ,nev(4,maxcubes),number(4,0:15)
     *       ,nchksum
     *       ,ixcmax,ixcmin,iycmax,iycmin,list_order
     *       ,ixcmaxold,ixcminold,iycmaxold,iycminold
     *       ,nface(8,6),linecol(8,8,maxcubes)
     *       ,nfv  (8,3),nvf  (6,4),newscore(20),nscore
     *       ,nvopp(8,3),npx,npy,kvtx(100),kcube(100),ivd(4),kvj(100)

c
      real(8)diagonal
c
      character(20) tag(0:20)
      character(25) statout_hdr,statout_hdr_old
c
      real(4),dimension(4) :: slopexz,slopexy,slopewy
c
      real(8),dimension(max_line_vertices,maxcubes) :: xc,yc
      integer,dimension(max_line_vertices,maxcubes) :: 
     *                                    ixc,iyc,ivx,ivy,ivz
     *                                   ,lvx,lvy,lvz,lvv,lvj
      integer,dimension(   maxcubes) :: x1,x2,z1,z2,nmasks,facecol
     *                         ,nearvert,farvert
     *                         ,nproc,narea,xmid,zmid,nx1,nx2
     *                         ,nz1,nz2,lvc,longact
      logical,dimension(   maxcubes) :: notviewed,active,sized,described
     *                                 ,unselected
      integer,dimension( 6,maxcubes) :: lsc
      logical,dimension( 6,maxcubes) :: visiface
      integer,dimension(0:max_line_vertices,maxcubes) :: hidestat
     *                 ,list_supplied
      logical,dimension( 8,maxcubes) :: visivert
      logical,dimension(12,maxcubes) :: visiline
      integer,dimension(line_segments,6,maxcubes) :: lsa,lsb
      integer,dimension(max_line_vertices,line_segments,maxcubes) ::
     *                                                 lvlist,lvid
      integer,dimension(maxcubes,maxcubes) :: idmasks
      logical,dimension(maxcubes,maxcubes) :: cmasks
      integer,dimension(maxcubes) :: max_x,max_y,min_x,min_y
      real(4),dimension(maxcubes) :: x_max,y_max,x_min,y_min
      real(4),dimension(maxcubes) :: vdmin,vdmax

c
      logical lretcode,replace,resolved,entry(max_line_vertices)
c
      character (1) faceaxis(6)
      character (150) infomsg
c
c      Common data for isoparix codes
c
      character (6) role,subname
      character (55) txtout
c
      real (8) params(40),ttitle
      real (8),dimension(5) :: realin,realout
c
      logical check,dispnow,bisector,graphics,needwin,autocycle,logmap
     *       ,author,constcol,cdiff,msgwaiting,calcphase,ixplane,need
     *       ,lookout,signoff,loadbal,row,good_message,newline
     *       ,screen_graphics,bitmap_graphics,off_graphics,iso_mpi_term
     *       ,cube_logic,square_logic
c
c
      integer dontcare,allmsg,nulltask,allgrp,msglow,msghi
     *        ,taskid,numtasks,master,artist,lchann
     *        ,source,dest,mpitype,nbuf(4)
     *        ,ngrf,iter_hi,iter_lo
     *        ,maisox,limita,limit,ldiff,iter_lim
     *        ,nedge,kedge,minperi,nwork,iterxs
c
c
c       Contains the allocatable size of the picture
c
      real(8),allocatable,dimension(:) :: cr,ci,sr,si
c
      integer(4) :: lindet(6,lindet_entries)
      integer(4) ngrafout(len_ngrafout),perimsg(len_perimsg)
      integer(4),allocatable,dimension(:,:) :: mset
      character(1),allocatable,dimension(:)   :: paint
c
      integer(4),dimension(1024) :: ncell,ncell_levs
      integer(8),allocatable,dimension(:)  :: kdpel
      integer(4),allocatable,dimension(:)  :: itermin,itermax
     *                                       ,msgcount_in,msgcount_out
c
      end
