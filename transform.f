      program transform
c
c      Transforms one surface into another,
c
      use surf_comms
c
      implicit real(8) (a-h,o-z)
c
      character(40) filename,surfacename,surfacename_a,surfacename_b
     *             ,viewname,lightname
c
      character(40),allocatable,dimension(:) :: sname,vname
c
      integer,dimension(100) :: ixt,iyt,izt
c
      real(8),dimension(100) :: ct,cstep,c_orig,st,dt
      real(8),dimension( 10) :: vn
      real(8),dimension(0:9,0:9,0:9) :: coeff_a,coeff_b,sdelta
c
      used=.false. ! Declare all cells unused
      sdelta=0.d0  ! initialise deltas
c
      read(1,*)surfacename_a
      read(1,*)surfacename_b
      read(1,*)viewname
      read(1,*)ncpus   !  For parallel batch file running
      open(7,file=viewname,   form='formatted',status='old')
c
c      Read original view details...
c
      do n=1,9
         read(7,*)vn(n)
      enddo
c
      rr=sqrt(vn(2)**2+vn(4)**2)   ! Radius of view rotation sqrt(x^2 +y^2)
c
      read(7,*)ifl
      read(7,*)ifs
      read(7,*)vn(10)
c
c
c      At some point, read these in, but for now....
c
      pi=4.d0*atan(1.d0)
      nrotations=3
      ntransforms=2
c
c     Condition nimages..
c
      write(*,108)
      read(*,107)dphi
      if(dphi.eq.0)stop
      nimages=float(ntransforms*2)*pi/dphi
      dphi=float(ntransforms*2)*pi/float(nimages)
c
      allocate(sname(0:nimages-1))    ! Number of surface files...
      sname=''
c
      allocate(vname(0:nimages-1))       ! Number of view files in display
      vname=''
c
      dtheta=float(nrotations*2)*pi/float(nimages)  ! Constant turn rate, whole turns
c
c  *****************************************
c      Create view files
c  *****************************************
c
      do nv=0,nimages-1
            theta=dtheta*float(nv)      ! 360 degree total turn..
            write(vname(nv),1031)nv
c
c      Create a new view file
c
            open(8,file=trim(vname(nv)),form='formatted'
     *                      ,status='unknown')
               vn(2)=rr*cos(theta)  
               vn(4)=rr*sin(theta)
               do n=1,9
                  write(8,107)vn(n)
               enddo
               write(8,*)ifl
               write(8,*)ifs
               write(8,107)vn(10)
               call flush(8)
            close(8)
      enddo
c
c  *****************************************
c      Read in old and new surface names...
c  *****************************************
      open(3,file=trim(surfacename_a),form='formatted',status='old'
     *      ,err=98)
      write(60,121)
      call read_surface(3)
      close(3)
      coeff_a=coeff
c
      open(3,file=trim(surfacename_b),form='formatted',status='old'
     *      ,err=99)
      write(60,122)
      call read_surface(3)
      close(3)
      coeff_b=coeff
c  *****************************************
c      Calculate the delta for each term in the 10x10x10 matrix
c  *****************************************
      sdelta=coeff_b-coeff_a
c
c##################### Write out the surface deltas ###########################
      write(60,123)
      nterm=0
      do iz=0,9
         do iy=0,9
            do ix=0,9
               if(used(ix,iy,iz)        ! Is this cell used by either surface?
     *           )then
                      nterm=nterm+1
                      ixt(nterm)=ix
                      iyt(nterm)=iy
                      izt(nterm)=iz
                      dt(nterm)=sdelta(ix,iy,iz)
                      st(nterm)=coeff_a(ix,iy,iz)
              write(60,101)ixt(nterm),iyt(nterm),izt(nterm)
     *                   ,st(nterm),dt(nterm)
                endif
            enddo
         enddo
      enddo
c########################################################################
c
c  *****************************************
c      Create surface files
c  *****************************************
c
      do ns=0,nimages-1
         write(sname(ns),103)ns
         sinsq=(sin(float(ns)*dphi)**2) !  Multiplication factor, surface deltas
         write(62,*)ns,sinsq
c
c      Create a new surface file, with varied co-efficient(s)
c
         open(8,file=trim(sname(ns)),form='formatted'
     *                      ,status='unknown')
         do nt=1,nterm
            ct(nt)=st(nt)+(dt(nt)*sinsq)
            write(8,101)ixt(nt),iyt(nt),izt(nt),ct(nt)
         enddo
         call flush(8)
         close(8)
      enddo
c
c
      ns=1      ! Surface ID
      nv=1      ! View ID
      nchann=0
c
      write(*,100)dphi,nimages,nrotations,ntransforms
c
      write(filename,115)trim(surfacename_a),trim(surfacename_b)
      write(*,*)filename
      open(40,file=trim(filename),form='formatted',status='unknown')
      write(40,113)  ! @echo off
      np=0
      do nc=50,50+ncpus-1
         write(filename,110)nc-49
         write(40,109)2**np,trim(filename)
         np=np+1
         open(nc,file=trim(filename),form='formatted',status='unknown')
         write(nc,111)  !  Open parenthesis
      enddo
      nchann=0
      do ni=0,nimages-1
           nchann=1+mod(ni,ncpus)
           write(49+nchann,105)trim(sname(ni)),trim(vname(ni))
     *                        ,trim(sname(ni)),trim(vname(ni))
c          write(*,*)ni,ncpus,nchann
      enddo
      write(40,114)
      call flush(40)
      close(40)
c
      do nc=50,50+ncpus-1
         write(nc,1121)nc-49  !  Record End of Job
         write(nc,112)nc  !  Close parenthesis
         call flush(nc)
      enddo
c
      stop
c
 98   write(0,120)surfacename_a
      stop
c
 99   write(0,120)surfacename_b
      stop
c
100   format('dphi:',e14.7,', Images:',i5,', Rotations:',i3
     *      ,', Transforms:',i3,/)
101   format(3i1,' ',2f15.3)
102   format('Step',i3,' of',i3,'.  Morphed terms:',i3,' and ',i3
     *       ,' File ',a,2(/3i1,f8.3),/'  viewed from ',a
     *       ,'.  Radians=',f10.7,/)
103   format('ts',i4.4)
1031  format('v',i4.4)
104   format('Step change for term',i4,' is ',f8.3)
105   format('build_surf ',a,' ',a
     *     ,/'torch      ',a,a,'.vec'
     *     ,/)
106   format('ls.',i3.3)
107   format(f12.6)
108   format('dphi?  Typical value ~ 0.1')
109   format('start /HIGH /AFFINITY 0x',z2.2,' ',a)
110   format(z2.2,'trans.bat')
111   format('(',/)
112   format('echo .',/'echo .',/'exit'
     *      ,/') > trans',i2.2,'.log')
1121  format('echo Task',i3,' completed at %TIME% >> batch.log')
113   format('@echo off',/'del batch.log')
114   format('echo .'
     *     ,/'pause',/'exit')
115   format('trans_',a,'_',a,'.bat')
116   format('Image:',i3,', Surface:',i3,', View:',i3)
117   format('Too large - separation changed to',i3)
118   format('Too many views per turn - views changed to',i3)
120   format('ERROR: Reading surface ',a)
121   format(/'Old surface:')
122   format(/'New surface:')
123   format(/' First surface    Deltas')
c
      end
