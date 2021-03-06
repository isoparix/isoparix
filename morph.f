      program morph
c
c      Deforms each term in sequence of a given surface
c
      use surf_comms
c
      implicit real(8) (a-h,o-z)
c
      character(40) filename,surfacename,viewname,lightname
c
      character(40),allocatable,dimension(:) :: sname,vname
c
      integer,dimension(100) :: ixt,iyt,izt
c
      real(8),dimension(100) :: ct,cstep,c_orig
      real(8),dimension( 10) :: vn
c
      read(1,*)surfacename
      read(1,*)viewname
      read(1,*)ncpus   !  For parallel batch file running
c
      write(filename,115)trim(surfacename),trim(viewname)
      write(*,*)filename
      open(40,file=trim(filename),form='formatted',status='unknown')
      write(40,113)  ! @echo off
      np=0
      do nc=50,50+ncpus-1
         write(filename,110)nc-49,trim(surfacename)
         open(nc,file=trim(filename),form='formatted',status='unknown')
         write(nc,111)nc-49  !  Open parenthesis
         call flush(nc)
         write(40,109)2**np,trim(filename)
         np=np+1
      enddo
      write(40,114)
      call flush(40)
      close(40)
c
      open(7,file=viewname,   form='formatted',status='old')
c
c      Read original view details...
c
      do n=1,9
         read(7,*)vn(n)
      enddo
c
      radius=sqrt(vn(2)**2+vn(3)**2+vn(4)**2) ! Radius of viewing sphere
      phi=asin(vn(3)/radius)
c
      read(7,*)ifl
      read(7,*)ifs
      read(7,*)vn(10)
      close(7)
c
      lchann=3
      open(lchann,file=surfacename,form='formatted',status='old')
      call read_surface(lchann)
      close(lchann)
c
      nterm=0
      do iz=0,izmax
         do iy=0,iymax
            do ix=0,ixmax
               if(coeff(ix,iy,iz).ne.0.0
     *           )then
                      nterm=nterm+1
                      ixt(nterm)=ix
                      iyt(nterm)=iy
                      izt(nterm)=iz
                       ct(nterm)=coeff(ix,iy,iz)
               write(*,101)ixt(nterm),iyt(nterm),izt(nterm),ct(nterm)
                endif
            enddo
         enddo
      enddo
c
      c_orig=ct   ! Save the original surface...
c 
      write(*,100)
      read(*,*)nsteps
      if(nsteps.eq.0)stop
      if(nsteps.gt.24
     *  )then
             nsteps=24
             write(0,118)nsteps
      endif
      ndivs=2*nsteps
      nviews=((2*ndivs-1))*nterm    ! Number of different views...
      allocate(sname(0:2*ndivs))    ! Number of surface files per term...
      allocate(vname(0:nviews))     ! Number of view files in display
      rsteps=1./float(nsteps)
c
      pi=4.d0*atan(1.d0)
      rpi=1.d0/pi
      dtheta=8.0*pi/float(nviews)            ! Constant turn rate, whole turns
c
c      Separation of terms that are changed, eg =2, change 2 and 4
c
      write(*,116)
      read(*,*)ndelta
      if(ndelta.ge.nterm    ! Stop this run...
     *  )then
             open(20,file=trim(surfacename)//'.ERROR.LOG'
     *           , status='unknown')
             write(20,117)trim(surfacename),ndelta,nterm
             call flush(20)
             close(20)
             stop
      endif
c
      do nsurf=1,nterm    ! Find step changes for all terms..
         cstep(nsurf)=ct(nsurf)*rsteps
         write(*,104)nsurf,cstep(nsurf)
      enddo
c
      nchann=0
      nv=0
      vname=''
c
      do nsurf=1,nterm    !  ...not nsurfaces - all of them!
         sname=''
c
         ct=c_orig  ! Make sure we are exactly back to original...
         do ns=1,2*ndivs-1
c
c      Create a new view file name
c
            theta=dtheta*float(nv)      ! 360 degree total turn..
c           phi=0.5*theta               ! Change here if we must...
            rxz=radius*dabs(cos(phi))   ! Radius in XZ-plane
            write(*,119)nsurf,ns,theta*rpi,phi*rpi
            write(vname(nv),1031)nv
c
c     Create a new view file
c
            open(8,file=trim(vname(nv)),form='formatted'
     *                      ,status='unknown')
               vn(2)=rxz   *cos(theta)
               vn(3)=radius*sin(phi)
               vn(4)=rxz   *sin(theta)
               do n=1,9
                  write(8,107)vn(n)
               enddo
               write(8,*)ifl
               write(8,*)ifs
               write(8,107)vn(10)
               call flush(8)
            close(8)
c
c      Create a new surface file name
c
            write(sname(ns),103)nsurf,ns
c
c      Create a surface file
c
            open(8,file=trim(sname(ns)),form='formatted'
     *                      ,status='unknown')
            do nt=1,nterm
               write(8,101)ixt(nt),iyt(nt),izt(nt),ct(nt)
            enddo
            call flush(8)
            close(8)
c
            ma=nsurf
            mb=nsurf+ndelta
            mb=1+mod(mb,nterm)
c
            write(*,102)ns,2*ndivs,ma,mb,trim(sname(ns))
     *                 ,ixt(ma),iyt(ma),izt(ma),ct(ma)
     *                 ,ixt(mb),iyt(mb),izt(mb),ct(mb)
     *                 ,trim(vname(nv)),theta,theta*rpi
c            
c        Vary the coefficients       
c            
            if(ns.ge.ndivs
     *        )then
                   ct(ma)=ct(ma)+cstep(ma)
                   if(mb.ne.ma
     *               )then
                          ct(mb)=ct(mb)+cstep(mb)
                   endif
               else
                   ct(ma)=ct(ma)-cstep(ma)
                   if(mb.ne.ma
     *               )then
                          ct(mb)=ct(mb)-cstep(mb)
                   endif
            endif
            nchann=nchann+1
            write(49+nchann,105)trim(sname(ns)),trim(vname(nv))
     *                         ,trim(sname(ns)),trim(vname(nv))
            if(nchann.eq.ncpus)nchann=0
c
            nv=nv+1
         enddo
c
      enddo
c
      do nc=50,50+ncpus-1
         write(nc,1121)nc-49  !  Record End of Job
         write(nc,112)nc-49,nc-49  !  Close parenthesis
         call flush(nc)
         close(nc)
      enddo
c
      stop
c
100   format('How many surfaces per term change?')
101   format(3i1,' ',f20.2)
102   format('Step',i3,' of',i3,'.  Morphed terms:',i3,' and ',i3
     *       ,' File ',a,2(/3i1,f8.3),/'  viewed from ',a
     *       ,'.  Radians=',f10.7,f7.3,' pi'/)
103   format('m',i2.2,'x',i2.2)
1031  format('v',i4.4)
104   format('Step change for term',i4,' is ',f8.3)
105   format('build_surf ',a,' ',a
     *     ,/'torch ',a,a,'.vec')
106   format('ls.',i3.3)
107   format(f12.6)
108   format(a,'.bmp')
109   format('start /HIGH /AFFINITY 0x',z2.2,' ',a)
110   format(z2.2,a,'.bat')
111   format('(',/'touch ',z2.2,'.monitor')
112   format('del ',z2.2,'.monitor',/'exit'
     *      ,/') > morph',i2.2,'.log')
1121  format('echo Task',i3,' completed at %TIME% >> batch.log')
113   format('@echo off',/'del batch.log')
114   format('echo .'
     *     ,/'exit')
115   format('morph_',a,'_',a,'.bat')
116   format('What separation between changed terms?')
117   format('Separation of terms for ',a,' too large at',i3
     *      ,'. NTERMS=',i3)
118   format('Too many views per turn - views changed to',i3)
119   format('NSURF:',i6,', NS:',i4,', theta:',f8.4,', phi=',f8.4)
c
      end
