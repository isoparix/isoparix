      program panorama
c
c      Generates a set of views to give a panoramic view around a surface
c
      use surf_comms
      implicit real(8) (a-h,o-z)
c
      character(40) filename,surfacename,viewname,vname,lightname
     *             ,vecfile,lightorig
c
      character(40),allocatable,dimension(:) :: sname
c
      real(8),dimension( 10) :: vn
c
      deg2rad=355./(180.0*113.0)
      rad2deg=1./deg2rad
      nrot=3
      arot=nrot
c
      read(1,*)surfacename
      read(1,*)viewname
      read(1,*)ncpus   !  For parallel batch file running
c
      write(filename,115)trim(surfacename)
      write(*,*)filename
      open(40,file=trim(filename),form='formatted',status='unknown')
      write(40,113)  ! @echo off
      np=0
      do nc=50,50+ncpus-1
         write(filename,110)nc-49,trim(surfacename) ! Start of batch stream
         open(nc,file=trim(filename),form='formatted',status='unknown')
         write(nc,111)nc-49     !  Open parenthesis,set monitor
         write(40,109)2**np,trim(filename)
         np=np+1
      enddo
      write(40,114)
      close(40)
      write(*,*)surfacename,viewname,ncpus
      open(7,file=viewname,   form='formatted',status='old')
c
c      Read view details...
c
         do n=1,9
            read(7,*)vn(n)
         enddo
c
         read(7,*)ifl
         read(7,*)ifs
         read(7,*)vn(10)
c
      close(7)
c
      call lightgen(vn(2),vn(3),vn(4),vn(7),vn(8),vn(9),lightname,ncpus
     *             ,r,theta,phi)
c
c     write(*,*)ncpus,rad2deg,rad2deg*theta,rad2deg*phi,theta,phi
c     stop
c
      write(*,100)
      read(*,*)nsteps
      if(nsteps.eq.0)stop
c
      pi=4.d0*atan(1.d0)
      allocate(sname(0:nsteps))    ! Number of view files per turn...
      rsteps=1./float(nsteps)
      nrot=3
      arot=nrot
      dtheta=2.0*pi/float(nsteps)
c     dphi  =dtheta/arot
      dphi=0.
c
c      Write a set of new views...
c
      write(*,1070)
      do n=1,nrot*nsteps
         theta=dtheta*float(n)
         vn(2)=r*cos(theta)
         vn(4)=r*sin(theta)
c
         theta_now=dmod(rad2deg*theta,3.6d+02)
           phi_now=dmod(rad2deg*phi,  3.6d+02)
         write(*,1071)n,theta_now,rad2deg*dtheta
     *                 ,phi_now,  rad2deg*dphi
     *                 ,vn(2),vn(3),vn(4)
         write(vname,106)n
         nstream=1+mod(n,ncpus)
         lchann=49+nstream
c
c      Write the command to build the vector data file
c
         vecfile=trim(surfacename)//trim(vname)
         write(lchann,105)trim(surfacename),trim(vname)
c
c      Use two commands for two bitmaps - one always illuminated from
c      the initial viewpoint angles, and the second illuminated from the 
c      current viewpoint angles (like holding a torch...)
c
         write(lightname,1062)n
         open(80,file=lightname,form='formatted',status='unknown')
            write(80,118)theta_now, phi_now
         close(80)
         write(lchann,1161)trim(vecfile)//'.vec'
         write(lchann,119)trim(vecfile)//'.vec',trim(vecfile)
c
         write(lightname,1061)nstream
         write(lchann,116)trim(vecfile)//'.vec',trim(lightname)
         write(lchann,120)trim(vecfile)//'.vec',trim(vecfile)
c
c        write(lchann,117)trim(vecfile)//'.vec'   ! Delete vector file
c
         open(12,file=trim(vname),form='formatted',status='unknown')
         do m=1,9
            write(12,107)vn(m)
         enddo
         write(12,*)ifl
         write(12,*)ifs
         write(12,107)vn(10)
         close(12)
      enddo
c
      do nc=50,50+ncpus-1
         write(nc,112)nc-49,nc-49  !  Close parenthesis
         close(nc)
      enddo
c
      stop
c
100   format('How many views per turn?')
101   format(3i1,f10.3)
105   format('build_surf',2(' ',a,' ',a))
106   format('pv.',i3.3)   !  Pan view
1061  format('ls.',i3.3)   !  Light stream
1062  format('lq.',i3.3)   !  Light step
107   format(8f10.3)
1070  format(/'     N  radtheta  degtheta    radphi    degphi'
     *       ,'     vn(2)     vn(3)     vn(4)')
1071  format(i6,8f10.3)
108   format(a,'.bmp')
1081  format('if exist ',a,'.bmp link ',a,'.bmp m'
     *     ,i2.2,'xa',i2.2,'.bmp ')
109   format('start /HIGH /AFFINITY 0x',z2.2,' ',a)
110   format(z2.2,'pan_',a,'.bat')
111   format('(',/'touch ',z2.2,'.monitor')
112   format('del ',z2.2,'.monitor'/'exit',/') > panorama',i2.2,'.log')
113   format('@echo off')
114   format('echo .'
     *     ,/'echo .'
     *     ,/'exit')
115   format('pan_',a,'.bat')
116   format('shine_surf ',a,' ',a)
1161  format('torch ',a)
117   format('del ',a,/)
118   format(f8.3,/f8.3)
119   format('move ',a,'.bmp A',a,'.bmp')
120   format('move ',a,'.bmp ',a,'.bmp')
c
      end
