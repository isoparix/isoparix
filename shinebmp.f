      PROGRAM SHINEBMP
C
      use bmp_comms
c
      IMPLICIT REAL *8 (A-H,O-Z)
      CHARACTER *40 INPFILE,vector_name
      CHARACTER *3 step_id
      DIMENSION SC(100),IPX(100),IPY(100),IPZ(100)
C
      integer,allocatable,dimension(:) :: LL
      character(1),allocatable :: brightness(:,:)
      real(8),allocatable,dimension(:,:) :: vec_i,vec_j,vec_k,cphi,atmp
     *                                     ,bll
      real(8)inclination
c
      logical dtheta,dphi,torch
c
      i=-1
      j=-1
      k=-1
      call rgbindex(i,j,k) ! gfortran insists on parameters, not numbers
c
      write(*,102)
      read(*,103)vector_name
      inpfile=trim(vector_name)
      write(*,*)inpfile
C
      OPEN(2,FILE=INPFILE,FORM='UNFORMATTED',STATUS='OLD',ERR=99)
      read(2)(SC(N),IPX(N),IPY(N),IPZ(N),N=1,100),XA,YA,ZA
     *,XCV,YCV,ZCV,VD,VDI,IFL,IFS,VA,ACC,NTERMS
C
      allocate(LL(IFL))
      allocate(brightness(ifl,ifs))
      allocate(vec_i(ifl,ifs))
      allocate(vec_j(ifl,ifs))
      allocate(vec_k(ifl,ifs))
      allocate( cphi(ifl,ifs))
      allocate(  bll(ifl,ifs))
      allocate( atmp(ifl,ifs))
c
      vec_i=0.
      vec_j=0.
      vec_k=0.
      atones=255.
      nrow=ifs+1
  1   continue
         read(2,ERR=99,end=3)LL
         nrow=nrow-1
         DO ncol=1,IFL
            if(ll(ncol).ne.0
     *        )then
                   CALL EXPAND(LL(ncol)
     *             ,vec_i(ncol,nrow),vec_j(ncol,nrow),vec_k(ncol,nrow))
            endif
            bll(ncol,nrow)=ll(ncol)
c           if(ll(ncol).ne.0)write(90,*)ncol,nrow,bll(ncol,nrow)
         enddo
         go to 1
   3  continue
      close(2)
C
      write(*,*)IFL,IFS,NTERMS
      write(*,*)IFL,IFS,VA,ACC,NTERMS
c
      deg2rad=355.0/(113.0*180.0)  ! 2*pi/360
      rad2deg=1./deg2rad
c
c      Calculate theta (x-z plane) and phi (y and x-z)
C
             dx=xa-xcv
             dy=ya-ycv
             dz=za-zcv
             r=sqrt(dx**2+dy**2+dz**2)
             rxz=sqrt(dx**2+dz**2)
             pi=355.0/113.0
             if(dz.ne.0
     *         )then
                    theta=atan(dx/dz)
                else
                    if(dx.gt.0.
     *                )then
                           theta= 355.0/226.0  ! pi/2
                       else
                           theta=-355.0/226.0  ! pi/2
                    endif
             endif
             if(dz.lt.0.0)theta=theta-pi
             phi=acos(sqrt(dx**2+dz**2)/r)
             phi  =rad2deg*phi
             theta=rad2deg*theta
      write(6,122)theta,phi,dx,dy,dz ! Is the light shining along line of sight?
      read(5,106)torch
      if(torch
     *  )then
             write(6,121)theta,phi
         else
             write(6,123) ! Direction of light
             write(*,1231)
             read(5,104)theta
             write(*,1232)
             read(5,104)phi
             write(6,120)theta,phi
      endif
      write(6,124) ! Alter theta?
      read(5,106)dtheta
      write(6,125) ! Alter phi?
      read(5,106)dphi
      if(dphi.or.dtheta
     *  )then
             write(6,126) ! Number of steps
             read(5,*)nsteps
             if(nsteps.le.0)nsteps=1
         else
             nsteps=1
      endif
      nrot=3
      arot=nrot
      theta_delta=360./float(nsteps)
        phi_delta=theta_delta/arot
C
      ambient=20.
      amfactor=1.0/(atones+ambient)
      do ns=0,nsteps-1
         if(nsteps.gt.1
     *     )then
                write(step_id,101)ns
                bmname=trim(vector_name)//step_id
            else
                bmname=trim(vector_name)
         endif
         ltbn=len_trim(bmname)
         if(ltbn.gt.29
     *     )then
                write(0,119)ltbn
                stop
         endif
c        if(torch)bmname='Z'//trim(bmname)
C
C
         aq=sin(deg2rad*theta)
         bq=sin(deg2rad*phi)
         cq=cos(deg2rad*theta)
         if(torch)
     *            write(*,128)trim(vector_name),dx,dy,dz,theta,phi,aq
     *                       ,bq,cq
         if(dtheta)theta=theta+theta_delta
         if(dphi  )phi  =phi  +phi_delta
C
         dq=1./sqrt((aq*aq)+(bq*bq)+(cq*cq))
         aq=aq*dq
         bq=bq*dq
         cq=cq*dq
c OK
c
c        write(0,*)ifl,ifs,aq,bq,cq
         CPHI=(vec_i*AQ)+(vec_j*BQ)+(vec_k*CQ)
c        do iy=1,ifs
c           do ix=1,ifl
c              write(90,*)ix,iy,vec_i(ix,iy),vec_j(ix,iy),vec_k(ix,iy)
c              CPHI(ix,iy)=(vec_i(ix,iy)*AQ)
c    *                    +(vec_j(ix,iy)*BQ)
c    *                    +(vec_k(ix,iy)*CQ)
c           enddo
c        enddo
c        write(0,*)ifl,ifs
         atmp=cphi*bll
         brightness=char(0)
         do iy=1,ifs
            do ix=1,ifl
               br=0.0
               if(bll(ix,iy).gt.0)br=ambient
               if(atmp(ix,iy).gt.0
     *           )then
                      abr=atones*dabs(cphi(ix,iy))
                      br=br+abr
               endif
               ibr=atones*br*amfactor
               brightness(ix,iy)=char(ibr)
            enddo
         enddo
c        where(atmp.gt.0)brightness=char(int((atones*dabs(cphi))+.5))
c        where(atmp.lt.0.and.bll .gt.0)brightness=char(100)
c
c      Brightness array has been defined - create bitmap
c
         call array2bmp(ifl,ifs,brightness)
      enddo
c
      stop
c
 99   CONTINUE
      write(0,100)
      stop
c
100   format('***** ERROR READING LL')
101   format(i3.3)
102   FORMAT(' NAME OF INPUT FILE? ')
103   FORMAT(A40)
104   FORMAT(6F9.3)
106   FORMAT(l1)
119   FORMAT('Length of surface_name too great',i6)
120   FORMAT('Light is defined to come from'
     *      ,' theta=',f8.2,' degrees, phi=',f8.2,' degrees')
121   FORMAT('Light is shining along the line of sight:'
     *      ,' theta=',f8.2,' degrees, phi=',f8.2,' degrees')
122   FORMAT('Light is shining along the line of sight (T/F)?',5f8.2)
123   FORMAT('Inclination of sun (two real degrees)')
1231  FORMAT('Theta?')
1232  FORMAT('Phi?')
124   FORMAT('Alter theta? T/F')
125   FORMAT('Alter phi? T/F')
126   FORMAT('Number of steps?')
127   FORMAT(//' ILLUMINATED FROM',3F10.2,/' CAMERA AT'
     1,/3F10.2,/' LENS ANGLE',F8.2,' DEGREES',/I7,' POINTS')
128   FORMAT(a,': dx=',f8.2,', dy=',f8.2,', dz=',f8.2
     *      ,', theta=',f7.1,', phi=',f7.1
     *      ,', aq=',f6.3,', bq=',f6.3,', cq=',f6.3)
      END
C
C
C
      SUBROUTINE EXPAND(m,A,B,C)
C
C      EXPANDS INTEGER INTO THREE UNIT VECTORS...
C
      IMPLICIT REAL *8 (A-H,O-Z)
      IF(m.LT.0
     *  )THEN
             N=-m
         ELSE
             N=m
      ENDIF
C
C      FIND SIGN OF C...
C
      IF(N.LT.2**30
     *  )THEN
             C=1.D0
         ELSE
             C=-1.D0
             N=N-(2**30)
      ENDIF
C
C      DECODE...
C
      I=N/32768
      A=(FLOAT(I)/16384.D0)-1.D0
      N=MOD(N,32768)
C
      B=(FLOAT(N)/16384.D0)-1.D0
C
C      USE UNIT VECTOR PROPERTY OF SQUARES SUMMING TO ONE,,,
C
      D=1.-(A*A)-(B*B)
      IF(D.LT.0.D0)D=0.D0
      C=C*SQRT(D)
C
c     write(*,100)m,a,b,c
100   format(i30,3f8.5)
      RETURN
      END
