      PROGRAM SHINEBMP
C
      use bmp_comms
c
      IMPLICIT REAL *8 (A-H,O-Z)
      CHARACTER *24 INPFILE
      CHARACTER *3 step_id
      DIMENSION SC(100),IPX(100),IPY(100),IPZ(100)
C
      allocatable :: LL(:)
      character(1),allocatable :: brightness(:,:)
      real(8),allocatable,dimension(:,:) :: vec_i,vec_j,vec_k,cphi,atmp
     *                                     ,bll
      real(8)inclination
c
      character(1)rgb(0:1023)
      character(1024)rgbq
      equivalence(rgb,rgbq)
c
      write(*,102)
      read(*,103)INPFILE
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
      atones=255
      nrow=0
  1   continue
         read(2,ERR=99,end=3)LL
         nrow=nrow+1
         write(*,*)nrow
         DO ncol=1,IFL
            if(ll(ncol).ne.0
     *        )then
                   CALL EXPAND(LL(ncol)
     *             ,vec_i(ncol,nrow),vec_j(ncol,nrow),vec_k(ncol,nrow))
            endif
            bll(ncol,nrow)=ll(ncol)
         enddo
         go to 1
   3  continue
      close(2)
C
      do n=0,255
         m=n*4
         rgb(m  )=char(n)
         rgb(m+1)=char(n)
         rgb(m+2)=char(n)
         rgb(m+3)=char(0)
      enddo
      rgbquad=rgbq
c
      write(6,123) ! Direction of light
      read(5,104)theta
      read(5,104)phi
      read(5,*)nsteps
      delta=360./float(nsteps)
      deg2rad=355.0/(113.0*180.0)  ! 2*pi/360
C
      do ns=0,nsteps-1
         write(step_id,101)ns
         bmname='shine'//step_id
C
         theta=theta+delta
         phi  =phi  +delta
         aq=cos(deg2rad*theta)
         bq=sin(deg2rad*phi)
         cq=sin(deg2rad*theta)
         write(*,104)aq,bq,cq
         CPHI=(vec_i*AQ)+(vec_j*BQ)+(vec_k*CQ)
         atmp=cphi*bll
         do nrow=1,ifs
            do ncol=1,ifl
               IF(atmp(ncol,nrow).gt.0.
     *           )THEN
                      mx=(atones*dabs(cphi(nrow,ncol)))+.5
                      brightness(ncol,nrow)=char(mx)
                  else
                      brightness(ncol,nrow)=char(0)
               ENDIF
            enddo
         enddo
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
103   FORMAT(A24)
104   FORMAT(6F9.3)
123   FORMAT('Inclination of sun (two real degrees),'
     *      ,' then integer number of steps?')
127   FORMAT(//' ILLUMINATED FROM',3F10.2,/' CAMERA AT'
     1,/3F10.2,/' LENS ANGLE',F8.2,' DEGREES',/I7,' POINTS')
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
