      PROGRAM SHINEBMP
C
      IMPLICIT REAL *8 (A-H,O-Z)
      CHARACTER *40 INPFILE
      CHARACTER *1 TXT(3072),bm1,bm2,rc,gc,bc,null,infohead(26)
      integer *2 xhot,yhot,xside,yside,planes,bpp
      integer *4 lfix,offbits,lhead
      integer,allocatable,dimension(:) :: ll !DIMENSION LL(1024)
      COMMON /CC/SC(100),IPX(100),IPY(100),IPZ(100),XA,YA,ZA
     *,XCV,YCV,ZCV,VD,VDI,IFL,IFS,VA,ACC,NTERMS
C
      null=char(0)
C
      write(*,102)
      read(*,103)INPFILE
      OPEN(10,FILE=INPFILE,FORM='UNFORMATTED',STATUS='OLD',ERR=99)
      REWIND 10
C
  1   CONTINUE
      write(6,123) ! Direction of light
      read(5,*)XE
      IF(XE.EQ.-1234.)STOP
      read(5,*)YE
      read(5,*)ZE
C
      DQ=1./SQRT((XE*XE)+(YE*YE)+(ZE*ZE))
      AQ=XE*DQ
      BQ=YE*DQ
      CQ=ZE*DQ
C
      REWIND 10
      LCOUNT=0
      CALL INITread
c
      allocate(ll(ifl))
c
      lrecl=ifl*3
      bm1='B'
      bm2='M'
      lfix=26
      xhot=0
      yhot=0
      offbits=lrecl
      lhead=12
      xside=ifl
      yside=ifs
      planes=1
      bpp=24
       atones=255
      OPEN(8,FILE='TEST.BMP',FORM='unFORMATTED',access='direct',
     *       recl=lrecl,err=99)
c
c      Write out header structure
c
c      write(*,200)
200      format(' Write out header')
      write(8,rec=1)bm1,bm2,lfix,xhot,yhot,offbits,lhead,xside,yside,
     *         planes,bpp,(txt(mx),mx=1,lrecl-26)
C
      DO 7 MX=1,3072
      TXT(MX)=null
  7   CONTINUE
C
      nline=1
  4   CONTINUE
      read(10,END=3,ERR=99)(LL(MX),MX=1,IFL)
       nline=nline+1
      nel=0
      DO 2 I=1,IFL
         NCOMP=LL(I)
         IF(NCOMP.EQ.0)THEN
                           nel=nel+1
                           txt(nel)=null
                           nel=nel+1
                           txt(nel)=null
                           nel=nel+1
                           txt(nel)=null
                           GO TO 5
         ENDIF
         LCOUNT=LCOUNT+1
         IF(NCOMP.LT.0)THEN
                           J=-NCOMP
                       ELSE
                           J=NCOMP
         ENDIF
         CALL EXPAND(J,A,B,C)
         CPHI=(A*AQ)+(B*BQ)+(C*CQ)
         atmp=cphi*float(ncomp)
         IF(atmp.GE.0.)THEN
                           mx=(atones*dabs(cphi))+.5
c
c        write(*,108)cphi,ncomp,atmp,atones,mx
                           rc=char(mx)
                           gc=char(mx)
                           bc=char(mx)
c
                           nel=nel+1
                           txt(nel)=rc
                           nel=nel+1
                           txt(nel)=gc
                           nel=nel+1
                           txt(nel)=bc
                               ELSE
                           nel=nel+1
                           txt(nel)=null
                           nel=nel+1
                           txt(nel)=null
                           nel=nel+1
                           txt(nel)=null
      ENDIF
  5   continue
c     write(*,100)ired,iblu,igrn,i,ifl
  2   continue
c     write(*,201)nline,nel
201   format(' Write out data line',i5,', length',i5)
      write(8,rec=nline)(txt(mx),mx=1,lrecl)
      go to 4
  3   continue
      write(*,125)LCOUNT
c     GO TO 1
c
 99   CONTINUE
100   format(10i6)
101   FORMAT(I2)
102   FORMAT(' NAME OF INPUT FILE? ')
103   FORMAT(A24)
104   FORMAT(F9.3)
105   FORMAT(3072a1)
106   FORMAT(2I14,4F10.6,I10)
107   FORMAT(/////A1)
108   format('cphi,ncomp,cphi*dfloat(ncomp),atones,mx'
     *     ,f9.4,i20,2f18.4,i6)
109   FORMAT(I3)
110   format(3a1)
125   FORMAT(' PICTURE COMPLETE',I7,' POINTS')
123   FORMAT(' DIRECTION OF LIGHT? (3 REAL CO-ORDS)',/)
127   FORMAT(//' ILLUMINATED FROM',3F10.2,/' CAMERA AT'
     1,/3F10.2,/' LENS ANGLE',F8.2,' DEGREES',/I7,' POINTS')
128   FORMAT(3F9.3,F10.4,2F8.4,3I6)
      END
C
      SUBROUTINE INITread
      COMMON /CC/SC(100),IPX(100),IPY(100),IPZ(100),XA,YA,ZA
     1,XCV,YCV,ZCV,VD,VDI,IFL,IFS,VA,ACC,NTERMS
      DOUBLE PRECISION ACC,SC,VA,VD,VDI,XA,XCV,YA,YCV,ZA,ZCV
C
      read(10)(SC(N),IPX(N),IPY(N),IPZ(N),N=1,100),XA,YA,ZA
     1,XCV,YCV,ZCV,VD,VDI,IFL,IFS,VA,ACC,NTERMS
C
      RETURN
      END
C
C
C
      SUBROUTINE EXPAND(N,A,B,C)
C
C      EXPANDS INTEGER INTO THREE UNIT VECTORS...
C
      IMPLICIT REAL *8 (A-H,O-Z)
C
C      FIND SIGN OF C...
C
      IF(N.LT.2**30)THEN
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
      RETURN
      END
