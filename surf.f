      PROGRAM surf
C                                                                      
C      THIS PROGRAM PHOTOGRAPHS A SURFACE.                             
C      THE SURFACE IS DEFINED IN CARTESIAN CO-ORDINATES, AND HAS       
C      THE GENERAL FORM S(X,Y,Z)=0.  THE 'PHOTOGRAPH' IS DEFINED       
C      IN A NUMBER OF WAYS. THE CAMERA MAY BE HELD AT ANY POINT        
C      (X1,Y1,Z1), AND DIRECTED AT ANY OTHER POINT (X2,Y2,Z2).  THE    
C      HORIZONTAL VIEWING ANGLE, (FOCAL LENGTH), MAY BE SPECIFIED, AND 
C      ALSO THE SIZE OF THE PICTURE (GRAIN SIZE OF FILM, OR RESOLUTION)
C                                                                      
C      THE ONLY RESTRICTION ON CAMERA ALIGNMENT IS THAT THE BASE OF    
C      THE CAMERA IS ALWAYS PARALLEL TO THE X-Z PLANE.  THIS IS        
C      THE EQUIVALENT OF POINTING A CONVENTIONAL SLR UP, DOWN, OR AT   
C      ANY POINT, BUT ALWAYS KEEPING THE BOTTOM OF THE PICTURE FRAME   
C      HORIZONTAL.                                                     
C                                                                      
C      TO SHOW HOW THE SURFACE IS SPECIFIED, IMAGINE A SPHERE OF       
C      RADIUS 2. THIS WOULD NORMALLY BE WRITTEN                        
C      X**2 + Y**2 + Z**2 = 4.0.  FOR THE SAKE OF THE PROGRAM, EACH    
C      TERM CONSISTS OF THE INTEGER POWERS OF X,Y, AND Z, AND A         
C      COEFFICIENT. SO, THE SPHERE BECOMES:                            
C                                                                      
C          200 1.0                                                     
C          020 1.0                                                     
C          002 1.0                                                     
C          000-4.0                                                     
C          000 0.0                                                     
C                                                                      
C      THE LAST TERM DEFINES THE END OF THE SURFACE.                   
C      THE PROGRAM FINDS THE CO-ORDINATE OF EVERY POINT THAT THE CAMERA
C      CAN SEE, AND THE UNIT VECTORS OF THE SURFACE AT THAT POINT.  IT 
C      WRITES THIS INFORMATION AWAY IN BINARY FORMAT FOR PROCESSING BY A
C      SECOND PROGRAM.  THIS ILLUMINATES THE SURFACE BY A POINT SOURCE,
C      AND PRODUCES A HALFTONE REPRESENTATION. THE NUMBER OF GREY LEVELS
C      AND THE POSITION OF THE LIGHT MAY BE VARIED.                    
C                                                                      
C      THE SURFACE DRAWN HAS HIDDEN LINES REMOVED, AND IS A TRUE       
C      PERSPECTIVE REPRESENTATION.                                     
C                                                                      
C                                                                      
C                                                                      
      use surf_comms
      IMPLICIT REAL *8 (A-H,O-Z)                                       
C                                                                      
      DIMENSION                                                        
     *AJD(10,10),AJDA(10,10),AKD(10,10),AKDB(10,10,200),                
     *ALD(10,10),ALDA(10,10),AX(28),AXA(28),AXS(28,28),                
     *IPX(200),IPXA(200),IPXB(200),
     *IPY(200),IPYA(200),IPYB(200),IPZ(200),
     *IPZA(200),IPZB(200),IXB(10),         
     *IYB(10),IZB(10),PLIM(28),                                        
     *RA(10),SA(10),SC(200),SPX(200),SPY(200),
     *SPZ(200),TA(10),UA(10),VA(10),                                    
     *WA(10),XA(200),YA(200),ZA(200),NGUESS(7)                   
C                                                                      
      integer,allocatable,dimension(:) :: ll   

C                                                                      
      CHARACTER *40 VUFILE,SURFCE                                      
      CHARACTER *40 OUTFIL                                             
      CHARACTER *1 FF                                                  
C                                                                      
      COMMON /CA/AXS,PLIM,LCK                                          
      COMMON /CB/AX,IDEG                                               
      COMMON /CC/ACC,AXA,AXACON,MB                                     
      COMMON /STATS/IROW,JCOL,NGTYPE                                   
c      
c      Inititalise all arrays
c      
      AJD=0
      AJDA=0
      AKD=0
      AKDB=0
      ALD=0
      ALDA=0
      AX=0
      AXA=0
      AXS=0
      bc=0
      coeff=0.
      IPX=0
      IPXA=0
      IPXB=0
      IPY=0
      IPYA=0
      IPYB=0
      IPZ=0
      IPZA=0
      IPZB=0
      IXB=0
      IYB=0
      IZB=0
      PLIM=0
      RA=0
      SA=0
      SC=0
      SPX=0
      SPY=0
      SPZ=0
      TA=0
      UA=0
      VA=0
      WA=0
      XA=0
      YA=0
      ZA=0
      NGUESS=0
C                                                                      
      FF=CHAR(12)                                                      
C                                                                      
      WRITE(*,116)                                                     
      READ(*,*)SURFCE                                               
      write(*,115)surfce
      OPEN(3,FILE=SURFCE,STATUS='OLD',FORM='FORMATTED',ERR=99)         
      lchann=3
      call read_surface(lchann)    !...into coeff...
C                                                                      
      IDEG=0                                                           
      NTERMS=0                                                         
      do kza=0,9
         do kya=0,9
            do kxa=0,9
               if(coeff(kxa,kya,kza).ne.0.
     *           )then
                      nterms=nterms+1
                      ipx(nterms)=kxa
                      ipy(nterms)=kya
                      ipz(nterms)=kza
                       sc(nterms)=coeff(kxa,kya,kza)
                      i=kxa+kya+kza
                      IF(I.GT.IDEG)IDEG=I
               endif
            enddo
         enddo
      enddo
      CLOSE(3)                                                         
      do mx=1,nterms
         WRITE(*,400)mx,IPX(mx),IPY(mx),IPZ(mx),SC(mx)
      enddo
400   format(i3,i8,2i1,f8.1)
      LCK=IDEG+1                                                       
      MB=IDEG-1                                                        
      VDMAX=IBASE**(FLOAT(2**(LENEXP-3)-1)/FLOAT(LCK))                 
      IF(VDMAX.LT.VD)VD=VDMAX                                          
C                                                                      
C      SET UP PASCAL TRIANGLE OF BINARY COEFFICIENTS IN BC.          
C                                                                      
c     call binomial
C
      LCK=IDEG+1
      MB=IDEG-1
C
C      SET UP PASCAL TRIANGLE OF BINARY COEFFICIENTS IN BC.
C
      DO 15 I=1,LCK
      DO 15 J=2,LCK
  15  BC(I,J)=0.D0
C
      DO 19 I=1,LCK
  19  BC(I,1)=1.D0
C
      L=1
      DO 17 J=2,LCK
      K=J-1
      DO 18 I=J,LCK
      BC(I,J)=BC(K,J)+BC(K,L)
  18  K=I
  17  L=J

C                                                                      
      WRITE(*,117)                                                     
      READ(*,*)VUFILE                                               
C                                                                      
      OUTFIL=trim(surfce)//trim(vufile)//'.vec'
      OPEN(10,FILE=trim(OUTFIL),STATUS='unknown',FORM='UNFORMATTED'
     (       ,ERR=99)
C                                                                      
      OPEN(8,FILE=trim(OUTFIL)//'.TESDAT',FORM='FORMATTED'
     *      ,STATUS='unknown',ERR=99)
C                                                                      
      CALL TIM(TSTART)                                                 
      NTAT=0                                                           
      LCOUNT=0                                                         
      MCOUNT=0                                                         
C                                                                      
C      USEFUL/PERFECT GUESSES, SINGLETON, BINARY CHOPS                 
C                                                                      
      DO 4 I=1,7                                                       
      NGUESS(I)=0                                                      
  4   CONTINUE                                                         
C                                                                      
C      READ IN THE DATA                                                
C                                                                      
      MANTIS=55                                                        
      LENEXP=8                                                         
      IBASE=16                                                         
      ACC=1.D0-(IBASE/(2.D0**((MANTIS/IBASE)*IBASE)))                  
C                                                                      
      OPEN(7,FILE=VUFILE,STATUS='OLD',FORM='FORMATTED',ERR=99)         
         READ(7,104)ASPECT
         READ(7,104)XE
         READ(7,104)YE
         READ(7,104)ZE
         READ(7,104)VD
         READ(7,104)VDI
         READ(7,104)XCV
         READ(7,104)YCV
         READ(7,104)ZCV
         READ(7,*  )IFL
         ifl=(ifl/8)*8
         allocate(ll(ifl))
         ll=0   ! Inititalise LL
         READ(7,*  )IFS
         ifs=(ifs/8)*8
         READ(7,104)VUANGL
      CLOSE(7)                                                         
c
c      Calculate theta (x-z plane) and phi (y and x-z)
C
             rad2deg=180.0*113.0/355.0
             sdx=xe-xcv
             sdy=ye-ycv
             sdz=ze-zcv
             r=sqrt(sdx**2+sdy**2+sdz**2)
             if(sdx.ne.0
     *         )then
                    theta=atan(sdz/sdx)
                else
                    theta=355.0/226.0  ! pi/2
             endif
             phi=acos(sqrt(sdx**2+sdz**2)/r)
             phi  =rad2deg*phi
             theta=rad2deg*theta
             write(8,121)theta,phi,xe,ye,ze,xcv,ycv,zcv,sdx,sdy,sdz
C                                                                      
C      INITIALISE GUESS VALUES.                                        
C                                                                      
      XFINAL=.5*(VD+VDI)                                               
      SLOPE=1.D16                                                      
c     DO 10 I=1,10                                                     
      IXB=0                                                         
      IYB=0                                                         
      IZB=0                                                         
c10   CONTINUE                                                         
C                                                                      
      IPXM=0                                                           
      IPYM=0                                                           
      IPZM=0                                                           
      DO I=1,NTERMS                                                  
         IPXA(I)=IPX(I)+1                                                 
         IPYA(I)=IPY(I)+1                                                 
         IPZA(I)=IPZ(I)+1                                                 
         IPXB(I)=IPX(I)-1                                                 
         IPYB(I)=IPY(I)-1                                                 
         IPZB(I)=IPZ(I)-1                                                 
         IXB(IPXA(I))=1                                                   
         IYB(IPYA(I))=1                                                   
         IZB(IPZA(I))=1                                                   
         IF(IPXA(I).GT.IPXM)IPXM=IPXA(I)                                  
         IF(IPYA(I).GT.IPYM)IPYM=IPYA(I)                                  
         IF(IPZA(I).GT.IPZM)IPZM=IPZA(I)                                  
         S=SC(I)                                                          
         SPX(I)=S*FLOAT(IPX(I))                                           
         SPY(I)=S*FLOAT(IPY(I))                                           
         SPZ(I)=S*FLOAT(IPZ(I))                                           
         AKDB(1,1,I)=S                                                    
      enddo
C                                                                      
      IXCT=IFS/10                                                  
C                                                                      
      WRITE(8,127)(IPX(I),IPY(I),IPZ(I),SC(I),I=1,NTERMS)              
      WRITE(8,138)XE,YE,ZE                                             
      WRITE(8,139)XCV,YCV,ZCV                                          
      WRITE(8,140)VD,VDI                                               
      WRITE(8,141)IFL,IFS                                              
      WRITE(8,142)VUANGL                                               
      WRITE(8,143)ACC                                                  
      WRITE(8,136)FF                                                   
      WRITE(10)(SC(N),IPX(N),IPY(N),IPZ(N),N=1,100),XE,YE,ZE            
     1,XCV,YCV,ZCV,VD,VDI,IFL,IFS,VUANGL,ACC,NTERMS                    
C                                                                      
      RA(1)=1.                                                         
      SA(1)=1.                                                         
      TA(1)=1.                                                         
      UA(1)=1.                                                         
      VA(1)=1.                                                         
      WA(1)=1.                                                         
      AJD(1,1)=1.                                                      
      AKD(1,1)=1.                                                      
      ALD(1,1)=1.                                                      
      AJDA(1,1)=1.                                                     
      ALDA(1,1)=1.                                                     
C                                                                      
      call sf_COEFFA(IPXM,SA,XE,IXB,AJD)                                  
      call sf_COEFFA(IPYM,UA,YE,IYB,AKD)                                  
      call sf_COEFFA(IPZM,WA,ZE,IZB,ALD)                                  
C                                                                      
      A=XCV-XE                                                         
      B=YCV-YE                                                         
      C=ZCV-ZE                                                         
      D=IFL-1                                                          
      GL=2.*DTAN(VUANGL*3.141592654D0/360.)/D                          
CX    GS=GL*FLOAT(DISPLY(2)*DISPLY(5))/FLOAT(DISPLY(3)*DISPLY(4))      
      GS=ASPECT*GL                                                     
      AL=1.+(D*.5)                                                     
      AS=1.+(FLOAT(IFS-1)*.5)                                          
      IL=(IFL/2)+1                                                     
      IS=(IFS/2)+1                                                     
      D=(A*A)+(C*C)                                                    
      E=1./DSQRT(D+(B*B))                                              
      D=DSQRT(D)                                                       
      CPHI=-D*E                                                        
      D=1./D                                                           
      F=D*E                                                            
      SPHI=B*E                                                         
      CPSI=A*D                                                         
      SPSI=-C*D                                                        
      CC=A*E                                                           
      SS=B*C*F                                                         
      CPSS=-A*B*F                                                      
      CPHS=-C*E                                                        
C                                                                      
C      SET UP THE AREA OF THE PICTURE TO BE PHOTOGRAPHED...            
C                                                                      
      WRITE(*,103)                                                     
      READ(*,*  )ICHECK                                                
      IF(ICHECK.EQ.0
     *   )THEN                                              
               IROWMN=1
               IROWMX=IFS
               JCOLMN=1                                            
               JCOLMX=IFL                                          
          ELSE                                                  
               WRITE(*,106)                                        
               READ(*,109)IROWMN                                   
               READ(*,109)IROWMX                                   
               READ(*,109)JCOLMN                                   
               READ(*,109)JCOLMX                                   
               write(*,*)irowmn,irowmx,jcolmn,jcolmx
      ENDIF                                                            
C                                                                      
      WRITE(*,100)                                                     
      WRITE(8,100)                                                     
      DO 16 IROW=IROWMx,IROWMn,-1
C                                                                      
C      INITIALISE LL..                                                 
C                                                                      
      LL=0                                                         
C                                                                      
      IF((IROW/IXCT)*IXCT.EQ.IROW.or.icheck.ne.0
     *  )then
             WRITE(*,101)IROW,mcount,LCOUNT,(NGUESS(MX),MX=1,7)
             WRITE(8,101)IROW,mcount,LCOUNT,(NGUESS(MX),MX=1,7)
             mcount=0
      endif
      GM=(FLOAT(IROW)-AS)*GS                                           
      T=SPHI+(GM*CPHI)                                                 
      CQ=CC-(GM*CPSS)                                                  
      DQ=(GM*SS)-CPHS                                                  
C                                                                      
      K=1                                                              
      DO L=2,IPYM                                                   
         TA(L)=TA(K)*T
         IF(IYB(L).NE.0
     *     )THEN  
                DO M=1,L
                   AK=AKD(M,L)*TA(M)
                   DO N=1,NTERMS
                      AKDB(M,L,N)=AK*SC(N)    
                   enddo
                enddo
         ENDIF
         K=L
      enddo
C                                                                      
      DO 36 JCOL=JCOLMN,JCOLMX                                         
      GN=(FLOAT(JCOL)-AL)*GL                                           
      R=CQ+(GN*SPSI)                                                   
      V=DQ+(GN*CPSI)                                                   
C                                                                      
      call sf_COEFFB(IPXM,RA,R,IXB,AJDA,AJD)                              
      call sf_COEFFB(IPZM,VA,V,IZB,ALDA,ALD)                              
C                                                                      
      AX=0.
      DO N=1,NTERMS
         IXA=IPXA(N) 
         IYA=IPYA(N)
         IZA=IPZA(N)
         DO KA=1,IYA
            AK=AKDB(KA,IYA,N)
            DO JA=1,IXA  
               AJ=AJDA(JA,IXA)*AK
               DO LA=1,IZA
                  LC=JA+KA+LA-2
                  AX(LC)=AX(LC)+(ALDA(LA,IZA)*AJ)
                  enddo
               enddo
            enddo
         enddo
C                                                                      
C                                                                      
C      START OF ROUTINE TO SOLVE POLYNOMIAL IN AX, AND TRANLATE RESULT 
C      (FIRST POSITIVE ROOT) TO X,Y AND Z CO-ORDS AND UNIT VECTORS OF  
C      THE SURFCE.                                                     
C                                                                      
C                                                                      
C      NORMALISE F(X) AND CREATE F'(X) IN FIRST TWO ROWS OF AXS.       
C                                                                      
      call sf_STURGN                                                      
C                                                                      
C      REDUCE POSITIVE SEARCH LIMIT IF POSSIBLE.                       
C                                                                      
      IF(PLIM(1).LT.VD
     *  )THEN                                            
              PLIMX=PLIM(1)                               
         ELSE                                            
              PLIMX=VD                                    
      ENDIF                                                            
C                                                                      
C      THIS SECTION FINDS THE NUMBER OF SIGN CHANGES IN THE VALUE      
C      OF THE STURM EQUATION AT PLIMX, AND VDI                         
C                                                                      
      call sf_NC(PLIMX,NPLIMX,FPLIMX,RESA)                                
      call sf_NC(VDI,  NVDI,  FVDI,  RESA)                                
C                                                                      
C      CHECK FOR EXISTENCE OF REAL ROOTS - IF NONE, ABANDON SEARCH.    
C                                                                      
      IF(NPLIMX.EQ.NVDI)GO TO 35                                       
C                                                                      
C      PLACE NORMALISED POLYNOMIAL INTO AXA                            
C                                                                      
      M=1                                                              
      DO I=2,IDEG                                                   
         AXA(M)=AXS(I,1)                                                  
         M=I
      enddo         
      AXACON=AXS(LCK,1)                                                
C                                                                      
C      IF THERE'S ONLY ONE ROOT BETWEEN PLIMX AND VDI, GO AND FIND IT. 
C                                                                      
      NVDIA=NVDI-1                                                     
      IF(NPLIMX.EQ.NVDIA
     *  )THEN                                          
             XVDI=VDI                                
             NGTYPE=3                                      
             call sf_SOLVER(X,XVDI,FVDI,PLIMX,FPLIMX)   
             GO TO 26                                
      ENDIF                                                            
C                                                                      
C      START OF SECTION TO GUESS AT NEXT ROOT, USING ROOT OF LAST      
C      SOLVED POLYNOMIAL AS A STARTING POINT.                          
C      CALCULATE VALUE OF OTHER GUESS, HOPEFULLY ON OTHER SIDE OF ROOT.
C      (NEWTON'S METHOD)                                               
C      FIRST, EVALUATE POLYNOMIAL AT ROOT....                          
C                                                                      
      ROOT=XFINAL                                                      
C                                                                      
      FROOT=ROOT                                                       
      DO 1 I=1,MB                                                      
  1   FROOT=(FROOT+AXA(I))*ROOT                                        
      FROOT=FROOT+AXACON                                               
C                                                                      
      if(SLOPE.eq.0)slope=1.d16
      ROOTA=ROOT-(2.D0*FROOT/SLOPE)                                    
C                                                                      
C      GET ROOT AND ROOTA A KNOWN WAY ROUND, WITH ROOTA < ROOT         
C                                                                      
      IF(ROOTA.GT.ROOT) THEN                                           
          TEMP=ROOT                                                    
          ROOT=ROOTA                                                   
          ROOTA=TEMP                                                   
      ENDIF                                                            
C                                                                      
C      CONTRAIN ROOT AND ROOTA                                         
C                                                                      
      IF(ROOT .LT.VDI)   ROOT=VDI                                      
      IF(ROOT .GT.PLIMX) ROOT=PLIMX                                    
      IF(ROOTA.LT.VDI)  ROOTA=VDI                                      
      IF(ROOTA.GT.PLIMX)ROOTA=PLIMX                                    
                                                                       
C                                                                      
C      FIND NUMBER OF SIGN CHANGES AT ROOTA AND ROOT                   
C                                                                      
      call sf_NC(ROOTA,NROOTA,FROOTA,BETA)                                
      call sf_NC(ROOT ,NROOT ,FROOT ,BETA)                                
      IF(ICHECK.NE.0)WRITE(8,105)IROW,JCOL,                            
     *                 VDI,  ROOTA,  ROOT,  PLIMX,                     
     *                FVDI, FROOTA, FROOT, FPLIMX,                     
     *               NVDI,NROOTA,NROOT,NPLIMX                          
                                                                       
C                                                                      
C      IS ROOT TRAPPED?  IF NOT, USE STURM AND BINARY CHOP.            
C                                                                      
                                                                       
      IF(NVDIA.EQ.NROOTA)THEN                                          
                         XVDI=VDI                                      
                         NGTYPE=1                                      
                         call sf_SOLVER(X,XVDI,FVDI,ROOTA,FROOTA)         
                         GO TO 26                                      
      ENDIF                                                            
C                                                                      
C      MAYBE A PERFECT GUESS...?                                       
C                                                                      
      IF(NVDIA.EQ.NROOT)THEN                                           
                        NGTYPE=2                                       
                        call sf_SOLVER(X,ROOTA,FROOTA,ROOT,FROOT)         
                        GO TO 26                                       
      ENDIF                                                            
C                                                                      
C      NEXT SECTION USES STURM'S THEOREM TO FIND THE POINT AT WHICH    
C      THERE IS ONLY ONE ROOT BETWEEN XLOW AND XHIGH                   
C      IF THERE ARE NO ROOTS BETWEEN VDI AND ROOTA, THEN THEY MUST BE  
C      BETWEEN ROOTA AND PLIMX...                                      
C                                                                      
      IF(NVDI.GT.NROOTA)THEN                                           
                            XLOW=VDI                                   
                            FLOW=FVDI                                  
                           NLOW=NVDI                                   
                           XHIGH=ROOTA                                 
                           FHIGH=FROOTA                                
                          NHIGH=NROOTA                                 
                          NGTYPE=4                                     
      ELSEIF(NROOTA.GT.NROOT)THEN                                      
                            XLOW=ROOTA                                 
                            FLOW=FROOTA                                
                           NLOW=NROOTA                                 
                           FHIGH=FROOT                                 
                           XHIGH=ROOT                                  
                          NHIGH=NROOT                                  
                          NGTYPE=5                                     
                         ELSE                                          
                            XLOW=ROOT                                  
                            FLOW=FROOT                                 
                           NLOW=NROOT                                  
                           XHIGH=PLIMX                                 
                           FHIGH=FPLIMX                                
                          NHIGH=NPLIMX                                 
                          NGTYPE=6                                     
      ENDIF                                                            
C                                                                      
      ALPHA=-999.                                                      
      FXG=999.                                                         
C                                                                      
 30   CONTINUE                                                         
C                                                                      
C      BEFORE BINARY CHOPPING, CHECK THAT ALPHA AND RES AREN'T EQUAL.  
C      THIS COULD BE COINCIDENT ROOTS OR ERROR. EITHER WILL CAUSE      
C      AN INFINITE LOOP.....                                           
C      IF THIS OCCURS, JUMP OUT, AND ACCEPT THE CURRENT VALUE OF       
C      XG AS THE ROOT.                                                 
C                                                                      
      IF(ALPHA.EQ.FXG)THEN                                             
                    NGTYPE=7                                           
                    WRITE(8,119)                                       
     *                 VDI,  ROOTA,  ROOT,  PLIMX, XLOW,  XG, XHIGH    
     *               ,FVDI, FROOTA, FROOT, FPLIMX, FLOW, FXG, FHIGH    
     *               ,NVDI,NROOTA,  NROOT, NPLIMX, NLOW, NXG, NHIGH    
           GO TO 26                                                    
      ENDIF                                                            
      XG=.5D0*(XLOW+XHIGH)                                             
      ALPHA=FXG                                                        
C                                                                      
C      THIS SECTION FINDS THE NUMBER OF SIGN CHANGES IN THE VALUE      
C      OF THE STURM EQUATION AT XG.                                    
C                                                                      
      call sf_NC(XG,NXG,FXG,BETA)                                         
C                                                                      
C      MOVE THE SEARCH LIMITS...                                       
C                                                                      
      IF(NLOW.GT.NXG)THEN                                              
                         XHIGH=XG                                      
                         NHIGH=NXG                                     
                         FHIGH=FXG                                     
                     ELSE                                              
                         XLOW=XG                                       
                         NLOW=NXG                                      
                         FLOW=FXG                                      
      ENDIF                                                            
      IF(NLOW-NHIGH.GT.1)GO TO 30                                      
      IF(ICHECK.NE.0)WRITE(8,107)XLOW,XHIGH,FLOW,FHIGH,NLOW,NHIGH      
      call sf_SOLVER(X,XLOW,FLOW,XHIGH,FHIGH)                             
C                                                                      
 26   CONTINUE                                                         
      NGUESS(NGTYPE)=NGUESS(NGTYPE)+1           
c      
      if(nguess(ngtype).eq.1)write(90,200)ngtype,(axa(mx),mx=1,ideg-1)
200   format('Guess type: ',i2,28e20.12)
C                                                                      
C      ROOT HAS BEEN FOUND - CALCULATE GRADIENT OF POLYNOMIAL AT X.    
C                                                                      
      A=IDEG                                                           
      SLOPE=A*X                                                        
      DO 70 IMZ=1,MB-1                                                 
      A=A-1.D0                                                         
 70   SLOPE=(SLOPE+(A*AXA(IMZ)))*X                                     
      SLOPE=SLOPE+AXA(MB)                                              
C                                                                      
C      START TO CALCULATE X,Y,Z CO-ORDS AND UNIT VECTORS OF SURFCE     
C      CORRESPONDING TO X.                                             
C                                                                      
      XFINAL=X                                                         
C                                                                      
C      CALCULATE THE VALUE OF FUNCTION AT XFINAL, PRINT IT IF REQUESTED.
C                                                                      
      IF(ICHECK.NE.0)THEN                                              
                   Y=X                                                 
                   DO 2 I=1,MB                                         
  2                      Y=(Y+AXA(I))*X                                
                   Y=Y+AXACON                                          
                   WRITE(8,123)X,Y,SLOPE,NGTYPE                        
      ENDIF                                                            
C                                                                      
      A=X*R                                                            
      B=X*T                                                            
      C=X*V                                                            
      XN=A+XE                                                          
      YN=B+YE                                                          
      ZN=C+ZE                                                          
      DQDX=0.                                                          
      DQDY=0.                                                          
      DQDZ=0.                                                          
C                                                                      
      call sf_COEFFC(XA,IPXM,XN)
      call sf_COEFFC(YA,IPYM,YN)
      call sf_COEFFC(ZA,IPZM,ZN)
C                                                                      
      DO 83 K=1,NTERMS                                                 
      IF(IPX(K).NE.0)DQDX=DQDX+SPX(K)*XA(IPX(K))*YA(IPYA(K))*ZA(IPZA(K))
      IF(IPY(K).NE.0)DQDY=DQDY+SPY(K)*YA(IPY(K))*ZA(IPZA(K))*XA(IPXA(K))
      IF(IPZ(K).NE.0)DQDZ=DQDZ+SPZ(K)*ZA(IPZ(K))*XA(IPXA(K))*YA(IPYA(K))
  83  CONTINUE                                                         
C                                                                      
      LCOUNT=LCOUNT+1                                                  
      MCOUNT=MCOUNT+1                                                  
C                                                                      
C      NORMALISE VECTORS...                                            
C                                                                      
      QA=1./DSQRT((DQDX*DQDX)+(DQDY*DQDY)+(DQDZ*DQDZ))                 
      DQDX=QA*DQDX                                                     
      DQDY=QA*DQDY                                                     
      DQDZ=QA*DQDZ                                                     
C                                                                      
C      COMPRESS DATA...                                                
C                                                                      
      if(icheck.ne.0
     *  )then
             write(8,144)dqdx,dqdy,dqdz
      endif
      call sf_CMPRES(NCOMP,DQDX,DQDY,DQDZ)                                
c     if(icheck.ne.0
c    *  )then
c            write(8,145)a*a,b*b,c*c
c     endif
C                                                                      
      Dtemp=DSQRT((A*A)+(B*B)+(C*C))                                   
      if(dtemp.gt.0.0
     *  )then
             d=-1.0/dtemp
         else
             D=1000000.
      endif
      A=A*D                                                            
      B=B*D                                                            
      C=C*D                                                            
      D=(A*DQDX)+(B*DQDY)+(C*DQDZ)                                     
      IF(D.LT.0.0)NCOMP=-NCOMP                                         
      LL(JCOL)=NCOMP                                                   
 35   CONTINUE                                                         
 36   CONTINUE                                                         
c     IF(LCOUNT.GT.0
c    *  )then
             WRITE(10)LL
c            write(*,*)irow
c     endif
 16   CONTINUE                                                         
      WRITE(8,136)FF                                                   
      WRITE(8,125)LCOUNT                                               
      WRITE(8,102)NGUESS                                               
      CALL TIM(TEND)                                                   
      TDIFF=TEND-TSTART                                                
      WRITE(8,130)TDIFF                                                
      WRITE(*,130)TDIFF
      STOP                                                             
 99   CONTINUE                                                         
      write(0,110)
      stop
100   FORMAT(
     */' Next  Points  Points  Useful Perfect Singltn',
     * '     ---Binary chops---    Sturm',
     */' Line   extra  so far Guesses Guesses   Roots',
     * '     Low     Med    High   Fails',/)
101   FORMAT(i5,12i8)
102   FORMAT(I35,'  USEFUL GUESSES',/I35,' PERFECT GUESSES',/          
     *I35,' SINGLETON ROOTS',/I35,' LOW BINARY CHOPS',/                
     *I35,' MED BINARY CHOPS',/I35,' HIGH BINARY CHOPS',/              
     *I35,' STURM FAILURES')                                           
103   FORMAT(' CHECK LEVEL?')                                          
104   FORMAT(F17.10)                                                   
105   FORMAT(/' ROW',I4,', COLUMN',I4,2(/4E18.10),/4I18)               
106   FORMAT(' SUPPLY IROWMN, IROWMX, JCOLMN, JCOLMX ')                
107   FORMAT('XLOW,XHIGH,FLOW,FHIGH,NLOW,NHIGH'
     *      ,/2(18X,2E18.10,/),18X,2I18)                                
109   FORMAT(I3)                                                       
110   FORMAT('ERROR OPENING FILE')
111   FORMAT(f8.3,/f8.3)
114   FORMAT(2F39.30)                                                  
115   FORMAT(A15)                                                      
116   FORMAT(' SURFACE FILE NAME?')                                    
117   FORMAT(' VIEW FILE NAME?')                                       
118   FORMAT(' OUTPUT FILE NAME?')                                     
119   FORMAT(2(/7E11.3),/7I11,//)                                      
121   FORMAT('SURF: theta=',f8.3,', phi=',f8.3,/9f12.3)
123   FORMAT(3E18.10,'    X, Y, DY/DX...',/'SOLUTION TYPE',I3)         
124   FORMAT(F12.9)                                                    
125   FORMAT(/,' CO-ORDS AND VECTORS FOUND!',I8,' POINTS')             
127   FORMAT(' SURFACE IS ',/100(1X,3I1,F16.2,/))                       
130   FORMAT(/' EXECUTION TIME=',F10.3,' SECONDS',/)                       
135   FORMAT(3I1,F16.2)                                                
136   FORMAT(A1)                                                       
138   FORMAT(/' EYEPOINT IS',3F10.3)                                   
139   FORMAT(/' CENTRE VIEWPOINT IS ',3F10.3)                          
140   FORMAT(/' MAX AND MIN VIEW LIMITS ARE',F10.3,' AND',F10.3)       
141   FORMAT(/' HORIZONTAL AND VERTICAL POINTS',2I6)                   
142   FORMAT(/' HORIZONTAL VIEWING ANGLE IS',F10.3,' DEGREES')         
143   FORMAT(/' ACCURACY IS  ',F23.20)                                 
144   format('Normalised Vectors:'
     *      ,' dqdx=',e18.10,', dqdy=',e18.10,', dqdz=',e18.10)
145   format('Squared Vectors:'
     *      ,' a=',e18.10,', b=',e18.10,', c=',e18.10)
      END                                                              
