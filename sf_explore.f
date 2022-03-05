      PROGRAM sf_explore
C                                                                      
C      THIS PROGRAM EXPLORES A SURFACE.                             
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
C      TERM CONISTS OF THE INTEGER POWERS OF X,Y, AND Z, AND A         
C      COEFFICIENT. SO, THE SPHERE BECOMES:                            
C                                                                      
C          200 1.0                                                     
C          020 1.0                                                     
C          002 1.0                                                     
C          000-4.0                                                     
C                                                                      
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
      IMPLICIT REAL *8 (A-H,O-Z)                                       
C                                                                      
      DIMENSION                                                        
     *AJD(10,10),AJDA(10,10),AKD(10,10),AKDB(10,10,100),                
     *ALD(10,10),ALDA(10,10),AX(28),AXA(28),AXS(28,28),                
     *BC(28,28),                                                       
     *IPX(100),IPXA(100),IPXB(100),                                       
     *IPY(100),IPYA(100),IPYB(100),IPZ(100),                               
     *IPZA(100),IPZB(100),IXB(10),                                       
     *IYB(10),IZB(10),PLIM(28),                                        
     *RA(10),SA(10),SC(100),SPX(100),SPY(100),                            
     *SPZ(100),TA(10),UA(10),VA(10),                                    
     *WA(10),XA(100),YA(100),ZA(100),LL(1024),NGUESS(7)                   
C                                                                      
      CHARACTER *15 VUFILE,SURFCE                                      
      CHARACTER *11 OUTFIL                                             
      CHARACTER *1 FF                                                  
C                                                                      
      COMMON /CA/AXS,PLIM,LCK                                          
      COMMON /CB/AX,IDEG                                               
      COMMON /CC/ACC,AXA,AXACON,MB                                     
      COMMON /CP/BC                                                    
      COMMON /STATS/IROW,JCOL,NGTYPE                                   
C                                                                      
      FF=CHAR(12)                                                      
C                                                                      
      WRITE(*,116)                                                     
      READ (*,*)SURFCE                                               
      write(*,115)surfce
      OPEN(3,FILE=SURFCE,STATUS='OLD',FORM='FORMATTED',ERR=99)         
C                                                                      
      IDEG=0                                                           
      NTERMS=0                                                         
   46 CONTINUE                                                         
      NTERMS=NTERMS+1                                                  
      READ(3,135,end=47)IPX(NTERMS),IPY(NTERMS),IPZ(NTERMS),SC(NTERMS)
      I=IPX(NTERMS)+IPY(NTERMS)+IPZ(NTERMS)                            
      IF(I.EQ.0.AND.SC(NTERMS).EQ.0.)GO TO 47                          
      IF(I.GT.IDEG)IDEG=I                                              
      GO TO 46                                                         
   47 CONTINUE                                                         
      CLOSE(3)                                                         
      NTERMS=NTERMS-1                                                  
C                                                                      
      LCK=IDEG+1                                                       
      MB=IDEG-1                                                        
      VDMAX=IBASE**(FLOAT(2**(LENEXP-3)-1)/FLOAT(LCK))                 
      IF(VDMAX.LT.VD)VD=VDMAX                                          
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
C                                                                      
      OPEN(8,FILE='TESDAT',FORM='FORMATTED',STATUS='unknown',ERR=99)       
C                                                                      
C      READ IN THE DATA                                                
C                                                                      
      MANTIS=55                                                        
      LENEXP=8                                                         
      IBASE=16                                                         
      ACC=1.D0-(IBASE/(2.D0**((MANTIS/IBASE)*IBASE)))                  
c
      aspect=1.0
      vd=1000.
      vdi=0.
      xcv=0.
      ycv=0.
      zcv=0.
      ifl=400
      ifs=400
      vuangl=45.
C                                                                      
c     READ(7,104)ASPECT                                                
c     READ(7,104)XE                                                    
c     READ(7,104)YE                                                    
c     READ(7,104)ZE                                                    
c     READ(7,104)VD                                                    
c     READ(7,104)VDI                                                   
c     READ(7,104)XCV                                                   
c     READ(7,104)YCV                                                   
c     READ(7,104)ZCV                                                   
c     READ(7,*  )IFL                                                   
c     ifl=(ifl/8)*8
c     READ(7,*  )IFS                                                   
c     ifs=(ifs/8)*8
c     READ(7,104)VUANGL                                                
c     CLOSE(7)                                                         
C                 
      deg2rad=355./(113.*180.)
      read(*,*)radius
      read(*,*)nsteps
      dtheta=710./float(113*nsteps)
      dphi  =dtheta*sqrt(2.)
      do ns=0,nsteps-1
C                                                                      
      NTAT=0                                                           
      LCOUNT=0                                                         
      MCOUNT=0                                                         
C                                                                      
C      USEFUL/PERFECT GUESSES, SINGLETON, BINARY CHOPS                 
C                                                                      
      DO 4 I=1,7                                                       
      NGUESS(I)=0                                                      
  4   CONTINUE                                                         
         CALL TIM(TSTART)                                                 
         theta=float(ns)*dtheta
         phi  =float(ns)*dphi
         xe=radius*cos(theta)
         ye=radius*sin(phi)
         ze=radius*sin(theta)
         dq=1./sqrt((xe*xe)+(ye*ye)+(ze*ze))
         aq=xe*dq
         bq=ye*dq
         cq=ze*dq
      write(*,108)ns,jcol,irow,aq,bq,cq
         write(OUTFIL,111)ns
         OPEN(10,FILE=OUTFIL,STATUS='unknown',FORM='UNFORMATTED',ERR=99)
c
C      INITIALISE GUESS VALUES.                                        
C                                                                      
      XFINAL=.5*(VD+VDI)                                               
      SLOPE=1.D16                                                      
C                                                                      
      DO 10 I=1,10                                                     
      IXB(I)=0                                                         
      IYB(I)=0                                                         
      IZB(I)=0                                                         
 10   CONTINUE                                                         
C                                                                      
      IPXM=0                                                           
      IPYM=0                                                           
      IPZM=0                                                           
      DO 9 I=1,NTERMS                                                  
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
    9 CONTINUE                                                         
C                                                                      
      IXCT=1+(IFS/10)                                                  
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
c     READ(*,*  )ICHECK                                                
      ICHECK=0
      IF(ICHECK.EQ.0)THEN                                              
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
      ENDIF                                                            
C                                                                      
      WRITE(*,100)                                                     
      DO 16 IROW=IROWMx,IROWMn,-1
C                                                                      
C      INITIALISE LL..                                                 
C                                                                      
      DO 3 MX=1,IFL                                                    
      LL(MX)=0                                                         
  3   CONTINUE                                                         
C                                                                      
      IF((IROW/IXCT)*IXCT.EQ.IROW)WRITE(*,101)                         
     *IROW,LCOUNT,(NGUESS(MX),MX=1,7)                                  
      GM=(FLOAT(IROW)-AS)*GS                                           
      T=SPHI+(GM*CPHI)                                                 
      CQ=CC-(GM*CPSS)                                                  
      DQ=(GM*SS)-CPHS                                                  
C                                                                      
      K=1                                                              
      DO 20 L=2,IPYM                                                   
      TA(L)=TA(K)*T                                                    
      IF(IYB(L).NE.0)THEN                                              
         DO 40 M=1,L                                                   
         AK=AKD(M,L)*TA(M)                                             
       DO 40 N=1,NTERMS                                                
       AKDB(M,L,N)=AK*SC(N)                                            
 40      CONTINUE                                                      
      ENDIF                                                            
   20 K=L                                                              
C                                                                      
      DO 36 JCOL=JCOLMN,JCOLMX                                         
      GN=(FLOAT(JCOL)-AL)*GL                                           
      R=CQ+(GN*SPSI)                                                   
      V=DQ+(GN*CPSI)                                                   
C                                                                      
      call sf_COEFFB(IPXM,RA,R,IXB,AJDA,AJD)                              
      call sf_COEFFB(IPZM,VA,V,IZB,ALDA,ALD)                              
C                                                                      
      DO 50 M=1,LCK                                                    
 50   AX(M)=0.                                                         
      DO 51 N=1,NTERMS                                                 
      IXA=IPXA(N)                                                      
      IYA=IPYA(N)                                                      
      IZA=IPZA(N)                                                      
      DO 51 KA=1,IYA                                                   
      AK=AKDB(KA,IYA,N)                                                
      DO 51 JA=1,IXA                                                   
      AJ=AJDA(JA,IXA)*AK                                               
      DO 51 LA=1,IZA                                                   
      LC=JA+KA+LA-2                                                    
      AX(LC)=AX(LC)+(ALDA(LA,IZA)*AJ)                                  
 51   CONTINUE                                                         
C                                                                      
C                                                                      
C      START OF ROUTINE TO SOLVE POLYNOMIAL IN AX, AND TRANLATE RESULT 
C      (FIRST POSITIVE ROOT) TO X,Y AND Z CO-ORDS AND UNIT VECTORS OF  
C      THE SURFCE.                                                     
C                                                                      
C                                                                      
C      NORMALISE F(X) AND CREATE F_prime(X) IN FIRST TWO ROWS OF AXS.       
C                                                                      
      call sf_STURGN                                                      
C                                                                      
C      REDUCE POSITIVE SEARCH LIMIT IF POSSIBLE.                       
C                                                                      
      IF(PLIM(1).LT.VD)THEN                                            
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
      DO 28 I=2,IDEG                                                   
      AXA(M)=AXS(I,1)                                                  
      M=I                                                              
 28   CONTINUE                                                         
      AXACON=AXS(LCK,1)                                                
C                                                                      
C      IF THERE IS ONLY ONE ROOT BETWEEN PLIMX AND VDI, GO AND FIND IT. 
C                                                                      
      NVDIA=NVDI-1                                                     
      IF(NPLIMX.EQ.NVDIA)THEN                                          
                               XVDI=VDI                                
                         NGTYPE=3                                      
                               call sf_SOLVER(X,XVDI,FVDI,PLIMX,FPLIMX)   
                               GO TO 26                                
      ENDIF                                                            
C                                                                      
C      START OF SECTION TO GUESS AT NEXT ROOT, USING ROOT OF LAST      
C      SOLVED POLYNOMIAL AS A STARTING POINT.                          
C      CALCULATE VALUE OF OTHER GUESS, HOPEFULLY ON OTHER SIDE OF ROOT.
C      (NEWTON METHOD)                                               
C      FIRST, EVALUATE POLYNOMIAL AT ROOT....                          
C                                                                      
      ROOT=XFINAL                                                      
C                                                                      
      FROOT=ROOT                                                       
      DO 1 I=1,MB                                                      
  1   FROOT=(FROOT+AXA(I))*ROOT                                        
      FROOT=FROOT+AXACON                                               
C                                                                      
      ROOTA=ROOT-(2.D0*FROOT/SLOPE)                                    
C                                                                      
C      GET ROOT AND ROOTA A KNOWN WAY ROUND, WITH ROOTA LESS THAN ROOT         
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
C      NEXT SECTION USES STURM THEOREM TO FIND THE POINT AT WHICH    
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
C      BEFORE BINARY CHOPPING, CHECK THAT ALPHA AND RES AREN NOT EQUAL.  
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
C                                                                      
C      NORMALISE VECTORS...                                            
C                                                                      
      QA=1./DSQRT((DQDX*DQDX)+(DQDY*DQDY)+(DQDZ*DQDZ))                 
      DQDX=QA*DQDX                                                     
      DQDY=QA*DQDY                                                     
      DQDZ=QA*DQDZ                                                     
c     brightness(jcol,irow)=(aq*dqdx)+(bq*dqdy)+(cq*dqdz)
      cphi                 =(aq*dqdx)+(bq*dqdy)+(cq*dqdz)
c     write(*,108)ns,jcol,irow,aq,bq,cq,dqdx,dqdy,dqdz,cphi
c     if(cphi.gt.1.0)read(*,*)junk
C                                                                      
C      COMPRESS DATA...                                                
C                                                                      
c     call sf_CMPRES(NCOMP,DQDX,DQDY,DQDZ)                                
C                                                                      
c     D=-1./DSQRT((A*A)+(B*B)+(C*C))                                   
c     A=A*D                                                            
c     B=B*D                                                            
c     C=C*D                                                            
c     D=(A*DQDX)+(B*DQDY)+(C*DQDZ)                                     
c     IF(D.LT.0.0)NCOMP=-NCOMP                                         
c     LL(JCOL)=NCOMP                                                   
 35   CONTINUE                                                         
 36   CONTINUE                                                         
      IF(LCOUNT.GT.0)WRITE(10)(LL(MX),MX=1,IFL)                        
 16   CONTINUE                                                         
      CLOSE(10)
      WRITE(8,136)FF                                                   
      WRITE(8,125)LCOUNT                                               
      WRITE(8,102)NGUESS                                               
      CALL TIM(TEND)                                                   
      TDIFF=TEND-TSTART                                                
      WRITE(8,130)TDIFF                                                
      WRITE(*,130)TDIFF
c
      enddo
c
      STOP                                                             
 99   CONTINUE                                                         
      write(0,110)
      stop
100   FORMAT(
     */'    Next  Points                            Low    Med   High',
     */'    Line    so    Useful Perfect Snglton  Binary Binary Binary',
     * '   Sturm',
     */'  Number    Far  Guesses Guesses   Roots   Chops  Chops  Chops',
     * '   Fails',/)
101   FORMAT(9i8)
102   FORMAT(I35,'  USEFUL GUESSES',/I35,' PERFECT GUESSES',/          
     *I35,' SINGLETON ROOTS',/I35,' LOW BINARY CHOPS',/                
     *I35,' MED BINARY CHOPS',/I35,' HIGH BINARY CHOPS',/              
     *I35,' STURM FAILURES')                                           
103   FORMAT(' CHECK LEVEL?')                                          
104   FORMAT(F17.10)                                                   
105   FORMAT(/' ROW',I4,', COLUMN',I4,2(/4E18.10),/4I18)               
106   FORMAT(' SUPPLY IROWMN, IROWMX, JCOLMN, JCOLMX ')                
107   FORMAT(2(18X,2E18.10,/),18X,2I18)                                
108   format(3i8,8f12.6)
109   FORMAT(I3)                                                       
110   FORMAT('ERROR OPENING FILE')
111   FORMAT('EXPLORE',3i4.4)
114   FORMAT(2F39.30)                                                  
115   FORMAT(A15)                                                      
116   FORMAT(' SURFACE FILE NAME?')                                    
117   FORMAT(' VIEW FILE NAME?')                                       
118   FORMAT(' OUTPUT FILE NAME?')                                     
119   FORMAT(2(/7E11.3),/7I11,//)                                      
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
      END                                                              
