      SUBROUTINE sf_STURGN                                                
      COMMON /CA/AXS,PLIM,LCK                                          
      COMMON /CB/AX,IDEG                                               
      DIMENSION AX(28),AXS(28,28),PLIM(28)                             
      DOUBLE PRECISION A,ALPHA,AMP,APP,AX,AXS,B,BETA,PLIM              
C                                                                      
C                                                                      
C      NORMALISE F(X) AND CREATE F'(X) IN FIRST TWO ROWS OF AXS.       
C                                                                      
      A=0.D0                                                           
      B=1.D0/AX(LCK)                                                   
      L=LCK                                                            
      DO 36 I=1,IDEG                                                   
      AXS(L,1)=AX(I)*B                                                 
      AXS(L,2)=AX(I)*B*A                                               
      A=I                                                              
      L=L-1                                                            
  36  CONTINUE                                                         
C                                                                      
C      MAKE SURE THAT THE FIRST TERM OF F(X) IS EXACTLY ONE.....       
C      AND OF F'(X) IS EXACTLY IDEG.                                   
C                                                                      
      AXS(1,1)=1.D0                                                    
      AXS(1,2)=IDEG                                                    
C                                                                      
C      FIND REMAINDERS, AND CREATE REST OF STURM MATRIX                
C                                                                      
      L=IDEG                                                           
      L1=LCK                                                           
      J=1                                                              
      J1=2                                                             
      DO 37 J2=3,LCK                                                   
      AXS(L,J2)=0.D0                                                   
      A=1.D0/AXS(1,J1)                                                 
      ALPHA=AXS(1,J)*A                                                 
      BETA=(AXS(2,J)-(ALPHA*AXS(2,J1)))*A                              
      I=1                                                              
      I1=2                                                             
      DO 39 I2=3,L1                                                    
      AXS(I,J2)=-AXS(I2,J)+(AXS(I1,J1)*BETA)+(ALPHA*AXS(I2,J1))        
      I=I1                                                             
      I1=I2                                                            
  39  CONTINUE                                                         
      J=J1                                                             
      J1=J2                                                            
      L1=L                                                             
      L=L-1                                                            
  37  CONTINUE                                                         
C                                                                      
C     FIND LIMIT OF +VE ROOTS OF STURM EQUATION                        
C                                                                      
      L=LCK                                                            
      DO 24 IAX=1,IDEG                                                 
      APP=0.                                                           
      AMP=0.                                                           
      PLIM(IAX)=0.                                                     
      B=1.D0/AXS(1,IAX)                                                
      DO 29 N=2,L                                                      
      A=AXS(N,IAX)*B                                                   
      IF(A.GE.0.)GO TO 29                                              
      IF(AMP.EQ.0.)AMP=N-1                                             
      IF(A.LT.APP)APP=A                                                
   29 CONTINUE                                                         
      IF(AMP.NE.0.)PLIM(IAX)= 1.+((-APP)**(1./AMP))                    
  24  L=L-1                                                            
      RETURN                                                           
      END                                                              
