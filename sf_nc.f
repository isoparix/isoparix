      SUBROUTINE sf_NC(X,NX,FX,FXA)                                       
C                                                                      
C      THIS SUBROUTINE FINDS THE NUMBER OF SIGN CHANGES IN THE VALUE   
C      OF THE STURM EQUATION AT X.                                     
C      CURRENT VALUE OF F(X) (FIRST ROW OF AXS) IS RETURNED IN FX.     
C                                                                      
      DIMENSION AXS(28,28),PLIM(28)                                    
      DOUBLE PRECISION AXS,FX,FXA,PLIM,X                               
      COMMON /CA/AXS,PLIM,LCK                                          
      NX=0                                                             
      FXA=AXS(1,LCK)                                                   
      J=LCK                                                            
      M=1                                                              
      DO 2 L=2,LCK                                                     
      J=J-1                                                            
      IF(PLIM(J).LT.X)THEN                                             
                          FX=AXS(1,J)                                  
                      ELSE                                             
                          FX=0.D0                                      
                          DO 1 IMZ=1,M                                 
  1                         FX=(FX+AXS(IMZ,J))*X                       
                          FX=FX+AXS(L,J)                               
      ENDIF                                                            
      IF(FX*FXA.LT.0.)NX=NX+1                                          
      FXA=FX                                                           
      M=L                                                              
  2   CONTINUE                                                         
      RETURN                                                           
      END                                                              
