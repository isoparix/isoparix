      SUBROUTINE SOLVER (X,X1,Y1,X2,Y2)                                
C                                                                      
C      SOLVES A WELL-BEHAVED POINT, RETURN RESULT IN X...              
C                                                                      
      IMPLICIT REAL *8 (A-H,O-Z)                                       
      COMMON /CC/ACC,AXA,AXACON,MB                                     
      COMMON /STATS/IROW,JCOL,NGTYPE                                   
                                                                       
      DIMENSION AXA(28)                                                
C                                                                      
C      FIRST, CHECK THAT THE POINT IS WELL-BEHAVED...                  
C                                                                      
      IF(Y1*Y2.GE.0.
     *  )THEN                                              
             IF(ABS(Y1).LT.ABS(Y2)
     *         )THEN                     
                    X=X1                                       
                ELSE                                           
                    X=X2                                       
                ENDIF                                          
                RETURN                                         
      ENDIF                                                            
      YA=Y1                                                            
      Y =Y2                                                            
C                                                                      
  1   CONTINUE                                                         
C                                                                      
      IF(Y*YA.GT.0
     *  )THEN                                               
C                         USE BINARY CHOP...                           
             X=.5*(X1+X2)                                 
         ELSE                                               
C                         USE REGULA FALSI                             
             X=((X2*Y1)-(X1*Y2))/(Y1-Y2)                  
      ENDIF                                                            
      IF(X1.GT.X*ACC)RETURN                                            
C                                                                      
C      EVALUATE THE POLYNOMIAL AT X, RETURN RESULT IN Y....            
C                                                                      
      YA=Y                                                             
      Y=X                                                              
      DO 2 I=1,MB                                                      
  2   Y=(Y+AXA(I))*X                                                   
      Y=Y+AXACON                                                       
C                                                                      
      IF(Y*Y1.LT.0.0
     *  )THEN                                              
             X2=X                                                          
             Y2=Y                                                          
         ELSE                                                       
             Y1=Y                                                          
             X1=X                                                          
      ENDIF                                                            
      GO TO 1                                                          
c
      END                                                              
