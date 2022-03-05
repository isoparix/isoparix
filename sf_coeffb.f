      SUBROUTINE sf_COEFFB(IPQM,QA,Q,IQB,AQDA,AQD)                        
C                                                                      
      IMPLICIT REAL *8 (A-H,O-Z)                                       
      DIMENSION QA(10),IQB(10),AQDA(10,10),AQD(10,10)                  
C                                                                      
      K=1                                                              
      DO 2 L=2,IPQM                                                    
      QA(L)=QA(K)*Q                                                    
      IF(IQB(L).NE.0)THEN                                              
         DO 1 M=1,L                                                    
  1      AQDA(M,L)=AQD(M,L)*QA(M)                                      
      ENDIF                                                            
  2   K=L                                                              
      RETURN                                                           
      END                                                              
