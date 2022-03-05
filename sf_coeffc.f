      SUBROUTINE sf_COEFFC(QA,IPQM,QN)                                    
C                                                                      
      IMPLICIT REAL *8 (A-H,O-Z)                                       
      DIMENSION QA(100)                                                 
C                                                                      
      K=1                                                              
      QA(1)=1.                                                         
      DO 1 L=2,IPQM                                                    
      QA(L)=QA(K)*QN                                                   
  1   K=L                                                              
      RETURN                                                           
      END                                                              
