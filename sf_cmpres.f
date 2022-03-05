      SUBROUTINE sf_CMPRES(NCOMP,A,B,C)                                   
C                                                                      
C      STORES UNIT VECTORS A, B, (AND SIGN OF C),                      
C      IN ONE INTEGER *4 VARIABLE...                                   
C                                                                      
      REAL *8 A,B,C                                                    
C                                                                      
      I=((A+1.D0)*16384.D0)+.5D0                                       
      J=((B+1.D0)*16384.D0)+.5D0                                       
C                                                                      
      IF(I.GT.32767)I=32767                                            
      IF(J.GT.32767)J=32767                                            
C                                                                      
      NCOMP=(I*32768)+J                                                
      IF(C.LT.0.)NCOMP=NCOMP+(2**30)                                   
C                                                                      
      RETURN                                                           
      END                                                              
