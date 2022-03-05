      SUBROUTINE sf_COEFFA(IPQM,QA,QE,IQB,AQD)                            
C                                                                      
      use surf_comms
C                                                                      
      REAL *8 QA(10),QE,AQD(10,10)
      INTEGER *4 IPQM,IQB(10)                                          
C                                                                      
      if(ipqm.gt.10
c     if(ipqm.gt.9
     *  )then
             write(0,100)ipqm
             stop
      endif
C                                                                      
      J=1                                                              
      DO 2 I=2,IPQM                                                    
         QA(I)=QA(J)*QE
         IF(IQB(I).NE.0
     *     )THEN
                KP=I
                DO 1 K=1,I
                  AQD(K,I)=BC(I,K)*QA(KP)
c                 AQD(K,I)=BC(k-1,i-1)*QA(KP)  ! For general binomial sub
  1             KP=KP-1
         ENDIF
  2   J=I                                                              
      RETURN                                                           
C                                                                      
100   format('ERROR in sf_coeffa: IPQM=',i6)
C                                                                      
      END                                                              
