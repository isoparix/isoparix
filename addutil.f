      subroutine addutil(ita,itb,u)
      use allcomms
C
C      ADDS UP UTILISATION BY COLLECTING U IN SLOTS IN ARRAY UTIL
C
      SUBNAM='ADDUTIL'
c     write(30,100)u,ita,itb
c     call flush(30)
      DO M=ITA,ITB
                             UTIL(M)=UTIL(M)+U
                             JUTL(M)=JUTL(M)+1
                             IF(U.GT.UTILMAX(M))UTILMAX(M)=U
                             IF(U.LT.UTILMIN(M))UTILMIN(M)=U
      ENDDO
C
      RETURN
c
100   format('Adding value of',e10.3,' between times',2i6)
      END
