      PROGRAM FOSY01
C
C
C          AUTHOR:  J S WATTS, CCTA C3, 01-211 8128
C
C          LANGUAGE:FORTRAN
C
C          INPUT:   NONE.  PARAMETERS SET IN PROGRAM CARDS.
C
C          OUTPUT:  TO UNIT RECORD DEVICE ON CHANNEL 6.
C
C          THIS PROGRAM MEASURES THE SPEED OF THE PROCESSOR IN KILO-WHET
C      INSTRUCTIONS PER SECOND (KWIPS). IT DOES THIS BY SETTING THE PARA
C      I IN THE CCTA TEST FOPR12 TO A PARTICULAR VALUE, AND THEN USING T
C      TIME-OF-DAY SYSTEM CLOCK (CALLED IN SUBROUTINE TIM) TO DETERMINE
C      THE TIME FOR REPEATED PASSES OF THAT PART OF FOPR12 SENSITIVE TO
C      VALUE OF I.
C
C          THE PROGRAM IS MEANT TO BE USED TO DETERMINE THE CPU POWER SP
C      BETWEEN MANY USERS.      IF, SAY, 50 USERS ACHIEVE A MEAN RATING
C      OF 50 KWIPS, THEN THE CPU POWER DELIVERED TO USERS AT THE 50-USER
C      LEVEL IS 2500 KWIPS.
C
      COMMON T,T1,T2,E1(4),J,K,L
      COMMON /TOTAL/SUM
      real (8) ta, tb
      T=0.499975
      T1=0.50025
      T2=2.0
      I=70000
      WRITE(6,1000)I
 1000 FORMAT(50H *************************************************/
     12H *,46X,2H */
     22H *,2X,39H FOSY01 -LAST UPDATED MAY 80 ISSUE NO.4,5X,2H */
     A2H *,2X,39H CCTA PROGRAM REFERENCE NUMBER 10/01/08,5X,2H */
     32H *,46X,2H */
     42H *,2X,18H PARAMETERS ARE I=,I10,16X,2H */
     52H *,46X,2H */
     6       50H *************************************************/)
      WRITE(6,102)
c102   FORMAT('     I  PASS   DURATION   TOTAL TIME',
102    FORMAT('  PASS     I   DURATION   TOTAL TIME',
     1'          TIME AT END OF PASS        KWIPS',/
     214X,'(SECONDS)    (SECONDS)      (SECONDS)   (HH:MM:SECONDS)')
      N1=0
      N2=12*I
      N3=14*I
      N4=345*I
      N5=0
      N6=210*I
      N7=32*I
      N8=899*I
      N9=616*I
      N10=0
      N11=93*I
      N12=0
      ICOUNT=0
      KX=I
      A=I*100
      CALL TIM(TA)
      TB=TA
      START=TA
c     mcpu=mclock()
      TC=0.
      THWIPS=0.
      GO TO 3
  2   CONTINUE
c     mcpu=mclock()
      SUM=0.
      ICOUNT=ICOUNT+1
      X1=1.0
      X2=-1.0
      X3=-1.0
      X4=-1.0
      IF(N1)19,19,11
   11 DO 18 I=1,N1,1
      X1=(X1+X2+X3-X4)*T
      X2=(X1+X2-X3+X4)*T
      X3=(X1-X2+X3+X4)*T
      X4=(-X1+X2+X3+X4)*T
   18 CONTINUE
   19 CONTINUE
      SUM=SUM+FLOAT(N1)+X1+X2+X3+X4
      E1(1)=1.0
      E1(2)=-1.0
      E1(3)=-1.0
      E1(4)=-1.0
      IF(N2)29,29,21
   21 DO 28 I=1,N2,1
      E1(1)=(E1(1)+E1(2)+E1(3)-E1(4))*T
      E1(2)=(E1(1)+E1(2)-E1(3)+E1(4))*T
      E1(3)=(E1(1)-E1(2)+E1(3)+E1(4))*T
      E1(4)=(-E1(1)+E1(2)+E1(3)+E1(4))*T
   28 CONTINUE
   29 CONTINUE
      SUM=SUM+FLOAT(N2+N3)+E1(1)+E1(2)+E1(3)+E1(4)
      IF(N3)39,39,31
   31 DO 38 I=1,N3,1
   38 CALL PA(E1)
   39 CONTINUE
      SUM=SUM+E1(1)+E1(2)+E1(3)+E1(4)
      J=1
      IF(N4)49,49,41
   41 DO 48 I=1,N4,1
      IF(J-1)43,42,43
   42 J=2
      GO TO 44
   43 J=3
   44 IF(J-2)46,46,45
   45 J=0
      GO TO 47
   46 J=1
   47 IF(J-1)411,412,412
  411 J=1
      GO TO 48
  412 J=0
   48 CONTINUE
   49 CONTINUE
      SUM=SUM+FLOAT(N4+J)+X1+X2+X3+X4
      J=1
      K=2
      L=3
      IF(N6)69,69,61
   61 DO 68 I=1,N6,1
      J=J*(K-J)*(L-K)
      K=L*K-(L-J)*K
      L=(L-K)*(K+J)
      E1(L-1)=J+K+L
      E1(K-1)=J*K*L
   68 CONTINUE
   69 CONTINUE
      SUM=SUM+FLOAT(J+K+N6)+E1(1)+E1(2)+E1(3)+E1(4)
      X=0.5
      Y=0.5
      IF(N7)79,79,71
   71 DO 78 I=1,N7,1
      X=T*ATAN(T2*SIN(X)*COS(X)/(COS(X+Y)+COS(X-Y)-1.0))
      Y=T*ATAN(T2*SIN(Y)*COS(Y)/(COS(X+Y)+COS(X-Y)-1.0))
   78 CONTINUE
   79 CONTINUE
      SUM=SUM+FLOAT(N7)+X+Y
      X=1.0
      Y=1.0
      Z=1.0
      IF(N8)89,89,81
   81 DO 88 I=1,N8,1
   88 CALL P3(X,Y,Z)
   89 CONTINUE
      SUM=SUM+FLOAT(N8)+X+Y+Z
      J=1
      K=2
      L=3
      E1(1)=1.0
      E1(2)=2.0
      E1(3)=3.0
      IF(N9)99,99,91
   91 DO 98 I=1,N9,1
   98 CALL PO
   99 CONTINUE
      SUM=SUM+FLOAT(N9)+E1(1)+E1(2)+E1(3)+E1(4)
      J=2
      K=3
      IF(N10)109,109,101
  101 DO 108 I=1,N10,1
      J=J+K
      K=J+K
      J=J-K
      K=K-J-J
  108 CONTINUE
  109 CONTINUE
      SUM=SUM+FLOAT(N10+J+K)+X1+X2+X3+X4
      X=0.75
      IF(N11)119,119,111
  111 DO 118 I=1,N11,1
  118 X=SQRT(EXP(ALOG(X)/T1))
  119 CONTINUE
      SUM=SUM+FLOAT(N11)+X
      CALL TIM(TB)
      TC=TB-TA
      IF(TC.LT.0.)STOP
      THWIPS=A/TC
  3   IHRS=TB/3600.
      B=TB-FLOAT(IHRS*3600)
      MINS=B/60.
      SECS=B-FLOAT(MINS*60)
      TIME=TB-START
c     cpu=.01*(mclock()-mcpu)
      WRITE(6,103)ICOUNT,KX,TC,TIME,TB,IHRS,MINS,SECS,THWIPS
      call isoflush(6)
      if(icount.eq.10)stop
c     WRITE(8,104)ICOUNT,KX,TC,cpu,TIME,TB,IHRS,MINS,SECS
103   FORMAT(2I6,F11.3,F13.3,F15.3,I6,':',I2,':',F6.3,F11.1)
104   FORMAT(2I6,2f7.3,F13.3,F15.3,I6,':',I2,':',F6.3)
      TA=TB
      GO TO 2
      stop
      END
C
C
C
      SUBROUTINE PA(E)
C-TRACE NONE
      COMMON T,T1,T2,E1(4),J,K,L
      DIMENSION E(4)
      J=0
    1 E(1)=(E(1)+E(2)+E(3)-E(4))*T
      E(2)=(E(1)+E(2)-E(3)+E(4))*T
      E(3)=(E(1)-E(2)+E(3)+E(4))*T
      E(4)=(-E(1)+E(2)+E(3)+E(4))/T2
      J=J+1
      IF(J-6)1,2,2
    2 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE PO
C-TRACE NONE
      COMMON T,T1,T2,E1(4),J,K,L
      E1(J)=E1(K)
      E1(K)=E1(L)
      E1(L)=E1(J)
      RETURN
      END
C
C
C
      SUBROUTINE P3(X,Y,Z)
C-TRACE NONE
      COMMON T,T1,T2,E1(4),J,K,L
      X1=X
      Y1=Y
      X1=T*(X1+Y1)
      Y1=T*(X1+Y1)
      Z=(X1+Y1)/T2
      RETURN
      END
