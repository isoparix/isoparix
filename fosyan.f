      PROGRAM FOSYAN
c
      integer qjob,qpass
c
      parameter (qjob=80,qpass=50000)
c
      COMMON PTIME(qpass,qjob),
     *MASK(qjob,qpass),CLOCK(qjob,qpass),JDMAP(200000),
     *FTJ(qjob),JOBDEN(qjob),MEANJB(qjob,qjob),
     *SDJB(qjob,qjob),MEAN(qjob),
     *TA(qjob),TB(qjob),NT(qjob),
     *SD(qjob),MA(8),MB(8),SC(qjob),YC(qjob),
     *NA(qjob),SA(qjob),YA(qjob),NB(qjob,qjob),
     *SB(qjob,qjob),YB(qjob,qjob),
     *PTMAX(qjob),PTMIN(qjob),JDL(qjob),
     *NC(qjob),MEANLP(qjob),SDLP(qjob),
     *JDBEGT(qjob),JDENDT(qjob),NCOL(20)
c
      DOUBLE PRECISION CLOCK,FTJ,EFT,ELT,TA,TB,TEM,A,S,T,X,X2,
     1SA,SB,YA,YB,SC,YC
c
      REAL MARGIN,MEAN,MEANJB,MEANLP,INTVAL,JDBEGT,JDENDT
c
      CHARACTER *1 PIC(qpass),ASTER,SPACE,MINUS,EXC,EAU
c
      DATA ASTER,SPACE,MINUS,EXC,EAU/'*',' ','-','|','O'/
C
C          THIS PROGRAM WILL ANALYSE THE RESULTS FROM THE FOSYNN PROGRAM
C      IT WILL FIND THE JOB-DENSITIES USED, THE PASSES WHICH ARE VALID,
C      AND THE SINGLE-JOB-TIMES AND THEIR STANDARD DEVIATIONS. THE RESUL
C      ARE PRESENTED BY JOB DENSITY, AND BY INDIVIDUAL JOB.
C      THE PROGRAM ALSO PRINTS A RECORD OF THE DATA INPUT.
C          THE PROGRAM NEEDS AS INPUT THE TIMES OF DAY GIVEN UNDER
C      THE HEADING 'TIME AT END OF PASS', IN SECONDS.   ALL THESE TIMES
C      SHOULD BE ENTERED, IN THE ORDER THEY APPEAR IN THE JOB, FROM EVER
C      JOB RUN.  THE ORDER OF JOBS IS NOT IMPORTANT.   THE TRANSITION FR
C      ONE JOB TO THE NEXT IS MARKED BY INSERTING 0.000 INTO THE INPUT S
C      THE INPUT IS TERMINATED BY ENTERING -999.0 TWICE.
C
C
C      CHANNEL 5 IS INPUT (SCREEN OR FILE)
C      CHANNEL 4 IS OUTPUT FOR PERMANENT RECORD OF INPUT
C      CHANNEL 6 IS SCREEN OR TEMPORARY FILE
C      CHANNEL 8 IS THE RESULTS - GRAPHS,TABLES,ETC.
C      CHANNEL 14 IS THE valid passes
C
C
C      NOTE THAT THE OUTPUT FROM CHANNEL 8 MAKES EXTENSIVE USE OF FORTRA
C      CARRIAGE CONTROL CHARACTERS FOR OVERPRINTING AND PAGE THROWS.
C
      WRITE(8,1000)
1000  FORMAT(15(/),
     A50H *************************************************/
     12H *,46X,2H */
     22H *,2X,42H FOSYAN -LAST UPDATED 27 JUL 82 ISSUE NO.E,2X,2H */
     22H *,2X,42H FOSYAN -LAST UPDATED 14 MAY 99 ISSUE NO.F,2X,2H */
     22H *,2X,42H FOSYAN -LAST UPDATED 25 FEB 02 ISSUE NO.G,2X,2H */
     22H *,2X,42H FOSYAN -LAST UPDATED 13 AUG 06 ISSUE NO.H,2X,2H */
     32H *,46X,2H */
     52H *,46X,2H */
     6       50H *************************************************
     7,/1H1)
C
C      ISSUE B - NO FUNCTIONAL CHANGES FROM ISSUE A
C
C      ISSUE C CALCULATES STANDARD DEVIATIONS AS 1/(N-1), NOT 1/N; AND
C      ONLY USES VALID JOB DENSITIES TO CALCULATE GRAPH SCALES.
C
C      ISSUE D PLOTS THE POSITION OF THE MEAN OF LAST-PASSES
C      ON THE GRAPH.
C
C      ISSUE E INSERTS A TRAP AGAINST JDA BEING ZERO JUST BEFORE
C      LABEL 22.
C
      DO I=1,qjob
         JDBEGT(I)=1000000.
         JDENDT(I)=0.
         PTMAX(I)=0.
         PTMIN(I)=1000000.
         TA(I)=0.D0
         TB(I)=0.D0
         NT(I)=0
         DO J=1,qjob
            MASK(J,I)=-1
            CLOCK(J,I)=0.D0
         enddo
      enddo
c
      EFT=1000000.
      ELT=EFT
      TEM=EFT
      JOB=1
      IT=0
      K=1
      NHYP=0
C
C      THE MARGIN IS THE FRACTION OF A SINGLE PASS WHICH ANOTHER
C      JOB'S START TIME MAY OVERLAP BEFORE THAT PASS IS DISREGARDED.
C      IF THE MARGIN IS POSITVE THE START TIME MAY OCCUR WITHIN X%
C      OF THE START, OR X% OF THE END.  IF NEGATIVE, THE START TIME
C      MUST BE OUTSIDE THE PASS BY X%.
C
      A=-1.   ! New initialisation
      MARGIN=0.0
      JOBEFT=0
      JOBELT=0
      WRITE(6,100)
   1  CONTINUE
      READ(5,101,end=8)S
      IF(S.EQ.-998.D0)GO TO 2
      WRITE(6,127)S
   3  T=A
      IF(T)4,5,6
   4  IF(T.EQ.-999.D0)GO TO 8
      GO TO 12
   5  IF(K.NE.0)GO TO 9
      IF(TEM.GT.ELT)GO TO 48
      ELT=TEM
      JOBELT=JOB
  48  CONTINUE
      JOB=JOB+1
      IT=0
      K=1
      GO TO 9
   6  IT=IT+1
      CLOCK(JOB,IT)=T
      K=0
      IF(IT.EQ.1)FTJ(JOB)=T
      IF(T.GT.EFT)GO TO 49
      EFT=T
      JOBEFT=JOB
  49  CONTINUE
      TEM=T
   9  IF(L.EQ.8)GO TO 12
      L=L+1
      GO TO 3
  12  CONTINUE
      A=S
      GO TO 1
   2  WRITE(6,102)TEM
      A=-1.D0
      GO TO 1
  8   CONTINUE
C
C
C      END OF INPUT PHASE - NOW WRITE OUT TIMES FOR EACH JOB
C
      IF(IT.EQ.0)JOB=JOB-1
      JOBTOT=JOB
      IF(TEM.lt.ELT
     *  )then
             ELT=TEM
             JOBELT=JOB
      endif
c
      DO I=1,JOBTOT
         JOBDEN(I)=0
         NA(I)=0
         SA(I)=0.D0
         YA(I)=0.D0
         NC(I)=0
         SC(I)=0.D0
         YC(I)=0.D0
c
         DO J=1,JOBTOT
            NB(J,I)=0
            SB(J,I)=0.D0
            YB(J,I)=0.D0
         enddo
c
      enddo
c
      WRITE(8,103)
      A=0.0D0
      KWARN=0
      DO 20 I=1,JOBTOT
         WRITE(8,104)I
         X=0.D0
         JWARN=0
         K=0
         L=0
         J=1
  10     T=CLOCK(I,J)
         IF(T.EQ.0)GO TO 15
         WRITE(4,101)T
         IF(T.GT.X)GO TO 14
         KWARN=1
         JWARN=1
  14     X=T
         L=1
         NT(I)=NT(I)+1
         K=K+1
         TA(K)=T
         IF(K.NE.7)GO TO 13
  16     CONTINUE
         WRITE(8,105)(TA(M),M=1,K)
         IF(JWARN.EQ.1)WRITE(8,115)
         JWARN=0
         K=0
  13     J=J+1
         IF(J.LE.qpass)GO TO 10
         GO TO 7
  15     IF(L.NE.1)GO TO 13
         IF(K.NE.0)GO TO 16
   7     WRITE(4,101)A
 20      CONTINUE
      A=-999.D0
      WRITE(4,101)A
      WRITE(4,101)A
      WRITE(8,116)EFT,JOBEFT,ELT,JOBELT
      IF(KWARN.EQ.1.OR.EFT.EQ.ELT)STOP
C
C      DATA NOW WRITTEN TO PRINTER FILE (ANALYSIS OUTPUT), AND TO PERMAN
C      FILE ON CHANNEL 4 FOR FUTURE REFERENCE, IF REQUIRED.
C
C
C      NEXT SECTION CALCULATES PASS DURATIONS, AND FINDS INVALID PASSES
C
      KM=ELT
      DO J=1,90000
         JDMAP(J)=0
      enddo
c
      DO J=1,JOBTOT
C
C      CALL JOB DENSITY MAP
C
         I=FTJ(J)
         IF(I.le.KM
     *     )then
                DO LM=I,KM
                   JDMAP(LM)=JDMAP(LM)+1
                enddo
         endif
      enddo
c
      WRITE(8,106)
      DO 22 J=1,JOBTOT
      WRITE(8,104)J
      JDA=0
      TEM=CLOCK(J,1)
      K=1
      L=0
      NTJ=NT(J)
C
C      THE NEXT FOUR LINES GUARD AGAINST A JOB STARTING, BUT
C      FAILING TO COMPLETE EVEN ONE PASS.
C
      IF(NTJ.GT.1)GO TO 47
      WRITE(8,130)
      GO TO 22
  47  CONTINUE
      DO 23 I=2,NTJ
      T=TEM
      TEM=CLOCK(J,I)
      JD=-1
      X=TEM-T
      XQ=1./X
      DO 24 N=1,JOBTOT
      IF(JD.EQ.0)GO TO 24
      IF(TEM.GT.ELT)JD=0
      C=FTJ(N)
      IF(C.GT.TEM.OR.C.LT.T)GO TO 24
      A=(C-T)*XQ
      B=1.-A
      IF(B.LT.A)A=B
      IF(A.GT.MARGIN)JD=0
  24  CONTINUE
      L=L+1
      TA(L)=X
      IF(JD.LT.0)GO TO 29
      TA(L)=-TA(L)
      GO TO 30
C
C      COLLECT INTERMEDIATE RESULTS FOR SD/MEAN CALCULATIONS.
C
  29  CONTINUE
C
C      JOB DENSITY FOR THIS PASS IS FOUND FROM THE VALUE OF THE
C      JOB DENSITY MAP, JDMAP, CORRESPONDING TO THE MID-TIME OF
C      THE PASS.
C
      IT=(TEM+T)*.5
      JD=JDMAP(IT)
C
C      THE NEXT FIVE LINES COLLECT THE RESULTS FROM THE FIRST VALID PASS
C      A PARTICULAR DENSITY,IN A PARTICULAR JOB.
C
      IF(JDA.ne.JD
     *  )then
             NC(JD)=NC(JD)+1
             SC(JD)=SC(JD)+X
             YC(JD)=YC(JD)+X2
             write(*,800)jd,jda,nc(jd),x
      endif
800   format('JD/JDA/NC(JD)',3i10,f10.3)
      JDA=JD
      JOBDEN(JD)=1
C
C      COLLECT START AND END TIMES OF EACH DENSITY
C
      IF(TEM.GT.JDENDT(JD))JDENDT(JD)=TEM
      IF(T.LT.JDBEGT(JD))JDBEGT(JD)=T
C
C      COLLECT MAX AND MIN PASS DURATION VALUES FOR HISTOGRAM.
C
      IF(X.GT.PTMAX(JD))PTMAX(JD)=X
      IF(X.LT.PTMIN(JD))PTMIN(JD)=X
      X2=X*X
      XA=X
      X2A=X2
C
C      COLLECT RESULTS BY JOB DENSITY
C
      NA(JD)=NA(JD)+1
      SA(JD)=SA(JD)+X
      YA(JD)=YA(JD)+X2
C
C      COLLECT RESULTS BY JOB, AND BY JOB DENSITY.
C
      write(14,134)jd,x
      PTIME(NA(JD),JD)=X
      NB(J,JD)=NB(J,JD)+1
      SB(J,JD)=SB(J,JD)+X
      YB(J,JD)=YB(J,JD)+X2
      MASK(J,K)=JD
  30  CONTINUE
      IF(L.NE.8)GO TO 23
      WRITE(8,101)(TA(M),M=1,8)
      L=0.
  23  K=I
      IF(L.NE.0)WRITE(8,101)(TA(M),M=1,L)
  22  CONTINUE
C
C      CALCULATE MEANS AND STANDARD DEVIATIONS.
C
      DO J=1,JOBTOT
         I=NT(J)-1
         DO K=1,I
            IF(MASK(J,K).gt.0
     *        )then
                   JD=MASK(J,K)
C
C      CALCULATE MEANS AND STANDARD DEVIATIONS FOR EACH DENSITY.
C
                   A=NA(JD)
                   B=1.
                   IF(A.GT.1)B=A-1.
                   S=SA(JD)
                   SD(JD)=DSQRT(DABS(YA(JD)-(S*S/A))/B)
                   MEAN(JD)=S/A
C
C      CALCULATE MEANS AND STANDARD DEVIATIONS FOR LAST PASSES
C      AT EACH DENSITY.
C
                   A=NC(JD)
                   B=1.
                   IF(A.GT.1)B=A-1.
                   S=SC(JD)
                   SDLP(JD)=DSQRT(DABS(YC(JD)-(S*S/A))/B)
                   MEANLP(JD)=S/A
C
C      CALCULATE MEANS AND STANDARD DEVIATIONS FOR INDIVIDUAL JOBS
C      AT EACH DENSITY.
C
                   A=NB(J,JD)
                   B=1.
                   IF(A.GT.1)B=A-1.
                   S=SB(J,JD)
                   SDJB(J,JD)=DSQRT(DABS(YB(J,JD)-(S*S/A))/B)
                   MEANJB(J,JD)=S/A
            endif
         enddo
      enddo
C
C      THE NEXT SECTION CALCULATES THE SINGLE-JOB TIMES
C      AND THE START AND END TIMES OF EACH DENSITY.  THE
C      RESULTS ARE PRINTED.
C
      WRITE( 8,107)
      WRITE(10,107)
      WRITE(12,1071)
      GFMAX=0.
      GFMIN=1000000.
      DO JD=1,JOBTOT
         IF(JOBDEN(JD).gt.0
     *     )then
                A=1.D0/DFLOAT(JD)
                S=MEAN(JD)*A
                T=SD(JD)*A
C
C      THE NEXT TWO LINES COLLECT MAX AND MIN VALUES FOR GRAPH.
C
                IF(NC(JD).eq.JD
     *            )then
                       IF((S+T).GT.GFMAX)GFMAX=S+T
                       IF((S-T).LT.GFMIN)GFMIN=S-T
                endif
                QB=JDENDT(JD)
                QA=JDBEGT(JD)
                Q=QB-QA
                WRITE( 8,108)JD,NA(JD),MEAN(JD),SD(JD),S,T,QA,QB,Q
                if(nc(jd).eq.jd
     *            )then
                       WRITE(12,108)JD,NA(JD),MEAN(JD)
     *                                ,SD(JD),S,T,QA,QB,Q
                endif
                S=MEANLP(JD)*A
                T=SDLP(JD)*A
                IHRSA=QA/3600.
                IHRSB=QB/3600.
                QAA=QA-FLOAT(IHRSA*3600)
                QBA=QB-FLOAT(IHRSB*3600)
                MINSA=QAA/60.
                MINSB=QBA/60.
                SECSA=QAA-FLOAT(MINSA*60)
                SECSB=QBA-FLOAT(MINSB*60)
                if(nc(jd).eq.jd
     *            )then
                       WRITE(10,108)JD,NC(JD),MEANLP(JD)
     *                              ,SDLP(JD),S,T,QA,QB,Q
                endif
                WRITE(8,117)NC(JD),MEANLP(JD),SDLP(JD),S,T
     1          ,IHRSA,MINSA,SECSA,IHRSB,MINSB,SECSB
         endif
      enddo
  31  CONTINUE
      IF(GFMAX.EQ.GFMIN)GO TO 53
      WRITE(8,118)
      IMA=(MARGIN*100.)+.00001
      WRITE(8,131)IMA
C
C      THE NEXT SECTION PLOTS THE GRAPH OF SINGLE-JOB TIMES
C      VERSUS THE JOB DENSITIES.  THE HORIZONTAL (TIME) AXIS
C      SCALE IS NORMALISED.
C
      SCALE=99./(GFMAX-GFMIN)
      AXMIN=((GFMAX+GFMIN)/2.)-(50./SCALE)
      AXMAX=AXMIN+(100./SCALE)
C
C      THE NEXT SECTION PUTS ONE SECOND BARS ON THE HORIZONTAL
C      AXIS.
C
      DO I=1,100
         PIC(I)=MINUS
      enddo
c
      MAX=GFMAX+1
      MIN=GFMIN+1
      IF(MIN.LT.1)MIN=1
      DO I=MIN,MAX
         QS=(FLOAT(I)-AXMIN)*SCALE
         KS=QS
         IF((QS-FLOAT(KS)).GT..5)KS=KS+1
         IF(KS.GT.0.AND.KS.LT.qjob)PIC(KS)=EXC
      enddo
c
      WRITE(8,129)SCALE,AXMIN,AXMAX,(PIC(MX),MX=1,99)
      DO 11 JD=1,JOBTOT
         WRITE(8,125)
         IF(JOBDEN(JD).EQ.0.OR.NC(JD).NE.JD)GO TO 11
C
C      THE NEXT FOUR LINES COLLECT VALUES FOR HYPERBOLIC CURVE FIT
C
         NHYP=NHYP+1
         A=1./FLOAT(JD)
         SC(NHYP)=A*MEAN(JD)
         NC(NHYP)=JD
         DO I=1,100
            PIC(I)=SPACE
         enddo
C
C      FIND POSITION OF LOWER ERROR BAR
C
         QS=((A*(MEAN(JD)-SD(JD)))-AXMIN)*SCALE
         IF(QS.LT.1)QS=.8
         KS=QS
         IF((QS-FLOAT(KS)).GT..5)KS=KS+1
         PIC(KS)=EXC
C
C      FIND POSITION OF UPPER ERROR BAR
C
         QS=((A*(MEAN(JD)+SD(JD)))-AXMIN)*SCALE
         IF(QS.LT.1)QS=.8
         KR=QS
         IF((QS-FLOAT(KR)).GT..5)KR=KR+1
         PIC(KR)=EXC
C
C      FIND POSITION OF LAST-PASSES MEAN VALUE.
C
         QS=(A*(MEANLP(JD))-AXMIN)*SCALE
         IF(QS.LT.1)QS=.8
         KT=QS
         IF((QS-FLOAT(KT)).GT..5)KT=KT+1
         PIC(KT)=EAU
C
C      FIND POSITION OF MEAN VALUE.
C
         QS=(A*(MEAN(JD))-AXMIN)*SCALE
         IF(QS.LT.1)QS=.8
         KT=QS
         IF((QS-FLOAT(KT)).GT..5)KT=KT+1
         PIC(KT)=ASTER
C
C      FILL IN ERROR LINES, AND PRINT VALUES FOR THIS DENSITY
C
         DO J=KS,KR
            IF(PIC(J).EQ.SPACE)PIC(J)=MINUS
         enddo
         WRITE(8,128)JD,(PIC(MX),MX=1,KR)
  11  CONTINUE
 53   CONTINUE
C
C      THE NEXT SECTION USES TRIPLETS OF JOB DENSITIES AND SINGLE
C      JOB TIMES TO GENERATE HYPERBOLIC CURVES OF THE FORM
C      (X-A)(Y-B)=C.
C
      IF(NHYP.LT.3)GO TO 51
      WRITE(8,132)
      NHYPA=NHYP-1
      NHYPB=NHYP-2
      DO 50 I=1,NHYPB
      Y1=SC(I)
      X1=NC(I)
      IA=I+1
      S=X1*Y1
      DO 50 J=IA,NHYPA
      Y2=SC(J)
      X2=NC(J)
      JA=J+1
      P=S-(X2*Y2)
      Q=Y1-Y2
      R=X1-X2
      DO 50 K=JA,NHYP
      Y3=SC(K)
      X3=NC(K)
      A=S-(X3*Y3)
      B=Y1-Y3
      C=X1-X3
      D=1./((C*Q)-(B*R))
      AH=((P*C)-(A*R))*D
      BH=((A*Q)-(B*P))*D
      CH=(X1-AH)*(Y1-BH)
      WRITE(8,133)NC(I),Y1,NC(J),Y2,NC(K),Y3,AH,BH,CH
  50  CONTINUE
  51  CONTINUE
C
C      RESULTS BY INDIVIDUAL JOB CALCULATED NEXT
C
      WRITE(8,109)
      JDC=0
      K=0
      DO 33 JD=1,JOBTOT
      IF(JOBDEN(JD).EQ.0)GO TO 33
      JDC=JDC+1
      JDL(JDC)=JD
      WRITE(8,110)JD
      K=0
C
C      PRINT MEAN AND STANDARD DEVIATIONS OF EACH JOB
C      AT EACH DENSITY.
C
      DO 32 J=1,JOBTOT
      IF(NB(J,JD).EQ.0)GO TO 32
      K=1+K
      MA(K)=J
      MB(K)=NB(J,JD)
      TA(K)=MEANJB(J,JD)
      TB(K)=SDJB(J,JD)
      IF(K.LT.6)GO TO 32
      WRITE(8,111)(MA(L),L=1,6)
      WRITE(8,112)(MB(L),L=1,6)
      WRITE(8,113)(TA(L),L=1,6)
      WRITE(8,114)(TB(L),L=1,6)
      K=0
  32  CONTINUE
      IF(K.EQ.0)GO TO 33
      WRITE(8,111)(MA(L),L=1,K)
      WRITE(8,112)(MB(L),L=1,K)
      WRITE(8,113)(TA(L),L=1,K)
      WRITE(8,114)(TB(L),L=1,K)
      K=0
 33   CONTINUE
C
C      SELECT CHARACTERISTICS OF HISTOGRAM
C
      IF(GFMAX.EQ.GFMIN)GO TO 54
      NSLOTS=11
      LENGTH=2
      LNHALF=(LENGTH/2)+1
      DO 21 I=1,100
  21  PIC(I)=ASTER
      DO 40 JD=1,JOBTOT
      IF(JOBDEN(JD).EQ.0)GO TO 40
      LMAX=0
C
C      FIND HISTOGRAM INTERVAL SIZES, AND LIMITS.
C
      DPT=PTMAX(JD)-PTMIN(JD)
      IF(DPT.EQ.0)GO TO 40
      INTVAL=DPT/(FLOAT(NSLOTS)*.998)
      AXMIN=((PTMAX(JD)+PTMIN(JD))-(INTVAL*FLOAT(NSLOTS)))/2.
      DO 41 I=1,20
 41   NCOL(I)=0
      NAJD=NA(JD)
C
C      ALLOCATE PASSES IN PTIME TO SLOTS IN HISTOGRAM.
C
      DO 42 I=1,NAJD
      KSLOT=((PTIME(I,JD)-AXMIN)/INTVAL)+1.
      NCOL(KSLOT)=NCOL(KSLOT)+1
      IF(NCOL(KSLOT).GT.LMAX)LMAX=NCOL(KSLOT)
  42  CONTINUE
      WRITE(8,121)JD
      Q=0.
C
C      NORMALISE THE HORIZONTAL (QUANTITY) AXIS, AND ASSIGN
C      LENGTH TO HISTOGRAM BARS.
C
      SCALE=100./FLOAT(LMAX)
      J=0
      DO 43 I=1,NSLOTS
      NCT=NCOL(I)
      J=J+NCT
      QS=SCALE*FLOAT(NCT)
      KS=QS
      IF((QS-FLOAT(KS)).GT.0.5)KS=KS+1
      Y=AXMIN+(INTVAL*Q)
      WRITE(8,122)Y
C
C      PRINT NON-ZERO HISTOGRAM BARS.
C
      DO 44 N=1,LENGTH
      WRITE(8,125)
      IF(NCT.EQ.0)GO TO 44
      WRITE(8,123)(PIC(MX),MX=1,KS)
      IF(N.EQ.LNHALF)WRITE(8,126)NCT
  44  CONTINUE
      Q=I
  43  CONTINUE
      Y=PTMAX(JD)+(INTVAL/2.)
C
C      PRINT HISTOGRAM SCALE DETAILS.
C
      WRITE(8,124)Y,SCALE,J,PTMIN(JD),PTMAX(JD)
  40  CONTINUE
  54  CONTINUE
C
C      NEXT SECTION PRINTS THE MEAN TIMES OF EACH JOB
C      AT EACH DENSITY.
C
      WRITE(8,119)(JDL(M),M=1,JDC)
      DO J=1,JOBTOT
         N=0
         DO JD=1,JOBTOT
            IF(JOBDEN(JD).ne.0
     *        )then
                   N=N+1
                   IF(NB(J,JD).EQ.0
     *               )then
                          TA(N)=0.D0
                      else
                          TA(N)=MEANJB(J,JD)
                   endif
            endif
         enddo
         WRITE(8,120)J,(TA(L),L=1,N)
      enddo
c
      WRITE(8,1001)
      STOP
100   FORMAT(' *** HERE ARE YOUR INSTRUCTIONS ***')
101   FORMAT(8F10.3)
102   FORMAT(' LAST LINE CANCELLED - THE FIRST VALID TIME WAS',F10.3)
103   FORMAT(1X,'******** THE END-OF-PASS TIMES FOR ALL JOBS ******',/)
104   FORMAT(/'      JOB NUMBER',I4,/)
105   FORMAT(1X,7F11.3)
106   FORMAT(1H1,' ***** PASS DURATIONS BY JOB - INVALID PASSES -VE',
     1' *****',/)
107   FORMAT(1H1,15X
     *,'***** RESULTS DERIVED FROM FIRST VALID PASSES IN ALL'
     *,' JOBS *****',//
     17X,'JOB',5X,'NUMBER OF   MEAN PASS    STANDARD  SINGLE-JOB',4X,
     A'STANDARD',16X,'***** TIME *****',
     2/5X,'DENSITY    SAMPLES',5X,'DURATION   DEVIATION',
     35X,'TIME',6X,'DEVIATION',12X,'FROM',14X,'TO',9X,'ELAPSED',//)
1071  FORMAT(1H1,15X
     *,'***** RESULTS DERIVED FROM ALL VALID PASSES IN ALL'
     *,' JOBS *****',//
     17X,'JOB',5X,'NUMBER OF   MEAN PASS    STANDARD  SINGLE-JOB',4X,
     A'STANDARD',16X,'***** TIME *****',
     2/5X,'DENSITY    SAMPLES',5X,'DURATION   DEVIATION',
     35X,'TIME',6X,'DEVIATION',12X,'FROM',14X,'TO',9X,'ELAPSED',//)
108   FORMAT(2I12,4F12.3,3F16.3)
109   FORMAT(1H1,15X,'***** RESULTS FROM INDIVIDUAL JOBS *****',//)
110   FORMAT(//,'       ***** JOB DENSITY',I3,' *****')
111   FORMAT(/10X,'JOB',6I10)
112   FORMAT(6X,'SAMPLES',6I10)
113   FORMAT(' AVG DURATION',6F10.3)
114   FORMAT(' STANDRD DEVN',6F10.3)
115   FORMAT(/' ***** TIMES IN LAST LINE DO NOT INCREASE *****',/)
116   FORMAT(//' EARLIEST FIRST TIME IS',F11.3,' IN JOB',I4,
     1        /' EARLIEST FINAL TIME IS',F11.3,' IN JOB',I4,//)
117   FORMAT(I24,' * ',F9.3,3F12.3,2(I6,':',I2,':',F6.3),/)
118   FORMAT(//'    *  RESULTS FROM FIRST VALID PASS IN EACH JOB',
     1' AT THIS DENSITY.',/)
119   FORMAT(1H1,'   JOB                  DENSITIES',/I12,21I7,/)
120   FORMAT(I6,21F7.1)
121   FORMAT(1H1,///'  ***** DISTRIBUTION OF PASS DURATIONS AT DENSITY',
     1I4,' *****',/)
122   FORMAT(F8.1,'-')
123   FORMAT(1H+,8X,140A1)
124   FORMAT(F8.1,'-',//' HORIZONTAL SCALE IS ONE PASS TO',F7.2,
     1' ASTERISKS',I65,/' SHORTEST PASS IS',F10.3,' SECONDS',/
     2'  LONGEST PASS IS',F10.3,' SECONDS')
125   FORMAT(8X,'|')
126   FORMAT(1H+,I113)
127   FORMAT(F15.3)
128   FORMAT(1H+,I6,' -',140A1)
129   FORMAT(1H1,'***** GRAPH OF SINGLE-JOB TIMES VERSUS JOB DENSITIES'
     1,' *****',/
     2'       (HORIZONTAL SCALE IS ONE SECOND TO',F8.3,' PP)'
     A,/' POINT IS ONLY PLOTTED IF SAMPLES AVAILABLE FROM'
     B,' N JOBS AT DENSITY N',//
     C,/' * INDICATES MEAN OF ALL PASSES '
     D,/' | INDICATES +/- ONE STANDARD DEVIATION OF MEAN OF ALL PASSES'
     E,/' O INDICATES MEAN OF LAST-PASSES'
     3,///F12.3,91X,F9.3,/8X,'|',99A1,'|')
130   FORMAT(' THIS JOB STARTED, BUT FAILED TO COMPLETE',
     1' EVEN ONE PASS')
131   FORMAT(/' NEW JOBS WERE ALLOWED TO OVERLAP A VALID PASS',
     1' BY UP TO',I10,' PER CENT',/)
132   FORMAT(1H1,
     1'***** HYPERBOLA (X-A)(Y-B)=C FITTED TO TRIPLETS OF RESULTS *****'
     2,/'      X-AXIS IS JOB DENSITY',
     3/'      Y-AXIS IS SINGLE JOB TIME',
     4//3('   X     Y      '),5X,'A',9X,'B',9X,'C',//)
133   FORMAT(3(I4,F9.3,3X),3F9.3)
134   format(i6,f9.3)
1001  FORMAT(1H1,15(/),
     A50H *************************************************/
     12H *,46X,2H */
     22H *,2X,42H FOSYAN -      ANALYSIS COMPLETE          ,2X,2H */
     32H *,46X,2H */
     52H *,46X,2H */
     6       50H *************************************************)
      end
