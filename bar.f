      subroutine bar
C
C      THIS subroutine PLOTS THE BAR REPRESENTING THE VALUE IN THIS
C      INTERVAL
C
      use allcomms
      CHARACTER *1 ASTER(100),axchar,histo(0:60,0:15)
      DIMENSION TIKVAL(5)
C
c
      do i=0,14
c
         lh=khist(i)
         do j=1,lh
            histo(j,i+1)='*'
         enddo
c
         do j=lh,60
            histo(j,i+1)=' '
         enddo
c
      enddo
c
      SUBNAM='BAR'
      IF(KBLKMAX.LE.0)RETURN
      NHRSX=-1
      ALLMAX=0.
      ALLMEAN=0.
      ALLMIN=10.**20
      DO I=KBLKMIN,KBLKMAX
         IF(MAPMAX(I).GT.ALLMAX)ALLMAX=MAPMAX(I)
         IF(MAP(I).GT.ALLMEAN)ALLMEAN=MAP(I)
         IF(MAPMIN(I).LT.ALLMIN)ALLMIN=MAPMIN(I)
c        write(30,201)i,mapmax(i),map(i),mapmin(i)
201   format(i4,3e20.7)  
      ENDDO
c
      write(18,1092)title,trim(SYSOLD)
      call add_toc_item(4,title)
      write(16,109)trim(SYSOLD)
      IF(ALLMAX.LE.0)then
                     write(16,108)
                     write(12,307)
     *      TITLE,ALLMAX,ALLMEAN,allmin,INTSECS,navg
                     return
                else
                     write(12,207)
     *      TITLE,ALLMAX,ALLMEAN,allmin,INTSECS,navg
     *     ,average,stddev
c    *     ,average,stddev,1.-((stddev**2)/average)
      endif
C
C      SET GRID TICK-MARK VALUES
C
         DO M=20,100,20
            TIKVAL(M/20)=.01*ALLMAX*FLOAT(M)
         ENDDO
C
c      How much should we compress the results bars?
c
      shrink=95./float(kblkmax-kblkmin+10)
      if(shrink.gt.1.)shrink=1.
c
      write(16,107)
     *                    ((histo(mx,my),mx=1,60),my=1,4)
     *            ,nitems, (histo(mx, 5),mx=1,60)
     *            ,ALLMAX ,(histo(mx, 6),mx=1,60)
     *            ,ALLMEAN,(histo(mx, 7),mx=1,60) 
     *            ,average,(histo(mx, 8),mx=1,60) 
     *            ,stddev, (histo(mx, 9),mx=1,60) 
     *            ,allmin, (histo(mx,10),mx=1,60) 
     *            ,        (histo(mx,11),mx=1,60) 
     *            ,INTSECS,(histo(mx,12),mx=1,60) 
     *            ,avgtype
     *            ,navg,   (histo(mx,13),mx=1,60) 
     *            ,        (histo(mx,14),mx=1,60) 
     *            ,        (histo(mx,15),mx=1,60) 
c
      A=100./ALLMAX
C
C      NOW DISPLAY THE VALUES
C
      write(16,1041)TIKVAL
      DO J=KBLKMIN,KBLKMAX
         LMAX=.5+(MAPMAX(J)*A)
         LAVG=.5+(MAP(J)   *A)
         LMIN=.5+(MAPMIN(J)*A)
         IF(LAVG.GT.100)LAVG=100
         IF(LMAX.GT.100)LMAX=100
         LTIM=0
c        write(30,200)j,kblkmin,kblkmax,lmax,lavg,lmin
c        call flush(30)
200   format(8i15)
         if(mapmin(j).gt.0.)then
                                axchar='|'
                            else
                                axchar='#'
         endif
C
         IF(LMAX.GT.0)THEN
                          IF(LMIN.GT.1)THEN
                                           DO MX=1,LMIN-1
                                              ASTER(MX)='.'
                                           ENDDO
                                       ELSE
                                           LMIN=1
                          ENDIF
                          DO MX=LMIN,LMAX
                             ASTER(MX)='|'
                          ENDDO
                          IF(LMAX.LT.100)THEN
                                             DO MX=LMAX+1,100
                                                          ASTER(MX)=' '
                                             ENDDO
                          ENDIF
                          IF(LAVG.GT.0)ASTER(LAVG)='*'
                          LTIM=2
                      ELSE
                          DO MX=1,100
                             ASTER(MX)=' '
                          ENDDO
         ENDIF
C
C      PUT IN CALIBRATION GRID LINES
C
         DO M=20,100,20
            IF(ASTER(M).NE.'*'.AND.ASTER(M).NE.'|')ASTER(M)=':'
         ENDDO
C
C      write OUT TIME OF DAY EVERY NEW HOUR
C
           NHRS=ITMAP(J)/3600
           MINS=(ITMAP(J)-(NHRS*3600))/60
           NSECS=ITMAP(J)-(NHRS*3600)-(MINS*60)
         IF(NHRS.GT.NHRSX)THEN
                              LTIM=LTIM+1
                              NHRSX=NHRS
         ENDIF
C
         IF(LTIM.EQ.0)write(16,100)axchar,aster
         IF(LTIM.EQ.1)write(16,101)NHRS,MINS,NSECS,axchar,ASTER
         IF(LTIM.EQ.2)write(16,102)MINS,NSECS,KUTL(J),axchar,ASTER
         IF(LTIM.EQ.3)write(16,103)NHRS,MINS,NSECS,KUTL(J),axchar,ASTER
c
c      Write out time-stamped values
c
         write(18,1011)NHRS,MINS,NSECS,mapmin(j),map(j),mapmax(j)
c
      ENDDO
      write(16,104)TIKVAL
C
100   FORMAT(                12X,a1,100a1)
101   FORMAT(I3,':',I2.2,':',I2.2,3X,a1,100A1)
1011  FORMAT(I3,':',I2.2,':',I2.2,3f12.1)
102   FORMAT('    ',I2.2,':',I2.2,I3,a1,100A1)
103   FORMAT(I3,':',I2.2,':',I2.2,I3,a1,100A1)
104   FORMAT('           |',7X,5F20.6
     *     ,/'           |'
     *     ,/'  NUMBER OF RECORDED'
     *     ,/'       INTERVALS'
     *     ,//)
1041  FORMAT(19X,5F20.6)
207   FORMAT(' ',A48,3f12.3,2i6,2f12.3,f6.3)
307   FORMAT(' ',A48,3f12.3,2i6,' - no activity')
107   FORMAT('<pre>'
     *    ,/52x,' |',60a1
     *    ,/' NOTES:............................................. |'
     *    ,60a1
     *    ,/52x,' |',60a1
     *    ,/52x,' |',60a1
     *    ,/' TOTAL SAMPLES:     ',i15  ,17x,' |',60a1
     *    ,/' MAXIMUM VALUE:     ',e15.6,17x,' |',60a1
     *    ,/' HIGHEST MEAN VALUE:',e15.6,17x,' |',60a1
     *    ,/' OVERALL MEAN VALUE:',e15.6,17x,' |',60a1
     *    ,/' STANDARD DEVIATION:',e15.6,17x,' |',60a1
     *    ,/' MINIMUM VALUE:     ',e15.6,17x,' |',60a1
     *    ,/52x,' |',60a1
     *    ,/' DATA COLLECTED AT TYPICAL INTERVALS OF',I5,' SECONDS |'
     *    ,60a1
     *    ,/'             ',a8,' OVER INTERVALS OF',I5,' SECONDS |'
     *    ,60a1
     *    ,/52x,' |',60a1
     *    ,/52x,' |',60a1
     *    ,/' KEY:      |||||||||||||||||||||*||||||||||||||||||||||'
     *    ,/
     *    ,'          MIN                 MEAN                   MAX'
     *    ,/
     *    ,'                  OF ALL SAMPLES IN THIS INTERVAL       '
     *   ,//' INTERVAL'
     *    ,/' MID-POINT'
     *    ,/' TIME    '
     *    ,//' HH-MM-SS')
108   format('<br>'
     *,'****** NO ACTIVITY RECORDED FOR THIS ASPECT OF WORKLOAD ******'
     *      ,'<br>')
109   FORMAT(a100)
1092  FORMAT(/A48,/a70,/)
      RETURN
      END
