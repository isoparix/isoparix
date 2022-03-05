      PROGRAM TXTANL
C
C     *************************************************
C     *                                               *
C     *   TXTANL -LAST UPDATED 11 DEC 80 ISSUE NO.1   *
C     *   TXTANL -LAST UPDATED 26 MAR 03 ISSUE NO.2   *
C     *                                               *
C     *************************************************
C
C      THIS PROGRAM WILL TAKE THE CONCATENATED RESULTS FROM FOSYNN
C      JOBS, AND EXTRACT FROM THEM THE 'TIME-OF-DAY-IN-SECONDS'
C      NUMBERS. IT WILL WRITE THESE TO ANOTHER FILE, WHICH MAY
C      THEN BE USED AS INPUT TO THE ANALYSIS PROGRM, FOSYAN.
C            INPUT: UNIT 5
C           OUTPUT: UNIT 6
C      THE RESULTS FROM INDIVIDUAL JOBS ARE SEPARATED BY '0.000',
C      AND THE ENTIRE LIST IS TERMINATED WITH TWO LINES '-999.0'.
C
      real (8) timeline(1000000)
c
      CHARACTER *1 LINE(80),SPACE,POINT,ZERO
      character (80) textline
c
      DATA SPACE,POINT,ZERO/1H ,1H.,1H0/
c
      logical midnight
c
      equivalence(line,textline)
c
      eftmin=1.e+8
      njob=0
      nline=0
      midnight=.false.
  1   continue
      READ(5,100,END=2,err=3)(LINE(M),M=1,80)
c
Comment lines have replaced first character
c    I  PASS   DURATION   TOTAL TIME          TIME AT END OF PASS        KWIPS
c             (SECONDS)    (SECONDS)      (SECONDS)   (HH:MM:SECONDS)
c 5000     0       .000         .000      63063.188    17:31: 3.188         .0
c 5000     1      8.535        8.535      63071.723    17:31:11.723    58581.2
c 5000     2      7.629       16.164      63079.352    17:31:19.352    65540.2
c2345678901234567890123456789012345678901234567890123456789012345678901234567890
c        11111111112222222222333333333344444444445555555555666666666677777777778
c
c Pass      Blocks    Blk_size   File size Wall-clock      Duration  Data_rate   Topen  Tclose
c    0        3000     1000000  3000000000 58508.567         15.338   195597.6    .005    .000
c    1        3000     1000000  3000000000 58523.678         15.105   198603.7    .000    .000
      IF(LINE(64).EQ.POINT.AND.LINE(77).EQ.POINT)then
c
c      This is a data line from a job, so write out the time
c
                             read(textline,102)npass,tday
c     write(*,102)npass,tday
                             if(npass.eq.0.)then
c      
c      This is the first line of a job
c
                                     nline=nline+1
                                     timeline(nline)=0.
                                     nline=nline+1
                                     timeline(nline)=tday
                                     njob=njob+1
                                     t=tday
                                     eft=tday
                                     go to 1
                             endif
c
                             nline=nline+1
                             timeline(nline)=tday
c
                             if(tday.lt.t
     *                         )then
                             write(80,105)njob,t,tday,timeline(nline)
                                    timeline(nline)=
     *                              timeline(nline)+86400.
                                    midnight=.true.
                                    if(eft.lt.eftmin
     *                                )then
                                           eftmin=eft
                                    endif
                             endif
                             t=tday
      endif
c
      GO TO 1
c
c      Next section for errors....
c
  3   continue
      write(0,109)
      read(*,*)
      go to 1
c
  2   continue
c
c      All the jobs have been read - correct for midnights, if any...
c
      if(midnight)then
                      write(80,106)eftmin
                      call isoflush(80)
                      do n=1,nline
                         t=timeline(n)
                         ta=t
                         tb=t
                         if(t.gt.0
     *                     )then
                                if(t.lt.eftmin
     *                            )then
                                       t=t+86400
                                       tb=t
                                endif 
                                t=t-eftmin+1.
                                timeline(n)=t
                          endif
c                         write(80,108)n,ta,tb,timeline(n)
                      enddo
      endif
c
      do n=1,nline
         write(*,104)timeline(n)
      enddo
c
      WRITE(6,103)
c
      STOP
c
100   FORMAT(80A1)
101   FORMAT('    0.000')
102   FORMAT(i6,36x,f10.3)
103   FORMAT(2(' -999.000',/))
104   format(f10.3)
105   format('TXTANL:  Job',i4,' crossed midnight.',4f10.3)
106   format('TXTANL:  Earliest pre-midnight time was',f10.3)
107   format('Job',i4,' first time is',f10.3
     *      ,'. Earliest first time is',f10.3)
108   format('Line:',i8,', Original:',f10.3
     *      ,', Absolute:',f10.3,', Adjusted',f10.3)
109   format('TXTANL:  ***** ERROR IN READ *****')
      END
