      subroutine plotutil(aspect,tstart,period,nsamples)
      use allcomms
C
C      USES THE INTERVAL RECORDS TO PLOT DAILY USE
c
c      All samples have already been averaged by the time they get here,
c      so if the 'average count' was 6, then each sample here is 
c      already the mean of 6 original iostat or vmstat data lines, 
c      IF a rate value....
C
      dimension ASPECT(NSAMPLES),TSTART(nline),PERIOD(NSAMPLES)
C
      real (8) sigx2
c
      aspmax=0.
      aspmin=huge(aspmin)
      average=0.
      sigx2=0.
      do i=0,14
         khist(i)=0
      enddo
c
      SUBNAM='PLOTUTIL'
      DO M=1,86400
         UTILMIN(M)=huge(utilmin(m))
         UTILMAX(M)=0.
         UTIL(M)=0.
         JUTL(M)=0
         KUTL(M)=0
      ENDDO
C
C      ADD THE VALUE IN ASPECT TO ALL THE ONE-SECOND SLOTS DEFINED BY
C      TSTART AND PERIOD.
C
      DO I=1,nsamples
         if(aspect(i).gt.aspmax)aspmax=aspect(i)
         if(aspect(i).lt.aspmin)aspmin=aspect(i)
         if(period(i).gt.0.)then
c
c        if(i.lt.500)write(30,105)i,nq,(((i-1)*nq)+1)
c        call flush(30)
                   ts=tstart(((i-1)*nq)+1)
c
c      Time should start in second '1'...
c
                   ITA=TS+1.
                   ITB=TS+PERIOD(I)
c        if(i.lt.500)write(30,104)i,ita,itb,period(i),aspect(i),title
c        call flush(30)
c
c      Keep a running total
c
                   average=average+aspect(i)
                   sigx2  =sigx2  +aspect(i)**2
                   IF(ITB.LE.86400)THEN
                                       CALL ADDUTIL(ITA,ITB,ASPECT(I))
                                   ELSE
                                       CALL ADDUTIL(ITA,86400,ASPECT(I))
                                       ITC=ITB-86400
                                       CALL ADDUTIL(  1,  ITC,ASPECT(I))
                   ENDIF
        ENDIF
      ENDDO
c
c      Find the average of all the samples...
c
      average=average/float(nsamples)
      stddev =sqrt((sigx2/float(nsamples))-average**2)
C
C      Average over NAVG seconds                               
C
      k=0
      kblkmin=-1
      itinit=0
c
      do i=1,86400,navg
c
         aju =0.               
         aumn=0.               
         au  =0.               
         aumx=0.
c
         nqa=0
         k=k+1
         mapmax(k)=0.
         mapmin(k)=huge(mapmin(k))
c
         do j=i,i+navg-1
           if(j.le.86400)then 
            if(jutl(j).gt.0)then
                        nqa=nqa+1
                        aju=aju+jutl(j)
                        au =au +util(j)
                        if(utilmax(j).gt.mapmax(k))mapmax(k)=utilmax(j)
                        if(utilmin(j).lt.mapmin(k))mapmin(k)=utilmin(j)
            endif   
           endif   
         enddo
c
         MAP   (K)=au/aju
         mapsum(K)=au/float(nqa)
         ITMAP (K)=i+itinit+(navg/2)
         KUTL  (K)=.5+(aju/float(nqa))
c
         if(kutl(k).gt.0)then
                             kblkmax=k
                             if(kblkmin.eq.-1)then 
                                                  kblkmin=k
                             endif
         endif
c  
c
      enddo
c
c      Write out number of samples and the min/mean/max
c
c     write(*,103)title
      recip=1./3600.
c     write(*,*)kblkmin,kblkmax
      do k=kblkmin,kblkmax
c        write(*,702)k,kblkmin,kblkmax
         time_of_day=recip*real(itmap(k))
c        if(kutl(k).gt.0)
c    *              write(40,102)time_of_day,kutl(k),mapsum(k),mapmin(k)
c    *               ,map(k),mapmax(k)
      enddo
c
c30      write(30,701)map(k),itmap(k),jutl(k),mapmax(k),mapmin(k)
701   format(e12.5,2i10,2e12.5)
702   format('K=',i8,' of',2i8)
c                                                  
c      Write histogram details                     
c                                                  
      do j=0,14                              
         khist(j)=0                                
      enddo                                        
c                                                  
      if(aspmax.gt.0.)then
c
c      Do 'standard' histogram
c
                dspan=15./(1.0001*aspmax) 
c                                                  
                do j=1,nsamples                              
                   kcell=aspect(j)*dspan                   
                   if(kcell.ge.0.and.
     *                kcell.le.14
     *               )then
                          khist(kcell)=khist(kcell)+1               
                      else
                          write(*,106)
     *                     aspmax,aspmin,dspan,aspect(j),j,kcell
                   endif
                enddo                                        
         else
c
c      Make first bar of histogram the only one used
c
                khist(0)=1
      endif
c
      maxhist=0                                                  
      do j=0,14
         if(khist(j).gt.maxhist)maxhist=khist(j)
      enddo
c
      ax=60./float(maxhist)
      do j=0,14
         khist(j)=.5+(float(khist(j))*ax)
      enddo
c
c30   write(30,100)title,kblkmax-kblkmin+1
      nitems=nsamples
c     call flush(30)
      CALL BAR
C
100   FORMAT(a60,'<br>',/I6,' INTERVALS IN DAILY PROFILE<br>')
102   format(f7.4,',',i6,4(',',e13.6))
103   format(/a60,'<br>')
104   format('Sample',i6,': ITA=',i6,', ITB=',i6
     *     ,', Period=',e10.3,', value=',e10.3,a62)
105   format('Sample',i6,': NQ=',i6,', (((I-1)*NQ)+1)=',i6)
106   format('ASPMAX=',e14.5
     *     ,/'ASPMIN=',e14.5
     *     ,/' DSPAN=',e14.5
     *     ,/'ASPECT=',e14.5
     *     ,/'SAMPLE=',i14
     *     ,/' KCELL=',i14
     *     ,/)
C
      RETURN
      END
