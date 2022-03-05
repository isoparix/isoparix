      program elder_shifter
c
c      Prepares:
c        1. A non-duplicating task lists duty_queueN.script, and
c        2. A set of edit scripts, shifting the link between
c           Qnn and elder_name(i) for each script
c
      character(10) elder_name(20)
      character(14) script_name
c
      logical available(26),eligible(0:26)
c
      integer task_id(10)
c
c      Create duty_queue4,script and ...5...
c
      open(10,form='formatted',file='duty_queue4.script'
     *     ,status='unknown')
      open(12,form='formatted',file='duty_queue5.script'
     *     ,status='unknown')
c
c      Read in the list of Elders
c
      ncount=1
  1   continue
      read(*,*,end=2)elder_name(ncount)
      ncount=ncount+1
      go to 1
  2   continue
      ncount=ncount-1
      write(*,*)'There are',ncount,' Elders in the list'
cx  3   continue
cx      read(*,*)ncount
      available=.true.
c
c**************************
c      Week 1
c**************************
c
      eligible =.true.
      do n=10,26,2
         eligible(n)=.false.
      enddo
c
      nfill=0
      ntasks=9     ! Number of tasks this Sunday
      task_id=0
      do n=26,1,-1 ! Max tasks per month
         if(available(n).and.eligible(n)
     *     )then
                nfill=nfill+1
                task_id(nfill)=n
                available(n)=.false.
                if(nfill.eq.ntasks)exit
                ibase=mod(n,ncount)
                do m=ibase,26,ncount
                   eligible(m)=.false.
                   !write(12,800)m
                enddo
          endif
      enddo
      if(nfill.ne.ntasks
     *  )then
             write(0,801)nfill,ntasks,1
      endif
      write( *,201)(task_id(mx),mx=1,ntasks)
      write(10,201)(task_id(mx),mx=1,ntasks)
      write(12,201)(task_id(mx),mx=1,ntasks)
c
c
c**************************
c      Week 2
c**************************
c
      eligible =.true.
c
      nfill=0
      ntasks=3     ! Number of tasks this Sunday
      task_id=0
      do n=26,1,-1 ! Max tasks per month
         if(available(n).and.eligible(n)
     *     )then
                nfill=nfill+1
                task_id(nfill)=n
                available(n)=.false.
                if(nfill.eq.ntasks)exit
                ibase=mod(n,ncount)
                do m=ibase,26,ncount
                   eligible(m)=.false.
                   !write(12,800)m
                enddo
          endif
      enddo
      if(nfill.ne.ntasks
     *  )then
             write(0,801)nfill,ntasks,2
      endif
      write( *,202)(task_id(mx),mx=1,ntasks)
      write(10,202)(task_id(mx),mx=1,ntasks)
      write(12,202)(task_id(mx),mx=1,ntasks)
c
c**************************
c      Week 3
c**************************
c
      eligible =.true.
c
      nfill=0
      ntasks=5     ! Number of tasks this Sunday
      task_id=0
      do n=1,26    ! Max tasks per month
         if(available(n).and.eligible(n)
     *     )then
                nfill=nfill+1
                task_id(nfill)=n
                available(n)=.false.
                if(nfill.eq.ntasks)exit
                ibase=mod(n,ncount)
                do m=ibase,26,ncount
                   eligible(m)=.false.
                   !write(12,800)m
                enddo
          endif
      enddo
      if(nfill.ne.ntasks
     *  )then
             write(0,801)nfill,ntasks,3
      endif
      write( *,203)(task_id(mx),mx=1,ntasks)
      write(10,203)(task_id(mx),mx=1,ntasks)
      write(12,203)(task_id(mx),mx=1,ntasks)
c
c**************************
c      Week 4
c**************************
c
      eligible =.true.
c
      nfill=0
      ntasks=4     ! Number of tasks this Sunday
      task_id=0
      do n=26,1,-1 ! Max tasks per month
         if(available(n).and.eligible(n)
     *     )then
                nfill=nfill+1
                task_id(nfill)=n
                available(n)=.false.
                if(nfill.eq.ntasks)exit
                ibase=mod(n,ncount)
                do m=ibase,26,ncount
                   eligible(m)=.false.
                   !write(12,800)m
                enddo
          endif
      enddo
      if(nfill.ne.ntasks
     *  )then
             write(0,801)nfill,ntasks,4
      endif
      write( *,204)(task_id(mx),mx=1,ntasks)
c     write(10,204)(task_id(mx),mx=1,ntasks)  ! Not four-week month
      write(12,204)(task_id(mx),mx=1,ntasks)
c
c**************************
c      Week 5
c**************************
c
      eligible =.true.
c
      nfill=0
      ntasks=5     ! Number of tasks this Sunday
      task_id=0
      do n=1,26    ! Max tasks per month
         if(available(n).and.eligible(n)
     *     )then
                nfill=nfill+1
                task_id(nfill)=n
                available(n)=.false.
                if(nfill.eq.ntasks)exit
                ibase=mod(n,ncount)
                do m=ibase,26,ncount
                   eligible(m)=.false.
                   !write(12,800)m
                enddo
          endif
      enddo
      if(nfill.ne.ntasks
     *  )then
             write(0,801)nfill,ntasks,5
      endif
      write( *,205)(task_id(mx),mx=1,ntasks)
      write(10,205)(task_id(mx),mx=1,ntasks)
      write(12,205)(task_id(mx),mx=1,ntasks)
c
      close(10)
      close(12)
C      go to 3
c
      ntask=30
      do nscript=1,12
         write(script_name,100)nscript
         open(4,file=script_name,form='formatted',status='unknown')
         write(4,103)
         id_elder=mod(nscript,ncount)
         do n=1,ntask
            id_elder=id_elder+1
            if(id_elder.gt.ncount)id_elder=1
c           write(*,102)nscript,id_elder,n,trim(elder_name(id_elder))
            write(4,101)n,trim(elder_name(id_elder))
         enddo
         write(4,104)
         close(4)
      enddo
c
      stop
c
100   format('shift',i2.2,'.script')
101   format('%s/Q',i2.2,'/',a,'/')
102   format(2i6,' %s/Q',i2.2,'/',a,'/')
103   format('1')
104   format('w',/'q')
c
201   format('1'                                        ! Week 1
     *      ,/':/Opening:/            s/:/: Q19/'
     *      ,/''
     *      ,/':/1000 Vestry/         s/></>Q',i2.2,'</'
     *      ,/':/1000 Mid-foyer/      s/></>Q',i2.2,'</'
     *      ,/':/1000 Communion prep/ s/></>Q',i2.2,'</'
     *      ,/':/1000 Server1/        s/></>Q',i2.2,'</'
     *      ,/':/1000 Server2/        s/></>Q',i2.2,'</'
     *      ,/':/1000 Server3/        s/></>Q',i2.2,'</'
     *      ,/':/1000 Server4/        s/></>Q',i2.2,'</'
     *      ,/':/1130 Vestry/         s/></>Q',i2.2,'</'
     *      ,/':/1830 Vestry/         s/></>Q',i2.2,'</'
     *      ,/''
     *      ,/'')
c
202   format(/':/1030 Vestry/         s/></>Q',i2.2,'</' ! Week 2
     *      ,/':/1030 Mid-foyer/      s/></>Q',i2.2,'</'
     *      ,/':/1830 Vestry/         s/></>Q',i2.2,'</'
     *      ,/''
     *      ,/'')
c
203   format(/':/1000 Vestry/         s/></>Q',i2.2,'</' ! Week 3
     *      ,/':/1000 Mid-foyer/      s/></>Q',i2.2,'</'
     *      ,/':/1130 Vestry/         s/></>Q',i2.2,'</'
     *      ,/':/1830 Vestry/         s/></>Q',i2.2,'</'
     *      ,/':/1830 Communion prep/ s/></>Q',i2.2,'</'
     *      ,/''
     *      ,/'')
c
204   format(/':/1000 Vestry/         s/></>Q',i2.2,'</' ! (Week 4)
     *      ,/':/1000 Mid-foyer/      s/></>Q',i2.2,'</'
     *      ,/':/1130 Vestry/         s/></>Q',i2.2,'</'
     *      ,/':/1830 Vestry/         s/></>Q',i2.2,'</'
     *      ,/''
     *      ,/'')
c
205   format(/':/1000 Vestry/         s/></>Q',i2.2,'</' ! Last week
     *      ,/':/1000 Mid-foyer/      s/></>Q',i2.2,'</'
     *      ,/':/1130 Vestry/         s/></>Q',i2.2,'</'
     *      ,/':/1130 Communion prep/ s/></>Q',i2.2,'</'
     *      ,/':/1830 Vestry/         s/></>Q',i2.2,'</'
     *      ,/''
     *      ,/'w'
     *      ,/'q')
c
800   format('Option',i3,' no longer eligible')                   
801   format('Filled:',i3,', Tasks:',i3,', Week',i3)
c
      end      
