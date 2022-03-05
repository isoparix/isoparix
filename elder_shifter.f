      program elder_shifter
c
c      Prepares:
c        1. A non-duplicating task lists duty_queueN.script, and
c        2. A set of edit scripts, shifting the link between
c           Qnn and elder_name(i) for each script
c
      use elder_comms
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
      available=.true.
c
c**************************
c      Week 1
c**************************
c
      nweek=1
      nduties=9
      call elder_filler(nduties,nweek)
      write( *,201)(task_id(mx),mx=1,nduties)
      write(10,201)(task_id(mx),mx=1,nduties)
      write(12,201)(task_id(mx),mx=1,nduties)
c
c
c**************************
c      Week 2
c**************************
c
      nweek=2
      nduties=3
      call elder_filler(nduties,nweek)
      write( *,202)(task_id(mx),mx=1,nduties)
      write(10,202)(task_id(mx),mx=1,nduties)
      write(12,202)(task_id(mx),mx=1,nduties)
c
c**************************
c      Week 3
c**************************
c
      nweek=3
      nduties=4   ! No Vestery Elder at 1830 communion service
      call elder_filler(nduties,nweek)
      write( *,203)(task_id(mx),mx=1,nduties)
      write(10,203)(task_id(mx),mx=1,nduties)
      write(12,203)(task_id(mx),mx=1,nduties)
c
c**************************
c      Week 4
c**************************
c
      nweek=4
      nduties=4
      call elder_filler(nduties,nweek)
      write( *,204)(task_id(mx),mx=1,nduties)
c     write(10,204)(task_id(mx),mx=1,nduties)  ! Not four-week month
      write(12,204)(task_id(mx),mx=1,nduties)
c
c**************************
c      Week 5
c**************************
c
      nweek=5
      nduties=5
      call elder_filler(nduties,nweek)
      write( *,205)(task_id(mx),mx=1,nduties)
      write(12,205)(task_id(mx),mx=1,nduties)
c     mx=task_id(1)
c     task_id(1)=task_id(5)
c     task_id(5)=mx
      write(10,205)(task_id(mx),mx=1,nduties)
c
      close(10)
      close(12)
c
      ntask=80
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
c    *      ,/':/1830 Vestry/         s/></>Q',i2.2,'</'
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
