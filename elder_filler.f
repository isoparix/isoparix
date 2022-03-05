      subroutine elder_filler(nduties,nweek)
      use elder_comms
c
c      Prepares:
c        1. A non-duplicating task lists duty_queueN.script, and
c        2. A set of edit scripts, shifting the link between
c           Qnn and elder_name(i) for each script
c
c
c**************************
c      Week nweek
c**************************
c
      eligible =.true.
      ibase=1+mod(nweek,3)
      do n=ibase,80,4
         eligible(n)=.false.
      enddo
c
      nfill=0
      task_id=99
      do n=1,80 ! Max duties per month
c        write(0,*)n,available(n),eligible(n)
         if(available(n).and.eligible(n)
     *     )then
                nfill=nfill+1
                task_id(nfill)=n
                available(n)=.false.
                if(nfill.eq.nduties)exit
                ibase=mod(n,ncount)
                do m=ibase,80,ncount
                   eligible(m)=.false.
                   write(0,800)m
                enddo
          endif
      enddo
      if(nfill.ne.nduties
     *  )then
             write(0,801)nfill,nduties,nweek
      endif
c
800   format('Option',i3,' no longer eligible')                   
801   format('Filled:',i3,', Duties:',i3,', Week',i3)
c
      return
c
      end      
