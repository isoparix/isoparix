      program killmulti
c
c      Kills processes in runlist 
c
      character (20) killcomm,jobname
c
      open(3,file='runlist',status='old',err=3)
      read(3,*)killppid
  1   continue
      read(3,*,end=2,err=4)jobname,killid,killpp
      if(killpp.eq.killppid
     *  )then
             write(killcomm,101)killid
         else
c
c      Careful we don't attack something important in error!
c
             write(*,105)jobname,killid
             go to 1
      endif
      write(*,102)killcomm
      call system(killcomm)
      go to 1
c
  4   continue
      write(*,104)jobname,killid
      go to 1
c
c      What if no runlist?
c
  3   continue
      write(*,103)
c
  2   continue
      stop
100   format(8x,i6)
101   format('kill -9',i13)
102   format('JOBKILL: ',a20)
103   format(' Error opening file "runlist" ')
104   format(' Error reading runlist entry - jobname/PID',a15,i13)
105   format(' Suspect jobname/PID: ',a,i13
     *      ,'.  Job will NOT be killed')
      end
