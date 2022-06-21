      subroutine micropause(microseconds)
c
c      Pause for (approximately)microseconds...
c
      real(8) t,ta,tb
      integer(4) microseconds, usleep_
      character(16)sleepus
c
c#ifdef cygwin
c      write(sleepus,100)microseconds
c      call system(sleepus)
c#elif defined _AIX
c      is_sleep=usleep_(microseconds)
c#else
cError Unknown operating system
c#endif
c
      call tim(ta)
      tb=ta+(.000001*real(microseconds))
   1  continue
      call tim(t)
      if(t.gt.tb
     *  )then   
             return
      endif       
      go to 1
c
100   format('usleep',i10)
c
      end
