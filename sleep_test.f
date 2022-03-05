      program sleep_test
c
      do n=1,2
         write(*,100)
         call system('usleep 5000000')
      enddo
c
      stop
c
100      format('About to sleep')
c
      end
