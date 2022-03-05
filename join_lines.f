      program join_lines
      character(10)b
      read(*,*)i
      read(*,*)b
      write(*,100)i,b
      stop
c
100   format(i2,a12)
c
      end

