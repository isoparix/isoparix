      program ztest
c
c      Checks hexadecimal editing - this is edit from Fedora
c
      character (4) test_char
      integer (4) test_int
c
      test_char='ABCD'
      test_int =255*256 +170
c
      write(*,100)test_char
      write(*,101)test_int
c
      stop
c
100   format(a4)
101   format(z8)
c
      end

