      program test_endian
c
c      How do we store integers?
c
      character (1) testchar(4)
      character (4) outchar
      integer   (4) testint
c
      equivalence (testint,testchar)
c
      testint=x'89ABCDEF'
c
      write(outchar,100)(testchar(mx),mx=1,4)
      write(*,101)outchar
c
      stop
c
100   format(4a1)
101   format(z8)
c
      end
