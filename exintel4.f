c
c
c
      integer (4) function exintel4(char_inp)
c
c      Converts a 4-byte Intel integer to a proper one
c
      character (4) char_inp
      character (4) char_tmp,char_tmpa
      character (1) cdata(4)
c
      integer (4) intel
c
      equivalence (char_tmpa,intel)
      equivalence (char_tmp, cdata)
c
      char_tmp =char_inp
      char_tmpa=cdata(4)//cdata(3)//cdata(2)//cdata(1)
c
      exintel4=intel
c
      end
