c
c
c
      integer (2) function exintel2(char_inp)
c
c      Converts a 2-byte Intel integer to a proper one
c
      character (2) char_inp
      character (2) char_tmp,char_tmpa
      character (1) cdata(2)
c
      integer (2) intel
c
      equivalence (char_tmpa,intel)
      equivalence (char_tmp, cdata)
c
      char_tmp =char_inp
      char_tmpa=cdata(2)//cdata(1)
c
      exintel2=intel
c
      end
