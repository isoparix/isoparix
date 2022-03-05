      character (2) function adintel2(k)
c
c      Converts integer K to Intel variant in adintel
c
      integer (2) k
      integer (2) k_tmp
      character (1) char_tmp(2)
      equivalence (char_tmp,k_tmp)
c
      k_tmp=k
      adintel2=char_tmp(2)//char_tmp(1)
c
      end
