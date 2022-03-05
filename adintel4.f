c
c
c
      character (4) function adintel4(k)
c
c      Converts integer K to Intel variant in adintel
c
      integer (4) k
      integer (4) k_tmp
      character (1) char_tmp(4)
      equivalence (char_tmp,k_tmp)
c
      k_tmp=k
      adintel4=char_tmp(4)//char_tmp(3)//char_tmp(2)//char_tmp(1)
c
      end
