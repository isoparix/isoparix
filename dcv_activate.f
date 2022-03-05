      subroutine dcv_activate(kbutton)
c
      character (20) actname
c
      write(actname,100)kbutton
c     write(*,*)actname
      call system(actname)
c
      return
c
100   format('./key_act_dcvbutt.',i2.2)
c
      end
