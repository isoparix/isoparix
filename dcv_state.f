      program dcv_state 
c
c      Reads DCV state
c
      use dcv_comms
c
c/DVDEMO/SCREEN_CONFIGS/004_7146x2264_1680x2264_Xbezel.cfg
c
  1   continue
c
      call system('sleep 1')
c
      call read_dcv_state
c
      call assess_dcv_state
c
      go to 1
c
      stop
c
      end

