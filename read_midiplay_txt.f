      program read_midiplay_txt
c
c      Reads the diagnostic txt files and keeps a running time
c      Input is
c(dt        11)       (Note on  ch(1b)  1) 0x45 0x00 (vel.   0)  A3
c      Output is
c(dt 137    11)       (Note on  ch(1b)  1) 0x45 0x00 (vel.   0)  A3
c
      character (3) dtlead
      character (53)diags
      character (68)txtline
c
      ntime=0
  1   continue
      read(3,102,end=2)txtline
      read(txtline,100)dtlead
      if(dtlead.eq.'Tra'
     *  )then
             ntime=0
      endif
      if(dtlead.eq.'(dt'
     *  )then
             read(txtline,100)dtlead,ndt,diags
             ntime=ntime+ndt
             write(*,101)dtlead,ntime,ndt,diags,ntime
         else
             write(*,*)txtline
      endif
      go to 1
c
  2   stop
c
100   format(a3,i10,a53)
101   format(a3,i6,i4,a53,i6)
102   format(a68)
c
      end
