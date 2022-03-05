      program test_make_midi_time
c
      use easy_midi_comms
c
      ntrack=6
      ninit=3100
  1   continue
         nbyte=ninit
         read(*,*)delta_time
         call make_midi_time(nbyte,ntrack)
         write(*,100)nbyte
     *             ,(track_data(mx,ntrack),mx=ninit,nbyte-1)
         go to 1
c
100   format('Next free byte:',i6,'. Result',6z3.2)
c
      stop
      end      
