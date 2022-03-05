      subroutine easy_midi_tempo
c
      use easy_midi_comms
c
c      Assumes values for delta_time and ntempo are set elsewhere...
c      ntempo, delta_time are not changed
c
c      Writes delta time and the meta-event details.
c      DOES advance mt(lt)....
c
      lt=idttmp
      minit=mt(lt)
      call make_midi_time(lt)
      track_time(lt)=track_time(lt)+delta_time
      track_data(mt(lt)  ,lt)=char(int(z'FF'))! Meta-event
      track_data(mt(lt)+1,lt)=char(int(z'51'))! 0x51 Tempo
      track_data(mt(lt)+2,lt)=char(int(z'03'))! 0x03 Tempo
      mt(lt)=mt(lt)+3
c
c      Compute and record tempo setting
c      500,000 decimal gives 120 beats_per_minute
c                       
      n=ntempo
      k=256*256
      do j=1,3
         m=n/k
         track_data(mt(lt),lt)=char(m)
         mt(lt)=mt(lt)+1
         n=n-(m*k)
         k=k/256
      enddo
c
      if(check
     *  )then
             if(ichar(track_data(minit,lt)).le.127
     *         )then
                    write(12,132)             ntempo,minit,mt(lt)-1
     *                    ,(ichar(track_data(mx,lt)),mx=minit,mt(lt)-1)
                else
                    write(12,133)             ntempo,minit,mt(lt)-1
     *                    ,(ichar(track_data(mx,lt)),mx=minit,mt(lt)-1)
             endif
      endif
c
c
      return
c
132   format('     Tempo: ntempo',i8,', Bytes',i4,' to',i4,':',7x
     *      ,16z3.2)
133   format('     Tempo: ntempo',i8,', Bytes',i4,' to',i4,':',4x
     *      ,16z3.2)
c
      end
