      subroutine easy_midi_timesig(inum,iden)
c
      use easy_midi_comms
c
c      Assumes value for delta_time is set elsewhere...
c      delta_time is not changed
c
c      Writes delta time and the meta-event details.
c      DOES advance mt(idttmp)....
c
      lt=idttmp
      minit=mt(lt)
      call make_midi_time(lt)
      track_time(lt)=track_time(lt)+delta_time
      track_data(mt(lt)  ,lt)=char(int(z'FF')) ! Meta-event
      track_data(mt(lt)+1,lt)=char(int(z'58')) ! Time signature
      track_data(mt(lt)+2,lt)=char(int(z'04')) ! Time signature
      track_data(mt(lt)+3,lt)=char(inum)  ! Numerator
      idena=.5+(log(float(iden))/log(2.0))
      track_data(mt(lt)+4,lt)=char(idena) ! Denominator as power of 2
      track_data(mt(lt)+5,lt)=char(int(z'18')) ! MIDI clocks/metronome tick
      track_data(mt(lt)+6,lt)=char(int(z'08')) ! Notated 32nd notes/MIDI quarter note
      mt(lt)=mt(lt)+7
c
      if(check
     *  )then
             if(ichar(track_data(minit,lt)).le.127
     *         )then
                    write(12,132)             inum,iden,minit,mt(lt)-1
     *                    ,(ichar(track_data(mx,lt)),mx=minit,mt(lt)-1)
                else
                    write(12,133)             inum,iden,minit,mt(lt)-1
     *                    ,(ichar(track_data(mx,lt)),mx=minit,mt(lt)-1)
             endif
      endif
c
c
      return
c
132   format('    Time signature',2i4,', Bytes',i4,' to',i4,':',7x
     *      ,16z3.2)
133   format('    Time signature',2i4,', Bytes',i4,' to',i4,':',4x
     *      ,16z3.2)
      
c
      end
