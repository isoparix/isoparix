      subroutine easy_midi_xvolume
c
      use easy_midi_comms
c
c      Assumes values for delta_time and nvolume are set elsewhere...
c      nvolume, delta_time are not changed
c
c      Writes delta time and the meta-event details.
c      DOES advance mt(idtvol)....
c
      lt=idtvol
      minit=mt(lt)
      call make_midi_time(lt)
      track_time(lt)=track_time(lt)+delta_time
c
      track_data(mt(lt)  ,lt)=char(int(z'F0'))! SysEx event
      track_data(mt(lt)+1,lt)=char(int(z'07'))! Seven bytes after this
c
      mt(lt)=mt(lt)+2
c
      track_data(mt(lt)  ,lt)=char(int(z'7F'))! RealTime
      track_data(mt(lt)+1,lt)=char(int(z'7F'))! SysEx channel(ignore chann)
      track_data(mt(lt)+2,lt)=char(int(z'04'))! Sub-ID Device Control
      track_data(mt(lt)+3,lt)=char(int(z'01'))! Sub-ID2 Master Volume
      track_data(mt(lt)+4,lt)=char(note_vol) ! Bits 0 -  6 of 14-bit volume
      track_data(mt(lt)+5,lt)=char(note_vol) ! Bits 7 - 13 of 14-bit volume
      track_data(mt(lt)+6,lt)=char(int(z'F7'))! End of SysEx
c
      mt(lt)=mt(lt)+7
c
      if(check
     *  )then
             if(ichar(track_data(minit,lt)).le.127
     *         )then
                    write(12,132)note_vol,minit,mt(lt)-1
     *                          ,(ichar(track_data(mx,lt)),mx=minit
     *                          ,mt(lt)-1)
                else
                    write(12,133)note_vol,minit,mt(lt)-1
     *                          ,(ichar(track_data(mx,lt)),mx=minit
     *                          ,mt(lt)-1)
             endif
      endif
c
      return
c
132   format('Expresn: nvolume',i7,', Bytes',i4,' to',i4,':    '
     *       ,16z3.2)
133   format('Expresn: nvolume',i7,', Bytes',i4,' to',i4,': '
     *       ,16z3.2)
c
      end
