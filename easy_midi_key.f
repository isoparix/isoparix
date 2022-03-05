      subroutine easy_midi_key(nsf,majmin)
c
      use easy_midi_comms
c
c      Assumes value for delta_time is set elsewhere...
c      delta_time is not changed
c
c      Writes delta time and the meta-event details.
c      DOES advance mt(idttmp)....
c
      lt=1
      minit=mt(lt)
      call make_midi_time(lt)
      track_time(lt)=track_time(lt)+delta_time
      track_data(mt(lt)  ,lt)=char(int(z'FF')) ! Meta-event
      track_data(mt(lt)+1,lt)=char(int(z'59')) ! Key signature
      track_data(mt(lt)+2,lt)=char(int(z'02')) ! Two bytes
      track_data(mt(lt)+3,lt)=char(nsf)   ! Key: sharps>0>flats
      track_data(mt(lt)+4,lt)=char(majmin)! Major=0, minor=1
      mt(lt)=mt(lt)+5
c
      if(check
     *  )then
             write(0,131)nsf,majmin
             if(ichar(track_data(minit,lt)).le.127
     *         )then
                    write(12,132)         nsf,majmin,minit,mt(lt)-1
     *                    ,(ichar(track_data(mx,lt)),mx=minit,mt(lt)-1)
                else
                    write(12,133)         nsf,majmin,minit,mt(lt)-1
     *                    ,(ichar(track_data(mx,lt)),mx=minit,mt(lt)-1)
             endif
      endif
c
c
      return
c
131   format('     Key signature',2i8)
132   format('     Key signature',2i4,', Bytes',i4,' to',i4,':',7x
     *      ,16z3.2)
133   format('     Key signature',2i4,', Bytes',i4,' to',i4,':',4x
     *      ,16z3.2)
      
c
      end
