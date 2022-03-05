      subroutine easy_midi_tvolume
c
      use easy_midi_comms
c
c      Sets track volume
c
      if(.not.running
     *  )then
             track_data(mt(idt)  ,idt)=char(int(z'00')) ! Delta-time zero
             mt(idt)=mt(idt)+1
      endif       
      track_data(mt(idt)  ,idt)=char(ncon_on_chann)! Controller
      track_data(mt(idt)+1,idt)=char(7)            ! Volume
      track_data(mt(idt)+2,idt)=char(note_vol)     ! Value
      if(check
     *  )then
             write(12,100)idt,note_vol,mt(idt),mt(idt)+2
     *     ,(ichar(track_data(mx,idt)),mx=mt(idt),mt(idt)+2)
      endif
      mt(idt)=mt(idt)+3
      last_event(idt)=.true.
      running=.false.
c
100   format('Track',i2,': nvolume',i7,', Bytes',i4,' to',i4,':    '
     *       ,3z3.2)
c
c
      return
c
c
      end
