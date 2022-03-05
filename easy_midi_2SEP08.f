      program easy_midi
c
c     Transcribes simple input notation to MIDI
c
c      Rules:
c       'text'  Text
c	#	Sharpened
c	_	Flattened (underscore)
c	=	Naturalised
c	a-g, A-G, r	The note or rest
c	Kn_/Kn#	Key Eg: K1_ (one flat) is key of F
c       Length	Duration: 1=hemidemisemiquaver
c       i                 2=demisemiquaver
c       i.                3=demisemiquaver (dotted)
c       j                 4=semiquaver
c       j.                6=semiquaver (dotted)
c       k                 8=quaver
c       k.                12=quaver (dotted)
c       l                 16=crotchet
c       l.                24=crotchet (dotted)
c       m                 32=minim
c       m.                48=minim (dotted)
c       n                 64=semibreve
c       n.                96=semibreve (dotted)
c	Qontinuity	Q0 - staccato; Q9 - legato
c       T[S|s]mn          Time signature m/n
c       TA	Track begins
c       TZ      Track end to F)
c
      use easy_midi_comms
c
c      Set defaults
c
      kont_def=7	! Staccato 0; Legato 9
      idtexp=2
      volume_track_time=0  ! Last time anything happened on volume track
      idttmp=3
      tempo_track_time=0  ! Last time anything happened on tempo track
      ntracks=idttmp ! The first score track will be ntracks+1
      interval=6
      ntempo=500000
      staccato=.false.
      jaccidental=0
      nstack=0
      chord=.false.
      running=.false.
      running_delta=0
      silent=.false.
      key_init=.true.
      noct=4		! octaVe
      length=24 	! Crotchet
      nkey=0		! C major
      kont=kont_def
      note_vel_def=64 	! Note velocity (S/s=5)
      note_vol=96 	! Note volume (U/u=5)
      nexp=note_vol
c
c      Set up the naturals
c
      natural(1)= 9
      natural(2)=11
      natural(3)= 0
      natural(4)= 2
      natural(5)= 4
      natural(6)= 5
      natural(7)= 7
c
      midi_data=' '
      track_data=' '
      track_time=0
c
c      Setup headers for all tracks, used or not...
c
      do idt=1,maxtracks
         track_data(1,idt)='M'
         track_data(2,idt)='T'
         track_data(3,idt)='r'
         track_data(4,idt)='k'
c
c      Four bytes follow with the size of the track, so position track index
c
         mt(idt)=9
c
      enddo
c
c      MIDI header
c
      midi_data( 1)='M'
      midi_data( 2)='T'
      midi_data( 3)='h'
      midi_data( 4)='d'
      midi_data( 5)=char(z'00')
      midi_data( 6)=char(z'00')
      midi_data( 7)=char(z'00')
      midi_data( 8)=char(z'06')
      midi_data( 9)=char(z'00')
      midi_data(10)=char(z'01')
      midi_data(11)=char(z'00')
c     midi_data(12)=char(z'01')	! Number of tracks in file - set later
      midi_data(13)=char(z'00') ! Pulses, or clocks, per quarter note
      midi_data(14)=char(z'18')	! Pulses, or clocks, per quarter note
      md_count=14
c
c      Definition track
c
      len_txt=12
      track_data(mt(1)  ,1)=char(z'00')		! Delta-time zero
      track_data(mt(1)+1,1)=char(z'FF')		! Meta-event
      track_data(mt(1)+2,1)=char(z'01')		! 0x01 Text
      track_data(mt(1)+3,1)=char(len_txt)	! len_txt bytes of text
      do m=mt(1)+4,mt(1)+3+len_txt
         track_data(m,1)='X'			! Initial fill
      enddo
      mt(1)=mt(1)+4+len_txt
c
c      General track
c
      track_data(mt(1)  ,1)=char(z'00')! Delta-time zero
      track_data(mt(1)+1,1)=char(z'FF')! Meta-event
      track_data(mt(1)+2,1)=char(z'03')! 0x03 Sequence/Track name
      track_data(mt(1)+3,1)=char(7)    ! Length of following text
      mt(1)=mt(1)+4
      track_data(mt(1)  ,1)='G'
      track_data(mt(1)+1,1)='e'
      track_data(mt(1)+2,1)='n'
      track_data(mt(1)+3,1)='e'
      track_data(mt(1)+4,1)='r'
      track_data(mt(1)+5,1)='a'
      track_data(mt(1)+6,1)='l'
      mt(1)=mt(1)+7
c
      len_copytxt=32
      track_data(mt(1)  ,1)=char(z'00')		! Delta-time zero
      track_data(mt(1)+1,1)=char(z'FF')		! Meta-event
      track_data(mt(1)+2,1)=char(z'02')		! 0x02 Copyright
      track_data(mt(1)+3,1)=char(len_copytxt)	! len_copytxt bytes of text
      ncopy_start=mt(1)+4
      ncopy_end  =mt(1)+3+len_copytxt
      do m=ncopy_start,ncopy_end
         track_data(m,1)='C'			! Initial fill
      enddo
      mt(1)=mt(1)+4+len_copytxt
c
      track_data(mt(1)  ,1)=char(z'00')	! Delta-time zero
      track_data(mt(1)+1,1)=char(z'FF')	! Meta-event
      track_data(mt(1)+2,1)=char(z'58')	! Time signature
      track_data(mt(1)+3,1)=char(z'04')	! Time signature
      track_data(mt(1)+4,1)=char(z'03')	! Numerator (default is 4)
      ntsnum=mt(1)+4
      track_data(mt(1)+5,1)=char(z'02')	! Denominator as power of 2 (2**2 = 4, so default is 4/4)
      ntsden=mt(1)+5
      track_data(mt(1)+6,1)=char(z'18')	! MIDI clocks in a metronome tick (set to 24)
      track_data(mt(1)+7,1)=char(z'08')	! Number of notated 32nd notes in a MIDI quarter note
      mt(1)=mt(1)+8
c
      track_data(mt(1)  ,1)=char(z'00')	! Delta-time zero
      track_data(mt(1)+1,1)=char(z'FF')	! Meta-event
      track_data(mt(1)+2,1)=char(z'59')	! Key signature
      track_data(mt(1)+3,1)=char(z'02')	! Key signature
      track_data(mt(1)+4,1)=char(z'00')	! -7 for 7 flats, -1 for 1 flat, etc, 0 for key of c, 1 for 1 sharp, etc.
      ntkey=mt(1)+4
      track_data(mt(1)+5,1)=char(z'00')	! 0 for major, 1 for minor
      ntmajorminor=mt(1)+5
      mt(1)=mt(1)+6                     ! This is now one greater than the exact track length
      write(*,*)ntkey,ntmajorminor
c
c      Other definition events come next
c
      track_started=.false.
c
c      Expression track
c
      track_data(mt(idtexp)  ,idtexp)=char(z'00')! Delta-time zero
      track_data(mt(idtexp)+1,idtexp)=char(z'FF')! Meta-event
      track_data(mt(idtexp)+2,idtexp)=char(z'03')! 0x03 Sequence/Track name
      track_data(mt(idtexp)+3,idtexp)=char(10)   ! Length of following text
      mt(idtexp)=mt(idtexp)+4
      track_data(mt(idtexp)  ,idtexp)='E'
      track_data(mt(idtexp)+1,idtexp)='x'
      track_data(mt(idtexp)+2,idtexp)='p'
      track_data(mt(idtexp)+3,idtexp)='r'
      track_data(mt(idtexp)+4,idtexp)='e'
      track_data(mt(idtexp)+5,idtexp)='s'
      track_data(mt(idtexp)+6,idtexp)='s'
      track_data(mt(idtexp)+7,idtexp)='i'
      track_data(mt(idtexp)+8,idtexp)='o'
      track_data(mt(idtexp)+9,idtexp)='n'
      mt(idtexp)=mt(idtexp)+10
c
c      Tempo track
c
      track_data(mt(idttmp)  ,idttmp)=char(z'00')! Delta-time zero
      track_data(mt(idttmp)+1,idttmp)=char(z'FF')! Meta-event
      track_data(mt(idttmp)+2,idttmp)=char(z'03')! 0x03 Sequence/Track name
      track_data(mt(idttmp)+3,idttmp)=char(5)    ! Length of following text
      mt(idttmp)=mt(idttmp)+4
      track_data(mt(idttmp)  ,idttmp)='T'
      track_data(mt(idttmp)+1,idttmp)='e'
      track_data(mt(idttmp)+2,idttmp)='m'
      track_data(mt(idttmp)+3,idttmp)='p'
      track_data(mt(idttmp)+4,idttmp)='i'
      mt(idttmp)=mt(idttmp)+5
c
      nline=0
  1   continue
      text=''
      read(*,100,err=98,end=99,iostat=ios)textin
      nline=nline+1
      write(0,109)nline,textin
c
c      Parse the input string and remove spaces and bar signs
c
      maxtext=0
      do n=1,120
         if(textin(n).ne.' '.and.
     *      textin(n).ne.'|'
     *     )then
                maxtext=maxtext+1
                text(maxtext)=textin(n)
         endif
      enddo
c
      i=0
  2   continue
c
      i=i+1
      if(i.gt.maxtext)go to 1
      if(text(i).eq.''.or.
     *   text(i).eq.'!'
     *  )then
             go to 1
      endif
c
      if(text(i).eq.'X'
     *  )then
             cspan=text(i+1)//text(i+2)
             read(cspan,105,err=87)span
             expression=text(i+3)//text(i+4)//text(i+5)
             read(expression,107,err=90)ntarget
             i=i+5
c
             delta_time=track_time-volume_track_time
             if(delta_time.lt.0
     *         )then
                    go to 85
             endif
             span=span*interval
             intspan=span+1
             kx=(ntarget-nexp)/intspan
             nexp=kx+nexp
             intspan=intspan-1
             do nspan=1,span+1
                minit=mt(idtexp)
                call make_midi_time(mt(idtexp),idtexp) ! dt to event on voltrk
                volume_track_time=volume_track_time+(delta_time)
c                
                track_data(mt(idtexp)  ,idtexp)=char(ncon_on_chann)
                track_data(mt(idtexp)+1,idtexp)=char(11) ! Expression (coarse)
                track_data(mt(idtexp)+2,idtexp)=char(nexp)
                last_exp=nexp
                write(10,134)kx,nexp,minit,mt(idtexp)+2
     *                          ,(ichar(track_data(mx,idtexp))
     *                           ,mx=minit,mt(idtexp)+2)
                mt(idtexp)=mt(idtexp)+3
                if(intspan.lt.1
     *             )then
                        exit
                endif
                kx=(ntarget-nexp)/intspan
                nexp=kx+nexp
                intspan=intspan-1
                delta_time=24/interval
             enddo
             go to 2
c 
      endif
c      
      if(text(i).eq.'T'.or.
     *   text(i).eq.'t'
     *  )then
c
             if(text(i+1).eq.'X'.or.
     *          text(i+1).eq.'x'
     *         )then
                    ntext=0
                    do m=name_start,name_end
                       ntext=ntext+1
                       if(text(i+1+ntext).ne.'\'
     *                   )then
                              track_data(m,idt)=text(i+1+ntext)
                          else
                              exit                      ! \ is end of text
                       endif
                    enddo
                    i=i+1+len_nametxt
                    go to 2
             endif
c
             if(text(i+1).eq.'S'.or.
     *          text(i+1).eq.'s'
     *         )then
                    time_sig=text(i+2)//text(i+3)
                    read(time_sig,106)inum,iden
                    track_data(ntsnum,1)=char(inum)	! Numerator (default is 4)
                    iden=.5+(log(float(iden))/log(2.0))
                    track_data(ntsden,1)=char(iden)	! Denominator as power of 2 (2**2 = 4, so default is 4/4)
                    i=i+3
                    go to 2
             endif
c
             if(text(i+1).eq.'E'.or.
     *          text(i+1).eq.'e'
     *         )then
                    cspan=text(i+2)//text(i+3)
                    read(cspan,105,err=87)span
                    beats_per_minute=text(i+4)//text(i+5)//text(i+6)
                    read(beats_per_minute,107,err=90)bpm
                    ntarget=(6*(10**7))/bpm
                    i=i+6
c
c      Write the delta time to this event on track idttmp
c
                    delta_time=track_time-tempo_track_time
                    if(delta_time.lt.0
     *                )then
                           go to 86
                    endif
                    span=span*interval
                    intspan=span+1
                    kx=(ntarget-ntempo)/intspan
                    ntempo=kx+ntempo
                    write(*,*)ntempo,kx,ntarget
                    intspan=intspan-1
                    do nspan=1,span+1
c
                       call easy_midi_tempo
c
                       if(intspan.lt.1
     *                    )then
                               exit
                       endif
c
                       kx=(ntarget-ntempo)/intspan
                       ntempo=kx+ntempo
                       intspan=intspan-1
                       delta_time=24/interval
                    enddo
c
                    go to 2
             endif
c
             if(text(i+1).eq.'A'.or.
     *          text(i+1).eq.'a'
     *         )then
c
c      Track begins (but check one isn't already running....)
c
                    if(track_started
     *                )then
                           go to 93
                       else
                           track_started=.true.
                    endif
c
c
                    note_vel=64
                    note_vol=64
                    running=.false.
                    silent=.false.
                    track_time=0
                    length_time=0
                    ntracks=ntracks+1
                    idt=ntracks
c
c                   ncon_on_chann=(16*11)+idt ! Before we set track number..
c                   note_on_chann=(16*9)+idt
                     npatch_chann=(16*12)
                    ncon_on_chann=(16*11) ! Before we set track number..
                    note_on_chann=(16*9)
c
c      Set up the Sequence/Track name
c
                    len_nametxt=12
                    track_data(mt(idt)  ,idt)=char(z'00')! Delta-time zero
                    track_data(mt(idt)+1,idt)=char(z'FF')! Meta-event
                    track_data(mt(idt)+2,idt)=char(z'03')! 0x03 Sequence/Track name
                    track_data(mt(idt)+3,idt)=char(len_nametxt)	! len_nametxt bytes of text
                    name_start=mt(idt)+4
                    name_end  =mt(idt)+3+len_nametxt
                    do m=name_start,name_end
                       track_data(m,idt)=' '			! Initial fill
                    enddo
                    mt(idt)=mt(idt)+4+len_nametxt
c
                    track_data(mt(idt)  ,idt)=char(z'00')		! Delta-time zero
                    track_data(mt(idt)+1,idt)=char(ncon_on_chann)	! 0xB0 is controller (the 'B') for channel 0-F
                    track_data(mt(idt)+2,idt)=char(7)	! Volume controller (coarse)
                    track_data(mt(idt)+3,idt)=char(note_vol)
                    mt(idt)=mt(idt)+4
c
                    write(20+idt,123)mt(idt)
c
                    i=i+1
                    go to 2
             endif
c
             if(text(i+1).eq.'Z'.or.
     *          text(i+1).eq.'z'
     *         )then
c
c      Track ends (but check one is already running....)
c
                    if(.not.track_started
     *                )then
                           go to 92
                       else
                           track_started=.false.
                           i=i+1
                           go to 3
                    endif
c
             endif
      endif
c
      if(text(i).eq.'{'
     *  )then
c
c     This is the start of a chord
c
             if(chord
     *         )then    ! We are starting a chord when one is started
                    go to 88
                else
                    chord=.true.
             endif
             write(12,117)
             go to 2
      endif
c
      if(text(i).eq.'}'
     *  )then
c
c     This is the end of a chord
c
             if(chord
     *         )then        
                    chord=.false.
                else    ! We are ending a chord when none exists - error
                    go to 89
             endif
             write(12,118)
             go to 3
      endif
c
      if(text(i).eq.'S'.or.
     *   text(i).eq.'s'
     *  )then
c
c     Change the velocity of a note-on event
c
             read(text(i+1),106,err=91)note_vel
             note_vel=12.7*float(note_vel+1)
             i=i+1
             go to 2
      endif
c
      if(text(i).eq.'Q'
     *  )then
c
c     Change the duration of a note
c
             read(text(i+1),106,err=96)kont      
             i=i+1
             staccato=.true.
             go to 2
      endif
c
      if(text(i).eq.'q'
     *  )then
c
c     Restore the note duration
c
             kont=kont_def
             staccato=.false.
             go to 2
      endif
c
c      Handle length names and dots
c
      if(text(i).eq.'3'
     *   )then
c
c   These are triplets..
c
              length=length/3
              go to 2
      endif
c      
      if(ichar(text(i)).ge.105.and.
     *   ichar(text(i)).le.110
     *   )then
              length=3*(2**((ichar(text(i))-104)-1))	! 3 * 2**(N-1)
              if(text(i+1).eq.'.'
     *          )then
                     length=(length*3)/2
                     i=i+1
              endif
              go to 2
      endif
c
c      Alternative length definition..
c
      if(text(i).eq.'L'
     *  )then
c
c     Set an exact note duration
c
             write(duration,101)text(i+1)//text(i+2)//text(i+3)
             read(duration,107,err=97)length
             i=i+3
             go to 2
      endif
c
      if(text(i).eq.'>'
     *  )then
c
c     Emphasis for a single note or chord
c
             go to 2
      endif
c
      if(text(i).eq.'^'
     *  )then
c
c     A pause for npause
c
             delta_time=track_time-tempo_track_time
             if(delta_time.lt.0
     *         )then
                    go to 86
             endif
             mx=ntempo
             ntempo=(3*ntempo)/2
             call easy_midi_tempo
             delta_time=12
             ntempo=mx
             call easy_midi_tempo
             go to 2
      endif
c
      if(text(i).eq.'K'
     *  )then
c
c     A new key...
c
             read(text(i+1),106,err=95)nkey
             if(nkey.ne.0
     *         )then
                    if(text(i+2).ne.'#'.and.
     *                 text(i+2).ne.'_'
     *                )then
                           go to 95
                       else
                           if(text(i+2).eq.'_'
     *                       )then
                                  nkey=-nkey
                           endif
                     endif
                     track_data(ntkey,1)=char(nkey)
c                    track_data(28,1)=char(nkey)
                     key_init=.true.
             endif
             i=i+2
             go to 2
      endif
c
      if(text(i).eq.'U'.or.
     *   text(i).eq.'u'
     *  )then
c
c     Change the volume for following notes
c
             read(text(i+1),106,err=91)note_vol
             note_vol=12.7*float(note_vol+1)
             i=i+1
             if(.not.running
     *         )then
                    track_data(mt(idt)  ,idt)=char(z'00') ! Delta-time zero
                    mt(idt)=mt(idt)+1
             endif       
             track_data(mt(idt)  ,idt)=char(ncon_on_chann)! Controller
             track_data(mt(idt)+1,idt)=char(7)            ! Volume
             track_data(mt(idt)+2,idt)=char(note_vol)     ! Value
             mt(idt)=mt(idt)+3
             running=.false.
             go to 2
      endif
c
      if(text(i).eq.'P'
     *  )then
c
c     Change the instrument
c
             patch_id=text(i+1)//text(i+2)//text(i+3)
             read(patch_id,107,err=84)patch
             i=i+3
             if(.not.running
     *         )then
                    track_data(mt(idt)  ,idt)=char(z'00') ! Delta-time zero
                    mt(idt)=mt(idt)+1
             endif       
             track_data(mt(idt)  ,idt)=char(npatch_chann) ! Patch channel
             track_data(mt(idt)+1,idt)=char(patch)        ! Patch
             mt(idt)=mt(idt)+2
             running=.false.
             go to 2
      endif
c
      if(text(i).eq.'V'.or.
     *   text(i).eq.'v'
     *  )then
c
c     Change the octave
c
             read(text(i+1),106,err=94)noct
             i=i+1
             go to 2
      endif
c
      if(key_init
     *  )then
c
c      Set up the notes..
c
             idnote=-9
             do n=1,7
                idnote(n+96)=natural(n)
             enddo
             idnote(114)=0	! Rest
c
c      Set key signatures
c
c
c      Flats
c
             if(nkey.le.-1)idnote( 98)=idnote( 98)-1
             if(nkey.le.-2)idnote(101)=idnote(101)-1
             if(nkey.le.-3)idnote( 97)=idnote( 97)-1
             if(nkey.le.-4)idnote(100)=idnote(100)-1
             if(nkey.le.-5)idnote(103)=idnote(103)-1
             if(nkey.le.-6)idnote( 99)=idnote( 99)-1
c
c      Sharps
c
             if(nkey.ge.1)idnote(102)=idnote(102)+1
             if(nkey.ge.2)idnote( 99)=idnote( 99)+1
             if(nkey.ge.3)idnote(103)=idnote(103)+1
             if(nkey.ge.4)idnote(100)=idnote(100)+1
             if(nkey.ge.5)idnote( 97)=idnote( 97)+1
c
             key_init=.false.
c
             write(8,124)(mx,idnote(mx),mx=0,255)
      endif
c
c      What's left must be a a note or a rest...
c
      mtext=ichar(text(i))
c     write(8,*)text(i),mtext,idnote(mtext)
      if(mtext.le.71.and.
     *   mtext.ge.65
     *  )then
c
c      Slide up the ASCII sequence to lower case
c
             mtext=mtext+32
             noct_shift=12
         else
             noct_shift=0
      endif
c
      mnote=idnote(mtext)
      if(mnote.lt.-1
     *  )then
             write(0,208)mnote,ichar(text(i)),(text(mx),mx=1,i)
             stop
      endif
c
c      sharpening, flattening, naturalising
c
      if(text(i+1).eq.'#'
     *  )then
c
c     Sharpen it...
c
             mnote=mnote+1
             jaccidental=1
      endif
c
      if(text(i+1).eq.'_'
     *  )then
c
c     Flatten it...
c
             mnote=mnote-1
             jaccidental=1
      endif
c
      if(text(i+1).eq.'='
     *  )then
c
c     Naturalise it...
c
             mnote=natural(mtext-96)
             jaccidental=1
      endif
      i=i+jaccidental
      jaccidental=0
      mnote=mnote+(noct*12)+noct_shift
c
      length_time=length_time+length
c
c      Start and stop the note
c
      if(text(i).eq.'r'
     *  )then
c
c      This is a rest
c
             mnote=0   ! Hope we never need this note...
             nv=0
         else
             nv=note_vel
      endif
c
c
c      Adjust for octave and set note timings
c
      if(mnote.eq.0
     *  )then
             note_off_time=0
             note_on_time =length
         else
             note_off_time=.5+(.1*float(9-kont)*length)
             note_on_time =length-note_off_time
      endif
c      
      if(silent
     *  )then
             track_data(mt(idt)  ,idt)=char(ncon_on_chann)! Controller
             track_data(mt(idt)+1,idt)=char(7)            ! Volume
             track_data(mt(idt)+2,idt)=char(note_vol)     ! Value
             write(2,128)
             silent=.false.
             running=.false.
             mt(idt)=mt(idt)+3
      endif
c      
      if(.not.running
     *  )then
c      
c      Initialise running mode for this (music) track
c
             track_data(mt(idt)  ,idt)=char(0)! Delta time
             track_data(mt(idt)+1,idt)=char(note_on_chann)! Note ON chann
             write(12,135) idt,ichar(track_data(mt(idt)  ,idt)),mt(idt)
             write(12,1351)idt,ichar(track_data(mt(idt)+1,idt))
     *                                         ,mt(idt)+1
             mt(idt)=mt(idt)+2
             running=.true.
      endif       
c
c      Record the note, or rest, on the stack
c
      nstack=nstack+1
      note_stack(nstack)=mnote
       len_stack(nstack)=length
       on_stack(nstack)=note_on_time
      write(12,1151)mnote,running,on_stack(nstack)
     *                          ,len_stack(nstack),nstack,chord
c
c      Turn on the note 
c      (note name and then velocity, then the delta time for next note)
c
      if(mnote.gt.0
     *  )then
             track_data(mt(idt)  ,idt)=char(mnote)  ! This note
             write(12,1352)idt,ichar(track_data(mt(idt)  ,idt))
     *                                         ,mt(idt)  
             track_data(mt(idt)+1,idt)=char(nv)     ! At this velocity
             write(12,1353)idt,ichar(track_data(mt(idt)+1,idt))
     *                                         ,mt(idt)+1
             track_data(mt(idt)+2,idt)=char(z'00')  ! Delta-time 0
             write(12,135) idt,ichar(track_data(mt(idt)+2,idt))
     *                                         ,mt(idt)+2
             ldt=0
             ldt_byte=mt(idt)+2
             mt(idt)=mt(idt)+3
      endif
c
      get_next_note=.true.
c
  3   continue
      if(.not.chord.and.
     *   nstack.gt.0
     *   )then
c
c      FInd out when something has to happen...
c
             max_note=0
             min_length=1000000
             min_on    =1000000
             do istack=1,nstack
c
                if(len_stack(istack).lt.min_length
     *            )then
                       min_length=len_stack(istack)
                       note_min_len=istack
                endif
c
                if( on_stack(istack).lt.min_on.and.
     *              on_stack(istack).gt.0           
     *            )then
                       min_on=on_stack(istack)
                       note_min_on=istack
                endif
c
             enddo
             len_on=min_length-min_on
c
c
c      ...and write the 'delta-time to the next event'
c
             if(min_length.lt.min_on
     *         )then
                    delta_time=min_length
                else
                    delta_time=min_on
             endif
             write(12,131)min_on,note_min_on,min_length,note_min_len
c             
             track_time=track_time+delta_time
             ldt_byte=mt(idt)-1
             ldt=delta_time
             call make_midi_time(ldt_byte,idt)  ! Translation for long delta_times
             write(12, 116)ichar(track_data(ldt_byte,idt)),ldt_byte
             write(2,1222)track_data(ldt_byte,idt),ldt_byte
c
             nstack_new=0
             do istack=1,nstack
                len_stack(istack)=len_stack(istack)-delta_time
                 on_stack(istack)= on_stack(istack)-delta_time
                if(len_stack(istack).eq.0.or.
     *              on_stack(istack).eq.0           
     *            )then
                       write(12,115)note_stack(istack)
c
c      Stop the note...
c
                       track_data(mt(idt) ,idt)
     *                                  =char(note_stack(istack))
                       write(12,1352)idt
     *          ,ichar(track_data(mt(idt)  ,idt)),mt(idt)  
                       track_data(mt(idt)+1,idt)=char(0)! At this velocity
                       write(12,1353)idt
     *          ,ichar(track_data(mt(idt)+1,idt)),mt(idt)+1
                       track_data(mt(idt)+2,idt)=char(0)! Delta time
                       write(12,135)idt
     *          ,ichar(track_data(mt(idt)+2,idt)),mt(idt)+2
                       mt(idt)=mt(idt)+3
                endif
             enddo
c
             do istack=1,nstack
                if(len_stack(istack).eq.0
     *            )then
                       get_next_note=.true.
                   else
                       get_next_note=.false.
                       nstack_new=nstack_new+1
                        len_stack(nstack_new)= len_stack(istack)
                         on_stack(nstack_new)=  on_stack(istack)
                       note_stack(nstack_new)=note_stack(istack)
                endif
             enddo
             nstack=nstack_new
             write(12,120)nstack
c
      endif
                    if(get_next_note
     *                )then
                           go to 2
                       else
                           go to 3
                    endif
c      
      go to 2
      stop
c
 84   continue
      write(0,*)patch_id
      write(0,*)nline,text
      write(0,*)nline,(text(mx),mx=1,i)
      write(0,219)
      stop
c
 85   continue
      write(0,218)
      write(0,133)volume_track_time,idt,track_time
     *                   ,delta_time
      write(0,*)nline,text
      write(0,*)nline,(text(mx),mx=1,i)
      stop
c
 86   continue
      write(0,217)
      write(0,133)tempo_track_time,idt,track_time
     *                   ,delta_time
      write(0,*)nline,text
      write(0,*)nline,(text(mx),mx=1,i)
      stop
c
 87   continue
      write(0,*)nline,text
      write(0,*)nline,(text(mx),mx=1,i+3)
      write(0,216)
      stop
c
 88   continue
      write(0,*)nline,text
      write(0,*)nline,(text(mx),mx=1,i)
      write(0,214)
      stop
c
 89   continue
      write(0,*)nline,text
      write(0,*)nline,(text(mx),mx=1,i)
      write(0,215)
      stop
c
 90   continue
      write(0,*)nline,text
      write(0,*)nline,(text(mx),mx=1,i)
      write(0,205)
      stop
c
 91   continue
      write(0,*)nline,text
      write(0,*)nline,(text(mx),mx=1,i)
      write(0,206)
      stop
c
 92   continue
      write(0,*)nline,text
      write(0,*)nline,(text(mx),mx=1,i)
      write(0,207)
      stop
c
 93   continue
      write(0,*)nline,text
      write(0,*)nline,(text(mx),mx=1,i)
      write(0,209)
      stop
c
 94   continue
      write(0,*)nline,text
      write(0,*)nline,(text(mx),mx=1,i)
      write(0,210)
      stop
c
 95   continue
      write(0,*)nline,text
      write(0,*)nline,(text(mx),mx=1,i)
      write(0,211)
      stop
c
 96   continue
      write(0,*)nline,text
      write(0,*)nline,(text(mx),mx=1,i)
      write(0,212)
      stop
c
 97   continue
      write(0,*)nline,text
      write(0,*)nline,(text(mx),mx=1,i)
      write(0,213)
      stop
c
 98   continue
      write(0,*)nline,text
      write(0,*)nline,(text(mx),mx=1,i)
      write(0,203)
      stop
c
 99   continue
c
      mt(1)=mt(1)+1
      mt(idtexp)=mt(idtexp)+1
      mt(idttmp)=mt(idttmp)+1
      midi_data(12)=char(ntracks)	! Number of tracks in file
c
c      Catenate all MIDI header and all the tracks
c
      do idt=1,ntracks
c
c      Tack on the end-of-track event
c
         track_data(mt(idt)-1,idt)=char(z'00')
         track_data(mt(idt)  ,idt)=char(z'FF')
         track_data(mt(idt)+1,idt)=char(z'2F')	! End track event
         track_data(mt(idt)+2,idt)=char(z'00')
         mt(idt)=mt(idt)+2                      ! Exact track length
c
c      Compute and record track length
c
         lentrk=mt(idt)-8
         lentrka=lentrk
         k=256*256*256
         do i=5,8
            m=lentrk/k
            track_data(i,idt)=char(m)
            lentrk=lentrk-(m*k)
            k=k/256
         enddo
         write(20+idt,102)idt
         write(20+idt,113)(track_data(mx,idt),mx=1,mt(idt))
         write( 0,112)idt,lentrka,(ichar(track_data(mx,idt)),mx=5,8)
     *              ,length_time,length_time/24
         write(10,112)idt,lentrka,(ichar(track_data(mx,idt)),mx=5,8)
     *              ,length_time,length_time/24
         write(10,113)(track_data(mx,idt),mx=1,mt(idt))
c
c      Add this track to the MIDI data
c
         do nb=1,mt(idt)
            md_count=md_count+1
            midi_data(md_count)=track_data(nb,idt)
         enddo
      enddo
      write(8,110)md_count
c
      open(12,file='x.mid',form='formatted',status='unknown')
      do i=1,md_count
         write(12,111)midi_data(i)
c
         if(ichar(midi_data(i+1)).ge.240.and.
     *      ichar(midi_data(i  )).le.127
     *     )then
c
c      A MIDI Event, preceded by a delta time, is about to happen...
c
                write(4,102)
                write(10,102)
         endif
c
         if(ichar(midi_data(i)).gt.31
     *     )then
                write(4,122) midi_data(i),midi_data(i),i,i-95,i
            else
                write(4,1221)             midi_data(i),i,i-95,i
         endif
c
      enddo
      close(12)
c
      stop
c
100   format(120a1)
101   format(a3)
102   format(/,i4)
1023  format('Note is ',a2,i4,' in base octave',i3
     *      ,'.  ON/OFF is',2i4,', volume is',i4
     *      ,', style is',i3,' in key',i3,6z3)
103   format(a1,i2)      
104   format(a3)
105   format(i2)
106   format(2i1)
107   format(i3)
108   format(6z3)
109   format(i4,' ',120a1)
110   format('Length of all MIDI data is',i8,' characters')
111   format(a1,$)
112   format(/'Length of midi track',i3,' is'
     *      ,i8,' characters',4(' ',z2)
     *      ,'.   Ticks:',i8,', crotchets:',i8)
113   format(40z3.2)
115   format('Note',i4,' turned off')
1151  format('Note',i4,' turned on. Running:',l2,', ON time',i3
     *      ,', full length',i3,': Stack',i3,', Chord:',l2)
116   format(22x,'Delta_time',i10,' Byte:',i6)
117   format(20x,'Chord starts')
118   format(20x,'Chord ends')
119   format(20x,'Stack height is now',i3)
120   format('Stack reset to',i3)
121   format(20x,'Terminating chord at end of track')
122   format(a1,z3.2,2i6,z4.2)
1221  format(z4.2,2i6,z4.2)
1222  format(20x,'Delta time:',z4.2,2i4)
123   format('Music track starts at byte:',i6)
124   format(2i6)
125   format(10i8)
127   format(20x,'Volume shut down')
128   format(20x,'Volume restored')
129   format(i32,' Note',i4,': ON time',i4,', full length',i4
     *          ,' Chord',l2)
130   format(i32,' Note',i4,': ON time',i4,', full length',i4,' -amend')
131   format('Stack scan:',10x,' Min ON time',i9,' at',i4
     *                        ,', Min length',i4,' at',i4)
132   format('     Tempo:',2i8,2i6,4x,10z3.2)                       
133   format('Trackx time',i6,', Track',i2.2,' time:',i6
     *      ,', delta_time',i6,z7,/)
134   format('Expression:',2i8,2i6,4x,12z3.2)                       
135   format(32x,'Delta time    on track',i2,':',z4.2,' at byte',i4)
1351  format(32x,'Note_ON event on track',i2,':',i4,' at byte',i4)
1352  format(32x,'Note          on track',i2,':',i4,' at byte',i4)
1353  format(32x,'Velocity      on track',i2,':',i4,' at byte',i4)
c
203   format('***** ERROR ON INPUT')
205   format('***** ERROR: Bad tempo')
206   format('***** ERROR: Bad volume')
207   format('***** ERROR: Cannot end Track - none started')
208   format('***** ERROR: ',i4,' from idnote(',i4,') is not a note '
     *      ,120a1)
209   format('***** ERROR: Track already started')
210   format('***** ERROR: Bad Octave')
211   format('***** ERROR: Bad Key')
212   format('***** ERROR: Bad Qontinuity')
213   format('***** ERROR: Bad Length')
214   format('***** ERROR: Chord already started')
215   format('***** ERROR: Trying to end chord but none started')
216   format('***** ERROR: Bad value for span')
217   format('***** ERROR: Tempo change is still in process'
     *      ,' at this time')
218   format('***** ERROR: Volume change is still in process'
     *      ,' at this time')
219   format('***** ERROR: Bad Patch read')
c
      end
c
c            if(kont.lt.9.and.max_note.ne.200
c    *         )then
c
c      Shortened (staccato) note...
c
c                   note_off_time=.1*float(9-kont)*delta_time
c                   delta_time=delta_time-note_off_time
c            endif
c
c            if(kont.lt.9.and..not.silent
c    *         )then             
c
c      After this time reduce the volume to appropriate 'staccato' level
c
c                   track_data(mt(idt)  ,idt)=char(ncon_on_chann)! Controller
c                   track_data(mt(idt)+1,idt)=char(7)            ! Volume...
c                   track_data(mt(idt)+2,idt)=char(0)            ! ..of zero
c                   write(2,127)
c                   mt(idt)=mt(idt)+3
c                   silent=.true.
c
c      Leave the volume down and set note on after running delta
c
c                   track_data(mt(idt),  idt)=char(note_off_time)
c                   track_data(mt(idt)+1,idt)=char(note_on_chann)
c                   mt(idt)=mt(idt)+2
c                    
c            endif
c                
c               track_data(mt(1)  ,1)=char(z'F0')! SysEx event
c               track_data(mt(1)+1,1)=char(z'07')! Seven bytes after this
c               track_data(mt(1)+2,1)=char(z'7F')! RealTime
c               track_data(mt(1)+3,1)=char(z'7F')! SysEx channel(ignore chann)
c               track_data(mt(1)+4,1)=char(z'04')! Sub-ID Device Control
c               track_data(mt(1)+5,1)=char(z'01')! Sub-ID2 Master Volume
c               track_data(mt(1)+6,1)=char(nexp) ! Bits 0 to  6 of 14-bit volume
c               track_data(mt(1)+7,1)=char(nexp) ! Bits 7 to 13 of 14-bit volume
c               track_data(mt(1)+8,1)=char(z'F7')! End of SysEx
