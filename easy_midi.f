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
      emphasis=.false.
      legato=.false.
      mrest=0
      check=.false.
      idtvol=2
      idttmp=3
      ntracks=idttmp ! The first score track will be idttmp+1
      interval=6
      kont_def=8	! Staccato 0; Legato 9
      ntempo=500000
      jaccidental=0
      nstack=0
      chord=.false.
      running=.false.
      key_init=.true.
      noct=5		! octaVe
      length=24 	! Crotchet
      nkey=0		! C major
      kont=kont_def
      note_vel_def=80 	! Note velocity (S/s=5)
      note_vol_def=80 	! Note volume (U/u=5)
      key_shift=0
      note_shift=0  ! This and line above assume no transposition
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
      idnote=-9
      do n=1,7
         idnote(n+96)=natural(n)
      enddo
      idnote(114)=0	! Rest
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
      midi_data( 5)=char(int(z'00'))
      midi_data( 6)=char(int(z'00'))
      midi_data( 7)=char(int(z'00'))
      midi_data( 8)=char(int(z'06'))
      midi_data( 9)=char(int(z'00'))
      midi_data(10)=char(int(z'01'))
      midi_data(11)=char(int(z'00'))
c     midi_data(12)=char(int(z'01'))	! Number of tracks in file - set later
      midi_data(13)=char(int(z'00')) ! Pulses, or clocks, per quarter note
      midi_data(14)=char(int(z'18'))	! Pulses, or clocks, per quarter note
      md_count=14
c
c      Definition track
c
      track_data(mt(1)  ,1)=char(int(z'00'))		! Delta-time zero
      track_data(mt(1)+1,1)=char(int(z'FF'))		! Meta-event
      track_data(mt(1)+2,1)=char(int(z'01'))		! 0x01 Text
      track_data(mt(1)+3,1)=char(34)
c
      track_data(mt(1)+ 4,1)='E'			! Initial fill
      track_data(mt(1)+ 5,1)='a'			! Initial fill
      track_data(mt(1)+ 6,1)='s'			! Initial fill
      track_data(mt(1)+ 7,1)='y'			! Initial fill
      track_data(mt(1)+ 8,1)='M'			! Initial fill
      track_data(mt(1)+ 9,1)='I'			! Initial fill
      track_data(mt(1)+10,1)='D'			! Initial fill
      track_data(mt(1)+11,1)='I'			! Initial fill
      track_data(mt(1)+12,1)=' '			! Initial fill
      track_data(mt(1)+13,1)='C'			! Initial fill
      track_data(mt(1)+14,1)='o'			! Initial fill
      track_data(mt(1)+15,1)='p'			! Initial fill
      track_data(mt(1)+16,1)='y'			! Initial fill
      track_data(mt(1)+17,1)='r'			! Initial fill
      track_data(mt(1)+18,1)='i'			! Initial fill
      track_data(mt(1)+19,1)='g'			! Initial fill
      track_data(mt(1)+20,1)='h'			! Initial fill
      track_data(mt(1)+21,1)='t'			! Initial fill
      track_data(mt(1)+22,1)=' '			! Initial fill
      track_data(mt(1)+23,1)='J'			! Initial fill
      track_data(mt(1)+24,1)='o'			! Initial fill
      track_data(mt(1)+25,1)='h'			! Initial fill
      track_data(mt(1)+26,1)='n'			! Initial fill
      track_data(mt(1)+27,1)=' '			! Initial fill
      track_data(mt(1)+28,1)='W'			! Initial fill
      track_data(mt(1)+29,1)='a'			! Initial fill
      track_data(mt(1)+30,1)='t'			! Initial fill
      track_data(mt(1)+31,1)='t'			! Initial fill
      track_data(mt(1)+32,1)='s'			! Initial fill
      track_data(mt(1)+33,1)=' '			! Initial fill
      track_data(mt(1)+34,1)='2'			! Initial fill
      track_data(mt(1)+35,1)='0'			! Initial fill
      track_data(mt(1)+36,1)='0'			! Initial fill
      track_data(mt(1)+37,1)='8'			! Initial fill
c
      mt(1)=mt(1)+38
c
c      General track
c
      track_data(mt(1)  ,1)=char(int(z'00'))! Delta-time zero
      track_data(mt(1)+1,1)=char(int(z'F0'))! SysEx
      track_data(mt(1)+2,1)=char(int(z'05'))! Five bytes after this
      track_data(mt(1)+3,1)=char(int(z'7E'))! Non-realtime
      track_data(mt(1)+4,1)=char(int(z'7F'))! SysEx channel "disregard channel"
      track_data(mt(1)+5,1)=char(int(z'09'))! GM system
      track_data(mt(1)+6,1)=char(int(z'01'))! GM enable - 00 OFF, 01 ON, 02 ??
      track_data(mt(1)+7,1)=char(int(z'F7'))! End of SysEx
      mt(1)=mt(1)+8
c
      track_data(mt(1)  ,1)=char(int(z'00'))! Delta-time zero
      track_data(mt(1)+1,1)=char(int(z'FF'))! Meta-event
      track_data(mt(1)+2,1)=char(int(z'03'))! 0x03 Sequence/Track name
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
      track_data(mt(1)  ,1)=char(int(z'00'))		! Delta-time zero
      track_data(mt(1)+1,1)=char(int(z'FF'))		! Meta-event
      track_data(mt(1)+2,1)=char(int(z'02'))		! 0x02 Copyright
      track_data(mt(1)+3,1)=char(len_copytxt)	! len_copytxt bytes of text
      ncopy_start=mt(1)+4
      ncopy_end  =mt(1)+3+len_copytxt
      do m=ncopy_start,ncopy_end
         track_data(m,1)='C'			! Initial fill
      enddo
      mt(1)=mt(1)+4+len_copytxt
c
      track_started=.false.
c
c      Expression track
c
      track_data(mt(idtvol)  ,idtvol)=char(int(z'00'))! Delta-time zero
      track_data(mt(idtvol)+1,idtvol)=char(int(z'FF'))! Meta-event
      track_data(mt(idtvol)+2,idtvol)=char(int(z'03'))! 0x03 Sequence/Track name
      track_data(mt(idtvol)+3,idtvol)=char(10)   ! Length of following text
      mt(idtvol)=mt(idtvol)+4
      track_data(mt(idtvol)  ,idtvol)='E'
      track_data(mt(idtvol)+1,idtvol)='x'
      track_data(mt(idtvol)+2,idtvol)='p'
      track_data(mt(idtvol)+3,idtvol)='r'
      track_data(mt(idtvol)+4,idtvol)='e'
      track_data(mt(idtvol)+5,idtvol)='s'
      track_data(mt(idtvol)+6,idtvol)='s'
      track_data(mt(idtvol)+7,idtvol)='i'
      track_data(mt(idtvol)+8,idtvol)='o'
      track_data(mt(idtvol)+9,idtvol)='n'
      mt(idtvol)=mt(idtvol)+10
c
c      Set default overall volume on expression track
c
      delta_time=0
      note_vol=note_vol_def
      call easy_midi_xvolume
c
c      Tempo track
c
      track_data(mt(idttmp)  ,idttmp)=char(int(z'00'))! Delta-time zero
      track_data(mt(idttmp)+1,idttmp)=char(int(z'FF'))! Meta-event
      track_data(mt(idttmp)+2,idttmp)=char(int(z'03'))! 0x03 Sequence/Track name
      track_data(mt(idttmp)+3,idttmp)=char(11)   ! Length of following text
      mt(idttmp)=mt(idttmp)+4
      track_data(mt(idttmp)   ,idttmp)='M'
      track_data(mt(idttmp)+ 1,idttmp)='e'
      track_data(mt(idttmp)+ 2,idttmp)='t'
      track_data(mt(idttmp)+ 3,idttmp)='a'
      track_data(mt(idttmp)+ 4,idttmp)='-'
      track_data(mt(idttmp)+ 5,idttmp)='e'
      track_data(mt(idttmp)+ 6,idttmp)='v'
      track_data(mt(idttmp)+ 7,idttmp)='e'
      track_data(mt(idttmp)+ 8,idttmp)='n'
      track_data(mt(idttmp)+ 9,idttmp)='t'
      track_data(mt(idttmp)+10,idttmp)='s'
      mt(idttmp)=mt(idttmp)+11
c
      nline=0
      lentext=0
c
c      Read in the whole text, the complete piece
c
      text=''
  1   continue
      read(3,100,err=98,end=8,iostat=ios)textin
      nline=nline+1
      write(0,109)nline,ios,textin
c
c      Parse the input string and remove spaces and bar signs
c
      nx=0
      do n=1,120
         nx=nx+1
         if(nx.gt.120)exit   ! Don't jump too far ahead...
         if(textin(nx).eq.'!'
     *     )then
                exit
         endif
c
         if(textin(nx).eq.'J'     ! Convert 'Jab' to 'k.ajb'
     *     )then
                lentext=lentext+1
                text(lentext)='k'
c
                lentext=lentext+1
                text(lentext)='.'
c
                nx=nx+1
                lentext=lentext+1
                text(lentext)=textin(nx)     ! 'a'
c
                lentext=lentext+1
                text(lentext)='j'
c
                nx=nx+1
                lentext=lentext+1
                text(lentext)=textin(nx)     ! 'b'
c
                nx=nx+1
         endif
c
         if(textin(nx).ne.' '.and.
     *      textin(nx).ne.'|'
     *     )then
                lentext=lentext+1
                text(lentext)=textin(nx)
         endif
c         
      enddo
c
      go to 1
c
c      Whole text has been read - start to produce MIDI file
c
  8   continue
      write(*,137)lentext
      write(*,100)(text(mx),mx=1,lentext)
c
      i=0
  2   continue
c
      i=i+1
      if(i.gt.lentext
     *  )then
             go to 99
      endif
c
      if(text(i).eq.'H'   ! NB: This should PRECEDE any 'K' for correct
     *  )then             !     key signature
c
c     This is a request to sHift key, ie to transpose
c
             call easy_midi_numerics(i+2,i+2,note_shift)
             if(note_shift.eq.0)go to 2 ! No change...
             if(text(i+1).eq.'-'    ! Anything else is treated as '+'
     *         )then
c
c      These are downward shifts
c
                    note_shift=-note_shift ! Notes
                    key_shift=-key_shift ! Sharps or flats
             endif
             key_shift=(note_shift*7)
             key_shift=mod(key_shift,12)
             if(iabs(key_shift).gt.0)write(*,114)note_shift
             i=i+2
             go to 2
      endif
c
      if(text(i).eq.':'   ! Start of repeat section
     *  )then
             call easy_midi_numerics(i+1,i+1,maxrep)
             if(maxrep.eq.0   ! ie, choose number of verses...
     *         )then
                    write(*,138)
                    read (*,*)maxrep
             endif
             i=i+1
             init_rep=i
             nrep=0
             go to 2
      endif
c
      if(text(i).eq.'&'   ! End of repeat section
     *  )then
             iend_rep=i
             nrep=nrep+1
             if(nrep.lt.maxrep
     *         )then
                    i=init_rep
             endif
             go to 2
      endif
c
      if(text(i).eq.'%'   ! Go to the coda
     *  )then
             if(nrep.eq.maxrep-1
     *         )then
                    i=iend_rep
             endif
             go to 2
      endif
c
      if(text(i).eq.'X'
     *  )then
             call easy_midi_numerics(i+1,i+2,span)
             call easy_midi_numerics(i+3,i+5,ntarget)
             i=i+5
c
             delta_time=track_time(idt)-track_time(idtvol)
             if(delta_time.lt.0
     *         )then
                    go to 85
             endif
             span=span*interval
             intspan=span+1
             kx=(ntarget-note_vol)/intspan
             note_vol=kx+note_vol
             intspan=intspan-1
             do nspan=1,span+1
                call easy_midi_xvolume
                if(intspan.lt.1
     *             )then
                        exit
                endif
                kx=(ntarget-note_vol)/intspan
                note_vol=kx+note_vol
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
c                   i=i+1+len_nametxt
                    i=i+1+ntext
                    go to 2
             endif
c
             if(text(i+1).eq.'S'.or.
     *          text(i+1).eq.'s'
     *         )then  
                    call easy_midi_numerics(i+2,i+2,inum)
                    call easy_midi_numerics(i+3,i+3,iden)
                    i=i+3
c
c      Write the delta time to this event on track idttmp
c
                    if(idt.le.maxtracks
     *                )then
                           delta_time=track_time(idt)-track_time(idttmp)
                       else
                           delta_time=0
                    endif
c
                    if(delta_time.lt.0
     *                )then
                           go to 86
                    endif
                    call easy_midi_timesig(inum,iden)
                    go to 2
             endif
c
             if(text(i+1).eq.'E'.or.
     *          text(i+1).eq.'e'
     *         )then
                    call easy_midi_numerics(i+2,i+3,span)
                    call easy_midi_numerics(i+4,i+6,bpm)
                    i=i+6
                    ntarget=(6*(10**7))/bpm
c
c      Write the delta time to this event on track idttmp
c
                    write(*,*)idt, idttmp
                    if(idt.le.maxtracks
     *                )then
                           delta_time=track_time(idt)-track_time(idttmp)
                       else
                           delta_time=0
                    endif
c
                    if(delta_time.lt.0
     *                )then
                           go to 86
                    endif
                    span=span*interval
                    intspan=span+1
                    kx=(ntarget-ntempo)/intspan
                    ntempo=kx+ntempo
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
                    ntracks=ntracks+1
                    idt=ntracks
                    note_vel=note_vel_def
                    note_vol=note_vol_def
                    running=.false.
                    track_time(idt)=0
                    length_time=0
c
c      First music track is idt=4
c                    
                     npatch_chann=(16*12)+idt-4    ! 0xC0 + this_track 
                    ncon_on_chann=(16*11)+idt-4    ! 0xB0 + this track
                    note_on_chann=(16*9) +idt-4    ! 0x90 + this track
c
c      Set up the Sequence/Track name
c
                    len_nametxt=20    !   Was 12 17AUG15
                    track_data(mt(idt)  ,idt)=char(int(z'00'))! Delta-time zero
                    track_data(mt(idt)+1,idt)=char(int(z'FF'))! Meta-event
                    track_data(mt(idt)+2,idt)=char(int(z'03'))! 0x03 Sequence/Track name
                    track_data(mt(idt)+3,idt)=char(len_nametxt)	! len_nametxt bytes of text
                    name_start=mt(idt)+4
                    name_end  =mt(idt)+3+len_nametxt
                    do m=name_start,name_end
                       track_data(m,idt)=' '			! Initial fill
                    enddo
                    mt(idt)=mt(idt)+4+len_nametxt
c
c      Set the default track volume
c
                    call easy_midi_tvolume
c
                    if(check
     *                )then
                           write(20+idt,123)mt(idt)
                    endif
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
      if(text(i).eq.'$'
     *  )then
c
c     Turn on/off checking and logs
c
              check=.not.check
              go to 2
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
             if(check
     *         )then
                    write(12,117)
             endif
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
             if(check
     *         )then
                    write(12,118)
             endif
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
             if(ichar(text(i+1)).lt.48.or.
     *          ichar(text(i+1)).gt.57
     *         )then
c
c      Full legato
c
                    kont=9
                    legato=.true.
                else
c
c      Read the value of kont
c
                    read(text(i+1),106,err=96)kont      
                    i=i+1
             endif
             go to 2
      endif
c
      if(text(i).eq.','
     *  )then
             legato=.not.legato
             if(legato
     *         )then
                    kont=9
                else
                    kont=kont_def
             endif
             go to 2
      endif
c
      if(text(i).eq.'q'
     *  )then
c
c     Restore the note duration
c
             kont=kont_def
             legato=.false.
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
      if(ichar(text(i)).ge.105.and.     ! 105 is 'i'
     *   ichar(text(i)).le.111          ! 111 is 'o'
     *   )then
              length=3*(2**((ichar(text(i))-104)-1))	! 3 * 2**(N-1)
              if(text(i+1).eq.'.'
     *          )then
                     length=(length*3)/2
                     if(check)write(*,140)text(i),text(i+1),length
                     i=i+1
                 else
                     if(check)write(*,141)text(i),length
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
             call easy_midi_numerics(i+1,i+3,length)
             i=i+3
             go to 2
      endif
c
      if(text(i).eq.'>'
     *  )then
c
c     Emphasis for a single note or chord
c
c
             delta_time=track_time(idt)-track_time(idtvol)
             if(delta_time.lt.0
     *         )then
                    go to 85
             endif
             note_vol_old=note_vol
             note_vol=127 ! Max loudness
             call easy_midi_tvolume
             emphasis=.true.
             go to 2
      endif
c
      if(text(i).eq.'K'
     *  )then
c
c     A new key...
c
             i=i+1
             call easy_midi_numerics(i,i,nkey)
             if(nkey.eq.0
     *         )then
                    newkey=nkey
                else
                    i=i+1
                    if(text(i).ne.'#'.and.
     *                 text(i).ne.'_'
     *                )then
                           go to 95  !  A bad key
                       else
                           if(text(i).eq.'_'
     *                       )then
                                  nkey=-nkey
                           endif
                           newkey=nkey+key_shift  ! Any transposition?
                           if(newkey.gt. 6)newkey=12-newkey
                           if(newkey.lt.-6)newkey=12+newkey
c
                           if(nkey.eq. 0)from_desc='sharps or flats'
                           if(nkey.eq. 1)from_desc='sharp'
                           if(nkey.gt. 1)from_desc='sharps'
                           if(nkey.eq.-1)from_desc='flat'
                           if(nkey.lt.-1)from_desc='flats'
c
                           if(newkey.eq. 0)to_desc='sharps or flats'
                           if(newkey.eq. 1)to_desc='sharp'
                           if(newkey.gt. 1)to_desc='sharps'
                           if(newkey.eq.-1)to_desc='flat'
                           if(newkey.lt.-1)to_desc='flats'
c
                           if(key_shift.ne.0
     *                       )then
                                    mkey=iabs(nkey)
                                  mewkey=iabs(newkey)
                                  write(*,116)mkey,trim(from_desc)
     *                                     ,mewkey,trim(to_desc)
                           endif
                    endif
             endif
c
c      Write the delta time to this event on track idttmp
c
             if(idt.le.maxtracks
     *         )then
                    delta_time=track_time(idt)-track_time(1)
                else
                    delta_time=0
             endif

             if(delta_time.lt.0
     *         )then
                    go to 86
             endif
             call easy_midi_key(newkey,0) ! Set key signature
c             track_data(ntkey,1)=char(nkey)
c      Set up the notes..
c
c            idnote( 97) ! A
c            idnote( 98) ! B
c            idnote( 99) ! C
c            idnote(100) ! D
c            idnote(101) ! E
c            idnote(102) ! F
c            idnote(103) ! G
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
             if(nkey.le.-1)idnote( 98)=idnote( 98)-1 ! B
             if(nkey.le.-2)idnote(101)=idnote(101)-1 ! E
             if(nkey.le.-3)idnote( 97)=idnote( 97)-1 ! A
             if(nkey.le.-4)idnote(100)=idnote(100)-1 ! D
             if(nkey.le.-5)idnote(103)=idnote(103)-1 ! G
             if(nkey.le.-6)idnote( 99)=idnote( 99)-1 ! C
             if(nkey.le.-7)idnote(102)=idnote(102)-1 ! F
c
c      Sharps
c
             if(nkey.ge. 1)idnote(102)=idnote(102)+1 ! F
             if(nkey.ge. 2)idnote( 99)=idnote( 99)+1 ! C
             if(nkey.ge. 3)idnote(103)=idnote(103)+1 ! G
             if(nkey.ge. 4)idnote(100)=idnote(100)+1 ! D
             if(nkey.ge. 5)idnote( 97)=idnote( 97)+1 ! A
             if(nkey.ge. 6)idnote(101)=idnote(101)+1 ! E
             if(nkey.ge. 7)idnote( 98)=idnote( 98)+1 ! B
c
             if(check
     *         )then
                    write(8,124)(mx,idnote(mx),mx=0,255)
             endif
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
             call easy_midi_tvolume
             go to 2
      endif
c
      if(text(i).eq.'M'
     *  )then
c
c     Change the position in the sound field - 'pan'
c
             call easy_midi_numerics(i+1,i+3,pan)
             if(.not.running
     *         )then
                    track_data(mt(idt)  ,idt)=char(int(z'00')) ! Delta-time zero
                    mt(idt)=mt(idt)+1
             endif       
             track_data(mt(idt)  ,idt)=char(ncon_on_chann)
             track_data(mt(idt)+1,idt)=char(10)   ! Position controller
             track_data(mt(idt)+2,idt)=char(pan)  ! Position on stage
             mt(idt)=mt(idt)+3
             if(check
     *         )then
                    write(12,143)text(i),pan,ncon_on_chann,idt,running
             endif
             running=.false.
             i=i+3
             go to 2
      endif
c
      if(text(i).eq.'P'
     *  )then
c
c     Change the instrument (NB: Bank Select on controller Bn doesn't work)
c
             call easy_midi_numerics(i+1,i+3,patch)
             if(.not.running
     *         )then
                    track_data(mt(idt)  ,idt)=char(int(z'00')) ! Delta-time zero
                    mt(idt)=mt(idt)+1
             endif       
             track_data(mt(idt)  ,idt)=char(npatch_chann)
             track_data(mt(idt)+1,idt)=char(patch)    ! Patch value
             mt(idt)=mt(idt)+2
             if(check
     *         )then
                    write(12,143)text(i),patch,ncon_on_chann,idt,running
             endif
             running=.false.
             i=i+3
             go to 2
      endif
c
      if(text(i).eq.'+'
     *  )then
c
c     Increase the octave
c
             noct=noct+1
             go to 2
      endif
c
      if(text(i).eq.'-'
     *  )then
c
c     Decrease the octave
c
             noct=noct-1
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
c      What's left must be a a note or a rest...
c
      mtext=ichar(text(i))
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
c      mnote is the note given to MIDI
c
      mnote=idnote(mtext)
      if(mnote.lt.-1
     *  )then
             lx=80
             if(i.gt.lx
     *         )then
                    ia=i-lx+1
                else
                    ia=1
             endif
             write(0,208)mnote,ichar(text(i)),text(i),(text(mx),mx=ia,i)
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
c
      if(text(i+1).eq."'"
     *  )then
c
c     A pause for npause
c
             delta_time=track_time(idt)-track_time(idttmp)
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
             i=i+1
      endif
c
      mnote=note_shift+mnote+(noct*12)+noct_shift
      length_time=length_time+length
c
c      Start and stop the note
c
      if(text(i).eq.'r'
     *  )then
c
c      This is a rest
c
             mnote=mrest
             nv=0
         else
             nv=note_vel
      endif
c
c
c      Adjust for octave and set note timings
c
      if(mnote.eq.mrest
     *  )then
             note_off_time=0
             note_on_time =length
         else
             note_off_time=.5+(.1*float(9-kont)*length)
             note_on_time =length-note_off_time
      endif
      if(check)write(0,108)mnote,length,note_off_time,note_on_time
c      
      if(.not.running
     *  )then
c      
c      Initialise running mode for this (music) track
c
             delta_time=0
             call make_midi_time(idt)             ! dt 
             track_data(mt(idt),idt)=char(note_on_chann)! Note ON chann
             if(check
     *         )then
                    write(12,1351)note_on_chann
     *                    ,idt,ichar(track_data(mt(idt),idt)),mt(idt)
             endif
             mt(idt)=mt(idt)+1
             running=.true.
      endif       
c
c      Record the note, or rest, on the stack
c
c      If you want to transpose by n_semitones, add or subtract
c      n_semitones to mnote HERE!!
c
      nstack=nstack+1
      note_stack(nstack)=mnote
       len_stack(nstack)=length
       on_stack(nstack)=note_on_time
       if(check
     *   )then
              write(12,1151)mnote,running,on_stack(nstack)
     *                    ,len_stack(nstack),nstack,chord
       endif
c
c      Turn on the note 
c      (note name and then velocity, then the delta time for next note)
c
      if(mnote.le.127
     *  )then
             track_data(mt(idt)  ,idt)=char(mnote)  ! This note
             track_data(mt(idt)+1,idt)=char(nv)     ! At this velocity
             if(check
     *         )then
                    write(12,1352)track_time(idt),idt
     *                     ,ichar(track_data(mt(idt),idt)),mt(idt)
                    write(12,1353)idt,ichar(track_data(mt(idt)+1,idt))
     *                                                ,mt(idt)+1
             endif
             mt(idt)=mt(idt)+2   ! Ready for the next dt
             delta_time=0
             call make_midi_time(idt)  ! Writes the (zero) dt, advances mt(idt)
      endif
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
c      ...and select the 'delta-time to the next event'
c
             if(min_length.lt.min_on
     *         )then
                    delta_time=min_length
                else
                    delta_time=min_on
             endif
             if(check
     *         )then
c                   write(12,136)(mx,note_stack(mx),on_stack(mx)
c    *                           ,len_stack(mx),mx=1,nstack),idt
                    do mx=nstack,1,-1
                       write(12,136)mx,note_stack(mx),on_stack(mx)
     *                                 ,len_stack(mx),idt
                    enddo
                    write(12,131)min_on,note_min_on,min_length
     *                          ,note_min_len,nstack
             endif
c             
             track_time(idt)=track_time(idt)+delta_time
             mt(idt)=mt(idt)-1
             call make_midi_time(idt)  ! Translation for long delta_times
c
             ndiff=delta_time
             delta_time=0
             do istack=1,nstack
                len_stack(istack)=len_stack(istack)-ndiff
                 on_stack(istack)= on_stack(istack)-ndiff
c
                if(on_stack(istack).eq.0
     *            )then
                       if(check
     *                   )then
                              write(12,115)note_stack(istack)
     *                                        ,istack,nstack
                       endif
c
c      Stop the note...
c
                       track_data(mt(idt),idt)=char(note_stack(istack))
                       track_data(mt(idt)+1,idt)=char(0) ! Note OFF velocity
c
                       if(check
     *                   )then
                              write(12,1352)track_time(idt),idt
     *                      ,ichar(track_data(mt(idt),idt)),mt(idt)
                              write(12,1353)idt
     *                      ,ichar(track_data(mt(idt)+1,idt)),mt(idt)+1
                       endif
c
                       mt(idt)=mt(idt)+2
                       call make_midi_time(idt)             ! dt 
                endif
c
             enddo
c
             nstack_new=0
             do istack=1,nstack
                if(len_stack(istack).gt.0
     *            )then
                       nstack_new=nstack_new+1
                        len_stack(nstack_new)= len_stack(istack)
                         on_stack(nstack_new)=  on_stack(istack)
                       note_stack(nstack_new)=note_stack(istack)
                endif
             enddo
c
             if(nstack_new.eq.nstack
     *         )then
                    go to 3
                else
                    if(check
     *                )then
                           write(12,120)nstack,nstack_new
                    endif
c
c      Reset any emphasis
c
                    if(nstack_new.eq.0
     *                )then
                           if(emphasis
     *                       )then
                                  delta_time=track_time(idt)-track_time
     *                                                         (idtvol)
                                  if(delta_time.lt.0
     *                              )then
                                         go to 85
                                  endif
                                  note_vol=note_vol_old
                                  call easy_midi_tvolume
                                  emphasis=.false.
                              else
                                  last_event(idt)=.false.
                           endif
                    endif
c
                    nstack=nstack_new
             endif
c
      endif
c      
      go to 2
      stop
c
 83   continue
      write(0,221)pan
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
      write(0,133)track_time(idtvol),idt,track_time(idt)
     *                   ,delta_time
      write(0,*)nline,text
      write(0,*)nline,(text(mx),mx=1,i)
      stop
c
 86   continue
      write(0,217)
      write(0,133)track_time(idttmp),idt,track_time(idt)
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
      write(0,*)nline,(text(mx),mx=1,i+1)
      write(0,*)nline,ichar(text(i+1))
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
      mt(idtvol)=mt(idtvol)+1
      mt(idttmp)=mt(idttmp)+1
      midi_data(12)=char(ntracks)	! Number of tracks in file
c
c      Catenate all MIDI header and all the tracks
c
      do idt=1,ntracks
c
c      Tack on the end-of-track event
c
         if(check)write(12,139)idt,last_event(idt)
         if(last_event(idt)  ! Track ended on some kind of event notification
     *      .and.
     *      idt.gt.3
     *     )then
                nx=0
            else
                nx=-1
         endif
c
         track_data(mt(idt)+nx  ,idt)=char(int(z'00'))
         track_data(mt(idt)+nx+1,idt)=char(int(z'FF'))
         track_data(mt(idt)+nx+2,idt)=char(int(z'2F'))	! End track event
         track_data(mt(idt)+nx+3,idt)=char(int(z'00'))
         mt(idt)=mt(idt)+nx+3                      ! Exact track length
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
         if(check
     *     )then
                write(20+idt,102)idt
                write(20+idt,113)(ichar(track_data(mx,idt))
     *                      ,mx=1,mt(idt))
                write( 0,112)idt,lentrka,(ichar(track_data(mx,idt))
     *                      ,mx=5,8),length_time,length_time/24
                write(10,112)idt,lentrka,(ichar(track_data(mx,idt))
     *                      ,mx=5,8),length_time,length_time/24
                write(10,113)(ichar(track_data(mx,idt)),mx=1,mt(idt))
          endif
c
c      Add this track to the MIDI data
c
         do nb=1,mt(idt)
            md_count=md_count+1
            midi_data(md_count)=track_data(nb,idt)
         enddo
      enddo
      if(check
     *  )then
             write(8,110)md_count
      endif
c
      open(42,file='x.mid',form='formatted',status='unknown')
      do i=1,md_count
         write(42,111)midi_data(i)
c
         if(check
     *     )then
                if(ichar(midi_data(i+1)).ge.240.and.
     *             ichar(midi_data(i  )).le.127
     *            )then
c
c      A MIDI Event, preceded by a delta time, is about to happen...
c
                       write(4,102)
                       write(10,102)
                endif
         endif
c
         if(check
     *     )then
                if(ichar(midi_data(i)).gt.31
     *            )then
                       write(4,122) midi_data(i),ichar(midi_data(i)),i
     *                             ,i-95,i
                   else
                       write(4,1221)ichar(midi_data(i)),i,i-95,i
                endif
         endif
c
      enddo
      close(42)
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
108   format('Note:',i4,', length:',i4,', note_on_time:',i4
     *      ,', note_off_time',i4)
109   format(2i4,' ',120a1,'*')
110   format('Length of all MIDI data is',i8,' characters')
111   format(a1,$)
112   format(/'Length of midi track',i3,' is'
     *      ,i8,' characters',4(' ',z2)
     *      ,'.   Ticks:',i8,', crotchets:',i8)
113   format(40z3.2)
114   format('Transposing by',i3,' semitone(s)')
115   format('Note',i4,' turned off (Stack item',i3,' of',i3,')')
1151  format('Note',i4,' turned on. Running:',l2,', ON time',i3
     *      ,', full length',i3,': Stack',i3,', Chord:',l2)
116   format('Key shifted from',i3,' ',a,' to',i3,' ',a)
117   format(/'################### Chord starts')
118   format( '################### Chord ends',/)
119   format(20x,'Stack height is now',i3)
120   format(/
     *     ,/'############################'
     *     ,/'Stack reset from',i3,' to',i3
     *     ,/'############################')
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
     *                        ,', Min length',i4,' at',i3,' of',i3)
132   format('     Tempo:',2i8,2i6,4x,10z3.2)                       
133   format('Trackx time',i6,', Track',i2.2,' time:',i6
     *      ,', delta_time',i6,z7,/)
134   format('Expression:',2i8,2i6,4x,12z3.2)                       
1351  format(17x,'Note_ON event on channel x',z2.2,' on track',i2,':',i4
     *      ,' at byte',i4)
1352  format('Track time:',i8,13x,'Note          on track',i2,':',i4
     *      ,' at byte',i4)
1353  format(32x,'Velocity      on track',i2,':',i4,' at byte',i4)
136   format('Stack item:',i3', Note:',i4,', ON:',i6,', Length:',i6
     *      ,' (Track:',i2,')')
137   format('Length of all MIDI data is',i8,' characters')
138   format('How many verses? ',i1)
139   format('last_event(',i1,') is',l2)
140   format(2a1,i4)
141   format(a1,' ',i4)
142   format('Track',i2,': nvolume',i7,', Bytes',i4,' to',i4,':    '
     *       ,3z3.2)
143   format('Stem code:',a2,', value',i4
     *      ,', channel',z4,', track',i3,', running',l2)
c
203   format('***** ERROR ON INPUT')
205   format('***** ERROR: Bad tempo')
206   format('***** ERROR: Bad volume')
207   format('***** ERROR: Cannot end Track - none started')
208   format('***** ERROR: ',i4,' from idnote(',i4,') [',a1
     *      ,'] is not a note.   Last 80 characters below:',/80a1)
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
220   format('***** ERROR: Bad time-span or target value.'
     *     ,/'             Must be 5 numbers, eg 08120')
221   format('***** ERROR: Bad pan value',i10,':  127 >= pan >= 0' )
c
      end
