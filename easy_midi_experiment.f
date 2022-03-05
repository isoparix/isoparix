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
c       TZ      Track ends
c	octaVe	MIDI convention hex(zero to F)
c
      parameter (maxtracks=8,maxtrklen=8000,maxdata=64000)
c
      dimension idnote(0:z'FF'),natural(7),mt(maxtracks)
     *         ,note_stack(16),len_stack(16),durn_stack(16)
c
      character(1) text(120),octave
      character(1) midi_data(maxdata),track_data(maxtrklen,maxtracks)
      character(1) textin(120)
      character(2) key,time_sig
      character(3) duration,beats_per_minute
      character(4) mtrk,mtrkend
      character(14) mthd
c
      integer (4) delta_time,stc_time,stc_vel,leg_time,leg_vel,durn_time
c
      logical track_started,key_init,running,chord,staccato
c
c      Set defaults
c
      staccato=.false.
      jaccidental=0
      nstack=0
      chord=.false.
      running=.false.
      key_init=.true.
      noct=4		! octaVe
      length=25 	! Crotchet
      nkey=0		! C major
      kont_def=7	! Staccato 0; Legato 9
      note_vel=64 	! Note velocity (S/s=5)
      note_vol=64 	! Note volume (U/u=5)
      kont=kont_def
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
      ntracks=1
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
      track_data(mt(1)+2,1)=char(z'51')	! 0x51 Tempo
      track_data(mt(1)+3,1)=char(z'03')	! 0x03 Tempo
      track_data(mt(1)+4,1)=char(z'07')	! 500,000 decimal gives 120 beats_per_minute
      nte=mt(1)+4
      track_data(mt(1)+5,1)=char(z'A1')
      track_data(mt(1)+6,1)=char(z'20')	! 
      mt(1)=mt(1)+7
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
      nline=0
  1   continue
      text=''
      read(*,100,err=98,end=99,iostat=ios)textin
      nline=nline+1
      write(0,*)nline,textin
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
                    beats_per_minute=text(i+2)//text(i+3)//text(i+4)
                    read(beats_per_minute,107,err=90)n
                    n=(6*(10**7))/n
c
c      Compute and record tempo setting
c
                    k=256*256
                    do j=nte,nte+2
                       m=n/k
                       track_data(j,1)=char(m)
                       n=n-(m*k)
                       k=k/256
                    enddo
c
                    i=i+4
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
                    note_vel=64
                    note_vol=64
                    running=.false.
                    length_time=0
                    ntracks=ntracks+1
                    idt=ntracks
c
                    ncon_on_chann=(16*11)+idt ! Before we set track number..
                    note_on_chann=(16*9)+idt
c
c      Set up the Sequence/Track name
c
                    len_nametxt=8
                    track_data(mt(idt)  ,idt)=char(z'00')		! Delta-time zero
                    track_data(mt(idt)+1,idt)=char(z'FF')		! Meta-event
                    track_data(mt(idt)+2,idt)=char(z'03')		! 0x03 Sequence/Track name
                    track_data(mt(idt)+3,idt)=char(len_nametxt)		! len_nametxt bytes of text
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
                    track_data(mt(idt)+3,idt)=char(note_vel)
                    mt(idt)=mt(idt)+4
c
                    track_data(mt(idt)  ,idt)=char(z'00')		! Delta-time zero
                    track_data(mt(idt)+1,idt)=char(ncon_on_chann)	! 0xB0 is controller (the 'B') for channel 0-F
                    track_data(mt(idt)+2,idt)=char(11)	! Pan controller (coarse)
                    track_data(mt(idt)+3,idt)=char(128)
                    mt(idt)=mt(idt)+4
                    write(4,123)mt(idt)
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
             if(chord
     *         )then    ! We are starting a chord when one is started
                    go to 88
                else
                    chord=.true.
             endif
             write(4,117)
             go to 2
      endif
c
      if(text(i).eq.'}'
     *  )then
             if(chord
     *         )then        
                    chord=.false.
                else    ! We are ending a chord when none exists - error
                    go to 89
             endif
             write(4,118)
             go to 3
      endif
c
      if(text(i).eq.'S'.or.
     *   text(i).eq.'s'
     *  )then
             read(text(i+1),106,err=91)note_vel
             note_vel=12.7*float(note_vel+1)
             i=i+1
             go to 2
      endif
c
      if(text(i).eq.'Q'
     *  )then
             read(text(i+1),106,err=96)kont
             i=i+1
             go to 2
      endif
c
      if(text(i).eq.'q'
     *  )then
             kont=kont_def
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
             write(duration,101)text(i+1)//text(i+2)//text(i+3)
             read(duration,107,err=97)length
             i=i+3
             go to 2
      endif
c
      if(text(i).eq.'K'
     *  )then
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
      if(text(i).eq.'V'.or.
     *   text(i).eq.'v'
     *  )then
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
             mnote=mnote+1
             jaccidental=1
      endif
c
      if(text(i+1).eq.'_'
     *  )then
             mnote=mnote-1
             jaccidental=1
      endif
c
      if(text(i+1).eq.'='
     *  )then
             mnote=natural(mtext-96)
             jaccidental=1
      endif
      i=i+jaccidental
      jaccidental=0
c
c      Adjust for octave and set note timings
c
      mnote=mnote+(noct*12)+noct_shift
c     note_on_time=(.1*float((1+kont)*length))+.5
c     note_off_time=length-note_on_time
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
             mnote=200   ! Outside of MIDI range 0 to 127
             nv=0
         else
             nv=note_vel
      endif
c
      if(.not.running
     *  )then
             track_data(mt(idt)  ,idt)=char(z'00')		! Delta-time of zero
             track_data(mt(idt)+1,idt)=char(note_on_chann)	! Note ON channel 0-F
             mt(idt)=mt(idt)+2
             running=.true.
      endif
c
c      Record the note, or rest, on the stack
c
      nstack=nstack+1
      note_stack(nstack)=mnote
       len_stack(nstack)=length
      durn_stack(nstack)=length
      write(4,114)note_stack(nstack),len_stack(nstack),nstack,chord
     *           ,mt(idt)
c
c      If it's a note, not a rest, turn on the note 
c      (note name and then velocity, then the delta time for next note)
c
      if(mnote.lt.128
     *  )then
             track_data(mt(idt)  ,idt)=char(mnote)  ! This note
             track_data(mt(idt)+1,idt)=char(nv)     ! At this velocity
             track_data(mt(idt)+2,idt)=char(z'00')  ! Delta-time 0
             write(2,1221)track_data(mt(idt)  ,idt),mt(idt)
             write(2,1221)track_data(mt(idt)+1,idt),mt(idt)+1
             write(2,1222)track_data(mt(idt)+2,idt),mt(idt)+2
             mt(idt)=mt(idt)+3
      endif
c
  3   continue
      if(.not.chord.and.
     *   nstack.gt.0
     *   )then
c
c      FInd out when something has to happen...
c
             min_length=1000000
             do istack=1,nstack
                if(len_stack(istack).lt.min_length
     *            )then
                       min_length= len_stack(istack)
                        durn_time=durn_stack(istack)
                endif
             enddo
c
c      ...and write the 'delta-time to the next event'
c
             delta_time=min_length
c            if(delta_time.gt.127
c    *         )then
c                   knote=delta_time/128
c                   delta_time=delta_time-(knote*128)
c                   knote=z'80'+knote
c                   track_data(mt(idt)+2,idt)=char(knote)
c                   write(4,116)knote,mt(idt)
c                   write(2,1222)track_data(mt(idt)+2,idt),mt(idt)+2
c                   mt(idt)=mt(idt)+1
c            endif      
c
                    note_off_time=(.1*float(durn_time*(9-kont)))+.5
                    if(note_off_time.gt.delta_time
     *                )then
                           note_off_time=delta_time
                    endif                
                    track_data(mt(idt)-1,idt)
     *                                   =char(delta_time-note_off_time)
                    write(4, 116)delta_time-note_off_time,mt(idt)-1
                    write(2,1222)track_data(mt(idt)-1,idt),mt(idt)-1
c
             nstack_new=0
             do istack=1,nstack
                len_stack(istack)=len_stack(istack)-min_length
                if(len_stack(istack).eq.0
     *            )then
                       if(note_stack(istack).lt.128
     *                   )then
                              write(4,115)note_stack(istack)
c
c      This is not a rest, so stop the note...
c
                              track_data(mt(idt) ,idt)
     *                                         =char(note_stack(istack))
                              track_data(mt(idt)+1,idt)=char(0)     ! At this volume
                              track_data(mt(idt)+2,idt)=char(0)     ! Stop all notes together
                 write(2,1221)track_data(mt(idt)  ,idt),mt(idt)
                 write(2,1221)track_data(mt(idt)+1,idt),mt(idt)+1
                 write(2,1222)track_data(mt(idt)+2,idt),mt(idt)+2
                              mt(idt)=mt(idt)+3
                       endif
                   else
                       note_off_time=delta_time
                       nstack_new=nstack_new+1
                        len_stack(nstack_new)= len_stack(istack)
                       note_stack(nstack_new)=note_stack(istack)
                endif
             enddo
c
c      Add the note_off_time
c
             track_data(mt(idt)-1,idt)=char(note_off_time)
             write(4, 116)note_off_time,mt(idt)-1
             write(2,1222)track_data(mt(idt)-1,idt),mt(idt)-1
             nstack=nstack_new
             write(4,120)nstack
             if(track_started
     *         )then        
                    go to 2
                else
                    if(nstack.gt.0
     *                )then
                           write(4,121)
                           go to 3
                    endif
             endif               
      endif
c      
c
c
c      Publish details about this note...
c
c     write(8,1023)text(i),mnote,noct
c    *            ,nton,note_off_time,nv,kont,nkey
c    *            ,(track_data(mx,idt),mx=mt(idt),mt(idt)+5)
c      
      go to 2
c
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
         write(4,102)idt
         write(4,113)(track_data(mx,idt),mx=1,lentrka)
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
         if(ichar(midi_data(i+1)).ge.128.and.
     *      ichar(midi_data(i  )).eq.0
     *     )then
c
c      A MIDI Event is about to happen...
c
                write(4,102)
         endif
c
         if(ichar(midi_data(i)).gt.31
     *     )then
                write( 4,122) midi_data(i),midi_data(i),i,i-95
            else
                write( 4,1221)             midi_data(i),i,i-95
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
104   format(a3)
105   format(i2)
106   format(2i1)
107   format(i3)
108   format(6z3)
109   format(i4,' ',120a1)
110   format('Length of all MIDI data is',i8,' characters')
111   format(a1,$)
112   format('Length of midi track',i3,' is'
     *      ,i8,' characters',4(' ',z2)
     *      ,'.   Ticks:',i8,', crotchets:',i8)
113   format(40z3.2)
114   format('Note',i4,', length',i4,': Stack height',i5,l3,' Byte:',i6)
115   format('Note',i4,' turned off')
116   format(22x,'Delta_time',i10,' Byte:',i6)
117   format(20x,'Chord starts')
118   format(20x,'Chord ends')
119   format(20x,'Stack height is now',i3)
120   format(22x,'Stack reset to',i3)
121   format(20x,'Terminating chord at end of track')
122   format(a1,z3.2,2i6)
1221  format(z4.2,2i6)
1222  format(20x,'Delta time:',z4.2,2i4)
123   format('Music track starts at byte:',i6)
124   format(2i6)
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
c
      end
