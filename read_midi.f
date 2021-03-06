      program read
c
c    Reads a MIDI file...
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
      dimension idnote(0:255),natural(7),mt(maxtracks)
     *         ,note_stack(16),len_stack(16) 
c
      character(1) text(120),octave
      character(1) midi_data(maxdata),track_data(maxtrklen,maxtracks)
      character(1) textin(120)
      character(2) key,time_sig
      character(3) duration,beats_per_minute
      character(4) mtrk,mtrkend
      character(14) mthd
c
      integer (4) delta_time,stc_time,stc_vel,leg_time,leg_vel
c
      logical track_started,key_init,running,chord,staccato
c
c      Set defaults
c
      n=0
      open(12,file='x.mid',form='formatted',status='unknown')
  1   continue
      read(12,103,iostat=irc,end=3,advance='no',eor=2
     *           ,size=md_count)midi_data
  2   continue
      write(*,110)md_count
      do i=1,md_count
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
                write( 4,122) midi_data(i)
     *                       ,ichar(midi_data(i)),n,n,i
            else
                write( 4,1221)
     *                        ichar(midi_data(i)),n,n,i
         endif
c
         n=n+1
      enddo
      go to 1
  3   continue
      close(12)
c
      stop
c
100   format(120a1)
101   format(a3)
102   format(/,i4)
103   format(100000a1)
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
122   format(a1,z3.2,i6,z6.2,i6)
1221  format(   z4.2,i6,z6.2,i6)
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
