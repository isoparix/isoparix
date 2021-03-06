      subroutine make_midi_time(ntrack)
c      
c      We assume values are already set for delta_time and mt(ntrack)
c
c      The number is converted into 7-bit bytes, and the most-significant 
c      bit of each byte is 1 except for the last byte of the number, 
c      which has a msb of 0.
c
      use easy_midi_comms
c
      integer dtime
c
      logical started
c
      if(ntrack.gt.maxtracks
     *  )then
             write(0,102)ntrack,maxtracks,delta_time
             stop
      endif
c
      ninit=mt(ntrack) 
      index=4
      started=.false.
      k=128**(index-1)
      dtime=delta_time
c      
      if( dtime.lt.0.or.
     *   (dtime/k).gt.128
     *  )then
             write(0,101)dtime
             return
      endif
c
      if(dtime.le.127
     *  )then
             track_data(mt(ntrack),ntrack)=char(dtime)
             mt(ntrack)=mt(ntrack)+1
         else
c
c      Cope with delta-times > 127
c
             do n=1,index
               if(dtime.ge.k
     *           )then
                      started=.true.
                      knote=dtime/k
                      dtime=dtime-(knote*k)
                      knote=int(z'80')+knote
                      track_data(mt(ntrack),ntrack)=char(knote)
                      mt(ntrack)=mt(ntrack)+1
                  else
                      if(started
c
c      We've started the number...
c
     *                  )then
                             track_data(mt(ntrack),ntrack)
     *                       =char(int(z'80'))
                             mt(ntrack)=mt(ntrack)+1
                       endif
                endif
                k=k/128
             enddo   
c
c      Make last byte have MSB of zero
c
             n=ichar(track_data(mt(ntrack)-1,ntrack))-int(z'80')
             track_data(mt(ntrack)-1,ntrack)=char(n)
      endif
c
      if(check
     *  )then
             do n=ninit,mt(ntrack)-1
                write(12,135)ntrack,ichar(track_data(n,ntrack)),n
             enddo
      endif
c
      return
c
100   format(10i12)
101   format('ERROR in MAKE_MIDI_TIME: dtime=',i10)
102   format('ERROR in MAKE_MIDI_TIME: ntrack=',i8,' (max=',i3,')'
     *      ,', delta_time is',i8)
103   format('make_midi_time',i6,' at byte',i6)
135   format(32x,'Delta time    on track',i2,': x',z2.2,' at byte',i4)
c      
      end
