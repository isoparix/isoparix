      program adventdates
c
c      Enables dates on Advent Calendar
c
c
      character(5)  datelead
      character(2)  datetext
      character(1000)divtext
c      
c      
      open(8,err=99,file='adventmaster.content',status='old')
      write(*,104)
      read(*,*)nallowed
  1   continue
      divtext=' '
      read(8,100,end=2,err=2)datelead,datetext,divtext
      if(datelead.eq.'<!--X'
     *  )then
c            write(*,*)datetext
             read(datetext,101)ndate
             if(ndate.le.nallowed
     *         )then
                    write(4,102)ndate,ndate
                else    
                    write(4,103)ndate,ndate
             endif        
         else
             write(4,100)datelead,datetext,trim(divtext)    
      endif       
      go to 1
c
  2   continue
      stop
c
 99   continue
      write(0,105)
      stop
c
100   format(a5,a2,a)
101   format(i2)
102   format('<!--X',i2.2,'-->'
     *      ,' <div class="divdate">',i0)
103   format('<!--X',i2.2,'-->'
     *      ,' <div class="divd0te">',i0)
104   format('Max allowed date?')
105   format('ERROR: File adventmaster.content not found')
c
      end      
