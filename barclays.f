      program barclays
c
      implicit real(8) (a-h,o-z)
c
c      141.00 CHOUDHRY S            CRUK INVNO 122176  BBP
c      138.00 CHOUDHRY S            CRUK MEETING 9NOV  BBP
c      89.00 CHOUDHRY S            CRUK MTG INVNO 328 BBP
c      149.00 CHOUDHRY S            CRUK22.11BOOKING   BBP
c      92.00 CHOUDHRY S            CRUKMEETING310117  BBP
c      141.00 CHOUDHRY S            INVNO 122166       BBP
c      414.00 DRIN DRIV ED          INV NO INV-0025    BBP
c      390.00 DRIN DRIV ED          INVOICE NO 122008  BBP
c      390.00 DRIN DRIV ED          INVOICE NO 122142  BBP
c      414.00 DRIN DRIV ED SW       INVOICE NO 392     BBP
c      138.00 FRIENDSHIP RL         SOUTH EAST RLS     BBP
c      184.00 FRIENDSHIP RL         SOUTH EAST RLS     BBP
c      258.00 FRIENDSHIP RL         SOUTH EAST RLS     BBP
c       
      character(18)text,old_text
      character(12)category
c
      old_text='Start'
      total=0.0
      global_i=0.0
      global_e=0.0
      nt=0
      nr=0
      nrg=0
c
      read(7,*,end=3,err=3)category
      open( 8,file='overall.log    ',form='formatted',status='unknown')
c
  1   continue
      read(1,  *,end=2,err=3)amount
      read(3,113,      err=3)text
      nr=nr+1
c     write(*,*)amount,ival
      if(text.ne.old_text
     *  )then
             write(8,*)trim(text)
c
             if(total.lt.0.0.and.nt.ne.0
     *         )then
                    write(10,102)total,nt,trim(old_text)
                    write(12,108)nt,total,trim(old_text)
             endif
c
             if(total.ge.0.0.and.nt.ne.0
     *         )then
                    write(10,104)total,nt,trim(old_text)
                    write(14,108)nt,total,trim(old_text)
             endif
c
             total=amount
             nt=1
             old_text=text
         else
             total=total+amount
             nt=nt+1
      endif
      nrg=nrg+1
      if(amount.gt.0.)global_i=global_i+amount
      if(amount.le.0.)global_e=global_e+amount
      go to 1
c
  2   continue
c
      if(nt.ne.0
     *  )then
             if(total.lt.0.0
     *         )then
                    write(10,102)total,nt,trim(old_text)
                    write(12,108)nt,total,trim(old_text)
                else
                    write(10,104)total,nt,trim(old_text)
                    write(14,108)nt,total,trim(old_text)
             endif
      endif
c
      write(16,107)category,global_e
      write(16,106)category,global_i
      write(16,105)category,global_i+global_e,nrg
c
      write( 0,107)category,global_e
      write( 0,106)category,global_i
      write( 0,105)category,global_i+global_e,nrg
      stop
c
  3   continue
      write(0,103)nr
      stop
c
102   format(f10.2,' Money out:',i6,'   ',a)
103   format(' **ERROR** after',i6,' reads')
104   format(f10.2,' Money  in:',i6,'   ',a)
105   format( a12,'      NETT:',f12.2,' via',i6,' entries',/)
106   format( a12,' MONEY  IN:',f12.2)
107   format(/a12,' MONEY OUT:',f12.2)
108   format(i6,', ',f12.2,",'",a18)
113   format(a18)
c
      end
