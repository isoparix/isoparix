      program barclays_pdf
c
c      Reads text version of PDF bank statement
c
      implicit real(8) (a-h,o-z)
c
c      These arrays have a time (x-axis) and a monetary value (y-axis)
c
      real(8),dimension (1000) :: cash_time,balance_time
     *                           ,cash_in,cash_out,balance
c
      character(120)txta,txtc,text_buffer
      character(3)amon,month(12)
      character(1)  txtb(120)
      equivalence (txta,txtb)
c
      logical cash_line
c
      real(8)total
c
      month( 1)='Jan'
      month( 2)='Feb'
      month( 3)='Mar'
      month( 4)='Apr'
      month( 5)='May'
      month( 6)='Jun'
      month( 7)='Jul'
      month( 8)='Aug'
      month( 9)='Sep'
      month(10)='Oct'
      month(11)='Nov'
      month(12)='Dec'
c
      iz=0
c
      write(*,*)'Read Barclay PDF'
      last_iday=0
      last_imon=0
      nbal=0
      ncash=0
      cash_in=0.0
      cash_out=0.0
      open(8,file='pdf_balances.csv',status='unknown',form='formatted')
      txtc=''
      cash_line=.false.
  1   continue
      txta=' '
      txtb=' '
      read(*,101,end=2,err=1)txta
c
      if(txtb(3).eq.'/'.and.
     *   txtb(6).eq.'/'
     *  )then               ! A date line
c            write(*,*)txtc
             cash_line=.true.
c            txtc=trim(txtc)//trim(txta)//', '   !  For the record
             read(txta,104)iday,imon,iyear
             amon=month(imon)
         else
             if(txtb(1).eq.'£'.or.
     *          txtb(2).eq.'£'
     *         )then               ! Transaction and balance
c                   write(*,*)txtc
                    m=0
                    do n=1,120
                       if(txtb(n).ne.'£'.and.
     *                    txtb(n).ne.','
     *                   )then
                              m=m+1
                              txtb(m)=txtb(n)
                       endif
                    enddo
c
                    if(imon.ne.last_imon
     *                )then
                           ncash=ncash+1 ! New month
                    endif
c
                    read(txta,*)transvalue,blnce
                    write(text_buffer,111)transvalue,blnce
                    txtc=trim(txtc)//trim(text_buffer)
                    write(8,*)trim(txtc)
                    txtc=''
                    cash_line=.false.
                 call isotime(iday,amon,iyear,iz,iz,iz,itod,ityear,lday)
                    year=float(iyear)+(float(ityear)/(86400*365.0))
                    if(iday.ne.last_iday.or.imon.ne.last_imon
     *                )then
                           write(*,107)year,iday,imon,iyear
     *                               ,transvalue,blnce
c                          write(8,108)year,blnce
                           last_imon=imon
                           last_iday=iday
                           cash_time(ncash)=year
                           nbal =nbal +1
                           balance_time(nbal)=year
                           balance     (nbal)=blnce
                    endif
c
                    if(transvalue.gt.0
     *                )then
                           cash_in (ncash)=cash_in (ncash)+transvalue
                       else
                           cash_out(ncash)=cash_out(ncash)-transvalue
                    endif
            endif
      endif
      if(cash_line)txtc=trim(txtc)//trim(txta)//' ,'   !  For the record
c
      go to 1
c
  2   continue
c
      do nb=1,nbal
         write(*,110)balance_time(nb),balance(nb)
      enddo
c
      do nc=1,ncash
         write(*,109)cash_time(nc),cash_in (nc),cash_out(nc)
      enddo
c
      stop
c
900   continue
      write(*,200)
      write(*,*)txtc
      write(*,*)txta
      stop
c
901   continue
      write(*,201)
      write(*,*)txtc
      write(*,*)txta
      go to 1
c
902   continue
      write(*,202)date
      write(*,*)date
      stop
c
100   format(i4,a120)
101   format(a120)
102   format(a10,a9,a9,f9.2,a)
103   format('    ',a10,a9,a9,f9.2,3i6)
104   format(i2,1x,i2,1x,i4)
105   format(2i6,5(f8.2,',',f10.2))
106   format(/'  Year Month',10x,'CumIncome',11x,'CumSpend'
     *   13x,'Income',14x,'Spend',13x,'Balance',/)
107   format(f12.6,2i3,i5,2f10.2)
108   format(f12.6,',',f12.2)
109   format(f12.6,', Cash in:',f12.2,', Cash out:',f12.2)
110   format(f12.6,', Balance:',f12.2)
111   format(f12.2,', ',f12.2)
200   format(/'Failure to read date, sortcode, or account')
201   format(/'Failure to read total')
202   format(/'Failure to read nday, nmon, or nyear from date =>'
     *        ,a,'<=')
c
      end
