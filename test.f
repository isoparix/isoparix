      program test
c
      real(8) income(12,2010:2020),spend(12,2010:2020)
c
      character(120)txta,txtc
      character(1)  txtb(120)
      equivalence (txta,txtb)
c
      character(10)date
      character(9)sortcode,account,category
c
      real(8)total
c
      income=0.0
      spend=0.0
c
      write(*,*)'Program test'
      nt=0
  1   continue
      txta=''
      txtb=''
      txtc=''
      read(*,101,end=2,err=1)txta
      txtc=txta   !  For the record
      nt=nt+1
      mcomma=0
c     write(0,*)txtc
      do n=1,120
         if(txtb(n).eq.','
     *     )then
                mcomma=1
                do i=n+1,120
                  txtb(mcomma)=txtb(i)
                  mcomma=mcomma+1
                enddo
                exit     !  This is the first comma
          endif
      enddo
      if(mcomma.eq.0)go to 1   ! A blank line...
      do n=1,mcomma-1
         if(txtb(n).eq.',')txtb(n)=' '  !   Clear remaining commas
      enddo
      read(txta,102,err=900)date,sortcode,account
      do n=1,29
         txtb(n)=' '  !   Clear extraneous date before total
      enddo
      read(txta,*,err=901,end=1)total
      read(date,104,err=902)nday,nmon,nyear
c     write(*,103)date,sortcode,account,total,nday,nmon,nyear
      if(total.lt.0.0)spend(nmon,nyear)=spend(nmon,nyear)-total
      if(total.gt.0.0)  income(nmon,nyear)=  income(nmon,nyear)+total
      go to 1
c
  2   continue
c
      amon=0.
      balance=0.
      cumspend=0.0
      cumincome=0.0
      write(*,106)
      do ny=2010,2020
        do nm=1,12
           if(income(nm,ny).ne.0
     *       )then
                  graph_date=float(ny)+(float(2*nm-1)/24.0)
                  amon=amon+1.0
                  balance=  balance+income(nm,ny)+spend(nm,ny)
                  rate=balance/amon
                  cumincome=cumincome+income(nm,ny)
                  cumspend=cumspend+spend(nm,ny)
                  write(*,105)ny,nm
     *                       ,graph_date,cumincome
     *                       ,graph_date,cumspend
     *                       ,graph_date,income(nm,ny)
     *                       ,graph_date,spend(nm,ny)
     *                       ,graph_date,income(nm,ny)+spend(nm,ny)
           endif
        enddo
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
200   format(/'Failure to read date, sortcode, or account')
201   format(/'Failure to read total')
202   format(/'Failure to read nday, nmon, or nyear from date =>'
     *        ,a,'<=')
c
      end
