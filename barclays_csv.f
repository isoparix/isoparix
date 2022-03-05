      program barclays_csv
c
c      Reads a Barclays .csv file
c
      implicit real(8) (a-h,o-z)
c
      real(8) nett,income(12,2010:2020),spend(12,2010:2020)
      real(8),dimension(200) :: cumig,cumsg,cumbal,among,plot
      real(8),dimension(31)  :: day_in,day_out,cum_in,cum_out,cum_daily
c
      integer,dimension(31) :: nc_in,nc_out
c
      character(120)txta,txtc
      character(30)grfname,series_name,category
      character(1)  txtb(120)
      equivalence (txta,txtb)
c
      character(10)date
      character(9)sortcode,account
c
      real(8)total,balance
c
      income=0.0
      spend=0.0
c     balance=36797.95  ! 1st April 2016
      balance=0.0
      day_in =0.0
      day_out=0.0
      nc_in=0
      nc_out=0
      nreads=0
c
      read(7,*)category
      write(*,*)'Program barclays_csv ',trim(category)
      nt=0
  1   continue
      txta=' '
      txtb=' '
      txtc=' '
      read(*,101,end=2,err=1)txta
      txtc=txta   !  For the record
      nt=nt+1
      mcomma=0
c     write(4,*)txtc
c
      do n=120,1,-1
         if(txtb(n).ne.' '
     *     )then
                write(4,*)txtb(n-2)//txtb(n-1)//txtb(n)
                exit
         endif
      enddo
c
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
c
      if(mcomma.eq.0)go to 1   ! A blank line...
c
      do n=1,mcomma-1
         if(txtb(n).eq.',')txtb(n)=' '  !   Clear remaining commas
      enddo
c
      read(txta,102,err=900)date,sortcode,account
      do n=1,29
         txtb(n)=' '  !   Clear extraneous date before total
      enddo
      read(txta,*,err=901,end=1)total
      read(date,104,err=902)nday,nmon,nyear
      nreads=nreads+1
c     write(*,103)date,sortcode,account,total,nday,nmon,nyear
      if(total.lt.0.0
     *  )then
             spend (nmon,nyear)=spend (nmon,nyear)-total
             day_out(nday)=day_out(nday)+total
             nc_out(nday)=nc_out(nday)+1
      endif
      if(total.gt.0.0
     *  )then
             income(nmon,nyear)=income(nmon,nyear)+total
             day_in(nday)=day_in(nday)+total
             nc_in(nday)=nc_in(nday)+1
      endif
      go to 1
c
  2   continue
c
      amon=0.
      nett=0.
      cumspend=0.0
      cumincome=0.0
      write(*,106)
      ndata=0
      do ny=2010,2020
        do nm=1,12
           if(income(nm,ny).ne.0.or.
     *         spend(nm,ny).ne.0
     *       )then
                  ndata=ndata+1
                  graph_date=float(ny)+(float(2*nm-1)/24.0)
                  amon=amon+1.0
                  nett=income(nm,ny)-spend(nm,ny)
                  rate=nett/amon
                  cumincome=cumincome+income(nm,ny)
                  cumig(ndata)=cumincome
                  cumspend=cumspend+spend(nm,ny)
                  cumsg(ndata)=cumspend
                  balance=balance+nett !
                  cumbal(ndata)=balance
                  write(*,105)ny,nm
     *                       ,graph_date,cumincome
     *                       ,graph_date,cumspend
     *                       ,graph_date,income(nm,ny)
     *                       ,graph_date,spend(nm,ny)
     *                       ,graph_date,nett
     *                       ,graph_date,balance
                  xmax=graph_date
                  if(ndata.eq.1)xmin=xmax
                  among(ndata)=graph_date
           endif
        enddo
      enddo
c
c      Write out mean values
c
      rndata=1.0/float(ndata)
      write(*,107)ndata,cumincome*rndata,cumspend*rndata
c
      ymax=-100000.
      ymin= 100000.
      do i=1,ndata
         if(cumig(i).gt.ymax)ymax=cumig(i)
         if(cumsg(i).gt.ymax)ymax=cumsg(i)
         if(cumig(i).lt.ymin)ymin=cumig(i)
         if(cumsg(i).lt.ymin)ymin=cumsg(i)
         if(cumbal(i).lt.ymin)ymin=cumbal(i)
         if(cumbal(i).lt.ymin)ymin=cumbal(i)
         plot((2*i)-1)=among(i)
      enddo
c
      nord=2*ndata
c     write(*,*)xmin,xmax,ymin,ymax,nord
      grfname=trim(category)//'_Cumulative'
      call grf_header(xmin,xmax,ymin,ymax,grfname)
c
c       ##########  Graph series  #########
c
      do i=1,ndata
         plot(2*i)=cumig(i)
      enddo
c
      series_name='Income'
      call grf_points(0,1,nord,plot,'Black   ','Black   ',2,1
     *               ,series_name)
c
c       ##########  Graph series  #########
c
      do i=1,ndata
         plot(2*i)=cumsg(i)
      enddo
c
      series_name='Expenditure'
      call grf_points(0,2,nord,plot,'Red     ','Red     ',2,1
     *               ,series_name)
c
c       ##########  Graph series  #########
c
      do i=1,ndata
         plot(2*i)=cumbal(i)
      enddo
c
      series_name='Balance'
      call grf_points(0,3,nord,plot,'Blue    ','Blue    ',2,1
     *               ,series_name)
c
      call grf_trailer(3)
c
      write(*,*)nreads,ndata
      sum_in=0.
      sum_out=0.
      ymin=0.0
      a=1./float(ndata)
      do n=1,31
         sum_in =sum_in + day_in(n)
         sum_out=sum_out-day_out(n)
         cum_in (n)=a*sum_in
         cum_out(n)=a*sum_out
         cum_daily(n)=cum_in(n)-cum_out(n)
         write(*,108)n,day_in(n),day_out(n),cum_in(n),cum_out(n)
     *                                     ,cum_daily(n)
      enddo
c
      ymax=amax1(cum_out(31),cum_in(31))
      ymin=minval(cum_daily)
c
      nord=2*31
      xmin=1.0
      xmax=31.0
      grfname=trim(category)//'_AverageDaily'
      call grf_header(xmin,xmax,ymin,ymax,grfname)
c
c       ##########  Graph series  #########
c
      do i=1,31
         plot((2*i)-1)=i
         plot(2*i)=cum_in(i)
      enddo
c
      series_name='Income'
      call grf_points(0,1,nord,plot,'Black   ','Black   ',2,1
     *               ,series_name)
c
c       ##########  Graph series  #########
c
      do i=1,31
         plot(2*i)=cum_out(i)
      enddo
c
      series_name='Expenditure'
      call grf_points(0,2,nord,plot,'Red     ','Red     ',2,1
     *               ,series_name)
c
c       ##########  Graph series  #########
c
      do i=1,31
         plot(2*i)=cum_daily(i)
      enddo
c
      series_name='Balance'
      call grf_points(0,3,nord,plot,'Blue    ','Blue    ',2,1
     *               ,series_name)
c
      call grf_trailer(3)
c
c
      call grf_trailer(2)
c
c
      stop
c
900   continue
      write(0,200)
      write(0,*)txtc
      write(0,*)txta
      stop
c
901   continue
      write(0,201)
      write(0,*)txtc
      write(0,*)txta
      go to 1
c
902   continue
      write(0,202)date
      write(0,*)date
      stop
c
100   format(i4,a120)
101   format(a120)
102   format(a10,a9,a9,f9.2,a)
103   format('    ',a10,a9,a9,f9.2,3i6)
104   format(i2,1x,i2,1x,i4)
105   format(i4,i3,6(f8.2,',',f10.2))
106   format(/'Year Mon',9x,'CumIncome',11x,'CumSpend'
     *   13x,'Income',14x,'Spend',15x,'Nett',12x,'Balance'/)
107   format(/'Mean (',i2,' values):',f9.2,f19.2)
108   format('Day',i3,' IN: ',f10.2,',     OUT:',f10.2,', AVG_IN:',f10.2
     *      ,',  AVG_OUT:',f10.2,',  AVG_BALANCE:',F10.2)
200   format(/'Failure to read date, sortcode, or account')
201   format(/'Failure to read total')
202   format(/'Failure to read nday, nmon, or nyear from date =>'
     *        ,a,'<=')
c
      end
