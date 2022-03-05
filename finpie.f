      program finpie
c
c      Delivers top n percent as csv entires for pie chart
c      Input is *.summary file
c
      implicit real(8)(a-h,o-z)
      real(8)income
c
      dimension amount(300),nitems(300)
c
      character(50)category(300)
      character(10)shortcat
c
      maxline=29
      nline=1
      total=0.0
      income=0.
c
  1   continue
      read(*,100,end=2)nitems(nline),amount(nline),category(nline)
      write(shortcat,104)category(nline)
c
      if(amount(nline).lt.0.0
     *  )then
             income=income+amount(nline)
             go to 1
      endif
c
      if(shortcat.eq.'ChargeCard'
     *  )then
             go to 1
      endif
c
      total=total+amount(nline)
      nline=nline+1
      go to 1
c
  2   continue
      income=-income
      surplus=income-total
      a=100./total
      remainder=total/20.0
      write(*,103)total,income,surplus
      nline=nline-1
      lineout=0
      do i=nline,1,-1
         if(nitems(i).eq.0
     *     )then
                write(*,101)trim(category(i)),amount(i),amount(i)*a
            else
                write(*,101)trim(category(i)),amount(i),amount(i)*a
     *                     ,nitems(i)
         endif
         lineout=lineout+1
         total=total-amount(i)
         if(total.lt.remainder.or.
     *      lineout.eq.maxline
     *     )then
                write(*,102)total,total*a
                exit
         endif
      enddo
c
      stop
c
100   format(i5,11x,f11.2,2x,a50)
101   format(a10,',',2f11.2,i5)
102   format( '     Other,',2f11.2)
103   format(/'Expenditure',f11.2,', Income',f11.2
     *      ,', Surplus',f11.2,/)
104   format(a10)
c
      end
