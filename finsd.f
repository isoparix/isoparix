      program finsd
c
c      Searches for SD of categories
c      Input is *.summary file
c
      implicit real(8)(a-h,o-z)
c
      dimension xtemp(50)
c
      logical key_missing
c
      character(8)search_key(50),category,category_old
c
      search_key( 1)='Food_Bev'
      search_key( 2)='CHEQUES '
      search_key( 3)='CASH WIT'
      search_key( 4)='Eat_out:'
      search_key( 5)='KINGSTON'
      search_key( 6)='Utilies:'
      search_key( 7)='JOHN LEW'
      search_key( 8)='Car_cost'
      search_key( 9)='BENTALLS'
      search_key(10)='Insuranc'
      search_key(11)='DENTAL C'
      search_key(12)='DIY: eg'
      search_key(13)='Petrol:'
      search_key(14)='TUDOR WI'
      search_key(15)='BT GROUP'
      search_key(16)='Charity:'
      search_key(17)='Hotel: e'
      search_key(18)='LEICESTE'
      search_key(19)='Entertai'
      search_key(20)='HILTON H'
      search_key(21)='Computin'
      search_key(22)='INTEREST'
      search_key(23)='Scope: e'
      search_key(24)='JOSH AND'
      search_key(25)='Clothing'
      search_key(26)='DGS/RPP'
      search_key(27)='AOL BROA'
      search_key(28)='POST OFF'
      search_key(29)='SHUROPOD'
      search_key(30)='Other: '
      search_key(31)='Income: '
      
      xtemp=0.0
      nsamples=0
      category_old=''
  1   continue
      read(*,100,end=2)nitems,amount,category
  3   continue
      if(category.eq.category_old
     *  )then
             nsamples=nsamples+1
             xtemp(nsamples)=amount
         else
             sigx=0.
             sigx2=0.
             do j=1,nsamples
                sigx =sigx + xtemp(j)
                sigx2=sigx2+(xtemp(j)**2)
             enddo
c
c      Calculate means and standard deviations...
c
             xmean=sigx/float(nsamples)
             if(nsamples.gt.1
     *         )then
                    sdx=sqrt((sigx2/float(nsamples))-xmean**2)
                else
                    sdx=0
             endif
c
c     write( 8,100)
c    *nsamples,xmean,sdx,amin,amax,sigx,sdx/xmean
                 
             write(*,102)nsamples,xmean,sigx,sdx,sdx/xmean,category_old
c    *                  ,(xtemp(mx),mx=1,nsamples)
             nsamples=0
             category_old=category
             go to 3
      endif
      go to 1
c
  2   continue
      if(nsamples.gt.0
     *  )then
             write(*,102)nsamples,xmean,sigx,sdx,sdx/xmean,category_old
c    *                  ,(xtemp(mx),mx=1,nsamples)
       endif
c
      stop
c
100   format(i5,11x,f11.2,2x,a8)
101   format(a8,2f11.2)
c102   format(a8,i11,4f11.3/100(f11.3,/))
102   format(i11,4f14.4,a10)
c
      end
