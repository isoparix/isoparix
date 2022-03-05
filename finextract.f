      program finpie
c
c      Searches for categories
c      Input is *.summary file
c
      implicit real(8)(a-h,o-z)
c
      dimension sum(60)
c
      logical key_missing
c
      character(8)search_key(60),category
c
      search_key( 1)='DGS/RPP '
      search_key( 2)='KINGSTON'
      search_key( 3)='AOL BROA'
      search_key( 4)='LEICESTE'
      search_key( 5)='MARKS AN'
      search_key( 6)='INTEREST'
      search_key( 7)='TUDOR WI'
      search_key( 8)='POST OFF'
      search_key( 9)='BT GROUP'
      search_key(10)='CASH WIT'
      search_key(11)='BENTALLS'
      search_key(12)='JOHN LEW'
      search_key(13)='CHEQUES '
      search_key(14)='Other   '
c
      open(2,file='catlist.txt',status='old',err=99)
      nkey=15
  3   continue
      read(2,103,end=4)search_key(nkey)
      nkey=nkey+1
      go to 3
  4   continue      
c
c     do mx=1,nkey
c         write(*,*)search_key(mx)
c     enddo
c
      nline=1
      sum=0.0
      total=0.0
  1   continue
      read(*,100,end=2)nitems,amount,category
      key_missing=.true.
      do n=1,nkey
         if(category.eq.search_key(n)
     *     )then
                sum(n)=amount
                key_missing=.false.
         endif
      enddo
      if(key_missing.and.amount.gt.0.0
     *  )then
             sum(14)=sum(14)+amount
      endif
      go to 1
c
  2   continue
      do n=1,nkey
         if(sum(n).ne.0.0.and.search_key(n).ne.'ChargeCa'
     *     )then
                total=total+sum(n)
                write(*,101)search_key(n),sum(n),total
         endif
      enddo
c
      stop
c
 99   continue
      write(0,102)
      stop
c
100   format(i5,11x,f11.2,2x,a8)
101   format(a8,2f11.2)
102   format('*** ERROR in finextract:  file catlist.txt not found ***')
103   format(a8)
c
      end
