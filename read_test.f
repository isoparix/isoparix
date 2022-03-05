      program read_test
c
c      Reads character file
c
      character(1),allocatable,dimension(:,:) :: picdata
c     character(1),allocatable,dimension(:)   :: picdata
c
      ixdim=32
      iydim=20
      write(*,110)ixdim,iydim
      allocate(picdata(0:ixdim-1,0:iydim-1))
c     allocate(picdata(ixdim*iydim))
      picdata='A'
c
      irc=0
      open(1,file='readtest.dat',form='formatted'
     *       ,access='stream',status='old',err=97)
      read(1,113,advance='NO',iostat=irc,eor=6,err=98)picdata
c
      if(irc.ne.0
     *  )then
             write(*,121)irc
             stop
      endif
c
      write(*,118)ichar(picdata) ! ichar needed because fsloppy-char not g95 now
      write(*,122)irc
      stop
c
  6   continue
      write(*,120)
      stop
c
 97   continue
      write(*,119)
      stop
c
 98   continue
      write(*,101)
      stop
c
100   format(24a1)
101   format('ERROR in read')
110   format('Picture dimensions are ',i0,'x',i0)
113   format(10000000a1)
c113   format(10000000(a1$))
c113   format(a)
118   format(32z3)
119   format('ERROR in open')
120   format('End of record found')
121   format('ERROR:  irc=',i8)
122   format('END:    irc=',i8)
c
      end
