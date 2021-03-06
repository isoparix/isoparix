      subroutine kxgen(nsamples,irc)
c
c      Generates the max allowable value for kx
c
      use dacomm
c
      dimension nc(20)
      integer (8) kxa,kxmh,kxmn,kxmax,kxmu,prime(20)
     *          ,ix,ixold,nb,nsamples
c
      irc=0
c
      prime( 1)=2
      prime( 2)=3
      prime( 3)=5
      prime( 4)=7
      prime( 5)=11
      prime( 6)=13
      prime( 7)=17
      prime( 8)=19
      prime( 9)=23
      prime(10)=29
      prime(11)=31
      prime(12)=37
      prime(13)=41
      prime(14)=43
      prime(15)=47
      prime(16)=53
      prime(17)=59
      prime(18)=61
      prime(19)=67
      prime(20)=71
c
      nprimes=20
c
c      Check that nsamples is a suitable value for random number generation,
c      and returns a good number if original was not OK
c     (Checks that nsamples is divisible only by the prime numbers above)
c
      nb=nsamples
  2   continue
      nc=0
      ix=nb
      if(ix.le.0
     *  )then
c            write(2,109)nsamples
             irc=3
             return
      endif
      do np=1,nprimes
c        write(2,104)ix,prime(np),mod(ix,prime(np))
  3      continue
         if(mod(ix,prime(np)).eq.0
     *     )then
c               write(2,105)ix,prime(np)
                ix=ix/prime(np)
                nc(np)=nc(np)+1
                go to 3   ! Try this prime again....
         endif
c
         if(ix.eq.1    ! IX has been perfectly factorised
     *     )then
                exit
         endif
c
      enddo
c
      if(ix.ne.1        ! IX has not been perfecty factorised 
     *  )then
             nb=nb-1    ! Reduce IX by 1, and try again
c            write(2,107)
             go to 2
      endif
c
c       Generate KX
c
      if(nc(1).eq.0)kx=1
      if(nc(1).eq.1)kx=2
      if(nc(1).gt.1)kx=4   ! nrange is divisble by 4 - so must kx be...
c
      do np=2,nprimes
         if(nc(np).gt.0
     *     )then
                kx=kx*prime(np)  ! KX has all the primes of nrange
         endif
      enddo
c
c       Check conditions on kx
c
      if(kx.ge.nb
     *  )then
c            write(2,106)nb,kx
             nb=nb-1
             go to 2
      endif       
c
c      Increase KX if possible, then increase it again by 1
c
      kx=1+(((nb-1)/kx)*kx)
c
      kran=nb/2   ! Seed value
      nrange=nb
c
c       Set first co-prime number
c
c     ncp=0
c     do np=nprimes,1,-1
c        if(nc(np).gt.0.and.ncp.gt.0
c    *     )then
c               exit
c        endif
c        ncp=prime(np)
c     enddo
      ncp=1
c
c          https://en.wikipedia.org/wiki/Linear_congruential_generator
c      The generator is defined by the recurrence relation:
c
c      X_{n+1} = ( a X_n + c ) mod m

c      where X is the sequence of pseudorandom values, and
c
c         m ==> nrange
c         a ==> kx
c         c ==> 1
c
c          nrange > 0       the "modulus"
c          0 < kx < nrange  the "multiplier" (or, nrange > kx)
c          0 <= c < nrange  the "increment"
c          X_0  0 <= X_0 < nrange  the "seed" or "start value"
c      
c      are integer constants that specify the generator. 
c      If c = 0, the generator is often called a multiplicative congruential 
c      generator (MCG), or Lehmer RNG. If c <> 0, the method is called a mixed 
c      congruential generator.
c
c          nrange and the offset c are relatively prime,
c          kx - 1 is divisible by all prime factors of nrange,
c          kx - 1 is divisible by 4 if nrange is divisible by 4.

c
c     write(2,100)kxmax,kx-1,nsamples,nrange,nprimes
c    *           ,(prime(mx),mx=1,nprimes)
c     write(2,101)(nc(mx),mx=1,nprimes)
c     call isoflush(2)
c
      return
c
100   format(
     *       /'   KXMAX=',i25
     *      ,/'    KX-1=',i25
     *      ,/'NSAMPLES=',i25,' ==> NRANGE=',i18
     *      ,/' NPRIMES=',i15,':',20i3)
101   format(25x,20i3)
102   format('ERROR IN KXGEN:'
     *      ,/'HUGE(KXMH)=',i25
     *      ,/'     KXMH =',i25
     *      ,/'     KXMN =',i25
     *      ,/'    KXMAX =',i25
     *      ,/' NSAMPLES =',i25
     *      )
103   format(2i25)
104   format('KXGEN: ix=',i12,', prime(np)=',i4,', mod=',i8)
105   format('KXGEN: ix=',i12,' is divisible by',i4)
106   format('ERROR IN KXGEN: KX will be >= NRANGE',2i8,/)
107   format(/)
109   format(/'Cannot find suitable range <= ',i0.0)
c
      end
