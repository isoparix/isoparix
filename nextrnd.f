      integer (8) function nextrnd()
c
c      Generates the next random number
c      (a multiplicative congruential random number generator)
c
c      This function returns every number in the range 1 to nsamples
c      in a random distribution, so long as nsamples contains no prime
c      factors other than those that make up (kx-1)/2.
c
c      So for instance, if kx=421, then (kx-1)/2=210 is made up of prime factors
c      2 x 3 x 5 x 7.   This means that if nsamples = 1000, every number
c      range from 1 to 1000 will be produced by this function before it
c      to the initial value set for kran, because 1000 factorises to no
c      primes other than 2 and 5.
c
c      However, if nsamples = 2 x 3 x 5 x 7 x 11 = 2310, every number
c      will NOT be hit, but only small repeating subsets.
c
c      Call subroutine kxgen first, to choose a good value for kx
c
      use dacomm
c
      integer(8)nran
c
      nran=ncp+(kran*kx)      ! Co-prime plus (last in series * KX)
      kran=mod(nran,nrange)
      nextrnd=kran
c
100   format('ERROR IN NEXTRND: nrange=',i14,', kx=',i14
     *      ,', kran=',i14,', nran/kx=',i14)
c
      return
      end
