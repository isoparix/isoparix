      program sieve
c
c      The sieve of Eratosthenes
c
      logical,allocatable,dimension(:) :: numbers
      logical noshow
c
      write(*,100)
      read(*,*)last_number, noshow
      allocate(numbers(1:last_number))
      numbers=.true.
      do n=2,last_number
         if(numbers(n)
     *     )then
                numbers(2*n:last_number:n)=.false.
         endif
      enddo
c
      nc=0
      do n=last_number,2,-1
         if(numbers(n)
     *     )then
                nc=nc+1
                write(*,*)n
                if(nc.eq.5.and.noshow)exit
         endif
      enddo
c
100   format('Nmax? T/F?')
c
      end
