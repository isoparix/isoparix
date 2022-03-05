c**********************************************************************
c   pi.f - compute pi by integrating f(x) = 4/(1 + x**2)     
c
c  (C) 2001 by Argonne National Laboratory.
c      See COPYRIGHT in top-level directory.
c     
c   Each node: 
c    1) receives the number of rectangles used in the approximation.
c    2) calculates the areas of it's rectangles.
c    3) Synchronizes for a global summation.
c   Node 0 prints the result.
c
c  Variables:
c
c    pi  the calculated result
c    n   number of points of integration.  
c    x           midpoint of each rectangle's interval
c    f           function to integrate
c    sum,pi      area of rectangles
c    tmp         temporary scratch space for global summation
c    i           do loop index
c****************************************************************************
      program main

      include 'mpif.h'

      double precision  PI25DT
      parameter        (PI25DT = 3.141592653589793238462643d0)

      double precision  mypi, pi, h, sum, x, f, a
      integer n, myid, numprocs, i, rc
c                                 function to integrate
      f(a) = 4.d0 / (1.d0 + a*a)

      call MPI_INIT( ierr )
      call MPI_COMM_RANK( MPI_COMM_WORLD, myid, ierr )
      call MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr )
      write(0,*)"Process ", myid, " of ", numprocs, " is alive"

      sizetype   = 1
      sumtype    = 2
      
 10   continue
      if ( myid .eq. 0 ) then
         write(0,*)'Enter the number of intervals: (0 quits)'
         read(5,*)n
         write(0,*)'Number of intervals= ',n
      endif

      if(n.le.0    ! Quit
     *  )then
             call MPI_FINALIZE(rc)
             write(0,*)'MPI_FINALIZE result code was',rc
             stop
      endif
      
      call MPI_BCAST(n,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
      if(myid.eq.0)write(0,*)'Broadcast complete - ierr=',ierr

c                                 calculate the interval size
      h = 1.0d0/n

      sum  = 0.0d0
      do 20 i = myid+1, n, numprocs
         x = h * (dble(i) - 0.5d0)
         sum = sum + f(x)
 20   continue
      mypi = h * sum

c                                 collect all the partial sums
      call MPI_REDUCE(mypi,pi,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,
     $     MPI_COMM_WORLD,ierr)

c                                 node 0 prints the answer.
      if (myid .eq. 0) then
         write(0, 97) pi, abs(pi - PI25DT)
 97      format('  pi is approximately: ', F18.16,
     +          '  Error is: ', F18.16)
      endif

      goto 10

      end
