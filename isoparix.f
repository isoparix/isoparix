      program isoparix
c
      use isocomm
      use parcomm
c
c      Discover things about the Parallel Environment
c
      iso_mpi_term=.false.
      call getmp
      nsleep=2000000    ! Microseconds
c
      if(numtasks.lt.3
     *  )then
             write(0,100)taskid,numtasks
         else
c
             if(taskid.gt.1
     *         )then
                    call isoslave
             endif
c
             if(taskid.eq.1
     *         )then
                    call isomast
             endif
c
             if(taskid.eq.0
     *         )then
                    call isoartst
             endif
c
      endif
c
c      Finalise MPI for this task
c
c     write(     0,102)taskid,iso_mpi_term
c     write(     0,101)iso_mpi_term,nsleep
c     write(txtout,101)iso_mpi_term,nsleep
c     call statout
c
      if(iso_mpi_term)call mpirelease
c
      call nanopause(nsleep)    !  Sleep to let everyone catch up
      call MPI_finalize(ierror)
      stop
c
100   format('ISOPARIX TaskID',i3,':',i4,' tasks is not enough - 3 min')
101   format('ISOPARIX: iso_mpi_term',l2,', MPI_finalize in',i8,' uS')
102   format('ISOPARIX TaskID',i3,': iso_mpi_term is',l2,/)      
c
      end
