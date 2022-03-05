      subroutine mpiwindup
c
c      Finalises MPI for this task
c
      use isocomm
      use parcomm
c
      write(txtout,100)
      call statout
c
      ierror=0
      call MPI_finalize(ierror)
c     write(txtout,101)ierror
c     call statout
      return
c
100   format('##### Finalising MPI #####')
101   format('This is MPIWINDUP:  IERROR is',i8)
c
      end
