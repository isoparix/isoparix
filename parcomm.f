      module parcomm
c
c              
      include 'mpif.h'
c     parameter (MPI_STATUS_SIZE=30)
c     integer istatus(MPI_STATUS_SIZE)
      parameter(icomm=MPI_COMM_WORLD)
      integer istatus(100)
c
      end
