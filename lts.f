      subroutine lts(nbytes)
c
c      Returns the length in bytes of a message, its source, and its
c      type
c
      use isocomm
      use parcomm
c
      mpitype=istatus(MPI_TAG)
      source =istatus(MPI_SOURCE)
      call MPI_get_count(istatus,MPI_BYTE,nbytes,ierror)
c
      if(check
     *  )then
c            write(lchann,101)MPI_COMM_WORLD,MPI_STATUS_SIZE,istatus
c    *                       ,MPI_TAG,MPI_SOURCE,MPI_BYTE,source,mpitype
c            write(     0,101)MPI_COMM_WORLD,MPI_STATUS_SIZE,istatus
c    *                       ,MPI_TAG,MPI_SOURCE,MPI_BYTE,source,mpitype
c            write(0     ,100)mpitype,trim(tag(mpitype))
c    *                       ,source,nbytes,ierror
             write(txtout,100)mpitype,trim(tag(mpitype))
     *                       ,source,nbytes,ierror
             call statout
      endif
c
100   format(i2,' ',a,' Src',i3,' Len',i12,' Err',i3)
101   format('LTS:  MPI_COMM_WORLD=',i12
     *     ,/'     MPI_STATUS_SIZE=',i12
     *     ,/'             istatus=',4i12
     *     ,/'             MPI_TAG=',i12
     *     ,/'          MPI_SOURCE=',i12
     *     ,/'            MPI_BYTE=',i12
     *     ,/'              source=',i12
     *     ,/'             mpitype=',i12
     *     ,/)
c
      return
      end
