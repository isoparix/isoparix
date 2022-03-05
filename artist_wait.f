      subroutine artist_wait
c
       use isocomm
c
      use parcomm
c
c      Wait for a message from the Artist, saying it's done its 
c      graphics stuff, if needed.
c
      if(check
     *  )then
             txtout='Waiting for Artist to open the window, etc'
             call statout
      endif
c
      ntag  =MPI_ANY_TAG
      source=MPI_ANY_SOURCE
  1   continue
c
      call MPI_recv(msgart,1,MPI_INTEGER4
     *              ,source,ntag,icomm,istatus,ierror)
      call lts(nbytes)
      if(mpitype.ne.msglow+14
     *  )then
             if(check
     *         )then
                    write(txtout,100)ntag
                    call statout
             endif
             if(mpitype.eq.msglow+16
     *         )then   !.....this is a panic shutdown....
                     iso_mpi_term=.true.
                     return
             endif        
             go to 1
      endif
c
100   format('Waiting for Artist_ready msg, not type',i4)
c
      return
      end
