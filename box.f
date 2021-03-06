      subroutine box(ix1,iy1,ix2,iy2,miniter)
c
       use isocomm
c
      use parcomm
c
      integer(4) :: itbis(5)
c
      if(miniter.lt.0
     *  )then
             write(txtout,100)ix1,iy1,ix2,iy2,miniter
             call statout
             return
      endif
c
c      Examines a box whose corners are supplied in the parameters.
c      Box is bisected, then quartered.   Any new boxes worth looking
c      at are written as a new array line.   Details of the periphery
c      are already known...
c
c      Bisect the box vertically
c
      ixh=(ix1+ix2)/2
c
c      Send bisector details to be mapped by artist on Work-in-Progress
c
      if(bisector)then
                      itbis(1)=ixh
                      itbis(2)=iy1+1
                      itbis(3)=ixh
                      itbis(4)=iy2-1
                      itbis(5)=miniter
c
             if (check
     *          )then
                     write(txtout,300)(itbis(mx),mx=1,5)
                     call statout
             endif
300   format('   BOX sending bisector:',5i6)
c
                      call MPI_send(itbis,5,MPI_INTEGER4
     *                             ,artist,msglow+11,icomm,ierror)
      endif
c
c
c      Look at the vertical bisector...
c
      call column(ixh,iy1+1,iy2-1,miniter)
c            write(txtout,101)ixh,iy1+1,iy2-1,miniter
c            call statout
c
c      Look at the left perimeter..
c
      call minit(ix1,iy1,ixh,iy2,ml)
      if(iso_mpi_term)return
c            write(txtout,102)ix1,iy1,ixh,iy2,newline
c            call statout
      if(newline)call bisbox(ix1,iy1,ixh,iy2,ml)
c
c      Look at the right perimeter...
c
      call minit(ixh,iy1,ix2,iy2,mr)
      if(iso_mpi_term)return
c            write(txtout,103)ixh,iy1,ix2,iy2,newline
c            call statout
      if(newline)call bisbox(ixh,iy1,ix2,iy2,mr)
c
      return
c
100   format('ERR in BOX: miniter<0',5i6)
101   format('BOX: Column completed',5i6)
102   format('BOX: MinitL completed',4i6,l6)
103   format('BOX: MinitR completed',4i6,l6)
c
      end
