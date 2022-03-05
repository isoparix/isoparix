      subroutine bisbox(ix1,iy1,ix2,iy2,miniter)
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
      endif
100   format('BISBOX:',5i8)
c
c      Looks at half the box, and prepares new boxes, if needed..
c
      iyh=(iy1+iy2)/2
c
c      Send bisector details to be mapped by artist on Work-in-Progress
c
      if(bisector
     *  )then
             itbis(1)=ix1+1
             itbis(2)=iyh
             itbis(3)=ix2-1
             itbis(4)=iyh
             itbis(5)=miniter
c
             if (check
     *          )then
                     write(txtout,300)(itbis(mx),mx=1,5)
                     call statout
             endif
300   format('BISBOX sending bisector:',5i6)
c
             call MPI_send(itbis,5,MPI_INTEGER4
     *                    ,artist,msglow+11,icomm,ierror)
      endif
c
c      Look at horizontal bisector....
c
      call liner(iyh,ix1+1,ix2-1,miniter)
c
c      Look at the top box....
c
c                  write(txtout,104)ix1,iy1,ix2,iyh
c            call statout
      call minit(ix1,iy1,ix2,iyh,mt)
      if(iso_mpi_term)return
c                  write(txtout,102)ix1,iy1,ix2,iy2,mt,newline
c            call statout
      if(newline
     *  )then
             call linout(ix1,iy1,ix2,iyh,mt)
      endif       
c
c      Look at the bottom box....
c
c                  write(txtout,105)ix1,iyh,ix2,iy2
c            call statout
      call minit(ix1,iyh,ix2,iy2,mb)
      if(iso_mpi_term)return
c                  write(txtout,103)ix1,iyh,ix2,iy2,mb,newline
c            call statout
      if(newline)call linout(ix1,iyh,ix2,iy2,mb)
c
      return
102   format('MinitT completed',5i6,l6)
103   format('MinitB completed',5i6,l6)
104   format('BIS: MinitT starting ',5i6)
105   format('BIS: MinitB starting ',5i6)
      end
