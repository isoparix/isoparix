      subroutine grafout
c
c      Sends all valid graphics lines to the artist
c      and a record of the number of pels to the master.
c
      use isocomm
      use parcomm
c
c     nbytes=ngline*2
      call MPI_send(npels,1,MPI_INTEGER4
     *              ,master,msglow+1,icomm,ierror)
c
      ngrafout(1)=npels
      nelements=ngline
      if(check)then
                   write(txtout,100)npels,nelements
                   call statout
      endif
      call MPI_send(ngrafout,nelements,MPI_INTEGER4
     *              ,artist,msglow+12,icomm,ierror)
      ngline=1    !  Was 2
      npels=0
      return
100   format('Sending',i10,' pels in',i10,' elements ')
      end
