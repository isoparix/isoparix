      subroutine mpirelease
c
c      Sends EOJ message to all other tasks
c
      use isocomm
      use parcomm
c
      ntag=msglow+16
      do i=0,numtasks-1
         if(i.ne.taskid
     *     )then
                write(txtout,102)taskid,i
                call statout
                call MPI_send(ntag,1,MPI_INTEGER4,i,ntag,icomm,ierror)
         endif
      enddo
      return
c
102   format('TaskID',i4,' sending release message to task',i4)
c
      end
