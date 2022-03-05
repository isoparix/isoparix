      program test_mpi
c
      use isocomm
      use parcomm
c
c      Discover things about the Parallel Environment
c
      logical msgready
c
      real (8) rmsg,qmsg(40)
c
      nmsg=0
      check=.false.
      call getmp
      lchann=10+taskid
c
      do i=1,40
         qmsg(i)=i
      enddo
c
      if(taskid.eq.0
     *  )then
             role='Master'
c            do n=1,2
                write(txtout,107)taskid 
                call statout
                call system('sleep 1')
                do itask=1,numtasks-1
c
c      Send out a real (8) message
c
                   write(txtout,119)itask
                   call statout
                   call MPI_send(qmsg,40,MPI_REAL8,itask,7,icomm,ierror)
c
       if(ierror.ne.0
     *   )then
              write(txtout,113)taskid,7,ierror
              call statout
       endif
c
                   write(txtout,112)itask
                   call statout
                enddo
c            enddo
         else
             role='Slave'
c
c      Use blocking probe to wait for a message
c
             write(txtout,102)
             call statout
             call MPI_probe(MPI_ANY_SOURCE,MPI_ANY_TAG,icomm,istatus
     *                      ,ierror)
c
       if(ierror.ne.0
     *   )then
              write(txtout,113)taskid,0,ierror
              call statout
       endif
c
             write(txtout,104)
             call statout
             call MPI_recv(qmsg,40,MPI_REAL8,0,7,icomm,istatus,ierror)
c
       if(ierror.ne.0
     *   )then
              write(txtout,113)taskid,7,ierror
              call statout
       endif
c
             n=0
             write(txtout,110)MPI_ANY_SOURCE,MPI_ANY_TAG
             call statout
c
c      Use non-blocking probe and a sleep to wait for a message
c
  1          continue
             write(txtout,103)
             call statout
             call MPI_iprobe(MPI_ANY_SOURCE,MPI_ANY_TAG,icomm,msgready
     *                       ,istatus,ierror)
c
       if(ierror.ne.0
     *   )then
              write(txtout,113)taskid,0,ierror
              call statout
       endif
c
c
c      Above statement used to fail - had to specify source explicitly
c      Fixed by release 5.4.0-2
c
             if(msgready
     *         )then
                    write(txtout,105)
                    call statout
                    write(txtout,108)n
                    call statout
                else
                    n=n+1
                    call system('sleep 1')
                    if(mod(n,10).eq.0
     *                )then
                           write(txtout,109)n
                           call statout
                    endif
                    go to 1
             endif
      endif
c
c      Send out a real (8) message, to all tasks (except this one).
c
      do itask=0,numtasks-1
         if(itask.ne.taskid
     *     )then    
                write(txtout,119)itask
                call statout
                call MPI_send(qmsg,40,MPI_REAL8,itask,8,icomm,ierror)
c
                if(ierror.ne.0
     *            )then
                       write(txtout,113)taskid,8,ierror
                       call statout
                endif
c
                write(txtout,112)itask
                call statout
         endif
      enddo
c
c      ...and do a blocking receive waiting for it..
c
  2   continue
      call MPI_recv(qmsg,40,MPI_REAL8,MPI_ANY_SOURCE,8,icomm
     *              ,istatus,ierror)
c
       if(ierror.ne.0
     *   )then
              write(txtout,113)taskid,8,ierror
              call statout
       endif
c
      nmsg=nmsg+1
      call lts(nbytes)
      write(txtout,111)nmsg,source
      call statout
      if(nmsg.lt.numtasks-1
     *  )then
             go to 2
      endif
c
      if(taskid.ne.0
     *  )then
c
c      Say this slave has finished...
c
             write(txtout,119)0
             call statout
             call MPI_send(qmsg,40,MPI_REAL8,0,15,icomm,ierror)
             write(txtout,114)
             call statout
c
c      This slave waits for terminator message type 16
c
             call MPI_recv(qmsg,40,MPI_REAL8,MPI_ANY_SOURCE,16,icomm
     *                    ,istatus,ierror)
             write(txtout,118)
             call statout
         else
             mx=numtasks-1
 16          continue
             call MPI_recv(qmsg,40,MPI_REAL8,MPI_ANY_SOURCE,15,icomm
     *                    ,istatus,ierror)
             write(txtout,117)
             call statout
             mx=mx-1
             if(mx.gt.0
     *         )then
                    go to 16
             endif
             call system('sleep 1')
c
c      Send terminator message type 16
c
             do itask=1,numtasks-1
                write(txtout,119)itask
                call statout
                call MPI_send(qmsg,40,MPI_REAL8,itask,16,icomm,ierror)
             enddo
c
             write(txtout,116)
             call statout
      endif
c
      call mpiwindup
c
100   format('Task 0 sent message to probe at task',i2)
101   format('Task 0 sent message to non-blocking probe at task',i2)
102   format('Waiting for message to blocking probe')
103   format('Waiting for message to non-blocking probe')
104   format('   Received message to blocking probe')
105   format('   Received message to non-blocking probe')
106   format('About to send a message from task',i2)
107   format('Task',i2,' about to sleep')
108   format('     Ending sleep with non-blocking probe count=',i4)
109   format('                       Non-blocking probe count=',i4)
110   format('MPI_ANY_SOURCE, MPI_ANY_TAG:', 2i5)
111   format('Recd message',i9,' via blocking receive from task',i3)
112   format('         Sent message to task',i2)
113   format('ERROR in task',i4,', tag',i3,', IERROR=',i12)
114   format('Sent <completed> message')
116   format('Sent <terminator> messages')
117   format('Recd <completed>  msg via blocking receive')
118   format('Recd <terminator> msg via blocking receive')
119   format('About to send message to task',i2)
      end
