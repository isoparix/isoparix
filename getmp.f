      subroutine getmp
c
c      Returns all the key stuff about the Parallel Environment
c
      use isocomm
c
      use parcomm
c
c
      check=.false.
c
      tag( 0)=' UNKNOWN            '
      tag( 1)='Slave stats transfer'
      tag( 2)='Idle slave notify   '
      tag( 3)='Spare work transfer '
      tag( 4)='Declare Master task '
      tag( 5)='Negative work       '
      tag( 6)='More work please    '
      tag( 7)='Parameter transfer  '
      tag( 8)='Final statistics    '
      tag( 9)='Coordinates, miniter'
      tag(10)='Artist start drawing'
      tag(11)='Bisector details    '
      tag(12)='Pixel xfer to artist'
      tag(13)='Request Master ID   '
      tag(14)='Window is opened    '
      tag(15)='A check message     '
      tag(16)='Immediate closedown '
      tag(17)='Send picture data   '
      tag(18)='Request picture data'
      tag(19)=' UNKNOWN            '
      tag(20)=' UNKNOWN            '
c
      if(check)then
                   txtout='GETMP: About to call MPI_INIT'
                   call statout
      endif
c
      call MPI_INIT(ierror)
c
      if(check)then
                   txtout='GETMP: About to call MPI_COMM_SIZE'
                   call statout
      endif
c
      call MPI_COMM_SIZE(MPI_COMM_WORLD,numtasks,ierror)
c
      if(check)then
                   txtout='GETMP: About to call MPI_COMM_RANK'
                   call statout
      endif
c
      call MPI_COMM_RANK(MPI_COMM_WORLD,taskid,ierror)
c
      msglow  =0
      msghi   =1000
      allocate(msgcount_in (-10:50))
      allocate(msgcount_out(-10:50))
c
      lchann=10+taskid
      write(lchann,100)msglow,msghi,taskid,numtasks
      call isoflush(lchann)
c
      check=.false.
      return
c
100   format(
     * 30x,'msglow: :',i12,/
     * 30x,'msghi   :',i12,/
     * 30x,'taskid  :',i12,/
     * 30x,'numtasks:',i12,/
     *)
101   format('GETMP MPI_Init:      IERROR=',i8)
102   format('GETMP MPI_Comm_size: IERROR=',i8)
103   format('GETMP MPI_Comm_rank: IERROR=',i8)
c
      end
