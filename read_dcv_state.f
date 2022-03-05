      subroutine read_dcv_state
c
c      Uses query_Xvn commands to read dcv state
c
      use allcomms
      use dcv_comms
c
      call system ('./querydcv')
c
c      Set all buttons UP (1) to start with
c
      updown=1
c
      open(21,form='formatted',status='unknown')
c     write(*,103)
c
      n=1
      read(21,100,end=98,err=99)rvn             ! RVN state
      if(rvn)updown(1)=-1
c
      n=2
      read(21,100,end=98,err=99)svn             ! SVN state
      if(svn
     *  )then
             updown(2)=-1
             n=3
             read(21,101,end=98,err=99)wall_file
             n=4
c
             read(21,101,end=98,err=99)transport
             if(transport.eq.'MPI'
     *         )then
                    updown(3)=-1
                    call dcv_activate(3)
             endif
c
            if(transport.eq.'OPENMPI'
     *        )then
                    updown(14)=-1
                    call dcv_activate(14)
             endif
c
            if(transport.eq.'SOCKET'
     *        )then
                    updown(8)=-1
                    call dcv_activate(8)
             endif
             n=5
             read(21,102,end=98,err=99)nsync           
             updown(4+nsync)=-1
             call dcv_activate(4+nsync)
c
             n=6
             read(21,100,end=98,err=99)disp_pointer    
             if(disp_pointer
     *         )then
                    updown(9)=-1
                    call system('./sed_svn_command pointer 1')
                else
                    updown(9)=1
                    call system('./sed_svn_command pointer 0')
             endif
c
             n=7
             read(21,100,end=98,err=99)window_selector 
             if(window_selector
     *         )then
                    updown(10)=-1
                    call system('./sed_svn_command selector 1')
                else
                    updown(10)=1
                    call system('./sed_svn_command selector 0')
             endif
c
             n=8
             read(21,100,end=98,err=99)client_render   
             if(client_render
     *         )then
                    updown(11)=-1
                    call system('./sed_svn_command clientrender 1')
                else
                    updown(11)=1
                    call system('./sed_svn_command clientrender 0')
             endif
c
c            n=9
c            read(21,101,end=98,err=99)local_address   
             n=10
             read(21,100,end=98,err=99)frame_sync      
             if(frame_sync
     *         )then
                    updown(12)=-1
                    call system('./sed_svn_command swap 1')
                else
                    updown(12)=1
                    call system('./sed_svn_command swap 0')
             endif
c
             call system('./svn_command >fort.81 2>/dev/null')
      endif
c
      close(21)
c
      do i=1,ntop_button
         if(exist_button(i)
     *     )then
                write(8,104)i,updown(i)
         endif
      enddo
c
      return
c
 98   continue
      write(0,200)n
      stop
c
 99   continue
      write(*,201)n
      stop
c

100   format(l7)
101   format(a60)
102   format(i1)
103   format('Reading DCV state')
104   format('READ_DCV_STATE: Button',i3,' updown',i3)
105   format(i2.2)
c
c
200   format('Premature end of file in dcv_state',i6)
201   format('Error reading file in dcv_state',i6)
c
      end
