      subroutine button_action(nbutton)
c
c      Carries out the action of the button
c
      use allcomms
c
      character *256 action_text
c
      if(nbutton.eq.1
     *  )then
             call system('./key_act_colstat.01 &')
             return
      endif
c
      if(nbutton.eq.2
     *  )then
             call makemap(0,0,ixm,iym)
c        call system('mv '//trim(adjustl(bmname))//'.bmp .')
             return
      endif
c
      if(nbutton.eq.3
     *  )then
             write(8,106) 
             call isoflush(8) 
             stop
             return
      endif
c
      if(nbutton.eq.4
     *  )then
             call system('./key_act_colstat.04 &')
             return
      endif
c
      if(nbutton.eq.5
     *  )then
             call system('./key_act_colstat.05 &')
             return
      endif
c
      if(nbutton.eq.6
     *  )then
             write(action_text,104)trim(ssh_node)
             write(*,103)trim(action_text)
             call system(action_text)
 
             write(action_text,105)trim(ssh_node)
             write(*,103)trim(action_text)
             call system(action_text)
c
             call system
     *           ('./wintext.exe /tmp/junk "User jobs - click to end"&')
             return
      endif
c
      if(nbutton.eq.7
     *  )then
             call system('./key_act_colstat.07 &')
             return
      endif
c
      if(nbutton.eq.8
     *  )then
             call system('./key_act_colstat.08 &')
             return
      endif
c
      if(nbutton.eq.9
     *  )then
             call system('./key_act_colstat.09 &')
             return
      endif
c
      if(nbutton.eq.10
     *  )then
             call system('./key_act_colstat.10 &')
             return
      endif
c
      if(nbutton.eq.11
     *  )then
             call system('./key_act_colstat.11 &')
             return
      endif
c
      if(nbutton.eq.12
     *  )then
             open(30,file='view.status', status='unknown')
             write(30,101)type_view,nhosts
             if(type_view.eq.'STATVIEW'
     *         )then
                     write(30,102)(ndisks(mx)
     *                            ,trim(nodename(mx)),mx=1,nhosts)
                else
                     write(30,103)(trim(nodename(mx)),mx=1,nhosts)
             endif
             close(30)
             call system
     *          ('./wintext.exe view.status "Status - click to end" &')
             return
      endif
c
      write(*,100)
      write(0,100)
c
      return
c
100   format('***** ERROR in ACTION_BUTTON: Invalid button ID')
101   format(/a8,' is monitoring',i4,' hosts',/)
102   format('There are',i4,' disks on node ',a)
103   format(a)
104   format('echo User jobs from ',a,' > /tmp/junk')
105   format('ssh ',a,' ps -ef | grep -v "     1  0 "'
     *      ,' | grep `whoami` >> /tmp/junk')
106   format('END-OF-PROGRAM requested')
      end
