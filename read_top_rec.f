      subroutine read_top_rec
c
c        Reads a topview record...
c
      use allcomms
c
        character *80 txt
        character *1 lead_1a(3)
        character *3 lead_1,cpu_id
c                    top_aspect defined in allcomms character *8
        character *8 top_user,top_nice,top_system 
     *              ,top_irq,top_softirq,top_iowait,top_idle
        character *3 devtype,devlead
c
      equivalence(lead_1,lead_1a)
c
      integer mpdummy(10)
c
c#####################################################################
c
      read_error=.false.
      eof_found=.false.
      ndev_s=0
      top_aspect='        '
c
  1   continue
c
      read(nchann,200,end=99, iostat=ios,err=98)lead_1,txt
c     write(0,700)lead_1,txt
700   format('READ_TOP_REC: lead_1,TXT is:',a3,a)
c
      if(lead_1.eq.'Cpu'
     *  )then
c
c      Read the CPU data
c
              call numstrip(txt)
Cpu0  :  3.0% us,  0.0% sy,  0.0% ni, 97.0% id,  0.0% wa,  0.0% hi,  0.0% si
              read(txt,*,err=96)id_top_cpu,cpuser_s,cpsys_s,xnice
     *                  ,cpidle_s,cpiow_s
c              write(*,104)top_aspect,cpuser_s,cpsys_s,cpiow_s,cpidle_s
              end_data=.false.
              top_type='CPU'
              write(top_aspect,111)id_top_cpu
              return
      endif
c
      if(lead_1.eq.'Mem'
     *  )then
              call numstrip(txt)
              read(txt,*)ntopav,ntopused
              top_mem_util=float(100*ntopused)/float(ntopav)
              end_data=.false.
              top_type='MEM'
              top_aspect='  memory'
              return
      endif
c
      if(lead_1.eq.'Swa'
     *  )then
              call numstrip(txt)
              read(txt,*)ntopav,ntopused
              top_swp_util=float(100*ntopused)/float(ntopav)
              end_data=.false.
              top_type='SWP'
              top_aspect='swapfile'
              read(*,112)     ! Skip next three lines....
              return
          else
c
c      If none of the above tests for lead1 are .true., then
c      this must be AIX and mpstat....
c
             if(lead_1.eq.'---'
     *         )then
                    end_data=.false.
                    top_type='SWP'
                    return
             endif
c
             if(lead_1.eq.'cpu'.or.
     *          lead_1.eq.'ALL'.or.
     *          lead_1.eq.'Sys'.or.
     *          lead_1.eq.'Tas'.or.
     *          lead_1.eq.'  P'.or.
     *          lead_1.eq.'  U'.or.
     *          lead_1.eq.'top'
     *         )then
                    go to 1
             endif
c
             read(lead_1,*,end=1)id_top_cpu
             read(txt,*,err=97)(mpdummy(mx),mx=1,10)
     *                 ,cpuser_s,cpsys_s,cpiow_s,cpidle_s

              end_data=.false.
              top_type='CPU'
              write(top_aspect,103)id_top_cpu
c             write(0,104)top_type,cpuser_s,cpsys_s,cpiow_s,cpidle_s
              return
      endif
c
      go to 1
c
c****************************************
c
c        All data read for this instance of top
c
 96   continue
c
c      Deal with line in error
c
      write(0,1022)
      write(0,*)txt
      read_error=.true.
      return
c
 97   continue
c
c      Deal with line in error
c
      write(0,1021)
      write(0,*)txt
      read_error=.true.
      return
c
 98   continue
c
c      Deal with line in error
c
      write(0,102)
      write(0,100)lead_1,txt
      read_error=.true.
      return
c
 99   continue
c
c      EOF....
c
      if(allocated(end_chann)
     *  )then
             write(0,106)nchann
             write(8,106)nchann
             call isoflush(8)
c
             end_chann(nchann)=.true.
             end_data=.true.
      endif
      top_type='EOF'
c
      return
c
100   format(a4,a)
200   format(a3,a80)
101   format('##### READ_TOP_REC: ABOUT TO READ FROM CHANNEL',i3)
102   format('##### ERROR in READ_TOP_REC:  read error in -')
103   format(i2.2)
1021  format('##### ERROR in READ_TOP_REC:  error in piped text')
1022  format('##### ERROR in READ_TOP_REC:  error reading CPU data')
104   format('READ_TOP_REC: ',a10,4f10.1)
106   format('##### READ_TOP_REC: END_OF_DATA encountered on channel'
     *      ,i4)
108   format('READ_TOP_REC: DEVTYPE=',a)
1081  format('READ_TOP_REC:  This is AIX top data')
1082  format('READ_TOP_REC:  This is RedHat top data')
109   format('Lower and upper bounds of end_chann',2i6)
111   format('cpu_',i3.3)
112   format(///)
c
        end
