      subroutine paint_hosts
c
c     Paint all the CPU usage of all the hosts
c
      use allcomms
      use bmp_comms
c
      character *60 tmp_char
c
c      Display the global view that has been selected by clicking on
c      the global lines...
c    *,'012345678901234567890123456789012345678901234567890123456789')
c
      n_text_line=n_cpu_attributes+3
      n_ppline=n_text_line+nh
c
      if(global_type.eq.1
     *  )then
             do nh=1,nhosts
                nh_select=nh
                tmp_char=trim(nodename(nh))
                title=
     * '----- User+system CPU on all hosts -----                    '
                call text_line(n_text_line)
                call paint_prep(allcpu_map(:,nh),nh,nh+7,tmp_char)
                bmname='User+system_CPU_on_all_hosts  '
             enddo
      endif
c
      if(global_type.eq.3
     *  )then
             do nh=1,nhosts
                nh_select=nh
                tmp_char=trim(nodename(nh))
                title=
     * '----- User CPU on all hosts                                 '
                call text_line(n_text_line)
                call paint_prep(cpuser_map(:,nh),nh,nh+7,tmp_char)
                bmname='User_CPU_on_all_hosts         '
             enddo
      endif
c
      if(global_type.eq.4
     *  )then
             do nh=1,nhosts
                nh_select=nh
                tmp_char=trim(nodename(nh))
                title=
     * '----- System CPU on all hosts -----                         '
                call text_line(n_text_line)
                call paint_prep(cpsys_map (:,nh),nh,nh+7,tmp_char)
                bmname='System_CPU_on_all_hosts       '
             enddo
      endif
c
      if(global_type.eq.5
     *  )then
             do nh=1,nhosts
                nh_select=nh
                tmp_char=trim(nodename(nh))
                title=
     * '----- I/O wait time on all hosts -----                      '
                call text_line(n_text_line)
                call paint_prep(cpiow_map (:,nh),nh,nh+7,tmp_char)
                bmname='IO_wait_time_on_all_hosts     '
             enddo
      endif
c
      if(global_type.eq.6
     *  )then
             do nh=1,nhosts
                nh_select=nh
                tmp_char=trim(nodename(nh))
                title=
     * '----- Idle time on all hosts -----                          '
                call text_line(n_text_line)
                call paint_prep(cpidle_map(:,nh),nh,nh+7,tmp_char)
                bmname='Idle_time_on_all_hosts        '
             enddo
      endif
c
      return
      end
