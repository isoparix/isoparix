      subroutine paint_all
c
c      Paint everything!!
c
      use allcomms
      use bmp_comms
c
      character (len=60) tmp_char,dskid
      character (len=15) rowtmp
      character (len=1)  rowtmpa(30)
c
      equivalence (rowtmpa,rowtmp)
c
c      Set up the columns to be painted...
c
      call column_set
c
      if(updating
     *  )then
             ix1=3+nhsp
             iy1=disp_top
             ix2=1+nhsp+ntimeslots
             iy2=line_y(nrows-1)
             ix3=ix1-1
             iy3=disp_top
c
c      Slide existing display one column to the left
c
             call x11movezone(%val(ix1),%val(iy1)
     *                        ,%val(ix2),%val(iy2)
     *                        ,%val(ix3),%val(iy3)
     *                        )
c
c      Vertical ruler has been wiped out - redraw it....
c
c     write(*,*)last_mousex,disp_top,disp_bot
             ruler=.false.
             call draw_ruler(-1,-1)
      endif
c
      if(allhosts
     *  )then
c
c     Paint all the global CPU usage of all the hosts
c
c     subroutine paint_prep(amap,nh,nrow,map_name)
c
c                  '012345678901234567890123456789012345678901234567890123456789')
             title='Above: Global CPU utilisation.  '
     *           //'Below: Individual CPU states'
             call text_line(2)
c
             nq=1	! This really does not count for global...
             tmp_char='Global total CPU'
             call paint_prep(gallcpu_map,nq,1,tmp_char)
             tmp_char='Global User'
             call paint_prep(gcpuser_map,nq,3,tmp_char)
             tmp_char='Global System'
             call paint_prep(gcpsys_map ,nq,4,tmp_char)
             tmp_char='Global IO_wait'
             call paint_prep(gcpiow_map ,nq,5,tmp_char)
             tmp_char='Global Idle'
             call paint_prep(gcpidle_map,nq,6,tmp_char)
c
             call paint_hosts
c
             lastrow=nhosts+9
        else     ! IE, NOT allhosts.....
c
c      Paint the activity of the CPU one host, averaged over all cores
c
             title='Above:  Total CPU utilisation.  '
     *           //'Below: Individual CPU states'
             call text_line(2)
c
             nq=nh_select
             tmp_char=trim(nodename(nh_select))
             call paint_prep(allcpu_map(:,nh_select),nq,1,tmp_char)
             tmp_char='User'
             call paint_prep(cpuser_map(:,nh_select),nq,3,tmp_char)
             tmp_char='System'
             call paint_prep(cpsys_map (:,nh_select),nq,4,tmp_char)
             tmp_char='IO_wait'
             call paint_prep(cpiow_map (:,nh_select),nq,5,tmp_char)
             tmp_char='Idle'
             call paint_prep(cpidle_map(:,nh_select),nq,6,tmp_char)
             rowtmp =trim(adjustl(row_name(1)))
c
c      Do we have multiple cores on nh_select?
c
c            write(*,109)nh_select,ncores(nh_select),ndisp_type
c    *                 ,ncore_view
             if(ncores(nh_select).gt.0
     *         )then
                    if(ndisp_type.eq.2
     *                )then
c
c      Paint the activity of the cores on one host
c
c
c      Do we need to repaint the core maps because it's a new core view??
c
                    if(delta_core_view
     *                )then
                           new_win=.true.
                           call column_set
                           new_win=.false.
                           delta_core_view=.false.
                    endif
                           if(ncore_view.eq.1
     *                       )then
                                  bmname=trim(adjustl(rowtmp))
     *                                  //'_CPU_user'
c
                                  title='Above: Node CPU utilisation.  '
     *                                //'Below: Core CPU User'
                                  call text_line(7)
c
                                  ncpos=8
                         do nc=nfirstcore(nh_select)
     *                        ,nfirstcore(nh_select)+ncores(nh_select)-1
               call paint_prep(coreuser_map(:,nc),nq,ncpos,corename(nc))
                             ncpos=ncpos+1
                          enddo
                           endif
c
                           if(ncore_view.eq.2
     *                       )then
                                  bmname=trim(adjustl(rowtmp))
     *                                  //'_CPU_system'
c
                                  title='Above: Node CPU utilisation.  '
     *                                //'Below: Core CPU System'
                                  call text_line(7)
c
                                  ncpos=8
                         do nc=nfirstcore(nh_select)
     *                        ,nfirstcore(nh_select)+ncores(nh_select)-1
               call paint_prep(coresys_map(:,nc),nq,ncpos,corename(nc))
                             ncpos=ncpos+1
                          enddo
                           endif
c
                           if(ncore_view.eq.3
     *                       )then
                                  bmname=trim(adjustl(rowtmp))
     *                                  //'_CPU_wait'
                                  title='Above: Node CPU utilisation.  '
     *                                //'Below: Core CPU Wait'
                                  call text_line(7)
c
                                  ncpos=8
                         do nc=nfirstcore(nh_select)
     *                        ,nfirstcore(nh_select)+ncores(nh_select)-1
               call paint_prep(coreiow_map(:,nc),nq,ncpos,corename(nc))
                             ncpos=ncpos+1
                          enddo
c
                           endif
c
                           if(ncore_view.eq.4
     *                       )then
                                  bmname=trim(adjustl(rowtmp))
     *                                  //'_CPU_idle'
c
                                  title='Above: Node CPU utilisation.  '
     *                                //'Below: Core CPU Idle'
                                  call text_line(7)
c
                                  ncpos=8
                         do nc=nfirstcore(nh_select)
     *                        ,nfirstcore(nh_select)+ncores(nh_select)-1
               call paint_prep(coreidle_map(:,nc),nq,ncpos,corename(nc))
                             ncpos=ncpos+1
                          enddo
                           endif
                    endif
             endif
c
c      Do we have any disks at all on nh_select?
c
             if(ndisks(nh_select).gt.0
     *         )then
             if(ndisp_type.eq.3
     *         )then
c
c      Paint the activity of the disks on one host
c
                    ndpos=8
                    if(rowtmpa(1).eq.'Q'
     *                )then
c
c      Eliminate the link directory characters...
c
c                          write(*,104)rowtmp
                           do mx=3,15
                              rowtmpa(mx-2)=rowtmpa(mx)
                           enddo
                           rowtmpa(14)=' ' 
                           rowtmpa(15)=' ' 
c                          write(*,104)rowtmp
                    endif
c
c      Do we need to repaint the disk maps because it's a new disk view??
c
                    if(delta_disk_view
     *                )then
                           new_win=.true.
                           call column_set
                           new_win=.false.
                           delta_disk_view=.false.
                    endif
c
                    if(ndisk_view.eq.0
     *                )then
                           bmname=trim(adjustl(rowtmp))//'_Busy'
c
                           write(title,101)
                           call text_line(7)
                           do nd=nfirstdev(nh_select)
     *                          ,nfirstdev(nh_select)
     *                             +ndisks(nh_select)-1
                             call paint_prep(dskact_map(:,nd),nq
     *                                      ,ndpos,dskname(nd))
                              ndpos=ndpos+1
                           enddo
                    endif
c
                    if(ndisk_view.eq.1
     *                )then
                           bmname=trim(adjustl(rowtmp))//'_Rates'
c
c      Do we need to repaint the disk maps because of new disk rates?
c
                           if(akbps_disk_rev(nh_select)
     *                       )then
                                  write(*,105)
                                  new_win=.true.
                                  call column_set
                           endif
c
                           write(title,100)akbps_disk_peak(nh_select)
                           call text_line(7)
                           do nd=nfirstdev(nh_select)
     *                          ,nfirstdev(nh_select)
     *                             +ndisks(nh_select)-1
                              if(akbps_disk_rev(nh_select)
     *                          )then
c
c      ...we also need to revise the disk rate maps....
c
                                     do n=1,maxtimes
                                        dskbps_map(n,nd)=
     *                                                (akbpsw_map(n,nd)
     *                                                +akbpsr_map(n,nd))
     *                                                 *rdmax(nh_select)
c     if(dskbps_map(n,nd).gt.100.
c    *  )then
c            write(0,200)n,nd,nh_select,dskbps_map(n,nd)
c     endif
200   format('REVISION ERROR: dskbps_map:',3i6,f10.3)
                                     enddo
                              endif
                              call paint_prep(dskbps_map(:,nd),nq
     *                                        ,ndpos,dskname(nd))
                              ndpos=ndpos+1
                           enddo
                           akbps_disk_rev(nh_select)=.false.
                    endif
c
                    if(ndisk_view.eq.2
     *                )then
                           bmname=trim(adjustl(rowtmp))//'_KB_Txn'
c
c      Do we need to repaint the KB/txn maps because of new values?
c
                           if(akbps_txn_rev(nh_select)
     *                       )then
                                  write(*,106)
                                  new_win=.true.
                                  call column_set
                           endif
c
                           write(title,103)akbps_txn_peak(nh_select)
                           call text_line(7)
                           do nd=nfirstdev(nh_select)
     *                          ,nfirstdev(nh_select)
     *                             +ndisks(nh_select)-1
                              if(akbps_txn_rev(nh_select)
     *                          )then
c
c      ...we also need to revise the txn rate maps....
c
                                     do n=1,maxtimes
                                        if(atps_map(n,nd).gt.0.
     *                                    )then
                                               akbpstxn_map(n,nd)=
     *                                                (akbpsw_map(n,nd)
     *                                                +akbpsr_map(n,nd))
     *                                                 *rrmax(nh_select)
     *                                                  /atps_map(n,nd)
                                           else
                                               akbpstxn_map(n,nd)=0
                                        endif
c     if(akbpstxn_map(n,nd).gt.100.
c    *  )then
c            write(0,201)n,nd,nh_select,akbpstxn_map(n,nd)
c     endif
201   format('REVISION ERROR: akpbstxn_map:',3i6,f10.3)
                                     enddo
                              endif
                              call paint_prep(akbpstxn_map(:,nd),nq
     *                                        ,ndpos,dskname(nd))
                              ndpos=ndpos+1
                           enddo
                           akbps_txn_rev(nh_select)=.false.
                    endif
c
                    lastrow=ndisks(nh_select)+9
                else
                    lastrow=8
             endif     ! End  of (ndisks(nh_select).gt.0
           else
               bmname=trim(adjustl(rowtmp))
c              write(*,107)nh_select,ndisks(nh_select),ndisp_type
      endif            ! From 'no-disks' test...
      endif
c
      ix1=nhsp+nfirstcol
      ix2=nhsp+nendcol+1
      call x11updatezone(%val(ix1),%val(0)
     *                   ,%val(ix2),%val(iym))
c
      fullmap=.false.
      new_win=.false.
      call column_set
c
c      Paint enclosing vertical bars
c
c      Draw the left-most vertical bars
c
      call x11bisect(%val(0),%val(0)
     *              ,%val(0),%val(iym)
     *              ,%val(nwhite))
c
      call title_rates
c     write(*,108)bmname
      return
c
100   format('Device transfer rates - current max:'
     *      ,f10.1,' kilobytes/S')
101   format('Devices - percentage active')
102   format('PAINT_ALL: Disk',i4)
103   format('Kilobytes/transaction - current max:',f10.1)
104   format('PAINT_ALL: Row name is ',a)
105   format('PAINT_ALL: New disk rates')
106   format('PAINT_ALL: New txn rates')
107   format('PAINT_ALL: Host:',i3,', Ndisks:',i4,', Display_type:',i2)
108   format('PAINT_ALL: BMNAME is ',a)
109   format('PAINT_ALL: Host:',i3,', Ncores:',i4,', Display_type:',i2
     *      ,', ncore_view:',i4)
c
      end
