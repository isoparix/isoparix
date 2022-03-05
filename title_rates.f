      subroutine title_rates
c
c      Display the data rates in the title line
c
      use allcomms
c
      parameter (len_datarates=200)
      character (len=len_datarates) datarates
c
      lentext=len_datarates
      datarates=' '
c
c      Work out correspondence with data
c
      if(updating
     *  )then
             mapslot=lastcol-ixm+2+last_mousex
             if(mapslot.le.0
     *         )then
                    mapslot=mapslot+ntimeslots
             endif
         else
             mapslot=last_mousex-nhsp
      endif
c
      if(  mapslot.le.0       .or.
     *     mapslot.gt.maxtimes
     *  )then
             return
      endif
c
      if(nh_select.lt.0       .or.
     *   nh_select.gt.maxhosts.or.
     *   nrow_ruler.le.0      .or.
     *   nrow_ruler.gt.maxrows
     *  )then
             write(0,101)mapslot,maxtimes,nh_select,maxhosts
     *                  ,nrow_ruler,maxrows
     *                  ,lastcol,last_mousex,ntimeslots,nhsp
c            call flush(0)
             return
      endif
c
c     write(*,110)nh_select,trim(nodename(nh_select)),trim(job_node)
      kd=0
      if(nrow_ruler.gt.7
     *  )then
             if(ndisp_type.eq.3
     *         )then
                    nd=nfirstdev(nh_select)+nrow_ruler-8
c             write(*,*)nh_select,nrow_ruler,nfirstdev(nh_select),nd
                    kq=.5+ dskact_map(mapslot,nd)
                    ak=    akbpsr_map(mapslot,nd)
     *                    +akbpsw_map(mapslot,nd)
                    ka=.5+ akbpsr_map(mapslot,nd)
                    kb=.5+ akbpsw_map(mapslot,nd)
                    kc=.5+ ak
                    if(kc.gt.0
     *                )then
                           kd=.5+(ak/atps_map(mapslot,nd))
c                          write(*,10311)nd,nh_select,tod(mapslot)
c    *                                 ,trim(row_name(nrow_ruler))
c    *                                 ,hist_map(mapslot,nrow_ruler)
c    *                                 ,ka,kb,kc,kd
c    *                                 ,atps_map(mapslot,nd)
                    endif
c
                else
                    kq=.5+    hist_map(mapslot,nrow_ruler)
                    ka=.5+ akbpsrt_map(mapslot,nh_select)
                    kb=.5+ akbpswt_map(mapslot,nh_select)
                    kc=.5+ akbpsrt_map(mapslot,nh_select)
     *                    +akbpswt_map(mapslot,nh_select)
             endif
         else
             kq=.5+    hist_map(mapslot,nrow_ruler)
             if(ndisp_type.eq.3
     *         )then
                    ka=.5+ akbpsrt_map(mapslot,nh_select)
                    kb=.5+ akbpswt_map(mapslot,nh_select)
                    kc=.5+ akbpsrt_map(mapslot,nh_select)
     *                    +akbpswt_map(mapslot,nh_select)
                else
                    ka=.5+ gkbpsrt_map(mapslot)
                    kb=.5+ gkbpswt_map(mapslot)
                    kc=.5+ gkbpsrt_map(mapslot)
     *                    +gkbpswt_map(mapslot)
             endif
      endif
c
c     write(0,*)mapslot,nrow_ruler
      call isoflush(0)
      if(nrow_ruler.lt.1)nrow_ruler=1
c
      if(nrow_ruler.ne.nrow_ruler_old
     *  )then
c
c      We've moved out of a row, so reset it to normal colours...
c
             if(nrow_ruler_old.gt.0
     *         )then
                    if(inverted_name(nrow_ruler_old)
     *                )then
                           call x11updatezone(%val(1)     ,%val(invy1)
     *                                        ,%val(nhsp-1),%val(invy2))
                           inverted_name(nrow_ruler_old)=.false.
c                   write(*,107)1,invy1,nhsp-1,invy2
c    *                                ,nrow_ruler_old
                    endif
             endif
c
             invy1=line_y(nrow_ruler)  +1
             invy2=line_y(nrow_ruler+1)-1
             nrow_ruler_old=nrow_ruler
             if(len_trim(row_name(nrow_ruler)).gt.0
     *         )then
                    call x11invertzone(%val(1)     ,%val(invy1)
     *                                 ,%val(nhsp-1),%val(invy2))
                    inverted_name(nrow_ruler)=.true.
                    call draw_ruler(invy1,invy2)
c                   write(*,106)1,invy1,nhsp-1,invy2,nrow_ruler
             endif
      endif
c
      if(len_trim(row_name(nrow_ruler)).gt.0
     *  )then
c
             if(kd.gt.0
     *         )then
                    write(datarates,1031)tod(mapslot)
     *                          ,trim(row_name(nrow_ruler))
     *                          ,kq,ka,kb,kc,kd
                else
                    write(datarates,103)tod(mapslot)
     *                          ,trim(row_name(nrow_ruler))
     *                          ,kq,ka,kb,kc
             endif
      endif
c
c     if(ruler
c    *  )then
             iy1=line_y(nrows-1)+1
             iy2=line_y(nrows)-1
             iytxt=((iy1+iy2)/2)+4
c
c      Blank out the name space first, then write it.   Check for 
c      non-contributing host...
c
             lentext=len_trim(datarates)
c            write(*,108)lentext,lentext*8
             ixtxt1=(ixm-(8*lentext))/2
c
             if(ixtxt1.lt.1
     *         )then
                    ixtxt1=1
             endif
c
             if(ixtxt1.lt.ixtxt1min
     *         )then
                    ixtxt1min=ixtxt1
             endif
c
             ixtxt2=(ixtxt1min+(8*lentext))
             call x11rectgc(%val(    0),%val(iy1)
     *                     ,%val(ixm-1),%val(iy2)
     *                     ,%val(nblack))
             call x11updatezone(%val(   0),%val(iy1)
     *                         ,%val(ixm-1),%val(iy2))
             if(.not.no_data(mapslot,nh_select).and.
     *          last_mousex.le.nendcol+nhsp              .and.
     *          last_mousex.gt.nhsp                      .and.
     *          row_name(nrow_ruler).ne.'               '.and.
     *          row_name(nrow_ruler).ne.'Calibration    '
     *         )then
c             write(*,104)last_mousex,nhsp,mapslot,ka,kb,kc,tod(mapslot)
                    call x11text(%val(ixtxt1min),%val(iytxt)
     *                          ,datarates,%val(0),%val(lentext))
c               else
c                   write(*,1041)mousex,nendcol,nhsp,mapslot,nh_select
c    *                          ,no_data(mapslot,nh_select)
c    *                          ,nrow_ruler
c    *                          ,row_name(nrow_ruler)
             endif
             call x11updatezone(%val(ixtxt1min),%val(iy1)
     *                         ,%val(ixtxt2),%val(iy2))
c     endif
c
      return
c
101   format('***** ERROR IN TITLE_RATES:    MAPSLOT=',i8,' Max=',i8
     *     , ', MIN=1'
     *     ,/'                             NH_SELECT=',i8,' Max=',i8
     *     , ', MIN=1'
     *     ,/'                            NROW_RULER=',i8,' Max=',i8
     *     , ', MIN=1'
     *     ,/'LASTCOL=',i8,', LAST_MOUSEX=',i8,', NTIMESLOTS=',i8
     *     , ', NHSP=',i8
     *     ,/)
102   format('Clearing text in',4i6)
103   format(a15,1x,a,i4,'%: Read:',i7,' KB/S,  Write:'
     *      ,i7,' KB/S,  Total',i9,' KB/S')
10311 format(2i4,1x,a15,1x,a,i4,'%: Read:',i7,' KB/S,  Write:'
     *      ,i7,' KB/S,  Total',i9,' KB/S',i15,' KB/txn',f12.1,' TPS')
1031  format(a15,1x,a,i4,'%: Read:',i7,' KB/S,  Write:'
     *      ,i7,' KB/S,  Total',i9,' KB/S',i4,' KB/txn')
104   format('LAST_MOUSEX=',i5,', NHSP=',i3,', MAPSLOT=',i5,3i8,' ',a15)
1041  format('    MOUSEX=',i5,',   NENDCOL=',i5,',    NHSP=',i3
     *     ,/'   MAPSLOT=',i5,', NH_SELECT=',i5,', NO_DATA=',l2
     *     ,/'NROW_RULER=',i3,',  ROW_NAME=',a15)
105   format(a
     *     ,/'NDISP_TYPE',i2,', MAPSLOT',i3,', NH_SELECT',i3
     *     , ', NROW_RULER-7',i6)
106   format('TITLE_RATES: Inverting rectangle',4i5,', row',i3)
107   format('TITLE_RATES: Resetting rectangle',4i5,', row',i3)
108   format('Text length:',i5,' characters,',i5,' pixels')
109   format('TITLE_RATES: Got this far....',i3)
110   format('TITLE_RATES: NH_SELECT, NODENAME, JOB_NODE',i4,2(' ',a))
c
      end
