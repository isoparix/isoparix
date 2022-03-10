      module allcomms
c
      parameter (max_buttons=50)
c
      integer(4) ntop_button
c
c #####################################################################
c #                                                                   #
c #        HTML info                                                  #
c #                                                                   #
c #####################################################################
c
      integer(4)n_htm_refs,list_depth
c
c #####################################################################
c #                                                                   #
c #        Formerly diskcomm.h                                        #
c #                                                                   #
c #####################################################################
c
      logical read_error,eof_found,aix,redhat
     *       ,delta_disk_view,delta_core_view
c
      real (4) tin_s,tout_s
     *        ,cpuser_s,    cpsys_s,    cpidle_s,    cpiow_s
     *        ,cpuser_s_all,cpsys_s_all,cpidle_s_all,cpiow_s_all
     *        ,aperiod,ahi,alo,apermax,apermin
     *        ,top_mem_util,top_swp_util
c
      integer intsecs,ncpu
c
      character (8) top_aspect
      character (3) top_type
c
c #####################################################################
c
c      These arrays are dimensioned for maxdev_s, the number of 
c      disks per host
c
c #####################################################################
c
      real (4), allocatable,dimension (:) :: akbps_s,rkbps_s,wkbps_s
     *                                      ,tps_s,tmact_s
      integer,  allocatable,dimension (:) :: kbread_s,kbwrtn_s
c
      character (15), allocatable, dimension (:) :: dev
c
c #####################################################################
c
c      These arrays are dimensioned for maxcores_s, the number of 
c      cores per host
c
c #####################################################################
c
      real (4), allocatable,dimension (:) :: coreuser_s,coresys_s
     *                                      ,coreidle_s,coreiow_s
c
      character (15), allocatable, dimension (:) :: core
c
c #####################################################################
c #                                                                   #
c #        Formerly iostat.f, vmstat.f, netstat.f                     #
c #                                                                   #
c #####################################################################
c
c #####################################################################
c
c      These arrays dimensioned at nqsamp,maxdev_s
c 
c #####################################################################
c
      real (4), allocatable,dimension(:,:) :: akbps,tps,tmact
      integer,  allocatable,dimension(:,:) :: kbread,kbwrtn,zone_butt

c
c #####################################################################
c #                                                                   #
c #        Formerly iocomm.h                                          #
c #                                                                   #
c #####################################################################
c
      REAL(4) tainit,xbox,ybox
c 
c #####################################################################
c
c      These arrays dimensioned at 0:nqsamp
c 
c #####################################################################
c
      real (4),allocatable,dimension (:) :: period0,t,xdata,ydata,zdata
     *            ,r,b,avm,fre,re,pi,po,fr,sr,cy,in,syf,cs,us,syc,id,wa
     *            ,tin,tout,cpuser  ,cpsys  ,cpidle  ,cpiow,  x,y
     *                     ,coreuser,coresys,coreidle,coreiow
     *            ,priminpak,priminerr,primexpak,primexerr,primcoll 
     *            ,totinpak,totinerr,totexpak,totexerr,totcoll 
      integer, allocatable,dimension (:) ::           idata,jdata,kdata
c 
c #####################################################################
c
c      These arrays dimensioned at 0:86400
c 
c #####################################################################
c
      real (4),  allocatable,dimension(:) :: util,utilmax,utilmin
      real (4),  allocatable,dimension(:) :: mapmin,map,mapmax,mapsum
      integer(2),allocatable,dimension(:) :: jutl,kutl
      integer(4),allocatable,dimension(:) :: itmap
c
      REAL (8) AMIN,AMAX,MEAN,DELTA,average,stddev
     *        ,mux,muy,sdx,sdy,x_max,x_min
c
      CHARACTER (100) SYSOLD
      CHARACTER (8) SUBNAM,avgtype
      CHARACTER (50) ERRTIT,VERSION
      CHARACTER (60) TITLE
      character (30) xname,yname
C
      integer ITPEAK,LIMDAY,IDSTART
     *       ,KBLKMAX,KBLKMIN,LPN,maxta,navg,nq,nline,lsqcount,NUMJOB
     *       ,KCELLS,LBARS,KCPU,LCPU,LPAR,nitems
     *       ,MDAYCUR,NDAYF,NDAYL,MSMIN,NTFIRST,NTLAST,NUTIL
     *       ,MY,MD,NHRS,MINS,NSECS,khist(0:14)
c
c #####################################################################
c #                                                                   #
c #        Formerly statcomm.h                                        #
c #                                                                   #
c #####################################################################
c
      integer ixbutton(max_buttons),ixabutton(max_buttons)
     *       ,iybutton(max_buttons)
     *     ,butt_col(2,max_buttons)  ,updown(max_buttons)
      character (30) button_text(2,max_buttons)
      logical exist_button(max_buttons)
c
      integer kbutton,button_line,n_pressed
     *       ,disp_top,disp_bot,disp_left,disp_right,butt_height
     *       ,ihsepbutt,ivsepbutt,leftbutt
     *       ,font_height,font_width,global_type
     *       ,minchann,maxchann,nchann,node_font
     *       ,ndy1,ndy2
c
      logical end_data,end_prog,allhosts,alldisks,numbers
     *        ,button_press,updating,over_button
     *        ,new_win,ruler,test_stat,fullmap
     *        ,open_window,help_displayed,catch_up
c
      character (60) hostname
      character (3) data_source		!top for topview, ios for iostat
      character (len=200) infomsg
      character (len=94) oldtitle,tmptitle
      character (len=35) title_header
      character (len=8000) disk_activity
      character (7) version_date
      character (8) type_view
c
c
c #####################################################################
c
c      These arrays are dimensioned 0:maxtimes
c
c #####################################################################
c
      real(4),       allocatable,dimension (:) ::
     *          calibn_map,gallcpu_map,gcpuser_map,gcpsys_map,gcpiow_map
     *         ,gcpidle_map,gkbpsrt_map,gkbpswt_map
      logical,       allocatable,dimension (:) :: ncol_delta
      character (15),allocatable,dimension (:) :: tod
      integer,       allocatable,dimension (:) ::
     *          ncol_map,number_map,line_times 
c
c
c #####################################################################
c
c      These arrays are dimensioned 0:maxtimes,maxrows
c
c #####################################################################
c
      integer,allocatable,dimension (:,:) :: hist_map
c
c #####################################################################
c
c      These arrays are dimensioned 0:maxtimes,maxhosts
c
c #####################################################################
c
      logical,allocatable,dimension (:,:) :: no_data
      real(4),allocatable,dimension (:,:) ::
     *          allcpu_map,cpuser_map,cpsys_map,cpiow_map,cpidle_map
     *         ,akbpsrt_map,akbpswt_map
c
c #####################################################################
c
c      These arrays are dimensioned 0:maxtimes,maxdevs, where maxdevs is
c      total number of disks in the entire system
c
c #####################################################################
c
      real(4),allocatable,dimension (:,:) ::
     * akbpstxn_map,dskbps_map,dskact_map,akbpsr_map,akbpsw_map,atps_map
c
c #####################################################################
c
c      These arrays are dimensioned 0:maxtimes,maxcores, where maxcores is
c      total number of cores in the entire system
c
c #####################################################################
c
      real(4),allocatable,dimension (:,:) ::
     * coreuser_map,coresys_map,coreidle_map,coreiow_map
c
c #####################################################################
c
c      These arrays are dimensioned maxrows
c
c #####################################################################
c
      logical    ,allocatable,dimension(:) :: absent_host,inverted_name
      character (60),allocatable,dimension(:) :: row_name
c
c #####################################################################
c
c      These arrays are dimensioned maxhosts
c
c #####################################################################
c
      real (4),allocatable,dimension(:) :: acpusr,acpsys,acpiow,acpidl
     *                                    ,akbps_disk_peak,rdmax
     *                                    ,akbps_disk_max
     *                                    ,akbps_txn_peak ,rrmax
     *                                    ,akbps_txn_max
      real (4),allocatable,dimension(:,:)::  txn_med,disk_med
      logical ,allocatable,dimension(:) :: host_track,akbps_disk_rev
     *                                               ,akbps_txn_rev
      integer ,allocatable,dimension(:) :: ndisks,incount,nfirstdev
     *                                    ,ncores,nfirstcore
     *                                    ,index_disk_low,index_txn_low
c
      character(60),allocatable,dimension (:) :: nodename,ssh_name
      character(60) job_node,ssh_node
c
      integer lenmed
c
c #####################################################################
c
c      These arrays are dimensioned max_screen_x
c
c #####################################################################
c
      integer,allocatable,dimension (:) :: line_x
c
c #####################################################################
c
c      These arrays are dimensioned max_screen_y
c
c #####################################################################
c
      integer,allocatable,dimension (:) :: line_y,line_host
c
c #####################################################################
c
c      These arrays are dimensioned maxdevs, where maxdevs is
c      total number of disks in the entire system
c
c #####################################################################
c
      character (60),allocatable,dimension(:) :: dskname,tempdisk
      real(4)       ,allocatable,dimension(:) :: 
     *   adskact,akbpsr,akbpsw,atps,tkbpsr,tkbpsw
c
c #####################################################################
c
c      These arrays are dimensioned maxcores, where maxcores is
c      total number of cores in the entire system
c
c #####################################################################
c
      character (60),allocatable,dimension(:) :: corename,tempcore
      real(4)       ,allocatable,dimension(:) :: 
     *   acoreusr,acoresys,acoreidl,acoreiow
c
      integer(4)   ixm,iym,ixa,iya,nhsp,ixmax,iymax,ntimeslots
     *            ,lastcol,ndisp_type,ndisk_view,lastrow,last_mousex
     *            ,nrow_ruler,nrows,npslot,nedge,ndisk_view_max
     *            ,nfirstcol,nendcol,ndatacol,mousex,mousey
     *            ,ncore_view,ncore_view_max
c
      integer(4)   next_butt_x,lcursorbar,maxnam,ixtxt1min
     *            ,invy1,invy2,nrow_ruler_old,ndelta_x,ndx1,ndx2,lmxold
     *            ,maxcols,isocols,nwhite,nblack
     *            ,nhosts,nh_select,naverage,nrecords,ndev,ndev_s
     *            ,n_cpu_attributes,ncore,ncore_s,ncoretotal
     *            ,nrtotal,nc_index,nd_index,ndevtotal,nhost_count
c
      real (8)     acols,raverage,tpaint
c
c #####################################################################
c
c      This array is dimensioned 100+(2*maxhosts)
c
c #####################################################################
c
      logical       ,allocatable,dimension(:) :: end_chann
c
c #####################################################################
c
c      This array is dimensioned 101:101+(2*maxhosts)
c
c #####################################################################
c
      character(60),allocatable,dimension(:) :: filename
c
c #####################################################################
c
c     PARAMETERS!!!!!!!
c
c #####################################################################
c
c     integer nqsamp,maxdevs,maxdev_s,maxhosts,max_buttons
      integer nqsamp,maxcores, maxcore_s,maxdevs,maxdev_s,maxhosts
     *              ,maxtimes,maxrows,max_screen_x,max_screen_y
c
      end module allcomms
