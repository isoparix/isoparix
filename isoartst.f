      subroutine isoartst
c
c      Design and copyright reserved J S Watts 2012
c
c       VNET: wattsjs at warvm5, 86695135 at ehone
c
       use isocomm
       use bmp_comms
       use parcomm
c
      character (30)date_txt
      character (10)datea,dateb
      character (1) msgtxt(80)
      character (37) label_txt(11)
      character (1) label_chars(0:36,0:10)
      equivalence(label_txt,label_chars)
      character (34) filename
      character (14) outfile
c
      integer (4) :: it(6)
      integer (4) :: itbis(5)
      integer (8) mhuge,nhuge
      integer (4) ncolcalc
c
      dimension newdata(6)
c
      real (8) tt,twork,tend,x,ts,t,td,tistart,tiend
     *        ,xcen,ycen,deltay,cra,cia,dx,dy,simin,srmin
c
      msgcount_in=0
      msgcount_out=0
c
c     Initiate stuff about the screen
c
      call tripleset
c
c      Generate the colour map for bitmaps...
c
c                           call defmap(maisox,7,iret)
                            call defmap(   254,7,iret)
                            if(iret.ne.0
     *                        )then
                                   go to 999
                            endif       
c
      ncolindex=-1
c
c      Record what work areas are created, for re-runs
c
      open(60,file='active.list.60',status='replace',form='formatted')
      open(20,file='equalcol.data',status='unknown',form='formatted')
c
      degrad=355./(113.*180.)
      tmod=360.
      calcphase=.false.
      needwin=.true.
c
c      Discover things about the Parallel Environment
c
c     check=.true.
      role='Artist'
c
c      Wait to see who the Master is...
c
      source=MPI_ANY_SOURCE
      ntag  =msglow+13
      call MPI_recv(master,1,MPI_INTEGER4,source
     *               ,MPI_ANY_TAG,icomm,istatus,ierror)
      if(check
     *  )then
             call lts(nbytes)
             msgcount_in(mpitype)=msgcount_in(mpitype)+1
      endif
c
c   Check to see if this a closedown (eg caused by bad data to Master)
c
      if(mpitype.eq.msglow+16
     *  )then
             go to 999
      endif
c
      write(txtout,119)master
      call statout
c
c      Tell the master who is the artist...
c
      artist=taskid
      call MPI_send(artist,1,MPI_INTEGER4,master,msglow+4,icomm,ierror)
      msgcount_out(ntag)=msgcount_out(ntag)+1
c
c      Receive the key parameters....
c
      nbad=0
      author=.false.
      cube_logic=.false.
      nbmp=999
      itheta=0
      iphi=30
      nwait=0
  1   continue
c
c
      if(calcphase.and.screen_graphics.and.cube_logic
     *  )then
             call microsleep(4000)
             itheta=itheta+1
             call eyepoint(diagonal,real(itheta,8),real(iphi,8))
             call x11clearpixmap() 
             call scene
             call x11flush()
      endif
      call tim(tt)
c
c      Check to see if we have a message.   If not, do interesting
c      rotational stuff....
c
c     source=MPI_ANY_SOURCE
c     ntag  =MPI_ANY_TAG
      call MPI_iprobe(MPI_ANY_SOURCE,MPI_ANY_TAG,icomm,msgwaiting
     *               ,istatus,ierror)
      if(msgwaiting
     *  )then
             call lts(nbytes)
         else
             nwait=nwait+1
             go to 1
      endif
c
c      We have a message.   Who sent it? how long is it? what type is it?
c                           go to 999
c
cq    good_message=.false.
cq    cdiff=.false.
c
cq    if(((tt-ttitle).gt.2.0).and.(.not.author))then
c
c      Tell everyone who wrote it!!
c
cq                             write(txtout,114)
cq                             call title
cq                             author=.true.
cq    endif
c
      if(mpitype.eq.msglow+15
     *  )then
c
c**********************************************************************
c                 A check message from someone...
c**********************************************************************
c
              good_message=.true.
              call MPI_recv(msgtxt,80,MPI_CHARACTER,source
     *                                    ,mpitype,icomm,istatus,ierror)
              msgchann=100+source
              if(statout_hdr.ne.statout_hdr_old
     *          )then
c
c      New bunch of messages - stick in a blank line for intelligibility
c
                     write(msgchann,146)
                     statout_hdr_old=statout_hdr
              endif
              write(msgchann,145)msgtxt
              call isoflush(msgchann)
      endif
c
      if(mpitype.eq.msglow+16
     *  )then
c
c**********************************************************************
c                 End of program
c**********************************************************************
c
                              good_message=.true.
                              go to 999
      endif
c
      if(mpitype.eq.msglow+7
     *  )then
c
c**********************************************************************
c                 Parameters have arrived
c**********************************************************************
c
              call cubeinit
              good_message=.true.
c
              if(check
     *          )then
                     txtout='Parameter message in the queue'
                     call statout
              endif
c
              nforty=40
c             write(0,*)'Calling MPI_recv tag',mpitype,', source',source
              call MPI_recv(params,nforty,MPI_REAL8,source
     *                                    ,mpitype,icomm,istatus,ierror)
c
c             write(0,*)'IERROR in MPI_recv',ierror
                     call lts(nbytes)
                     msgcount_in(mpitype)=msgcount_in(mpitype)+1
c
              if(check
     *          )then
                     txtout='Parameters received'
                     call statout
c                    call lts(nbytes)
              endif
c
              call tim(twork)
              tstart=twork
c
              iter_hi=0
              iter_lo=huge(iter_lo)
              mpels=0
              ntotpels=0
              if(check)call param_list
c
c      Get the parameters
c
              simin  =params( 1)
              srmin  =params( 2)
              dx     =params( 3)
              dy     =params( 4)
              iym    =params( 5)+.5
              ixm    =params( 6)+.5
              limit  =params( 7)+.5
              master =params(12)+.5
              ica    =params(16)+.5
              immed  =params(17)+.5
              mcycle =params(20)+.5
              maptype=params(21)+.5
              minperi=params(22)+.5
              nbis   =params(23)+.5
              kbis   =params(24)+.5
              tswirl =params(25)
              tdelay =params(26)*.001
              nwork  =params(27)+.5
              ngfx   =params(28)+.5
c
             if(nbis.eq.1
     *         )then
                    kbis=1
                    bisector=.true.
                    constcol=.true.
                    cube_logic=.false.
              endif
c
             if(nwork.eq.0
     *         )then
                    square_logic=.false.
                    cube_logic=.false.
             endif
c
             if(nwork.eq.1
     *         )then
                    square_logic=.true.
                    cube_logic=.false.
             endif
c
             if(nwork.eq.2
     *         )then
                    cube_logic=.true.
                    square_logic=.false.
             endif
c
              write(60,1011)ixm,iym
              write(60,1012)bisector,square_logic,cube_logic
              call isoflush(60)
c
              nerror=0
              if(allocated(kdpel))deallocate(kdpel)
              allocate(kdpel(0:limit+1),stat=ierror)
              if(ierror.ne.0)nerror=nerror+1
              kdpel=0
              n_x_prim=0
c
              if(allocated(paint))deallocate(paint)
              allocate(paint(0:limit+1),stat=ierror)
              if(ierror.ne.0)nerror=nerror+10
c
              if(allocated(itermin))deallocate(itermin)
              allocate(itermin(0:limit+1),stat=ierror)
              if(ierror.ne.0)nerror=nerror+100
c
              if(allocated(itermax))deallocate(itermax)
              allocate(itermax(0:limit+1),stat=ierror)
              if(ierror.ne.0)nerror=nerror+1000
c
              if(allocated(mset))deallocate(mset)
              allocate(mset(0:ixm+1,0:iym+1),stat=ierror)
              if(ierror.ne.0)nerror=nerror+10000
              mhuge=(ixm+2)*(iym+2)
c
              if(allocated(mapdata))deallocate(mapdata)
              allocate(mapdata(  ixm  ,  iym  ),stat=ierror)
              if(ierror.ne.0)nerror=nerror+100000
              nhuge=ixm*iym
c
              mset(ixm+1,iym+1)=-999
              mapdata(ixm,iym) =char(255)
c
              write(lchann,148)nerror        ,size(kdpel)
     *                                       ,size(paint)
     *                                       ,size(itermin)
     *                                       ,size(itermax)
     *       ,size(mset,1)   ,size(mset,2)   ,size(mset),mhuge
     *       ,size(mapdata,1),size(mapdata,2),size(mapdata),nhuge
              call isoflush(lchann)
              if(nerror.ne.0)return
c
              if(mcycle.gt.0
     *          )then
                     autocycle=.true.
                 else
                     autocycle=.false.
              endif
c
c      What sort of graphics do we have?
c
c        Graphics? ngfx is composed of bits...
c                  Bit 0: Display screen
c                  Bit 1: Draw bitmap
c                  Bit 2: Save the raw display data
c        Graphics? (0=none,1=screen,2=bitmap,3=screen+bitmap)
c
              if(ngfx.eq.0.
     *          )then
                     screen_graphics=.false.
                     bitmap_graphics=.false.
                        off_graphics=.false.
                             needwin=.false.
                     nwork=0
              endif
c
              if(ngfx.eq.1.
     *          )then
                     screen_graphics=.true.
                     bitmap_graphics=.false.
                        off_graphics=.false.
              endif
c
              if(ngfx.eq.2.
     *          )then
                     screen_graphics=.false.
                     bitmap_graphics=.true.
                        off_graphics=.false.
                             needwin=.false.
                     nwork=0
              endif
c
              if(ngfx.eq.3.
     *          )then
                     screen_graphics=.true.
                     bitmap_graphics=.true.
                        off_graphics=.false.
              endif
c
              if(ngfx.eq.4.
     *          )then
                     screen_graphics=.false.
                     bitmap_graphics=.false.
                        off_graphics=.true.
              endif
c
              if(ngfx.eq.5.
     *          )then
                     screen_graphics=.true.
                     bitmap_graphics=.false.
                        off_graphics=.true.
              endif
c
              if(ngfx.eq.6.
     *          )then
                     screen_graphics=.false.
                     bitmap_graphics=.true.
                        off_graphics=.true.
              endif
c
              if(ngfx.eq.7.
     *          )then
                     screen_graphics=.true.
                     bitmap_graphics=.true.
                        off_graphics=.true.
              endif
c
c      Set up check level, and whether or not to have immediate display
c
              if(ica.eq.1
     *          )then
                     check=.true.
                 else
                     check=.false.
              endif
c
c      Don't use DISPNOW....
c
              dispnow=.false.
c             if(immed.eq.1
c    *          )then
c                    dispnow=.true.
c                else
c                    dispnow=.false.
c             endif
c
              if(cube_logic
     *          )then
c
c      Need to scale screen, etc
c
                     itheta=0
                     iphi  =30
                     call screenscale(itheta,iphi)
                     calcphase=.true.
c
c      Set up cube sizes
c
                     rmax=40.
                     basefrac=.05
                     hmax=(ixm+iym)/2
                     hbase=hmax*basefrac
                     rscale=hmax*(1.-basefrac)/alog(rmax)
                     aden=1./dfloat(minperi+1)    
              endif
c
              maxpels=ixm*iym
              if(needwin
     *          )then
c
c      Open the graphics X-window...
c
                     if(check
     *                 )then
                            txtout='About to open X-window'
                            call statout
                     endif
c
c      ..transport cmap params to isox11.c as we open the window...
c
                     call x11winope(
     *                %val(ixm)    ,%val(iym),%ref(maisox)
     *               ,%ref(isocols),%ref(irc)
     *               ,%val(phired), %val(phigreen), %val(phiblue)
     *               ,%val(  ared), %val(  agreen), %val(  ablue)
     *               ,%val(  pred), %val(  pgreen), %val(  pblue)
     *                             )
c
c
c      Check the return code from x11winope...
c
      if(irc.lt.0   !  Failure
     *  )then
             iso_mpi_term=.true.
             return
      endif
c                     
c
                     if(check
     *                 )then
                            txtout='X-window has been attempted'
                            call statout
                     endif
c
                     if(isocols.eq.0
     *                 )then
                            isocols=maisox
                            write(2,124)maisox,isocols
                     endif
c
                     call isoflush(2)
                     if(irc.ne.0
     *                 )then
                            write(txtout,108)irc,maisox,isocols
                            call statout
                            iso_mpi_term=.true.
                            go to 999  !  Panic and close down
                     endif
c
c      set the colours for each process
c
                     do n=1,numtasks
                        mycol(n)=(n*isocols)/(2*numtasks)
                     enddo
c
                     if(screen_graphics
     *                 )then
                            call x11clearpixmap()
                            call x11flush()
                            write(txtout,115)
                            call statout
                     endif
              endif   ! if(needwin
              needwin=.false.
c
c      The Artist has now opened the window etc etc.   Tell the waiting
c      world this has been done, so they can carry on...
c
              ntag=msglow+14
              call MPI_send(taskid,1,MPI_INTEGER4,master
     *                     ,ntag,icomm,ierror)
              msgcount_out(ntag)=msgcount_out(ntag)+1
              if(check
     *          )then
                     write(txtout,125)ntag,master
                     call statout
              endif 
c
              go to 1
      endif
c
      if(mpitype.eq.msglow+12
     *  )then
c
c**********************************************************************
c                 Picture data have arrived
c**********************************************************************
c
             good_message=.true.
             call MPI_recv(ngrafout,len_ngrafout,MPI_INTEGER4,source
     *                    ,mpitype,icomm,istatus,ierror)
c
              if(check
     *          )then
c                    call lts(nbytes)
                     msgcount_in(mpitype)=msgcount_in(mpitype)+1
              endif
c
              npels=ngrafout(1)
              ntotpels=ntotpels+npels
              call title
              if(check
     *          )then
                     write(txtout,102)npels,source,ntotpels,maxpels
                     call statout
              endif
c
              call deconx(source) 
              if(iso_mpi_term)go to 999
              if(ntotpels.lt.maxpels)go to 1
      endif
c
      if(mpitype.eq.msglow+11.and.bisector
     *  )then
c
c**********************************************************************
c      This is a bisector
c**********************************************************************
c
             good_message=.true.
             call MPI_recv(itbis,5,MPI_INTEGER4,source
     *                       ,mpitype,icomm,istatus,ierror)
             if(check
     *         )then
c                   call lts(nbytes)
                    write(txtout,132)source,itbis
                    call statout
                    msgcount_in(mpitype)=msgcount_in(mpitype)+1
             endif
c
             if(screen_graphics
     *         )then
                    mx1=itbis(1)
                    mx2=itbis(2)
                    mx3=itbis(3)
                    mx4=itbis(4)
                    if(constcol
     *                )then
                           call x11whiteline(%val(mx1),%val(mx2)
     *                                      ,%val(mx3),%val(mx4))
                       else
                           mx5=mycol(source)    ! Source
c                          mx5=itbis(5)         ! Miniter
                           call x11bisect(%val(mx1),%val(mx2),%val(mx3)
     *                                   ,%val(mx4),%val(mx5))
                    endif
                    call x11flush()
             endif
c
             if(bitmap_graphics.and.
     *          n_x_prim.lt.max_x_prim
     *         )then
                    n_x_prim=n_x_prim+1
                    x_prim(n_x_prim,1)=itbis(1)
                    x_prim(n_x_prim,2)=itbis(2)
                    x_prim(n_x_prim,3)=itbis(3)
                    x_prim(n_x_prim,4)=itbis(4)
                    x_prim(n_x_prim,5)=0
                    x_prim(n_x_prim,6)=source
                    x_prim(n_x_prim,7)=3      ! A bisector...
             endif
c      
             if(n_x_prim.eq.max_x_prim
     *         )then
                    write(0,112)max_x_prim
             endif
c
             go to 1
      endif
c
      if(mpitype.eq.msglow+9
     *  )then
c
c**********************************************************************
c      This is a work area...
c**********************************************************************
c
             good_message=.true.
             call MPI_recv(it,6,MPI_INTEGER4,source
     *                      ,mpitype,icomm,istatus,ierror)
c
              if(check
     *          )then
c                    call lts(nbytes)
                     msgcount_in(mpitype)=msgcount_in(mpitype)+1
              endif
c
             source=it(6)  !Source is really the master but use slave ID 
             write(txtout,101)source,(it(mx),mx=1,5)
             call title
             if(check)call statout
c
c            if(nwork.eq.2.and.
             if(cube_logic.and.
     *          it(4).ge.0 !  Don't try to draw a box on a row or column..
     *         )then
c
c      We're monitoring with cubes....
c
                    ix1=it(1)-(ixm/2)
                    iz1=it(2)-(iym/2)
                    ix2=it(3)-(ixm/2)
                    iz2=it(4)-(iym/2)
c
c      Normalise cube height:
c
                    ratio=dfloat(it(5)+1)*aden
                    ratiolog=alog(ratio)
                    iheight=hbase+(rscale*ratiolog)
                    write(60,1011)source,ix1,iz1,ix2,iz2,iheight
                    call isoflush(60)
                    call cubeadd(source,ix1,iz1,ix2,iz2,iheight)
                    call x11clearpixmap()
                    call scene
                    call x11flush()
              endif
c
c            if(nwork.eq.1
             if(square_logic
     *         )then
c
c      We're monitoring with squares...
c
                    mx1=it(1)
                    mx2=it(2)
                    mx3=it(3)
                    mx4=it(4)
                    mx5=mycol(it(6))
                    call x11work(%val(mx1),
     *                           %val(mx2),
     *                           %val(mx3),
     *                           %val(mx4),
     *                           %val(mx5))
                    call x11flush()
             endif
c
             go to 1
      endif
c
      if(.not.good_message
     *  )then
C            write(txtout,147)mpitype,tag(mpitype),source,nbytes,ierror
C            call statout
             txtout='This message NOT wanted here...!             '
             call statout
             nbad=nbad+1
             if(nbad.gt.1000
     *         )then
                    write(txtout,103)nbad
                    call statout
                    iso_mpi_term=.true.
                    go to 999
             endif
             go to 1
      endif
c
      if(ntotpels.lt.maxpels
     *  )then
             go to 1
         else
             calcphase=.false.
c
             if(screen_graphics.and.cube_logic
     *         )then
c
c      Everything has been computed, draw the floor on its own
c
                    call x11clearpixmap() 
                    call flooralone
                    call x11flush()
c
c      Rotate the scene, and tilt it upwards....
c
                    nsteps=30
                    a=1./dfloat(nsteps)
c
c     write(*,2000)ixcmax,ixcmin,iycmax,iycmin,ixm,iym,t
2000  format(6i8,' Ratio=',f10.7)
c
                    itheta=mod(itheta,360)
                    if(itheta.lt.90
     *                )then 
                           deltheta=a*dfloat(90-itheta)
                       else
                           deltheta=a*dfloat(450-itheta)
                    endif
                    delphi=a*dfloat(90-iphi)
                    tha=.5+dfloat(itheta)
                    pha=.5+dfloat(iphi)
c
                    do n=1,nsteps
c
                       if(n.eq.nsteps
     *                   )then
                              ixcmax=0
                              ixcmin=huge(i)
                              iycmax=0
                              iycmin=huge(i)
                       endif
c
                       a=n
                       itheta=tha+(a*deltheta)
                       iphi  =pha+(a*delphi)
                       call eyepoint(diagonal,real(itheta,8)
     *                                       ,real(iphi,8))
                       call x11clearpixmap() 
                       call flooralone
                       call x11flush()
                       call microsleep(80000)
                    enddo
c
                    rarea=1./dfloat(ixm*iym)
                    do n=1,nsteps
cPJDM
c                   write(6,*) 'PJDM: n, nsteps = ',n,nsteps
c                   write(6,*) 'PJDM: ixm,iym,rarea = ',ixm,iym,rarea
c                   write(6,*) 'PJDM: ixcmax,ixcmin,iycmax,iycmin = ',
c    *                          ixcmax,ixcmin,iycmax,iycmin 
cPJDM

                       t=dsqrt(dfloat((ixcmax-ixcmin+1)
     *                               *(iycmax-iycmin+1))
     *                       *rarea)
                       ar=t**(1./dfloat(nsteps-n+1))
                       scale=scale*ar
c                      xoffset=xoffset*ar
c                      yoffset=yoffset*ar
                       call eyepoint(diagonal,real(itheta,8)
     *                                       ,real(iphi,8))
                       call x11clearpixmap() 
                       call flooralone
                       call x11flush()
                    enddo
             endif
c
c      Build equal-area colour maps and draw the final pictures!
c
      if(screen_graphics
     *  )then
             ncolindex=isocols
      endif
c
      if(bitmap_graphics
     *  )then
             ncolindex=254
      endif
c
      call equalcol(ncolindex,.true.)
      if(iso_mpi_term
     *   )then
              write(txtout,149)
              call statout
              go to 999
      endif
c
c      ...put mset's colour-mapped values into mapdata      
c
              if(check
     *          )then
                     write(txtout,153)maxval(mset)
                     call statout
              endif
c
      do iy=1,iym
         do ix=1,ixm
            mapdata(ix,iy)=paint(mset(ix,iy))
         enddo
      enddo
c
c      Don't need MSET now, so deallocate it...
c
      if(screen_graphics.and.
     *   params(23).lt.2
     *  )then
             do itask=2,numtasks-1
                do i=1,n_x_prim
c
                   if(x_prim(i,6).eq.itask
     *               )then
                          mx=ichar(paint(x_prim(i,5)))
                          if(x_prim(i,7).eq.2
     *                      )then
                                 call x11rectfi(%val(x_prim(i,1))
     *                                         ,%val(x_prim(i,2))
     *                                         ,%val(x_prim(i,3))
     *                                         ,%val(x_prim(i,4))
     *                                         ,%val(mx)
     *                                         )
                          endif
c
                          if(x_prim(i,7).eq.1
     *                      )then
                                 call x11line  (%val(x_prim(i,1))
     *                                         ,%val(x_prim(i,2))
     *                                         ,%val(x_prim(i,3))
     *                                         ,%val(x_prim(i,4))
     *                                         ,%val(mx)
     *                                         )
                          endif
c
                          if(x_prim(i,7).eq.0
     *                      )then
c                                write(0,*)mx,(x_prim(i,my),my=1,7)
                                 call x11pixel (%val(x_prim(i,1))
     *                                         ,%val(x_prim(i,2))
     *                                         ,%val(mx)
     *                                         )
                          endif
                   endif
                enddo
                call x11flush()
             enddo
      endif
c
c      ***************************************************************
c      *                                                             *
c      *         Write out a bitmap of this wondrous thing..!!       *
c      *                                                             *
c      ***************************************************************
c
      if(bitmap_graphics
     *  )then
c
             nbmp=nbmp-1
             write(bmname,128)nbmp
c
             if(check
     *         )then
                    write(txtout,152)nbmp,bmname
                    call statout
             endif                    
c
             if(bisector
     *         )then
                    do i=1,n_x_prim
                       if(x_prim(i,7).eq.3
     *                   )then
                              mapdata(x_prim(i,1):x_prim(i,3)
     *                               ,x_prim(i,2):x_prim(i,4)
     *                               )=char(128)
                       endif
                    enddo
             endif
c             
             rewind(1)
c
             if(params(23).ne.2
     *         )then             
c
c      Write a label on the picture
c
                   if(check
     *               )then
                   txtout='Sending request for field data to Master    '
                          call statout
                   endif
c
                    ntag=msglow+18
                    call MPI_send(artist,1,MPI_INTEGER4,master,ntag
     *                           ,icomm,ierror)
                    ntag=msglow+17 ! Receive field data
                    call MPI_recv(realin,5,MPI_REAL8,master
     *                                       ,ntag,icomm,istatus,ierror)
                   if(check
     *               )then
                   txtout='Received field data from Master             '
                          call statout
                   endif
                   xcen=realin(1)
                   ycen=realin(2)
                 deltay=realin(3)
                    cra=realin(4)
                    cia=realin(5)
                   call date_and_time(datea,dateb)
c
c      Basic label is 37 chars of  8 pixels wide
c                     10 chars of 13 pixels high
c
                    if(10*13*16.gt.iym
     *                )then
                           mag=1
                       else    
                           mag=iym/(10*13*16) ! 10 line label, 1/16th height
                    endif       
c
                    if(mag*10*13.le.iym.and.
     *                 mag*37* 8.le.ixm
     *                )then
                           write(label_txt,144)xcen,ycen,deltay,cra,cia
     *                                        ,ixm,iym,mag,datea,dateb
                           lx=mag*8
                           ly=mag*13
                           call label_maker(mag,lx,ly,ixm,iym,mapdata
     *                                     ,label_chars,36,10)
                    endif
             endif
      endif
c
      if(off_graphics
     *  )then
c
c      Write out key arrays for further use, eg by JavaView
c
             open(62,file=trim(bmname)//'.off',status='unknown'
     *              ,form='unformatted')
             write(62)ixm,iym,limit
             write(62)rgbquad       ! char(1024)
             write(62)paint         ! char(0:limit+1)
             write(62)mset          ! integer(2)(0:ixm+1,0:iym+1)
             write(62)mapdata       ! character(1)(ixm,iym)
             close(62)
      endif
c
c
      if(bitmap_graphics
     *  )then
             call array2bmp(ixm,iym,mapdata)
      endif
c
      call tim(tend)
      tcpu=(tend-tstart)*.01
      realout(1)=tstart
      realout(2)=tend
      realout(3)=0.
      realout(4)=0.
      realout(5)=tcpu
c 
      if(check
     *  )then
             write(txtout,106)(realout(mx),mx=1,5)
      endif
c
c      Deliver final statistics
c
            ntag=msglow+8
            call MPI_send(realout,5,MPI_REAL8,master,ntag,icomm,ierror)
            msgcount_out(ntag)=msgcount_out(ntag)+1
c
            do n=-10,50
               if(msgcount_in(n).ne.0
     *           )then
                      write(lchann,133)n,msgcount_in(n)
               endif
            enddo
c
            do n=-10,50
               if(msgcount_out(n).ne.0
     *           )then
                      write(lchann,134)n,msgcount_out(n)
               endif
            enddo
c
      if(screen_graphics
     *  )then
c
c      This is a graphics run
c
c*********************************************************************
c                           SWIRL
c*********************************************************************
c
              t=0. 
              call tim(ts)
c
c      Swirl for at least tswirl seconds...
c
              if(tswirl.gt.0.
     *          )then
                     do while(t-ts.lt.tswirl)
c
c      Make sure we complete a colour cycle
c
                        do kx=1,isocols
                           call x11swirlup(%val(kx))
                           call x11spotbutton(ja,jb,jc)
c
c      Delay the colour change by tdelay seconds
c
                           td=0.
                           call tim(t)
                           do while (td-t.lt.tdelay)
                              call tim(td)
                           enddo
                        enddo
                     enddo
c
c
c*********************************************************************
c                         KALEIDOSCOPE
c**********************************************************************
c
                     t=0. 
                     call tim(ts)
c
c      Swirl for at least tswirl seconds...
c
                     do while(t-ts.lt.tswirl)
c
c      Make sure we complete a colour cycle
c
                        do kx=1,isocols
                           call x11kaleido(%val(kx))
                           call x11spotbutton(ja,jb,jc)
c
c      Delay the colour change by tdelay seconds
c
                           td=0.
                           call tim(t)
                           do while (td-t.lt.tdelay)
                              call tim(td)
                           enddo
                        enddo
                     enddo
                endif			! End of tswirl
      endif
c
      if(autocycle
     *  )then
c
c      Select an edge point at random
c
             nbut=1
             call edge(kxcen,kycen,kdy,mcycle)
             if(iso_mpi_term)go to 999
         else
             if(screen_graphics
     *         )then
c
c      Select the next point by the mouse or keyboard
c
                    call picker(nbut,kxcen,kycen,ixmp,iymp,kdy,dx,dy
     *                         ,simin,srmin)
                            call x11clearpixmap()
                            call x11flush()
                else
                    nbut=-999	!No screen graphics, no autocycle
              endif
      endif
c
c 3   continue
c
c      nbut=-10 : Exposed
c      nbut=111 : Resized
c      nbut=  1 : Button 1
c      nbut=  2 : Button 2
c      nbut=  3 : Button 3
c      nbut= 13 : Keyboard 4
c      nbut= 14 : Keyboard 5
c      nbut= 15 : Keyboard 6
c
c      End of user interaction - announce the next action
c
             if(nbut.eq.111
     *         )then
                    needwin=.true.
                    write(0,300)ixmp-1,iymp-1
                    call x11close()
             endif
             if(nbut.eq.1  .or.nbut.eq.10)write(0,301)
             if(nbut.eq.2  .or.nbut.eq.11)write(0,302)
             if(nbut.eq.3  .or.nbut.eq.12)write(0,303)
             if(nbut.eq.69 )write(0,304)
c
             ntotpels=0
             newdata(1)=nbut
             newdata(2)=kxcen
             newdata(3)=kycen
             newdata(4)=ixmp-1
             newdata(5)=iymp-1
             newdata(6)=kdy
             call MPI_send(newdata,6,MPI_INTEGER4
     *                    ,master,ntag,icomm,ierror)
             msgcount_out(ntag)=msgcount_out(ntag)+1
             if(check
     *         )then
                    write(txtout,116)(newdata(mx),mx=1,6),ntag
                    call statout
             endif
                      
      endif
      go to 1
c
c      Error handling and program return...
c
999   continue
      if(screen_graphics)call x11close()
      write(txtout,150)
      call statout
      return
c
100   format('   Bisector from',i3,':',5i5)
101   format('Work gvn to proc',i3,':',5i5)
1011  format(8i5)
1012  format(8l5)
102   format('Recd',i9,' pels ex',i2,':',i9,' of',i9)
103   format(i5,' unknown messages received - stopping')
104   format(z8.8,'.data ')
105   format('Reconstruction',i4,' complete:',i11,' total')
106   format('Stats',5f8.1)
107   format('Restoring data from slave',i4,16x)
108   format('No X-win - no graphics, RC=',3i6)
109   format(i2)
110   format('Error from mouse:',6i4,4x)
111   format(i4)
112   format('ISOARTST: Buffer full of',i8,'X-primitives')
113   format('New picture selected (',4i5
     *      ,'), starting in',i3,' seconds')
114   format('Design and copyright reserved John Watts 2012')
115   format('No new window required')
116   format('NEWDATA',6i5,', TAG:',i4)
117   format('Waiting for a message from anyone...!'
     *      ,' icomm=',i10,', msgwaiting is',l2', nwait=',i3)
1171  format('                                     '
     *      ,' Length',i10,', type is',i4', nwait=',i3)
1172  format('                  About to call LTS..'
     *      ,', nwait=',i3)
118   format('Source: ',i8,', IT:     ',5i8,
     *      /'IHEIGHT:',i32,', RATIO:',f8.2,', LOG(RATIO):',f8.3)
119   format('MASTER task is:',i4)
120   format('ISOARTST (ex-PICKER): nbut,kxcen,kycen,ixm,iym,kdy',6i8)
121   format('No changeable colours')
122   format('ISOCOLS has value of',i4)
123   format('IXCMAX=',i5,', IXCMIN=',i5,', IYCMAX=',i5,', IYCMIN=',i5
     *      ,', SCALE=',f6.4,', T=',f6.4,', RATIO=',f6.4)
124   format('MAISOX=',i6,', ISOCOLS forced to =',i6)
125   format('Artist_ready msg (type',i4,') sent to task',i4)
1251  format('Artist_ready msg (type',i4,') schd to task',i4)
127   format('ISOARTST - Iteration:',i6,', bitmap colour=',i4
     *      ,', screen colour=',i4)
128   format(i4.4,26(' '))
129   format('ISOARTST: Calling DATOUT with SOURCE: ',i3,', LCHANN=',i3
     *      ,', NEL:',i12)
130   format('ISOARTST: IXM=',i7,', IYM=',i7)
131   format('ISOARTST: Changing scale...  AR=',e12.5)
132   format('From',i3,':  Bisector at',5i6)
133   format('Messages in  (type',i3,'):',i8)
134   format('Messages out (type',i3,'):',i8)
144   format(
     *        '+=========== ISOPARIX ==============+'
     *      ,/'* Centre X:',e24.16,              ' *'
     *      ,/'* Centre Y:',e24.16,              ' *'
     *      ,/'*  Delta Y:',e11.3,13x,           ' *'
     *      ,/'*  Const X:',e24.16,              ' *'
     *      ,/'*  Const Y:',e24.16,              ' *'
     *      ,/'*  Picture:',2i6,',  Label:',i2, 'x *'
     *      ,/'*                                   *'
     *      ,/'*       Copyright: John Watts       *'
     *      ,/'*       ',a10,' ',a10,      '       *'
     *       /'+=========== ISOPARIX ==============+'
     *      )
145   format(80a1)
146   format(/)
147   format(i2,' ',a20,', Src:',i3,', Len',i8,', Err:',i3)
148   format('Allocations in ISOARTST (NERROR=',i8,'):'
     *     ,/'KDPEL  ',i36
     *     ,/'PAINT  ',i36
     *     ,/'ITERMIN',i36
     *     ,/'ITERMAX',i36
     *     ,/'MSET   ',4i12
     *     ,/'MAPDATA',4i12
     *      )
149   format('FAILURE in call to equalcol - returning')
150   format('Goodbye from ISOARTST...')
151   format('mycol(',i3,') is',i4)
152   format('bmname:',i4,' ',a)
153   format('Max value of MSET',i10)
155   format('Bisector, squares, cubes',3l5)
200   format(5(e24.17,/))
201   format(a30)
c
300   format(///'*********',/'Window closed/resized/recreated/remapped'
     *      ' to ',i0.0,' x ',i0.0,/'*********',//)
301   format(///'*********',/'New area selected',/'*********',//)
302   format(///'*********',/'Mandelbrot <==> Julia swap'
     *        ,/'*********',//)
3021  format(///'*********',/'Mandelbrot <==> Julia',/'*********',//)
3022  format(///'*********',/'Julia ==> Mandelbrot',/'*********',//)
303   format(///'*********',/'Back to previous selection'
     *        ,/'*********',//)
304   format(///'*********',/'Closedown',/'*********',//)

      end
