       subroutine isomast
c
c      copyright @ j s watts 1988
c
      use isocomm
      use parcomm
c
      real (8) tend,tstart,tseq,xcen,xcen1,ycen,ycen1,deltay,deltay1
     *        ,cra,cia,simin,srmin,dx,dy,tistart,tiend
c
      character (3) yesno
      character (12) seqname
c     character (10) datea,dateb
      integer (4) :: line_detail(100000),msgin(len_ngrafout),slave(512)
     *              ,swc,isc,int4in,workarea(6),itbis(5)
     *              ,slavstat(0:511),slavreq(0:511)
c
      logical mandel
      dimension summdet(0:511,5),newdata(6)
c
      ldiff=5
      role='Master'
c
c      Typical data file....
c
c-0.6401923E+00           ;X-coord of picture cntre 
c-0.0E+00                 ;Y-coord of picture centre 
c 0.227E+01               ;Height of vertical side 
c 0.0e+00                 ;Real constant
c 0.0e+00                 ;Imaginary constant 
c 
c      Read the data file.....
c
      txtout=' About to read data file'
      call statout
      read(1,100,err=900,end=901)xcen,ycen,deltay,cra,cia
c
c      Typical picture file...
c
c9999                     ;initial, absolute iteration limit
c0200                     ;Size in x direction
c0150                     ;Size in y direction
c2048                     ;side length (max 2048)
c500000                   ;Size (i6) data packet sent to Artist (max len_ngrafout)
c100000000000310          ;Switches
c|||||||||-|-|||
c||||||||| | ||+==========;Graphics? (0=none,1=screen,2=bitmap,3=screen+bitmap)
c||||||||| | |+===========;Work distribution display (0=none,1=squares, 2=cubes)
c||||||||| | *============;Delay between colour steps in mS (swirl speed)
c||||||||| *==============;Swirl period in seconds (0=no swirl)
c||||||||*================;Bisectors in constant colour (1=yes, 0=no)
c|||||||*=================;Show bisectors (1=yes, 0=no)
c||||||*==================;Colour mapping (1=equal_colour, >=2=black/white)
c|||||*===================;0 for user selection, 1 for automatic recycle
c||||*===================;Immediate picture? (1=yes, 0=no)
c|||*=====================;Check for Artist (1=yes, 0=no)
c||*======================;Check for Slaves (1=yes, 0=no)
c|*=======================;Check for Master (1=yes, 0=no)
c*========================;Load balancing (1=yes, 0=no)
c 
c      Read the picture file.....
c
      txtout=' About to read picture file'
      call statout
      read(3,101,err=902,end=903)limit1,ixm,iym,lside,ngrf
     *          ,lb,icm,ics,ica,immed,mcycle,loglin,nbis,kbis
     *          ,ntswirl,ntdelay,nwork,n_graphics
      ixmold=ixm
      iymold=iym
c
      ngrf=len_ngrafout
      minperi=0
      xcen1  =xcen
      ycen1  =ycen
      deltay1=deltay 
c
      if(immed.eq.0)ngrf=len_ngrafout
c
      if(mcycle.eq.0)then
                         autocycle=.false.
                     else
                         autocycle=.true.
      endif
c
      if(icm.eq.1)then 
                      check=.true.
                  else
                      check=.false.
      endif
c
      if(lb.eq.0)then
                     loadbal=.false.
                 else
                     loadbal=.true.
      endif
c
c      Discover things about the Parallel Environment
c
c      Tell the Artist who is the Master
c
      ntag=msglow+13
      do itask=0,numtasks-1
         if(itask.ne.taskid)then
            call MPI_send(taskid,1,MPI_INTEGER4,itask,ntag,icomm,ierror)
         endif
      enddo
c
      if(check
     *  )then
             write(txtout,145)    ! About to wait for msglow+4 message
             call statout
      endif
c
      source=MPI_ANY_SOURCE
      ntag=msglow+4
      call MPI_recv(artist,1,MPI_INTEGER4
     *                              ,source,ntag,icomm,istatus,ierror)
      call lts(nbytes)
      write(txtout,119)artist
      call statout
c
c      Create and open a file to record the picture sequences
c
      call tim(tseq)
      it=tseq
      write(seqname,131)it
      open(8,file=seqname,form='formatted')
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
c          The start of the repeated section                   c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
c
c      isc: idle slave count
c      swc: spare work count
c      lside: length of side of mset(*,*)
c
 50   continue
c
c      Write out the picture we're about to tackle
c
      limit=limit1
      write(8,130)xcen,ycen,deltay,cra,cia,limit,ixm,iym
c
      if(cia.eq.0..and.cra.eq.0.)then
                                     mandel=.true.
                                 else
                                     mandel=.false.
      endif
c
c     tistart=timef()
      call tim(tstart)
      tistart=tstart
      minperi=1000000
      mwatch=0
      nstats=0
      nbtotp=0
      nbtotm=0
      npels=0
      nelem=0
      isc=0
      swc=0
      lookout=.false.
      signoff=.false.
      need=.true.
c
c      0=available, 1=idle, 2=has been asked for work
c
      do i=0,511
         slavstat(i)=-1
         slavreq (i)=0 
      enddo
c
c  These next four lines define the on-screen picture
c
      dy=deltay/dfloat(iym-1)
      dx=dy
      simin=ycen-(.5_8*deltay)
      srmin=xcen-(.5_8*dx*dfloat(ixm-1))
c
      params( 1)=simin      !
      params( 2)=srmin      !
      params( 3)=dy         !
      params( 4)=dx         !
      params( 5)=iym        !
      params( 6)=ixm        !
      params( 7)=limit      !
      params( 8)=ldiff      !
      params( 9)=cia        !
      params(10)=cra        !
      params(11)=artist     !
      params(12)=taskid     !
      params(13)=lb         ! Load balancing (1=yes, 0=no) 
      params(14)=icm        ! Check for Master (1=yes, 0=no)
      params(15)=ics        ! Check for Slaves (1=yes, 0=no)
      params(16)=ica        ! Check for Artist (1=yes, 0=no)
      params(17)=immed      ! Immediate picture? (1=yes, 0=no)
      params(18)=ngrf       !
      params(19)=maxcols    !
      params(20)=mcycle     ! 0 for user selection, 1 for automatic recycle
      params(21)=loglin     ! Colour mapping (1=equal_colour, >=2=black/white)
c     params(22)=minperi    !
      params(23)=nbis       ! Show bisectors (1=yes, 0=no)
      params(24)=kbis       ! Bisectors in constant colour (1=yes, 0=no) 
      params(25)=ntswirl    ! Swirl period in seconds (0=no swirl)
      params(26)=ntdelay    ! Delay between colour steps in mS (swirl speed)
      params(27)=nwork      ! Work distribution display (0=none,1=squares, 2=cubes)
      params(28)=n_graphics ! Graphics? (0=none,1=screen,2=bitmap,3=screen+bitmap)
c
      do i=29,40
         params(i)=-999.0
      enddo
c
      call param_list  !
c
c      send the key parameters....
c
      ntag=msglow+7
      if(check)then
                 txtout='Sending key parameters                       '
                 call statout
      endif
c
      do itask=0,numtasks-1
c        if(itask.ne.taskid.and.itask.ne.artist)then !!JSW 31JUL16
         if(itask.ne.taskid                    )then ! MUST talk to Artist
            call MPI_send(params,40,MPI_REAL8,itask,ntag,icomm,ierror)
            nbtotm=nbtotm+320
c
            if(check)then
                         write(txtout,108)itask,ntag,ierror
                         call statout
            endif
c
         endif
      enddo
c
c      Create the initial workloads in LINDET
c
c
c x1   to x2,   y1           . (x1,y1) at current NDET+1 for known peri
c x1   to x2,   y2           .              .
c y1+1 to y2-1, x1           .              .
c y1+1 to y2-1, x2           .              .
c source (negative for new) -6   Current NDET+1 for new work
c miniter                   -5              . 2     =0
c iy2                       -4              . 3
c ix2                       -3              . 4
c iy1                       -2              . 5
c ix1                       -1              . 6
c nel           Current NDET                  7     =6
c              *** READING ***     *** WRITING ***
c                FROM LINDET          TO LINDET
c
c      Store work as:
c      perimeter data (if any), source, miniter, iy2, ix2, iy1, ix1
c
      maxpels=ixm*iym
      nslaves=numtasks-2
c
      ndet=0
      swc=0
c
      call side_calc(ixm,iym,nslaves,nx,ny)
c
      write(     *,126 )nx,ny,xcen,ycen,deltay,cra,cia,ixm,iym
      write(lchann,126 )nx,ny,xcen,ycen,deltay,cra,cia,ixm,iym
      call isoflush(lchann)
c     call date_and_time(datea,dateb)
c     open(44,file='label.txt',form='formatted', status='unknown')
c     write(44,144)xcen,ycen,deltay,cra,cia,ixm,iym,datea,dateb
c     close(44)
c     call isoflush(44)
      nlines=0
      nperi=nx*ny*7
      ndetwork=nperi
c
      nxrem=nx
      ixrem=ixm
      ix2=0
      do i=1,nx
c
         ix1=1+ix2
         ix2=ix2+(ixrem/nxrem)
         nxrem=nxrem-1
         ixrem=ixm-ix2
c
         nyrem=ny
         iyrem=iym
         iy2=0
c
         do j=1,ny
c
               iy1=1+iy2
               iy2=iy2+(iyrem/nyrem)
               nyrem=nyrem-1
               iyrem=iym-iy2
c
               swc=swc+1
               line_detail(ndet+7)=6
               line_detail(ndet+6)=ix1
               line_detail(ndet+5)=iy1
               line_detail(ndet+4)=ix2
               line_detail(ndet+3)=iy2
               line_detail(ndet+2)=0
               line_detail(ndet+1)=-swc
c              write(     *,125)(line_detail(mx),mx=ndet+6,ndet+1,-1)
               ndet=ndet+7
c
c      Create work-lines for initial perimeter calculations
c
               do ipx=ix1,ix2,ix2-ix1
                  ip=-1
                  if(ipx.eq.1.or.ipx.eq.ixm)ip=ipx
                  if(ip.gt.0
     *              )then
c
c      Set up a 'column' workload
c
                       line_detail(nperi+7)=6
                       line_detail(nperi+6)=ip
                       line_detail(nperi+5)=iy1
                       line_detail(nperi+4)=iy2
                       line_detail(nperi+3)=-1 
                       line_detail(nperi+2)=0
                       line_detail(nperi+1)=0
                       nperi=nperi+7
c
                       if(nbis.eq.1
     *                   )then
c
c      Send bisector details to be mapped by artist on Work-in-Progress
c
                              itbis(1)=ip
                              itbis(2)=iy1
                              itbis(3)=ip
                              itbis(4)=iy2
                              itbis(5)=0
                              call MPI_send(itbis,5,MPI_INTEGER4,artist
     *                                     ,msglow+11,icomm,ierror)
                              if(check
     *                          )then
                                     write(txtout,147)ip,iy1,iy2
                                     call statout
                              endif
                          endif
                  endif
               enddo
c
               do ipy=iy1,iy2,iy2-iy1
                  ip=-1
                  if(ipy.eq.1.or.ipy.eq.iym)ip=ipy
                  if(ip.gt.0
     *               )then
c
c      Set up a 'liner' workload
c
                       line_detail(nperi+7)=6
                       line_detail(nperi+6)=ip
                       line_detail(nperi+5)=ix1
                       line_detail(nperi+4)=ix2
                       line_detail(nperi+3)=-2 
                       line_detail(nperi+2)=0
                       line_detail(nperi+1)=0
                       nperi=nperi+7
c
                       if(nbis.eq.1
     *                   )then
c
c      Send bisector details to be mapped by artist on Work-in-Progress
c
                              itbis(1)=ix1
                              itbis(2)=ip
                              itbis(3)=ix2
                              itbis(4)=ip
                              itbis(5)=0
                              call MPI_send(itbis,5,MPI_INTEGER4,artist
     *                                     ,msglow+11,icomm,ierror)
                              if(check
     *                          )then
                                     write(txtout,148)ip,ix1,ix2
                                     call statout
                              endif
                          endif
                  endif
               enddo   
c               
               if(check
     *           )then
                      write(txtout,300)j,ny
                      call statout
               endif
         enddo
         if(check
     *     )then
                write(txtout,300)i,nx
                call statout
         endif
      enddo
300   format('Loop',i6,' of NY=',i6,' completed')
301   format('Loop',i6,' of NX=',i6,' completed')
c
      ndet=nperi
c
      if(check
     *  )then   
             do n=1,nperi,7
                write(txtout,200)(line_detail(mx),mx=n,n+6)
                call statout
             enddo
      endif
c
  1   continue
c
c      wait for a message - any message - from a slave.
c
      source=MPI_ANY_SOURCE
      ntag  =MPI_ANY_TAG
      if(check)then
                txtout='Waiting for a message from a slave           '
                call statout
      endif
c
      call MPI_probe(source,ntag,icomm,istatus,ierror)
      call lts(nbytes)
      nbtotp=nbtotp+nbytes
c
      if(mpitype.eq.msglow+14
     *  )then
c
c**********************************************************************
c     This is a message from the Artist saying; Go ahead, I'm ready..
c**********************************************************************
c
             call MPI_recv(junk,1,MPI_INTEGER4  ! Receive it, clear probe
     *                              ,source,ntag,icomm,istatus,ierror)
             if(check
     *         )then
                    txtout='Received clearance from Artist to go ahead'   
                    call statout
             endif
      endif
c
      if(mpitype.eq.msglow+18
     *  )then
c
c**********************************************************************
c     This is a message from the Artist requesting data for a label
c**********************************************************************
c
             call MPI_recv(junk,1,MPI_INTEGER4  ! Receive it, clear probe
     *                              ,source,ntag,icomm,istatus,ierror)
             if(check
     *         )then
                    txtout='Received request from Artist for field data'
                    call statout
             endif
             realout(1)=xcen
             realout(2)=ycen
             realout(3)=deltay
             realout(4)=cra
             realout(5)=cia
             ntag=msglow+17
             call MPI_send(realout,5,MPI_REAL8,artist,ntag,icomm,ierror)
             if(check
     *         )then
                    txtout='Sent field data to Artist for label     '
                    call statout
             endif
             go to 1

      endif
c
      if(mpitype.eq.msglow+16
     *  )then
c
c**********************************************************************
c     This is a panic message from someone else in trouble
c**********************************************************************
c
             call MPI_recv(junk,1,MPI_INTEGER4  ! Receive it, clear probe
     *                              ,source,ntag,icomm,istatus,ierror)
              return
      endif
c
c
c     if(mpitype.gt.msglow+12
      if(mpitype.gt.msglow+20
     *   )then
c
c**********************************************************************
c     This is an invalid message
c**********************************************************************
c
              write(txtout,112)source,mpitype,nbytes
              call statout
        endif
c
      if(mpitype.eq.msglow+8)then
c
c**********************************************************************
c     This is a message containing slave statistics - store them
c**********************************************************************
c
         call MPI_recv(realin,5,MPI_REAL8,source,ntag,icomm
     *              ,istatus,ierror)
         nstats=nstats+1
         do i=1,5
            summdet(source,i)=realin(i)
         enddo
c
         if(check)then
                      write(txtout,115)nstats,(realin(mx),mx=1,5)
                      call statout
         endif
c
c      Print the Artist's stats now
c
         if(source.eq.artist)then
               write(     *,116)
               write(lchann,116)
               call summline(summdet,artist)
c
c      We should end if no graphics or just a bitmap preparation
c
               if((n_graphics.eq.0.or.n_graphics.eq.2).and. ! No graphics or bitmap only
     *             mcycle.eq.0     ! User selection
     *           )then
                      iso_mpi_term=.true.
                      return
               endif
c
c**********************************************************************
c      Wait for a message - any message - from the Artist
c**********************************************************************
c
         source=artist  
         ntag  =msglow+8
         if(check)then
                txtout='Waiting for a message from the Artist        '
                call statout
         endif
         call MPI_recv(newdata,6,MPI_INTEGER4,source,ntag,icomm
     *            ,istatus,ierror)
         call lts(nbytes)
c
         if(check
     *     )then
                write(txtout,127)newdata
                call statout
         endif
c
         if(newdata(1).eq.69     ! F3 response to PICKER
     *     )then
c
c      The F3 key has been pressed, send a stop signal.
c  
                iso_mpi_term=.true.
                return
          endif
c
c        if(newdata(1).eq.-10  !  Was 10... 
         if(newdata(1).eq.111
     *     )then
c
c      The window has been resized.  Draw different size at same resltn
c
c            Leave deltay, xcen and ycen alone
c
               ixm=newdata(4)
               iym=newdata(5)
               dy=deltay/dfloat(iym-1)
               dx=dy
               simin=ycen-(.5_8*deltay)
               srmin=xcen-(.5_8*dx*dfloat(ixm-1))
         endif
c
         if(newdata(1).eq.-998
     *     )then
c
c      No limit point found on autozoom, so restart
c
                xcen=xcen1 
                ycen=ycen1
                deltay=deltay1
         endif
c
         if(newdata(1).eq.1.or.newdata(1).eq.10
     *     )then
c
c      Button1 has been pressed - 
c
                deltay=dy*dfloat(newdata(6))
                if(mcycle.ne.2
     *            )then
c
c      ....zoom in on new centre... (mcycle=2 is zoom-out, same centre)
c
                       xcen=srmin+(dx*dfloat(    newdata(2)))
                       ycen=simin+(dy*dfloat(iym-newdata(3)))
                endif
c
c      Check we haven't zoomed in or out too much
c                                   
                 if(autocycle
     *             )then
                        if(deltay.gt.10.
     *                    )then
                               iso_mpi_term=.true.
                               return
                        endif
c
                        if(deltay.lt.1.e-15
     *                    )then
                               xcen=xcen1 
                               ycen=ycen1
                               deltay=deltay1
                        endif 
                 endif 
         endif
c
         if(newdata(1).eq.3.or.newdata(1).eq.12
     *     )then
c
c      Button3 has been pressed - zoom out.
c
                deltay=10.*deltay
         endif
c
         if(newdata(1).eq.2.or.newdata(1).eq.11
     *     )then
c
c      Button2 has been pressed - change from Mandelbrot to Julia.
c
                if(mandel
     *            )then
c
c      Change to Julia
c 
                       dyold =deltay
                       xcold =xcen
                       ycold =ycen
                       cra =srmin+(dx*dfloat(    newdata(2)))
                       cia =simin+(dy*dfloat(iym-newdata(3)))
                       xcen  =0.
                       ycen  =0.
                       deltay=2.8
                   else
c
c      Change to Mandelbrot
c 
                       xcen  =cra
                       ycen  =cia
                       cra   =0.  
                       cia   =0.
                       deltay=2.27
                endif 
         endif
c
         go to 50
         endif
c
      endif
c
      if(mpitype.eq.msglow+1)then
c
c
c**********************************************************************
c     This is a message containing slave statistics - store them
c**********************************************************************
c
         call MPI_recv(npelsin,1,MPI_INTEGER4,source,ntag,icomm
     *              ,istatus,ierror)
c
         npels=npels+npelsin
         if(check)then
                      write(txtout,102)npelsin,source,npels
                      call statout
         endif
c
         if(npels.gt.maxpels)then
c
c   Give warning that too many pels have arrived, and set checking on
c
                      check=.true.
                      write(txtout,102)npelsin,source,npels
                      call statout
                      write(txtout,113)npels,maxpels
                      call statout
         endif
c
      endif
c
      if(mpitype.eq.msglow+2)then
c
c**********************************************************************
c        Message from idle slave.   Record slave id for future work
c**********************************************************************
c
         call MPI_recv(npelsin,1,MPI_INTEGER4,source,ntag,icomm
     *              ,istatus,ierror)
         isc=isc+1
c
         if(check)then
                     if(npelsin.eq.0)then
                                         write(txtout,103)source,isc
                                         call statout
                                     else
                                         mx=npelsin
                                         if(mx.eq.-99999)mx=0
                                         write(txtout,134)source,mx
                                         call statout
                      endif
         endif
c
         if(npelsin.lt.0)then
                         if(npelsin.eq.-99999)npelsin=0
                         if(-npelsin.lt.minperi)minperi=-npelsin
         endif
c
c      0=available, 1=idle, 2=has been asked for work
c
         slavstat(source)=1
         slave(isc)=source
      endif
c
      if(mpitype.eq.msglow+3)then
c
c**********************************************************************
c        Message was spare work. Store it to give to idle slaves
c**********************************************************************
c
c      Receive and store work as:
c      perimeter data (if any), source, miniter, iy2, ix2, iy1, ix1
c
      call MPI_recv(msgin,len_ngrafout,MPI_INTEGER4,source,ntag,icomm
     *              ,istatus,ierror)
         nel=nbytes/4
         do i=1,nel
            line_detail(ndet+i)=msgin(i)
         enddo
         ndet=ndet+nel+1
         line_detail(ndet)=nel
c
c      0=available, 1=idle, 2=has been asked for work
c     Note that this slave can now be asked for yet more work...
c
         slavstat(source)=0
         slavreq (source)=slavreq(source)-1
c
c      Has work been received in response to a previous run? Not asked  
c      for, but must use it....
c
         if(slavreq(source).lt.0)then
                        if(check)write(txtout,136)source,slavreq(source)
                        slavreq(source)=0
         endif
c
         swc=swc+1
c
         if(check)then
                      write(txtout,104)source,swc,ndet
                      call statout
         endif
c
      endif
c
  2   continue
c
         if(check)then
                      write(txtout,105)isc,swc,npels
                      call statout
         endif
c
c**********************************************************************
c      End of all message handling.   Now share out the work
c      check for end-of-job first...
c**********************************************************************
c
      if(isc.eq.nslaves.and.swc.eq.0)then
         if(npels.eq.maxpels.and.nstats.eq.isc)then
c                                tiend=timef()
                                 call tim(tend)
                                 tiend=tend
                                 tcpu =(tiend-tistart)*.01
                                 summdet(taskid,1)=tstart
                                 summdet(taskid,2)=tend
                                 summdet(taskid,5)=tcpu
c
c      Assume all data known
c
c                                compress=float(npels)/float(nelem)
                                 write(     *,120)xcen,ycen,deltay
                                 write(     *,121)cra,cia,lside,ixm,iym
     *                                       ,minperi,limit
     *                                       ,npels
     *                                       ,nbtotm,nbtotp,nslaves
                                 write(lchann,120)xcen,ycen,deltay
                                 write(lchann,121)cra,cia,lside,ixm,iym
     *                                       ,minperi,limit
     *                                       ,npels
     *                                       ,nbtotm,nbtotp,nslaves
c
                                 if(loadbal
     *                             )then
                                        yesno='Yes'
                                    else
                                        yesno='No '
                                 endif
                                 write(     *,122)yesno
                                 write(lchann,122)yesno
c
                                 cputot=0.
                                 do i=0,numtasks-1
                                    if(i.ne.artist
     *                                )then
                                           call summline(summdet,i)
                                           cputot=cputot+summdet(i,5)
                                    endif
                                 enddo
                                 slvtot=cputot-summdet(taskid,5)
c
                                 write(     *,124)slvtot,cputot
                                 write(lchann,124)slvtot,cputot
c
c      Tell the artist to start drawing
c
c                                call MPI_send(junk,1,MPI_INTEGER4
c    *                                  ,artist,msglow+10,icomm,ierror)
                                 go to 1
         endif
         if(signoff)go to 1
c
c      Sign off the slaves by sending 'negative' work, but don't
c      end this job, to give them time to hand in their remaining
c      work...
c
         if(check)then
                       write(txtout,114)npels
                       call statout
         endif
c
         perimsg(1)=-1
         ntag=msglow+5
         do itask=0,numtasks-1
            if(itask.ne.taskid.and.itask.ne.artist
     *        )then
                   call MPI_send(perimsg,2
     *                           ,MPI_INTEGER4,itask,ntag,icomm,ierror)
                   nbtotm=nbtotm+4
c
                   if(check
     *               )then
                          write(txtout,110)itask
                          call statout
                   endif
c
            endif
         enddo
         signoff=.true.
         go to 1
      endif
c
  3   continue
c
      if(isc.le.0)go to 1
      if(swc.gt.0)then
c
c        We have an idle slave and spare work.   Send out work as
c        ix1, iy1, ix2, iy2, minperi, source, perimeter data (if any)
c
         if(ndet.eq.ndetwork)then
c
c      Make sure all perimeter bits are completed
c
                                 if(isc.lt.numtasks-2)then
                                       if(check)then
                                          write(txtout,135)isc,minperi
                                          call statout
                                       endif 
                                       go to 1
                                 endif
c
c      Reset the perimeter minima on the initial worklines
c
                                 do i=2,ndetwork,7
                                    line_detail(i)=minperi
                                 enddo
                                 limit=limit+minperi
                                 params( 7)=limit
                                 params(22)=minperi
c
c      Send out the parameters (including new limit) to Artist
c
                                 ntag=msglow+7
                                 call MPI_send(params,40,MPI_REAL8
     *                                        ,artist,ntag,icomm,ierror)
                                 nbtotm=nbtotm+320
c
                                 if(check)then
                                          write(txtout,135)isc,minperi
                                          call statout
                                 endif  
                                 ndetwork=-1000
c
c      Wait for the Artist to confirm that windows are open etc
c
                                 call artist_wait
                                 if(iso_mpi_term)go to 904 ! Panic shutdown
         endif
c
         nel=line_detail(ndet)
         do i=1,nel
            perimsg(i)=line_detail(ndet-i)
         enddo
         ndet=ndet-nel-1
         dest=slave(isc)
c
c      Fire work-area off to the slave at 'dest' and to the artist, 
c      to draw a block representing
c      this work area - if we're monitoring this..... 
c
         if(nwork.gt.0
     *     )then
                do n=1,5
                   workarea(n)=perimsg(n)
                enddo
                workarea(6)=dest
                call MPI_send(workarea,6,MPI_INTEGER4
     *                       ,artist,msglow+9,icomm,ierror)
         endif
c
         ntag=msglow+5
         call MPI_send(perimsg,nel,MPI_INTEGER4,dest,ntag,icomm,ierror)
         nbtotm=nbtotm+nbytes
c
         if(check
     *     )then
                write(txtout,106)perimsg(6),dest,(perimsg(mx),mx=1,5)
                call statout
         endif
c
c      0=available, 1=idle, 2=has been asked for work
c
         slavstat(dest)=0
         isc=isc-1
         if(perimsg(4).gt.0)swc=swc-1
         go to 3
      else
c
c      We have an idle slave, and no work to give it.
c      Send out a work request to eligible slaves (if we want
c      load-balancing...
c
         if(loadbal)then
            nplea=0
            ntag=msglow+6
c
c      Send out ISC requests for work, spread as evenly as possible
c      across available slaves
c
cx          do while (nplea.lt.isc)
              do itask=0,numtasks-1
c
c      0=available, 1=idle, 2=has been asked for work
c
               if(itask.ne.taskid
     *       .and.itask.ne.artist
     *       .and.slavstat(itask).ne.1
     *       .and.slavreq (itask).eq.0
     *       .and.nplea.lt.isc
     *                                )then
c
                  if(check)then
                               write(txtout,109)itask,slavreq(itask)
                               call statout
                  endif
c
              call MPI_send(need,1,MPI_LOGICAL,itask,ntag,icomm,ierror)
                  nplea=nplea+1
                  nbtotm=nbtotm+1
cx                slavstat(itask)=2
                  slavreq (itask)=slavreq(itask)+1
               endif
              enddo
cx          enddo  
c
c      Check for idle slaves, yet no requests
c
            if(check)then
               if(nplea.eq.0)then
                           do itask=0,numtasks-1
                                write(txtout,133)
     *                             itask,slavstat(itask),slavreq(itask)
                                call statout
                           enddo 
               endif
            endif
         endif
      endif
      go to 1
c
c      Error handling and program halt...
c
900   continue
      write(0,*)'Error reading data file'
      txtout='Error reading data file'
      call statout
      iso_mpi_term=.true.
      return
c
901   continue
      write(0,*)'Premature end of data file'
      txtout='Premature end of data file'
      call statout
      iso_mpi_term=.true.
      return
c
902   continue
      write(0,*)'Error reading picture file'
      txtout='Error reading picture file'
      call statout
      iso_mpi_term=.true.
      return
c
903   continue
      write(0,*)'Premature end of picture file'
      txtout='Premature end of picture file'
      call statout
      iso_mpi_term=.true.
      return
c
904   continue
      write(0,*)'Problem opening window       '
      txtout='Problem opening window       '
      call statout
      iso_mpi_term=.true.
      return
c
100   format(5(e24.17,/))
101   format(5(i8,/),9i1,2i2,6i1)
102   format('Pels',i12,' from',i3,6x,' Total',i12)
103   format('Slave',i4,' noted as one of',i4,' idle slaves')
104   format('Work from',i4,'. Work lines=',i4,', ndet=',i8)
105   format(i4,' idle slaves,',i10,' lines,',i14,' pels')
106   format('Work ex',i4,' to',i4,': ',5i7)
107   format(30x,'Number of work lines=',i4)
108   format('To slave',i4,':  tag',i5'. ierror=',i6)
109   format('Asking slave',i4,' for work (',i2,' times already)')
110   format('Signed off slave',i4,'                         ')
111   format(/10(12i6,/))
112   format('Odd msg from source',i3,', type',i2,', length',i7)
113   format('Excess pels: Recd',i13,', expected',i13)
114   format('Signing off slaves - received ',i14,' pels')
115   format('St',i3,5f8.1)
116   format('Artist stats')
119   format('ARTIST task is:',i4)
120   format(//20('='),' End of program - summary statistics ',19('=')
     *    ,//3(e25.17))
121   format(2e25.17
     *     ,/' lside=',i5,', ixm=',i7,', iym=',i7
     *     ,/'Perimeter minimum=',i6,', Limit=',i6
     *     ,/i14,' pels'
     *     ,/i10,' bytes sent,',i10,' bytes received'
     *     ,/' Number of slaves=',i4,' (plus one artist...)')
122   format(' Load balancing: ',a3,//76('=')
     *    ,//' Task    Started        Ended    Elapsed     Idle'
     *     ,'     Busy User CPU Ubusy  Ucpu'
     *     ,/)
124   format(/' SLAVE CPU:',f56.3
     *     ,//' TOTAL CPU:',f56.3)
125   format(6i8)
126   format(/' Initial cut of',i3,' by ',i3,' workload:'
     *     ,/'  Centre X:',e24.16
     *     ,/'  Centre Y:',e24.16
     *     ,/'   Delta Y:',e11.3
     *     ,/' Constants:',2e24.16
     *     ,/'   Picture:',2i6)
1261  format(//5(e24.17,/))
127   format('Artist data:   ',6i5)
128   format('WARNING Not expected,  self-generated by:',i4)
129   format('WARNING Requested, but self-generated by:',i4)
130   format(5(e24.17,/),3(i9,/))
131   format(i5,'.picseq')
132   format('Confirmation of work from',i3,22x)
133   format('No request for work: Task=',i4,
     *       ', Status=',i2,', Requests=',i2)
134   format('Perimeter section completed by',i4,', min',i6)
135   format('ISC=',i3,'.',5x,'Perimeter minimum now at: ',i6)
136   format('Work from',i4,' (not requested).  Request',i6)
144   format(
     *       /'+=========== ISOPARIX ==============+'
     *      ,/'* Centre X:',e24.16,' *'
     *      ,/'* Centre Y:',e24.16,' *'
     *      ,/'*  Delta Y:',e11.3,13x,' *'
     *      ,/'*  Const X:',e24.16,' *'
     *      ,/'*  Const Y:',e24.16,' *'
     *      ,/'*  Picture:',2i6,12x,' *'
     *      ,/'*                                   *'
     *      ,/'*       Copyright: John Watts       *'
     *      ,/'*        ',2a10,'       *'
     *       /'+=========== ISOPARIX ==============+'
     *      ,/)
145   format('Waiting for msglow+4 tag to confirm Artist')
146   format('##### ERROR in perimeter:',14i6)
147   format('Perimeter bisector sent: X=',i5,', Y from',i5,' to',i5)
148   format('Perimeter bisector sent: Y=',i5,', X from',i5,' to',i5)
200   format('Perimeter',i4,6i7)
      end
