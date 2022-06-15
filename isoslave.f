       subroutine isoslave
c
c      Copyright @ J S Watts 1988
c
c       VNET: wattsjs at warvm5, 86695135 at ehone
c
      use isocomm
      use parcomm
c
      real (8) twork,tworkrec,tidle,tend,tstart
     *        ,srmin,simin,cra,cia,dx,dy
c
      integer (4) :: gtemp(20)
      integer (4) :: init(6)
c
      character(14)sleep_command
c
      logical msgneed,needx,liminc,quiescent
c
      role='Slave'
      write(txtout,140)
      call statout
      quiescent=.false.
      nsleep=1
      check=.false.
c
c      Receive the key parameters....
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                      Start of repeat section                         c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
 50   continue
c
      call MPI_iprobe(MPI_ANY_SOURCE,MPI_ANY_TAG,icomm,msgneed
     *               ,istatus,ierror)
      if(msgneed
     *  )then
             call lts(nbytes)
         else
             if( quiescent
     *         )then
                    if(check
     *                )then
                           nquiescent=nquiescent+1
                           write(txtout,142)nsleep,nquiescent
                           call statout
                    endif
                    call sleep(nsleep)
             endif
c
             go to 50
      endif
c
      if(mpitype.eq.msglow+16    !   Call to shutdown
     *  )then
             go to 999
      endif
c
      if(mpitype.eq.msglow+13
     *  )then
             call MPI_recv(master,1,MPI_INTEGER4
     *       ,source,mpitype,icomm,istatus,ierror)
              if(check
     *          )then
                     write(txtout,1102)mpitype,source,nbytes
                     call statout
              endif
      endif
c
      if(mpitype.eq.msglow+7
     *  )then
             call MPI_recv(params,40,MPI_REAL8
     *       ,source,mpitype,icomm,istatus,ierror)
              if(check
     *          )then
                     write(txtout,1101)mpitype,source,nbytes
                     call statout
              endif
         else
c
c      This is not the message we want - ie, a parameter list (type 7)
c
              if(check
     *          )then
                     write(txtout,110)mpitype,source,nbytes
                     call statout
              endif
              nquiescent=0
              go to 50
      endif
c
      call tim(tstart)
      quiescent=.false.
      iterxs=0
      call tim(twork)
      tstart=twork
      nboxsel=0
      tworktot=0.
      tidletot=0.
      npels=0
      mpels=0
      kpels=0
      mline=0
      nline=0
      ngline=1
      need=.false.
      liminc=.false.
c
      if(check)call param_list
      simin =params( 1)
      srmin =params( 2)
      dy    =params( 3)
      dx    =params( 4)
      iym   =params( 5)+0.5
      ixm   =params( 6)+0.5
      limit =params( 7)+0.5
      ldiff =params( 8)+0.5
      cia   =params( 9)
      cra   =params(10)
      artist=params(11)+0.5
      master=params(12)+0.5
      lb    =params(13)+0.5
      ics   =params(15)+0.5
      ngrf  =params(18)+0.5
      nwork =params(27)+0.5
c                            
      if(allocated(cr))deallocate(cr,stat=istat)
      if(allocated(sr))deallocate(sr,stat=istat)
      if(allocated(ci))deallocate(ci,stat=istat)
      if(allocated(si))deallocate(si,stat=istat)
c                            
      allocate(cr(ixm))
      allocate(sr(ixm))
      allocate(ci(iym))
      allocate(si(iym))
c
      if(params(23).gt.0
     *  )then
             bisector=.true.
         else
             bisector=.false.
      endif
c  
      if(lb.eq.0
     *  )then
                     loadbal=.false.
                 else
                     loadbal=.true.
      endif
c  
      if(ics.eq.0
     *  )then
                     check=.false.
                 else
                     check=.true.
      endif
c
      if(cra+cia.eq.0.
     *  )then
c
c      Calculate the imaginary values for each row...
c
                           do iya=1,iym
                              iy=iym-iya+1
                              si(iy)=(dfloat(iya)*dy)+simin
                              ci(iy)=si(iy)
                           enddo
c
c      Calculate the real values for each column...
c
                           do ix=1,ixm
                              sr(ix)=(dfloat(ix)*dx)+srmin
                              cr(ix)=sr(ix)
                           enddo
                       else
c
c      Calculate the imaginary values for each row...
c
                           do iya=1,iym
                              iy=iym-iya+1
                              si(iy)=(dfloat(iya)*dy)+simin
                              ci(iy)=cia
                           enddo
c
c      Calculate the real values for each column...
c
                           do ix=1,ixm
                              sr(ix)=(dfloat(ix)*dx)+srmin
                              cr(ix)=cra
                           enddo
      endif
c
c      Workloads in LINDET, or MSGIN
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
      if(check
     *  )then
             write(txtout,144)master,artist
             call statout
      endif
      pwork=0.0
 14   continue
      if(check
     *  )then 
             write(txtout,102)ngline,npels,mpels,nboxsel,mline
             call statout
      endif
c
      if(mline.le.0
     *  )then
c
c        Announce that this slave is idle and waiting for work
c
         call tim(tidle)
         pwork=tidle-twork
         tworktot=tworktot+pwork
         if(check
     *     )then
10200 format('         ngline     npels     mpels   nboxsel   pwork')
                write(txtout,10200)
                call statout
                write(txtout,1021)ngline,npels,mpels,nboxsel,mline
                call statout
         endif
         mpels=0
         kpels=0
c
         ntag  =msglow+2
         call MPI_send(mline,1,MPI_INTEGER4,master,ntag,icomm,ierror)
         mline=0
c
c      Only interested, at this point, in some kind of work!!
c
  1      continue
c
         call MPI_recv(perimsg,len_perimsg,MPI_INTEGER4,MPI_ANY_SOURCE
     *                ,MPI_ANY_TAG,icomm,istatus,ierror)
         call lts(nbytes)
c
c      ...or a terminate message...
c
      if(mpitype.eq.msglow+16    !   Call to shutdown
     *  )then
             go to 999
      endif
c
c      TEST ***********
c
         if(nbytes.eq.0
     *     )then
                write(txtout,115)mpitype,source,nbytes
                call statout
                go to 1
         endif
c
         if(mpitype.ne.msglow+5
     *     )then
                if(check
     *            )then
                       write(txtout,103)mpitype,source,nbytes
                       call statout
                endif
                go to 1
         endif
c
         call tim(twork)
c
        pidle=twork-tidle
        tidletot=tidletot+pidle
c
c        What sort of work have we received?                           
c
         if(perimsg(4).lt.0.and.
     *      perimsg(1).gt.0
     *     )then
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                      Start of an initial perimeter section           c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
                if(check
     *            )then
                       write(txtout,1011)(perimsg(mx),mx=1,6),pidle
                       call statout
                endif
c
                 ngline=0
                 ja=perimsg(1)
                 k1=perimsg(2)
                 k2=perimsg(3)
                 if(perimsg(4).eq.-1
     *             )then
                        if(check
     *                    )then
                               write(txtout,136)ja,k1,k2
                               call statout
                         endif
c
                         if(allocated(mset)
     *                     )then
                                if(lbound(mset,1).gt.ja.or.
     *                             ubound(mset,1).lt.ja.or.
     *                             lbound(mset,2).gt.k1.or.
     *                             ubound(mset,2).lt.k2
     *                            )then
                                       deallocate(mset)
                                endif
                         endif
c
                         if(.not.allocated(mset)
     *                     )then
                                allocate(mset(ja:ja,k1:k2))
                                if(check
     *                            )then
                                       mset=-9999
                                       write(txtout,137)ja,ja,k1,k2
     *                                       ,size(mset,1),size(mset,2)
                                       call statout
                                endif
                         endif
c
                         call column(ja,k1,k2,0)
                 endif
c
                 if(perimsg(4).eq.-2
     *             )then
                         if(check
     *                     )then
                                write(txtout,135)ja,k1,k2
                                call statout
                         endif
c
                         if(allocated(mset)
     *                     )then
                                if(lbound(mset,1).gt.k1.or.
     *                             ubound(mset,1).lt.k2.or.
     *                             lbound(mset,2).gt.ja.or.
     *                             ubound(mset,2).lt.ja
     *                            )then
                                       deallocate(mset)
                                endif
                         endif
c
                         if(.not.allocated(mset)
     *                     )then
                                allocate(mset(k1:k2,ja:ja))
                                if(check
     *                            )then
                                       mset=-9998
                                       write(txtout,138)k1,k2,ja,ja
     *                                       ,size(mset,1),size(mset,2)
                                       call statout
                                endif
                         endif
                         call liner (ja,k1,k2,0)
                 endif
c
c      Examine the calculated values
c
                 mline=-99999
                 do i=4,ngline
                    if(ngrafout(i).lt.0.and.
     *                 ngrafout(i).gt.mline
     *                )then
                           mline=ngrafout(i)
                     endif
                 enddo
c      write(*     ,200)ngline,mline,(ngrafout(mx),mx=1,ngline)
c200   format(7i10)
                 ngline=1    ! Was 2
                 npels=0
                 mpels=0
                 if(check
     *             )then
                        write(txtout,134)mline
                        call statout
                 endif
                 go to 14
         endif
c
c
c
         if(perimsg(1).lt.0)then
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                      Start of sign-off section                       c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c        Any work to send out?  Flush buffer anyway...
c
            call grafout
c
            call tim(tend)
c           tiend=mclock()
            tcpu=(tend-tstart)*.01
            realout(1)=tstart
            realout(2)=tend
            realout(3)=tidletot
            realout(4)=tworktot
            realout(5)=tcpu
c
            if(check)then
                         write(txtout,106)(realout(mx),mx=1,5)
                         call statout
                         write(txtout,104)iterxs
                         call statout
            endif
c
c      Deliver final statistics
c
            ntag=msglow+8
            call MPI_send(realout,5,MPI_REAL8,master,ntag,icomm,ierror)
c
            quiescent=.true. ! Let this slave sleep whilst the artist works....
            nquiescent=0
            go to 50
         endif
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c        Got real work, put it on the stack                            c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
         if(check
     *     )then
                write(txtout,101)(perimsg(mx),mx=1,6),pidle
                call statout
         endif
c
c      Put data into lindet(*,1) from perimsg after receipt
c      Data has come in as:
c      ix1, iy1, ix2, iy2, miniter, source, perimeter data (if any)
c
                         if(allocated(mset)
     *                     )then
                                if(lbound(mset,1).gt.perimsg(1).or.
     *                             ubound(mset,1).lt.perimsg(3).or.
     *                             lbound(mset,2).gt.perimsg(2).or.
     *                             ubound(mset,2).lt.perimsg(4)
     *                            )then
                                       deallocate(mset)
                                endif
                         endif
c
                         if(.not.allocated(mset)
     *                     )then
                                allocate(mset(perimsg(1):perimsg(3)
     *                                       ,perimsg(2):perimsg(4)))
                                if(check
     *                            )then
                                       mset=-9997
                                       write(txtout,139)
     *                                        perimsg(1),perimsg(3)
     *                                       ,perimsg(2),perimsg(4)
     *                                       ,size(mset,1),size(mset,2)
                                       call statout
                                endif
                         endif
c
c      Record when it arrived...
c
         call tim(tworkrec)
c
c      Now feed the perimeter details into lindet
c
         do i=1,5
            lindet(i,1)=perimsg(i)
         enddo
c
            if(lindet(5,1).lt.0
     *        )then
                   write(txtout,1052)(lindet(mx,1),mx=1,5)
                   call statout
                   go to 999
            endif
c
         mline=1
c
      if(perimsg(6).gt.0)then
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                      Start of work picked up from another slave      c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      Restore MSET perimeter
c
            ndet=nbytes/4
c
c      Length of horizontal and vertical sides...
c
            nx=lindet(3,1)-lindet(1,1)+1
            ny=lindet(4,1)-lindet(2,1)
c
            if(check
     *        )then
                   write(txtout,105)ndet,2*(nx+ny),nx,ny
                   call statout
            endif
c
            if(ndet.lt.2*(nx+ny)
     *        )then
                   write(txtout,1051)ndet,2*(nx+ny),nx,ny
                   call statout
                   go to 999
            endif
c
            do i=lindet(1,1),lindet(3,1)
               mset(i,lindet(2,1))=perimsg(ndet)
               ndet=ndet-1
            enddo
c
c
            do i=lindet(1,1),lindet(3,1)
               mset(i,lindet(4,1))=perimsg(ndet)
               ndet=ndet-1
            enddo
c
            do i=lindet(2,1)+1,lindet(4,1)-1
               mset(lindet(1,1),i)=perimsg(ndet)
               ndet=ndet-1
            enddo
c
            do i=lindet(2,1)+1,lindet(4,1)-1
               mset(lindet(3,1),i)=perimsg(ndet)
               ndet=ndet-1
            enddo
c
c
         else
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                      Start of brand-new work from the Master         c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      Calculate the perimeter, after assigning I*2 values to I*4
c
            ix1    =lindet(1,1)
            iy1    =lindet(2,1)
            ix2    =lindet(3,1)
            iy2    =lindet(4,1)
            miniter=lindet(5,1)
c
c      Increase the limit value by miniter (the perimeter minimum)
c
            if(.not.liminc.and.miniter.lt.limit+1
     *        )then
                   liminc=.true.
                   limit=limit+miniter
                   if(check
     *               )then
                          write(txtout,112)miniter,limit
                          call statout
                   endif
            endif
c 
            call column(ix1,iy1,  iy2,  miniter)
            call column(ix2,iy1,  iy2,  miniter)
            call liner (iy1,ix1+1,ix2-1,miniter)
            call liner (iy2,ix1+1,ix2-1,miniter)
c
c      Check our new box isn't in a constant colour area....
c
            if(check
     *        )then
                   write(txtout,146)ix1,iy1,ix2,iy2,miniter
                   call statout
            endif
c
            call minit (ix1,iy1,ix2,iy2,miniter)
            if(iso_mpi_term)return
            if(.not.newline
     *        )then
                   mline=0
                   go to 14
            endif
c
         endif
      endif
c
c        We have work, but does anyone else need (or can be given) some?
c
      if(loadbal)then
       if(need)then
c
c           Yes, but do we have enough to spare?
c
              if(check.and.need
     *          )then
                     txtout='Plea for work received'
                     call statout
              endif
c 
            if(mline.gt.1
     *        )then
c
c           ....yes, so find the biggest box and send it.
c
               mx=0
               do i=1,mline
                  if(lindet(3,i)-lindet(1,i).gt.mx
     *              )then
                         mx=lindet(3,i)-lindet(1,i)
                         maxline=i
                  endif
               enddo
c
               if(check
     *           )then
                      do i=1,mline
                         if(i.eq.maxline
     *                     )then
                                write(txtout,149)i,(lindet(mx,i),mx=1,5)
                            else
                                write(txtout,148)i,(lindet(mx,i),mx=1,5)
                         endif
                         call statout
                      enddo   
               endif       
c
c      Put data from lindet(*,maxline) into perimsg for transmission
c                        and mset
c      Store as:
c      Perimeter data, source, miniter, iy2, ix2, iy1, ix1
c
               ndet=0
c
               ix=lindet(1,maxline)-1
               iy=lindet(2,maxline)
               n =lindet(3,maxline)-lindet(1,maxline)+1
c
               do i=1,n
                  ndet=ndet+1
                  perimsg(ndet)=mset(i+ix,iy)
               enddo
c
               iy=lindet(4,maxline)
               do i=1,n
                  ndet=ndet+1
                  perimsg(ndet)=mset(i+ix,iy)
               enddo
c
               n =lindet(4,maxline)-lindet(2,maxline)-1
               iy=lindet(2,maxline)
c
               ix=lindet(1,maxline)
               do i=1,n
                  ndet=ndet+1
                  perimsg(ndet)=mset(ix,i+iy)
               enddo
c
               ix=lindet(3,maxline)
               if(check
     *           )then
                      write(txtout,14500)
                      call statout
                      write(txtout,145)ix,iy,n,ndet
                      call statout
               endif
c
               do i=1,n
                  ndet=ndet+1
                  perimsg(ndet)=mset(ix,i+iy)
               enddo
c
               perimsg(ndet+1)= taskid
c
               mx=ndet+7
               do i=1,5
                  perimsg(mx-i)=lindet(i,maxline)
                  lindet(i,maxline)=lindet(i,mline)
               enddo
               nbytes=2*ndet+12
c
               ntag=msglow+3
               call MPI_send(perimsg,nbytes/2,MPI_INTEGER4
     *                      ,master,ntag,icomm,ierror)
c
               if(check
     *           )then
                      write(txtout,100)maxline,mline
                      call statout
                      write(txtout,113)(perimsg(mx),mx=ndet+6,ndet+2,-1)
                      call statout
               endif
c
               mline=mline-1
               need=.false.
c
               if(check
     *           )then
                      do i=1,mline
                         write(txtout,148)i,(lindet(mx,i),mx=1,5)
                         call statout
                      enddo   
               endif       
            endif
       else    
            if(mod(nboxsel,20).eq.2)then
c
c      Set up a non-blocking probe for 'more work' requests
c
               ntag=msglow+6
               call MPI_iprobe(master,ntag,icomm,msgneed,istatus,ierror)
c
               if(msgneed)then
c
c      ...receive it...
c
                              call MPI_recv(needx,1,MPI_LOGICAL
     *                                ,master,ntag,icomm,istatus,ierror)
                              call lts(nbytes)
                              need=.true.
                              if(check)then
                                           write(txtout,114)
                                           call statout
                              endif
               endif
c
            endif
       endif
      endif
c
c      Assign parameters from stored line...
c
      ix1=lindet(1,mline)
      iy1=lindet(2,mline)
      ix2=lindet(3,mline)
      iy2=lindet(4,mline)
      miniter=lindet(5,mline)
      mline=mline-1
      nboxsel=nboxsel+1
c
 13   continue
c
      gtemp(1)=ix1
      gtemp(2)=iy1
      gtemp(3)=ix2
      gtemp(4)=iy2
      gtemp(5)=miniter
c     call MPI_send(gtemp,5,MPI_INTEGER4,artist,msglow+9,icomm,ierror)
      call box(ix1,iy1,ix2,iy2,miniter)
      go to 14
c
999   continue
      write(txtout,150)
      call statout
      return
c
100   format('Stack item',i4,' of ',i4,' sent as work         ')
101   format('Work:',6i6,f10.3)
1011  format('Peri:',6i6,f10.3)
102   format('   Idle:',5i9)
1021  format('Waiting:',5i9)
103   format('Work wanted, not type',i4,' ex',i4,' length',i6)
104   format('Excess iteration count is',i10,'         ')
105   format('NDET=',i8,', PERIM=',i8,', NX=',i7,', NY=',i7)
1051  format('NDET=',i8,',*PERIM=',i8,', NX=',i7,', NY=',i7)
1052  format('PERIM=',4i7,', LINDET(5,1)=',i7)
106   format('Stats',5f8.1)
107   format('Have taken look',i3,' for need - MSGNEED=',i7)
108   format('NBOXSEL=',i4,', NBYTES=',i8,', MSGNEED=',i6)
109   format('Parms message?    Type',i3,' ex',i4,' length',i8)
110   format('Parms wanted, not type',i3,' ex',i4,' length',i8)
1101  format('Parms received: type',i3,' ex',i4,' length',i8)
1102  format('Recvd MasterID: type',i3,' ex',i4,' length',i8)
111   format('******* Unsatisfied request for work removed')
112   format('Limit value increased by',i6,' to',i6)
113   format(6i6)
114   format('Non-blocking probe has seen request for work')
115   format('Zero-length msg: type',i4,' ex',i4,' length',i6)
134   format('Perimeter section completed, min value',i7)
135   format('Calc perimeter at Y=',i6,': X=',i6,' to',i6)
136   format('Calc perimeter at X=',i6,': Y=',i6,' to',i6)
137   format('MSET col ',6i6)
138   format('MSET line',6i6)
139   format('MSET work',6i6)
140   format('Main subroutine entered.')
141   format('MPI_ANY_SOURCE=',i4,', MPI_ANY_TAG=',i4)
142   format('Sleeping for ',i0.0,' seconds',i8)
144   format('Params received: Master is',i3,', Artist is',i3)
14500 format('                 ix        iy         n      ndet')
145   format('Giftwork:',4i10)
146   format('ISOSLAVE MINIT',5i8)
147   format('No more quiescent msgs from task',i4)
148   format(' Stack:',6i8)
149   format('*Stack:',6i8)
150   format('Goodbye from this ISOSLAVE')
240   format(' Pre-iprobe in loop 50')
241   format('Post-iprobe in loop 50')
242   format('Post-msgneed test in loop 50')
243   format('msgneed false in loop 50')
246   format('Sent to BOX',5i8)
247   format('BOX completed ',8i5)
c
      end
