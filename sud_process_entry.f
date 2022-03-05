      subroutine sud_process_entry(nrow,ncol,idn,nexist)
c      
      use sudoku_comms
c
      if(idn.le.0.or.
     *   known(nrow,ncol).gt.0
     *  )then
c
c      We have an invalid entry...
c
                     write(*,101)idn,nrow,ncol,known(nrow,ncol)
                     nexist=known(nrow,ncol)
                     proc_entry_error=.true.
                     return
                 else
                     known(nrow,ncol)=idn
                     nknowns=nknowns+1
c                            IDN    1,2,3,4,5,6,7,8,9                     
                     k=(idn-1)/3  ! 0,0,0,1,1,1,2,2,2
                     l= idn-(k*3) ! 1,2,3,1,2,3,1,2,3
                     k=k+1        ! 1,1,1,2,2,2,3,3,3
                     proc_entry_error=.false.
c                  write(*,*)nrow,ncol,k,l,idn
c
c      Take out the equivalent location in all other major rows/cols
c
                     do n=1,9
                        poss(n,ncol,k,l)=.false.
                        poss(nrow,n,k,l)=.false.
                     enddo
c
                    nsq=(3*((nrow-1)/3))+((ncol-1)/3)+1
                    poss(nsqrowmin(nsq):nsqrowmin(nsq)+2
     *                  ,nsqcolmin(nsq):nsqcolmin(nsq)+2,k,l)=.false.
                    poss(nrow,ncol,1:3,1:3)=.false.
c
      endif
c
            if(nknowns.eq.81
     *        )then
                   write(*,102)nknowns,nsteps
                   write(8,102)nknowns,nsteps
                   call sud_display
                   stop
           endif
      return
101   format(40x,i2,' Row',i2,', column',i2,' already fixed at',i2)
102   format(/' Known squares:',i3
     *      ,/'Solution steps:',i3,//)
      end
