      program sudoku
c
c      Interactive version
c
      use sudoku_comms
c
c      Initial data entry
c
      nsqrowmin(1)=1
      nsqrowmin(2)=1
      nsqrowmin(3)=1
      nsqrowmin(4)=4
      nsqrowmin(5)=4
      nsqrowmin(6)=4
      nsqrowmin(7)=7
      nsqrowmin(8)=7
      nsqrowmin(9)=7
c
      nsqcolmin(1)=1
      nsqcolmin(2)=4
      nsqcolmin(3)=7
      nsqcolmin(4)=1
      nsqcolmin(5)=4
      nsqcolmin(6)=7
      nsqcolmin(7)=1
      nsqcolmin(8)=4
      nsqcolmin(9)=7
c
      poss=.true.
      known=0
      nknowns=0
      nsteps=0
      do nrow=1,9
         read(3,100)numline
         do ncol=1,9
            idn=numline(ncol)
            if(idn.gt.0)call sud_process_entry(nrow,ncol,idn,nexist)
            if(proc_entry_error
     *        )then
                   write(0,104)nrow,ncol,idn,nexist
                   stop
            endif
         enddo
      enddo
      write(*,102)
      read(*,*)check
      call sud_display
c
c      Save statuses
c
      possa=poss
      knowna=known
      nknownsa=nknowns
c
      nhelp=6
   1  continue
c
      nfixes=0
c
      if(nhelp.gt.1
     *  )then
c
c      Check against rows amd columns
c     
             call sud_loners
      endif
c
c      When we retrun from sud_loners, is it with an error, or because
c      we've run out of ideas?
c
      if(sud_error
     *  )then            ! Restore previous states
             poss=possa
             known=knowna
             nknowns=nknownsa
      endif
c
      if(nfixes.eq.0.and.
     *   nknowns.lt.81
     *  )then
c
c      Save statuses
c
             possa=poss
             knowna=known
             nknownsa=nknowns
c
c      Seek keyboard input...
c     
             call sud_small_display
   2         continue
             write(*,101)
             read(*,100)nrow,ncol,idn
             write(*,103)nrow,ncol,idn
             call sud_process_entry(nrow,ncol,idn,nexist)
             if(proc_entry_error
     *         )then
                    write(0,104)nexist
                    go to 2
             endif
      endif
c
      go to 1
c
      stop
c
100   format(9i1)
101   format('Keyboard entry - row/col/N, 3I1, rcN:')
102   format('Check? T/F')
103   format('GUESS: Row ',i1,', Col ',i1,', Digit ',i1)
104   format('ERROR creating initial pattern - trying to over-write',i2)
c
      end
