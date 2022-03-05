      subroutine sud_loners
c
c      Fixes loners
c
c      A return from this subroutine means either an error (bad guess or
c      bad logic), or that we've run out of ideas...
c
      use sudoku_comms
c
      logical :: sc,sr,fixed,cand_error
      integer :: nsqline(3)
      integer,dimension(100)  :: row_cand,col_cand,n_cand,id_val
      integer,dimension(1000) :: id_cand
c
      write(*,102)
  1   continue
      if(check
     *  )then
             write(*,108)
             call sud_small_display
      endif
      nflush=0
      ncandidates=0
      minor_count=0
      last_minor=0
      row_cand=0
      col_cand=0
       id_cand=0
        n_cand=0
      id_val=0
c
      do k=1,3                !       1 1 1 2 2 2 3 3 3
         do l=1,3             !       1 2 3 1 2 3 1 2 3
            idn=(3*(k-1))+l   !       1 2 3 4 5 6 7 8 9
c
c      Do this analysis by numbers...
c
                 rinc=0
                 cinr=0
                rinsq=0
                cinsq=0
             sq_count=0
            row_count=0
            col_count=0
       known_sq_count=0    ! The known counts are to check against
      known_row_count=0    ! bad  guesses or
      known_col_count=0    ! bad logic...
            sud_error=.false.
c
            do ncol=1,9
               nx=((ncol-1)/3)+1
               do nrow=1,9
                  nsq=(3*((nrow-1)/3))+nx ! nsq goes: 1,2,3,...,9
c
c      Error checking....
c      Finding out how many defined instances of IDN in 
c      each row, column, minor square
c
                  if(known(nrow,ncol).eq.idn
     *              )then   ! IDN is fixed (known) at this row and column
                          known_sq_count (nsq)= known_sq_count (nsq)+1
                         known_row_count(nrow)=known_row_count(nrow)+1
                         known_col_count(ncol)=known_col_count(ncol)+1
                         if(known_sq_count (nsq) .gt.1.or.
     *                      known_row_count(nrow).gt.1.or.
     *                      known_col_count(ncol).gt.1
     *                     )then
c
c          Error - more than one instance in row, col, or minor square
c
                                sud_error=.true.
                                write(0,103)idn,nsq ,known_sq_count(nsq)
     *                                       ,nrow,known_row_count(nrow)
     *                                       ,ncol,known_col_count(ncol)
                                call sud_display
                                stop  ! We've hit an error
                         endif
                  endif
c
                  if(poss(nrow,ncol,k,l)
     *              )then
                         minor_count(nrow,ncol)=minor_count(nrow,ncol)+1
                         minor_idn(nrow,ncol,minor_count(nrow,ncol))=idn
c                         
                         sq_count(nsq)=sq_count(nsq)+1
                         rinsq(nsq,sq_count(nsq))=nrow
                         cinsq(nsq,sq_count(nsq))=ncol
c
                         row_count(nrow)=row_count(nrow)+1
                         cinr(nrow,row_count(nrow))=ncol
c
                         col_count(ncol)=col_count(ncol)+1
                         rinc(ncol,col_count(ncol))=nrow
c
                   endif
               enddo
            enddo
c             
c      We now know the location of:
c        Every 'possible' within a given square by row and column
c        Every 'possible' within a given row by column
c        Every 'possible' within a given column by row
c             
c             
c      Use 'projections'....
c             
            do id=1,9
               ia=1+((id-1)/3)              !      1 1 1 2 2 2 3 3 3
c
               nsqline=0
               do iq=1,col_count(id)        !      1,2....col_count(id)
                  ik=1+((rinc(id,iq)-1)/3)  !      1 1 1 2 2 2 3 3 3  
                  nsqline(ik)=nsqline(ik)+1
               enddo
c
               do i=1,3                     !      1     2     3
                  ib=ia+(3*(i-1))           !      1 1 1 5 5 5 9 9 9
                  if(            1.lt.sq_count(ib).and.
     *                           4.gt.sq_count(ib).and.
     *               nsqline(i)   .eq.sq_count(ib).and.
     *               col_count(id).gt.sq_count(ib)
     *              )then   ! Going to flush digit from this column
                         write(*,1006)(3*(k-1))+l,id
                         do ir=1,9
                            if((3*((ir-1)/3))+ia.ne.ib
     *                        )then
                                   poss(ir,id,k,l)=.false.
                                   nflush=nflush+1
                            endif
                         enddo
                  endif
               enddo
c
               ia=3*((id-1)/3)
c
               nsqline=0
               do iq=1,row_count(id)
                  ik=1+((cinr(id,iq)-1)/3)
                  nsqline(ik)=nsqline(ik)+1
               enddo
c
               do i=1,3
                  ib=ia+i
                  if(            1.lt.sq_count(ib).and.
     *                           4.gt.sq_count(ib).and.
     *               nsqline(i)   .eq.sq_count(ib).and.
     *               row_count(id).gt.sq_count(ib)
     *              )then   ! Going to flush digit from this row
                         write(*,1005)(3*(k-1))+l,id
                         do ic=1,9
                            if(ia+((ic-1)/3)+1.ne.ib
     *                        )then
                                   poss(id,ic,k,l)=.false.
                                   nflush=nflush+1
                            endif
                         enddo
                  endif
               enddo
c
               if( sq_count(id).eq.1.and.
     *            known(rinsq(id,1),cinsq(id,1)).eq.0
     *           )then   !   Only 'possible' digit within a square
                      ncandidates=ncandidates+1
                      write(*,1001)id,idn,rinsq(id,1),cinsq(id,1)
                      row_cand(ncandidates)=rinsq(id,1)
                      col_cand(ncandidates)=cinsq(id,1)
                        n_cand(ncandidates)=idn
               endif
c             
               if(row_count(id).eq.1.and.
     *            known(id,cinr(id,1)).eq.0
     *           )then   !   Only 'possible' digit within a row
                      ncandidates=ncandidates+1
                      write(*,1002)id,idn,id,cinr(id,1)
                      row_cand(ncandidates)=id
                      col_cand(ncandidates)=cinr(id,1)
                        n_cand(ncandidates)=idn
               endif
c             
               if(col_count(id).eq.1.and.
     *            known(rinc(id,1),id).eq.0
     *           )then   !   Only 'possible' digit within a column
                      ncandidates=ncandidates+1
                      write(*,1003)id,idn,rinc(id,1),id
                      row_cand(ncandidates)=rinc(id,1)
                      col_cand(ncandidates)=id
                        n_cand(ncandidates)=idn
               endif
            enddo
         enddo
      enddo
c
c      Fix singletons - only 'possible' number in a location
c      (probably scavenging exercise...)
c
      do nrow=1,9
         do ncol=1,9
            if(minor_count(nrow,ncol).eq.1.and.
     *               known(nrow,ncol).eq.0
     *        )then   !   Only 'possible' digit at (nrow,col)
                   ncandidates=ncandidates+1
                   write(*,1004)minor_idn(nrow,ncol,1),nrow,ncol
                   row_cand(ncandidates)=nrow
                   col_cand(ncandidates)=ncol
                     n_cand(ncandidates)=minor_idn(nrow,ncol,1)
            endif
         enddo
      enddo
c
      do n=1,ncandidates
         if(row_cand(n).ne.0
     *     )then
                nconsol=1
                do m=1+n,ncandidates
                   if(row_cand(n).eq.row_cand(m).and.
     *                col_cand(n).eq.col_cand(m).and.
     *                  n_cand(n).eq.  n_cand(m)
     *               )then
                          nconsol=nconsol+1
                          row_cand(m)=0
                   endif
                enddo
                write(*,1007)n_cand(n),row_cand(n),col_cand(n),nconsol
                call sud_process_entry(row_cand(n),col_cand(n)
     *                                  ,n_cand(n),nexist)
                if(proc_entry_error
     *            )then
                       if(nexist.ne.n_cand(n)
     *                   )then
                              write(*,106)nexist
                       endif
                   else
                       nfixes=nfixes+1
                endif
         endif
      enddo
c
       if(ncandidates.gt.0.or.
     *    nflush.gt.0
     *   )then
              go to 1
       endif 
c
       write(*,109)
       call sud_display
       if(sud_error)stop
       return     ! We've run out of ideas....
c
            
c
100   format('Fixed',i3,' loners - scanning again')
1001  format('Square',i2,': Digit',i2,' at row',i2,' column',i2)
1002  format('   Row',i2,': Digit',i2,' at row',i2,' column',i2)
1003  format('Column',i2,': Digit',i2,' at row',i2,' column',i2)
1004  format(    'Single  : Digit',i2,' at row',i2,' column',i2)
1005  format('Flushing: Digit',i2,' at row',i2)
1006  format('Flushing: Digit',i2,' at       column',i2)
1007  format('Fixing:   Digit',i2,' at row',i2,' column',i2
     *      ,' (x',i1,')')
102   format(/'LONER CHECK',/)
103   format('LONER:  Bad guess (or bad logic...).  Error with',i2
     *     ,/'        Square:',i2,', count=',i2
     *     ,/'           Row:',i2,', count=',i2
     *     ,/'        Column:',i2,', count=',i2
     *     ,/)
104   format('ERROR: Row ',i1,', col ',i1,' has',i3,' candidates')
106   format('              ERROR: Trying to over-write existing ',i1)
107   format('ERROR: Trying to replicate existing ',i1)
108   format(/'Start of loner')
109   format(/'End of loner')
110   format(10i3)
300   format('nsq=',i2,', nrow=',i2,', ncol=',i2,', idn=',i2)
301   format('Possible positions for',i2
     *    ,//3(3(3(3l1,'|'),/),3('---+'),/))
302   format('Row',i2,' holds all',i2,' entries for square',i2)
303   format('Col',i2,' holds all',i2,' entries for square',i2)
c
       end
