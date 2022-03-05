      subroutine deconx(nslave)
c
      use isocomm
c
c      This subroutine a} accumulates the distribution of number of pixels
c      with particular iteration counts; and b) fills the array mset with
c      the iteration count associated with each co-ordinate.
c
      integer x_prim_type
c
      logical new_iter_limit
c
      npixels=ngrafout(1)
      if(ngrafout(1).lt.0
     *  )then
             write(0,101)ngrafout(1),nslave
             iso_mpi_term=.true.
             return
      endif
c
c     write(0,800)(ngrafout(mx),mx=1,200)
c800   format(10(20i6,/))
c
      if(check
     *  )then
             write(txtout,111)ngrafout(1)
             call statout
      endif
c
c     write(*,110)lchann
      ldata=lchann+nslave
c
c      This is how the data came from liner/column
c                             liner  column
c-data      ngrafout(ngline+1)=ix1    ix
c-data      ngrafout(ngline+2)=iy     iy1
c-data      ngrafout(ngline+3)=-1     -2
c-data      ngline=ngline+3
c
c      A line from column is thus:
c
c  X Y1 -2  N  -colour  N -colour  N -colour -colour -colour N  -colour
c           ========== ========== ========== ======= ======= ==========
c  X Y1 -2     -colour    -colour  N -colour -colour -colour N  -colour
c
c       Two positive numbers in succession is thus start of new line.
c       Third number must be -1 (row) or -2 (column)
c
 
c
c
      nupels=0
      m=1
  1   continue
c
      if(ngrafout(m+1).gt.0
     *  )then
            if(ngrafout(m+2).gt.0
     *        )then
                  if(ngrafout(m+3).gt.0
     *              )then
c
c      This is a rectangle to fill
c
                        ix1 =ngrafout(m+1)
                        iy1 =ngrafout(m+2)
                        ix2 =ngrafout(m+3)
                        iy2 =ngrafout(m+4)
                        iter=-ngrafout(m+5)
                        m=m+5
                        x_prim_type=2
                        go to 20
                  else
c
c      This is the start of a line or column
c
                        if(ngrafout(m+3).lt.-2
     *                    )then
                              write(lchann,108)(ngrafout(mx),mx=m-6,m+3)
                               write(txtout,107)
                               call statout
                               call isoflush(lchann)
                               return
                        endif
c
c      Row or column?
c
                        if(ngrafout(m+3).eq.-1
     *                    )then
                               row=.true.
                               ix2=ngrafout(m+1)-1
                               iy1=ngrafout(m+2)
                               iy2=iy1
                           else
                               row=.false.
                               ix1=ngrafout(m+1)
                               ix2=ix1
                               iy2=ngrafout(m+2)-1
                        endif
                        m=m+3
                        go to 1
                  endif
c
            else
c
c      This is a run of pels in a single row or column
c
                  if(row
     *              )then
                         ix1=ix2+1
                         ix2=ix2+ngrafout(m+1)
                     else
                         iy1=iy2+1
                         iy2=iy2+ngrafout(m+1)
                  endif
                  iter=-ngrafout(m+2)
                  m=m+2
                  x_prim_type=1
                  go to 20
            endif
c
      else
c
c      This is a singleton pel, continuing the row or column
c
            iter=-ngrafout(m+1)
            m=m+1
c
            if(row
     *        )then
                   ix1=ix2+1
                   ix2=ix1
               else
                   iy1=iy2+1
                   iy2=iy1
            endif
            x_prim_type=0
            go to 20
      endif
c
 20   continue
c
c      Fill the mset array...
c
      mset(ix1:ix2,iy1:iy2)=iter
c
      if(screen_graphics.and.
     *   n_x_prim.lt.max_x_prim
     *  )then
             n_x_prim=n_x_prim+1
             x_prim(n_x_prim,1)=ix1
             x_prim(n_x_prim,2)=iy1
             x_prim(n_x_prim,3)=ix2
             x_prim(n_x_prim,4)=iy2
             x_prim(n_x_prim,5)=iter
             x_prim(n_x_prim,6)=source
             x_prim(n_x_prim,7)=x_prim_type
      endif
c      
      if(n_x_prim.eq.max_x_prim
     *  )then
             write(0,112)max_x_prim
      endif
c
c      Count the pels...
c
      kcpels=(ix2-ix1+1)*(iy2-iy1+1)
      nupels=nupels+kcpels
c
c     if(check
c    *  )then
c            write(60,113)n_x_prim,(x_prim(n_x_prim,mx),mx=1,7)
c    *                   ,kcpels,nupels
c            call isoflush(60)
c     endif
c
      if(iter.gt.limit+1.or.iter.lt.0
     *  )then
             write(     0,109)iter,ix1,iy1,ix2,iy2,kcpels,nupels
             write(lchann,109)iter,ix1,iy1,ix2,iy2,kcpels,nupels
             call isoflush(lchann)
             return
      endif
c
      if(iter.le.limit.and.
     *   iter.gt.iter_hi
     *  )then
             iter_hi=iter
             if(check
     *         )then
                    write(txtout,1041)iter_lo,iter_hi,nupels
                    call statout
             endif
      endif
c
      if(iter.lt.iter_lo
     *  )then
             iter_lo=iter
             if(check
     *         )then
                    write(txtout,1042)iter_lo,iter_hi,nupels
                    call statout
             endif
      endif
c
      kdpel(iter)=kdpel(iter)+kcpels
c
      if(nupels.eq.npixels
     *  )then
             if(check
     *         )then
                    write(txtout,103)nupels
                    call statout
                    if(screen_graphics
     *                )then
                           write(txtout,1031)n_x_prim
                           call statout
                    endif
             endif
             call isoflush(lchann)
             return
         else
             go to 1
      endif
c
100   format('##### Error in DECONX: rectangle is',5i7
     *     ,/'                 Pels reconstructed=',i8
     *     ,/'                 Elements examined =',i8
     *     ,/'                 Last elements',/10i7,/)
101   format('##### Error in DECONX: NGRAFOUT(1)=',i10
     *     ,' pixels from slave',i3)        
102   format('##### Error in DECONX: NCOL=',i6)         
103   format(i14,' pels reconstructed in DECONX')
1031  format(i14,' X-primitives now on record')
1041  format('DECONX - ITERMIN',i6,', MAX',i8,', NUPELS=',i8,' hi')
1042  format('DECONX - ITERMIN',i6,', MAX',i8,', NUPELS=',i8,' lo')
107   format('            **** ERROR IN DECONX ****        ')
108   format('##### DECONX: last elements',/10i6)
109   format('##### DECONX: ITER=',i6,' at',6i6)         
110   format('##### DECONX: LCHANN=',i3)
111   format( 'NGRAFOUT(1)=',i10)
112   format('##### DECONX: Buffer full of',i8,' X-primitives.')
113   format('X_PRIM:',10i8)
      end
