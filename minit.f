      subroutine minit(ix1,iy1,ix2,iy2,miniter)
c
c      Finds minimum iterations for a block...
c
      use isocomm
c
      miniter=10000000
      maxiter=0
      jx1=ix1+1
      jx2=ix2-1
      jy1=iy1+1
      jy2=iy2-1
c
c      Look at the X-perimeter....
c
      do ix=ix1,ix2
         if(mset(ix,iy1).lt.miniter)miniter=mset(ix,iy1)
         if(mset(ix,iy1).gt.maxiter)maxiter=mset(ix,iy1)
         if(mset(ix,iy1).lt.0
     *     )then
                write(     0,100)ix,iy1,mset(ix,iy1)
                write(txtout,100)ix,iy1,mset(ix,iy1)
                call statout
                iso_mpi_term=.true.
                return
         endif
         if(mset(ix,iy2).lt.miniter)miniter=mset(ix,iy2)
         if(mset(ix,iy2).gt.maxiter)maxiter=mset(ix,iy2)
         if(mset(ix,iy2).lt.0
     *     )then
                write(     0,100)ix,iy2,mset(ix,iy2)
                write(txtout,100)ix,iy2,mset(ix,iy2)
                call statout
                iso_mpi_term=.true.
                return
         endif
      enddo
c
c      Look at the Y-perimeter...
c
      do iy=jy1,jy2
         if(mset(ix1,iy).lt.miniter)miniter=mset(ix1,iy)
         if(mset(ix1,iy).gt.maxiter)maxiter=mset(ix1,iy)
         if(mset(ix1,iy).lt.0
     *     )then
                write(     0,100)ix1,iy,mset(ix1,iy)
                write(txtout,100)ix1,iy,mset(ix1,iy)
                call statout
                iso_mpi_term=.true.
                return
         endif
         if(mset(ix2,iy).lt.miniter)miniter=mset(ix2,iy)
         if(mset(ix2,iy).gt.maxiter)maxiter=mset(ix2,iy)
         if(mset(ix2,iy).lt.0
     *     )then
                write(     0,100)ix2,iy,mset(ix2,iy)
                write(txtout,100)ix2,iy,mset(ix2,iy)
                call statout
                iso_mpi_term=.true.
                return
         endif
      enddo
c
c               write(txtout,102)ix1,iy1,ix2,iy2,miniter,maxiter
c               call statout
c
c      If min and max are the same, all the points inside the box
c      are the same, so say so.    MAYBE... 11th August 2021...
c
      if(miniter.ne.maxiter
     *  )then
             newline=.true.
c
c      Are we at finest sub-division?
c
             if(iy2.le.iy1+ldiff.or.
     *          ix2.le.ix1+ldiff
     *         )then
                    miniter_ext=miniter ! Original prediction
c
c      This is the smallest box, with different value points inside it
c      so calculate them now...
c
                    do ix=jx1,jx2
                       call column(ix,jy1,jy2,miniter_ext)
                    enddo
                    newline=.false.
c
             min_internal=minval(mset(Ix1+1:ix2-1,iy1+1:iy2-1))
CXX          if(min_internal.lt.miniter_ext   ! Failure of boundary prediction
CXX  *         )then
CXX                 write(80+taskid,103)iy1,iy2,miniter_ext,min_internal
CXX                 write(txtout,1031)iy1,iy2,miniter_ext,min_internal
CXX                 call statout
CXX                 do ix=ix1,ix2
CXX                    write(80+taskid,104)ix,(mset(ix,iy1:iy2))
CXX                 enddo
CXX             endif
             endif
c
         else
c     write(txtout,101)ix1,iy1,ix2,iy2,miniter
c     call statout
             call filler(ix1,iy1,ix2,iy2,miniter)
             newline=.false.
      endif
c
      return
100   format('ERR -VE MSET:',2i5,i20)
101   format('Call Filler:',6i5)
102   format('Perimeter:'6i7)
103   format(/'Y limits:',2i6,'. Expected:',i6,', Actual:',i6)
1031  format( 'Y range:' ,i13,' -',i4'. Exp',i6,', Actual:',i6)
104   format('MSET at IX=',i6,':',20i6)
105   format(/'X limits for column:',2i6)
106   format('Y limits for column:',2i6)
      end
