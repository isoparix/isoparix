      integer function iterdist(medlev)
c
c      Record the colour distribution
c
      use isocomm
c
      itermin=1000000
      itermax=0
c
      npixels=0
      npixela=0
      ml=medlev
      nq=0
      ncell_count=1
      itermin(ncell_count)=0
      nlevels=0
c
      do n=iter_lo,iter_hi
         if(ncell_count.gt.1024
     *     )then
                exit
         endif
         npixela=npixela+kdpel(n)
         nlevels=nlevels+1
         if(kdpel(n).le.ml
     *     )then
                ml=ml-kdpel(n)
                nq=nq+kdpel(n)
            else
                if(kdpel(n).gt.medlev
     *            )then
                       nq=kdpel(n)
                       ml=0
                endif
         endif
c
         if(nq.gt.0.and.
     *     (ml.eq.0.or.kdpel(n+1).gt.ml.or.n.eq.iter_hi)
     *     )then
                itermax(ncell_count)=n
                ncell_levs(ncell_count)=nlevels
                ncell(ncell_count)=nq
                npixels=npixels+ncell(ncell_count)
c
c      Check we're catching everything...
c
                if(npixels.ne.npixela
     *            )then
                       levtot=0
                       write(*,100)n,kdpel(n),npixels,npixela,npixolda
     *                            ,ml,nq,kdpel(n+1)
                       do m=itermin(ncell_count),itermax(ncell_count)
                          levtot=levtot+kdpel(m)
                          write(*,101)m,kdpel(m),levtot
                       enddo
                endif
c
               if(n.lt.iter_hi
     *           )then
                      ncell_count=ncell_count+1
                      itermin(ncell_count)=n+1
                      ml=medlev
                      nq=0
                      nlevels=0
                      npixolda=npixela
               endif
        endif
      enddo
c
      iterdist=ncell_count
      return
c
100   format('##### ERROR IN ITERDIST: N=',i4,', KDPEL(N)=',i8
     *      ,', NPIXELS=',i8,', NPIXELA=',i8,', PREVIOUS=',i8
     *      ,', ML=',i6,', NQ=',i8,', KDPEL(N+!)=',i8)
101   format('##### ERROR IN ITERDIST: N=',i4,', KDPEL(N)=',2i8)
102   format('EQUALCOL: LIMIT=',i10,', NCELL_COUNT=',i10)
      end
