      subroutine equalcol(ncolindex,cyclic)
c
c      Builds an equal-area colour map.   ncolindex is the number of
c      colours in the available palette.    Two of these are reserved
c      as 'Black' and 'White', though they could be any pair of colours.
c
      use isocomm
c
c      Count the paintable, filled iteration levels
c
      logical cyclic
c
      integer(4) npix,npixels,nscreenpixels,levtot
c
      npixels=0
      nlevels=0
      nscreenpixels=ixm*iym
c     write(     *,106)iter_lo,iter_hi,nscreenpixels
c     write(lchann,106)iter_lo,iter_hi,nscreenpixels
c
      if(nscreenpixels.le.0.or.
     *         iter_hi.le.0.or.
     *       ncolindex.le.0
     *  )then
             write(0     ,111)ixm,iym,nscreenpixels,iter_hi,ncolindex
             write(lchann,111)ixm,iym,nscreenpixels,iter_hi,ncolindex
             iso_mpi_term=.true.
             return              ! Return in a panic...
          else
             if(check
     *         )then
                    write(txtout,112)
                    call statout 
             endif
      endif
c
      do n=iter_lo,iter_hi
         if(kdpel(n).gt.0    ! kdpel(n) is the number of pixels at iteration count n
     *     )then
                nlevels=nlevels+1
                npixels=npixels+kdpel(n)
         endif
      enddo
c
      if(nlevels.eq.0
     *  )then
c
c    Nothng to be painted...!!
c
             write(lchann,110)iter_lo,iter_hi
c            write(0,110)iter_lo,iter_hi
             return
      endif
c
      n_palette=2*ncolindex
c     write(lchann,105)npixels,n_palette,nlevels,iter_lo,iter_hi
c
      if(nlevels.le.n_palette
     *  )then
c            if(nlevels.lt.n_palette)write(lchann,109)nlevels
             n_palette=nlevels
             ncell_count=iterdist(1)
         else
c
             medlev=2.0*real(npixels)/real(n_palette)
c
c      Have to do a binary chop here...
c
             ncell_count=0
             n1=medlev
             n2=1
             m1=iterdist(n1)
             m2=iterdist(n2)
             if((n_palette-m1)*(n_palette-m2).gt.0
     *         )then
c   
c      We'll never converge!
c
c                   write(     0,108)n1,n2,m1,m2,n_palette
                    write(lchann,108)n1,n2,m1,m2,n_palette
                    iso_mpi_term=.true.
                    return
             endif
c
             msign=m1-n_palette
c            write(lchann,101)n1,n2,n,ncell_count,m1-ncell_count,msign
             call isoflush(lchann)
  1          continue
             n=(n1+n2)/2
             ncell_count=iterdist(n)
c            write(lchann,101)n1,n2,n,ncell_count
c    *                        ,n_palette-ncell_count,msign
             if(ncell_count.ne.n_palette
     *         )then
                    if((n_palette-ncell_count)*msign.gt.0
     *                )then
                           n2=n
                       else
                           n1=n
                           nx=n
                    endif
c
                    if(n1-n2.gt.1
     *                )then
                           go to 1
                    endif
             endif
             ncell_count=iterdist(nx)
      endif
c
c     if(n_palette.eq.ncell_count
c    *  )then
c            write(lchann,1031)n_palette
c        else
c            write(lchann,103 )n_palette,ncell_count
c     endif
c
c     call celldist(ncell_count)
c
c      Fill in the colours in the paint box
c
c      Set the 'infinite' iterations to be colour zero...
c
      itermin(0)=limit+1
      itermax(0)=limit+1
c
      npix=0
      rnp=1./n_palette
      b=ncolindex
      do id_palette=0,ncell_count
         levtot=0
         ncol=.5+(b*(1.-(2.*sqrt(((rnp*real(id_palette))-.5)**2))))
c
         nempty=0
         do m=itermin(id_palette),itermax(id_palette)
            paint(m)=char(ncol)
            levtot=levtot+kdpel(m)
            if(kdpel(m).eq.0
     *        )then
                   nempty=nempty+1
            endif
         enddo
c
         npix=npix+levtot
c        if(itermin(id_palette).eq.itermax(id_palette)
c    *     )then
c                write(lchann,1041)ncol,id_palette,itermin(id_palette)
c    *                                            ,levtot,npix
c           else
c                write(lchann,104 )ncol,id_palette,itermin(id_palette)
c    *                                            ,itermax(id_palette)
c    *          ,levtot,nempty,npix
c        endif
       enddo
c
       if( npix.ne.nscreenpixels
     *   )then
c
c      We have not mapped all the iterations, or over-mapped them...
c
              write(0     ,107)npix,nscreenpixels,ixm,iym,n_palette
     *                        ,ncell_count,iter_lo,iter_hi,limit
              write(lchann,107)npix,nscreenpixels,ixm,iym,n_palette
     *                        ,ncell_count,iter_lo,iter_hi,limit
c             iso_mpi_term=.true.
c             return
      endif
c
      call isoflush(lchann)
      return
c
100   format('MAX_LEVEL=',i6)
101   format('N1:',i8,', N2:',i8,', N:',i8,', COLOURS:',i6,2i10)
1011  format('N1:',i8,', N2:',i8,', M1:',i8,', M2:',i8)
102   format('NPIXELS:',i8)
103   format('Requested',i6,' and was given',i6,' colour slots')
1031  format('Requested and received',i6,' colour slots')
104   format('Colour',i8,': PaletteID',i6,', iterations',i6,' to',i6
     *      ,' used by',i10,' pixels. Empty levels:',i13,i14)
1041  format('Colour',i8,': PaletteID',i6,', iteration ',i6,9x
     *      ,' used by',i10,' pixels. ',i40)
1042  format('Paintbox colour',i4,', iterations',i6,' to',i6)
105   format(/'EQUALCOL: Spreading',i10,' pixels over',i8
     *       ,' palette entries.'
     *     ,/'           ',i8,' Iteration levels from',i8,' to',i8)
106   format('Entering EQUALCOL with ITER_LO=',i8,' and ITER_HI=',i8
     *      ,'.  Screen pixels=',i12)
107   format(/'##### ERROR IN EQUALCOL: Pixels mapped',i12
     *       ,' <> screen pixels',i12,' (IXM=',i8,' X',i8,'=IYM)'
     *  ,/25x,'Palette',i8,', cells',i8
     *  ,/25x,'ITER_LO=',i8,', ITER_HI=',i8,', LIMIT=',i8
     *     ,/)
108   format('##### ERROR IN EQUALCOL - NO CONVERGENCE POSSIBLE',5i10)
109   format('          N_PALETTE changed to',i8,/)
110   format('##### ERROR IN EQUALCOL - NO FILLED ITERATION LEVELS'
     *      ,' BETWEEN',i8,' AND',i8)
111   format('##### ERROR IN EQUALCOL: ',i0.0,' x ',i0.0
     *      ,', nscreenpixels= ',i0.0,', iter_hi= ',i0.0
     *      ,', ncolindex= ',i0.0)
112   format('EQLCL: Count paintable, filled iteration lvls')
1121  format(i6,'x',i6,' iter_hi',i6,' pixels',i11)
c
      end
