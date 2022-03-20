      subroutine validate(ix1,iz1,ix2,iz2,nx)
c
c      Checks the corners of the new cube
c
      use isocomm
c
      if((ix2.le.ix1).or.(iz2.le.iz1)
     *  )then
             write(     *,101)ix1,iz1,ix2,iz2,nx
             write(txtout,101)ix1,iz1,ix2,iz2,nx
             call statout
             return
      endif
c
      do npx=1,nprocmax
         do ncx=1,nactive(npx)
            nc=listact(ncx,npx)
            npos=0
c
c      Test if the (x1,z1) vertex is inside square NC
c
                          if(
     *                       (ix1.gt.nx1(nc).and.ix1.lt.nx2(nc))
     *                  .and.(iz1.gt.nz1(nc).and.iz1.lt.nz2(nc))
     *                      )npos=npos+1
c
c      Test if the (x2,z2) vertex is inside square NC
c
                          if(
     *                       (ix2.gt.nx1(nc).and.ix2.lt.nx2(nc))
     *                  .and.(iz2.gt.nz1(nc).and.iz2.lt.nz2(nc))
     *                      )npos=npos+1
c
c      Test if the (x2,z1) vertex is inside square NC
c
                          if(
     *                       (ix2.gt.nx1(nc).and.ix2.lt.nx2(nc))
     *                  .and.(iz1.gt.nz1(nc).and.iz1.lt.nz2(nc))
     *                      )npos=npos+1
c
c      Test if the (x1,z2) vertex is inside square NC
c
                          if(
     *                       (ix1.gt.nx1(nc).and.ix1.lt.nx2(nc))
     *                  .and.(iz2.gt.nz1(nc).and.iz2.lt.nz2(nc))
     *                      )npos=npos+1
c
                          if(npos.gt.0.and.npos.lt.4
     *                   .and.ix1.ne.nx1(nc)
     *                   .and.ix1.ne.nx2(nc)
     *                   .and.ix2.ne.nx1(nc)
     *                   .and.ix2.ne.nx2(nc)
     *                   .and.iz1.ne.nz1(nc)
     *                   .and.iz1.ne.nz2(nc)
     *                   .and.iz2.ne.nz1(nc)
     *                   .and.iz2.ne.nz2(nc)
     *                      )then
c
c      We have an overlapping cube....  Disaster.
c
                              write(0,103)nc,npos,nx
     *                                  ,nx1(nc),nz1(nc),nx2(nc),nz2(nc)
     *             ,nx1(nc)+ixmh,nz1(nc)+iymh,nx2(nc)+ixmh,nz2(nc)+iymh
     *                                  ,ix1    ,iz1    ,ix2    ,iz2
     *                             ,ix1+ixmh,iz1+iymh,ix2+ixmh,iz2+iymh
c                             lretcode=.false.
                              return
                           endif
c
         enddo
      enddo
      return
c
101   format('VALIDN_ERROR: X1,Z1,X2,Z2,NX:',5i5)
103   format('***** VALIDATION ERROR: Cube',i4
     *      ,' (NPOS=',i1,', NX=',i3,')'
     *     ,/'                                ',4i5
     *     ,/'       original data were....   ',4i5
     *     ,/'      is intersected by new cube',4i5
     *     ,/'       original data were....   ',4i5
     *     ,/) 
c
      end
