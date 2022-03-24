      subroutine cubesplit(nprocess,ix1,iz1,ix2,iz2,iheight)
c
c      Determines if a new cube is enclosed by an old and active one.  
c      If so, and it's a different process, splits up 'old and active'
c      in to up to 8 parts, surrounding the new cube
c
      use isocomm
c
      dimension newcube(8)
c
      if(check
     *  )then
             write(lchann,108)nprocess,nprocmax 
             call isoflush(lchann)
      endif
      if(nprocess.gt.nprocmax)nprocmax=nprocess
      newcube=0
      nnew=0
      nx=-1
      karea=huge(karea)
      if(check
     *  )then
             write(lchann,104)nprocess,ix1,iz1,ix2,iz2
     *                      ,(nactive(mx),mx=1,nprocmax)
             call isoflush(lchann)
      endif
      do npx=1,nprocmax
         do ncx=1,nactive(npx)
            nc=listact(ncx,npx)
            if(check
     *        )then
                   write(lchann,1041)nc,nx1(nc),nz1(nc),nx2(nc),nz2(nc)
                   call isoflush(lchann)
            endif
c
            if(
     *              (ix1.ge.nx1(nc).and.ix2.le.nx2(nc))
     *         .and.(iz1.ge.nz1(nc).and.iz2.le.nz2(nc))
     *         )then
                    if(narea(nc).lt.karea
     *                )then
                           karea=narea(nc)
                           nx=nc
                           if(check
     *                       )then
                                  write(lchann,100)nprocess,nc,nproc(nc)
                                  call isoflush(lchann)
                           endif
                    endif
            endif
         enddo
      enddo
c
      if(check
     *  )then
             write(lchann,109)nx
             call isoflush(lchann)
      endif
      if(nx.lt.0)then
                     nx=0
                 else
c
c      Now know which cube has this new cube inside it.   
c      Generate new cubes from old cube NX
c
                     if(check
     *                 )then
                            write(lchann,105)nx,nproc(nx)
                            call isoflush(lchann)
                     endif
                     nproc_orig=nproc(nx)
                     active(nx)=.false.
                     ky=ivy(1,nx)
c
c     Maintain short active list
c
                     nax=0
                     do na=1,nactive(nproc_orig)
                       if(listact(na,nproc_orig).ne.nx
     *                   )then
                              nax=nax+1
                              listact(nax,nproc_orig)
     *                                          =listact(na, nproc_orig)
                       endif
                     enddo 
                     nactive(nproc_orig)=nax
                     call prepact
c JSW                nnew=0
c
c      Draw as necessary....
c
c      Draw left?
                     if(ix1.ne.nx1(nx))then
c   Above...
                                   if(nz1(nx).ne.iz1)then
                   call cubelist(nproc(nx),nx1(nx),nz1(nx),ix1,iz1,ky,0)
                                      nnew=nnew+1
                                      newcube(1)=1
                                   endif
c   Level...
                   call cubelist(nproc(nx),nx1(nx),iz1,ix1,iz2,ky,0)
                                      nnew=nnew+1
                                      newcube(2)=1
c   Below...
                                   if(iz2.ne.nz2(nx))then
                   call cubelist(nproc(nx),nx1(nx),iz2,ix1,nz2(nx),ky,0)
                                      nnew=nnew+1
                                      newcube(3)=1
                                   endif
c
                     endif
c      Draw right?
                     if(ix2.ne.nx2(nx))then
c   Above...
                                   if(iz1.ne.nz1(nx))then
                   call cubelist(nproc(nx),ix2,nz1(nx),nx2(nx),iz1,ky,0)
                                      nnew=nnew+1
                                      newcube(4)=1
                                   endif
c   Level...
                   call cubelist(nproc(nx),ix2,iz1,nx2(nx),iz2,ky,0)
                                      nnew=nnew+1
                                      newcube(5)=1
c   Below...
                                   if(iz2.ne.nz2(nx))then
                   call cubelist(nproc(nx),ix2,iz2,nx2(nx),nz2(nx),ky,0)
                                      nnew=nnew+1
                                      newcube(6)=1
                                   endif
c
                     endif
c
c      Draw above?
c
                                   if(iz1.ne.nz1(nx))then
c
                   call cubelist(nproc(nx),ix1,nz1(nx),ix2,iz1,ky,0)
                                      nnew=nnew+1
                                      newcube(7)=1
c
                                   endif
c
c      Draw below?
c
                                   if(iz2.ne.nz2(nx))then
c
                   call cubelist(nproc(nx),ix1,iz2,ix2,nz2(nx),ky,0)
                                      nnew=nnew+1
                                      newcube(8)=1
c
                                   endif
c
      endif
      if(check
     *  )then
             write(lchann,101)nx,nnew,newcube
             call isoflush(lchann)
      endif       
c     if(nnew.gt.8)stop
c
c      Add a brand-new cube (nx=0)...
c      ...or finally replace the old (now split) cube by the new one
c      using the old cube's ID.
c
c      Set previous work descriptions as now inactive, if this is a new
c      cube and not a split-up (replacement) of an earlier cube.
c
      do nk=1,nlong
         if(nproc(longact(nk)).eq.nprocess
     *     )then
                active(longact(nk))=.false.
         endif
      enddo
c
c      Maintain short active list
c
      nactive(nprocess)=0
c
      call cubelist(nprocess,ix1,iz1,ix2,iz2,iheight,nx)
      call prepact
c
c
c      Define the boundary lines of the new...
c
      call bound(nprocess)
c
c      ...and of the old, if we actually generated anything new...
c      (01FEB10)
c
      if(nnew.gt.0
     *  )then
             call bound(nproc_orig)
             if(check
     *         )then
                    write(lchann,107)nprocess,nproc_orig
                    call isoflush(lchann)
              endif
      endif
c
      return
c
100   format('CUBESPLIT: New cube from process',i4,' is inside cube',i4
     *      ,' owned by process',i4)
101   format('CUBESPLIT:         NX=',i12
     *     ,/28x,'       Left          Right       Middle'
     *     ,/28x,'  Abv  Lvl  Blw  Abv  Lvl  Blw  Abv  Blw'
     *     ,/'Cube split',i12,' ways',8i5)
102   format('Splitting:')
103   format(' Exact box match at', 4i5)
104   format('CUBESPLIT: Process:',i5,', New cube:',8i6)
1041  format('CUBESPLIT: Cube ID:',i5,', Old cube:',8i5)
105   format('...setting cube',i3,' process',i3,' inactive')
106   format(/'***** CUBE',i3,':',/)
107   format('CUBESPLIT: NPROCESS=',i5,', NPROC_ORIG=',i12)
108   format('CUBESPLIT: NPROCESS=',i7,', NPROCMAX=',i6)
109   format('CUBESPLIT: Initial NX=',i12)
201   format('Colours:',i3,', process',i3,', lines',12i3)
800   format(/'   BEFORE:',/)
801   format(20i4)
802   format(/'    AFTER:',/)
803   format(/'CANDIDATE:',/i8,4i4)
1001  format('            Face',i4,', LineSegmentCount',i3,':')
c
      end
