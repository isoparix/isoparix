      subroutine cubelist(nprocess,ix1,iz1,ix2,iz2,ih,nx)
c
c      Takes in the work area corners, and transforms them to an
c      entry in the list of cubes to be drawn.  
c
      use isocomm
c
      call validate(ix1,iz1,ix2,iz2,nx)
c
      if(nx.eq.0)then
                     ncubes=ncubes+1
                     nc=ncubes
                     if(check
     *                 )then
                            write(lchann,102)nc
                            call isoflush(lchann)
                     endif
                 else
                     nc=nx
                     if(check
     *                 )then
                            write(lchann,103)nx
                            call isoflush(lchann)
                     endif
c
      endif
c 
c      Maintain the short active list...
c
      if(check
     *  )then
             write(lchann,101)nprocess,nactive(nprocess)
     *               ,(listact(mx,nprocess),mx=1,nactive(nprocess))
             call isoflush(lchann)
      endif
c
      nactive(nprocess)=nactive(nprocess)+1
      listact(nactive(nprocess),nprocess)=nc
c
      if(check
     *  )then
             write(lchann,1011)nprocess,nactive(nprocess)
     *               ,(listact(mx,nprocess),mx=1,nactive(nprocess))
             call isoflush(lchann)
      endif
c
       nproc(nc)=nprocess
      active(nc)=.true.
       narea(nc)=(ix2-ix1+1)*(iz2-iz1+1)
c
       nx1(nc)=ix1
       nx2(nc)=ix2
       nz1(nc)=iz1
       nz2(nc)=iz2
c
       x1(nc)=ix1
       x2(nc)=ix2
       z1(nc)=iz1
       z2(nc)=iz2
c
      facecol(nc)=mycol(nprocess)
c
      if(check
     *  )then
             write(lchann,100)nc,ix1,iz1,ix2,iz2,ih
             call isoflush(lchann)
      endif
      nvertex=9
      do i=ix2,ix1,ix1-ix2
        do k=iz2,iz1,iz1-iz2
          do j=0,ih,ih
c
              nvertex=nvertex-1
c
              ivx(nvertex,nc)=i
              ivy(nvertex,nc)=j
              ivz(nvertex,nc)=k
c
c
           enddo
         enddo
      enddo
c
c      Assign screen co-ordinates to all line vertices
c
      call cubespec(nc)
      call prepact
c
100   format('Cubelist',i3,': X1=',i5,', Z1=',i5,', X2=',i5
     *                    ,', Z2=',i5,' Height=',i5)
101   format('Cubelist: Process',i3,', active cubes',i3,':',150i4)
1011  format('          Process',i3,', active cubes',i3,':',150i4)
102   format('Adding a new cube to make a total of',i3)
103   format('Replacing cube',i3)
c
      return
c
      end
