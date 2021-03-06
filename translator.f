      program translator
c
c      Displaces surface by dx, dy and dz
c
      use surf_comms
c
c      calculate the binary coefficients
c
      call binomial
c
c      Read in the initial surface from fort.3
c
      lchann=3
      call read_surface(lchann)
c
      write(*,101)
      read (*,*)dx
c
      write(*,102)
      read (*,*)dy
c
      write(*,103)
      read (*,*)dz
c
      dx=-dx
      dy=-dy
      dz=-dz
c
c      Find terms with non-zero values, translate X
c
      write(*,1061)
      do iz=0,izmax
         do iy=0,iymax
            do ix=0,ixmax
               a=coeff(ix,iy,iz)
               if(a.ne.0.0
     *           )then
                      write(*,105)ix,iy,iz,a,dx,dy,dz
                      if(dx.ne.0
     *                  )then
                             k=ix
                             do j=0,k-1
                                coeff(j,iy,iz)=
     *                          coeff(j,iy,iz)+(a*bc(j,k)*(dx**(k-j)))
                             enddo
                      endif
               endif
            enddo
         enddo
      enddo
c
c      Find terms with non-zero values, translate Y
c
      write(*,1062)
      do iz=0,izmax
         do iy=0,iymax
            do ix=0,ixmax
               a=coeff(ix,iy,iz)
               if(a.ne.0.0
     *           )then
                      write(*,105)ix,iy,iz,a,dx,dy,dz
                      if(dy.ne.0
     *                  )then
                             k=iy
                             do j=0,k-1
                                coeff(ix,j,iz)=
     *                          coeff(ix,j,iz)+(a*bc(j,k)*(dy**(k-j)))
                             enddo
                      endif
               endif
            enddo
         enddo
      enddo
c
c      Find terms with non-zero values, translate Z
c
      write(*,1063)
      do iz=0,izmax
         do iy=0,iymax
            do ix=0,ixmax
               a=coeff(ix,iy,iz)
               if(a.ne.0.0
     *           )then
                      write(*,105)ix,iy,iz,a,dx,dy,dz
                      if(dz.ne.0
     *                  )then
                             k=iz
                             do j=0,k-1
                                coeff(ix,iy,j)=
     *                          coeff(ix,iy,j)+(a*bc(j,k)*(dz**(k-j)))
                             enddo
                      endif
c
               endif
            enddo
         enddo
      enddo
c
      write(*,107)
      do iz=0,izmax
         do iy=0,iymax
            do ix=0,ixmax
               a=coeff(ix,iy,iz)
               if(a*a.gt.0.0001
     *           )then
                      write(8,105)ix,iy,iz,a
                      write(*,105)ix,iy,iz,a
               endif
            enddo
         enddo
      enddo
c
c
c
      stop
c
101   format('Move x=0 to x=?')
102   format('Move y=0 to y=?')
103   format('Move z=0 to z=?')
105   format(3i1,' ',4f20.4)
1061  format(/'Surface pre-X shift is:')
1062  format(/'Surface pre-Y shift is:')
1063  format(/'Surface pre-Z shift is:')
107   format(/'Displaced surface is:')
c
      end
