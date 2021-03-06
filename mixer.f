      program mixer
c
c      Mixes two surf files
c
      use surf_comms
c
      implicit real(8) (a-h,o-z)
      integer, dimension(0:99) :: ixa,iya,iza,ixb,iyb,izb
      real,    dimension(0:99) :: coeffa,coeffb 
      real,    dimension(0:9,0:9,0:9) :: coeff_mix
c
c      Read in the first surface to be combined
c
      lchann=1
      call read_surface(lchann)
c
      indexa=0
      do iz=0,9
         do iy=0,9
            do ix=0,9
               if(coeff(ix,iy,iz).ne.0.0
     *           )then
                      indexa=indexa+1
                      ixa(indexa)=ix
                      iya(indexa)=iy
                      iza(indexa)=iz
                      coeffa(indexa)=coeff(ix,iy,iz)
               endif
            enddo
         enddo
      enddo
c
c      Read in the second surface to be combined
c
      lchann=3
      call read_surface(lchann)
c
      indexb=0
      do iz=0,9
         do iy=0,9
            do ix=0,9
               if(coeff(ix,iy,iz).ne.0.0
     *           )then
                      indexb=indexb+1
                      ixb(indexb)=ix
                      iyb(indexb)=iy
                      izb(indexb)=iz
                      coeffb(indexb)=coeff(ix,iy,iz)
               endif
            enddo
         enddo
      enddo
c
c
c      Combine the surfaces
c
      coeff_mix=0.0
      do j=1,indexb
         jx=ixb(j)
         jy=iyb(j)
         jz=izb(j)
         cj=coeffb(j)
         do i=1,indexa
            ixc=jx+ixa(i)
            iyc=jy+iya(i)
            izc=jz+iza(i)
c           write(0,101)jx,    jy,    jz,    cj
c    *                 ,ixa(i),iya(i),iza(i),coeffa(i)
c    *                 ,ixc,   iyc,   izc,   cj*coeffa(i)
            coeff_mix(ixc,iyc,izc)=coeff_mix(ixc,iyc,izc)+(cj*coeffa(i))
         enddo
      enddo
c
c      Write out the combined surface
c
      do k=0,9
         do j=0,9
            do i=0,9
               if(coeff_mix(i,j,k)**2.gt.0.0001
     *           )then
                      write(*,100)i,j,k,coeff_mix(i,j,k)
                      write(8,100)i,j,k,coeff_mix(i,j,k)
               endif
            enddo
         enddo
      enddo
c
      stop
c
100   format(3i1,' ',f20.4)
101   format(3i1,' ',f20.4,'  x  ',3i1,' ',f20.4,' ==> ',3i1,' ',f20.4)
c
      end
