      subroutine read_surface(lchann)
c
c      Read in the initial surface from FORTRAN unit lchann
c
      use surf_comms
c
      implicit real(8) (a-h,o-z)
c
      character(80)surf_line
      character(1) surf_text(80)
      equivalence (surf_text,surf_line)
c
      ixmax=0
      iymax=0
      izmax=0
      coeff=0.0
c
  1   continue
      read(lchann,100,end=2)surf_line
      if(len_trim(surf_line).eq.0
     *  )then
             write(*,101)
             go to 1
      endif
      if(surf_text(1).eq.'!')go to 1   ! Just a comment line
      do n=1,80
         if(surf_text(n).eq.'!'
     *     )then
                do m=n,80
                   surf_text(m)=''
                enddo
                exit
         endif
      enddo
      write(60,*)surf_line
c
      read(surf_line,*,err=3)k,c
c
      ix=k/100
      k=k-(ix*100)
      iy=k/10
      iz=k-(iy*10)
c
      if(ix.gt.ixmax)ixmax=ix
      if(iy.gt.iymax)iymax=iy
      if(iz.gt.izmax)izmax=iz
c
      coeff(ix,iy,iz)=c+coeff(ix,iy,iz)  ! Allows multiple entries
      used (ix,iy,iz)=.true.             ! This cell is used
      go to 1
  2   continue
      return
c
  3   continue
      write(0,102)surf_line
      stop
c
100   format(a80)
101   format('READ_SURFACE: Blank line in surface - ignored')
102   format('READ_SURFACE - Error in surf_line: ',a)
c
      end
