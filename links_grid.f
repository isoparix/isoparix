      subroutine links_grid
c
c      Creates irregular grid, where no straight line
c      passes through more than two points...
c
      use links_comms
c
      a=nodecount
      nx=sqrt(a)
      ny=1+(a/float(nx))
      write(*,*)nx,ny,nx+ny,nodecount
c
      theta=0.
      r2=sqrt(2.0)
      node=0
      do i=1,nx
         cenx=3*i
         do j=1,ny
            ceny=3*j
            node=node+1
            theta=theta+r2
            xnode(node)=cenx+sin(theta)
            ynode(node)=ceny+cos(theta)
            write(*,100)node,i,j,xnode(node),ynode(node)
         enddo
      enddo
c
      return
c
100   format(3i4,2f8.3)
c
      end
