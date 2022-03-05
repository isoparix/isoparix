      subroutine flipflop(n,array)
c
c      Flip-flops a power_of_2 square array
c
      complex(8) array(n,n),t(n)
c
c      Swap rows and columns nt and n-nt+1, so for 8x8, swap 1-8,2-7,3-6,4-5
c
c       Flip top to bottom
c
      write(*,*)
      write(*,*)'Flip top to bottom, flop left and right'
c
      nyb=n
      do nya=1,n/2
          t=array(:,nya)
          array(:,nya)=array(:,nyb)
          array(:,nyb)=t
          nyb=nyb-1
      enddo
c
c      Flop left to right
c
      nxb=n
      do nxa=1,n/2
          t=array(nxa,:)
          array(nxa,:)=array(nxb,:)
          array(nxb,:)=t
          nxb=nxb-1
      enddo
c
      write(*,*)'Flip-flop complete'
      write(*,*)
c
      return
      end
