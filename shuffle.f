      subroutine shuffle(x,iy)
      use venn_comms
c
c      Moves data points along an array
c
c    Use array ticks(limticks,iny;:inyh) and limticks
c
      implicit real(8)(a-h,o-z)
c
      unplaced=.true.
      n=1
      do while (unplaced)
         if(x.lt.ticks(n,iy)
     *     )then
                do nslip=limticks,n+1,-1
                   ticks(nslip,iy)=ticks(nslip-1,iy)
                enddo
                ticks(n,iy)=x
                unplaced=.false.
         endif
         n=n+1
         if(n.gt.limticks
     *     )then
                write(0,100)x,limticks,iy
                stop
         endif
      enddo
c
      return
c
100   format("SHUFFLE ERROR: x=",f10.3,", limticks=",i4,", iy=",i4)
c
      end
