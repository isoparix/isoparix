      subroutine ip_distribute(distributed)
c
c      Spreads pixels evenly thoughout the intensity levels
c
      use ip_comms
c
      logical distributed
c
      max_hidden_level=0
      do n=maxnzp+1,255
         if(nval(n).gt.max_hidden_level
     *     )then
                max_hidden_level=nval(n)
         endif
      enddo
      write(*,104)max_hidden_level

c
      write(8,101)nval
c
c     low_count=0
      low_count=lo_contrast
      empty=0.0
      do n=0,255
         if(nval(n).le.low_count
     *     )then
                empty=empty+1.0
         endif
      enddo
c
      if(empty.lt.1.0
     *  )then
             write(0,100)
             distributed=.false.
             return
         else
             write(0,102)int(empty),low_count
             distributed=.true.
      endif
c
      lcurrent=255
      avail=256.0-empty
      delta_int=256.0/avail
      all_int=0.
      write(0,*)avail,interval
      ixfer=0
      do n=255,1,-1
         if(nval(n).gt.low_count
     *     )then
                ixfer(n)=lcurrent
                all_int=all_int+delta_int
                lcurrent=255.5-all_int
c               write(0,103)n,ixfer(n),lcurrent,all_int
                if(avail.lt.1.0
     *            )then
                       exit
                endif
         endif
      enddo
c
      write(8,101)nval,ixfer
c
c      Spread out the pixel values
c
      do iy=0,iydim-1
         do ix=0,ixdim-1
c           if(ipic(ix,iy).gt.255.or.
c    *         ipic(ix,iy).lt.0)write(0,*)ix,iy,ipic(ix,iy)
            ipic(ix,iy)=ixfer(ipic(ix,iy))
         enddo
      enddo
c
100   format('No empty cells in the intensity histogram')
101   format(/16(16i8,/))
102   format(i6,' empty cells in intensity histo. LOW_COUNT set to',i5)
103   format(3i6,3f8.2)
104   format('Max hidden quantity beyond maxnzp is:',i6)
c
      end
