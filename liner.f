      subroutine liner(iy,ix1,ix2,miniter)
c
c      Evaluates a line at IY between IX1 and IX2..
c
      use isocomm
c
      if(ngrf-ngline.lt.4+(ix2-ix1))call grafout
c
      ngrafout(ngline+1)=ix1
      ngrafout(ngline+2)=iy
      ngrafout(ngline+3)=-1
      ngline=ngline+3
c
      subname=' Liner'
      ia=ix1
      call looper(ix1,iy,miniter,ncola)
      do ix=ix1+1,ix2+1
         ncol=-1
         if(ix.le.ix2)call looper(ix,iy,miniter,ncol)
         if(ncola.ne.ncol)then
c
c      We have changed colour...
c
            if(ix-ia.gt.1)then
c
c      ....and it's more than one pixel's worth.
c
              ngrafout(ngline+1)=ix-ia
              ngrafout(ngline+2)=-ncola
              ngline=ngline+2
            else
c
c      Write previous singleton pixel
c
              ngline=ngline+1
              ngrafout(ngline)=-ncola
            endif
            ncola=ncol
            ia=ix
         endif
      enddo
c
      mx=ix2-ix1+1
      npels=npels+mx
c      write(*,200)ix2,ix1,(ix2-ix1+1),npels
c200   format(2i12,i36,i12)
      mpels=mpels+mx
c
      subname=' unset'
      return
      end
