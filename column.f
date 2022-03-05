      subroutine column(ix,iy1,iy2,miniter)
c
c      Evaluates a line at IX between IY1 and IY2..
c
       use isocomm
c
      logical rectype
c
      if(ngrf-ngline.lt.4+(iy2-iy1))call grafout
c
      subname='Column'
      ngrafout(ngline+1)=ix
      ngrafout(ngline+2)=iy1
      ngrafout(ngline+3)=-2
      ngline=ngline+3
c
      ia=iy1
      call looper(ix,iy1,miniter,ncola)
      do iy=iy1+1,iy2+1
         ncol=-1
         if(iy.le.iy2)call looper(ix,iy,miniter,ncol)
         if(ncola.ne.ncol
     *     )then
c
c      We have changed colour...
c
                if(iy-ia.gt.1
     *            )then
c
c      ....and it's more than one pixel's worth.
c
                       ngrafout(ngline+1)=iy-ia
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
                ia=iy
         endif
      enddo
c
      mx=iy2-iy1+1
      npels=npels+mx
c      write(*,200)iy2,iy1,(iy2-iy1+1),npels
c200   format(2i12,i36,i12)
      mpels=mpels+mx
c
      subname=' unset'
      return
      end
