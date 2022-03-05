      subroutine gridsmooth
c
c      Makes sure all polygons touch each other
c
      use venn_comms
c
      implicit real(8) (a-h,o-z)
c
c      Go through all the recorded end-points, and make sure their
c      differently colured neighbours have the same values
c
      write(*,106)nkoords
      do n=1,nkoords
         iy=koords(3,n)
         do m=1,2
            ix=koords(m,n)
            ax=agrid(ix,iy)
            bx=bgrid(ix,iy)
            ndepth=grid(ix,iy)
            write(80,107)ix,iy,ax,ndepth
c
            do k=-1,1,2
c              if(grid(ix+k,iy).ne.ndepth
c    *           )then
c                     if(agrid(ix+k,iy).gt.10000000.
c    *                  )then
                             agrid(ix+k,iy)=ax
                             bgrid(ix+k,iy)=bx
c                        else
c                            agrid(ix+k,iy)=0.5*(ax+agrid(ix+k,iy))
c                     endif
c              endif
            enddo
c
c           do k=-1,1,2
c              if(grid(ix,iy+k).ne.ndepth
c    *           )then
c                     if(agrid(ix,iy+k).gt.10000000.
c    *                  )then
c                            agrid(ix,iy+k)=ax
c                        else
c                            agrid(ix,iy+k)=0.5*(ax+agrid(ix,iy+k))
c                     endif
c              endif
c           enddo
c
         enddo
      enddo
c
      call flush(80)
      return
c
106   format('GRIDSMOOTH: Examining all',i8,' recorded end-points')
107   format('GRIDSMOOTH: ix,iy,ax,depth',i16,i5,e25.16,i6)
c
      end
