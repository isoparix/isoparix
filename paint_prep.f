      subroutine paint_prep(amap,nh,nrow,map_name)
c
c      Prepare map for painting, and calls paint routine
c
      use allcomms
c
      real(4),dimension(0:maxtimes) :: amap
c
      character (len=60) map_name
c
      nx=ndatacol
      do nt=nfirstcol,nendcol
c
         if(nx.gt.ntimeslots
     *     )then
                nx=1
         endif
c
         if(no_data(nx,nh)
     *     )then
                ncol_map(nt)=-1
c               write(0,102)nx,nt,nh,trim(map_name)
            else
                ncol_map(nt)=amap(nx)*acols
c               if(ncol_map(nt).gt.maxcols.or.
c    *             ncol_map(nt).lt.-1
c    *            )then
c                    write(0,103)nx,amap(nx),nt,acols,nh
c    *                          ,ncol_map(nt),maxcols,trim(map_name)
c                    call flush(0)
c                    ncol_map(nt)=0
c               endif
         endif
c
c        write(8,104)nx,nrow
c        call isoflush(8)
         hist_map(nx,nrow)=amap(nx)+.5
         nx=nx+1
c
      enddo
c
      call paint_map(map_name,nrow)
c
100   format('PAINT_PREP  INPUT MAP:',50f6.1)
101   format('           OUTPUT MAP:',50i6)
102   format('PAINT_PREP: No data at data column',i5
     *             ,', display column',i5,', host',i3,': Map=',a20)
103   format('PAINT_PREP: Data column',i5,', value=',f8.3,', maps to'
     *              ,' display column',i5,', ACOLS',f8.3,', host',i3
     *      ,', colour/max=',2i4,': Map= ',a)
104   format('PAINT_PREP: nx=',i5,', nrow=',i5)
c
      return
      end
