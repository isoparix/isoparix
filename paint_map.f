      subroutine paint_map(map_name,nrow)
c
c      Paints a single time map on the X-window...
c
      use allcomms
c
      character (len=3) num
      character (len=60) map_name
c
c     write(*,100)map_name,nrow,nfirstcol,nendcol,updating
c
c      Check parameters..
c
      if(nrow     .le.0.or.nrow     .gt.nrows     .or.
     *   nfirstcol.le.0.or.nfirstcol.gt.ntimeslots.or.
     *   nendcol  .le.0.or.nendcol  .gt.ntimeslots.or.
     *   nendcol  .lt.nfirstcol
     *  )then
            write(8,103)map_name,ntimeslots,nrow,nrows,nfirstcol,nendcol
            write(0,103)map_name,ntimeslots,nrow,nrows,nfirstcol,nendcol
            call isoflush(8)
            return
      endif
c
      iy1=line_y(nrow)
      iy2=line_y(nrow+1)
c
c      Condition map_name,if necessary...
c
      if(row_name(nrow).ne.map_name
     *  )then
             row_name(nrow)=trim(adjustl(map_name))
             iytxt=((iy1+iy2)/2)+4
c
c      Choose font from those listed by xlsfonts command
c      I use 8x13 (all fonts must be followed by a null character).
c
c      Blank out the name space first, then write it.   Do we have a
c      variable colour map?
c
             call x11rectgc(%val(1)     ,%val(iy1+1)
     *                     ,%val(nhsp-1),%val(iy2-1),%val(nblack))
c
             if(map_name.eq.
     *    'Calibration                                                 '
     *          .or.nrow.le.7
     *         )then
                    call x11text(%val(4),%val(iytxt)
     *                      ,trim(adjustl(map_name))
     *                      ,%val(0),%val(len_trim(map_name)))
                else
                    call x11text(%val(4),%val(iytxt)
     *                      ,trim(adjustl(map_name))
     *                      ,%val(node_font),%val(len_trim(map_name)))
             endif
c
             if(new_win.and.updating
     *         )then
                    ix2=ixm
                else
                    ix2=1+nhsp
             endif
c 
c      Put white lines around the map name...
c
            call x11whiteline(%val(0),%val(iy1), %val(1+nhsp),%val(iy1))
            call x11whiteline(%val(0),%val(iy2), %val(1+nhsp),%val(iy2))
            call x11whiteline(%val(nhsp),%val(iy1),%val(nhsp),%val(iy2))
            call x11updatezone(%val(0)   ,%val(iy1)
     *                         ,%val(nhsp),%val(iy2))
      endif
c
c      Paint the relevant columns
c
      do nx=nfirstcol,nendcol
         ix1=nhsp+nx
         ncol=ncol_map(nx)
         if(ncol.ge.maxcols
     *     )then
c
c      This is most like a 'median max' level exceeded.   Ignore it...
c
                ncol=maxcols-1
         endif
c
c        if(ncol.lt.-1.or.
c    *      ncol.gt.maxcols
c    *     )then
c               write(0,104)map_name,number_map(nx),nx,lastcol
c    *                     ,ix1,iy1,iy2,ncol,maxcols,nfirstcol,nendcol
c               return
c        endif
c
         if(ncol.eq.-1
     *     )then
                call x11whiteline(%val(ix1),%val(iy1+1)
     *                           ,%val(ix1),%val(iy2  ))
                if(nrecords.le.0
     *            )then
c
c      Flag non-contributing hosts with diagonal white line, 
c      only if this is a live session...
c
                       call x11whiteline(%val(0)     ,%val(iy1)
     *                                  ,%val(nhsp+1),%val(iy2))
                       call x11updatezone(%val(0)   ,%val(iy1)
     *                                  ,%val(nhsp+1),%val(iy2))
                       row_name(nrow)='Non-Contrib'
                endif
            else
                call x11line(%val(ix1),%val(iy1+1)
     *                      ,%val(ix1),%val(iy2),%val(ncol))
         endif
c
c      Draw the right-most data boundary dot
c
         call x11whiteline(%val(ix1),%val(iy1)
     *                    ,%val(ix1),%val(iy1))
         call x11whiteline(%val(ix1),%val(iy2)
     *                    ,%val(ix1),%val(iy2))
      enddo
      call x11whiteline(%val(ix1+1),%val(iy1)
     *                 ,%val(ix1+1),%val(iy2))
c
      call x11updatezone(%val(nhsp+nfirstcol),%val(iy1)
     *                   ,%val(nhsp+ nendcol),%val(iy2))
c
      return
c
100   format('PAINT_MAP: MAP_NAME is ',a20,', at row/col/col',3i5
     *      ,': Updating',l2)
101   format(i3)
102   format('PAINT_MAP: NTX=',i5,', NCOL_MAP(NTX)=',i5
     *      ,', IX1=',i5,', IX2=',i5
     *)
103   format('***** ERROR - PAINT_MAP:'
     *     ,/'                   MAP_NAME is ',a60
     *     ,/'                     NTIMESLOTS',i12
     *     ,/'                          NROW=',i12,', NROWS=',i12
     *     ,/'                     NFIRSTCOL=',i12
     *     ,/'                       NENDCOL=',i12
     *      )
104   format('***** ERROR - PAINT_MAP: ',a60
     *     ,/'    number        nx   lastcol       ix1       iy1'
     *     , '       iy2      ncol    maxcols nfirstcol   nendcol'
     *     ,/10i10,/)
105   format('Striking through name ',a20,' at ',4i5)
      end
