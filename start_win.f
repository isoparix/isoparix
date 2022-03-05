      subroutine start_win
c
c     Base window size on number of rows required
c
      use allcomms
c
c
c      Set depth of 'readable' row
c
      ncalib=15
      nclear_rows=9
c
      iym=iymax-nedge
      if(nrows.gt.nclear_rows
     *  )then
             idepth=float(iym-disp_top-(nclear_rows*ncalib))
     *             /float(nrows-nclear_rows)
             iya=min0(ncalib,idepth)
         else
             iya=ncalib
      endif
c
c      Check against too many hosts or disks...
c
      if(iya.lt.2
     *  )then
             iya=2
      endif
c
      ndelta_x=iya/2
      if(ndelta_x.lt.2)ndelta_x=2
c
      iym=(disp_top+nclear_rows*ncalib)+(iya*(nrows-nclear_rows))
      disp_bot=iym-(2*ncalib)
      call winset
      call x11work(%val(  0),%val(       0)
     *            ,%val(ixm),%val(disp_top-1),%val(10))
      call x11updatezone(%val(  0),%val(       0)
     *                  ,%val(ixm),%val(disp_top))
c
c      Set ACOLS so that acols=isocols/100
c
      if(isocols.eq.0
     *  )then
             acols=.01*float(maxcols-1)
         else
             acols=.01*float(isocols-1)
      endif
c
      call button_setup('colstat')
c
c      Calculate where to draw the dividing lines....
c      Top seven and bottom two rows are always readable...
c
      nlx=0
      do nh=1,7
         line_y(nh)=nlx
         nlx=nlx+ncalib
         row_name(nh)=' '
      enddo
c
      if(nrows.gt.nclear_rows
     *  )then
             do nh=8,nrows-1
                line_y(nh)=nlx
                nlx=nlx+iya
                row_name(nh)=' '
             enddo
      endif
c
      nlx=nlx-iya
      do nh=nrows,nrows+1
         nlx=nlx+ncalib
         line_y(nh)=nlx
         row_name(nh)=' '
      enddo
c
c      Allow for space for the buttons...
c
      do nh=1,nrows+1
         line_y(nh)=line_y(nh)+disp_top
      enddo
c
c      .....and where on the screen do the hosts live?
c
      do nh=1,nrows
c
         do i=line_y(nh),line_y(nh+1)-1
            line_host(i)=nh
         enddo
c
      enddo
c
c      Do the calibration line
c
      fullmap=.true.
      do n=1,ntimeslots
         number_map(n)=calibn_map(n)
         ncol_map(n)=calibn_map(n)*acols
      enddo 
      nfirstcol=1
      nendcol=ntimeslots
      call paint_map(
     *'Calibration                                                 '
     *               ,nrows)
c
      if(iya.ge.12
     *  )then
             node_font=0	!'8x13\0'
             font_height=13
             font_width=8 
             go to 1
      endif
c
      if(iya.ge.8
     *  )then
             node_font=1	!'6x10\0'
             font_height=10
             font_width=6 
             go to 1
      endif
c
      if(iya.lt.8
     *  )then
             node_font=2	!'5x7\0'
             font_height=7
             font_width=5 
             go to 1
      endif
c
  1   continue
      write(8,100)maxcols,isocols,acols,ixmax,iymax,ixm,iym,nrows
     *           ,ntimeslots,nhosts,iya,nrecords,nblack,nwhite,node_font
      call isoflush(8)
c
      return
c
100   format('START_WIN: MAXCOLS=',i4,',    ISOCOLS=',i4,', ACOLS=',f8.4
     *     ,/'             IXMAX=',i4,',      IYMAX=',i4
     *     ,/'               IXM=',i4,',        IYM=',i4
     *     ,/'             NROWS=',i4,', NTIMESLOTS=',i4,', NHOSTS=',i4
     *     ,/'             DEPTH=',i4,',   NRECORDS=',i4
     *     ,/'             BLACK=',i4,',      WHITE=',i16
     *     ,/'         NODE_FONT=',i6
     *     ,/)
106   format('Lines',i5,' to',i5,' associated with host',i3)
c
      return
      end
