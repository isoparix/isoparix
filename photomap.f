      subroutine photomap(nvertex,nc)
c
      use isocomm
c
      xp=ivx(nvertex,nc)
      yp=ivy(nvertex,nc)
      zp=ivz(nvertex,nc)
c
      call photospot(xp,yp,zp
     *               ,xc(nvertex,nc), yc(nvertex,nc)
     *              ,ixc(nvertex,nc),iyc(nvertex,nc)
     *)
c
      if(xc(nvertex,nc).gt.x_max(nc))x_max(nc)=xc(nvertex,nc)
      if(xc(nvertex,nc).lt.x_min(nc))x_min(nc)=xc(nvertex,nc)
      if(yc(nvertex,nc).gt.y_max(nc))y_max(nc)=yc(nvertex,nc)
      if(yc(nvertex,nc).lt.y_min(nc))y_min(nc)=yc(nvertex,nc)
c
      max_x(nc)=.5+x_max(nc)
      min_x(nc)=.5+x_min(nc)
      max_y(nc)=.5+y_max(nc)
      min_y(nc)=.5+y_min(nc)
c
c      Only do the next bit to position the true cube vertices, not 
c      any 'line' vertices
c
      if(nvertex.gt.8)return
c
      visivert(nvertex,nc)=.true.
c
      if(d2.gt.vdmax(nc)
     *  )then
             vdmax(nc)=d2
             farvert(nc)=nvertex
      endif
c
      if(d2.lt.vdmin(nc)
     *  )then
             vdmin(nc)=d2
             nearvert(nc)=nvertex
      endif
c
      return
      end
