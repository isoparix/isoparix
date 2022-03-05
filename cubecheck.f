      subroutine cubecheck(nc)
c
      use isocomm
c
c      Writes out details of cube NC
c
      write(32,100)nc,nproc(nc),lvc(nc),max_line_vertices
      call isoflush(32)
      if(lvc(nc).gt.max_line_vertices
     *  )then
             write( 0,100)nc,nproc(nc),lvc(nc),max_line_vertices
             return
         else
             maxv=8
      endif
c
      write(32,1001)(    mx    ,mx=1,maxv)
      write(32,1002)(ivx(mx,nc),mx=1,maxv)
      write(32,1003)(ivy(mx,nc),mx=1,maxv)
      write(32,1004)(ivz(mx,nc),mx=1,maxv)
      write(32,1005)(ixc(mx,nc),mx=1,maxv)
      write(32,1006)(iyc(mx,nc),mx=1,maxv)
      write(32,1015)( xc(mx,nc),mx=1,maxv)
      write(32,1016)( yc(mx,nc),mx=1,maxv)
      write(32,1007)(lvv(mx,nc),mx=1,maxv)
      write(32,1011)(hidestat(mx,nc),mx=1,maxv)
      write(32,1008)nearvert(nc),farvert(nc)
      write(32,1012)sqrt(vdmin(nc)),sqrt(vdmax(nc))
      write(32,1009)xe,ye,ze
      write(32,1010)facecol(nc)
      write(32,2000)
      call isoflush(32)
c
      iretcode=0
      do m=1,maxv
         iretcode=ixc(m,nc)+iyc(m,nc)
      enddo
c
100   format('Cube',i3,', process',i3,' has',i3,' line vertices '
     *       ,'(max_line_vertices=',i3,')')
1001  format('         VERTEX:',16i6)
1002  format('        IVX - X:',16i6)
1003  format('        IVY - Y:',16i6)
1004  format('        IVZ - Z:',16i6)
1005  format(' IXC - Screen X:',16i6)
1006  format(' IYC - Screen Y:',16i6)
1007  format('LineVertexValue:',16i6)
1008  format('Near/far vertex:', 2i12)
1009  format('    Eyepoint is:',3f10.2)
1010  format(' Face colour is:',  i6)
1011  format('-behind, +hides:',16i6)
1012  format('Min/max distance',2f12.1)
1015  format('  XC -   Real X:',16f6.1)
1016  format('  YC -   Real Y:',16f6.1)
2000  format(/)
c
      return
c
      end
