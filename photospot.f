      subroutine photospot(xp,yp,zp,x,y,ixr,iyr)
c
c      Returns real and nearest-integer co-ordinates on screen of
c      (xp,yp,zp)
c 
      use isocomm
c
c     write(*,100)
      call tanxy(xp,yp,zp,xphoto,yphoto)
      x=(xphoto/scale)+xoffset
      y=(yphoto/scale)+yoffset
      ixr=.5+x
      iyr=.5+y
c
      if(ixr.gt.ixcmax)ixcmax=ixr
      if(ixr.lt.ixcmin)ixcmin=ixr
      if(iyr.gt.iycmax)iycmax=iyr
      if(iyr.lt.iycmin)iycmin=iyr
c
      if(check
     *  )then
             write(lchann,101)xp,yp,zp,scale,xoffset,yoffset
     *                           ,xphoto, yphoto,x,y,ixr,iyr
             call isoflush(lchann)
      endif
c
100   format(/'START OF PHOTOSPOT:')
101   format('PHOTOSPOT:'
     *     /,'Initial co-ordinates',3f18.2
     *     /,'Scale, x/y-offsets  ',3f18.2
     *     /,'X-photo, Y-photo    ',2f18.2
     *     /,'X and Y             ',2f18.2
     *     /,'Screen X and Y   ',2i18
     *     /,'END OF PHOTOSPOT')
c
      return
      end 
