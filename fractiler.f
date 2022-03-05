      program fractiler
c
c      Creates a tiled picture from one big picture and gives centres
c      and dimensions of tiles
c
       use isocomm
       use bmp_comms
c
       real(8)xcen,ycen,deltay,cra,cia
c
       character(1)choice
c
c      Read in key arrays
c
      write(*,100)
      read(*,*)margin
      read(63)ixm,iym,limit
c
      xcen=.5*float(ixm)
      ycen=.5*float(iym)
c
      dx=float(ixm)/float(margin)
      dy=float(iym)/float(margin)
c
      ixma=.5+(xcen-dx)     !  Co-ordinate of right edge of left tiles
      ixmb=.5+(xcen+dx)     !  Co-ordinate of left edge of right tiles
      iyma=.5+(ycen-dy)     !  Co-ordinate of lower edge of top tiles
      iymb=.5+(ycen+dy)     !  Co-ordinate of top edge of lower tiles
c
c      Allocate arrays
c
      if(allocated(mset))deallocate(mset)
      allocate(mset(0:ixm+1,0:iym+1),stat=ierror)
c
      if(allocated(mapdata))deallocate(mapdata)
      allocate(mapdata(ixm,iym),stat=ierror)
c
      if(allocated(paint))deallocate(paint)
      allocate(paint(0:limit+1),stat=ierror)
c
      read(63)rgbquad       ! char(1024)
      read(63)paint         ! char(0:limit+1)
      read(63)mset          ! integer(2)(0:ixm+1,0:iym+1)
      read(63)mapdata       ! character(1)(ixm,iym)
      close(63)
c
      mapdata(1:ixm,iyma+1:iymb-1)=char(255)   !  Horizontal bar
      mapdata(ixma+1:ixmb-1,1:iym)=char(255)   !  Vertical   bar
c
c      Write out the 'tiled' picture
c
      bmname='whole_pic'
      call array2bmp(ixm,iym,mapdata)
c
c      Try other colour maps...
c
  1   continue
      write(*,101)
      read(*,*)choice
c
      if(choice.eq.'c'
     *  )then
             write(*,102)
             call defmap(254,5,iret)   ! Read from keyboard, channel 5
             call array2bmp(ixm,iym,mapdata)
             go to 1
      endif
c
      if(choice.eq.'q'
     *  )then
             stop
      endif
c
      if(choice.eq.'t'
     *  )then
             go to 2
      endif
c
      go to 1
c
  2   continue
c
      bmname= 'top_left'
      call make_tile(   1,ixma,   1,iyma)
      bmname='top_right'
      call make_tile(ixmb,ixm ,   1,iyma)
      bmname='bottom_right'
      call make_tile(ixmb,ixm ,iymb,iym )
      bmname='bottom_left' 
      call make_tile(   1,ixma,iymb,iym )
c
      stop
c
100   format('Enter integer margin - larger integer, smaller margin')
101   format('c to recolour, q to quit, t to tile with these colours')
102   format('0.5 0.5 0.5'
     *      ,  ';Distance through sin**2(x) wave cycle (x/pi) at start'
     *     ,/'0.0 0.0 0.0      ;Minimum level of this colour (0-1)'
     *     ,/'1   2  3       ;Number of periods of this colour')
      
c
      end
c
c
c
      subroutine make_tile(ix1,ix2,iy1,iy2)
c
       use isocomm
       use bmp_comms
c
      ixdim=ix2-ix1+1
      iydim=iy2-iy1+1
      write(*,100)bmname,ixdim,iydim
      if(allocated(maptile))deallocate(maptile)
      allocate(maptile(ixdim,iydim),stat=ierror)
      maptile= mapdata(ix1:ix2,iy1:iy2)
      call array2bmp(ixdim,iydim,maptile)
      return
c
100   format('Writing tile: ',a30,2i8)
c
      end
