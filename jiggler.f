      program jiggler
c
c      Keeps an X-window moving - RVN on the cheap...
c
c      Open up the display window
c
      call x11winope
     * (%val(100)
     * ,%val(100)
     * ,%ref(maxcols),%ref(isocols),%ref(irc)
     * ,%val(1), %val(1), %val(1)
     * ,%val(1), %val(1), %val(1)
     * ,%val(1), %val(1), %val(1)
     * )
c
      call system('./xw')
      read(11,100)idwindow
      read(11,*)left_x
      read(11,*)left_y
      read(11,*)iwidth
      read(11,*)iheight
      read(11,*)ibw
      close(11)
c
      write(*,101)idwindow,left_x,left_y,iwidth,iheight
c
      left_y=left_y+1
      k=1
  1   continue
         call x11config(%val(idwindow))
c
c        call x11config(%val(idwindow),%val(left_x),%val(left_y)
c    *                     ,%val(iwidth),%val(iheight),%val(ibw))
c        call x11mouse(nbut,mousex,mousey,newx,newy)
c        if(nbut.ne.-999
c    *     )then
c               write(*,*)nbut,mousex,mousey,newx,newy
c        endif
         call system('usleep 50000')
c
      go to 1
c
      stop
c
100   format(z9)
101   format('    WindowID: 0x',z9
     *     ,/'Upper-left X:',i6
     *     ,/'Upper-left Y:',i6
     *     ,/'      Height:',i6
     *     ,/'       Width:',i6
     *      )
      end
