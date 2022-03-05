      program wallgen
c
c      Generates multi-screen wallconfig with or without bezels/edge-blending
c
      character (16) screen(0:31,0:31)
      character (42) wallname
      character (6) edge_x,edge_y
c
  1   continue
      write(*,101)
      read(*,*)nx
      write(*,102)
      read(*,*)ny
c
      if(nx.lt.1.or.ny.lt.1
     *  )then
             write(*,112)
             go to 1
      endif
c
      nscreens=0
      do j=0,ny-1
         do i=0,nx-1
            read(3,100,err=97,end=98)screen(i,j)
            nscreens=nscreens+1
         enddo
      enddo
c
      write(*,103)
      read(*,*)nx_display
      write(*,104)
      read(*,*)ny_display
c
  2   continue
      if(nx_display.lt.1.or.ny_display.lt.1
     *  )then
             write(*,112)
             go to 2
      endif
c
      nq=0
      if(nx.gt.1
     *  )then
             nq=1
             write(*,105)
             read(*,*)nx_bezel
             if(nx_bezel.lt.0)edge_x='Xblend'
             if(nx_bezel.eq.0)edge_x='Xbutts'
             if(nx_bezel.gt.0)edge_x='Xbezel'
         else
             edge_x=''
      endif
c
      if(ny.gt.1
     *  )then
             nq=nq+2
             write(*,106)
             read(*,*)ny_bezel
             if(ny_bezel.lt.0)edge_y='Yblend'
             if(ny_bezel.eq.0)edge_y='Ybutts'
             if(ny_bezel.gt.0)edge_y='Ybezel'
         else
             edge_y=''
      endif
c
      nx_wall=(nx*nx_display)+((nx-1)*nx_bezel)
      ny_wall=(ny*ny_display)+((ny-1)*ny_bezel)
c
      if(nq.eq.0)write(wallname,1070)nscreens,nx_wall,ny_wall
     *                           ,nx_display,ny_display
c
      if(nq.eq.1)write(wallname,1071)nscreens,nx_wall,ny_wall
     *                           ,nx_display,ny_display
     *           ,trim(adjustl(edge_x))
c
      if(nq.eq.2)write(wallname,1071)nscreens,nx_wall,ny_wall
     *                           ,nx_display,ny_display
     *           ,trim(adjustl(edge_y))
c
      if(nq.eq.3)write(wallname,1072)nscreens,nx_wall,ny_wall
     *                           ,nx_display,ny_display
     *           ,trim(adjustl(edge_x)),trim(adjustl(edge_y))
c
      write(*,111)wallname
c
      open(8,file=wallname,form='formatted',status='unknown')
      lwx=nx_display+nx_bezel
      lwy=ny_display+ny_bezel
      write(*,113)nx_wall,ny_wall
      do j=0,ny-1
         ly=j*lwy
         do i=0,nx-1
            lx=i*lwx
            write(*,108)screen(i,j),nx_wall,ny_wall,lx,ly
     *                            ,nx_display,ny_display
            write(8,108)screen(i,j),nx_wall,ny_wall,lx,ly
     *                            ,nx_display,ny_display
         enddo
      enddo
      stop
c
 97   continue
      write(0,110)
      stop
c
 98   continue
      write(0,109)nscreens,nx*ny
      stop
c
100   format(a)
101   format('How many screens in X direction?')
102   format('How many screens in Y direction?')
103   format('Individual screen resolution in X direction?')
104   format('Individual screen resolution in Y direction?')
105   format('Bezel width in X direction? (Negative for edge-blending)')
106   format('Bezel width in Y direction? (Negative for edge-blending)')
1070  format(i3.3,2('_',i4.4,'x',i4.4),'.cfg')
1071  format(i3.3,2('_',i4.4,'x',i4.4),'_',a,'.cfg')
1072  format(i3.3,2('_',i4.4,'x',i4.4),2('_',a),'.cfg')
108   format(a,' ',6i6)
109   format('Error reading screen name file - read',i6,' of',i6
     *      ,' expected screens')
110   format('Error reading screen name file')
111   format('Wallname file is: ',a)
112   format('Both of these values must be > zero..')
113   format(/'Overall wall dimensions are',i6,' x',i6,/)
c
      end
