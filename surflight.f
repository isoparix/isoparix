c      Calculate theta (x-z plane) and phi (y and x-z)
C
      rad2deg=float(180*113)/355.0
      dx=xe-xcv
      dy=ye-ycv
      dz=ze-zcv
      r=sqrt(dx**2+dy**2+dz**2)
      if(dz.ne.0
     *  )then
             theta=atan(dx/dz)
         else
             theta=355.0/226.0  ! pi/2
      endif
C
      phi=acos(sqrt(dx**2+dz**2)/r)
      OPEN(40,FILE=trim(vufile)//'.light',FORM='FORMATTED'
     *      ,STATUS='unknown',ERR=99)
         write(40,111)rad2deg*theta,rad2deg*phi
         write( *,111)rad2deg*theta,rad2deg*phi
      close(40)
      
C
