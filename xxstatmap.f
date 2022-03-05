      subroutine xxstatmap
c
c     Prepare a Windows colour map in quarters blue-cyan-green-yellow-red
c
      use bmp_comms
      use ip_comms
c
      character(1)rgb(0:1023)
      character(1024)rgbq
      equivalence(rgb,rgbq)
c
      navail=255
      k1=0
      k2=(4*navail)/16
c
      a=k2-k1
      rr=1./a
      c=0.
c
      do k=k1,k2
           m=k*4
           rgb(m+2)=char(0)
           rgb(m+1)=char(int(255.*sqrt(c*rr)))
           rgb(m+0)=char(255)
           rgb(m+3)=char(0)
           c=c+1.
      enddo
c
      k1=k2
      k2=navail/2
c
      a=k2-k1
      rr=1./a
      c=0.
c
      do k=k1,k2
           m=k*4
           rgb(m+2)=char(0)
           rgb(m+1)=char(255)
           rgb(m+0)=char(int(255.*(sqrt(1.-(c*rr)))))
           rgb(m+3)=char(0)
           c=c+1.
      enddo
c
      k1=k2
      k2=(12*navail)/16
c
      a=k2-k1
      rr=1./a
      c=0.
c
      do k=k1,k2
           m=k*4
           rgb(m+2)=char(int(255.*sqrt(c*rr)))
           rgb(m+1)=char(255)
           rgb(m+0)=char(0)
           rgb(m+3)=char(0)
           c=c+1.
      enddo
c
      k1=k2
      k2=navail
c
      a=k2-k1
      rr=1./a
      c=0.
c
      do k=k1,k2
           m=k*4
           rgb(m+2)=char(255)
           rgb(m+1)=char(int(255.*(sqrt(1.-(c*rr)))))
           rgb(m+0)=char(0)
           rgb(m+3)=char(0)
           c=c+1.
      enddo
c
      rgb(0:2)      =char(0)    ! Black
      rgb(3)        =char(0)    ! Black
c
c     rgb(4:6)      =char(192)  ! Grey
c     rgb(7)        =char(0)    ! Grey
c
      rgb(1020:1022)=char(255)  ! White
      rgb(1023)      =char(0)   ! White
c
      rgbquad=rgbq
c
      return
c
      end
