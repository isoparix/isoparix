      subroutine rectput(ix1,iy1,ix2,iy2,ncol)
c
c      Stores rectangle detail for later despatch and display
c
      use isocomm
c
      if(ngrf-ngline.lt.5)call grafout
c
c      Add on the rectangle details
c
      ngrafout(ngline+1)=ix1
      ngrafout(ngline+2)=iy1
      ngrafout(ngline+3)=ix2
      ngrafout(ngline+4)=iy2
      ngrafout(ngline+5)=-ncol
      ngline=ngline+5
      npels=npels+(ix2-ix1+1)*(iy2-iy1+1)
c      write(*,200)ix2,ix1,iy2,iy1,(ix2-ix1+1)*(iy2-iy1+1),npels
c200   format(6i12)
      mpels=mpels+(ix2-ix1+1)*(iy2-iy1+1)
      kpels=kpels+(ix2-ix1+1)*(iy2-iy1+1)
c
      return
      end
