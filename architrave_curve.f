      program architrave_curve
c      
c      
c      
  1   continue
      write(*,100)
      read(*,*)width
      if(width.le.0)stop
      write(*,101)
      read(*,*)radius
      write(*,102)
      read(*,*)angle
      rngle=angle*3.14159265358979/360.
      write(*,104)angle,radius, width
c
      write(*,105)
      do nsections=1,11,2
         al=tan(rngle/float(nsections))
         al1=2.*al* radius
         al2=2.*al*(radius-width)
         write(*,106)nsections,al1,al2,al*400.,al1*float(nsections)
      enddo
      go to 1
c
100   format(//'Width of architrave?')
101   format('Outer radius?')
102   format('Angle between end-faces in degrees?')
103   format('Number of sections?')
104   format('To join endfaces at', f5.1,' degrees,'
     *      ,' outer radius',f6.1,' with architrave',f5.1,' wide:')
105   format(/'Number of    Outer    Inner  Angle  Total'
     *      ,/' sections distance distance at 400 length')
106   format(i9,2f9.2,2f7.2)
c
      stop
      end
