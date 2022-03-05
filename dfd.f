      subroutine dfd(alon,alat,x,y)
c
c      Reads in data pairs n decimal degrees and gives offsets from 'Derby'
c
      implicit real(8) (a-h,o-z)
c
      xref=-1.5D+00
      yref=53.0D+00
c     radius=6378.1D+00 ! Kilometres
      radius=3963.2D+00 ! Miles
c
c      Calculate 'flat-earth' co-ordinates
c
      x=sphere_dist(alon,alat,xref,alat,radius)
      y=sphere_dist(alon,alat,alon,yref,radius)
      if(xref.gt.alon)x=-x
      if(yref.gt.alat)y=-y
c
      return
c
      end
