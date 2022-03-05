      subroutine origin
c
c      Derives relationships between local degrees and distance 
c
      use venn_comms
c
      implicit real(8) (a-h,o-z)
c
      delta_degree=0.01
      alat=-delta_degree  ! Cross-latitude miles are the
      blat= delta_degree  ! same the world over...
      alon=ctrlon-delta_degree
      blon=ctrlon+delta_degree
      radius=3963.2D+00
      miles_per_deglon=0.5*sphere_dist(alon,ctrlat,blon,ctrlat,radius)
     *            /delta_degree
      miles_per_deglat=0.5*sphere_dist(ctrlon,alat,ctrlon,blat,radius)
     *            /delta_degree
      if(check)write(*,100)ctrlon,ctrlat,miles_per_deglon
     *                                  ,miles_per_deglat
c
100   format('ORIGIN:    At longitude',f10.5,' and latitude:',f10.5
     *     ,/'           Miles per degree longitude=',f10.5
     *     ,/'           Miles per degree latitude =',f10.5)
c
      return
      end
