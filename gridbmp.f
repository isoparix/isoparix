      subroutine gridbmp
c
c    Produces a bit map of the grid
c
      use venn_comms
      use bmp_comms
c
      implicit real(8) (a-h,o-z)
c
      real(8) north,south,east,west
c
      character(1),dimension(nxlow:nxhi,nylow:nyhi) :: kgrid
      character(40) overlay
c
      if(check)write(*,*)ncolstep,maxdepth,nxlow,nxhi,nylow,nyhi
      iyg=nyhi+1
      do iy=nylow,nyhi
         iyg=iyg-1
         do ix=nxlow,nxhi
            if(grid(ix,iy).ge.0
     *        )then
                   kgrid(ix,iyg)=char(ncolstep*grid(ix,iy))
               else
                   kgrid(ix,iyg)=char(255)
            endif
         enddo
      enddo
c
      ixdim=nxhi-nxlow+1
      iydim=nyhi-nylow+1
      call array2bmp(ixdim,iydim,kgrid)
c
c      Create Google overlay
c
      north=float(nyhi )
      south=float(nylow)
      east =float(nxhi )
      west =float(nxlow)
c
      north=y2lat(north)
      south=y2lat(south)
      east =x2lon(east)
      west =x2lon(west)
      gscale=0.75
c
      write(overlay,101)trim(title)
      open(200,file=trim(overlay),status='unknown',form='formatted')
      write(200,100)trim(bmname),gscale,north,south,east,west
      call flush(200)
      close(200)
c
      return
c
100   format('<?xml version="1.0" encoding="UTF-8"?>'
     *     ,/'<kml xmlns="http://www.opengis.net/kml/2.2"'
     *     , ' xmlns:gx="http://www.google.com/kml/ext/2.2"'
     *     , ' xmlns:kml="http://www.opengis.net/kml/2.2"'
     *     , ' xmlns:atom="http://www.w3.org/2005/Atom">'
     *     ,/'<GroundOverlay>'
     *     ,/'   <name>Untitled Image Overlay</name>'
     *     ,/'   <color>82ffffff</color>'
     *     ,/'   <Icon>'
     *     ,/'      <href>',a,'.bmp</href>'
     *     ,/'      <viewRefreshMode>onRequest</viewRefreshMode>'
     *     ,/'      <viewBoundScale>',f0.3,'</viewBoundScale>'
     *     ,/'   </Icon>'
     *     ,/'   <LatLonBox>'
     *     ,/'      <north>',f0.10,'</north>'
     *     ,/'      <south>',f0.10,'</south>'
     *     ,/'       <east>',f0.10,'</east>'
     *     ,/'       <west>',f0.10,'</west>'
     *     ,/'   </LatLonBox>'
     *     ,/'</GroundOverlay>'
     *     ,/'</kml>'
     *      )
101   format(a,'_bmp.kml')
      end
