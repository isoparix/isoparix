      program polymaps
      write(*,2000)'A test map'
      write(*,2001)
      write(*,2002) 51.455774,-.312119
      write(*,2002) 51.456853,-.312119
      write(*,2002) 51.456853,-.307424
      write(*,2002) 51.456827,-.307424
      write(*,2002) 51.456827,-.307465
      write(*,2002) 51.456519,-.307465
      write(*,2002) 51.456519,-.307506
      write(*,2002) 51.456108,-.307506
      write(*,2002) 51.456108,-.307465
      write(*,2002) 51.455800,-.307465
      write(*,2002) 51.455800,-.307424
      write(*,2002) 51.455774,-.307424
      write(*,2002) 51.455774,-.312119
      write(*,2003)
      write(*,2004)
      stop
2000   format('      <!DOCTYPE html>'
     *      ,/'<html>'
     *      ,/'  <head>'
     *      ,/'  <meta name="viewport" content="initial-scale=1.0'
     *      ,                                ',user-scalable=no">'
     *      ,/'  <meta charset="utf-8">'
     *      ,/'  <title>Google Maps JavaScript API v3 ',a,'</title>'
     *      ,/'  <link href="http://code.google.com//apis/maps/'
     *      ,     'documentation/javascript/examples/default.css"'
     *      ,     ' rel="stylesheet" type="text/css" rel="stylesheet">'
     *      ,/'  <script type="text/javascript" '
     *      ,/      'src="https://maps.googleapis.com/maps/api/js?'
     *      ,       'key=AIzaSyBrt_zwpr1_uZXfGENhPDVQCIuNZIVKPk8'
     *      ,       '&sensor=false">'
     *      ,/'  </script>'
     *      ,/'  <script type="text/javascript">'
     *      ,/'    var map;'
     *      ,/'    var infoWindow;'
     *      ,/'    function initialize() {'
     *      ,/'    var myLatLng = new google.maps.LatLng(',f0.6,f0.6');'
     *      ,/'    var mapOptions = {zoom: ',i3,', center: myLatLng'
     *      ,       ',mapTypeId: google.maps.MapTypeId.ROADMAP};'
     *      ,/'      map = new google.maps.Map(document.getElementById'
     *      ,       "('map-canvas'),mapOptions);"
     *       )
2001  format('//'
     *     ,/'//   Start of a polygon section'
     *     ,/'//'
     *     ,/'     var polygonCoords = ['
     *      )
2002  format('new google.maps.LatLng(',f0.6,',',f0.6,'),')
2003  format('                         ];'
     *     ,/' var polygon',i3.3,' = new google.maps.Polygon'
     *     ,/'({paths: polygonCoords,'
     *     ,/'  strokeColor: "#FFFFFF",'
     *     ,/'  strokeOpacity: 0.0,'
     *     ,/'  strokeWeight: 1,'
     *     ,/'  fillColor: "#',z6,'",'
     *     ,/'  fillOpacity: 0.35'
     *     ,/'});'
     *     ,/'polygonNumber.setMap(map);'
     *     ,/' //'
     *     ,/' //   End of a polygon section'
     *     ,/' //'
     *     ,/' }'
     *      )
2004  format('</script>'
     *     ,/'   </head>'
     *     ,/'   <body onload="initialize()">'
     *     ,/'     <div id="map-canvas"></div>'
     *     ,/'   </body>'
     *     ,/'</html>'
     *      )
c
      end
