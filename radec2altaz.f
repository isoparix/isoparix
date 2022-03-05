      function [Alt Az] = RADec2:AltAz(RA,Dec,Latitude,Longitude,time)
c       By: Christopher Wilcox and Ty Martinez
c       Jan 22 2010
c       Naval Research Laboratory
c       
c       Description:  Convert Right Ascension/Declination angles in degrees to 
c                     Altitude/Azimuth in degrees for an Alt/Az telescope mount.
c      
c       Input:    RA - Right Ascension in degrees
c                 Dec - Declination in degrees
c                 Latidute - Observer's Latitude (Negative for South) in degrees
c                 Longitude - Observer's Longiture (Negative for West) in degrees
c                 (optional) time - Date vector, as returned from 'clock.m', if 
c                                   not supplied, the current date/time is used.
c       Output:   Altitude - Telescope Altitude in degrees
c                 Azimuth - Telescope Azimuth in degrees
c

      if(nargin.eq.4
     *  )then 
             time = clock
      endif
c
      iyear= time(1)
      month= time(2)
      iday = time(3)
      ihour= time(4)
      mins = time(5)
      isec = time(6)

      JD=floor(365.25*(iyear+4716.0))
     *  +floor(30.6001*(month+1.0))+2.0
     *  -floor(iyear/100.0)
     *  +floor(floor(iyear/100.0 )/4.0)
     +  +iday-1524.5
     *  +(ihour+mins/60+sec/3600)/24
      D = JD - 2451543.5
      w = 282.9404 + 4.70935e-5*D
      M = mod(356.0470 + 0.9856002585*D,360)
      L = w + M
      GMST0 = mod(L + 180,360)/15
      UT_hour = hour + min/60 + sec/3600
      SiderealTime = GMST0 + UT_hour + Longitude/15
c
      HourAngle=(SiderealTime*15-RA)
      A=cosd(HourAngle)*cosd(Dec)*cosd(90-Latitude)-sind(Dec)*sind(90-Latitude)
      B=sind(HourAngle)*cosd(Dec)
      C=cosd(HourAngle)*cosd(Dec)*sind(90-Latitude)+sind(Dec)*cosd(90-Latitude)
c
      Az =atan2(B,A)*180/pi + 180
      Alt=asin(C)*180/pi
c
      return
c
      end
            
