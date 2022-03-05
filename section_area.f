      program section_area
c
c      Calculates cross-sectional area in sq mm of various sections
c      Input in inches, output in sq mm
c
      implicit real(8)(a-h,o-z)
c
      character(1)section_type
c
      q=25.4**2
      rhoal=.0027
  1   continue
      write(*,101)
      read(*,*)section_type,a,t
c
      if(section_type.eq.'t'
     *  )then
             write(*,102)
             read(*,*)b
             area=q*((a*b)-2.0*((b-t)*(.5*(a-t))))
             write(*,1001)'T-section:',a,b,t,area,area*rhoal
             go to 1
      endif
c
      if(section_type.eq.'b'
     *  )then
             area=q*((a**2)-((a-(2*t))**2))
             deflection=1./((a**4)-((a-(2*t))**4))
             write(*,100)'Square box-section:',a,t,area,area*rhoal
     *                  ,deflection
             go to 1
      endif
c
      if(section_type.eq.'p'
     *  )then
             area=q*355.*((a/2.)**2-((a-(2.*t))/2.)**2)/113.
             write(*,100)'Pipe:',a,t,area,area*rhoal
             go to 1
      endif
c
      if(section_type.eq.'l'
     *  )then
             area=q*(a**2-(a-t)**2)
             write(*,100)'Angle:',a,t,area,area*rhoal
             go to 1
      endif
c
      go to 1
c
      stop
c
100   format(a,2f8.4,': Sq mm, or cc/metre, =',f8.2
     *      ,'.  Mass/m (Al) =',f8.2,' kg. Relative deflection:',f8.4)
1001  format(a,3f8.4,': Sq mm, or cc/metre, =',f8.2
     *      ,'.  Mass/m (Al) =',f8.2,' kg')
101   format(/'Section type (b/l/t/p), and dimensions in inches?')
102   format('Enter depth of T-section, in inches')
c
      end
