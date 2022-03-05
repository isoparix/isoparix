      real (8) function tdelta(tlast,tfirst)
c
c      Returns difference in times_of_day, compensating for midnight
c
      real (8) tlast,tfirst
c
      if(tlast.lt.tfirst
     *  )then
             tlast=tlast+86400.
      endif
c
      tdelta=tlast-tfirst
      return
      end
