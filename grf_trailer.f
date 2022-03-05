      subroutine grf_trailer(npstanza)
c      
      write(20,102)npstanza
      close(20)
c      
      return
c      
102   format('[Data]'
     *     ,/'TextLabelCount = 0'
     *     ,/'FuncCount = 0'
     *     ,/'PointSeriesCount =',i4
     *     ,/'ShadeCount = 0'
     *     ,/'RelationCount = 0'
     *     ,/'OleObjectCount = 0'
     *     ,/)
c      
      end
