      real (4) function amedian(data_value,aspect_med,index_low)
c
c      Returns median of top N data points
c
      use allcomms
c
      real (4) aspect_med(lenmed)
c
      if(data_value.gt.aspect_med(index_low)
     *  )then
             do index=1,index_low
                if(data_value.gt.aspect_med(index)
     *            )then
                       index_x=index
                       exit
                endif
             enddo
c
             if(index_low.lt.lenmed
     *         )then
                    index_low=index_low+1
             endif
c
             do index=index_low,index_x+1,-1
                aspect_med(index)=aspect_med(index-1)
             enddo
             aspect_med(index_x)=data_value
c
      endif
c
      if(aspect_med(index_low).eq.0.
     *  )then
             m=index_low-1
         else
             m=index_low
      endif
c
      k=(1+m)/2
      if(mod(m,2).eq.0
     *  )then
             amedian=(aspect_med(k)+aspect_med(k+1))/2
         else
             amedian=aspect_med(k)
      endif
c
c            do index=1,index_low
c               write(*,100)index,aspect_med(index)
c            enddo
      return
c
100   format(i4,f10.1)
c
      end
