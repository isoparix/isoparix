      subroutine close_lists(nhead)
c
c      Close lists if need be
c
      use allcomms
c
      if(nhead.lt.list_depth
     *  )then
             k=list_depth
             do j=nhead+1,k
                write(10,104)
                list_depth=list_depth-1
             enddo
      endif
c
      return
c
104   format('</ul>')
c
      end
