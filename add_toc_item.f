      subroutine add_toc_item(nhead,text)
c
c      Adds list items to Table of Contents in indented fashion
c
      use allcomms
      character *48 text
c
      n_htm_refs=n_htm_refs+1
c
c      Add lists if need be
c
      if(nhead.gt.list_depth
     *  )then
             do j=list_depth+1,nhead
                write(10,103)
                list_depth=list_depth+1
             enddo
      endif
c
c      Close lists if need be
c
      call close_lists(nhead)
c
      write(10,100)n_htm_refs,n_htm_refs,trim(text)
      write(16,101)nhead,n_htm_refs,n_htm_refs,trim(text),nhead
c
100   format('<li><A NAME=ToC_',i0.0,' HREF="#Header_',i0.0,'" >'
     *       ,a,'</A><BR>')
101   format('<H',i0.0,'><A NAME="Header_',i0.0,'" HREF="#ToC_',i0.0
     *      ,'" >',a,'</A></H',i0.0,'>')
103   format('<ul>')
c
      return
      end
