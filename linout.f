      subroutine linout(ix1,iy1,ix2,iy2,miniter)
c
c      Writes the next box to be looked at
c
      use isocomm
c
      if(miniter.lt.0
     *  )then
             write(txtout,100)ix1,iy1,ix2,iy2,miniter
             call statout
             return
      endif
c
      if(mline.ge.lindet_entries
     *  )then
             write(txtout,101)lindet_entries
             call statout
             return
      endif
c
      mline=mline+1
      if(mline.gt.nline)nline=mline
      lindet(1,mline)=ix1
      lindet(2,mline)=iy1
      lindet(3,mline)=ix2
      lindet(4,mline)=iy2
      lindet(5,mline)=miniter
      return
c
100   format('ERR LINOUT: miniter<0',4i6,i9)
101   format('ERR LINOUT: All',i6,' lindet entries full')
c
      end
