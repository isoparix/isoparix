      subroutine vrecord(ntouch,ncb,i,j,iq,idvertex)
      use isocomm
c
c      Records details of a touched vertex. 
c      I represents the piece of space filled by the process, as in
c                           3 | 4
c                           --+--
c                           1 | 2
c      and J is the touched side, as in    (+ represents a vertex)
c                             1
c                          +-----+
c                          |     |
c                        4 |     | 2
c                          |     |
c                          +-----+
c                             3
      ntouch=ntouch+1
      kvtx (ntouch)=idvertex
      kvj  (ntouch)=j
      kcube(ntouch)=ncb
      ivd(i)=1
      if(j.gt.0)then
                    ivd(j)=1
                    nev(j,ncb)=nev(j,ncb)+1
c
c      Find the ceiling for this value IQ
c
                    iqv=-1
                    do nv=1,nev(j,ncb)
                       if(iqv.lt.0.and.iq.lt.lvlist(nv,j,ncb))then
                                                                  iqv=nv
                       endif
                    enddo
c
c      Found it, so move everyone else up - or not...
c
                    if(iqv.gt.0)then
                                 do nv=nev(j,ncb),iqv+1,-1
                                    lvlist(nv,j,ncb)=lvlist(nv-1,j,ncb)
                                    lvid  (nv,j,ncb)=lvid  (nv-1,j,ncb) 
                                 enddo
                         else
                                 iqv=nev(j,ncb)
                    endif
c
c      ...and slot in the new value.
c
                    lvlist(iqv,j,ncb)=iq
                    lvid  (iqv,j,ncb)=idvertex 
      endif
c
c    ==J==
c      4 is X1=ixa
c      3 is Z1=ixa
c      2 is X2=ixa
c      1 is Z2=ixa
c
100   format('Line vertex',i3,' ID',i3,' entry',i5,' created on edge',i2
     *      ,' of cube',i3,' at',2i5,12(/2i5))
c
      return
      end
