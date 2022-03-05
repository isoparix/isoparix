c
         do my=1,tzc
            tl(my)=zl(my,iyp)
            tg(my)=zg(my,iyp)
            tr(my)=zr(my,iyp)
         enddo
c
         iz=zr(1,iyp)
         za=zg(1,iyp)
         do ix=2,zcount(iyp)
            zb=zg(ix,iyp)
            if(iz.eq.zl(ix,iyp)
     *        )then
                   do mx=2,zcount(iyq)
                      if(za.gt.zg(mx-1,iyq).and.
     *                   zb.gt.zg(mx-1,iyq).and.
     *                   za.lt.zg(mx,iyq).and.
     *                   zb.lt.zg(mx,iyq).and.
     *                   iz.ne.zl(mx,iyq)
     *                  )then
                             bracketed=.true.
                             write(18,223)iyp,yg(iyp),za,zb,iz
     *                       ,zg(mx-1,iyq),zg(mx,iyq),zr(mx-1,iyq)
     *                       ,iyq
c
c      Reflect the bracketing...
c
c                          za   zb
c            ix-2         ix-1  ix             ix+1
c      iyp     *------------*||||*--------------*   zcount(iyp)>zcount(iyq)
c      NEW     *--------------------------------* 
c      iyq            *-----------------------*
c                    mx                      mx+1
c
                             ty=yg(iyp)
                             tl(ix-1)=-2   ! Discard this point
                             tl(ix  )=-2   ! Discard this point
                      endif
                   enddo
            endif
            za=zb
            iz=zr(ix,iyp)
         enddo
c
