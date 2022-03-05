      logical function polytest(xsa,xsb,xea,xeb)
c
c      Test for new polygon
c
      implicit real(8) (a-h,o-z)
      logical check
      check=.false.
c
c     if(check
c    *  )then
c            write(18,204)ida,xsa,xea,yea
c    *                   ,idb,xsb,xeb,yeb
c     endif
c
c      ida is depth(n)
c      idb is depth(n+1)
c
c      xsa is xstart(n)
c      xsb is xstart(n+1)
c
c      xea is xend  (n)
c      xeb is xend  (n+1)
c
c      yea is yend  (n)
c      yeb is yend  (n+1)
c
      polytest=.false.
c     if(ida.ne.idb
c    *  )then
c            if(check
c    *         )then
c                   write(18,200)int(yea)
c            endif
c            polytest=.true.
c     endif
c
      if(xsa.ge.xeb
     *  )then
             if(check
     *         )then
                    write(18,201)int(yea)
             endif
             polytest=.true.
      endif
c
      if(xea.le.xsb
     *  )then
             if(check
     *         )then
                    write(18,202)int(yea)
             endif
             polytest=.true.
      endif
c
c     if(yea.eq.yeb
c    *  )then
c            if(check
c    *         )then
c                   write(18,205)int(yea)
c            endif
c            polytest=.true.
c     endif
c
c     if((sqrt((yea-yeb)**2)).gt.1.1
c    *  )then
c            if(check
c    *         )then
c                   write(18,203)int(yea)
c            endif
c            polytest=.true.
c     endif
c
      return
c
      
200   format('POLYTEST     Y=',i5,': Change of depth')
201   format('POLYTEST     Y=',i5,': XSTART > next XEND')
202   format('POLYTEST     Y=',i5,': XEND < next XSTART')
203   format('POLYTEST     Y=',i5,': Line break')
204   format('POLYTEST: ',i4,3f10.4
     *      ,'          ',i4,3f10.4)
205   format('POLYTEST: Y-values are equal')
c
      end
