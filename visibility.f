      logical function visibility(xtest,ytest
     *                           ,xv1,yv1,xv2,yv2,xv3,yv3,xv4,yv4)
c
c      Is (xtest,ytest) inside or outside convex quadrilateral 
c      bounded by (xv1,yv1),.....(xv4,yv4)?
c
c      'visibility - .true. means (xtest,ytest) in NOT in quad..
c
      use isocomm
      implicit real(8) (a-h,o-z)
c
      visibility=.false.
      resolved=.false.
      npx=0
      npy=0
c
c      Try simple tests first...
c
              if(xtest.lt.xv1)npx=npx+1
              if(xtest.lt.xv2)npx=npx+1
              if(xtest.lt.xv3)npx=npx+1
              if(xtest.lt.xv4)npx=npx+1
              if(npx.eq.0.or.npx.eq.4)then
                                          resolved=.true.
                                          visibility=.true.
c                                                write(*,1041)
c                                     else
c            write(*,800)xtest,npx,xv1,xv2,xv3,xv4
c    *                  ,xtest-xv1,xtest-xv2,xtest-xv3,xtest-xv4
              endif
c
      if(.not.resolved
     *  )then
              if(ytest.lt.yv1)npy=npy+1
              if(ytest.lt.yv2)npy=npy+1
              if(ytest.lt.yv3)npy=npy+1
              if(ytest.lt.yv4)npy=npy+1
              if(npy.eq.0.or.npy.eq.4)then
                                          resolved=.true.
                                          visibility=.true.
c                                                write(*,1042)
c                                     else
c            write(*,801)ytest,npy,yv1,yv2,yv3,yv4
c    *                  ,ytest-yv1,ytest-yv2,ytest-yv3,ytest-yv4
              endif
 
      endif
c
      if(.not.resolved
     *  )then
             alpha=acos(cosrule(xv1,yv1,xtest,ytest,xv2,yv2))
             beta =acos(cosrule(xv2,yv2,xtest,ytest,xv3,yv3))
             gamma=acos(cosrule(xv3,yv3,xtest,ytest,xv4,yv4))
             delta=acos(cosrule(xv4,yv4,xtest,ytest,xv1,yv1))
c
             if(check
     *         )then
             write(20,800)xtest,npx,xv1,xv2,xv3,xv4
     *                  ,xtest-xv1,xtest-xv2,xtest-xv3,xtest-xv4
             write(20,801)ytest,npy,yv1,yv2,yv3,yv4
     *                  ,ytest-yv1,ytest-yv2,ytest-yv3,ytest-yv4
      write(20,103)alpha,beta,gamma,delta,113.*(alpha+beta+gamma+delta)
             endif
c
             if(113.*(alpha+beta+gamma+delta).lt.709.999
     *         )then
                    visibility=.true.
             endif
      endif
c
800   format('Xtest:',f6.1,i2,4f6.1,' : ',4f6.1)
801   format('Ytest:',f6.1,i2,4f6.1,' : ',4f6.1)
c
100   format('HIDDEN by cube',i3)
101   format('VISIBLE over cube',i3)
102   format('Vertex',i3,' of cube',i3,' is ',$)      
103   format('Angles:',4f6.3,', 113 * SUM=',f10.5,/)
1041  format('Vertex is visible by X-co-ords')
1042  format('Vertex is visible by Y-co-ords')
1043  format('Vertex is NOT visible by X- and Y-co-ords')
1044  format('Vertex is visible by cosines')
1045  format('Vertex is NOT visible by cosines')
      end
