c     subroutine intext(x,y,ncross,inside)
      logical function intext(x,y)
c 
c       Is the point (x,y) inside the polygon defined by segment(:,:)
c 
      use venn_comms
c 
      implicit real(8) (a-h,o-z)
c 
      logical intersector,inside
c 
      real(8),dimension(4) :: segment
c 
c       Create a test segment
c 
      segment(1)=x
      segment(2)=y
      segment(3)=xmax
      segment(4)=ymax
c 
      segslope=slope(x,y,xmax,ymax)
      segcept =cept (x,y,xmax,ymax)
c 
      xinold=huge(x)
      yinold=huge(y)
      ncross=0
      do n=1,ns_00     !  All the segments
         call crossover(segslope_00(n),segcept_00(n),segslope,segcept
     *                 ,segment_00(:,n),segment,x_intercept,y_intercept
     *                 ,intersector)
         if(intersector
     *     )then
                if(x_intercept.ne.xinold.or.y_intercept.ne.yinold
     *            )then     !  Not the join of two segments...
c                      write(*,101)x_intercept,y_intercept
c    *                            ,segment_00(:,n)
                       ncross=ncross+1
c                  else
c                      write(*,102)x_intercept,y_intercept
c    *                            ,segment_00(:,n)
                endif
                xinold=x_intercept
                yinold=y_intercept
         endif
      enddo
c 
      if(ncross-(2*(ncross/2)).eq.0
     *  )then
             intext=.false.
         else
             intext=.true.
      endif
c 
c     write(*,100)x,y,xmax,ymax,ncross,inside
c 
100   format('INTEXT:',4f10.5,i3,l3)
101   format('INTEXT: Intersection at',2f10.5,' with',4f10.5)
102   format('INTEXT: Intersection at',2f10.5,' with',4f10.5
     *      ,' (duplicate - ignored)')
c 
      return
      end
