      subroutine view_avg(title,linesum,nedge,nroll,ldim,lmax
     *           ,speak,edge_avg)
c
c      Looks at all the averages, calcs first and second derivatives
c
      implicit real(8) (a-h,o-z)
c
      character(5) title
      dimension linesum(0:nedge-1)
c
      a=1./float(nroll*ldim)
      b=1./float(ldim)
      k=nroll/2
      z=-huge(z)
      speak=z
      avgold=z
      avgmax=z
      rnroll=1./float(nroll)
      lmax=0
c
      do i=0,nedge-1
         if(i.lt.nroll-1
     *     )then
c               write(*,100)title,i,linesum(i)
            else
                nrollsum=0
                do j=i,i-nroll+1,-1
                   nrollsum=nrollsum+linesum(j)
                enddo
                avg=a*float(nrollsum)
                if(avgold.gt.z
     *            )then
                       deriv1=avg-avgold
                       if(deriv1.gt.speak
     *                   )then
                              if(speak.gt.1.0
     *                          )then
                                     lmax=i-k
                                     edge_avg=b*float(linesum(lmax))
                              endif
                              speak=deriv1
c                             write(*,101)title,i,linesum(i),nrollsum
c    *                            ,lmax,avg,speak
                          else
c                             write(*,109)title,i,linesum(i),nrollsum
c    *                            ,i-k,avg,deriv1
                       endif
                   else
c                      write(*,109)title,i,linesum(i),nrollsum,i-k,avg
                endif
                avgold=avg
         endif
      enddo
c
      return
c
100   format(a5,i5,3i10,2f10.3)
101   format(a5,i5,3i10,2f10.3,' - max slope')
      end
c
c23456789012345678901234567890123456789012345678901234567890123456789012
c        1         2         3         4         5         6         7         
