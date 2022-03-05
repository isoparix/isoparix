      program cake
c
c      Q: Where you put the dowels in a multi-tier cake?
c      A: on the circle where the load inside equals the load outside
c         the circle.
c      Calculate the weight of successive cylinders working in from the
c      outside and see where inside = outside load
c
      implicit real*8 (a-h,o-z)
      dimension tier_diam(5),tier_height(5),tier_load(5),tier_sum(5)
     *         ,tier_weight(5),total_sum(5),tier_sum_old(5)
      logical halftier(5)
c
      tier_diam(1)=13 !12
      tier_diam(2)=11 !10
      tier_diam(3)=9  !8
      tier_diam(4)=7  !6
c
      tier_height(1)=3
      tier_height(2)=3
      tier_height(3)=3
      tier_height(4)=3
c
      ntiers=4
      nsteps=5000
      dsteps=tier_diam(1)/float(nsteps)
c
      qpi=355./(113.0*4.0)  ! Quarter PI, or PI/4...
c
      do nt=1,ntiers
         tier_weight(nt)=qpi*tier_height(nt)*tier_diam(nt)**2
      enddo
c
      halftier=.true.
      total_sum=0.
      do nt=1,ntiers
         do nta=ntiers,nt,-1
            total_sum(nt)=total_sum(nt)+tier_weight(nta)
         enddo
      enddo
c
      do ns=1,nsteps
         d=dsteps*float(ns)
         do nt=ntiers,1,-1
            if(d.le.tier_diam(nt)
     *        )then
                   tier_load(nt)=tier_height(nt)*(d**2)*qpi
            endif
         enddo
c
         tier_sum_old=tier_sum
         tier_sum=0.
         do nt=1,ntiers
            do nta=ntiers,nt,-1
               tier_sum(nt)=tier_sum(nt)+tier_load(nta)
            enddo
            if(tier_sum(nt).gt.0.5*total_sum(nt).and.halftier(nt)
     *        )then
                   dhalf=d-(0.5*dsteps)
                   write(*,101)nt,tier_diam(nt),dhalf
c    *                        ,0.5*(tier_sum(nt)+tier_sum_old(nt))
     *                        ,total_sum(nt)
                   halftier(nt)=.false.
            endif
         enddo
c
c
c        write(*,100)d,(tier_load(mx),mx=1,ntiers
c
      enddo
c
100   format(10f12.3)
101   format('Balance circle below tier',i3,', diameter ',f6.2
     *      ,', has diameter'
     *      ,f8.2,' and carries load of',f8.2)
c
      end
