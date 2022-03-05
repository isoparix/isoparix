      subroutine polyfill(lca,lcb,ntype)
c
c      Populates grid from centres are located in pxp and pyp.
c      LGRID is a grid of logical values, the same size as GRID
c
      use venn_comms
c
      implicit real(8) (a-h,o-z)
c
      logical intext,real_roots,unplaced
c
c     if(check
c    *  )then
c            do nc=lca,lcb
c               write(*,800)nc,lca,lcb,ntype,prp(nc,ntype)
c    *                     ,pxp(nc),pyp(nc)
c            enddo
c     endif
      if(check)write(18,802)lca,lcb,ntype
802   format(/'CALL TO POLYFILL:',3i6,/)
      call flush(18)
c
      rmax = maxval(prp(lca:lcb,ntype))
      aymax=(maxval(pyp(lca:lcb))+rmax)*lines_per_mile
      aymin=(minval(pyp(lca:lcb))-rmax)*lines_per_mile
      kymax=aymax+1.0
      kymin=aymin-1.0
c
c           nyl=-.5+((pyp(nc)-r)*lines_per_mile) ! Highest possible Y for this circle
c           nyh= .5+((pyp(nc)+r)*lines_per_mile) ! Lowest  possible Y for this circle
c
      write(18,830)kymin,kymax,lines_per_mile,ygrid(kymin),ygrid(kymax)
830   format('kymin=',i4,' kymax=',i4,' lines_per_mile='
     *      ,f7.2,' range',2f7.2)
      do iy=kymin,kymax      !  FOR loop over these values
         y=ygrid(iy)
         xl= huge(x)
         xh=-huge(x)
         real_roots=.false.
         do nc=lca,lcb    !   FOR loop over all the centres
            t=prp(nc,ntype)**2-(y-pyp(nc))**2
c
c      Record real start and end points of this line.
c
            if(t.ge.0.0
     *        )then   !  We have real roots top and bottom
c
                   real_roots=.true.
                   ds=dsqrt(t)
c
                   x=(pxp(nc)-ds)*lines_per_mile 
                   xl=dmin1(x,xl)
c
                   x=(pxp(nc)+ds)*lines_per_mile 
                   xh=dmax1(x,xh)
c
            endif   !  Real roots check
         enddo  ! nc=1,ncentres
c
c   Now have lowest and highest positions of all circle boundaries on
c   this line IY
c
         if(real_roots
     *     )then
         write(18,831)iy,xl,xh,layer
831   format("At Y=",i3," Leftmost=",f7.2,", Rightmost=",f7.2
     *      ," Layer=",i3)
c               write(*,800)iy,lca,lcb,ntype,xl,xh
800   format(4i4,5f18.9)            
                zcount(iy)=zcount(iy)+1
c
                zal(zcount(iy),iy)=-1
                agrid(zcount(iy),iy)=xl
                if(ntype.eq.1
     *            )then
                       zar(zcount(iy),iy)=0
                       zbl(zcount(iy),iy)=0
                   else
                       zar(zcount(iy),iy)=layer
                       zbl(zcount(iy),iy)=layer
                endif
                bgrid(zcount(iy),iy)=xh
                zbr(zcount(iy),iy)=-1
         endif
c
      enddo   ! kymin,kyma
c
      maxdepth=maxval(zar)   !   Maximum value of the whole grid
      if(check)write(18,105)lca,lcb,layer,maxdepth
c
      return
c
101   format('POLYFILL: nc,nxl,nxh,nyl,nyh,pxp,pyp',5i5,2f10.5)
102   format('POLYFILL: ix,iy,rad1,d2,rad',2i5,3f10.5)
103   format('POLYFILL: pxp,pyp',2f10.5)
104   format('Type ',i1,', grid line',i4,': At left-most grid pixel'
     *       ', X=:',i4,': top_X=',f8.3,', top_Y=',f8.3
     *      ,/58x,  'low_X=',f8.3,', low_Y=',f8.3
     *      ,/23x,'At right-most grid pixel, X=:',i4
     *      ,     ': top_X=',f8.3,', top_Y=',f8.3
     *      ,/58x,  'low_X=',f8.3,', low_Y=',f8.3
     *      ,/)
105   format('POLYFILL: Mapped ncentres from',i6,' to',i6
     *      ,'Layer=',i4,', Highest grid value now',i4)
106   format(/'POLYFILL:iy,nyl,nyh,ya,yb',3i6,2f15.7)
107   format('POLYFILL:',3i6,2f15.7)
1081  format('POLYFILL: Taking the low_X of',2i6,3f15.7)
1082  format('POLYFILL: Taking the low_X of',2i6,3f15.7)
1083  format('POLYFILL: Taking the  hi_X of',2i6,3f15.7)
1084  format('POLYFILL: Taking the  hi_X of',2i6,3f15.7)
      end
