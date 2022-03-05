      subroutine polysplit(np)
c
c      Put in extra points as circle centres if the original points are
c      too far apart
c
      use venn_comms
c
      implicit real(8) (a-h,o-z)
c
      if(np.le.0
     *  )then
             write(0,103)np
             stop
         else
             write(18,104)np
      endif
c
      loc_centres(1)=1
      ncentres=1
c     if(np.eq.1
c    *  )then  
c            pxp(1)=xp(1)
c            pyp(1)=yp(1)
c            prp(1,1)=r1p(1)
c            prp(1,2)=r2p(1)
c            return
c     endif
c
      do n=2,np
         pxp(ncentres)=xp(n-1)
         pyp(ncentres)=yp(n-1)
         prp(ncentres,1)=r1p(n-1)
         prp(ncentres,2)=r2p(n-1)
c
         d=sqrt(pyth2d(xp(n-1),yp(n-1),xp(n),yp(n)))
         ndivs=int(d*lines_per_mile)
c
c      Space out the centres and radii of the circles
c
         delta_x =( xp(n)- xp(n-1))/float(1+ndivs)
         delta_y =( yp(n)- yp(n-1))/float(1+ndivs)
         delta_r1=(r1p(n)-r1p(n-1))/float(1+ndivs)
         delta_r2=(r2p(n)-r2p(n-1))/float(1+ndivs)
c
c      Create all the intermediate centres and radii
c
c               write(12,101)n,d,miles_per_line,ndivs
c      *                    ,xp(n-1),yp(n-1),xp(n),yp(n)
         do m=1,ndivs
            ncentres=ncentres+1
            pxp(ncentres)  = xp(n-1)+(float(m)*delta_x)
            pyp(ncentres)  = yp(n-1)+(float(m)*delta_y)
            prp(ncentres,1)=r1p(n-1)+(float(m)*delta_r1)
            prp(ncentres,2)=r2p(n-1)+(float(m)*delta_r2)
         enddo
c
         ncentres=ncentres+1
         loc_centres(n)=ncentres  ! Record the end-point
         pxp(ncentres)=xp(n)
         pyp(ncentres)=yp(n)
         prp(ncentres,1)=r1p(n)
         prp(ncentres,2)=r2p(n)
c
c        if(check
c    *     )then
c               do nx=loc_centres(n-1),loc_centres(n)
c                  write(12,114)nx,pxp(nx),pyp(nx),prp(nx,1),prp(nx,2)
c               enddo
c        endif
      enddo  ! n=2,np
c
      return
c
100   format('POLYSPLIT:',i4.4,'_',i4.4,'_base_poly')
101   format('POLYSPLIT: n=',i6,', d,miles_per_line:',2f10.3
     *      ,', ndivs=',i6
     *      ,' from',2f10.2,' to',2f10.2," (below)")
102   format('POLYSPLIT: na=',i3,', nb=',i3,', extend is:',l2)
103   format('POLYSPLIT: ERROR - NP=',i10)
104   format('POLYSPLIT: NP=',i10)
114   format('POLYSPLIT: Circle pair',i6,', centre=',f10.5,',',f10.5
     *      ,', radii=',2f10.5)
      end   
