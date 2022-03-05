      subroutine cubespec(nc)
c
      use isocomm
c
      logical visibility,tvis,hider
c
c     Map cube NC.  Define its place on the screen, its hidden faces etc
c
      vdmax(nc)=0.
      x_max(nc)=0.
      y_max(nc)=0.
c
      vdmin(nc)=huge(vdmin(nc))
      x_min(nc)=huge(x_min(nc))
      y_min(nc)=huge(y_min(nc))
    
c
      do nv=1,4
         call photomap(nvf(5,nv),nc)
      enddo
c
      do nv=9,lvc(nc)+1
         call photomap(nv,nc)
      enddo
c
      if(.not.active(nc))return
 
      do nv=1,4
         call photomap(nvf(2,nv),nc)
      enddo
c
      do nf=1,6
         visiface(nf,nc)=.true.
      enddo
c
c      Look at each of the three faces that contain the nearest vertex
c
      visivert(farvert(nc),nc)=.false.
      nr=nearvert(nc)
      do nfnv=1,3
c
         iface=nfv(nr,nfnv)
c
         nv1=nvf(iface,1)
         nv2=nvf(iface,2)
         nv3=nvf(iface,4)
         nv4=nvf(iface,3)
c
c      Question:  can we see the other points? or are they obscured by 
c      this face?  First - which is the opposite face (as on a dice)? 
c
         kface=7-iface
         do nvk=1,4
            nv=nvf(kface,nvk)
            if(visivert(nv,nc)
     *        )then
c                  write(*,103)nv,nc,iface
c
c      Which faces can be drawn?
c 
                    if(.not.visibility(xc(nv ,nc),yc(nv ,nc)
     *                                ,xc(nv1,nc),yc(nv1,nc)
     *                                ,xc(nv2,nc),yc(nv2,nc)
     *                                ,xc(nv3,nc),yc(nv3,nc)
     *                                ,xc(nv4,nc),yc(nv4,nc))
     *                )then
                           visivert(nv,nc)=.false.
                    endif
            endif
         enddo
      enddo
c
      do nv=1,8
         if(.not.visivert(nv,nc)
     *     )then
c               write(*,105)nv,nc
                do nf=1,3
                   visiface(nfv(nv,nf),nc)=.false.
c                  write(*,1051)nfv(nv,nf),nc
                enddo
         endif
      enddo
c
      if(check
     *  )then
             call cubecheck(nc)
      endif
c
      return
c
102   format('VERTEX',i3,' is X=',i5,', Y=',i5,', Z=',i5)
103   format('Testing vertex',i2,' of cube',i3, ' against face',i2)
105   format('Point',i3,' of cube',i3,' is HIDDEN')
1051  format('Face ',i3,' of cube',i3,' is HIDDEN')
      end
