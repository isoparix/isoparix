      subroutine hidrem
c
c      Who covers what? If cmasks is true, then the second 
c      element of cmasks is covered by the first.
c
      use isocomm
c
      logical visibility,tvis,hider,quickwin
c
      character (1) fx
      character (15) covres
      dimension listtmp(20)
      real(4),dimension(5) :: xa,ya,xb,yb,cepta,slopea
      real(4)ceptb,slopeb
      integer(4),dimension(5) :: nq
c
      logical clash,q_inter,q_visi,intersector
      logical,dimension(maxcubes)          :: unresolved_cube
      logical,dimension(maxcubes,maxcubes) :: unresolved_pair
c
c******************************************************************
c
c     write(*,114)
c
      nxclude=0
      nvis=0
      ncep=0
      nrc=0
      unresolved_pair=.true.
c
      do nla=1,nlong-1
         nca=longact(nla)
         xmaxa=x_max(nca)
         ymaxa=y_max(nca)
         xmina=x_min(nca)
         ymina=y_min(nca)
         unresolved_cube(nca)=.false.
         do nlb=nla+1,nlong
            ncb=longact(nlb)
c
c      Set only the relevant entries...
c
                   cmasks(ncb,nca)=.false.
                   cmasks(nca,ncb)=.false.
c
c     Can we eliminate interference simply?
c
                   if(x_min(ncb).gt.xmaxa.or.
     *                y_min(ncb).gt.ymaxa.or.
     *                x_max(ncb).lt.xmina.or.
     *                y_max(ncb).lt.ymina
     *               )then
                          nxclude=nxclude+1
                          unresolved_pair(nca,ncb)=.false.
                          unresolved_pair(ncb,nca)=.false.
                      else
                          unresolved_cube(nca)=.true.
                   endif
         enddo
         if(unresolved_cube(nca))nrc=nrc+1
      enddo
c
c      Test each parallel face of NCB against each visible face of NCA
c
      do nla=1,nlong
         nca=longact(nla)
         if(unresolved_cube(nca)
     *     )then
         do nf=1,6
            if(visiface(nf,nca)
     *        )then
                   nclash=0
c
c      We can see this face of cube NCA....
c
                   nq(1)=nvf(nf,1)
                   nq(2)=nvf(nf,2)
                   nq(3)=nvf(nf,4)	! Note order for convex
                   nq(4)=nvf(nf,3)
                   nq(5)=nq(1)
c
                   do i=1,5
c                     write(0,801)i,nca,xc(nq(i),nca),yc(nq(i),nca)
                      xa(i)=xc(nq(i),nca)
                      ya(i)=yc(nq(i),nca)
                   enddo
c
                   do i=1,4
c                      write(0,800)i,xa(i),ya(i),xa(i+1),ya(i+1)
                       cepta(i)= cept(xa(i),ya(i),xa(i+1),ya(i+1))
                      slopea(i)=slope(xa(i),ya(i),xa(i+1),ya(i+1))
                   enddo
c
                   xf=ivx(nq(1),nca)	! We only use correct
                   yf=ivy(nq(1),nca)	! one of these 
                   zf=ivz(nq(1),nca)	! later on....
c
                   do nlb=1,nlong
                      ncb=longact(nlb)
c
                      hider=.false.
                      if(ncb.ne.nca              .and.
     *                   unresolved_pair(nca,ncb).and.
     *                   .not.cmasks(nca,ncb)    .and.
     *                   .not.cmasks(ncb,nca)
     *                  )then
                             clash=.false.
                             nfx=nf
                             do m=1,2
c
                                q_inter=.false.
                                q_visi =.false.
c
                                nfx=7-nfx
c
c
                                nq(1)=nvf(nfx,1)
                                nq(2)=nvf(nfx,2)
                                nq(3)=nvf(nfx,4)! Note order for convex
                                nq(4)=nvf(nfx,3)
                                nq(5)=nq(1)
c
                                do i=1,5
                                   xb(i)=xc(nq(i),ncb)
                                   yb(i)=yc(nq(i),ncb)
                                enddo
c
                                xt=ivx(nq(1),ncb)
                                yt=ivy(nq(1),ncb)
                                zt=ivz(nq(1),ncb)
c
                                do n=1,4
                                   slopeb=slope(xb(n),  yb(n)
     *                                         ,xb(n+1),yb(n+1))
                                    ceptb= cept(xb(n),  yb(n)
     *                                         ,xb(n+1),yb(n+1))
                                   do k=1,4
c     write(0,802)n,k,slopeb,ceptb,slopea(k),cepta(k),xb(n),yb(n)
c    *                                              ,xb(n+1),yb(n+1c
c    *                                              ,ya(k),ya(k+1)
                                      if(intersector(slopeb,   ceptb
     *                                              ,slopea(k),cepta(k)
     *                                              ,yb(n),yb(n+1)
     *                                              ,ya(k),ya(k+1))
     *                                  )then
                                             q_inter=.true.
                                             ncep=ncep+1
                                             clash=.true.
                                             exit
                                      endif	! intersector
                                   enddo	! k
                                   if(clash)exit
                                enddo		! n
c
                                do n=1,4
                                    if(.not.visibility(xb(n),yb(n)
     *                                    ,xa(1),ya(1),xa(2),ya(2)
     *                                    ,xa(3),ya(3),xa(4),ya(4))
     *                                )then
                                           q_visi =.true.
                                           nvis=nvis+1
                                           clash=.true.
                                           exit
                                    endif	! visibility
                                enddo		! n
c
c                               if(.not.q_inter.and.q_visi
c    *                            )then
c                                      call x11clearpixmap()
c                                      call drawface(nca, nf,20,1)
c                                      call drawface(ncb,nfx,40,1)
c                                      write(*,120)nf,nca,nfx,ncb
c                                      call x11flush()
c                                      call system('sleep 1')
c                               endif
c                               write(*,124)q_inter,q_visi,clash
c
                                if(clash
     *                            )then
c
c      ...this 'test' vertex clashes with this face....
c
                                       nclash=nclash+1
                                       if(nf.eq.1)t=(xf-xe)*(xf-xt)
                                       if(nf.eq.2)t=(yf-ye)*(yf-yt)
                                       if(nf.eq.3)t=(zf-ze)*(zf-zt)
                                       if(nf.eq.4)t=(zf-ze)*(zf-zt)
                                       if(nf.eq.5)t=(yf-ye)*(yf-yt)
                                       if(nf.eq.6)t=(xf-xe)*(xf-xt)
c
c                  write(*,116)nfx,ncb,nf,nca,t,nclash
                                       if(t.lt.0.
     *                                   )then
                                              cmasks(nca,ncb)=.true.
                                              hider=.true.
                                       endif
c
                                       if(t.gt.0.
     *                                   )then
                                              cmasks(ncb,nca)=.true.
                                              hider=.true.
                                       endif
                                       exit
                                 endif	! clash
  1                              continue
                              enddo		! m
                      endif	! ncb.ne.nca
                   enddo			! ncb
c                  call x11flush()
c                  read(*,*)junk
            endif	! visiface
         enddo   !nf
         endif
      enddo ! nca
c
c     write(*,121)nxclude,nvis,ncep,nxclude+nvis+ncep,nrc
c
c **************** Check for errors ***********************************
c
c     write(32,1141)
c     nlist=0
c     do nla=1,nlong
c        j=longact(nla)
c        nlist=nlist+1
c        listtmp(nlist)=longact(nla)
c     enddo
c
c     do nla=1,nlong
c        j=listtmp(nla)
c        do nlb=nla+1,nlong
c           i=listtmp(nlb)
c           if(.not.cmasks(i,j).and.
c    *         .not.cmasks(j,i)
c    *        )then
c                  write(32,119)i,j,ncubes
c           endif
c        enddo
c     enddo
c     call flush(32)
c
      return
c
800   format('HIDREM call to CEPT:',i10,4e14.5)                   
801   format(2i6,2e14.7)                      
802   format('Pre-INTERSECTOR:    ',2i5,10e14.5)                   
100   format(/'There are',i5,' active cubes in the list'
     *      /,'   Entry    Cube  Process',100(/3i8))
101   format('Next nearest point in the triple is',i3)
102   format('DMIN= ',e12.7,', DMAX= ',e12.7)
103   format('Triple at',i3 
     *      ,': Vertex',i3,' hidden - total=',i3)
104   format('Vertex:',i3,' (',4i6,')')
105   format('ERROR IN HIDREM: Cubes',i4,' and',i4
     *      ,' of',i4,' hide each other')
106   format('Dimension',a2,': Cube',i3,' vertex',i2,' at',i5
     *      ,a15,' cube'
     *      ,i3,' face(',4i2,') at',i5,':',6i5)
107   format('Cube',i3,' covers',$)
108   format(i5,$)
1081  format('NS=',i8,', NSTEPS=',i8,', Percent',f6.1,', Hider:',L2)
109   format(' nothing.')
110   format('.')
112   format(' Testing vertex',i2,' of cube',i2
     *      ,' against Z=',f7.2,' of cube',i2)
1121  format(' Testing vertex',i2,' of cube',i2
     *      ,' against X=',f7.2,' of cube',i2)
113   format('Face',i2,' is parallel to the ',a1,'-axis')
114   format('HIDREM')
1141  format('HIDREM: Undefined relationship check')
115   format('Face',i2,' of cube',i3)
116   format(' Clash of face',i2,' of cube',i3
     *      ,' against face',i2,' of cube',i3
     *      ,'. Sign',e12.5,' - clashes',i5)
117   format(/'Vertex',i2,' of cube',i3
     *      ,' against face',i2,' of cube',i3)
118   format('Cube',i3,' covers cube',i3,' delta_max/min',2f8.2)
119   format('Undefined relationship between cubes',i4,' and',i4
     *      ,' of',i4)
120   format(' ***** Testing face',i2,' of cube',i3
     *             ,' against face',i2,' of cube',i3,' *****')
121   format('HIDREM: NXCLUDE=',i4,', NVIS=',i4,',NCEP=',i4
     *      ,', TOTAL=',3i5)
122   format('        Possible interaction:',2i3)
123   format(2e20.12)
124   format('Intersection?',l2,'.   Visibility?',l2,'.    Clash?',l2)
125   format(8f12.6)
c
      end
