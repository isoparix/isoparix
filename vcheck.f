      subroutine vcheck(ixa,iza,nprocess)
c
      use isocomm
c
c
c      Checks all vertices in active cubes of this process - see if they
c      align with this one
c
c      LVC(NC)   is the Line_Vertex_Count for cube NC
c      LVX(N,NC) is the Line_Vertex_X_Value for Line_Vertex N in cube NC
c      LVZ(N,NC) is the Line_Vertex_Z_Value for Line_Vertex N in cube NC
c      LVV(N,NC) is the Line_Vertex_Value   for Line_Vertex N in cube NC
c   
      ivd=0
c     write(*,103)nprocess,nactive(nprocess)
c    *               ,(listact(mx,nprocess),mx=1,nactive(nprocess))
c
c      Keep a count of how many and which cubes we touch - and where...
c
      ntouch=0
c
      do npx=1,nactive(nprocess)
         ncb=listact(npx,nprocess)
c
            if(nx1(ncb).eq.ixa)then
                                   if(nz1(ncb).eq.iza)then
c
c      The point (ixa,iza) lies on the point X1=ixa,Z1=iza, 
c      on the bottom left-hand corner of cube NCB
c
                               call vrecord(ntouch,ncb,3,0,0,     1)
                                                          go to 1
                                   endif
c
                                   if(nz2(ncb).eq.iza)then
c
c      The point (ixa,iza) lies on the point X1=ixa,Z2=iza, on the top 
c      left-hand corner of cube NCB
c
                               call vrecord(ntouch,ncb,4,0,0,     3)
                                                          go to 1
                                   endif
c
                                   if(nz2(ncb).gt.iza.and.
     *                                nz1(ncb).lt.iza)then
c
c      The point (ixa,iza) lies on the line X1=ixa, on the left-hand
c      edge of cube NCB
c
                                                  lvc(ncb)=lvc(ncb)+2
                               call vrecord(ntouch,ncb,3,4,iza,lvc(ncb))
                                                          go to 1
                                   endif
            endif
c
            if(nx2(ncb).eq.ixa)then
                                   if(nz1(ncb).eq.iza)then
c
c      The point (ixa,iza) lies on the point X2=ixa,Z1=iza, on the bottom 
c      right-hand corner of cube NCB
c
                               call vrecord(ntouch,ncb,2,0,0,     5)
                                                          go to 1
                                   endif
c
                                   if(nz2(ncb).eq.iza)then
c
c      The point (ixa,iza) lies on the point X2=ixa,Z2=iza, on the top 
c      right-hand corner of cube NCB
c
                               call vrecord(ntouch,ncb,1,0,0,     7)
                                                          go to 1
                                   endif
c
                                   if(nz2(ncb).gt.iza.and.
     *                                nz1(ncb).lt.iza)then
c
c      The point (ixa,iza) lies on the line X2=ixa, on the right-hand
c      edge of cube NCB
c
                                                  lvc(ncb)=lvc(ncb)+2
                               call vrecord(ntouch,ncb,1,2,iza,lvc(ncb))
                                                          go to 1
                                   endif

            endif
c
            if(nx2(ncb).gt.ixa.and.
     *         nx1(ncb).lt.ixa)then
                                   if(nz1(ncb).eq.iza)then
c
c     The point (ixa,iza) lies on the line Z1=iza, on the bottom
c     edge of cube NCB
c
                                                  lvc(ncb)=lvc(ncb)+2
                               call vrecord(ntouch,ncb,2,3,ixa,lvc(ncb))
                                                          go to 1
                                   endif
c
                                   if(nz2(ncb).eq.iza)then
c
c     The point (ixa,iza) lies on the line Z2=iza, on the top
c     edge of cube NCB
c
                                                  lvc(ncb)=lvc(ncb)+2
                               call vrecord(ntouch,ncb,4,1,ixa,lvc(ncb))
                                                          go to 1
                                   endif        
            endif
c
  1         continue
c     write(*,102)ivd(4),ivd(1),ivd(3),ivd(2)
c
      enddo
      nbound=ivd(1)+2*ivd(2)+4*ivd(3)+8*ivd(4)
c
c     write(*,100)ixa,iza,nbound
c
c      Associate the value of NBOUND with all the Line_Vertices touched
c
      do n=1,ntouch
c
         lvv(kvtx(n),kcube(n))=nbound
         if( kvtx(n).gt.8)then
c
                              ivx(kvtx(n)  ,kcube(n))=ixa
                              ivy(kvtx(n)  ,kcube(n))=ivy(1,kcube(n))
                              ivz(kvtx(n)  ,kcube(n))=iza
c
                              ivx(kvtx(n)+1,kcube(n))=ixa
                              ivy(kvtx(n)+1,kcube(n))=0
                              ivz(kvtx(n)+1,kcube(n))=iza
         endif
c
      enddo
c
100   format('Point',2i5,' has vertex value',i3)
101   format(36x,'Mapping line vertex',i2,' in cube',i3)
102   format(/2(2i1,/))
103   format('Vcheck:   Process',i3,', active cubes',i3,':',15i4)
104   format('                       Examining cube:',i4)
      return
      end
