      program offprep
c      
c      Takes array and converts it, culling if necessary, to OFF
c      format for JavaView
c      
      use isocomm
c
           real(4),allocatable,dimension(:)  :: xvertex,yvertex,zvertex
      character(1),allocatable,dimension(:)  :: cvertex
        integer(4),allocatable,dimension(:,:)  :: face
c
      character(1)rgb(0:1023)
c      
      open(62,file='0001.off',status='unknown'
     *    ,form='unformatted')
      read(62)ixm,izm,limit
      write(64,*)ixm,izm,limit
c      
      if(allocated(paint))deallocate(paint)
      allocate(paint(0:limit+1),stat=ierror)
c      
      if(allocated(mset))deallocate(mset)
      allocate(mset(0:ixm+1,0:izm+1),stat=ierror)
c      
      read(*,*)longside   ! Longest side for 3D mesh
      k=max0(ixm,izm)
      nsep=1+(k/longside)
      jxm=ixm/nsep
      jzm=izm/nsep
      nfaces=2*(((jxm+1)*(jzm+1))+((jzm+1)*jxm)+((jxm+1)*jzm)
     *      +(2*(jxm+1))+(2*(jzm+1)))
      nvertices=2*((4*(jxm+1)*(jzm+1))+(2*(jxm+2))+(2*(jzm+2)))
      write(*,100)ixm,izm,jxm,jzm,nvertices,nfaces
c      
      if(allocated(xvertex))deallocate(xvertex)
      if(allocated(yvertex))deallocate(yvertex)
      if(allocated(zvertex))deallocate(zvertex)
      if(allocated(cvertex))deallocate(zvertex)
      if(allocated   (face))deallocate(face)
c      
      allocate(xvertex(nvertices),stat=ierror)
      allocate(yvertex(nvertices),stat=ierror)
      allocate(zvertex(nvertices),stat=ierror)
      allocate(cvertex(nvertices),stat=ierror)
      allocate(face   (5,nfaces) ,stat=ierror)
c      
      read(62)rgb
      read(62)paint
      read(62)mset
      close(62)
c
      nf=0
      nvertex=0
      yvertex=0.0
      bigvalue=0
      smavalue=1000000
      cvertex=''
      jzq=izm/nsep
      do iz=1,izm,nsep
         iza=iz+nsep
         jz=iz/nsep
         do ix=1,ixm,nsep
            nf=nf+1
            ixa=ix+nsep
            jx=ix/nsep
            face(5,nf)=ichar(paint(mset(ix,iz)))
            value=log(float(int(mset(ix,iz))))
            if(value.gt.bigvalue)bigvalue=value
            if(value.lt.smavalue)smavalue=value
c
            face(1,nf)=nvertex
            nvertex=nvertex+1
            xvertex(nvertex)=float(ix)
            yvertex(nvertex)=value
            zvertex(nvertex)=float(iz)
c
            if(longside.lt.10
     *        )then
                   write(*,101)nvertex,nf,jx,jz,ix,iz,ixa,iza
     *                ,xvertex(nvertex),yvertex(nvertex)
     *                ,zvertex(nvertex)
            endif
c
            face(2,nf)=nvertex
            nvertex=nvertex+1
            xvertex(nvertex)=float(ixa)
            yvertex(nvertex)=value
            zvertex(nvertex)=float(iz)
c
            if(longside.lt.10
     *        )then
                   write(*,101)nvertex,nf,jx,jz,ix,iz,ixa,iza
     *                ,xvertex(nvertex),yvertex(nvertex)
     *                ,zvertex(nvertex)
            endif
c
            face(3,nf)=nvertex
            nvertex=nvertex+1
            xvertex(nvertex)=float(ixa)
            yvertex(nvertex)=value
            zvertex(nvertex)=float(iza)
c
            if(longside.lt.10
     *        )then
                   write(*,101)nvertex,nf,jx,jz,ix,iz,ixa,iza
     *                ,xvertex(nvertex),yvertex(nvertex)
     *                ,zvertex(nvertex)
            endif
c
            face(4,nf)=nvertex
            nvertex=nvertex+1
            xvertex(nvertex)=float(ix)
            yvertex(nvertex)=value
            zvertex(nvertex)=float(iza)
c
            if(longside.lt.10
     *        )then
                   write(*,101)nvertex,nf,jx,jz,ix,iz,ixa,iza
     *                ,xvertex(nvertex),yvertex(nvertex)
     *                ,zvertex(nvertex)
            endif
c
         enddo
      enddo
c
c      Mormalise vertical scale
c
      write(*,*)bigvalue,smavalue
      yvertex=yvertex-smavalue
      ratio=float(ixm)/(bigvalue-smavalue)
      yvertex=ratio*yvertex
c
c      Set up perimeter base vertices and faces
c
      finaliz=iza
      nfp=0
      do ix=1,ixm,nsep
         ixa=ix+nsep
         nf=nf+1
         nfp=nfp+1
c
         face(1,nf)=nvertex
         nvertex=nvertex+1
         xvertex(nvertex)=float(ix)
         yvertex(nvertex)=0.0
         zvertex(nvertex)=0.0
c
         face(2,nf)=nvertex
         nvertex=nvertex+1
         xvertex(nvertex)=float(ixa)
         yvertex(nvertex)=0.0
         zvertex(nvertex)=0.0
c
         face(3,nf)=face(2,nfp)
         face(4,nf)=face(1,nfp)
         face(5,nf)=face(5,nfp)
c
         nf=nf+1
 
         face(4,nf)=nvertex
         nvertex=nvertex+1
         xvertex(nvertex)=float(ix)
         yvertex(nvertex)=0.0
         zvertex(nvertex)=finaliz
c
         face(3,nf)=nvertex
         nvertex=nvertex+1
         xvertex(nvertex)=float(ixa)
         yvertex(nvertex)=0.0
         zvertex(nvertex)=finaliz
c
         face(1,nf)=face(4,((jxm+1)*jzm)+nfp)
         face(2,nf)=face(3,((jxm+1)*jzm)+nfp)
         face(5,nf)=face(5,((jxm+1)*jzm)+nfp)
c
      enddo
c
c      Vertical adjacent derived faces
c
      do jz=1,jzm+1
         nfa=(jxm+1)*(jz-1)
         do jx=1,jxm
            nf=nf+1
            face(1,nf)=face(2,jx+nfa)
            face(2,nf)=face(1,jx+nfa+1)
            face(3,nf)=face(4,jx+nfa+1)
            face(4,nf)=face(3,jx+nfa)
            if(yvertex(1+face(1,jx+nfa)).gt.
     *         yvertex(1+face(1,jx+nfa+1))
     *        )then
                   face(5,nf)=face(5,jx+nfa)
               else
                   face(5,nf)=face(5,jx+nfa+1)
            endif
            if(longside.lt.10
     *        )then
                   write(*,106)nf,nfa,jx,jz,(face(mx,nf),mx=1,5)
            endif
         enddo
      enddo
c
c      Horizontal adjacent derived faces
c
      do jx=1,jxm+1
         nfa=(jx-1)*jzm
         do jz=1,jzm
            nf=nf+1
            face(1,nf)=face(3,jz+nfa)
            face(2,nf)=face(2,jz+nfa+jxm+1)
            face(3,nf)=face(1,jz+nfa+jxm+1)
            face(4,nf)=face(4,jz+nfa)
            if(yvertex(1+face(1,jz+nfa)).gt.
     *         yvertex(1+face(1,jz+nfa+jxm+1))
     *        )then
                   face(5,nf)=face(5,jz+nfa)
               else
                   face(5,nf)=face(5,jz+nfa+jxm+1)
            endif
            if(longside.lt.10
     *        )then
                   write(*,106)nf,nfa,jx,jz,(face(mx,nf),mx=1,5)
            endif
         enddo
      enddo
      nfaces=nf
      nvertices=nvertex
c
      write(*,100)ixm,izm,jxm,jzm,nvertices,nfaces
      open(4,file='out.off',form='formatted',status='unknown')
      write(4,102)nvertices,nfaces,0
c
      do nv=1,nvertices
         write(4,103)xvertex(nv),yvertex(nv),zvertex(nv)
      enddo
c
      write(4,104)(face(mx,1),mx=1,4),255,0,0
      do nf=2,nfaces
         write(4,104)(face(mx,nf),mx=1,4)
     *      ,ichar(rgb(4*face(5,nf):2+(4*face(5,nf))))
      enddo
      close(4)
c
      stop
c
100   format('Original matrix:',2i6,', New matrix:',2i6
     *      ,' Vertices:',i6,', Faces',i6)
101   format('Vertex:',i3,' Face:',i3,5i6,i3,3f10.3,i6)
102   format('OFF',/3i6)
103   format(3f8.1)
104   format('4',7i5)
105   format(256(/4i5))
106   format(20i6)
c      
      end
c      
