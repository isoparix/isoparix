      subroutine tripleset
c
c      1. Sets triples of other vertices associated with one vertex
c      2. Sets triples of faces          associated with one vertex
c
      use isocomm
c
      dimension nopp(8)
c
      open(32,file='newcube.history',status='unknown',form='formatted')
c
c      NFACE is NFACE(VERTEX,FACE)
c
      do nf=1,6
         do nv=1,8
            nface(nv,nf)=0
         enddo
      enddo
c
      faceaxis(1)='X'
      faceaxis(2)='Y'
      faceaxis(3)='Z'
      faceaxis(4)='Z'
      faceaxis(5)='Y'
      faceaxis(6)='X'
c
      nface(1,1)=1
      nface(1,2)=1
      nface(1,4)=1
c
      nface(3,1)=1
      nface(3,2)=1
      nface(3,3)=1
c
      nface(2,1)=1
      nface(2,4)=1
      nface(2,5)=1
c
      nface(4,1)=1
      nface(4,3)=1
      nface(4,5)=1
c
      nface(5,2)=1
      nface(5,4)=1
      nface(5,6)=1
c
      nface(7,2)=1
      nface(7,3)=1
      nface(7,6)=1
c
      nface(6,4)=1
      nface(6,5)=1
      nface(6,6)=1
c
      nface(8,3)=1
      nface(8,5)=1
      nface(8,6)=1
c
      write(8,101)
      do nv=1,8
         ia=0
         do nf=1,6
            if(nface(nv,nf).eq.1)then
                                     ia=ia+1
                                     nfv(nv,ia)=nf
            endif
         enddo
      write(8,103)nv,(nfv(nv,mx),mx=1,3)
      enddo
c
      write(8,102)
      do nf=1,6
         ia=0
         do nv=1,8
            if(nface(nv,nf).eq.1
     *        )then
                   ia=ia+1
                   nvf(nf,ia)=nv
            endif
         enddo
         write(8,104)nf,(nvf(nf,mx),mx=1,4)
      enddo
c
c      Find opposite vertices...
c
c
      nopp=0
      do nf=1,6
                         nopp(nvf(nf,1)) =nopp(nvf(nf,1))+1
         nvopp(nvf(nf,1),nopp(nvf(nf,1)))=nvf(nf,4)
c
                         nopp(nvf(nf,4)) =nopp(nvf(nf,4))+1
         nvopp(nvf(nf,4),nopp(nvf(nf,4)))=nvf(nf,1)
c
                         nopp(nvf(nf,2)) =nopp(nvf(nf,2))+1
         nvopp(nvf(nf,2),nopp(nvf(nf,2)))=nvf(nf,3)
c
                         nopp(nvf(nf,3)) =nopp(nvf(nf,3))+1
         nvopp(nvf(nf,3),nopp(nvf(nf,3)))=nvf(nf,2)
c
      enddo
c
      write(8,105)
      do nv=1,8
         write(8,103)nv,(nvopp(nv,mx),mx=1,3)
      enddo
c
c      Generate binary numbers
c
      n=0
      do l=0,1   
         do k=0,1   
            do j=0,1   
               do i=0,1   
                  number(1,n)=i
                  number(2,n)=j
                  number(3,n)=k
                  number(4,n)=l
                  n=n+1
               enddo
            enddo
         enddo
      enddo
c
      call isoflush(8)
      return
c
100   format(12i3)
101   format(/'NFV: ROWS=VERTICES, COLUMNS=FACES'/)
102   format(/'NVF: ROWS=FACES, COLUMNS=VERTICES'/)
103   format('              Vertex',i2,':',12i3)
104   format('                Face',i2,':',12i3)
105   format(/'NVOPP: OPPOSITE VERTICES',/)
c
      end
