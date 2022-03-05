      subroutine drawcube(idcube)
c
c      Draws cuboid from 8 xy pairs
c
      use isocomm
c
      integer, dimension (6) :: list_of_faces
c
      character *3 label
c
      list_of_faces=0
      ntotface=0
      do nf=1,6
         if(visiface(nf,idcube)
     *     )then
                call drawface(idcube,nf,facecol(idcube),3)
                ntotface=ntotface+1
                list_of_faces(ntotface)=nf
c               write(32,100 )nf,idcube,ntotface
            else
c               write(32,1001)nf,idcube,ntotface
         endif
      enddo 
c
      if(check)write(lchann,103)
      call photospot(float(ivx(nearvert(idcube),idcube))
     *              ,float(ivy(nearvert(idcube),idcube))
     *              ,float(ivz(nearvert(idcube),idcube))
     *              ,tx,ty,kxc,kyc)
c
c      Stick a label on the 'nearest vertex'
c
c     write(label,102)nearvert(idcube),idcube
c     call x11text(%val(kxc),%val(kyc)
c    *            ,%ref(trim(adjustl(label)))
c    *            ,%val(0),%val(len_trim(label)))
c
c     write(32,101)idcube,ntotface,(list_of_faces(mx),mx=1,ntotface)
c     call flush(32)
      return
100   format('          Drawn face',i3,' cube',i3
     *      ,' (Total face count',i4,')')
1001  format('      Not drawn face',i3,' cube',i3
     *      ,' (Total face count',i4,')')
101   format('DRAWCUBE: Cube ',i3.0,', visible faces ',i2,' ==>',6i2)
102   format(i1,1x,i1)
103   format('DRAWCUBE calling PHOTOSPOT....')
      end
