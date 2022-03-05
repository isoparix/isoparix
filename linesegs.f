      subroutine linesegs(idv1,idv2,nf,idcube)
c
      use isocomm
c
      lsc(nf,idcube)=lsc(nf,idcube)+1
      lsa(lsc(nf,idcube),nf,idcube)=idv1
      lsb(lsc(nf,idcube),nf,idcube)=idv2
c
c     write(*,100)idv1,idv2,nf,lsc(nf,idcube),idcube  
      call linecheck(idv1,idv2,nf,idcube)
c
      return
c
100   format('Looking at vertices',2i2,' in face',i2,' count',i2
     *      ,' in cube',i3,4i5)
      end
