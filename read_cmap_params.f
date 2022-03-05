      subroutine read_cmap_params(colchann)
c
      use bmp_comms
c
      integer(4)colchann
c
c      Reads the basic parameters to describe the colour map
c
      read(colchann,*,err=900,end=901)phired,phigreen,phiblue
      if(colchann.eq.5
     *  )then
             r256=1./256.
             read(colchann,*,err=900,end=901)nred,ngreen,nblue
             ared=  r256*float(nred)
             agreen=r256*float(ngreen)
             ablue= r256*float(nblue)
         else
             read(colchann,*,err=900,end=901)  ared,  agreen,  ablue
      endif
      read(colchann,*,err=900,end=901)  ired,  igreen,  iblue
c
      write(8,*)phired,phigreen,phiblue
      write(8,*)  ared,  agreen,  ablue
      write(8,*)  ired,  igreen,  iblue
c
      pred  =ired  
      pgreen=igreen
      pblue =iblue 
c
      pi=355./113.
c
      phired  =phired  *pi
      phigreen=phigreen*pi
      phiblue =phiblue *pi
c
      return
c
900   continue
      write(0,100)
      return
c
901   continue
      write(0,101)
      return
c
100   format('READ_CMAP_PARAMS: Error reading colour map parameters')
101   format('READ_CMAP_PARAMS: End of file of colour map parameters')
c
      end
