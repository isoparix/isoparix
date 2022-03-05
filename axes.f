      subroutine axes
c
      use isocomm
c
c      Draw a blob on the screen at the origin..
c
c
      zero=0.0
c
      call photospot(zero,zero,zero,x,y,kxc,kyc)
c
      if(check
     *  )then
             write(lchann,100)x,y,ixm,iym,kxc,kyc
             call isoflush(lchann)
      endif
c
      kxc=kxc+(ixm/2)-80
      kyc=kyc+(iym/2)-80
c
      call x11rectfi(%val(kxc-2),%val(kyc-2)
     *              ,%val(kxc+2),%val(kyc+2),%val(4))
c
c      Find location of x,y,z postion vectors length len_vec
c 
      vec_len=30.
c
      xpa=30.0
      ypa=50.0
      zpa=70.0
c
      call photospot(xpa,zero,zero,x,y,kxx,kyx)
      call photospot(zero,ypa,zero,x,y,kxy,kyy)
      call photospot(zero,zero,zpa,x,y,kxz,kyz)
c
      kxx=kxx+(ixm/2)-80
      kxy=kxy+(ixm/2)-80
      kxz=kxz+(ixm/2)-80
c
      kyx=kyx+(iym/2)-80
      kyy=kyy+(iym/2)-80
      kyz=kyz+(iym/2)-80
c
      call x11whiteline(%val(kxc),%val(kyc),%val(kxx),%val(kyx))
      call x11whiteline(%val(kxc),%val(kyc),%val(kxy),%val(kyy))
      call x11whiteline(%val(kxc),%val(kyc),%val(kxz),%val(kyz))
c 
      return
100   format('AXES_1: x=',f12.2,', y=',f12.2,', ixm=',i12,', iym=',i12
     *      ,', kxc=',i12,', kyc=',i12)
      end
