      integer function andrew_code_index(oc)
c
      character(8)oc,ocb
      character(1)oca(8)
      equivalence(oca,ocb)
c
      ocb=oc
      nxa=ichar(oca(1))-47
      nxb=ichar(oca(2))-47
      if(nxa.gt.10)nxa=nxa-7
      if(nxb.gt.10)nxb=nxb-7
      andrew_code_index=(36*nxa)+nxb
c     write(*,*)oc,nxa,nxb,andrew_code_index
c
      return
      end
