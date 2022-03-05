      subroutine samewrite(ac,nsc,ntot,ncout)
c
c      Handles output strings of any length
c
      use bmp_comms
c
      character (1) ac
c      
      nsc=nsc+1
      ntot=0
      ngoal=nsc
      nbuckets=1+(nsc/254)
      nsambuckets=nsambuckets+nbuckets
c
      do n=nbuckets,1,-1
         nchars=nsc/n
c        write(*,100)nchars,ac
         ncout=ncout+1
         tmparray(ncout)=char(nchars)
         ncout=ncout+1
         tmparray(ncout)=ac
c
         nsc=nsc-nchars
         ntot=ntot+nchars
      enddo
c
      nsc=0
      if(ntot.ne.ngoal
     *  )then
c
c      DISASTER!!!
c
             write(0,101)ntot,ngoal
c        else
c            write(8,102)ntot,ichar(ac),nbuckets
      endif
c
      return
c
100   format(i8,' ',a1)
101   format(/'#########################################'
     *      ,/'#         ERROR IN SAMEWRITE            #',2i6
     *      ,/'#########################################')
102   format('SAMEWRITE: Wrote',i8,' of character',z4,' in'
     *       ,i6,' buckets')
c
      return
      end
