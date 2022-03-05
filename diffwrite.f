      subroutine diffwrite(datin,ll,nlast,ndc,ntot,ncout)
c
c      Handles output strings of any length
c
      use bmp_comms
c
      character (1) datin(ll)
c      
      ntot=0
      ngoal=ndc+1
c
      if(ndc.le.1
     *  )then
             do i=nlast-ndc,nlast
c               write(*,100)1,datin(i)
c
                ncout=ncout+1
                tmparray(ncout)=char(1)
                ncout=ncout+1
                tmparray(ncout)=datin(i)
c
                ntot=ntot+1
             enddo
         else
             ma=nlast-ndc
             nbuckets=1+(ndc/254)
             ndc=ndc+1
             ndifbuckets=ndifbuckets+nbuckets
c
             do n=nbuckets,1,-1
                   nchars=ndc/n
                   mb=ma+nchars-1
c
c                  if(mod(nchars,2).eq.0
c    *               )then
c                        write(*,107)0,nchars,(datin(mx),mx=ma,mb)
c                     else
c                         write(*,107)0,nchars,(datin(mx),mx=ma,mb),'Q'
c                  endif
c
                   ncout=ncout+1
                   tmparray(ncout)=char(0)
                   ncout=ncout+1
                   tmparray(ncout)=char(nchars)
                   do mx=ma,mb
                      ncout=ncout+1
                      tmparray(ncout)=datin(mx)
                   enddo
                   if(mod(nchars,2).ne.0
     *               )then
                          ncout=ncout+1
                          tmparray(ncout)=char(0)
                   endif
c
                   ntot=ntot+nchars
                   ndc=ndc-nchars
                   ma=mb+1
             enddo
c
      endif
c
      ndc=0
      if(ntot.ne.ngoal
     *  )then
c
c      DISASTER!!!
c
             write(0,101)ntot,ngoal
             stop
c        else
c           write(8,102)ntot,nbuckets
      endif
c
      return
c
100   format(i8,' ',a1)
101   format(/'#########################################'
     *      ,/'#         ERROR IN DIFFWRITE            #',2i6
     *      ,/'#########################################')
102   format('DIFFWRITE: Wrote',i8,'    characters    in',i6,' buckets')
107   format(2i4,' ',512a1)
      end
