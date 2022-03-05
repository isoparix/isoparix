      subroutine summline(summdet,itask)
c
c      Prints out a summary line
c
      use isocomm
c
      dimension summdet(0:511,5)
c
c     summdet:  tstart,tend,tidle,twork,tcpu
c
      tstart=summdet(itask,1)
      tend  =summdet(itask,2)
      tidle =summdet(itask,3)
      twork =summdet(itask,4)
      tcpu  =summdet(itask,5)
c
      telap=tend-tstart
      ubusy=100.*twork/telap
      ucpu =100.*tcpu/telap
c
      itim=tstart
      ihrss=itim/3600
      itim=itim-(ihrss*3600)
      minss=itim/60
      secss=tstart-float(ihrss*3600)-float(minss*60)
c
      itim=tend
      ihrse=itim/3600
      itim=itim-(ihrse*3600)
      minse=itim/60
      secse=tend-float(ihrse*3600)-float(minse*60)
c
      if(itask.ne.taskid.and.itask.ne.artist)then
           write(     *,100)itask,ihrss,minss,secss,ihrse,minse,secse
     *                     ,telap,tidle,twork,tcpu,ubusy,ucpu
           write(lchann,100)itask,ihrss,minss,secss,ihrse,minse,secse
     *                     ,telap,tidle,twork,tcpu,ubusy,ucpu
      else
           write(     *,101)itask,ihrss,minss,secss,ihrse,minse,secse
     *                     ,telap,            tcpu,      ucpu
           write(lchann,101)itask,ihrss,minss,secss,ihrse,minse,secse
     *                     ,telap,            tcpu,      ucpu
      endif
c
      return
c
100   format(i5,2(1x,2(i2.2,':'),f6.3),4f9.3,      2f6.1)
101   format(i5,2(1x,2(i2.2,':'),f6.3), f9.3,f27.3,f12.1)
c
      end
