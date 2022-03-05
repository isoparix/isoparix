      subroutine statout
c
c      displays status of this node job
c
       use isocomm
c
      real (8) t
c
      call tim(t)
      itim=t
      ihrs=itim/3600
      itim=itim-(ihrs*3600)
      mins=itim/60
      secs=t-float(ihrs*3600)-float(mins*60)
c
      write(statout_hdr,101)ihrs,mins,secs,role,taskid
c     if(statout_hdr.ne.statout_hdr_old
c    *  )then
c
c      Write a helpful separating blank?
c
c            write(lchann,100)
c            statout_hdr_old=statout_hdr
c     endif
c
      write(lchann,102)ihrs,mins,secs,role,taskid,txtout
      call isoflush(lchann)
c
      return
c
c      25 spaces of header, then immediate txtout...
c
100   format(/)
101   format(i2,':',i2.2,':',f7.4,' ',a7,i3,' ')
102   format(i2,':',i2.2,':',f7.4,' ',a7,i3,' ',a55)
c
      end
