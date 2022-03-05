      subroutine data_gen
c
      use dg
c
      real(4),      dimension(ntestdisks+(2*ntestactive)) :: 
     *                        tempdata,tempdatb,tempdatc,tempdatd
      real(4),      dimension(ntestcores+(2*ntestactive)) :: 
     *                        tempuser,tempsys,tempidle,tempiow
c
      character(12),dimension(ntestdisks+(2*ntestactive)) :: tempdisk
      character(12),dimension(ntestcores+(2*ntestactive)) :: tempcore
c
      parameter (len_out=10000)
c
      character *1 outarray(len_out)
      character (len_out) outrec,outhost,outdisk,outcore
c
      equivalence (outrec,outarray)
c
c      Generates a stream of test data
c
      ax=t_test/10.
      ax=ax-int(ax)
c
      bt=1./256.
      x=t_test
      y=x+.3
      z=x+.6
      a=cos(x)
      b=cos(y)
      c=cos(z)
c
      a2=a*a
      b2=b*b
c
      hostname=nodename(ntestactive)
      cpuser=45.*a2
      cpsys =45.*b2
      cpiow =a2*(100.-cpuser-cpsys)/(a2+b2)
      cpidle=100.-cpuser-cpsys-cpiow
c
      ncore=ntestcores+(2*(ntestactive-1))
c
      ancore=ncore
      total_user=ancore*cpuser
      total_sys =ancore*cpsys
      total_iow =ancore*cpiow
c
      do n=1,ncore
         write(tempcore(n),103)ntestactive,n
c
         if(total_user.gt.100.0
     *     )then
                tempuser(n)=100.0
            else
                tempuser(n)=total_user
         endif
         total_user=total_user-tempuser(n)
c
         if(tempuser(n).lt.100.0
     *     )then
                if(total_sys.gt.100.0-tempuser(n)
     *            )then
                       tempsys(n)=100.-tempuser(n)
                   else
                       tempsys(n)=total_sys
                endif
                total_sys=total_sys-tempsys(n)
        endif
c
        if(tempuser(n)+tempsys(n).lt.100.0
     *    )then
               if(total_iow.gt.100.-tempuser(n)-tempsys(n)
     *           )then
                      tempiow(n)=100.-tempuser(n)-tempsys(n)
                  else
                      tempiow(n)=total_iow
               endif
               total_iow=total_iow-tempiow(n)
         endif
c
         tempidle(n)=100.0-tempuser(n)-tempsys(n)-tempiow(n)
c
      enddo
c
      ndev=ntestdisks+(2*(ntestactive-1))
      do n=1,ndev
         write(tempdisk(n),100)ntestactive,n
         if(ax.gt..4
     *     )then
                tempdata(n)=100.*float(n)/float(ndev)
                tempdatb(n)=100.*float(n)
                tempdatc(n)=200.*float(n)
                tempdatd(n)=(tempdatb(n)+tempdatc(n))*bt
            else
                tempdata(n)=0.
                tempdatb(n)=0.
                tempdatc(n)=0.
                tempdatd(n)=0.
         endif
      enddo
c
c     write(10,105)ntestdisks,ntestcores,ntestactive,hostname,ndev,ncore
c     call isoflush(10)
c
      if(mod(lastcol,20).eq.0
     *  )then
                tempdata(1)=100.
                tempdatb(1)=999.
                tempdatc(1)=111.
                tempdatd(1)=22.2
      endif
c
c      Write the synthetic data, via outrec and remblanks
c
      write(outhost,1021)hostname,cpuser,cpsys,cpiow,cpidle
      write(outdisk,1022)ndev
     *        ,(tempdisk(mx),tempdata(mx),tempdatb(mx),tempdatc(mx)
     *        , tempdatd(mx),mx=1,ndev)
      write(outcore,1022)ncore
     *        ,(tempcore(mx),tempuser(mx),tempsys(mx),tempiow(mx)
     *        , tempidle(mx),mx=1,ncore)
c
      outrec=trim(outhost)//trim(outdisk)//trim(outcore)
      nchars=len(trim(outrec))
      call remblanks(outarray,nchars)
c
      return
c
100   format('hdisk',i3.3,'_',i3.3)
101   format('Hostname ',a)
1021  format(a,4f6.1)
1022  format(i4,500(' ',a,f6.1,3f9.1))
103   format('core',i3.3,'_',i3.3)
104   format(/)
105   format(3i6,' ',a,2i6)
c
      end
