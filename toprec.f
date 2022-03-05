      subroutine toprec(source_id,single_line_output)
c
c        Produces a single useful line from top readings
c
      use allcomms
c
      parameter (len_out=88)
c
      logical dn,single_line_output
c
        character (60)  source_id,srcid_in,srcid_out
        character (1)  srcid(60),outarray(len_out)
        character  (len_out) outrec 
        equivalence (srcid_in,srcid),(outrec,outarray)
c
c#####################################################################
c
      end_data=.true.
c
c      Clear domain part of source_id
c
      srcid_in=source_id
      dn=.false.
      do i=1,60
         if(srcid(i).eq.'.')dn=.true.
         if(dn)srcid(i)=' '
      enddo
c
      call read_top_rec
      ndev=0
      srcid_out=trim(srcid_in)//'_'//trim(adjustl(top_aspect))
c
c****************************************
c
c       All data read for this instance of top
c
      if(.not.single_line_output)return
c
c      ...we're running live, so write out a useful line
c
             nchars=88
c
c      (see format 102...)
c
      if(top_type.eq.'CPU'
     *  )then
c     
             ncpu=ncpu+1
             cpuser_s_all=cpuser_s_all+cpuser_s
             cpsys_s_all =cpsys_s_all +cpsys_s
             cpidle_s_all=cpidle_s_all+cpidle_s
             cpiow_s_all =cpiow_s_all +cpiow_s
c
             write(outrec,102)srcid_out,cpuser_s
     *                        ,cpsys_s,cpidle_s,cpiow_s,ndev
             call remblanks(outarray,nchars)
c
      endif
c
      if(top_type.eq.'SWP'
     *  )then
c     
             cpuser_s_all=cpuser_s_all/float(ncpu)
             cpsys_s_all =cpsys_s_all /float(ncpu)
             cpidle_s_all=cpidle_s_all/float(ncpu)
             cpiow_s_all =cpiow_s_all /float(ncpu)
c
             srcid_out='Average_cpu_'//trim(srcid_in)
             write(outrec,102)srcid_out,cpuser_s_all
     *                 ,cpsys_s_all,cpidle_s_all,cpiow_s_all,ndev
             call remblanks(outarray,nchars)
c
             ncpu=0
             cpuser_s_all=0.
             cpsys_s_all =0.
             cpiow_s_all =0.
             cpidle_s_all=0.
c
      endif
c
      return
c
102   format(a60,4f6.1,i4)
c
        end
