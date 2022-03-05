      program scfr
c
c      Reads filtered output from vmstat and delivers
c      the crucial ratio of Pages Scanned to Pages Freed
c
      integer r,b,avm,fre,re,pi,po,fr,sr
     *          ,cy,in,sy,cs,cpu,cpusys,cpuid,cpuwa
      integer,dimension(20) :: vmstat_data
c
      character (1)  txtb(95)
      character (6)  opsys_name,opname
      character (95) txta
c
      equivalence (txta,txtb)
c
      ndata=0
      opname=opsys_name()
      if(opname.eq.'Linux '
     *  )then
             ndata=16
      endif
c
      if(opname.eq.'AIX   '
     *  )then
             ndata=17
      endif
c
      if(ndata.ne.0
     *  )then
             write(*,108)opname
         else
             write(*,109)opname
             stop
      endif
c  
      nline=-1
  1   continue
c
      read(*,103,end=2)txta
c     write(*,110)txtb(2),txta
        if(
     *     (txtb(2).ne.'0').and.
     *     (txtb(2).ne.'1').and.
     *     (txtb(2).ne.'2').and.
     *     (txtb(2).ne.'3').and.
     *     (txtb(2).ne.'4').and.
     *     (txtb(2).ne.'5').and.
     *     (txtb(2).ne.'6').and.
     *     (txtb(2).ne.'7').and.
     *     (txtb(2).ne.'8').and.
     *     (txtb(2).ne.'9')
     *    )then
             go to 1
      endif
c
      nline=nline+1
      if(mod(nline,20).eq.0
     *  )then
c
             if(opname.eq.'AIX   '
     *         )then
                    write(*,107)opname
             endif
c
             if(opname.eq.'Linux '
     *         )then
                    write(*,1071)opname
             endif
c
      endif
      if(opname.eq.'AIX   '
     *  )then
             read(txta,*)r,b,avm,fre,re,pi,po,fr,sr
     *                 ,cy,in,sy,cs,cpu,cpusys,cpuid,cpuwa
             if(fr.gt.0
     *         )then
                    write(*,102)r,b,avm,fre,re,pi,po,fr,sr
     *                 ,cy,in,sy,cs,cpu,cpusys,cpuid,cpuwa
     *                 ,float(sr)/float(fr)
c                   write(40,104)avm
c                   write(42,104)fre
c                   write(44,104)fr
c                   write(46,104)sc
c                   write(48,105)float(sr)/float(fr)
                else
                    write(*,1021)r,b,avm,fre,re,pi,po,fr,sr
     *                 ,cy,in,sy,cs,cpu,cpusys,cpuid,cpuwa
c                   write(40,104)avm
c                   write(42,104)fre
c                   write(44,104)fr
c                   write(46,104)sc
c                   write(48,106)
             endif
      endif
c
      if(opname.eq.'Linux '
     *  )then
             read(txta,*) (vmstat_data(mx),mx=1,ndata)
             write(*,1022)(vmstat_data(mx),mx=1,ndata)
      endif
      go to 1
c
  2   continue
c
      stop
c
100   format('Pages scanned:',i6,', Pages freed:',i6,', Ratio:',f6.2)
101   format('Pages scanned:',i6,', Pages freed:',i6)
102   format(2i6,2i10,i3,2i4,2i8,i3,3i8,4i3,f7.1)
1021  format(2i6,2i10,i3,2i4,2i8,i3,3i8,4i3,'     - ')
1022  format(3i6,3i10,2i4,4i6,4i4)
103   format(a95)
104   format(i9)
105   format(f9.1)
106   format(' ')
107   format('# ',a6
     *     ,/'#    r     b       avm       fre re  '
     *     , 'pi  po      fr      sr'
     *     , ' cy      in      sy      cs us sy id wa  sr/fr '
     *     ,/'#')
1071  format('# ',a6
     *     ,/'#    r     b  swpd      free      buff     cache'
     *     , '  si  so    bi    bo    in    cs  us  sy  id  wa'
     *     ,/'#')
108   format('Operating system is ',a6)
109   format('Unknown operating system<',a6,'> - exit')
110   format('txtb(2) is:',a1,'.  txta is:',a95)
c
      end
