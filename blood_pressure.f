      program blood_pressure
c
c      Reads in date and time sys/dia
c
      implicit real*8 (a-h,o-z)
c
      character (3) amon,txt
      character(8) fillcol,linecol
      character(30)series_name
c
      real*8, dimension(100) :: tod_set,fracday_set,sys_set,dia_set
     *                          ,pulse_set
      real*8,allocatable,dimension (:) :: data_pairs 
c
      nline=0
      fracday_old=-100.
      r60=1./60.
      r24=1./24.
      rday=1./86400.
      write(*,101)
      itya=18
      isecs=0
  1   continue
      read(*,*,end=2) iday,amon,ihrs,mins,nsys,ndia,npulse,txt
      call isotime(iday,amon,itya,ihrs,mins,isecs,itod,ityear,lday)
      tod=float(ihrs)+(r60*float(mins))
      nline=nline+1
      if(nline.eq.1
     *  )then
             ityoff=ityear-itod
      endif
      ityear=ityear-ityoff
      day=rday*float(ityear)
      fracday=float(lday)+(tod*r24)
      write(*,100)nline,iday,amon,tod,fracday,nsys,ndia,npulse
     *           ,nsys-ndia,txt
      if(fracday.lt.fracday_old
     *  )then
c            stop
             fracday=fracday+365.00
             itya=itya+1
      endif
      fracday_old=fracday
c
      tod_set    (nline)=tod
      fracday_set(nline)=fracday
      sys_set    (nline)=nsys
      dia_set    (nline)=ndia
      pulse_set  (nline)=npulse
c
      go to 1
   2  continue
c
      series_name='tmp_BP_by_Date'
      allocate(data_pairs(nline*2))
      data_pairs(1:nline)        =sys_set(1:nline)
      data_pairs(nline+1:2*nline)=dia_set(1:nline)
      write(*,103)
c     write(*,104)data_pairs
      do n=1,nline
         write(*,102)n,tod_set(n),fracday_set(n),sys_set(n)
     *               ,dia_set(n),pulse_set(n)
      enddo
c
c          grf_header(xmin,xmax,ymin,ymax,graph_name)
c          grf_points(0,npstanza,ncoord,points_array
c                    ,fillcol,linecol,nsize,nvisible,series_name)
c
      call grf_header(minval(fracday_set(1:nline))
     *               ,maxval(fracday_set(1:nline))
     *               ,minval(data_pairs)
     *               ,maxval(data_pairs)
     *               ,series_name)
c
c      Set up x-axis by day...
c
      np=0
      do n=1,nline
         np=np+1
         data_pairs(np)=fracday_set(n)
         np=np+1
      enddo
c
c
c      Set up systolic....
c
      np=0
      do n=1,nline
         np=np+2
         data_pairs(np)=sys_set(n)
      enddo
c
      fillcol='Red     '
      linecol='Red     '
      call grf_points(0,1,np,data_pairs,fillcol,linecol,3,1
     *               ,'Systolic')
c
c
c      Set up diastolic....
c
      np=0
      do n=1,nline
         np=np+2
         data_pairs(np)=dia_set(n)
      enddo
c
      fillcol='Blue    '
      linecol='Blue    '
      call grf_points(0,2,np,data_pairs,fillcol,linecol,3,1
     *               ,'Diastolic')
c
      call grf_trailer(2)
c
      stop
c
100   format(2i3,a6,2f8.2,2i10,i6,i8,a4)
101   format('Expects integer day, month as Aug, integer hours,'
     *      ,' minutes, systolic, diastolic, pulse and text'
     *    ,//'Day Month   Hours     Day  Systolic Diastolic Pulse'
     *      ,' Sys-Dia',//)  
102   format(i3,5f8.2)
103   format(/'Graph data:')
104   format(f8.2)
c
      end
