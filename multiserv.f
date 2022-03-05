      program multiserv
c
c      Generates data for series of Qs where
c
c      Customer is either:
c             i) in the system (waiting or being served)
c            ii) outside the system, and arriving
c
c      Interval from time he leaves the system until he returns
c      once again is exponentially distributed with mean 1/lamda
c
c      Ref: Kleinrock, L Queueing systems, vol 1, pp108-9
c      Ref: Kleinrock, L Queueing systems, vol 2, pp12,208 
c      Wiley & Sons 1976
c
      use qcomm
c
      implicit real(8) (a-h,o-z)
c
      parameter (n_servers=20)
      dimension ncp(n_servers),st(n_servers)
c
      open(2,file='txnwin.prn',status='unknown')
      open(3,file='multiserv.list',status='old')
      nlines=25
c
c     a=projector(1.d0,3.d0,3.d0,7.d0,11.d0)
c
      ntxmax=30
      nstotal=0
      sttotal=0
      tpsmax=100000000.
  1   continue
      nstotal=nstotal+1
      read(3,*,end=2)ncp(nstotal),st(nstotal)
      sttotal=sttotal+st(nstotal)
      tps=ncp(nstotal)/st(nstotal)
      if(tps.lt.tpsmax
     *  )then
             tpsmax=tps
      endif
c
      write(*,100)nstotal,ncp(nstotal),st(nstotal),tps
      if(nstotal.lt.n_servers
     *  )then
             go to 1
         else
             write(*,101)n_servers
             go to 2
      endif
  2   continue
      nstotal=nstotal-1
c
  3   continue
      write(*,103)tpsmax
      deltps=tpsmax*.98d0/dfloat(ntxmax)
c     read(*,*)txps
c     if(txps.gt.tpsmax
c    *  )then
c            write(*,105)
c            go to 3
c     endif
c
c     if(txps.le.0.
c    *  )then
c            stop
c     endif
c
      do ntx=1,ntxmax
         txps=dfloat(ntx)*deltps
         write(2,112)
         total_resp_time=0.
         do id_serv=1,nstotal
            ncpu=ncp(id_serv)
            serv_time=st(id_serv)
            call txneval(txps)
            total_resp_time=total_resp_time+ws
         enddo
         write(*,102)sttotal,txps,total_resp_time
     *              ,total_resp_time/sttotal
         write(2,102)sttotal,txps,total_resp_time
     *              ,total_resp_time/sttotal
      enddo
c
100   format('Server',i3,' with',i3,' CPUs and a service time of',f8.4
     *      ,' seconds can handle',f8.2,' transactions/sec')
102   format('Total service time/txn:',f8.4,' seconds.'
     *       ,'  Overall response time at',f6.2,' txns'
     *      ,'/sec is',f12.4,' seconds (normalised',f12.4,')')
101   format('Only room for',i4,' servers...')
103   format(/'Enter the number of transactions per second'
     * ,      ' (max is',f6.2,', zero to end program)')
104   format(/'Enter the number of servers (eg number of CPUs)'
     *,/      'and the service time of one txn on one server/CPU')
105   format('***** ERROR: System cannot sustain that transaction rate'
     *     ,/'             Add more servers (eg CPUs), reduce'
     *     , ' transaction cost'
     *     ,/'             or accept lower transaction rate'
     *     ,/)     
112   format(
     * //' Srv CPU     Service         Avg wait in       '
     * , 'Avg utiln    TXNs per  Normalised'
     * ,/'  ID',12x,'time',6x,'system',7x,'queue'
     *  ,'  one server      second   resp time'
     *  ,/)
      end
c
c
c
      subroutine txneval(txps)
c
c      mcust=finite number of customers
c      ncpu=number of servers (eg CPUs)
c      invrho is the inverse of rho.   Used if no value of rho available.
c      rho is the utilisation imposed on the server by a single stream
c      tsiat is product of service time and interarrival rate
c
c      Set defaults
c
      use qcomm
c
      implicit real(8) (a-h,o-z)
c
c
      if(ncpu.lt.1)stop
      rncpu=1./float(ncpu)
c
c      Specify transaction utilisation?
c
      rho=.001
c
      invrho=.5+(1./rho)
      tsiat=rho/(1.-rho)
      r=tsiat*rncpu
c 
      utarget=100.*serv_time*txps*rncpu
c
      j1=1
      j2=12*ncpu*invrho/10
      ipa=1
      ipb=4*ncpu*invrho
      umax=99.0
      mcmax=1.5*float(ncpu)/rho
c
c     write(*,1121)ncpu,serv_time,tpsmax
c
      ax=float(mcmax)/float(nlines)
c
      j2=mcmax
      call solver(j1,j2,mcust,utarget)
      if(iretcode.eq.0
     *  )then
c
c      Display the numbers of users that bracket this txn rate
c
             call dispres_2(j1,j2)
      endif
c
      return
c
106   format('***** ERROR: Failure in boundary check')
1121  format(//
     *   '                             Number of servers:',i10
     * ,/'          Service time of a single transaction:',f10.2
     * , ' seconds'
     * ,/'              Absolute max transactions/second:',f10.2)
c
      end
c
c
c
      subroutine getanswer(answer)
c
c      Returns y or n
c
      character *1 answer
c
  1   continue
c
      read(*,106)answer
c
      if(answer.eq.'y'.or.answer.eq.'Y')then
                                            answer='y'
                                            return
      endif
c
      if(answer.eq.'n'.or.answer.eq.'N')then
                                            answer='n'
                                            return
      endif
c
      write(*,100)
      go to 1
c
100   format('Please answer Y or y or N or n')
101   format('            Default value will be set')
106   format(a1)
c
      return
      end
c
c
c
      subroutine dispres_2(j1,j2)
c
      use qcomm
c
      implicit real(8) (a-h,o-z)
c
      character *1 hex_d
c
      hex_d=char(13)
c
      call qfx(j1)
      txns1=eff/serv_time
        ws1=ws *serv_time
        wq1=wq *serv_time
      util1=util
c
      call qfx(j2)
      txns2=eff/serv_time
        ws2=ws *serv_time
        wq2=wq *serv_time
      util2=util
c
      txns=projector(util1,txns1,util2,txns2,utarget)
        ws=projector(util1,  ws1,util2,  ws2,utarget)
        wq=projector(util1,  wq1,util2,  wq2,utarget)
c
      wsn=ws/serv_time
c
      write(2,111)id_serv,ncpu,serv_time,ws,wq,utarget,txns,wsn
c
      return
111   format(2i4,3f12.4,2f12.2,f12.4)
112   format(2i4,3f12.4,2f12.2,f12.4,a1)
      end  
c
c
c
      subroutine solver(j1,j2,j,ytarget)
c
c      Iterates to solution t=f(x) between x1 and x2
c
      use qcomm
c
      implicit real(8) (a-h,o-z)
c
c
      common refine
c
      iretcode=-1
      ncount=0
      call qfx(j1)
      y1=util
c     write(*,101)ncount,j1,y1
      call qfx(j2)
      y2=util
c     write(*,101)ncount,j2,y2
      if((y1-ytarget)*(y2-ytarget).gt.0.)then
                                 write(*,100)ytarget,j1,j2,y1,y2
                                 return
      endif
c
  2   continue
c
c      Binary chop
c
      j=(j2+j1)/2
      call qfx(j)
      y=util
c
c      Save former values of J2 and J1
c
      j1old=j1
      j2old=j2
c
c     Reassign j1 or j2
c
      if(y.gt.ytarget)then
                          j2=j
                      else
                          j1=j
      endif
      ncount=ncount+1
c     write(*,101)ncount,j,y
      if((j2.ne.j2old).or.(j1.ne.j1old))then
                                            go to 2
      endif
c
      iretcode=0
      return
c
100   format('***** ERROR: No solution for',f10.5
     *      ,' between',2i12,' users'
     *      ,/46x,2f12.5)
101   format(i4,i20,3f20.7)
c
      end
c
c
c
      subroutine qfx(mcust)
c
      use qcomm
c
      implicit real(8) (a-h,o-z)
c
c
         psum=0.
         amc=mcust
         do k=1,mcust
            pkx(k)=1
            if(k.lt.ncpu
     *        )then
                   do i=1,k 
                      pkx(k)=tsiat*pkx(k)*((mcust+1-i)/float(k+1-i))
                   enddo
               else
                   do i=1,ncpu
                      pkx(k)=tsiat*pkx(k)*((mcust+1-i)/float(ncpu+1-i))
                   enddo

                   do i=ncpu+1,k
                      pkx(k)=    r*pkx(k)* (mcust+1-i)
                   enddo
            endif
            psum=psum+pkx(k)
         enddo

         pkx(0)=1./(1.+psum)
c
         elless=0.
         do k=1,mcust
            pkx(k)=pkx(k)*pkx(0)
            elless=pkx(k)*float(k)+elless
         enddo
c
         eff=tsiat*(amc-elless)
         ellq=elless-eff
         ws=elless/eff
         wq=ellq/eff
         real_util=eff*rncpu
         util=100.*real_util
c
c     write(*,*)mcust,tsiat,amc,elless,ellq,ws,wq,util
c
      return
      end
