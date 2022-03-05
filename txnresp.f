      program txnresp
c
c      Generates data for Q where
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
      include 'qcomm.h'
      character *1 stans,rhoans,tpsans
c
      nlines=25
c
  1   continue
c
c      mcust=finite number of customers
c      ncpu=number of servers (eg CPUs)
c      invrho is the inverse of rho.   Used if nov value of rho available.
c      rho is the utilisation imposed on the server by a single stream
c      tsiat is product of service time and interarrival rate
c
c      Set number of CPUs
c
      tpsans='n'
      rhoans='n'
      stans ='n'
c
      write(*,104)
      read(*,*)ncpu
      rncpu=1./float(ncpu)
c
  3   continue
c
c      Specify transaction utilisation?
c
      write(*,200)
      call getanswer(rhoans)
      if(rhoans.eq.'y')then
                           write(*,101)
                           read(*,*)rho
                           rho=.01*rho
                       else
                           rho=.01
      endif
c
      invrho=.5+(1./rho)
      tsiat=rho/(1.-rho)
      r=tsiat*rncpu
c
      write(*,202)
      call getanswer(stans)
      if(stans.eq.'y')then
                        write(*,102)
                        read(*,*)serv_time
                        if(serv_time.lt.0)stop
c
                        write(*,201)
                        call getanswer(tpsans)
                        if(tpsans.eq.'y')then
                                     write(*,103)
                                     read(*,*)txps
                                     utarget=100.*serv_time*txps*rncpu
                                     if(utarget.gt.100.)then
                                                            write(*,105)
                                                            go to 1
                                     endif
                        endif
c
                    else
                        serv_time=1.
      endif
c 
      tpsmax=float(ncpu)/serv_time
c
      write(*,112)ncpu,100.*rho,serv_time,tpsmax
c
      j1=1
      j2=12*ncpu*invrho/10
      umax=99.5
      call solver(1,2*ncpu*invrho,mc,umax)
c
      if(iretcode.ne.0)then
                           write(*,106)
                           go to 1
      endif
c
      ax=float(mc)/float(nlines)
c
      if(tpsans.eq.'y')then
                           j2=mc
                           call solver(j1,j2,mcust,utarget)
                           if(iretcode.eq.0)then
c
c      Display the numbers of users that bracket this txn rate
c
                                                call dispres(j1)
                                                call dispres(j2)
                           endif
                       else
                           if(j2.le.nlines)then
                                               do mc=1,j2
                                                  call dispres(mc)
                                               enddo
                                           else
                                               do m=1,nlines
                                                  mc=.5+(float(m)*ax)
                            if(m.eq.1.and.mc.ne.1)call dispres(1)
                                                  call dispres(mc)
                                               enddo
                           endif
      endif
c
      go to 1
c
101   format( 'Enter the % utilisation of a single stream on server',i3)
102   format( 'Enter the service time (in seconds) of one transaction')
103   format( 'Enter the number of transactions per second')
104   format(/'Enter the number of servers (eg number of CPUs)')
105   format('***** ERROR: System cannot sustain that transaction rate'
     *     ,/'             Add more servers (eg CPUs), reduce'
     *     , ' transaction cost'
     *     ,/'             or accept lower transaction rate'
     *     ,/)     
106   format('***** ERROR: Failure in boundary check')
107   format('Enter target utilisation of one CPU')
112   format(//
     *   '                             Number of servers:',i10
     * ,/'  Utilisation of one transaction on one server:',f10.2,' %'
     * ,/'          Service time of a single transaction:',f10.2
     * , ' seconds'
     * ,/'              Absolute max transactions/second:',f10.2
     * //' No of       Avg number in            Avg wait in   '
     * , '  Avg utilisation  TXNs per'
     * ,/' Users     system      queue       system      queue'
     *  ,'   of one server    second '
     *  ,/)
200   format('Do you wish to specify the utilisation of one server by',
     *       ' one transaction (y/n)?')
201   format('Do you wish to specify the number of transactions/sec',
     *       ' (y/n)?')
202   format('Do you wish to specify the service time of one'
     *      ,' transaction (y/n)?')
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
                                            write(*,101)
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
      subroutine dispres(mcust)
c
      include 'qcomm.h'
c
      call qfx(mcust)
c
      txns=eff/serv_time
      ws  =ws *serv_time
      wq  =wq *serv_time
c   
      write(*,111)mcust,elless,ellq,ws,wq,util,txns
c
      return
111   format(i4,4f12.2,f12.1,f12.2)
      end  
c
c
c
      subroutine solver(j1,j2,j,ytarget)
c
c      Iterates to solution t=f(x) between x1 and x2
c
      include 'qcomm.h'
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
      include 'qcomm.h'
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
         util=100.*eff*rncpu
c
c     write(*,*)mcust,tsiat,amc,elless,ellq,ws,wq,util
c
      return
      end

