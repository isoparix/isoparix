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
      use qcomm
c
      implicit real(8) (a-h,o-z)
c
      character *1 stans,rhoans,tpsans,userans,numans
c
      open(2,file='txnwin.prn',status='unknown')
      nlines=25
c
      percentrho=10.0
      serv_time=1.
  1   continue
c
c      mcust=finite number of customers
c      ncpu=number of servers (eg CPUs)
c      invrho is the inverse of rho.   Used if no value of rho available.
c      rho is the utilisation imposed on the server by a single stream
c      tsiat is product of service time and interarrival rate
c
c      Set defaults
c
      stans='n'
      rhoans='n'
      tpsans='n'
      userans='n'
      
      write(*,104)
      read(*,*)ncpu
      if(ncpu.lt.1)stop
      rncpu=1./float(ncpu)
c
c      Set number of CPUs
c
  3   continue
c
c      Specify output format
c
      write(*,204)
      call getanswer(numans)
      if(numans.eq.'n')then
                           numbers=.false.
                       else
                           numbers=.true.
      endif 
c
c      Specify transaction utilisation?
c
      write(*,200)percentrho
      call getanswer(rhoans)
      if(rhoans.eq.'y')then
                           write(*,101)
                           read(*,*)percentrho
      endif
      rho=.01*percentrho
c
      invrho=.5+(1./rho)
      tsiat=rho/(1.-rho)
      r=tsiat*rncpu
c 
      tpsmax=float(ncpu)/serv_time
c
      write(*,202)serv_time
      call getanswer(stans)
      if(stans.eq.'y')then
                        write(*,102)
                        read(*,*)serv_time
                        if(serv_time.lt.0)stop
                        tpsmax=float(ncpu)/serv_time
      endif
c
      write(*,201)tpsmax
      call getanswer(tpsans)
      if(tpsans.eq.'y')then
                   write(*,103)tpsmax
                   read(*,*)txps
                   utarget=100.*serv_time*txps*rncpu
                   if(utarget.gt.100.)then
                                          write(*,105)
                                          go to 1
                   endif
      endif
c
c
      j1=1
      j2=12*ncpu*invrho/10
      ipa=1
      ipb=4*ncpu*invrho
      umax=99.0
c     call solver(ipa,ipb,mcmax,umax)
      mcmax=1.5*float(ncpu)/rho
c
      if(tpsans.eq.'n')then
                           write(*,203)mcmax
                           call getanswer(userans)
                           if(userans.eq.'y')then
                                                 write(*,108)
                                                 read (*,*)mcmax
                           endif
      endif
c
      if(iretcode.ne.0)then
                           write(*,106)
                           go to 1
      endif
c
      write(*,112)ncpu,100.*rho,serv_time,tpsmax
c
      ax=float(mcmax)/float(nlines)
c
      if(tpsans.eq.'y')then
                           j2=mcmax
                           call solver(j1,j2,mcust,utarget)
                           if(iretcode.eq.0)then
c
c      Display the numbers of users that bracket this txn rate
c
                                                call qfx(j1)
                                                call dispres
                                                call qfx(j2)
                                                call dispres
                                                call interp(j1,j2)
                                                call dispres
                           endif
                       else
                           if(mcmax.le.nlines)then
                                               do m=1,mcmax
                                                  call qfx(m)
                                                  call dispres
                                               enddo
                                           else
                                               do m=1,nlines
                                                  mc=.5+(float(m)*ax)
                                                  if(m.eq.1.and.mc.ne.1
     *                                              )then
                                                         call qfx(1)
                                                         call dispres
                                                  endif
                                                  call qfx(mc)
                                                  call dispres
                                               enddo
                           endif
      endif
c
      go to 1
c
101   format( 'Enter the % utilisation of a single stream on server',i3)
102   format( 'Enter the service time (in seconds) of one transaction')
103   format( 'Enter the number of transactions per second'
     * ,      ' (max is',f6.2,')')
104   format(/'Enter the number of servers (eg number of CPUs)'
     *,/      '(Zero to end program)')
105   format('***** ERROR: System cannot sustain that transaction rate'
     *     ,/'             Add more servers (eg CPUs), reduce'
     *     , ' transaction cost'
     *     ,/'             or accept lower transaction rate'
     *     ,/)     
106   format('***** ERROR: Failure in boundary check')
107   format('Enter target utilisation of one CPU')
108   format(/'Enter the maximum number of users')
112   format(//
     *   '                             Number of servers:',i10
     * ,/'  Utilisation of one transaction on one server:',f10.2,' %'
     * ,/'          Service time of a single transaction:',f10.2
     * , ' seconds'
     * ,/'              Absolute max transactions/second:',f10.2
     * //'    No of       Avg number in            Avg wait in   '
     * , '  Avg utilisation TXNs per Normalised'
     * ,/'    Users     system      queue       system      queue'
     *  ,'   of one server   second   resp time'
     *  ,/)
200   format('The default utilisation of one server by',
     *       ' one user is',f9.3,' %'
     *      ,/'  Do you wish to change this (y/n)?')
201   format('The maximum number of transactions/sec is',f12.1
     *    ,/'  Do you wish to specify a particular txn rate (y/n)?')
202   format('The default service time of one'
     *      ,' transaction is',f9.3,' seconds'
     *      ,/'  Do you wish to change this (y/n)?')
203   format('The default max number of users is',i12
     *      ,/'  Do you wish to change this value (y/n)?')
204   format('The default output format for q lengths is numbers'
     *      ,/'  Do you wish to change for percentages (y/n)?')
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
      subroutine interp(j1,j2)
c
      use qcomm
c
      implicit real(8) (a-h,o-z)
c
      x1=j1
      x2=j2
c
      call qfx(j1)
        util1=util
         eff1= eff
          ws1=ws
          wq1=wq
      elless1=elless
        ellq1=ellq
c
      call qfx(j2)
        util2=util
         eff2= eff
          ws2=ws
          wq2=wq
      elless2=elless
        ellq2=ellq
c
        util=utarget
         eff=projector(util1,   eff1,util2,   eff2,utarget)
          ws=projector(util1,    ws1,util2,    ws2,utarget)
          wq=projector(util1,    wq1,util2,    wq2,utarget)
      elless=projector(util1,elless1,util2,elless2,utarget)
        ellq=projector(util1,  ellq1,util2,  ellq2,utarget)
       users=projector(util1,     x1,util2,     x2,utarget)
c
      return
      end
c
c
c
      subroutine dispres
c
      use qcomm
c
      implicit real(8) (a-h,o-z)
      character *1 hex_d
c
      hex_d=char(13)
      txns=eff/serv_time
      wsa =ws *serv_time
      wq  =wq *serv_time
c   
      if(numbers
     *  )then
c
             ellsum=100./(elless+ellq)
             elless=elless*ellsum
             ellq  =ellq  *ellsum
c
      endif
      write(*,111)int(users),elless,ellq,wsa,wq,util,txns,ws
      write(2,112)int(users),elless,ellq,wsa,wq,util,txns,ws,hex_d
c
      return
111   format(i9,7f12.4)
112   format(i9,7f12.4,a1)
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
         psum=0.
         users=mcust
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
         eff=tsiat*(users-elless)
         ellq=elless-eff
         ws=elless/eff
         wq=ellq/eff
         real_util=eff*rncpu
         util=100.*real_util
c
c     write(*,*)mcust,tsiat,users,elless,ellq,ws,wq,util
c
      return
      end
