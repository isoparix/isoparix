      program data_generator
c
      use dg
c
c      For test purposes....
c
      write(0,104)
      read( 2,*)ntestcpu
      allocate (nodename(ntestcpu))
      write(0,105)
      read( 2,*)ntestdisks
      write(0,106)
      read( 2,*)ntestcores
c
      ntest_sleep=interval*1000000
c
      maxdevs=0
      maxcores=0
      open(34,file='list_of_nodes',status='unknown',form='formatted')
      do n=1,ntestcpu
         write(nodename(n),100)n
         write(34,103)nodename(n)
         write(3 ,103)nodename(n)
         maxdevs =maxdevs +ntestdisks+(2*n)
         maxcores=maxcores+ntestcores+(2*n)
      enddo
      call isoflush(34)
      call isoflush(3)
      close(34)
c
      open(34,file='all_stat.iostat',status='unknown',form='formatted')
      write(34,*)ntestcpu,ntestdisks+(2*ntestcpu),0,maxdevs
     *                   ,ntestcores+(2*ntestcpu),maxcores
      call isoflush(34)
      close(34)
c
      lastcol=1
      t_test=0.
  1   continue
      t_test=t_test+.1
c
c      Odd nodes first...
c
         do n=1,ntestcpu,2
            ntestactive=n
            call data_gen
         enddo
c
c      ...then the even ones..
c
         do n=2,ntestcpu,2
            ntestactive=n
            call data_gen
         enddo
c
      go to 1
      stop
c
100   format('TestNode',i3.3)
103   format(a)
104   format('Number of nodes?')
105   format('Base number of disks? (Increases by 2/node)')
106   format('Base number of cores? (Increases by 2/node)')
107   format('Interval (seconds) per set of samples?')
108   format('Number of samples?')
      end
