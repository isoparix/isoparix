      subroutine predict(avg,sdn,nodes,nsamples)
c
c   Calculate loadings from mean and sd at sample size
c
      implicit real (8) (a-h,o-z)
c
c      Check for more than one sample....
c
      if(nodes.le.0)then
                        write(8,103)nodes
                        return
      endif
c
      z900=1.28
      z950=1.65
      z990=2.34
      z999=3.1
      z9999=3.75
      z99997=4.0
c
      write(8,121)avg,sdn,nsamples
      write(8,101)nodes
c
      do m=1,20
         n=(2**(m/2))+(2**((m/2)-1)*mod(m,2))
         sd=sdn*sqrt(float(n)/float(nodes))
         a =avg*float(n)
         x2=a+(2.*sd)
         x3=a+(3.*sd)
         x4=a+(4.*sd)
         x5=a+(5.*sd)
         x6=a+(6.*sd)  

         write(8,100)n,a,sd,x2,x3,x4,x5,x6
      enddo
c
      write(8,102)
c
100   format(i5,7f12.1)
101   format(/
     *       'Number of systems used as base is',i4 
     *    ,//'If the data represented by these samples were aggregated'
     *     ,/'by nodes, the total means'
     *      ,' and percentiles would be as follows:'
     *    ,//'Nodes'
     *      ,'        Mean'
     *      ,'     Std dev'
     *      ,'    Mean+2SD'
     *      ,'    Mean+3SD'
     *      ,'    Mean+4SD'
     *      ,'    Mean+5SD'
     *      ,'    Mean+6SD'
     *,/)
102   format(//)
103   format(/i6,' systems is too few to make'
     *          ,' predictions about percentiles!')
121   format(//'   Mean=',f16.3      
     *       ,/'     SD=',f16.3      
     *       ,/'Samples=',i16        
     *       ,/)                  

c
      return
      end
