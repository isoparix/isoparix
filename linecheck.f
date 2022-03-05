      subroutine linecheck(idv1,idv2,nf,nc)
c
      use isocomm
c
c      Check the line segments - does it make sense to pair these 
c      vertices in this cube?
c
            do ls=1,lsc(nf,nc)
               i=0
               j=0
               k=0
               if(ivx(idv1,nc).eq.ivx(idv2,nc))i=1
               if(ivy(idv1,nc).eq.ivy(idv2,nc))j=1
               if(ivz(idv1,nc).eq.ivz(idv2,nc))k=1
               ln=i+j+k
               if(ln.ne.2)then
                              write(*,100)ln,i,j,k,ls,nf,nc
     *                                  ,idv1,idv2
     *                                  ,ivx(idv1,nc),ivx(idv2,nc)
     *                                  ,ivy(idv1,nc),ivy(idv2,nc)
     *                                  ,ivz(idv1,nc),ivz(idv2,nc)
                              call cubecheck(nc)
               endif
            enddo
c
100   format('***** ERROR: LN:',4i2,', LS',i3,', NF',i3,' NC',i3
     *     ,/'         V1:',i5,', V2:',i5
     *     ,/'            ',i5,'     ',i5
     *     ,/'            ',i5,'     ',i5
     *     ,/'            ',i5,'     ',i5
     *      )
c
      return
      end
