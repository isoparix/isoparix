      subroutine clash_check
c
c      Finds clashes of links
c
      use links_comms
c
      write(*,108)linkcount
      do i=1,linkcount
         ka=linkid_a(i)
         kb=linkid_b(i)
         do j=i+1,linkcount
            clash=.false.
            if(
     *          linkid_a(j).eq.ka.or.  ! Link end-points identical
     *          linkid_a(j).eq.kb.or.  ! Link end-points identical
     *          linkid_b(j).eq.ka.or.  ! Link end-points identical
     *          linkid_b(j).eq.kb.or.  ! Link end-points identical
     *             mlink(j).eq.mlink(i)! Links parallel
     *           )then
                      xicalc=.false.
                  else
                      xicalc=.true.
                      xi=(clink(j)-clink(i))/(mlink(i)-mlink(j))
c
c                   write(*,111)xi,xnode(ka)         ,xnode(kb)
c    *                            ,xnode(linkid_a(j)),xnode(linkid_b(j))
                 if((xnode(linkid_a(j))-xi)*(xnode(linkid_b(j))-xi).lt.0
     *                         .and.       
     *                       (xnode(ka)-xi)*(xnode(kb)-xi).lt.0
     *                )then
                           clash=.true.
                 endif
            endif
c
            if(clash
     *        )then
                   nclash=nclash+1
                   write(*,109)i,ka,kb,j,linkid_a(j),linkid_b(j),xi
c              else
c                  if(xicalc
c    *               )then
c                         write(*,1101)i,ka,kb,j,linkid_a(j),linkid_b(j)
c    *                                  ,xi
c                     else
c                         write(*,110) i,ka,kb,j,linkid_a(j),linkid_b(j)
c                  endif
            endif
         enddo
      enddo
c
      write(*,114)linkcount,nodecount,nclash
c
      return
c
108   format(/'There are',i3,' links',/)
109   format('Link',i2,' (',2i2,')  clashes with link'
     *             ,i2,' (',2i2,') at x=',e12.2)
110   format('Link',i2,' (',2i2,')      OK       link'
     *             ,i2,' (',2i2,')')
1101  format('Link',i2,' (',2i2,')      OK       link'
     *             ,i2,' (',2i2,') at x=',e12.2)
114   format(/i5,' links across',i3,' nodes. Clashes:',i2)
c
      end
