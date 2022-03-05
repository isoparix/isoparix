      subroutine link_builder
c
      use links_comms
c
      linkcount=0
      do j=2,nodecount
         do i=1,j-1
            if(link(i,j)
     *        )then
                   linkcount=linkcount+1
                   linkid_a(linkcount)=i
                   linkid_b(linkcount)=j
c
                    dx=xnode(j)-xnode(i)
                    if(dx.eq.0.0
     *                )then               
                           dx=0.0000000001
                    endif
                    mlink(linkcount)= (ynode(j)-ynode(i))/dx
                    clink(linkcount)=((xnode(j)*ynode(i))-
     *                                (xnode(i)*ynode(j)))/dx
            endif
         enddo
      enddo
c
      return
c
      end
