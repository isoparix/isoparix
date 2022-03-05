      program links
c
c       Checks for intersecting links
c
      use links_comms
c
      link=.false.
      nlinks=0
      nodecount=0
c
      nchann=3
   1  continue
c
      read(nchann,100,end=2)item,numbers
      if(item.eq.'L'
     *  )then
             read(numbers,101)node1,node2 ! Integer nodes
             if(node1.gt.0.and.node2.gt.0
     *         )then
                    if(node1.gt.nodecount)nodecount=node1
                    if(node2.gt.nodecount)nodecount=node2
                    link(node1,node2)=.true.
                    link(node2,node1)=.true.
                    nlinks(node1)=nlinks(node1)+1
                    nlinks(node2)=nlinks(node2)+1
                else
                    link(node1,node2)=.false.
                    if(nlinks(node1).gt.0)nlinks(node1)=nlinks(node1)-1
                    if(nlinks(node2).gt.0)nlinks(node2)=nlinks(node2)-1
             endif
      endif
c
c     if(item.eq.'N'
c    *  )then
c            read(numbers,102)x,y ! Real co-ordinates
c            nodecount=nodecount+1
c            xnode(nodecount)=x
c            ynode(nodecount)=y
c     endif
c
      if(item.eq.'n'
     *  )then
             read(numbers,113)n,x,y ! Node to be changed
             write(*,112)xnode(n),ynode(n),x,y
             xnode(n)=x
             ynode(n)=y
      endif
c
      call links_grid
c
      nclash=0
      write(*,104)
      write(*,103)(node,xnode(node),ynode(node),nlinks(node)
     *            ,node=1,nodecount)
      write(*,107)(mx,mx=1,nodecount-1)
      do j=2,nodecount
         write(*,106)j,(link(mx,j),mx=1,j-1)
      enddo
c
      call link_builder
c
      call clash_check
c
      go to 1
c
   2  continue
      nchann=5
      go to 1
c
      stop
c
100   format(a1,a15)
101   format(2i5)
102   format(2f5.1)
103   format(i2,2f5.1,i3)
104   format(/)
106   format(i3,10l3)
107   format(/i6,10i3)
111   format(/e12.2,4f5.1)
112   format('Node at',2f5.1,' to be changed to',2f5.1)
113   format(i5,2f5.1)
c
      end
