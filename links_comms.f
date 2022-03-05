      module links_comms
c
      parameter (maxnodes=10)
c
      integer linkcount,nodecount
c
      logical        link(maxnodes,maxnodes),clash,xicalc
c    *         ,sep_link((maxnodes*(maxnodes-1))/2 ! Not worth the space
c    *                  ,(maxnodes*(maxnodes-1))/2)
c
      real mlink
c
      dimension     xnode(maxnodes)
     *         ,    ynode(maxnodes)
     *         ,   nlinks(maxnodes)
     *         , linkid_a((maxnodes*(maxnodes-1))/2)
     *         , linkid_b((maxnodes*(maxnodes-1))/2)
     *         ,    mlink((maxnodes*(maxnodes-1))/2)
     *         ,    clink((maxnodes*(maxnodes-1))/2)
c
      character (1) item
      character (15)numbers
c
      end
