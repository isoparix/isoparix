      subroutine ip_spread
c
c      Lightens or darkens picture data by ref to nwhite
c
      use ip_comms
c
      integer(4),dimension(0:255) :: icdf,nslots
c
      real(4)vcdf
c
c      Set the transfer function for higher contrast...
c
      nx=0
      do n=255,lo_contrast,-1
         if(nx.eq.0)maxnzpa=n
         nx=nx+nval(n)
         lval(n)=nx
      enddo
      contrast_pels=nx
      maxnzpa=maxnzpa-1
c
c     hi_contrast=maxnzp
c
      yhigh=hi_contrast
      ylow=lo_contrast
      write(*,*) 'IP_SPREAD: ',lo_contrast,ylow,hi_contrast,yhigh
c
c
c      CDF should go in a straight line: 
c         from (hi_contrast,hi_contrast) to (255,255), and
c         from (nthresh,nthresh) to (0,0)
c
      do n=hi_contrast,255
         ixfer(n)=n
      enddo
c
      do n=0,lo_contrast
         ixfer(n)=n
      enddo
c
      n1=hi_contrast
      mx=n1
      idold=0
      nid=0
      nslots=0
      ndivs=100
      s=(yhigh-ylow)/float(ndivs)
      nq=.5+(contrast_pels/float(ndivs))
      do n=hi_contrast,lo_contrast,-1
         id=lval(n)/nq
         if(id.gt.idold.or.
     *       n.eq.lo_contrast
     *     )then
                write(8,101)n1,n+1,nid
                a=1./float(nid)
                b=0.
                do m=n1,n+1,-1
                   b=b+1.0
                    xfer(m)=float(idold)+(a*b)
                   ixfer(m)=.5+(yhigh-(xfer(m)*s))
                   write(8,103)m,mval(m),nq,idold,xfer(m),ixfer(m)
                enddo
                n1=n
                nid=0
                idold=id
         endif
         nid=nid+1
      enddo
c
c      Record the transfer function
c
      do ix=0,255
         write(8,101)ix,nval(ix),mval(ix),ixfer(ix)
      enddo
      call isoflush(8)
c
      do ix=0,255
         ixk=ix+266
         ixl=ix+530
         iyk=255-ixfer(ix)+nh_current
         histarray(ixk,iyk)=255-histarray(ixk,iyk)
         histarray(ixl,iyk)=255-histarray(ixl,iyk)
c        histarray(ixl,iyl)=255-histarray(ixl,iyl)
      enddo
c
      do iy=0,iydim-1
         do ix=0,ixdim-1
            picdata(ix,iy)=char(ixfer(ipic(ix,iy)))
         enddo
      enddo
c
      return
c
101   format(12i11)
102   format(/)
103   format(4i10,f10.3,i10)
c
      end
