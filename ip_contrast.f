      subroutine ip_contrast
c
c      Lightens or darkens picture data by ref to nwhite
c
      use ip_comms
c
      logical piecewise
c
      piecewise=.true.
c
c      How many pels above the dark threshold?
c
      write(*,104)nthresh,maxnzp,nonzero_pels
c
c      Set the transfer function for higher contrast...
c
      if(piecewise
     *  )then
             nwhite=maxnzp
             nmid=.5+balance
             ymid=100.0
             nblack=nthresh+1
             yblack=nblack
c
c      From nblack to nmid, and from nmid to nwhite, use a piecwise
c      linear approach, to maintain the low levels
c
             am=slope(float(nblack),yblack,float(nmid),ymid)
             ac=cept (float(nblack),yblack,float(nmid),ymid)
             write(*,*)am,ac
             do n=nblack,nmid
                ixfer(n)=0.5+((am*float(n))+ac)
             enddo
c
             am=slope(float(nmid),ymid,float(nwhite),255.0)
             ac=cept (float(nmid),ymid,float(nwhite),255.0)
             write(*,*)am,ac
             do n=nmid+1,nwhite
                ixfer(n)=0.5+((am*float(n))+ac)
             enddo
         else
c
c      From nblack to nwhite, use a power law, Gamma...
c
             nblack=nthresh
             nwhite=maxnzp
             x1=nblack
             y1=0.0
             x2=nwhite
             y2=255.0
             x3=128.0
             y3=128.
             rxdiff=1./(x2-x1)
              ydiff=y2-y1
             rydiff=1./ydiff
             gamma=log(rydiff*(y3-y1))/log(rxdiff*(x3-x1))
             write(*,103)gamma
             a=1.0
             do n=nwhite,nblack+1,-1
                b=a
                a=(ydiff*((rxdiff*(float(n)-x1))**gamma))+y1
                ixfer(n)=a+.5
             enddo
             delta=b-a
      endif
c
c      From zero to nblack:
c
      do n=nblack,0,-1
         ixfer(n)=0
      enddo
c
c      From nwhite to 255:
c
      ixfer(nwhite:255)=255
c
c
      do iy=0,iydim-1
         do ix=0,ixdim-1
            picdata(ix,iy)=char(ixfer(ipic(ix,iy)))
         enddo
      enddo
c
c      Record the transfer function
c
      write(8,101)nblack,nmid,nwhite
      do ix=0,255
         write(8,101)ix,ixfer(ix)
         ixk=ix+266
         ixl=ix+530
         iyk=255-ixfer(ix)+nh_current
         histarray(ixk,iyk)=255-histarray(ixk,iyk)
         histarray(ixl,iyk)=255-histarray(ixl,iyk)
      enddo
c
      call isoflush(8)
      return
100   format('Brightening by',f12.5)
101   format(12i11)
102   format('Transfer function')
103   format('Gamma is:',f10.6)
104   format(i3,'<n<=',i4,' Visible:',i10)
c
      end
