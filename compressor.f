      subroutine compressor(datin,ll,ncout)
c
      use bmp_comms
c      
c      Run_Length_Encodes data in datin
c
      character (1) datin(ll),ac,bc
c      
c      Following is an example of an 8-bit RLE bitmap.
c      The two-digit hexadecimal values in the
c      second column represent a color index for a single pixel
c      
c      Compressed data		Expanded data
c      
c      02 78			78 78 
c      03 04			04 04 04 
c      09 1E			1E 1E 1E 1E 1E 1E 1E 1E 1E 
c      05 06			06 06 06 06 06 
c      00 00			End of line 
c      00 01			End of RLE bitmap 
c      00 02 05 01		Move 5 right and 1 down 
c      00 03 45 56 67 00	45 56 67 
c
c     write(*,100)datin
c
      llsub=0
      ac=datin(1)
      nmax=254
      ndc=0
      nsc=0
      do n=2,ll
         bc=datin(n)
         if(ac.eq.bc
     *     )then
c
c      This pixel is the same as the last one - increase the SameCount
c
                nsc=nsc+1
                if(ndc.gt.0
     *            )then
c
c      There is a string of different pixels - write them out
c
                       nlast=n-2
                       ndc=ndc-1
                       call diffwrite(datin,ll,nlast,ndc,ntot,ncout)
                       llsub=llsub+ntot
                endif
            else
c
c      This pixel is different to the last one
c
                if(nsc.gt.0
     *            )then
c
c      There is a string of same pixels - write them out
c
                       call samewrite(ac,nsc,ntot,ncout)
                       llsub=llsub+ntot
                   else
                       ndc=ndc+1
                endif
c
         endif
         ac=bc
      enddo
c
c
      if(nsc.gt.0
     *  )then
             call samewrite(ac,nsc,ntot,ncout)
             llsub=llsub+ntot
      endif
c
      if(ndc.gt.0.or.(ll-llsub.eq.1)
     *  )then
             nlast=ll
             call diffwrite(datin,ll,nlast,ndc,ntot,ncout)
             llsub=llsub+ntot
      endif
c
c      Write 'end-of-line'characters...
c
      ncout=ncout+1
      tmparray(ncout)=char(0)
      ncout=ncout+1
      tmparray(ncout)=char(0)
c
      if(llsub.ne.ll
     *  )then
c
c      DISASTER!!!
c
             write(0,101)llsub,ll
c        else
c            write(0,102)llsub,ll
      endif
c
      return
c
100   format(32z3.2)
101   format(/'#########################################'
     *      ,/'#         ERROR IN COMPRESSOR            #',2i6
     *      ,/'#########################################')
102   format('COMPRESSOR: Wrote',i8,' of',i8,' characters',/)
      return
c
107   format(i8,' of ',512a1)
c
      end
