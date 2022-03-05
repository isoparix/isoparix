      program bmpread3
c
c     Uses the data in a bitmap to check it
c
c      The character array inparray has values between 0 and 255, which
c      correspond to colour values.  The association between colour
c      values and actual screen or printed colours is made via the 
c      rgbquad colour map.
c
c      Following is an example of an 8-bit RLE bitmap.
c      The two-digit hexadecimal values in the
c      second column represent a color index for a single pixel
c      
c      Compressed data		Expanded data
c      
c      03 04			04 04 04 
c      05 06			06 06 06 06 06 
c      00 03 45 56 67 00	45 56 67 
c      02 78			78 78 
c      00 02 05 01		Move 5 right and 1 down 
c      02 78			78 78 
c      00 00			End of line 
c      09 1E			1E 1E 1E 1E 1E 1E 1E 1E 1E 
c      00 01			End of RLE bitmap 
c      
c ###### BITMAPFILEHEADER START
c
      logical compress			!8-bit RLE, or not?
c
      integer (4) nhist(0:100000)
      integer (4) ixbm			!X-dimension of picture
      integer (4) iybm			!Y-dimension of picture
      character (34) filename
c
      integer (2) exintel2
      integer (4) exintel4
c
      character (2) adintel2
      character (4) adintel4
c
      character (1),allocatable,dimension(:) :: bitmap_array,tmp_array
c
      character (1) header_short_array(27),eorec,cr
      character (27) header_short
c
      character (1) header_array(1078),char_one,char_zero
      character (1078) header_chars
c
      equivalence(header_chars,header_array)
      equivalence(header_short,header_short_array)
c
      character (2) resource	!Resource type BM for bitmaps
      character (4) lenbfh	!Size of BitMap File
      character (2) xhot,yhot	!Hot-spot co-ordinates for icons only
      character (4) offbits	!Byte offset to bitmap data
c 
      integer (2) int_xhot,int_yhot,int_nplanes,int_nbpp
c
c ###### BITMAPFILEHEADER END
c
c ###### BITMAPINFOHEADER2 START
c
      character (4) lenbmih2	!Length of struct
      character (4) bmwidth	!Bit-map width  in pels
      character (4) bmheight	!Bit-map height in pels
      character (2) nplanes	!Number of bit planes
      character (2) nbpp	!Number of bits per pel per plane
      character (4) idcompress	!Compression type RLE 8-bit=1
      character (4) image_size	!Size in bytes of the image
      character (4) xppm,yppm	!Horizontal/vertical pels/metre
      character (4) ncolindex	!Number of colour indices
      character (4) important	!Number of important indices (0=all)
c
      equivalence(xhot      ,int_xhot)
      equivalence(yhot      ,int_yhot)
      equivalence(offbits   ,int_offbits)
      equivalence(lenbfh    ,int_lenbfh)
      equivalence(lenbmih2  ,int_lenbmih2)
      equivalence(bmwidth   ,int_bmwidth)
      equivalence(bmheight  ,int_bmheight)
      equivalence(nplanes   ,int_nplanes)
      equivalence(nbpp      ,int_nbpp)
      equivalence(idcompress,int_idcompress)
      equivalence(image_size,int_image_size)
      equivalence(xppm      ,int_xppm)
      equivalence(yppm      ,int_yppm)
      equivalence(ncolindex ,int_ncolindex)
      equivalence(important ,int_important)
c
      logical error_msg
c
      lchann=10
c
      char_one =char(1)
      char_zero=char(0)
c
c ###### BITMAPINFOHEADER2 END
c
c      Read in header info
c
      read(81,100)
     *            resource       ! 0000 - 0001
     *           ,lenbfh         ! 0002 - 0005
     *           ,xhot           ! 0006 - 0007
     *           ,yhot           ! 0008 - 0009
     *           ,offbits        ! 000A - 000D
     *           ,lenbmih2       ! 000E - 0011
     *           ,bmwidth        ! 0012 - 0015
     *           ,bmheight       ! 0016 - 0019
     *           ,nplanes        ! 001A - 001B
     *           ,nbpp           ! 001C - 001D
     *           ,idcompress     ! 001E - 0021
     *           ,image_size     ! 0022 - 0025
     *           ,xppm           ! 0026 - 0029
     *           ,yppm           ! 002A - 002D
     *           ,ncolindex      ! 002E - 0031
     *           ,important      ! 0032 - 0035
c
      write(*,1001)resource
     *           ,int_lenbfh    ,int_lenbfh
     *           ,int_xhot      ,int_xhot
     *           ,int_yhot      ,int_yhot
     *           ,int_offbits   ,int_offbits
     *           ,int_lenbmih2  ,int_lenbmih2
     *           ,int_bmwidth   ,int_bmwidth
     *           ,int_bmheight  ,int_bmheight
     *           ,int_nplanes   ,int_nplanes
     *           ,int_nbpp      ,int_nbpp
     *           ,int_idcompress,int_idcompress
     *           ,int_image_size,int_image_size
     *           ,int_xppm      ,int_xppm
     *           ,int_yppm      ,int_yppm
     *           ,int_ncolindex ,int_ncolindex
     *           ,int_important ,int_important
      close(81)

      kx=int_lenbfh+1
      allocate(bitmap_array(0:kx+10))
      allocate(   tmp_array(1:kx+10))
      write(*,*)size(bitmap_array)
c
      eorec=char(10)
      cr   =char(13)
      maxrec=0
      nrecs=0
      ncx=0
      m=0
      nhist=0
  4   continue
      read(81,103,advance='no',iostat=irc,end=2,eor=3
     *           ,size=ncount)tmp_array
  3   continue
c
      if(irc.gt.0.or.ncount.lt.0)write(*,115)irc,ncount,m
      nlast=ncount
c     nhist(ncount)=nhist(ncount)+1
      if(ncount.gt.maxrec)maxrec=ncount
      nrecs=nrecs+1
      ncx=ncx+ncount
c
      if(ncount.gt.0
     *  )then
             n=m+ncount-1
             bitmap_array(m:n)=tmp_array(1:ncount)
             m=n+1
      endif
c
      bitmap_array(m)=eorec
      m=m+1
      go to 4
c
  2   continue
      if(irc.gt.0.or.ncount.lt.0)write(*,116)irc,ncount,m
      write(*,114)int_lenbfh,m,int_lenbfh-m,ncount,ncx,nrecs,maxrec
      deallocate(tmp_array)
c
      write(10,102)(ichar(bitmap_array(mx)),mx=54,int_offbits-1)
c
      m=int_offbits
c      
c      Compressed data		Expanded data
c      
c      02 78			78 78 
c      05 06			06 06 06 06 06 
c      09 1E			1E 1E 1E 1E 1E 1E 1E 1E 1E 
c
c      00 00			End of line 
c      00 01			End of RLE bitmap 
c      00 02 05 01		Move 5 right and 1 down 
c      00 03 45 56 67 00	45 56 67 
c      
      ncharline=0
      nchartot=0
      nline=0
c
      nrepblocks=0
      nrepchars=0
      ndiffblocks=0
      ndiffchars=0
c
      error_msg=.false.
c
      mstart=m
  5   continue
      j=ichar(bitmap_array(m))
      if(j.eq.0
     *  )then
             k=ichar(bitmap_array(m+1))
c
             if(k.eq.0
     *         )then
c
c      Two zeros in succession - end of line...
c
                    nchartot=nchartot+ncharline
                    nline=nline+1
                    write(*,200)ncharline,nchartot,nline
     *                         ,nrepblocks,nrepchars
     *                         ,ndiffblocks,ndiffchars
c
c      Clear accumulators
c
                    nrepblocks=0
                    nrepchars=0
                    ndiffblocks=0
                    ndiffchars=0
c
                    if(ncharline.ne.int_bmwidth
     *                )then
c
c      Wrong number of pixels in this line....
c
                           write(*,203)int_bmwidth
                           mend=m+1
                           go to 60
                    endif
                    ncharline=0
                    m=m+2
                    mstart=m
             endif
c
             if(k.eq.1
     *         )then
c
c      A zero and then a 1 - end of bitmap...
c
                    write(*,201)nchartot,nline
                    stop
             endif
c
             if(k.eq.2
     *         )then
c
c      A skip....   Shouldn't happen....
c
                    write(*,202)m,nline+1
                    go to 60
                    stop
             endif
c
             if(k.gt.2
     *         )then
c
c      Zero and not (0 or 1 or 2) - string of different pixels...
c
                    ncharline=ncharline+k
                    if(error_msg
     *                )then
                           write(90,301)k,k,nline,ncharline
                    endif
c                                              K 1 2 3 4 5 6 ....
                    m=m+2+(2*((1+k)/2)) ! M+2+   2 2 4 4 6 6
                    ndiffblocks=ndiffblocks+1
                    ndiffchars =ndiffchars+k
             endif
c
         else
c
c      Repetition of 'j' pixels
c
                    ncharline=ncharline+j
                    if(error_msg
     *                )then
                        write(90,300)j,j,ichar(bitmap_array(m+1)),nline
     *                              ,ncharline
                    endif
                    m=m+2
                    nrepblocks=nrepblocks+1
                    nrepchars =nrepchars+j
      endif
      go to 5
c
 60   continue
c
c      Repeat last line if there's an error, then stop
c
                           if(error_msg) stop
                           error_msg=.true.
             write(90,302)(ichar(bitmap_array(mx)),mx=mstart,mend)
                           m=mstart
                           ncharline=0
                           go to 5
c
200   format('End of line:  ',i6,' pixels of',i10,' in line',i6
     *      ,'. Repeat blocks:',i6,', chars:',i6
     *      ,'. Diff blocks:',i6,', chars:',i6
     *      )
201   format('End of bitmap:',i10,' pixels in',i6,' lines')
202   format('Skip found at',i10,' in line',i10,'.  Program stops.')
203   format('Wrong number of pixels in last line - should be', i6)
300   format('Repeat x ',z4,i8,' of',z4,' in line',i8,': Total -',i6)
301   format(z4,i8,' different chars in line',i8,': Total -',i6)
302   format(10000(40z3,/))

c
 99   continue
c
c      Can't open file....
c
      stop
c
100   format(a2,a4,2a2,4a4,2a2,6a4,a1024)
101   format(a1,$)
102   format(1000(8(4(z3.2),' '),/))
103   format(100000000a1)
104   format('***** Line:',i4,', ncout=',i8)
105   format('READBMP3: Compression ratio for image part of ',a,f8.3
     *       ,' - reduction to',i3,'%')
106   format(/'Chars in:',i6,', chars out:',i6
     *      ,', compression ratio: ',f8.3,/)
107   format('Characters in outfile:',i8)
108   format('##### ERROR IN READBMP3: Cannot open ',a)
109   format('READBMP3: Writing header, colour-map to bitmap_array..')
110   format('READBMP3: Reading picture data in to    bitmap_array..')
111   format('READBMP3: Writing                       bitmap_array..')
112   format('READBMP3: ',i8,z8)
113   format('A0 encountered at',i10,'.  IRC=',i4,20z3.2)
114   format(10i10)
115   format('##### EOR ERROR: irc=',i10,', ncount=',i10,', m=',i10)
116   format('##### EOF ERROR: irc=',i10,', ncount=',i10,', m=',i10)
117   format(i10,' records of length',i8)
118   format(z4.2,10i10)
1001  format( 'Resource   ',a8
     *      ,/'lenbfh     ',z8,i10
     *      ,/'xhot       ',z8,i10
     *      ,/'yhot       ',z8,i10
     *      ,/'offbits    ',z8,i10
     *      ,/'lenbmih4   ',z8,i10
     *      ,/'bmwidth    ',z8,i10
     *      ,/'bmheight   ',z8,i10
     *      ,/'nplanes    ',z8,i10
     *      ,/'nbpp       ',z8,i10
     *      ,/'idcompress ',z8,i10
     *      ,/'image_size ',z8,i10
     *      ,/'xppm       ',z8,i10
     *      ,/'yppm       ',z8,i10
     *      ,/'ncolindex  ',z8,i10
     *      ,/'important  ',z8,i10
     *      )
c
      end
