      subroutine array2bmp(ixbm,iybm,inparray)
c
c     Uses the data in an array to create a bitmap
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
      use bmp_comms
c
      logical compress			!8-bit RLE, or not?
c
      integer (4) tmparray_size
      integer (4) ixbm			!X-dimension of picture
      integer (4) iybm			!Y-dimension of picture
      character (1) inparray(ixbm,iybm)	!Array of pixel values
      character (34) filename
c
      integer (2) exintel2
      integer (4) exintel4
c
      character (2) adintel2
      character (4) adintel4
c
      character (1) header_array(1078),char_one,char_zero
      character (1078) header_chars
      equivalence(header_chars,header_array)
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
c     lchann=10
c     open(lchann,file=trim(bmname)//'_bmp.log',status='unknown'
c    *           ,form='formatted')
c
      if(.not.allocated(tmparray)
     *  )then
c            tmparray_size=2147483000
             tmparray_size=ixbm*iybm*2
             allocate(tmparray(tmparray_size),stat=ierror)
             mx=size(tmparray)
             if(mx.ne.tmparray_size
     *         )then
                    write(0,147)mx,tmparray_size,ierror
                    return
             endif
      endif
c
      char_one =char(1)
      char_zero=char(0)
c
c ###### BITMAPINFOHEADER2 END
c
c      Compress the image data without writing it...
c
      ncout=0
      nsambuckets=0
      ndifbuckets=0
      do iy=iybm,1,-1
         call compressor(inparray(:,iy),ixbm,ncout)
c        write(8,112)iy,iybm-iy+1,nsambuckets,ndifbuckets
         nsambuckets=0
         ndifbuckets=0
      enddo
c
c      Add on two bytes to represent 'end-of-bitmap'?
c
      ncout=ncout+1
      tmparray(ncout)=char_zero
      ncout=ncout+1
      tmparray(ncout)=char_one
c
c      This is going to be a compressed 256-colour bitmap, 
c      so we can set some details 'up front'
c
      resource      ='BM'
c
      int_lenbfh    =1078+ncout
      int_xhot      =0
      int_yhot      =0
      int_offbits   =1078
      int_lenbmih2  =40
      int_bmwidth   =ixbm
      int_bmheight  =iybm
      int_nplanes   =1
      int_nbpp      =8
      int_idcompress=1
      int_image_size=ncout
      int_xppm      =0
      int_yppm      =0
      int_ncolindex =256
      int_important =0
c
!#ifdef cygwin
      continue
!#elif defined _AIX
!      lenbfh    =adintel4(int_lenbfh)
!      xhot      =adintel2(int_xhot)
!      yhot      =adintel2(int_yhot)
!      offbits   =adintel4(int_offbits)
!      lenbmih2  =adintel4(int_lenbmih2)
!      bmwidth   =adintel4(int_bmwidth)
!      bmheight  =adintel4(int_bmheight)
!      nplanes   =adintel2(int_nplanes)
!      nbpp      =adintel2(int_nbpp)
!      idcompress=adintel4(int_idcompress)
!      image_size=adintel4(int_image_size)
!      xppm      =adintel4(int_xppm)
!      yppm      =adintel4(int_yppm)
!      ncolindex =adintel4(int_ncolindex)
!      important =adintel4(int_important)
!#else
!Error Onknown operating system
!#endif
c
c     write(lchann,107)int_lenbfh
c
c      Write out header info
c
c     write(lchann,1001)
c    *            resource       ! 0000 - 0001
c    *           ,int_lenbfh         ! 0002 - 0005
c    *           ,int_xhot           ! 0006 - 0007
c    *           ,int_yhot           ! 0008 - 0009
c    *           ,int_offbits        ! 000A - 000D
c    *           ,int_lenbmih2       ! 000E - 0011
c    *           ,int_bmwidth        ! 0012 - 0015
c    *           ,int_bmheight       ! 0016 - 0019
c    *           ,int_nplanes        ! 001A - 001B
c    *           ,int_nbpp           ! 001C - 001D
c    *           ,int_idcompress     ! 001E - 0021
c    *           ,int_image_size     ! 0022 - 0025
c    *           ,int_xppm           ! 0026 - 0029
c    *           ,int_yppm           ! 002A - 002D
c    *           ,int_ncolindex      ! 002E - 0031
c    *           ,int_important      ! 0032 - 0035
c    *           ,rgbquad        ! 0033 - 
c
      write(header_chars,100)
     *            resource
     *           ,lenbfh
     *           ,xhot
     *           ,yhot
     *           ,offbits
     *           ,lenbmih2
     *           ,bmwidth
     *           ,bmheight
     *           ,nplanes
     *           ,nbpp
     *           ,idcompress
     *           ,image_size
     *           ,xppm
     *           ,yppm
     *           ,ncolindex
     *           ,important,rgbquad
c
c      Write out the picture data
c
      filename=trim(bmname)//'.bmp'
      open(40,file=filename
     *    ,form='formatted',status='replace',err=99)
c
      do i=1,1078
         write(40,101)header_array(i)
      enddo
c
      do n=1,ncout
         write(40,101)tmparray(n)
      enddo
c
      close(40)
c
      comp_ratio=real(ixbm*iybm)/real(ncout)
c     write(lchann,105)trim(adjustl(filename)),comp_ratio
c    *                           ,int(.5+(100./comp_ratio))
      write( *,105)trim(adjustl(filename)),comp_ratio
     *                       ,int(.5+(100./comp_ratio))
c     close(lchann)
c     call isoflush(lchann)
      call isoflush(40)
c
      deallocate(tmparray)
c
      return
c
 99   continue
c
c      Cannot open file....
c
      write(*,108)trim(bmname),filename
      return
c
100   format(a2,a4,2a2,4a4,2a2,6a4,a1024)
1001  format( 'Resource   ',a8
     *      ,/'lenbfh     ',z8
     *      ,/'xhot       ',z8
     *      ,/'yhot       ',z8
     *      ,/'offbits    ',z8
     *      ,/'lenbmih4   ',z8
     *      ,/'bmwidth    ',z8
     *      ,/'bmheight   ',z8
     *      ,/'nplanes    ',z8
     *      ,/'nbpp       ',z8
     *      ,/'idcompress ',z8
     *      ,/'image_size ',z8
     *      ,/'xppm       ',z8
     *      ,/'yppm       ',z8
     *      ,/'ncolindex  ',z8
     *      ,/'important  ',z8
     *      ,/'Colour map ',/300(4z4)
     *      )
101   format(a1,$)
104   format('***** Line:',i4,', ncout=',i8)
105   format('ARRAY2BMP: Compression ratio for image part of ',a,f8.3
     *       ,' - reduction to',i4,'%')
106   format(/'Chars in:',i6,', chars out:',i6
     *      ,', compression ratio: ',f8.3,/)
107   format('Characters in outfile:',i12)
108   format('ERROR IN ARRAY2BMP: Cannot use ',a,' as ',a)
109   format('ARRAY2BMP: Writing header, colour-map to bitmap_array..')
110   format('ARRAY2BMP: Reading picture data in to    bitmap_array..')
111   format('ARRAY2BMP: Writing                       bitmap_array..')
112   format('ARRAY2BMP: Completed line',2i6,' with'
     *       ,i8,' same buckets and',i8,' diff buckets')
147   format('ARRAY2BMP: Array is',i14,', should be',i14
     *      ,'.  Error code:',i4)
c
      end
