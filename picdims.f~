      subroutine picdims(pictype,ixdim,iydim)
c
c      Get picture dimensions from stream data in fort.7
c
      character(1)txt(200)
      character(4)pictype
      character(200)txtdata
c
      equivalence(txt,txtdata)
c
cIMG_2164.jpg JPEG 2304x3072 2304x3072+0+0 DirectClass 8-bit 3.23167mb 1.232u 0:02
ctest.cr2=>C:/Users/John/AppData/Local/Temp/magick-9RCiuuCC.ppm CR2 3684x2760 3684x2760+0+0 16-bit DirectClass 61.01MB
cJPEG 2304x3072 2304x3072+0+0 DirectClass 8-bit 3.23167mb 1.232u 0:02
c CR2 3684x2760 3684x2760+0+0 16-bit DirectClass 61.01MB
      read(7,109,iostat=ios)txt
      do i=1,200
         pictype=txt(i)//txt(i+1)//txt(i+2)//txt(i+3)
         if(pictype.eq.'JPEG'.or.
     *      pictype.eq.' CR2'.or.
     *      pictype.eq.' GIF'.or.
     *      pictype.eq.' BMP'
     *     )then 
                iref=i
                exit
         endif
      enddo
c
      ntxt=1
      do n=iref,200
         txt(ntxt)=txt(n)
         if(ntxt.gt.5
     *     )then
                if(txt(n).eq.' '
     *            )then
                       txt(ntxt:200)=' '
                       exit
                endif
                if(txt(ntxt).eq.'x'
     *            )then
                       txt(ntxt)=' '
                endif
         endif
         ntxt=ntxt+1
      enddo
c
      read(txtdata,*,err=99)pictype,ixdim,iydim
c
      close(7)
      return
c
 99   continue
      write(0,100)
      stop
c
100   format('PICDIMS: Error reading picture type and dimsnsions')
109   format(200a1)
c
      end
