      subroutine rgbindex(nred,ngreen,nblue)
c
c      icolour: -1 starts at zero, goes up
c      icolour: -2 starts at 255, goes down 
c      icolour: +n set at n throughout
C
      use bmp_comms
c
      character(1)rgb(0:1023)
      character(1024)rgbq
c
      equivalence(rgb,rgbq)
c     
      write(*,*)nred,ngreen,nblue
c #########################################   RED ################ 
c
      idr=0
c
      if(nred.eq.-1
     *  )then
             nred=0
             idr=1
      endif
c
      if(nred.eq.-2
     *  )then
             nred=255
             idr=-1
      endif
c
c ######################################### GREEN ################ 
c
      idg=0
c
      if(ngreen.eq.-1
     *  )then
             ngreen=0
             idg=1
      endif
c
      if(ngreen.eq.-2
     *  )then
             ngreen=255
             idg=-1
      endif
c
c #########################################  BLUE ################ 
c
      idb=0
c
      if(nblue.eq.-1
     *  )then
             nblue=0
             idb=1
      endif
c
      if(nblue.eq.-2
     *  )then
             nblue=255
             idb=-1
      endif
c
      write(*,100)nred,idr,ngreen,idg,nblue,idb
      rgb(0:3)=char(0)
      do n=0,255
         m=n*4
         rgb(m  )=char(nred)
         rgb(m+1)=char(ngreen)
         rgb(m+2)=char(nblue)
         rgb(m+3)=char(0)
c        write(*,100)n,nred,ngreen,nblue
         nred  =nred  +idr
         ngreen=ngreen+idg
         nblue =nblue +idb
      enddo
      rgbquad=rgbq
c
      return
c
100   format('  Red: Start/increment=',2i6
     *     ,/'Green: Start/increment=',2i6
     *     ,/' Blue: Start/increment=',2i6
     *      )
c
      end
