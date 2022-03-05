      program msetview
c      
c      Takes mset array and views it, by individual colour
c      
      use isocomm
      use bmp_comms
c
      character(1)rgb(0:1023)
      character(4)idfile
      character(8)filename
c      
      call rgbindex(-1,-1,-1)
c      
c     rgb=char(253)
      mx=43
      m=mx*4
      rgb(m)=char(250)
c      
      read(*,*)idfile
      filename=idfile//'.off'
      write(*,*)filename
      open(62,file=filename,status='unknown'
     *    ,form='unformatted')
      read(62)ixm,iym,limit
      write(*,*)ixm,iym,limit
c      
      if(allocated(paint))deallocate(paint)
      allocate(paint(0:limit+1),stat=ierror)
c      
      if(allocated(mset))deallocate(mset)
      allocate(mset(0:ixm+1,0:iym+1),stat=ierror)
c      
      if(allocated(mapdata))deallocate(mapdata)
      allocate(mapdata(ixm,iym),stat=ierror)
c      
      read(62)rgb
      read(62)paint
      read(62)mset
      read(62)mapdata
      close(62)
c
      bmname="43"
c     mset(ixm+1,iym+1)=mset(ixm,iym)   !  How did this get to be -999?
      msetmax=maxval(mset)
      msetmin=minval(mset)
      write(*,100)ixm,iym,msetmax,msetmin,limit
c
      call array2bmp(ixm,iym,mapdata)
c
      stop
c
100   format('Original matrix:',2i6,', max',i8,', min',i8
     *      ,', limit',i8)
c      
      end
c      
