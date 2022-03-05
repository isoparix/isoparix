      subroutine dasread(filename,nfirst,nblks,nstep,nsize,txfer,dr)
c
c      Reads the data by direct access in sequential fashion
c
      integer (8) nfirst,nblks,nstep,ntotal
c
      real (8) ta,tb,tstart,tend,topen,tclose,txfer
c
      character (1) dadata(nsize)
      character (50) filename
c
      ntotal=0
      call tim(tstart)
      do i=nfirst,nblks,nstep
         ntotal=ntotal+1
         read(3,rec=i)dadata
      enddo
      call tim(tend)
c
      call da_txfer(tstart,tend,ntotal,nsize,txfer,dr)
c
      return
      end      
