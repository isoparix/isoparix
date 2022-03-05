      subroutine datout(lsource,nel)
c
       use isocomm
c
c      Check the channel!!
c
c     write(*,101)lsource,lchann,nel
      if(lchann+lsource.lt.10
     *  )then
             write(*,100)lsource,lchann,nel
             write(0,100)lsource,lchann,nel
             iso_mpi_term=.true.
      endif
c
c      Write out the received data
c
      write(lsource+lchann)ngrafout
c
      return
c
100   format('***** ERROR IN DATOUT: LSOURCE=',i6,', LCHANN=',i6
     *      ,':  Writing',i8,' elements')
101   format('DATOUT: LSOURCE=',i6,', LCHANN=',i6
     *      ,':  Writing',i8,' elements')
      end
