      subroutine open_files
c
c      Opens work files for the analysis programs
c
      use allcomms
      character (250) statname
c
      n_htm_refs=0
      list_depth=1
c
      read(*,*,end=99,err=98)statname
c
      write(*,*)'Output files: '//trim(statname)//'.*'
c
      open( 8,form='formatted',status='unknown'
     *                   ,file=trim(statname)//'.pred')
      open(10,form='formatted',status='unknown'
     *                   ,file=trim(statname)//'.htm')
      open(12,form='formatted',status='unknown'
     *                   ,file=trim(statname)//'.summary')
      open(14,form='formatted',status='unknown'
     *                   ,file=trim(statname)//'.lsquare')
      open(16,form='formatted',status='unknown'
     *                   ,file=trim(statname)//'.tmp')
      open(18,form='formatted',status='unknown'
     *                   ,file=trim(statname)//'.timedat')
      open(20,form='formatted',status='unknown'
     *                   ,file=trim(statname)//'.samplims')
c
      return
c
 98   continue
      write(*,100)
      stop
c
 99   continue
      write(*,101)
      stop
c
100   format('##### ERROR IN OPEN_FILES - CANNNOT READ STATNAME #####')
101   format('##### ERROR IN OPEN_FILES - EOF READING STATNAME #####')
c
      end
