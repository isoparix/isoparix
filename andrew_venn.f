      program andrew_venn
c
c      Associate an area on a grid with an individual circle
c      Repeat for as many times as we have circles
c      Add 1 for each time a grid point is inside a circle
c      Create polygons from equal value edges
c      Convert polygon edges from x,y pairs to long/lat pairs
c
      use venn_comms
      use bmp_comms
c
      implicit real(8) (a-h,o-z)
c
      logical(1) intext
c
      call xxstatmap
c
      call ge_input
cc
      call gridmap
cc
      call system('ls -l '//trim(title)//'*.*')
cc
       stop
100   format('ANDREW_VENN: ncentres=',i6,' for poly type',i3)
       end
