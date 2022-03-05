      subroutine disp_check
c
c      Displays grid
c
      use sudoku_comms
c
      call sud_display
      write(*,100)
      read(*,*)junk
c
      return
100   format('Enter number to continue')
      end
