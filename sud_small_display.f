      subroutine sud_small_display
c
c      Displays current pattern
c
      use sudoku_comms
c
      character (1) :: letter,canvas(90,10)
c
      logical nfound,nfixed
c
      nc=0
         canvas='X'
      do n=1,9          !     1 2 3 4 5 6 7 8 9
         letter=char(48+n)
         k=(n-1)/3      !     0 0 0 1 1 1 2 2 2
         l= n-(k*3)     !     1 2 3 1 2 3 1 2 3
         k=k+1          !     1 1 1 2 2 2 3 3 3
c
c      Display pattern of possibilities for number n
c
         nfound=.false.
         nfixed=.false.
         do ncol=1,9
            do nrow=1,9
               if(poss(nrow,ncol,k,l)
     *           )then
                      canvas(ncol+nc,nrow)=letter
                      nfound=.true.
                  else
                      canvas(ncol+nc,nrow)='.'
               endif
            enddo
         enddo
c
c      Mark knowns in, in an otherwise clear square
c
         do ncol=1,9
            ncolbegin=1+(3*((ncol-1)/3))
            do nrow=1,9
               nrowbegin=1+(3*((nrow-1)/3))
               if(known(nrow,ncol).eq.n
     *           )then
                      do nrc=nrowbegin,nrowbegin+2
                         do ncc=ncolbegin,ncolbegin+2
                            canvas(ncc+nc,nrc)=' '
                         enddo
                      enddo
                      canvas(ncol+nc,nrow)=letter
               endif
            enddo
         enddo
c
         nc=nc+9
      enddo
      write(*,100)((canvas(mx,my),mx= 1,45),my=1,9)
     *           ,((canvas(mx,my),mx=46,81),my=1,9)
c
100   format(  
     *         3(
     *            5(   '+===+===+===+   '         ),/
     *         3( 5('|',3a1,'|',3a1,'|',3a1,'|   '),/)
     *          ),5(   '+===+===+===+   '         )
     *     ,//
     *         3(
     *            4(   '+===+===+===+   '         ),/
     *         3( 4('|',3a1,'|',3a1,'|',3a1,'|   '),/)
     *          ),4(   '+===+===+===+   '         )
     *      )
c
      return
c
      end
