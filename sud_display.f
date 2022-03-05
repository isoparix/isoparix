      subroutine sud_display
c
c      Displays current pattern
c
      use sudoku_comms
c
      character (1) :: charline(27),letter
c
      nposs=0
      write(*,103)(mx,mx=1,9)
      do nrow=1,9
         do minor_row=1,3
            npos=0
            do ncol=1,9
               do minor_col=1,3
                  npos=npos+1
                  if(known(nrow,ncol).gt.0
     *              )then
                         if(minor_row.eq.1.or.
     *                      minor_row.eq.3
     *                     )then
                                charline(npos)=' '
                            else
                                if(minor_col.eq.1.or.
     *                             minor_col.eq.3
     *                            )then
                                       charline(npos)=' '
                                   else
                                       charline(npos)=
     *                               char(48+known(nrow,ncol))
                                endif
                         endif
                     else
                         if(poss(nrow,ncol
     *                          ,minor_row,minor_col)
     *                     )then
                                nposs=nposs+1
                                charline(npos)=
     *                          char(48+minor_col+(minor*(minor_row-1)))
                            else
                                charline(npos)='.'
                          endif
                  endif
               enddo 
            enddo 
            if(minor_row.eq.2
     *        )then
                   write(*,100)nrow,charline,nrow
               else
                   write(*,105)charline
            endif
         enddo
         if(mod(nrow,3).eq.0
     *     )then
                if(nrow.eq.9
     *            )then
                       write(*,104)(mx,mx=1,9)
                   else
                       write(*,102)
                endif
            else
                write(*,101)
         endif
      enddo 
      write(*,106)nknowns,nposs
c
      if(nknowns+nposs.lt.81
     *  )then
             write(*,107)
             sud_error=.true.
         else
             sud_error=.False.
      endif
c
      return
c
100   format(i3
     *     ,    '   *',3(' ',3a1,' | ',3a1,' | ',3a1,' *'),i3)
101   format('      *',3('-----+-----+-----*'))
102   format('      *',3('=================*'))
103   format(i10,8i6
     *     ,/'      *',3('=================*'))
104   format('      *',3('=================*')
     *    ,//i10,8i6,//)
105   format('      *',3(' ',3a1,' | ',3a1,' | ',3a1,' *'))
c
c
c100   format(i3
c     *     ,    '   *',3(' ',3a1,' | ',3a1,' | ',3a1,' *'),i3)
c101   format('      *',3('     |     |     *')
c     *     ,/'      *',3('-----+-----+-----*')
c     *     ,/'      *',3('     |     |     *'))
c102   format('      *',3('     |     |     *')
c     *     ,/'      *',3('=================*')
c     *     ,/'      *',3('     |     |     *'))
c103   format(i10,8i6
c     *     ,/'TOP'
c     *     ,/'      *',3('=================*')
c     *     ,/'      *',3('     |     |     *'))
c104   format('      *',3('     |     |     *')
c     *     ,/'      *',3('=================*')
c     *    ,//i10,8i6,//)
c105   format('      *',3(' ',3a1,' | ',3a1,' | ',3a1,' *'))
c
106   format('Knowns:',i3,', Possibles:',i4)
107   format('Incorrect guess (or logic failure...) - program ends')
c
      end
