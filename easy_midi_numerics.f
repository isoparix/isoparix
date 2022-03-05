      subroutine easy_midi_numerics(j,k,l)
c
c      Check that characters text(J) to text(k) are numeric
c      Returns the value in l
c
      use easy_midi_comms
c
      character *3 numtext
c
      logical non_numeric
c
      non_numeric=.false.
      do ix=j,k
         if(ichar(text(ix)).lt.48.or.
     *      ichar(text(ix)).gt.57
     *     )then
                non_numeric=.true.
                nn=ix
                exit
         endif
      enddo
c
      if(non_numeric
     *  )then
             if(nn.eq.1    ! No numerics at all!
     *         )then
                    write(0,100)k-j+1,(text(mx),mx=j,k)
                    stop
                else
                    k=nn-1
             endif
         else
             m=k-j+1
c
             if(m.eq.1
     *         )then
                    numtext='  '//text(j)
             endif
c
             if(m.eq.2
     *         )then
                    numtext=' '//text(j)//text(j+1)
             endif
c
             if(m.eq.3
     *         )then
                    numtext=text(j)//text(j+1)//text(j+2)
             endif
c
             read(numtext,103)l
             if(check)write(*,104)numtext,j,k,m,l
c
             return
      endif
c
100   format('EASY_MIDI_NUMERICS - ERROR: Bad digits.  Should be',i2
     *      ,' numbers - found this ',6a1)
103   format(i3)
104   format('EASY_MIDI_NUMERICS:',a6,3i4,'-digit number is',i4)
c
      end
