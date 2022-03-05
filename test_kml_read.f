      program test_kml_read
c
c      Reads a 'native KML file
c
      character(1) intext(2000),tab
      character(2000) textb
      equivalence(intext,textb)
c
      tab=char(9)
  1   continue
      intext=' '
      read(*,100,end=2,iostat=irc)intext
      do n=1,len_trim(textb)
         if(intext(n).eq.tab
     *     )then
                intext(n)=' '
         endif
      enddo
      textb=adjustl(textb)
      do n=1,len_trim(textb)
         if(intext(n).eq.' '
     *     )then
                intext(n)=char(10)
         endif
      enddo
      write(*,*)trim(textb)

      go to 1
  2   continue
      stop
100   format(2000a1)
101   format(10i6)
      end
