      subroutine numstrip(txt)
c
c      Strips txt of everything except digits and spaces
c
      character *80 txt,txttmp
      character *1 txta(80)
      equivalence(txttmp,txta)
c
      txttmp=txt
      ntxt=0
      do i=1,80
        if( txta(i).eq.' '.or.
     *      txta(i).eq.'.'.or.
     *     (txta(i).ge.'0'.and.txta(i).le.'9')
     *    )then
               ntxt=ntxt+1
               txta(ntxt)=txta(i)
         endif
      enddo
c
      do i=ntxt,80
         txta(i)=' '
      enddo
c
      txt=txttmp
c
      return
      end
