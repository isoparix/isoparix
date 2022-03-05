      program compress_test
c
c      Supplies and compresses test strings
c
      use bmp_comms
c
      character (1) text(3000)
      character (3000) textb
      equivalence (textb,text)
c
   1  continue
      read(*,100,end=2)text
      ll=len_trim(textb)
      write(*,101)ll,(text(mx),mx=1,ll)
      if(allocated(tmparray))deallocate(tmparray,stat=istat)
      allocate(tmparray(ll*2))
      ncout=0
      call compressor(text,ll,ncout)
      write(*,102)ncout,ll
      go to 1
  2   continue
100   format(3000a1)
101   format(/'##### New line - length:',i5,' ',3000a1)
102   format( '##### New line: ncout=',i6,', length=',i5,' ',3000a1)
      end
