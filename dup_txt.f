      program dup_txt
c
c   Duplicates characters for edit script
c      
      character(16)text
c      
  1   continue
      text=""
      read (*,100,end=2)text
      write(*,101)text,text
      go to 1
  2   continue
      stop
c
100   format(a16)
101   format("%s/",a16,"/X: eg ",a16,"/")    
c
      end
