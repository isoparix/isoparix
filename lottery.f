      program test_random
c
c      Program to generate:
c           six random numbers between 1 and 59 for Lotto
c           five    "    "       "   ` 1  "  50  "  Euro
c           twp     "    "       "     1  "  12  "  Euro bonus
c
      use dacomm
c
c      Check that nblks is a suitable value for random number generation
c
      integer(8)nblks,nextrnd,nlott(6),neuro(6),nbonus(6)
c
  1   continue
      read(7,*)nblks     ! Seed..
  4   continue
      if(nblks.le.0)stop
      call kxgen(nblks,irc)
      if(irc.ne.0)go to 4
c
      nr=nblks/2
      write(*,*)nr,nblks
      call generate(nlott, 59)
      call generate(neuro, 50)
      call generate(nbonus,12)
      write(*,101)nlott(1:6),neuro(2:6),nbonus(5:6)
c
101   format(/'Lottery:'6i3,//' Euro_5:',5i3,//' Euro_2:',2i3)
c
      end
c
c
c
      subroutine generate(numbers,numbersmax)
c
      use dacomm
c
      integer(8)numbers(6)
c
      nn=0
      do n=1,nrange
         nr=nextrnd()
         if(nr.le.numbersmax.and.nr.gt.0
     *     )then
                nn=nn+1
                numbers(nn)=nr
                if(nn.eq.6)exit
         endif
      enddo
c
      return
      end
