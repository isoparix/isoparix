      subroutine makeline(jside,itest,midface,nc)
c
      use isocomm
c
            do ne=1,nev(jside,nc)-1
               if(number(itest,lvv(lvid(ne  ,jside,nc),nc)).eq.0)then
c
                     call linesegs(lvid(ne  ,jside,nc)
     *                            ,lvid(ne+1,jside,nc)  ,2,nc)
c
                     call linesegs(lvid(ne  ,jside,nc)
     *                            ,lvid(ne+1,jside,nc)  ,midface,nc)
c
                     call linesegs(lvid(ne  ,jside,nc)+1
     *                            ,lvid(ne+1,jside,nc)+1,midface,nc)
c
                     call linesegs(lvid(ne  ,jside,nc)+1
     *                            ,lvid(ne+1,jside,nc)+1,5,nc)
               endif
            enddo
c
100   format('***** MakeLine between vertices',2i3,', JSIDE',i2,'?')
101   format('Vertices',2i3,' J-side',i2,', cube',i3
     *      ,' faces',2i3,' using test',i2)
102   format('Results for cube',i3)
103   format(2(5i8,/))
      return
      end
