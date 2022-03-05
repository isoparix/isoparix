      program river
c
c       Servant, dog, father, two sons, mother two daughters
c       Dog must be on its own, or with servant
c       Daughters must never be with father, without mother present
c            Sons must never be with mother, without father present
c
      integer f,m,d1,d2,s1,s2,st,dg
c
      character *56 east, west
c
      logical group(0:255)
c
      group=.true.
c
      n=0
      im=255
      do  f=1,0,-1
         do  m=1,0,-1
            do s1=1,0,-1
               do s2=1,0,-1
                  do d1=1,0,-1
                     do d2=1,0,-1
                        do st=1,0,-1
                           do dg=1,0,-1
                              if(
     *                           ((d1.eq.f.and.d1.ne.m).or.
     *                            (d2.eq.f.and.d2.ne.m).or.
     *                            (s1.eq.m.and.s1.ne.f).or.
     *                            (s2.eq.m.and.s2.ne.f)
     *                           )
     *                                  .or.
     *                            ((dg.eq. m.or.
     *                              dg.eq. f.or.
     *                              dg.eq.s1.or.
     *                              dg.eq.s2.or.
     *                              dg.eq.d1.or.
     *                              dg.eq.d2).and.dg.ne.st
     *                             )
     *                           )then
                                      group(im)=.false.
                                  else
                                      n=n+1
c
                                      east=""
                                      west=""
c
                                      if( f.eq.1)east=trim(east)
     *                                           //'Father,'
                                      if( m.eq.1)east=trim(east)
     *                                           //'Mother,'
                                      if(d1.eq.1)east=trim(east)
     *                                           //'Daughter1,'
                                      if(d2.eq.1)east=trim(east)
     *                                           //'Daughter2,'
                                      if(s1.eq.1)east=trim(east)
     *                                           //'Son1,'
                                      if(s2.eq.1)east=trim(east)
     *                                           //'Son2,'
                                      if(st.eq.1)east=trim(east)
     *                                           //'Servant,'
                                      if(dg.eq.1)east=trim(east)
     *                                           //'Dog'
c
                                      if( f.eq.0)west=trim(west)
     *                                           //'Father,'
                                      if( m.eq.0)west=trim(west)
     *                                           //'Mother,'
                                      if(d1.eq.0)west=trim(west)
     *                                           //'Daughter1,'
                                      if(d2.eq.0)west=trim(west)
     *                                           //'Daughter2,'
                                      if(s1.eq.0)west=trim(west)
     *                                           //'Son1,'
                                      if(s2.eq.0)west=trim(west)
     *                                           //'Son2,'
                                      if(st.eq.0)west=trim(west)
     *                                           //'Servant, '
                                      if(dg.eq.0)west=trim(west)
     *                                           //'Dog'
c
c                                     write(*,100)n,im,f,m,d1,d2
c    *                                           ,s1,s2,st,dg,west,east
                                      write(*,100)west,east
                              endif
                              im=im-1
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
      enddo
c
      stop
c
100   format(' West: ',a,' East: ',a)
c
      end
