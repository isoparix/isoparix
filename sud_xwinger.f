      subroutine xwinger(k,l,direction)
C
      use sudoku_comms        
c
c      Check for X-wings...
c
      character(1) direction ! Co-ordinates r(ows) and c(olumns)
c
      write(*,102)
      do linea=1,8
         if(line_count(linea).eq.2
     *     )then
                la=line_record(linea,1)
                do lineb=linea+1,9
                   if(line_count(lineb).eq.2
     *               )then
c           nsquare=(3*((nrow-1)/3))+((ncol-1)/3)+1
                          lb=line_record(lineb,2)
                          if(la.eq.line_record(lineb,1).and.
     *                       lb.eq.line_record(linea,2)
     *                      )then
                                 if(
     *                       (3*(linea-1)/3)+((la-1)/3).ne.
     *                       (3*(lineb-1)/3)+((lb-1)/3)
     *                             )then
c
c      We have an X-wing!
c
      do my=1,8
         write(*,101)line_count(my),direction,my
     *             ,(line_record(my,mx),mx=1,line_count(my))
      enddo
                   write(*,100)3*(k-1)+l
     *                        ,linea,line_record(linea,1)
     *                        ,lineb,line_record(lineb,2)
                   if(check)call disp_check
                   do linec=1,9
                      if(linec.ne.linea.and.
     *                   linec.ne.lineb
     *                  )then
                             if(direction.eq.'r'
     *                         )then
                                    if(known(linec,la).eq.0
     *                                )then
                                           poss(linec,la,k,l)=.false.
                                    endif
                                    if(known(linec,lb).eq.0
     *                                )then
                                           poss(linec,lb,k,l)=.false.
                                    endif
                                else
                                    if(known(la,linec).eq.0
     *                                )then
                                           poss(la,linec,k,l)=.false.
                                    endif
                                    if(known(lb,linec).eq.0
     *                                )then
                                           poss(lb,linec,k,l)=.false.
                                    endif
                             endif
                      endif
                   enddo
c
                                    else
                               write(*,103)1+(3*(lineb-1)/3)+((lb-1)/3)
     *                               ,linea,lineb,la,lb
                                        return
                                 endif ! 3*((....
                                 endif ! la.eq.line_record....
                   endif ! line_count(lineb)
                enddo  ! lineb
         endif  ! line_count(linea)
      enddo  ! linea
c
      return
c
100   format('X-wing in',i2,' found at',2(i4,':',i1))
101   format('XWINGER count',i2,', ',a1,i1,':',10i3)
102   format(/'XWING CHECK',/)
103   format('NOT an X-wing - all in same square',5i4)
c
      end
