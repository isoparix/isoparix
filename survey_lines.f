      program survey_lines
c
      character(2) line_id
      character(8)taskname,taskheada(20),taskheadb(20),eld_name
c
      integer(4)ntimes(50,20)
c
      character(10) elder_name(20),atimes(20)
c
c      Read in the list of Elders
c
      ncount=1
c
      elder_name=''
      ntask=0
   3  continue
      read(3,*,end=4)line_id
      if(line_id.eq.'##'
     *  )then
             ncount=0
             ntask=ntask+1
             read(3,*,end=4)taskheada(ntask)
             read(3,*,end=4)taskheadb(ntask)
c            write(*,*)taskheada(ntask)
c            write(*,*)taskheadb(ntask)
          else
             ncount=ncount+1
             read(line_id,*,end=4)ntimes(ntask,ncount)
             read(3,*,end=4)eld_name
c
c      Cross-check on the names....
c
             if(trim(eld_name).ne.trim(elder_name(ncount))
     *         )then
                    elder_name(ncount)=eld_name
                    write(*,*)eld_name
              endif
      endif
      go to 3
   4  continue
c
      write(*,101)(taskheada(mx),mx=1,ntask)
      write(*,101)(taskheadb(mx),mx=1,ntask)
      do n=1,ncount
         do mx=1,ntask
            if(ntimes(mx,n).gt.0
     *        )then
                   write(atimes(mx),102)ntimes(mx,n)
               else
                   atimes(mx)='     -    '
            endif
         enddo
         write(*,100)elder_name(n),(atimes(mx),mx=1,ntask)
      enddo
c
      stop
c
 99   continue
      write(0,*)'ERROR in survey_lines.f: no file elder_list_a.txt'
      stop
c
100   format(a12,20a7)
101   format(14x,20a7)
102   format(i6)
c
      end

