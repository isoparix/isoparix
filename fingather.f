      program fingather
c
c      Gathers sorted Bank, GoldCard and Amex outputs from finanal
c
      implicit real(8) (a-h,o-z)      
      character(180) line_text,line_text_old
      character(1), dimension (180) :: line_texta,line_textb
      character(14)date
      character(3)category
      character(8)gather_name, gather_name_old,dummy_item
      character(20)category_name(50)
c
      dimension idquote(20),idcomma(20)
c
      equivalence(line_text,line_textb)
      equivalence(line_texta,line_text_old)
c     equivalence(line_texta(1:8),dummy_item)
      equivalence(line_texta     ,dummy_item)
c      
      line_text_old=''
      gather_name_old=''
c
      open(3,file='catlist.txt', status='old',err=99)
      ncat=1
      category_name=''
  3   continue
      read(3,111,end=4)category_name(ncat)
      write(14,113)ncat,category_name(ncat)
      ncat=ncat+1
      go to 3
  4   continue
c
      items=0
      sum=0.
      write(*,106)
  1   continue
      read(1,101,end=2,err=2)date,category,value,line_text
      read(line_text,107)gather_name
c     write(*,*)gather_name
c     write(*,101)      date,category,value,trim(line_text)
      if(gather_name.eq.gather_name_old
     *  )then
             items=items+1
             sum=sum+value
             write(4,105)value,trim(line_text)
         else
c             
             if(items.eq.1
     *         )then
                    write(8,105)sum,trim(line_text_old)
                    write(12,109)trim(line_text_old)
c                   read(*,*)nc
c                   if(nc.gt.0.and.nc.le.ncat
c    *                )then
c                          write(20,112)dummy_item
c    *                                ,trim(category_name(nc))
c    *                                ,dummy_item
c                   endif
             endif
c             
             if(items.gt.1
     *         )then
                    write(4,104)items,sum/float(items),sum
                    write(8,104)items,sum/float(items),sum
     *                         ,trim(line_text_old)
             endif
c             
             write(4,106)
             line_text_old=line_text
             gather_name_old=gather_name
             items=1
             sum=value
             write(4,105)value,trim(line_text)
      endif
c      
c      
      go to 1
  2   continue
c             
             if(items.eq.1
     *         )then
                    write(8,105)sum,trim(line_text_old)
             endif
c             
             if(items.gt.1
     *         )then
                    write(4,104)items,sum/float(items),sum
                    write(8,104)items,sum/float(items),sum
     *                         ,trim(line_text_old)
             endif
c             
      stop      
c      
 99   continue
      write(0,108)
      stop
c      
100   format(a180)
101   format(a14,a4,f12.2,2x,a)
102   format(49x,10a1)
103   format(f10.2)
104   format(i5,2f11.2,2x,a)
105   format(f27.2,2x,a)
106   format(/)
107   format(a8)
108   format(/'ERROR in fingather.f:  File catlist.txt NOT FOUND',/)
109   format(a)
111   format(a20)
112   format('%s/',a,'/',a,' ',a,'/')
113   format(i8,' ',a20)
c
      end      
