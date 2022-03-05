      program dofor
c      
c      Changes do to for loop
c      
      character(1) txt(80)
      character(80) txtline
      equivalence (txt,txtline)
c
      character(50)lead_space,var_name,expr1,expr2,expr3
c
      logical do_exist
c
      do_exist=.false.
      txtline=''
      open(4,file='tmp.php',status='unknown',form='formatted')
  1   continue
      read(*,100,end=99)txtline
             ne_b=len_trim(txtline)
      do n=1,ne_b-3
         if(txtline(n:n+3) .eq.' do '
     *     )then
                do_exist=.true.
                index_a=n+4
                exit
         endif
      enddo
c
      if(do_exist
     *  )then
             do n=1+index_a,80
                if(txt(n).eq.'='
     *            )then
                       index_b=n-1
                       ns_a=n+1
                       exit
                endif
             enddo
             var_name=txtline(index_a:index_b)
c
             do n=1+ns_a,80
                if(txt(n).eq.','
     *            )then
                       ns_b=n-1
                       ne_a=n+1
                       exit
                endif
             enddo
             expr1=txtline(ns_a:ns_b)
             expr2=txtline(ne_a:ne_b)
c
             do_exist=.false.
             write(4,101)txtline(1:index_a-4),trim(var_name),trim(expr1)
     *                  ,trim(var_name),trim(expr2),trim(var_name)
             txtline=''
         else
             write(4,*)trim(txtline)
      endif
c
      go to 1
 99   stop
100   format(a80)
101   format(a,'for Q ',a,'=',a,'; ',a,'<=',a,'; ',a,'++; Q {')
      end
