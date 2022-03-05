      subroutine param_list
c
c      Writes out the list of parameters passed between iso*.exe
c
      use isocomm
c
      character (10) param_file_name
c
      write(param_file_name,101)taskid
      open(80,file=param_file_name,status='unknown',form='formatted')
      write(80,102)lchann
      do i =1,40
         write(80,100)i,lchann-10,params(i)
      enddo
c
      call isoflush(80)
      close(80)
      return
c
100   format('Parameter',i4,' in process',i4,': ',e26.17)
101   format('param_',i4.4)
102   format(/'##### From LCHANN=',i3,' #####',/)
      end

