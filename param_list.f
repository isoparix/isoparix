      subroutine param_list
c
c      Writes out the list of parameters passed between iso*.exe
c
      use isocomm
c
      character (10) param_file_name
c
      write(param_file_name,101)taskid
c     open(80,file=param_file_name,status='unknown',form='formatted')
      write(lchann,102)lchann
      do i =1,4
         write(lchann,100)i,lchann-10,params(i)
      enddo
      do i =5,28
         write(lchann,103)int(params(i)),i,lchann-10
      enddo
c
      call isoflush(lchann)
c     close(80)
      return
c
100   format('Parameter',i4,' in process',i4,': ',e26.17)
101   format('param_',i4.4)
102   format(/'##### From LCHANN=',i3,' #####',/)
103   format(i8,' Parameter',i4,' in process',i4)
      end

