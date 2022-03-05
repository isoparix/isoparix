      character (6) function opsys_name()
c
c      Returns OS name
c
      call system('uname > fort.200')
      read(200,*)opsys_name
      call system('rm -f fort.200')
      return
c
      end
