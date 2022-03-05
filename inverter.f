      program inverter
c
c      Takes the summary file, and divides the Single Job Time
c      into the file size supplied at run-time
c
      character (8) norm_resp
c
      sjt1=-1.
      write(*,104)
      read(*,*)filesize
      write(*,102)
      read(3,103)
  1   continue
      read(3,100,end=2)jobdens,sjt
      if(jobdens.eq.1
     *  )then
             sjt1=sjt
      endif
      datarate=filesize/sjt
      rdens=1./float(jobdens)
      if(sjt1.gt.0.
     *  )then
             write(norm_resp,105)float(jobdens)*sjt/sjt1
         else
             norm_resp='    -   '
      endif
      write(*,101)jobdens,datarate,sjt,rdens,norm_resp
      go to 1
  2   continue
      stop
c
c      Allow some space for text at the start of each data line in 100
c
100   format(9x,i3,36x,f12.3)
101   format(i12,3f12.3,5x,a8)
102   format(/' Job density   Data rate         SJT        1/JD'
     *       ,' NormRespTime',/)
103   format(/////)
104   format('Please enter the file size used')
105   format(f8.3)
c
      end
