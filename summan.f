      program summan
c
c      (was program wgdat)
c
c      Reads xxstat summary files
c
      parameter (nqsamples=1500)
      real (8) allmax (nqsamples)
     *       ,allmean(nqsamples)
     *       ,allmin (nqsamples)
     *       ,average(nqsamples) 
      character (12) attrib 
      character (49) filename(nqsamples),charjunk,title
      character (120) textline
c
c      Unit  8 for the numbers
c      Unit 10 for the histogram details
c
      i=1
      nsys=0
  1   continue
      read(*,*,end=2)filename(i)
      nsys=nsys+1
      read(*,102)textline
      read(textline,207,err=99)
     *      TITLE,ALLMAX(i),ALLMEAN(i),allmin(i),INTSECS,navg
     *     ,average(i)
c
  3   continue
      i=i+1
      go to 1
c
  2   continue
      i=i-1
c
      if(i.gt.0)then
                    write(*,103)i,nsys
                else
                    stop
      endif
c
      write(10,106)title,intsecs,navg,i,nsys
      write( 8,106)title,intsecs,navg,i,nsys
      write( 8,107)
c
      attrib='Average    '
      write(10,1061)attrib,title,intsecs,navg,i
      call stddev(filename,attrib,average,i)
c
      attrib='Highest mn '
      write(10,1061)attrib,title,intsecs,navg,i
      call stddev(filename,attrib,allmean,i)
c
      attrib='Maximum val'
      write(10,1061)attrib,title,intsecs,navg,i
      call stddev(filename,attrib,allmax ,i)
c
      attrib='Minimum val'
      write(10,1061)attrib,title,intsecs,navg,i
      call stddev(filename,attrib,allmin ,i)
c
      write( 8,108)
      stop
c
101   format('******** MESSAGE FROM SUMMAN.EXE AT RECORD'
     *      ,i6,1x,a49,/a120)
102   format(a120)
103   format(i20,' samples collected from',i4,' systems')
106   format('1'
     *    ,//':h1.',a60
     *    ,//'Sampled at intervals of',i4,' seconds'
     *     ,/'Summed or averaged (rates, %) over',i5,' seconds'  
     *     ,/'Number of samples',i6,' (collected from',i4,' systems)'
     *     ,/)
1061  format('.pa'
     *     ,/'1'
     *    ,//':h3.',a12,' - ',a49
     *    ,//'Sampled at intervals of',i4,' seconds'
     *     ,/'Summed (quantities), or averaged (rates,%), over'
     *     ,i5,' seconds'  
     *     ,/'Number of samples',i6
     *     ,/)
107   format(/'.bf gt15'
     *  ,/20x,'           Mean'               
     *       ,'             SD'               
     *       ,'        Minimum'               
     *       ,'        Maximum'               
     *       ,'          Total'               
     *       ,/)                                
108   format('.pf')
207   FORMAT(A49,3e12.5,2i6,e12.5)
2071  FORMAT(A49,3e12.5,2i6)
c
 99   continue
c
c      First, try if it's a No Activity line
c
      read(textline,2071,err=98)
     *      TITLE,ALLMAX(i),ALLMEAN(i),allmin(i),INTSECS,navg
      if(allmax(i).eq.0.0.and.
     *   allmin(i).eq.0.0)then
                              average(i)=0.
                              go to 3
      endif
c
 98   continue
c
c      Definite failure....
c
      write(*,101)i,filename(i),textline
      go to 1
c
      end   
