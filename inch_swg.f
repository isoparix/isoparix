      program inchswg
c
c      Calculates ID of section with OD in inches and
c      wall thickness in SWG
c
c      From:   http://www.clag.org.uk/swg.html
c
      implicit real(8) (a-h,o-z)
      dimension swg(31)
c
      swg( 1)=0.300
      swg( 2)=0.276
      swg( 3)=0.252
      swg( 4)=0.232
      swg( 5)=0.212
      swg( 6)=0.192
      swg( 7)=0.176
      swg( 8)=0.160
      swg( 9)=0.144
      swg(10)=0.128
      swg(11)=0.116
      swg(12)=0.10
      swg(13)=0.092
      swg(14)=0.080
      swg(15)=0.072
      swg(16)=0.064
      swg(17)=0.056
      swg(18)=0.048
      swg(19)=0.040
      swg(20)=0.036
      swg(21)=0.032
      swg(22)=0.028
      swg(23)=0.024
      swg(24)=0.022
      swg(25)=0.020
      swg(26)=0.018
      swg(27)=0.0164
      swg(28)=0.0148
      swg(29)=0.0136
      swg(30)=0.0124
      swg(31)=.0116
c
      nres=8
      write(*,105)
      do inch=0,4
         do nfraction=0,nres-1
            do id_swg=16,10,-6
               twt=2.0*swg(id_swg)    !  inches
               section_od=float(inch)+float(nfraction)/float(nres)
               section_id=section_od-twt   !  inches
               smm_od=section_od*25.4 
               smm_id=section_id*25.4
               twt_mm=twt*25.4
               nd=nres
               nn=nfraction
   1           continue
               nd_old=nd
               if(mod(nd,2).eq.0.and.mod(nn,2).eq.0
     *           )then
                      nd=nd/2
                      nn=nn/2
                      go to 1
               endif
c
               if(smm_id.le.0.)go to 2
               if(nfraction.eq.0
     *           )then
                      if(id_swg.eq.16
     *                  )then
                             write(*,105)
                             write(*,106)inch,smm_od,id_swg
     *                                   ,section_id,smm_id
                         else
                             write(*,109)id_swg
     *                                   ,section_id,smm_id
     *                                   ,inch,smm_od
                      endif
                  else
                      if(inch.eq.0
     *                  )then
                             if(id_swg.eq.16
     *                         )then
                                    write(*,107)nn,nd,smm_od
     *                                         ,id_swg,section_id,smm_id
                                else
                                    write(*,110)id_swg,section_id,smm_id
     *                                         ,nn,nd,smm_od
                             endif
                         else
                             if(id_swg.eq.16
     *                         )then
                                    write(*,103)inch,nn,nd,smm_od
     *                                    ,id_swg,section_id,smm_id
                                else
                                    write(*,111)id_swg,section_id,smm_id
     *                                         ,inch,nn,nd,smm_od
                             endif
                      endif
               endif
  2            continue
            enddo
c           write(*,108)
         enddo
      enddo
c
100   format('Section OD in decimal inches?')
101   format('Section wall in integer SWG?')
102   format(40x,/'SWG=',i3,', SWG (inches)=',f6.3
     *      ,'  Total wall thickness (inches)=',f6.3,', (mm)=',f6.3
     *     ,/'      Section OD (inches)=',f7.3
     *     ,/'      Section ID (inches)=',f7.3
     *     ,/'      Section OD (mm)    =',f7.3
     *     ,/'      Section ID (mm)    =',f7.3
     *     ,/)
103   format(i3,'-',i1,'/',i1,'"',f8.2,i4,2f8.2)
104   format(16x,i4,2f8.2)
105   format(/'  Outside width        Inside width'
     *      ,/'Imperial  Metric SWG Imperial  Metric',/)
106   format(i3,'"    ',f8.2,i4,2f8.2)
107   format(i5,'/',i1,'"',f8.2,i4,2f8.2)
108   format(/)
109   format(i20,2f8.2,i3,'"    ',f8.2)
110   format(i20,2f8.2,i5,'/',i1,'"',f8.2)
111   format(i20,2f8.2,i3,'-',i1,'/',i1,'"',f8.2)
c
      end
