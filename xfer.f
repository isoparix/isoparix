      npolygon=0
      do ndepth=maxdepth,1,-1
         nx=1
         do while (nx.gt.0)
            nplist=0
            nx=0
            new_poly=.false.
            polyactive=.true.
            cp=0.0   !  Dummy initial centre point
            do iyq=nylow,nytop
               if(polyactive
     *           )then
                      if(nplist.gt.0)polyactive=.false.
                      cpdiff=10000000.  ! Centre-point difference
                      found_segment=.false.
                      do ix=1,zcount(iyq)
                         if(zr(ix,iyq).eq.zl(ix+1,iyq).and.
     *                      zr(ix,iyq).eq.ndepth   !  Match depth
     *                     )then
                                if(nplist.gt.0
     *                            )then
                                       new_poly=polytest(
     *                                     xleft (nplist),zg(ix,  iyq)
     *                                    ,xright(nplist),zg(ix+1,iyq)
     *                                                  )
                                 endif
c
                                 if(.not.new_poly
     *                             )then ! This is a candidate
                                        found_segment=.true.
                                        cpn=.5*(zg(ix+1,iyq)+zg(ix,iyq))
                                        cpd=(cpn-cp)**2
                                        if(cpd.lt.cpdiff
     *                                    )then
                                               ixa=ix
                                               cp=cpn
                                               cpdiff=cpd
                                        endif
                                 endif
                         endif !  General conditions on zone
                      enddo  ! IX...
c
                      if(found_segment
     *                  )then
                             nplist=nplist+1
                             xleft (nplist)=zg(ixa,  iyq)
                             xright(nplist)=zg(ixa+1,iyq)
                             yleft (nplist)=yg(iyq)
                             yright(nplist)=yg(iyq)
                             depth (nplist)=ndepth
                             zr(ixa,  iyq)=0
                             zl(ixa+1,iyq)=0
                             nx=nx+1
                             polyactive=.true.
                      endif  ! found_segment
c
               endif  ! polyactive
            enddo  ! IYQ
            if(nplist.gt.0
     *        )then
                   npolygon=npolygon+1
                   call kml_prep(nplist,npolygon,ndepth)
            endif
         enddo   !  WHILE...
      enddo   !  NDEPTH...
