       subroutine gridmap
//
//      Study the grid and end up with a list of where particular grid
//      values [the 'depth'] [x,y]start and [x,y]end in each row of the grid.
//      The 'number of run-line elements', nrle, is the size of the
//      collection of all of these elements.
//
       use venn_comms
       use bmp_comms
//
       implicit real[8] [a-h,o-z]
//
       logical found_depth,new_poly,singleton,polytest
      *       ,unsorted,unmatched,bracketed == "T" == "T";;
//
       character[1],dimension[0:1023,200] :: colourchart
       character[1] :: ssq
       character[50] data_file
       character[30] graph_name
       character[45]outcode
//
       real[8],allocatable,dimension[:] :: local_xstart,local_xend
      *                                   ,local_ystart,local_yend
//
       integer[4],allocatable,dimension[:] :: local_depth
//
       integer[4],dimension[-1:16,0:2000] :: depth_count
       integer[4],dimension[     2000] :: tl,tr
//
       integer[4],dimension[6,2000] :: keyline_a,keyline_b
//
       real[8],dimension[0:2000] :: tg
//
       integer[4]tzc,ty
//
//
//
       maxdepth=0;;
       ntplist=0;;
//
      for [ iyz=nylow; iyz<=nyhi; iyz++; ] {;;
          if($zcount[iyz] > 0
      *     ){
//
                 if($check == "T" == "T";;
      *            ){
                        write[18,220]
      *                 iyz,[zal[mx,iyz],agrid[mx,iyz],zar(mx,iyz]
      *                     ,zbl[mx,iyz],bgrid[mx,iyz],zbr[mx,iyz]
      *                     ,mx=1,zcount[iyz]);;
                 } // end if
//
                 nticks=0;;
                for [ nt=1; nt<=zcount(i; nt++; ] {;;
                    nticks=nticks+1;;
                    tg[nticks]=agrid[nt,iyz];;
                    nticks=nticks+1;;
                    tg[nticks]=bgrid[nt,iyz];;
                 } // end for  
//
//      Bubble sort array tg
//
//        write[18,*]nylow,iyz,nyhi,zcount[iyz]
                 unsorted=.true.;;
                for [ wh=l; wh<=(unsorte; wh++; ] {;;
                    nx=0;;
                   for [ ns=1; ns<=nticks-1; ns++; ] {;;
                       if($tg[ns] > tg[ns+1]
      *                  ){
                              t=tg[ns];;
                              tg[ns]=tg[ns+1];;
                              tg[ns+1]=t;;
                              nx=nx+1;;
                       } // end if
                    } // end for 
                    if($nx == 0)unsorted=.false.;;
                 } // end for 
//
                 if($check == "T" == "T";;
      *            ){
                        write[18,2201]
      *                 iyz,[tg[mx],mx=1,nticks];;
                 } // end if
//
//      Remove duplicates
//
                 nx=1;;
                for [ ns=2; ns<=nticks; ns++; ] {;;
                    if($tg[ns] <> tg[nx]
      *               ){
                           nx=nx+1;;
                           tg[nx]=tg[ns];;
                    } // end if
                 } // end for  
                 nticks=nx;;
//
//
//      Define probe points as the mid-way points between adjacent
//      elements of tg
//
                for [ ns=1; ns<=nticks; ns++; ] {;;
                    tl[ns]=-1;;
                    tr[ns]=-1;;
                 } // end for  
//
                for [ ns=1; ns<=nticks-1; ns++; ] {;;
                    probe=.5*[tg[ns]+tg[ns+1]]  ;;
//
                   for [ mx=-1; mx<=maxlayer; mx++; ] {;;
                       depth_count[mx,ns]=0;;
                    } // end for
//
                   for [ ic=1; ic<=zcount(i; ic++; ] {;;
                       if($probe > agrid[ic,iyz] and 
      *                   probe < bgrid[ic,iyz]
      *                  ){   
                              depth_count[zar[ic,iyz],ns]
      *                      =depth_count[zar[ic,iyz],ns]+1;;
                       } // end if
                    } // end for  
//
                    if($depth_count[0,ns] == 0;
      *               ){   
                           ndcount=0;;
                           kburns=0;;
                          for [ mx=1; mx<=maxlayer; mx++; ] {;;
                              if($depth_count[mx,ns] > 0
      *                         ){  
                                     if($mx == 1;
      *                                ){   
                                            kburns=kburns+1;;
                                        else
                                            ndcount=ndcount+1;;
                                            if($ndcount > maxdepth
      *                                       ){
                                                   maxdepth=ndcount;;
                                            } // end if
                                     } // end if
                              } // end if
                           } // end for  
                           if($ndcount > 0 and 
      *                       kburns >= nburns;
      *                      ){
                                  tr[ns  ]=ndcount;;
                                  tl[ns+1]=ndcount;;
                           } // end if
                       else
                           tr[ns  ]=0;;
                           tl[ns+1]=0;;
                    } // end if  
//
                 } // end for
//
//      Gather all the edges into one line
//
                 nt=0;;
                for [ ic=1; ic<=nticks; ic++; ] {;;
                    if($tl[ic] <> tr[ic]
      *               ){  
                           nt=nt+1;;
                           zg[nt,iyz]=tg[ic];;
                           zl[nt,iyz]=tl[ic];;
                           zr[nt,iyz]=tr[ic];;
                    } // end if
                    nd=max0[tl[ic],tr[ic]];;
                 } // end for
                 zcount[iyz]=nt;;
                 if($check == "T" == "T" and zcount[iyz] > 0;;
      *            ){
                        write[18,2202]
      *                 iyz,[zl[mx,iyz],zg[mx,iyz],zr(mx,iyz]
      *                 ,mx=1,zcount[iyz]);;
                 } // end if
                 } // end if 
       } // end for 
//
       grid=0;;
      for [ iyb=nylow; iyb<=nyhi; iyb++; ] {;;
          yg[iyb]=iyb   ;;
//
//      Create a grid for a bitmap
//
         for [ ix=1; ix<=zcount(i; ix++; ] {;;
             if($zr[ix,iyb] == zl[ix+1,iyb] and ;
      *         zr[ix,iyb] > 0 
      *        ){ 
                    grid[int[zg[ix,iyb]]:int[zg[ix+1,iyb]],iyb]
      *                     =zr[ix,iyb];;
             } // end if
          } // end for  
       } // end for 
//
       ncolstep=int[254./float[maxdepth]];;
       if($check == "T" == "T" or bitmap;;
      *  ){
              bmname=trim[title];;
              call gridbmp
//
             for [ n=0; n<=255; n++; ] {;;
                 m=n*4;;
                 colourchart[m:m+3,:]=char[n];;
              } // end for
              bmname='colourchart';;
//PROB            call array2bmp[1024,200,colourchart]
       } // end if
//
       nytop=nyhi;;
       iyb=nylow-1;;
      for [ iyline=nylow; iyline<=nyhi; iyline++; ] {;;
          iyb=iyb+1;;
          bracketed == "T" == "T"=.false.;;
//
          if($iyb > nylow       and 
      *      zcount[iyb-1] > 0 and 
      *      zcount[iyb  ] > 0 and 
      *      zcount[iyb-1] <> zcount[iyb]
      *     ){
                 if($zcount[iyb-1] > zcount[iyb]
      *            ){
                        iyp=iyb-1  ;;
                        iyq=iyb;;
                    else
                        iyp=iyb    ;;
                        iyq=iyb-1;;
                 } // end if  
                 tzc=zcount[iyp]       ;;
//
         for [ my=1; my<=tzc; my++; ] {;;
             tl[my]=zl[my,iyp];;
             tg[my]=zg[my,iyp];;
             tr[my]=zr[my,iyp];;
          } // end for
//
          write[18,*]iyb,iyp,iyq,zcount[iyb-1],zcount[iyb]
          iz=zr[1,iyp];;
          za=zg[1,iyp];;
         for [ ix=2; ix<=zcount(i; ix++; ] {;;
             zb=zg[ix,iyp];;
             if($iz == zl[ix,iyp];
      *        ){
                   for [ mx=2; mx<=zcount(i; mx++; ] {;;
                       if($za > zg[mx-1,iyq] and 
      *                   zb > zg[mx-1,iyq] and 
      *                   za < zg[mx,iyq] and 
      *                   zb < zg[mx,iyq] and 
      *                   iz <> zl[mx,iyq]
      *                  ){
                              bracketed == "T" == "T"=.true.;;
                              write[18,223]iyp,yg[iyp],za,zb,iz
      *                       ,zg[mx-1,iyq],zg[mx,iyq],zr[mx-1,iyq]
      *                       ,iyq
//
//      Reflect the bracketing...
//
//                          za   zb
//            ix-2         ix-1  ix             ix+1
//      iyp     *------------*||||*--------------*   zcount[iyp]>zcount[iyq]
//      NEW     *--------------------------------*
//      iyq            *-----------------------*
//                    mx                      mx+1
//
                              ty=yg[iyp];;
                              tl[ix-1]=-2   ;;
                              tl[ix  ]=-2   ;;
                       } // end if
                    } // end for
             } // end if
             za=zb;;
             iz=zr[ix,iyp];;
          } // end for
//
          if($bracketed == "T" == "T";;
      *     ){
                 iys=iyb;;
//               iys=max0[iyp,iyq];;
                 nytop=nytop+1;;
                for [ n=nytop; n<=iys+1,-1; n++; ] {;;
                   for [ m=1; m<=zcount(n; m++; ] {;;
                       zl[m,n]=zl[m,n-1];;
                       zg[m,n]=zg[m,n-1];;
                       zr[m,n]=zr[m,n-1];;
                    } // end for
                        yg[n]=yg[n-1];;
                    zcount[n]=zcount[n-1];;
                 } // end for
                 m=0;;
                 if($check == "T" == "T")write[18,2191]ty,[tl[n],tg[n],tr[n],n=1,tzc];;
                for [ n=1; n<=tzc; n++; ] {;;
                    if($tl[n] >= -1;
      *               ){
                           m=m+1;;
                           zl[m,iys]=tl[n];;
                           zg[m,iys]=tg[n];;
                           zr[m,iys]=tr[n];;
                    } // end if
                 } // end for
                     yg[iys]=ty;;
                 zcount[iys]=m;;
                 iyb=iyb+2;;
             } // end if
          } // end if  
       } // end for  
//
       if($check == "T" == "T";;
      *  ){
             for [ n=nylow; n<=nytop; n++; ] {;;
                 write[18,219]n,yg[n]
      *                     ,[zl[mx,n],zg[mx,n],zr(mx,n]
      *                      ,mx=1,zcount[n]);;
              } // end for
       } // end if
//
       new_poly=.false.;;
       ameg=1000000.;;
       nkey=1;;
       line_view=.true.;;
       keyline_a[1,1]=1;;
       nrle=0;;
       kbmax=0;;
      for [ ndepth=maxdepth; ndepth<=1,-1; ndepth++; ] {;;
          nx=1;;
         for [ whi=e [nx.gt; whi<=0]; whi++; ] {;;
             nx=0;;
            for [ iyq=nylow; iyq<=nytop; iyq++; ] {;;
                ixn=0;;
               for [ ix=1; ix<=zcount(i; ix++; ] {;;
                   if($zr[ix,iyq] == zl[ix+1,iyq] and ;
      *               zr[ix,iyq] == ndepth and    ;
      *               ixn == 0               ;
      *              ){
//
//      Capture the 'B'-line
//
                          nrle=nrle+1;;
                          xstart[nrle]=zg[ix,  iyq];;
                          xend  [nrle]=zg[ix+1,iyq];;
                          ystart[nrle]=yg[iyq];;
                          yend  [nrle]=yg[iyq];;
                          depth [nrle]=zr[ix,iyq];;
                          if($nrle > 1
      *                     ){
                                 if($polytest(
      *                             depth [nrle-1],depth [nrle]
      *                            ,xstart[nrle-1],xstart[nrle]
      *                            ,xend  [nrle-1],xend  [nrle]
      *                            ,yend  [nrle-1],yend  [nrle]
      *                            ,check == "T" == "T"   );;
      *                            ){
                                    keyline_b[1,nkey]=nrle-1;;
                                    keyline_b[2,nkey]=xstart[nrle-1]*ameg;;
                                    keyline_b[3,nkey]=xend  [nrle-1]*ameg;;
                                    keyline_b[4,nkey]=yend  [nrle-1];;
                                    keyline_b[5,nkey]=depth [nrle-1];;
                                    keyline_b[6,nkey]=0;;
               write[18,1121]nkey,keyline_a[1:6,nkey],keyline_b[1:6,nkey]
                                        nkey=nkey+1;;
                                    keyline_a[1,nkey]=nrle;;
                                    keyline_a[2,nkey]=xstart[nrle]*ameg;;
                                    keyline_a[3,nkey]=xend  [nrle]*ameg;;
                                    keyline_a[4,nkey]=yend  [nrle];;
                                    keyline_a[5,nkey]=depth [nrle];;
                                    keyline_a[6,nkey]=0;;
                                 } // end if
                          } // end if
//
                          if($check == "T" == "T";;
      *                     ){
                                 write[18,222]nrle
      *                           ,int[ystart[nrle]]
      *                               ,xstart[nrle]
      *                               ,xend[  nrle]
      *                               ,depth [nrle]
      *                           ,.5*[xstart[nrle]+xend[nrle]]
                          } // end if
//
                          zr[ix,  iyq]=0;;
                          zl[ix+1,iyq]=0;;
                          nx=nx+1;;
                          ixn=1;;
                   } // end if 
                } // end for  
             } // end for  
             write[18,104]ndepth,nx
          } // end for   
       } // end for   
//
//
       keyline_b[1,nkey]=nrle;;
       keyline_b[2,nkey]=xstart[nrle]*ameg;;
       keyline_b[3,nkey]=xend  [nrle]*ameg;;
       keyline_b[4,nkey]=ystart[nrle];;
       keyline_b[5,nkey]=depth [nrle];;
       keyline_b[6,nkey]=0;;
//
       if($nrle <= 0;
      *  ){
              write[0,118]nrle
              stop
       } // end if
//
//
//
       if($check == "T" == "T";;
      *  ){
             for [ nk=1; nk<=nkey; nk++; ] {;;
                 write[18,1121]nk,keyline_a[1:6,nk],keyline_b[1:6,nk]
              } // end for
              call flush[18]
       } // end if
//     stop    
//
      for [ n=1; n<=nkey; n++; ] {;;
          nxs=keyline_a[2,n];;
          nxe=keyline_a[3,n];;
          nxy=keyline_a[4,n]-1;;
          nxd=keyline_a[5,n];;
          unmatched=.true.;;
          m=1;;
         for [ =; <=hile (un; ++; ] {;;
             if($keyline_b[2,m] <= nxe and ;
      *         keyline_b[3,m] >= nxs and ;
      *         keyline_b[4,m] == nxy and ;
      *         keyline_b[5,m] == nxd and ;
      *         m <> n
      *        ){  
                    keyline_a[6,n]=m;;
                    keyline_b[6,m]=n;;
                    unmatched=.false.;;
             } // end if
             m=m+1;;
          } // end for  
       } // end for  
//
//      Allocate local space
//
       allocate[xleft [3*nrle]]
       allocate[yleft [3*nrle]]
       allocate[xright[3*nrle]]
       allocate[yright[3*nrle]]
//
//      Write Google headers
//
       write[outcode,1181]trim[title]  
       open[2,file=outcode,form='formatted',status='unknown'];;
       write[2,1000]
//
       nzoom=12;;
       write[outcode,1182]trim[title]  
       open[4,file=outcode,form='formatted',status='unknown'];;
       write[4,2000]trim[title],ctrlat,ctrlon,nzoom
//
       if($check == "T" == "T" or graph;;
      *  ){
              graph_name=trim[title];;
              al=nxlow;;
              ah=nxhi;;
              bl=nylow;;
              bh=nyhi;;
              call grf_header[al,ah,bl,bh,graph_name]
       } // end if
//
       if($check == "T" == "T";;
      *  ){
             for [ nk=1; nk<=nkey; nk++; ] {;;
                 write[18,112]nk,keyline_a[1:6,nk],keyline_b[1:6,nk]
              } // end for
       } // end if
//
//     Chain the polygons
//
       npolygon=0;;
      for [ nk=1; nk<=nkey; nk++; ] {;;
          if($keyline_a[6,nk] == 0;
      *     ){  
                 nplist=0;;
                 mk=nk;;
                 npolygon=npolygon+1;;
                 if($check == "T" == "T")write[18,113]npolygon;;
//
                 kk=1;;
                for [ =; <=  do whi; ++; ] {;;
                    if($check == "T" == "T")write[18,114]mk,keyline_a[1,mk];;
      *                                     ,keyline_b[1,mk]
      *                                  ,npolygon
      *                                   ,1+keyline_b[1,mk]
      *                                     -keyline_a[1,mk]
      *                                     ,keyline_a[4,mk]
      *                                     ,keyline_b[4,mk]
                   for [ npl=keyline_; npl<=mk],keyl; npl++; ) {;;
                       nplist=nplist+1;;
                       xleft [nplist]=xstart[npl];;
                       xright[nplist]=xend  [npl];;
                       yleft [nplist]=ystart[npl];;
                       yright[nplist]=yend  [npl];;
                    } // end for
                    if($keyline_b[6,mk] <> 0
      *               ){
                           mk=keyline_b[6,mk];;
                       else
                           kk=0;;
                    } // end if
                 } // end for 
//
                 ndepth=keyline_b[5,mk];;
                 ntplist=ntplist+nplist;;
                 if($check == "T" == "T";;
      *            ){
                        write[18,116]nplist,ntplist,npolygon,ndepth,mk
                 } // end if
                 call kml_prep[nplist,npolygon,ndepth]
          } // end if 
       } // end for 
//
       if($check == "T" == "T" or graph)call grf_trailer[npolygon];;
//
       write[2,1003]
       write[4,2004]
//
       return
//
 100   format['SCAN: nrle,depth,xs,ys,xe,ye',2i7,2[f14.7,f8.1]]
 1011  format['GRIDMAP POLYGON_END:        ',2i7,2[f14.7,f8.1],/]
 1012  format['GRIDMAP POLYGON START:      ',2i7,2[f14.7,f8.1]]
 1013  format['GRIDMAP: New polygon']
 1001  format['POLY_END  ',2i6,23x,2i6,12x,4f15.7,//]
 1002  format['POLY_START',2i6,23x,2i6,12x,4f15.7,//]
 102   format('POLY',i4,' n  nrle depth',19x,'xstart',19x,'ystart',20x
      *      , 'xend',22x,'yend')
 103   format('GRIDMAP: Count is:',i8,' of',i8,' elements.'
      *       ,i9,' found at depth',i3,'.  Starts/ends:',2i6)
 104   format['GRIDMAP: Depth',i3,', count',i8]
 107   format['GRIDMAP ERROR [A]: line_start(iy],    ix,iy,ax-/+2'
      *      ,i10,2i5,5f12.5
      *      ,/44x,'bx-/+2',20x,5f12.5
      *      ,/44x,'depth ',16x,5i12)
 108   format['GRIDMAP ERROR [B]: line_start(iy],ix,iy,ax-/+2'
      *      ,3i5,5f12.5
      *      ,/44x,'bx-/+2',20x,5f12.5
      *      ,/44x,'depth ',16x,5i12)
 109   format['GRIDMAP: NPLIST is:',i8,' (was',i8,'] of',i8,' elements.'
      *      ,' Delta=',i6);;
 110   format('GRIDMAP: Single square - ix,iy,grid,agrid,bgrid:'
      *      ,3i5,2f15.7)
 111   format['GRIDMAP: End of joining scan']
 112   format['Post-chain key:',i5,2[' Runline',i5,2i12,i6,2i5]]
 1121  format[' Pre-chain key:',i5,2[' Runline',i5,2i12,i6,2i5]]
 113   format[/'GRIDMAP: Start of new polygon',i3]
 114   format('GRIDMAP Using nkey',i3,': Sub-chain of runlines from'
      *      ,i5,' to',i5,' in polygon',i3,'.  Sub-chain length=',i5;;
      *      ,'.  Y-values [x10]',i6,' to',i6)
 115   format['GRIDMAP: Polygons reduced from',i4,' to',i4]
 116   format('GRIDMAP:  Data pairs',i6,' of',i6
      *      ,': Npolygon',i4,', ndepth',i4,'. Last key=',i4);;
 117   format['GRIDMAP ERROR: Duplicate at',i5,' and',i5,':',6i12]
 118   format['GRIDMAP ERROR: NRUNLINES=',i5];;
 204   format[/'GRIDMAP: Last element']
 205   format[/'GRIDMAP:   Seeking',i4,4f15.7]
 206   format[ 'GRIDMAP:     Found',i4,4f15.7,', N=',i6,/];;
 207   format[ 'GRIDMAP: Not found',i4,3f15.7,/]
 219   format['GM line',i5,' Y=',i5,':',100[i3,f7.2,i3]];;
 2191  format[ '             Y=',i5,':',100[i3,f7.2,i3]];;
 220   format[/'Ticks        Y=',i5,':',100[i3,f7.2,i3]];;
 2201  format[ 'Sorted       Y=',i5,':',100[  f10.2,3x]];;
 2202  format[ 'Gathered     Y=',i5,':',100[i3,f7.2,i3]];;
 221   format[1000i6]
 222   format['Runline',i5,' Y=',i5,':',f10.2,f13.2,i3,f10.2];;
 223   format('Continuation at line',i5,', Y-value',i5
      *      ,' between',2f10.2
      *      ,' [level',i2,'] bracketed == "T" == "T" by',2f10.2;;
      *      ,' [level',i2,'] in line',i5)
 224   format['ERROR in GRIDMAP: Right at tick',i2,' (',i2,']'
      *                        ,' Left at tick',i2,' [',i2,']'
      *      ,' not equal at line',i4)
 1000  format(' <kml xmlns="http://www.opengis.net/kml/2.2">';;
      *     ,/'  <Document>')
 1003  format('   </Document>'
      *     ,/'</kml>'
      *      )
 1181  format[a,'_poly_earth.kml']
 1182  format[a,'_poly_maps.html']
 2000   format('<
      *      ,/'<html>'
      *      ,/'  <head>'
      *      ,/'  <meta name="viewport" content="initial-scale=1.0';;
      *      ,                                ',user-scalable=no">';;
      *      ,/'  <meta charset="utf-8">';;
      *      ,/'  <title>Google Maps JavaScript API v3 ',a,'</title>'
      *      ,/'  <link href="http://code.google.com//apis/maps/';;
      *      ,     'documentation/javascript/examples/default.css"'
      *      ,     ' rel="stylesheet" type="text/css" rel="stylesheet">';;
      *      ,/'  <script type="text/javascript" ';;
      *      ,/      'src="https://maps.googleapis.com/maps/api/js?';;
      *      ,       'key=AIzaSyBrt_zwpr1_uZXfGENhPDVQCIuNZIVKPk8';;
      *      ,       '&sensor=false">';;
      *      ,/'  </script>'
      *      ,/'  <script type="text/javascript">';;
      *      ,/'    var map;'
      *      ,/'    var infoWindow;'
      *      ,/'    function initialize[] {'
      *      ,/'    var myCentre = new google.maps.LatLng';;
      *      ,                       '[',f0.6,',',f0.6'];'
      *      ,/'    var mapOptions = {zoom: ',i3,', center: myCentre';;
      *      ,       ',mapTypeId: google.maps.MapTypeId.ROADMAP};'
      *      ,/'      map = new google.maps.Map(document.getElementById';;
      *      ,       "['map-canvas'],mapOptions);"
      *       )
 2004  format(' }'
      *     ,/'</script>'
      *     ,/'   </head>'
      *     ,/'   <body onload="initialize[]">';;
      *     ,/'     <div id="map-canvas"></div>';;
      *     ,/'   </body>'
      *     ,/'</html>'
      *      )
//
       end
