      subroutine cubeadd(nprocess,ix1,iz1,ix2,iz2,iheight)
c
c      Reads in a new cube and adds it to the list of 'supplied', as
c      opposed to 'generated' cubes.    Then cube details are reset
c      and the cubes are presented to cubeadd in descending order of size
c
      use isocomm
c
      if(check
     *  )then
             write(lchann,100)nprocess,ix1,iz1,ix2,iz2,iheight
             call isoflush(lchann)
      endif
c
      list_order=list_order+1
c
      list_supplied(1,list_order)=nprocess   
      list_supplied(2,list_order)=ix1
      list_supplied(3,list_order)=iz1
      list_supplied(4,list_order)=ix2
      list_supplied(5,list_order)=iz2
      list_supplied(6,list_order)=iheight
      list_supplied(7,list_order)=list_order
      list_supplied(8,list_order)=iabs((ix2-ix1+1)*(iz2-iz1+1))
c
c      Reset cube stats
c
      nlong=0
      nactive=0
      nprocmax=0
      ncubes=0
c
c      Feed the cubes to cubesplit in order of size
c
      do n=1,list_order
         unselected=.true.
      enddo
c
      maxsize=0
      do nx=1,list_order
c
c      Very crude sort into order of size
c
         maxsize=0
         do n=1,list_order
            if(unselected(n)
     *        )then
                   if(list_supplied(8,n).gt.maxsize
     *               )then
                          maxsize=list_supplied(8,n)
                          nselect=n
                   endif
            endif
            if(check
     *        )then
                   write(lchann,105)nselect,maxsize,list_order,n
     *                     ,(list_supplied(mx,n),mx=1,8)
                   call isoflush(lchann)
            endif
         enddo
c
c
         unselected(nselect)=.false.
         call cubesplit(list_supplied(1,nselect)
     *                 ,list_supplied(2,nselect)
     *                 ,list_supplied(3,nselect)
     *                 ,list_supplied(4,nselect)
     *                 ,list_supplied(5,nselect)
     *                 ,list_supplied(6,nselect)
     *                 )
c
      enddo
c
c      Discover which cube was created for the new entry....
c
      memcube=-99
      do nc=1,ncubes
         if(nproc(nc).eq.nprocess
     *     )then
                active(nc)=.false.
                if(     ix1.eq.nx1(nc)
     *             .and.ix2.eq.nx2(nc)
     *             .and.iz1.eq.nz1(nc)
     *             .and.iz2.eq.nz2(nc)
     *            )then
                       memcube=nc
                endif
         endif
      enddo
c
      if(memcube.eq.-99
     *  )then
             write(lchann,104)ncubes,nprocess
             call isoflush(lchann)
             return
      endif
c
c      Make this latest cube active, and all others of his process 
c      inactive....
c
      nactive(nprocess)=1
      active(memcube)=.true.
      listact(1,nprocess)=memcube
      call prepact
c
      return
c
100   format('CUBEADD: nprocess:',i8
     *     ,/'              ix1:',i8
     *     ,/'              iz1:',i8
     *     ,/'              ix2:',i8
     *     ,/'              iz2:',i8
     *     ,/'           height:',i8
     *      )
101   format(/'***** Last cube activated  (ID=',i3,'?) *****',/)
102   format(/'***** Activating last cube (ID=',i3,' ) *****',/)
103   format('CUBEADD: Process',i3,', active cubes',i3,':',150i4)
104   format('CUBEADD: No cube of ',i0.0,' cubes'
     *      ,' was owned by nprocess ',i0.0)
105   format('CUBEADD: NSELECT=',i3,', MAXSIZE=',i12,', LIST_ORDER=',i3
     *      ,', N=',i3
     *     ,/'         LIST_SUPPLIED(MX,N)=',7i6,i12)
c 
      end
