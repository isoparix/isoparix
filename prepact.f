      subroutine prepact
c
c      Prepares list of ALL active cubes
c
      use isocomm
c
      nlong=0
      do npx=1,nprocmax
         do ncx=1,nactive(npx)
            nlong=nlong+1
            longact(nlong)=listact(ncx,npx)
         enddo
      enddo
c
      if(check
     *  )then
             write(lchann,100)nlong,(mx,longact(mx)
     *                    ,nproc(longact(mx)),mx=1,nlong)
             call isoflush(lchann)
      endif
c
100   format(/'PREPACT: There are',i5,' active cubes in the list'
     *      /,'   Entry    Cube  Process',100(/3i8))
c
      return
      end
