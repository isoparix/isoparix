      program test_alloc
c
      character(1),allocatable,dimension(:,:)   :: tmparray
      integer(8)tmparray_size
c
  1   continue
      read(*,*)n
      if(allocated(tmparray))deallocate(tmparray,stat=istat)
      if(n.gt.0
     *  )then
             tmparray_size=n
         else
             tmparray_size=huge(tmparray_size)
      endif
      allocate(tmparray(tmparray_size,tmparray_size),stat=ierror)
      write(0,113)n,tmparray_size,size(tmparray,1),size(tmparray,2)
     *                           ,size(tmparray),ierror
      go to 1
113   format('TEST_ALLOC: Array is',5i12,'.  Error code:',i4)
c
      end
