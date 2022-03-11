subroutine isoflush(ichan)
!
! Comment added to test CVS, and another test
!
   integer :: ichan
!#ifdef cygwin
   call flush(ichan)
!#elif defined _AIX
!   call flush_(ichan)
!#else
!Error Unknown operating system
!#endif

end subroutine isoflush
