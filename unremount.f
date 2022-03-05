      subroutine unremount(filesystem,mountdata)
c
c      Unmounts, and then re-mounts file system
c
      character (50) filesystem
      character (50) mountdata
c
      call system('umount '//filesystem,irc)
c
      if(irc.ne.0
     *  )then
             write(0,100)trim(filesystem)
             return
      endif
c
      call system('mount '//mountdata//' '//filesystem,irc)
c
      if(irc.ne.0
     *  )then
             write(0,101)trim(mountdata),trim(filesystem)
             stop
      endif
c
      return
c
100   format('***** WARNING in UNREMOUNT: Could not unmount ',a)
101   format('***** ERROR in UNREMOUNT: Could not mount ',a,' at ',a)
c
      end
