      program multibuild
c
c      Builds the multiple job stream, and input files, for SP stress tests
c
      integer (8) npasses,npass_total,filesize,blocksize
c
      character (1) mount
      character (13) filename
      character (12) nodename(128)
      character (32) filesystem(128)
      character (80) input_line
c
c      On which file do we wish to run this stuff?
c
      nfiles=0
      npass_total=0
 31   continue
      nfiles=nfiles+1
      read(9,106,end=41)filesystem(nfiles)
      go to 31
 41   continue
      nfiles=nfiles-1
c
c      On which nodes do we wish to run this stuff?
c
      nodes=0
  3   continue
      nodes=nodes+1
      read(7,*,end=4)nodename(nodes)
      go to 3
  4   continue
      nodes=nodes-1
c
      read(*,*)mount
      read(*,*)filesize
      read(*,*)blocksize
      read (*,*)npasses
      write(*,100)npasses
      lastjob=0
      node=0
      nfil=0
  1   continue
      read(*,*,end=2)nextlast
      npass_total=npass_total+nextlast
      do job=lastjob+1,nextlast
         node=1+node
         nfil=1+nfil
         if(node.gt.nodes)node=1
         if(nfil.gt.nfiles)nfil=1
         write(*,101)nodename(node),job,job
c
c      Create input file
c
         write(filename,104)job
         open(4,file=filename,status='unknown')
         write(4,1051)mount,filesystem(nfil),filesize,blocksize
         close(4)
c
c      Write out a list of quick tests to see that everything is ready
c
      enddo
      lastjob=nextlast
c
      write(*,102)lastjob
      go to 1
c
  2   continue
c
      npass_total=npass_total*npasses
      write(*,103)npass_total
c
      stop
c            
100   format('######################################################'
     *     ,/'#'
     *     ,/'#  Start of automatically generated script'
     *     ,/'#'
     *     ,/'let "npasses=',i3,'"'
     *     ,/'#'
     *     ,/'#############################'
     *) 
101   format('$REMOTE_SUB ',a12
     *      ,' "$DATEST_HOME/$1 <$DATEST_HOME/darep.inp',z4.4
     *      ,' >$RESDIR/prog_output/$1.out',z4.4,' &" &')
102   format('#'
     *     ,/'./jobwait $1 ${npasses}',i4
     *     ,/'#############################'
     *) 
103   format('#'
     *     ,/'##### This run stream will generate >',i5,' passes'
     *     ,/'#'
     *     ,/'#  End of automatically generated script'
     *     ,/'#'
     *     ,/'######################################################'
     *) 
104   format('darep.inp',z4.4)
105   format(a1,/'/darepdir',z4.4,2(/i12))
1051  format(a1,/a32,2(/i12))
106   format(a32)
c
      end
     
