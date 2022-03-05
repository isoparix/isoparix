      module sudoku_comms
c
      parameter (major=9,minor=3)
c
      logical check,sud_error,proc_entry_error
c
      logical,dimension(9,9,3,3) :: poss,possa
c
      integer :: nknowns,nknownsa,nfixes,nsteps
c
      integer,dimension(9,9,9) ::  minor_idn
c
      integer,dimension(9,9) ::line_record,rinc,cinr,rinsq,cinsq
     *                        ,minor_count,known,knowna
c
      integer,dimension(9) ::  known_sq_count,known_row_count
     *                        ,known_col_count,sq_count,row_count
     *                        ,col_count,nsqrowmin,nsqcolmin,numline
     *                        ,line_count
c
      end      
