      subroutine column_set
c
c      Associates screen columns and data columns
c
      use allcomms
c
c
c      NFIRSTCOL is the first map column to be updated
c        NENDCOL is the last   "    "    "  "     "
c       NDATACOL is the first data column to be transferred
c
      if(.not.updating
     *  )then
             if(new_win
     *         )then
                    nfirstcol=1
                else
                    if(nrecords.gt.0     .and.
     *                 lastcol .ge.npslot
     *                )then
                           nfirstcol=lastcol-npslot+1
                       else
                           nfirstcol=lastcol
                    endif
             endif
              ndatacol=nfirstcol
               nendcol=lastcol
      endif
c
      if(.not.new_win.and.updating
     *  )then
              ndatacol=lastcol
             nfirstcol=ntimeslots
               nendcol=ntimeslots
      endif
c
      if(new_win.and.updating
     *  )then
              ndatacol=lastcol
             nfirstcol=1
               nendcol=ntimeslots
      endif
c
      return
      end
