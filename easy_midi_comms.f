      module easy_midi_comms
c
      parameter (maxtracks=12,maxtrklen=8000,maxdata=64000)
      parameter (maxtextin=120,maxtext=20000)
c
cGCC10dimension idnote(0:z'FF'),natural(7),mt(maxtracks)
      dimension idnote(0:255  ),natural(7),mt(maxtracks)
     *         ,note_stack(16),len_stack(16),on_stack(16)
c
      character(1) text(maxtext),octave,chardt(4)
      character(1) midi_data(maxdata),track_data(maxtrklen,maxtracks)
      character(1) textin(maxtextin)
      character(2) key,time_sig,cspan
      character(3) duration,beats_per_minute,expression,patch_id
      character(4) mtrk,mtrkend
      character(14) mthd
      character(15) from_desc,to_desc
c
      integer (4) delta_time,stc_time,stc_vel,leg_time,leg_vel,span
     *           ,track_time(maxtracks),bpm,on_stack,delta,patch,pan
     *           ,idt,idttmp,idtvol,ntempo,note_vol,ncon_on_chann
c
      logical track_started,key_init,running,chord,staccato,silent
     *       ,get_next_note,check,legato,rep,emphasis
     *       ,last_event(maxtracks)
c
      end
