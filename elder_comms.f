      module elder_comms
c
c      Helps to prepare:
c        1. A non-duplicating task lists duty_queueN.script, and
c        2. A set of edit scripts, shifting the link between
c           Qnn and elder_name(i) for each script
c
      character(10) elder_name(20)
      character(14) script_name
c
      logical available(1000),eligible(0:1000)
c
      integer task_id(10),ncount
c
      end
