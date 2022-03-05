      program fingather
c
      character(8)gather_name, gather_name_old,dummy_item
      character(1)line_texta(180)
c
      line_texta(1)='1'
      line_texta(2)='2'
      line_texta(3)='3'
      line_texta(4)='4'
      line_texta(5)='5'
      line_texta(6)='6'
      line_texta(7)='7'
      line_texta(8)='8'
      line_texta(9:180)='X'
      write(*,*)line_texta(4:16)
      stop
      end
