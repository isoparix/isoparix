      subroutine quad_transpose(array,lside)
c
c      Transposes quadrants A B  to C D
c                           D C     B A
c
      complex(8) array(lside,lside)
      complex(8) t(lside/2,lside/2)
c
      ia=lside/2
      if(ia*2.ne.lside
     *  )then
             write(0,*)'Array must be an even-sided square'
             return
      endif
c
      ib=ia+1
c
      t=array(1:ia,1:ia)                            !  Quadrant A in t
        array(1:ia,1:ia)=array(ib:lside,ib:lside)   !  Quadrant C now in A
                         array(ib:lside,ib:lside)=t !  Quadrant C replaced
c
      t=array(ib:lside,1:ia)                        !  Quadrant B in t
        array(ib:lside,1:ia)=array(1:ia,ib:lside)   !  Quadrant D now in B
                             array(1:ia,ib:lside)=t !  Quadrant D replaced
c
      return
      end
