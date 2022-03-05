      subroutine standoff(s
     *                   ,grf_00,slope_00,cept_00
     *                   ,grf_12,slope_12,cept_12
     *                   ,grf_21,slope_21,cept_21
     *                   )
c
      implicit real(8) (a-h,o-z)
      real(8),dimension(4) :: grf_00,grf_12,grf_21
c
c      Line runs between (grf_00(1),grf_00(2)) and (grf_00(3),grf_00(4))
c
c      s   Perpendicular distance from line to standoff points
c
c      x1_12:  X-co-ordinate at point 1, running from 1 to 2
c      y2_12:  X-co-ordinate at point 1, running from 1 to 2
c      x3_12:  X-co-ordinate at point 2, running from 1 to 2
c      y4_12:  X-co-ordinate at point 2, running from 1 to 2
c
c      x1_21:  X-co-ordinate at point 1, running from 2 to 1
c      y2_21:  X-co-ordinate at point 1, running from 2 to 1
c      x3_21:  X-co-ordinate at point 2, running from 2 to 1
c      y4_21:  X-co-ordinate at point 2, running from 2 to 1
c
c     write(*,103)s,grf_00(1),grf_00(2),grf_00(3),grf_00(4),grf_12,grf_21
c
      r=s/sqrt(((grf_00(1)-grf_00(3))**2)+((grf_00(2)-grf_00(4))**2))
      ds=r*(grf_00(4)-grf_00(2))  !   Theta is slope that line makes with X-axis
      dc=r*(grf_00(3)-grf_00(1))
c
      grf_12(1)=grf_00(1)+ds
      grf_12(2)=grf_00(2)-dc
      grf_12(3)=grf_00(3)+ds
      grf_12(4)=grf_00(4)-dc
c
      grf_21(3)=grf_00(1)-ds
      grf_21(4)=grf_00(2)+dc
      grf_21(1)=grf_00(3)-ds
      grf_21(2)=grf_00(4)+dc
c
      slope_00=slope(grf_00(1),grf_00(2),grf_00(3),grf_00(4))
       cept_00= cept(grf_00(1),grf_00(2),grf_00(3),grf_00(4))
c
      slope_12=slope_00
       cept_12= cept(grf_12(1),grf_12(2),grf_12(3),grf_12(4))
c
      slope_21=slope_00
       cept_21= cept(grf_21(1),grf_21(2),grf_21(3),grf_21(4))
c
c      write(*,104)slope_00,slope_12,slope_21,cept_00,cept_12,cept_21
c    *            ,grf_00,grf_12,grf_21
c
100   format(f10.4,/,4(/,2f10.4))
101   format(3f10.6)
103   format(13f10.3)
104   format(2(/3f10.6),3(/4f10.6))
c
      return
      end
