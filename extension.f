      subroutine extension(theta,pivot,radius,or,pq,actor,phi,px,py)
c
      implicit real *8 (a-h,o-z)
c
c      Calculate cartesian co-ords of point P; and length of OP, 
c      in quadrilateral OPQR
c      OR and PQ are (possibly unequal) cantilevers
c      OQ and PR are of equal length (=radius), and cross over each other
c
      cost=cos(theta)
      sint=sin(theta)
c
      rq2=radius**2-(2.0*or*radius*cost)+or**2
c
      if(rq2.lt.0.0
     *  )then
             phi=-1000.
             return
      endif
c
      beta_adj=radius**2+rq2-pq**2
      beta_hyp=2.0*radius*sqrt(rq2)
      b=beta_hyp**2-beta_adj**2       ! beta is angle PRQ
c
      if(b.lt.0.0
     *  )then
             phi=-1000.
             return
      endif
c
      beta_opp=sqrt(b)                ! beta is angle PRQ
c
      alpha_opp=radius*sint
      alpha_adj=(radius*cost)-or
      alpha_hyp=sqrt(alpha_adj**2+alpha_opp**2)   ! alpha is slope of QR
c
c     write(0,101)alpha_opp,alpha_adj,alpha_hyp,beta_opp,beta_adj
c    *           ,beta_hyp,alpha_adj/alpha_hyp,beta_adj/beta_hyp
c
      rhh=radius/(alpha_hyp*beta_hyp)
      px=(((alpha_adj*beta_adj)-(alpha_opp*beta_opp))*rhh)+or
      py= ((alpha_opp*beta_adj)+(alpha_adj*beta_opp))*rhh
c
      op=sqrt(px**2+py**2)
c
      pr_check=sqrt((px-or)**2+py**2)
      pq_check=sqrt((px-(radius*cost))**2
     *             +(py-(radius*sint))**2
     *             )
c
c      Calculate Pythagorean distance from P(p,0) to R
c
      actor=sqrt(((px+pivot)**2)+(py**2))
      if(py.lt.0.0
     *   )then
              actor=-actor
      endif
      phi=atan(py/px)
c
c     write(0,100)pr_check,pq_check,theta,pivot,radius,or,pq,actor
c    *           ,phi,px,py
c
      return
c
100   format(/' PR_CHECK PQ_CHECK    theta    pivot   radius       OR'
     *      , '       PQ    actor      phi       Px       Py'
     *      ,/12f9.2/)
101   format(6f11.3,6f12.4)
c
      end
