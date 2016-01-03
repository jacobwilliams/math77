      program DRSELI
c>>1994-10-19 DRSELI Krogh  Changes to use M77CON
c>>1992-03-09 DRSELI WV Snyder Create separate single and double demos.
c>>1991-10-04 DRSELI WV Snyder JPL Original code.
c--S replaces "?": DR?ELI,?RCVAL,?ELEFI,?ELPII,?RDVAL,?RFVAL,?RJVAL
c
c     Demonstration driver for incomplete elliptic integral procedures.
c
      real             ALPHA2, E, F, K, K2, PHI, PI, R, RC, RD, RF, RJ
      real             SINPHI, T, U, X, Y, Z
      integer IERR
c
c     Compute arc sine x using ASIN and RC, for x = 0.5
c
      print *, 'Identities from write-up:'
      x = 0.5e0
      call srcval (1.0e0-x*x,1.0e0,rc,ierr)
      if (ierr.eq.0) then
         t = asin(x) - x * rc
         print '('' ASIN(0.5) - 0.5*RC(1-0.5**2,1)    ='',g15.8)', t
      else
         print '('' SRCVAL returns error signal '',i1)', ierr
      end if
c
c     Evaluate identities given by equations (8-10) in the write-up
c     with k**2 = 1/2, sin(phi)**2 = 1/4, alpha**2 = 1/2, c = 1.
c     From this, we have a = 3/4, b = r = 7/8.
c
      alpha2 = 0.5e0
      k = sqrt(0.5e0)
      k2 = 0.5e0
      sinphi = 0.5e0
      phi = asin(sinphi)
      r = 0.875e0
      x = 0.75e0
      y = 0.875e0
      z = 1.0e0
      call selefi (phi,k,f,e,ierr)
      if (ierr.ne.0) then
         print '('' SELEFI returns error signal '',i1)', ierr
         go to 99
      end if
      call selpii (phi,k2,alpha2,pi,ierr)
      if (ierr.ne.0) then
         print '('' SELPII returns error signal '',i1)', ierr
         go to 99
      end if
      call srdval (x,y,z,rd,ierr)
      if (ierr.ne.0) then
         print '('' SRDVAL returns error signal '',i1)', ierr
         go to 99
      end if
      call srfval (x,y,z,rf,ierr)
      if (ierr.ne.0) then
         print '('' SRFVAL returns error signal '',i1)', ierr
         go to 99
      end if
      call srjval (x,y,z,r,rj,ierr)
      if (ierr.ne.0) then
         print '('' SRJVAL returns error signal '',i1)', ierr
         go to 99
      end if
      u = sqrt(z**3) * rd
      t = 3.0e0 / (k2 * sinphi**3) * (f-e)
      r = (u-t) / u
      print '('' Equation (8), (LHS - RHS)/LHS     ='',g15.8)', r
      u = sqrt(z) * rf
      t = f / sinphi
      r = (u-t) / u
      print '('' Equation (9), (LHS - RHS)/LHS     ='',g15.8)', r
      u = sqrt(z**3) * rj
      t = 3 / (alpha2 * sinphi**3) * (pi - f)
      r = (u-t) / u
      print '('' Equation (10), (LHS - RHS)/LHS    ='',g15.8)', r
c
99    stop
      end
