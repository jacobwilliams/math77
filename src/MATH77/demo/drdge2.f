c     program DRDGE2
c>> 2001-05-22 DRDGE2 Krogh Minor change for making .f90 version.
c>> 1994-10-19 DRDGE2 Krogh  Changes to use M77CON
c>> 1992-03-10 DRDGE2 Lawson
c
c     Demo driver for DGED, DGEFS, DGEI, DGESLT
c     ------------------------------------------------------------------
c--D replaces "?": ?RDGE2, ?GED, ?GEFS, ?GEI, ?GESLT, ?MATP
c     ------------------------------------------------------------------
      integer NMAX, INFO
      parameter (NMAX = 3)
      double precision A(3,3), B(3,1), C(3,1), DET(2), WORK(NMAX)
      integer I, J, IPVT(NMAX)
c
      data (A(1,J),J=1,3) / 0.579D0, -.394D0, 0.915D0 /
      data (A(2,J),J=1,3) / -0.795D0, 0.226D0, -0.868D0 /
      data (A(3,J),J=1,3) / 0.141D0, -0.329D0, -0.286D0 /
c
      data (B(I,1),I=1,3) / 5.873D0, -5.324D0, 1.069D0 /
      data (C(I,1),I=1,3) / -6.267d0, 4.951d0, -5.177d0 /
c     ------------------------------------------------------------------
      call DMATP(A,NMAX,NMAX,NMAX,' A(,) =')
      print*,' '
      call DMATP(B,NMAX,NMAX,1,' B(,) =')
      call DGEFS(A,NMAX,NMAX,B,NMAX,1,IPVT,INFO)
      print*,' '
      call DMATP(B,NMAX,NMAX,1,' Solution of A*x = b is:')
c
      print*,' '
      call DMATP(C,NMAX,NMAX,1,' C(,) =')
      call DGESLT(A, NMAX, NMAX, IPVT, C)
      print*,' '
      call DMATP(C,NMAX,NMAX,1,' Solution of (A**t)*x = c is:')
c
      call DGED(A, NMAX, NMAX, IPVT, DET)
      print'(/a,f10.6,a,f5.2,a/)',
     *   ' Determinant of A is:',DET(1),' * 10**(',DET(2),')'
c
      call DGEI(A, NMAX, NMAX, IPVT, WORK)
      call DMATP(A,NMAX,NMAX,NMAX,' Inverse of A =')
      end
