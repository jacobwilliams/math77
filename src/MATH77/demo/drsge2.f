c     program SRDGE2
c>> 2001-05-22 SRDGE2 Krogh Minor change for making .f90 version.
c>> 1994-10-19 SRDGE2 Krogh  Changes to use M77CON
c>> 1992-03-10 SRDGE2 Lawson
c
c     Demo driver for SGED, SGEFS, SGEI, SGESLT
c     ------------------------------------------------------------------
c--S replaces "?": ?RDGE2, ?GED, ?GEFS, ?GEI, ?GESLT, ?MATP
c     ------------------------------------------------------------------
      integer NMAX, INFO
      parameter (NMAX = 3)
      real             A(3,3), B(3,1), C(3,1), DET(2), WORK(NMAX)
      integer I, J, IPVT(NMAX)
c
      data (A(1,J),J=1,3) / 0.579E0, -.394E0, 0.915E0 /
      data (A(2,J),J=1,3) / -0.795E0, 0.226E0, -0.868E0 /
      data (A(3,J),J=1,3) / 0.141E0, -0.329E0, -0.286E0 /
c
      data (B(I,1),I=1,3) / 5.873E0, -5.324E0, 1.069E0 /
      data (C(I,1),I=1,3) / -6.267e0, 4.951e0, -5.177e0 /
c     ------------------------------------------------------------------
      call SMATP(A,NMAX,NMAX,NMAX,' A(,) =')
      print*,' '
      call SMATP(B,NMAX,NMAX,1,' B(,) =')
      call SGEFS(A,NMAX,NMAX,B,NMAX,1,IPVT,INFO)
      print*,' '
      call SMATP(B,NMAX,NMAX,1,' Solution of A*x = b is:')
c
      print*,' '
      call SMATP(C,NMAX,NMAX,1,' C(,) =')
      call SGESLT(A, NMAX, NMAX, IPVT, C)
      print*,' '
      call SMATP(C,NMAX,NMAX,1,' Solution of (A**t)*x = c is:')
c
      call SGED(A, NMAX, NMAX, IPVT, DET)
      print'(/a,f10.6,a,f5.2,a/)',
     *   ' Determinant of A is:',DET(1),' * 10**(',DET(2),')'
c
      call SGEI(A, NMAX, NMAX, IPVT, WORK)
      call SMATP(A,NMAX,NMAX,NMAX,' Inverse of A =')
      end
