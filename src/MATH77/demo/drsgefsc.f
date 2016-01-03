c     program DRSGEFSC
c>> 2001-05-22 DRSGEFSC Krogh Minor change for making .f90 version.
c>> 1996-05-28 DRSGEFSC Krogh Moved format up.
c>> 1994-10-19 DRSGEFSC Krogh  Changes to use M77CON
c>> 1994-08-09 DRSGEFSC WVS removed '0' from format
c>> 1992-03-18 DRSGEFSC CLL Added "c" to "program" line above.
c>> 1987-12-09 DRSGEFSC Lawson  Initial Code.
c--S replaces "?": DR?GEFSC, ?GEFSC, ?GEFS, ?MATP
c
c     Demo driver for SGEFSC
c     ------------------------------------------------------------------
      integer NMAX
      parameter (NMAX = 3)
      real             A(3,3), B(3,1), Z(NMAX), RCOND1
      integer I, J, IPVT(NMAX)
c
      data (A(1,J),J=1,3) / 0.579E0, -.394E0, 0.915E0 /
      data (A(2,J),J=1,3) / -0.795E0, 0.226E0, -0.868E0 /
      data (A(3,J),J=1,3) / 0.141E0, -0.329E0, -0.286E0 /
c
      data (B(I,1),I=1,3) / 5.873E0, -5.324E0, 1.069E0 /
c
 100  format(/' RCOND1 =',F7.4)
c     ------------------------------------------------------------------
      call SMATP(A,NMAX,NMAX,NMAX,'0 A(,) =')
      call SMATP(B,NMAX,NMAX,1,'0 B(,) =')
c
      call SGEFSC(A,NMAX,NMAX,B,NMAX,1,IPVT,RCOND1,Z)
c     call SGEFS(A,NMAX,NMAX,B,NMAX,1,IPVT,INFO)
c
      call SMATP(B,NMAX,NMAX,1,'0 SOLN(,) =')
      print 100, RCOND1
c
      end
