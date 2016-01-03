c     program DRDGEFSC
c>> 2001-05-22 DRDGEFSC Krogh Minor change for making .f90 version.
c>> 1996-05-28 DRDGEFSC Krogh Moved format up.
c>> 1994-10-19 DRDGEFSC Krogh  Changes to use M77CON
c>> 1994-08-09 DRDGEFSC WVS removed '0' from format
c>> 1992-03-18 DRDGEFSC CLL Added "c" to "program" line above.
c>> 1987-12-09 DRDGEFSC Lawson  Initial Code.
c--D replaces "?": DR?GEFSC, ?GEFSC, ?GEFS, ?MATP
c
c     Demo driver for DGEFSC
c     ------------------------------------------------------------------
      integer NMAX
      parameter (NMAX = 3)
      double precision A(3,3), B(3,1), Z(NMAX), RCOND1
      integer I, J, IPVT(NMAX)
c
      data (A(1,J),J=1,3) / 0.579D0, -.394D0, 0.915D0 /
      data (A(2,J),J=1,3) / -0.795D0, 0.226D0, -0.868D0 /
      data (A(3,J),J=1,3) / 0.141D0, -0.329D0, -0.286D0 /
c
      data (B(I,1),I=1,3) / 5.873D0, -5.324D0, 1.069D0 /
c
 100  format(/' RCOND1 =',F7.4)
c     ------------------------------------------------------------------
      call DMATP(A,NMAX,NMAX,NMAX,'0 A(,) =')
      call DMATP(B,NMAX,NMAX,1,'0 B(,) =')
c
      call DGEFSC(A,NMAX,NMAX,B,NMAX,1,IPVT,RCOND1,Z)
c     call DGEFS(A,NMAX,NMAX,B,NMAX,1,IPVT,INFO)
c
      call DMATP(B,NMAX,NMAX,1,'0 SOLN(,) =')
      print 100, RCOND1
c
      end
