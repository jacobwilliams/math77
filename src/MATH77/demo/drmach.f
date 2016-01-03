      program DRMACH
c>> 1996-05-28 DRMACH Krogh Added external statement, moved formats.
c>> 1994-08-09 DRMACH WVS Removed '0' from format
c>> 1991-11-20 DRMACH CLL Edited for Fortran 90.
c>> 1989-10-30 DRMACH CLL Changed format to handle 14 digit Cray integer
c>> 1987-12-09 DRMACH   Lawson  Initial Code.
c                        Demo driver for D1MACH, I1MACH, and R1MACH
      external D1MACH, I1MACH, R1MACH
      double precision D1MACH, D1
      integer I1MACH, I1, J
      real R1MACH, R1
   10 format(15X,A)
   20 format(15X,41('-'))
   30 format(/2X,'J',6X,'I1MACH(J)',7X,'R1MACH(J)',13X,'D1MACH(J)')
   40 format(I3, I15,3X,G15.8,3X,G25.18)
   50 format(I3, I15)
c     ------------------------------------------------------------------
      print 10,'MACHINE CONSTANTS for ...                '
      print 20
      print 30
      print*,' '
      do 60 J=1,5
        I1 = I1MACH(J)
        R1 = R1MACH(J)
        D1 = D1MACH(J)
        print 40, J,I1,R1,D1
   60 continue
      do 70 J=6,16
        print 50, J,I1MACH(J)
   70 continue
c
      end
