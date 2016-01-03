c     DRVECPR
c>> 2006-04-02 DRVECPR Krogh Added checks with exponents.
c>> 2001-05-22 DRVECPR Krogh Minor change for making .f90 version.
c>> 1991-11-22 DRVECPR   Krogh Initial Code.
c
      integer LWIDTH, LUNIT, NUMDIG, N, I
      parameter (LWIDTH=79)
      parameter (LUNIT=0)
      parameter (NUMDIG = 0)
      parameter (N = 6)
      integer A(N)
      real B1(N), B2(N)
      double precision C1(N), C2(N), C3(N)
c
      data (A(I),I=1,N) / 1,2,3,4,5,6 /
      data (B1(I),I=1,N) / 1.E0,2.E0,3.E0,4.E0,5.E0,6.E0/
      data (B2(I),I=1,N) / 1.E4,2.E-4,-3.E0,4.E0,5.E0,6.E0/
      data (C1(I),I=1,N) / 1.D2,2.D0,3.D0,-4.D1,5.D0,6.D0 /
      data (C2(I),I=1,N) / 1.D2,2.D0,3.D0,-4.D11,5.D0,6.D0 /
      data (C3(I),I=1,N) / 1.D14,2.D1,-3.D100,4.D0,5.D0,6.D3/
c
      call IVECPR (A,N,'0 IVECPR.. A()=', LWIDTH, LUNIT)
      call SVECPR (B1,N,'0 SVECPR.. B1()=', LWIDTH, LUNIT, NUMDIG)
      call DVECPR (C1,N,'0 DVECPR.. C1()=', LWIDTH, LUNIT, NUMDIG)
      call SVECPR (B2,N,'0 SVECPR.. B2()=', LWIDTH, LUNIT, NUMDIG)
      call DVECPR (C2,N,'0 DVECPR.. C2()=', LWIDTH, LUNIT, NUMDIG)
      call DVECPR (C3,N,'0 DVECPR.. C3()=', LWIDTH, LUNIT, NUMDIG)
c
      end
