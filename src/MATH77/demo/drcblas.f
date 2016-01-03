c     program DRCBLAS
c>> 1997-06-13 DRCBLAS Krogh  Added External Statement.
c>> 1992-03-13 CLL
c   DRCBLAS..  Demo driver for SCNRM2, CCOPY, CDOTC, and CDOTU.
c     ------------------------------------------------------------------
      external SCNRM2, CDOTC, CDOTU
      real    SCNRM2
      complex CDOTC, CDOTU

      real    S2
      complex C1, C2, C3, C4, CX(4), CY(4), CZ(4)
      integer N
      data CX / (5.0,  3.0), (-2.0,  6.0), (4.0, -7.0), (-8.0, -11.0)/
      data CY / (1.0, -2.0), (4.0,  8.0), (-5.0, -6.0), (-3.0, 7.0)/
c     ------------------------------------------------------------------
      print*,
     * ' DRCBLAS..  Demo driver for SCNRM2, CCOPY, CDOTC, and CDOTU.'
      N = 4
      call CCOPY(N, CY, 1, CZ, 1)
c
      C1 = CDOTC(N, CX, 1, CY, 1) - cmplx(8.0, -201.0)
      C2 = CDOTC(N, CX, 1, CZ, 1) - cmplx(8.0, -201.0)
      print'(/a)',
     *   ' Using CCOPY and CDOTC.  Expect C1 = C2 = (0.0, 0.0)'
      print'(a,2f8.2/10x,a,2f8.2)',' Computed: C1 =',C1,' C2 =',C2
c
      C3 = CDOTU(N, CX, 1, CY, 1) - cmplx(-6.0, -11.0)
      C4 = CDOTU(N, CX, 1, CZ, 1) - cmplx(-6.0, -11.0)
      print'(/a)',
     *   ' Using CCOPY and CDOTU.  Expect C3 = C4 = (0.0, 0.0)'
      print'(a,2f8.2/10x,a,2f8.2)',' Computed: C3 =',C3,' C4 =',C4
c
      S2 = SCNRM2(N, CX, 1) - 18.0
      print'(/a,f9.4)',
     *   ' Using SCNRM2.  Expect S2 = 0.0.  Computed: ',S2
      end
