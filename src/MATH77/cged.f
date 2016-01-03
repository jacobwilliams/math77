      subroutine CGED(A,LDA,N,IPVT,DET)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2001-05-25 CGED  Krogh Minor change for making .f90 version.
c>> 1996-03-30 CGED  Krogh   Added external statement.
C>> 1992-04-14 CLL Using (*) in dimensions rather than (1).
C>> 1990-01-19 CLL added test to avoid looping when some A(I,I) = 0.
C>> 1987-10-28 CGED   Lawson  Initial code.
C
C     CGED computes the determinant of the matrix A
C     using the LU factorization of A given in the array A().
c     ------------------------------------------------------------------
c                        Subroutine arguments
C
C     A(,)  [in]  An array of size at least N x N.  On entry must
c            contain the LU factors of an N x N  matrix, A.  It is
c            expected that this factorization will have been computed by
c            use of _GEFA, either directly or indirectly via use of
c            _GEFCO, _GEFS or _GEFSC.  This subr will not alter the
c            contents of A(,)
C
C     LDA  [in]  Leading dimensioning parameter for the array A(,).
C
C     N  [in]  The order of the original matrix, A.
C
C     IPVT()  [in]  An integer array of length at least N, containg a
c           record of the row interchanges made during factorization of
c           A.
C
C     DET() [out]  Array of length 2.  On return will contain a
c           representation of the determinant of A of the form
C                DETERMINANT = DET(1) * 10.0**DET(2)
C           DET(2) will be integer valued: plus, minus or zero.
c           If the determinant is zero then DET(1) = 0.0 and DET(2) = 0.
c           If the determinant is nonzero the scaling chosen will not be
c           determined exclusively by the value of the determinant, but
c           will tend to choose DET(2) = 0.0 when the determinant is not
c           extremely large or small in magnitude.  This is a change
c           from the normalization used in LINPACK.
c     ------------------------------------------------------------------
c                       Machine dependent values
c
c     The following values are set depending on the machine underflow
c     and overflow limits.
c           FBIG   = 10.0**STEP
c           FSMALL = 10.0**(-STEP) = 1.0/FBIG
c           TBIG   = FBIG/10.0 = 10.0**(STEP-1)
c           TSMALL = FSMALL*10.0 = 10.0**(-(STEP-1)) = 1.0/TBIG
c
c     where STEP is a positive integer value chosen so that FBIG**2 is
c     slightly smaller than the overflow limit and FBIG**(-2) is
c     slightly larger than the underflow limit.
c     If one wanted a machine-independent version of this code, one
c     could set STEP small enough so that FBIG**2 and FBIG**(-2) will
c     not be off-scale for any "reasonable" computer.  For instance one
c     might set STEP = 10.  However, setting STEP as large as possible
c     reduces the number of scaling multiplications and increases the
c     likelihood that the subroutine will be able to set DET(2) = zero.
c     These parameters added by C. L. Lawson, JPL, Aug 1987.
c     ------------------------------------------------------------------
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
c     Ref: LINPACK Users' Guide, by J. J. Dongarra, C. B. Moler,
c     J. R. Bunch, and G. W. Stewart, publ by Soc. for Indust. and Appl.
c     Math, Philadelphia, 1979.
c     Adapted from LINPACK for the JPL Math77 library by
c     C. L. Lawson, JPL, Aug 1987.
c     ------------------------------------------------------------------
c     Subprograms referenced: R1MACH
C     ------------------------------------------------------------------
      external R1MACH
      integer LDA,N,IPVT(*),  I, K
      complex A(LDA,*),DET(2), CONE, CZERO, DT1, ATERM, CNUM
      real R1MACH
      real STEP, TBIG, TSMALL, FBIG, FSMALL, DT2
      real ZERO, ONE, TEN, SIZE
      logical FIRST
      parameter(ZERO = 0.0E0, ONE = 1.0E0, TEN = 10.0E0)
      parameter(CZERO = (0.0E0,0.0E0), CONE = (1.0E0,0.0E0))
      save FIRST,STEP,FBIG,FSMALL,TBIG,TSMALL
      data FIRST / .true. /
C     ------------------------------------------------------------------
      SIZE(CNUM) = abs(real(CNUM)) + abs(aimag(CNUM))
C     ------------------------------------------------------------------
c                                     Set machine-dependent values.
      if(FIRST) then
         FIRST = .false.
         K = min(log10(R1MACH(2)/TEN), -log10(R1MACH(1)*TEN)) / 2
         STEP = K-1
         FBIG = TEN**STEP
         FSMALL = ONE/FBIG
         TBIG = FBIG/TEN
         TSMALL = FSMALL * TEN
      endif
C                                         Initialize DT1 and DT2.
         DT1 = CONE
         DT2 = ZERO
c                                         Begin main loop.
         do 150 I = 1, N
            if (IPVT(I) .NE. I) DT1 = -DT1
            ATERM = A(I,I)
            if(ATERM .eq. CZERO) then
               DT1 = CZERO
               DT2 = ZERO
               go to 160
            endif
c
c              Scale ATERM, if necessary, to bring its magnitude into
c              the range from TSMALL to TBIG.
c
            if( SIZE(ATERM) .ge. TBIG) then
   10          ATERM = ATERM * FSMALL
               DT2 = DT2 + STEP
               if( SIZE(ATERM) .ge. TBIG) go to 10
            elseif(SIZE(ATERM) .le. TSMALL) then
   20          ATERM = ATERM * FBIG
               DT2 = DT2 - STEP
               if( SIZE(ATERM) .le. TSMALL) go to 20
            endif
c
c              Scale DT1, if necessary, to bring its magnitude into
c              the range from TSMALL to TBIG.
c
            if (SIZE(DT1) .ge. TBIG) then
               DT1 = DT1 * FSMALL
               DT2 = DT2 + STEP
            elseif (SIZE(DT1) .lt. TSMALL) then
               DT1 = DT1 * FBIG
               DT2 = DT2 - STEP
            endif
c                                         Update the determinant value.
            DT1 = ATERM*DT1
            if (DT1 .eq. CZERO) then
               DT2 = ZERO
               go to 160
            endif
  150    continue
  160 continue
      DET(1) = DT1
      DET(2) = DT2
      return
      end
