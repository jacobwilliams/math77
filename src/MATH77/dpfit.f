      SUBROUTINE DPFIT (M,X,Y,SIG,NMAX,SEEKN,COMTRN,CHBBAS,P,NFIT,
     * SIGFAC, W)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 DPFIT  Krogh  Changes to use M77CON
c>> 1990-08-13 DPFIT  CLL Fixed to assure NFIT .ge. 0 in OK cases.
C>> 1987-12-09 DPFIT  Lawson  Initial code.
C     Least squares polynomial fit to discrete data.
C     Uses either the Monomial or the Chebyshev basis.
C     ------------------------------------------------------------------
C     M  [integer, in]  No. of data points.
C
C     (X(I),I=1,M)  [float, in]  Abcissas of data.
C
C     (Y(I),I=1,M)  [float, in]  Ordinates of data.
C
C     (SIG(I),I=1,M)  [float, in]  Standard deviations of data Y().
C          If SIG(1) .lt. 0., the subr will funcrtion as though all
C          SIG(I) ate equal to abs(SIG(1)).
C          In this latter case SIG() can be dimensioned 1 rather than M.
C
C     NMAX [integer, in]  Specifies the highest degree polynomial to be
c          considered.
C
C     SEEKN  [logical, in]  If .true. this subr will determine the
c           optimal degree NFIT in the range [0, NMAX] for fitting the
c           given data and compute that fit.
C           If .false. this subr will do the fit with NFIT = NMAX
C           unless this produces a near-singular problem, in which case
C           NFIT will be reduced.
C
C     COMTRN  [logical, in]  If .true. this subr will compute
c           transformation parameters, P(1) and P(2), so that the
c           transformed abcissa variable ranges from -1.0 to +1.0.
C           If .false., will use P(1) and P(2) as given on entry.
C
C     CHBBAS  [logical, in]  If .true. this subr will use the Chebyshev
c           basis.  If .false., will use the Monomial basis.
C
C     (P(J),J=1,NMAX+3)  [float, inout]  P(1) and P(2) define a
c            transformation of the abcissa variable as follows:
C
C                     S = ( X - P(1) ) / P(2)
C
C           (P(I+3),I=0,...,NMAX) are polynomial coefficients computed
C           by this subr.  P(I+3) is the coeff of S**I, if the Monomial
C           bases is used and of the I-th degree Chebyshev polynomial if
C           the Chebyshev basis is used.
C           If this subr sets NFIT < NMAX then it will also set the
c           coefficients P(I+3) for I = NFIT+1, ..., NMAX to zero.
C
C     NFIT [integer, out]  On a successful return NFIT will be the
c           degree of the fitted polynomial.  It will be set as
c           described above in the specification of SEEKN.
c           If input values are inconsistent, NFIT will be set to -1
c           to indicate an error condition, and no fit will be done.
C
C     SIGFAC [float,out]  Factor by which the given SIG() values should
C           be multiplied to improve consistency with the fit, i.e.,
c           SIGFAC*SIG(i) is the a posteriori estimate of the standard
c           deviation of the error in Y(i).  In particular,
c           if all SIG(i) = 1.0, then SIGFAC is an estimate
c           of the standard deviation of the data error.
c           Let SUMSQ = the sum from 1 to M of
c               ((Residual at ith point)/SIG(i))**2
c           Then SIGFAC = sqrt(SUMSQ / max(M-NFIT-1, 1)).
C
C     W() [float, work]  Working space. Must be dimensioned at least
C           (NMAX+3)*(NMAX+3).  W() will be used in this subr as a
c           2-dim array: W(NDIM+3,NDIM+3).
C     ------------------------------------------------------------------
C     C.L.LAWSON, JPL, 1969 DEC 10
C     C.L.L., JPL, 1970 JAN 12      Calling sequence changed.
C     C.L.L., JPL, 1982 Aug 24:
c          To improve portability we are replacing use of
c          subrs BHSLR1 and BHSLR2 by SROTG and SROT.
C          This uses Givens rotations instead of Householder
c          transformations.  Accuracy should be the same.
c          Execution time will be greater.  Storage required
c          for the work array W() will be less.
c          Name changed from PFIT to LSPOL2.
c     C.L.Lawson, JPL, 1984 March 6.  Adapted to Fortran 77.
c          See type declaration for W(,).
C          Counting on the Fortran 77 rule that a DO-loop will
c          be skipped if the values of the control parameters
c          imply no iterations.
c          Name changed from LSPOL2 to [S/D]PFIT,
C     1984 APR 18 Using 'Modified Givens' rather than standard
C          Givens to reduce execution time. Requires one more
C          column in W().
c     1990-08-10 CLL. In cases of the Y() values lying randomly around
c     zero with no polynomial trend, and SEEKN = .true., the preferred
c     fit according to our degree determination test may be with no
c     coefficients at all.
c     In this case the subr formerly set NSOLVE = 0 and NFIT = -1.
c     Setting NFIT = -1 is a bad choice on two counts.  It indicates an
c     error, whereas this is not an error condition.  Also it may be
c     incompatible with subsequent programs that expect a valid
c     polynomial degree for use in polynomial evaluation.
c     Changed to set NFIT = max(NSOLVE-1, 0) so we can still set
c     NSOLVE = 0 in this case but NFIT will not be set less than 0.
c     Having NSOLVE = 0 causes all polynomial coefficients to be set to
c     zero on return.
c     ------------------------------------------------------------------
c--D replaces "?": ?PFIT, ?ERV1, ?ERM1, ?ROTMG, ?ROTM
c     Both versions use IERM1, IERV1
c     Generic intrinsic functions referenced: SQRT, MAX, MIN, ABS
c     ------------------------------------------------------------------
c           The dimensions of X(), Y(), and SIG() must be at least M.
C
      external IERM1, IERV1, DERV1, DERM1, D1MACH, DROTMG, DROTM
      logical          SEEKN,  COMTRN,  CHBBAS
      integer I, II, IDATA, IDIM, IRANK, IROW, J, LIMIT, LROW, M
      integer N, NFIT, NMAX, NP1, NP2, NP3, NSOLVE
      double precision D1MACH
      double precision CMIN, DENOM, FAC, HALF
      double precision ONE, P(NMAX+3), PARAM(5)
      double precision S, S2, SIG(*), SIGFAC, SIGMA, SIZE
      double precision T, TEMP, TEN, TENEPS, TWO
      double precision W(NMAX+3,NMAX+3), X(*), XMAX, XMIN, Y(*), ZERO
      parameter(ZERO = 0.D0, HALF = .5D0, ONE = 1.D0, TWO = 2.D0)
      parameter(TEN = 10.D0, FAC = 1.01D0 )
C     ------------------------------------------------------------------
C
C     N = NMAX = MAX DEGREE TO BE CONSIDERED.
C     NP1 = N+1 = NO. OF COEFFS IN A POLY OF DEGREE N
C     NP2 = N+2 = COL INDEX FOR y DATA AND ROW INDEX FOR RESIDUAL NORM.
C     NP3 = N+3.  Col NP3 holds the scale factors for the modified
c           Givens method.  Row NP3 is used for the entry of additional
c           data after the first NP2 data points have been entered.
C     Total array space used used in W() is NP3 rows by NP3 cols.
C
      N = NMAX
      NP1=N+1
      NP2=NP1+1
      NP3 = NP2+1
      IF (N.LT.0 .OR. M.LE.0 .OR. SIG(1).EQ.ZERO) THEN
        CALL IERM1('DPFIT',1,0,
     *    'No fit done. Require NMAX .ge. 0, M > 0, and SIG(1) .ne. 0.',
     *    'NMAX',N,',')
        CALL IERV1('M',M,',')
        CALL DERV1('SIG(1)',SIG(1),'.')
        NFIT = -1
        RETURN
      END IF
C
c     D1MACH(4) is the smallest no. that can be added to 1.0
c     and will give a no. larger than 1.0 in storage.
c
      TENEPS = TEN * D1MACH(4)
      IDIM = NP3
      SIGMA=ABS(SIG(1))
C     ------------------------------------------------------------------
C                                       COMPUTE P(1),P(2) IF REQUESTED
C
C     CHANGE OF INDEPENDENT VARIABLE IS GIVEN BY  S=(X-P(1))/P(2)
C                                             OR  X=P(1) + P(2)*S
C
      IF (COMTRN) THEN
        XMIN = X(1)
        XMAX = XMIN
        DO 30 I=2,M
        XMIN = MIN(XMIN,X(I))
   30   XMAX = MAX(XMAX,X(I))
        P(1) = (XMAX+XMIN)*HALF
        P(2) = (XMAX-XMIN)*HALF
        IF (P(2) .EQ. ZERO) P(2)=ONE
      ELSE
        IF (P(2) .EQ. ZERO) THEN
          CALL DERM1('DPFIT',2,0,
     *      'No fit done. With COMTRN = .FALSE. require P(2) .ne. 0.',
     *      'P(2)',P(2),'.')
          NFIT = -1
          RETURN
        END IF
      END IF
C
C     ------------------------------------------------------------------
C
C                               ACCUMULATION LOOP BEGINS HERE
C          IDATA COUNTS THE TOTAL NO. OF DATA POINTS ACCUMULATED.
C          LROW is the index of the row of W(,) in which the
c          new row of data will be placed.
C
      DO 150 IDATA = 1, M
        LROW = MIN( IDATA, NP3 )
        S=(X(IDATA)-P(1))/P(2)
        IF ( SIG(1) .GT. ZERO ) THEN
          SIGMA = SIG(IDATA)
          IF (SIGMA .LE. ZERO) THEN
            CALL DERM1('DPFIT',3,0,
     *        'No fit done. With SIG(1) > 0. require all SIG(I) > 0.',
     *        'SIG(1)',SIG(1),',')
            CALL IERV1('I',IDATA,',')
            CALL DERV1('SIG(I)',SIGMA,'.')
            NFIT = -1
            RETURN
          END IF
        END IF
        W(LROW,1) = ONE/SIGMA
        W(LROW,NP2) = Y(IDATA)/SIGMA
        W(LROW,NP3) = ONE
        IF (N .GT. 0) THEN
          IF (CHBBAS) THEN
C                                      Chebyshev basis
            W(LROW,2) = S / SIGMA
            S2 = TWO * S
            DO 120 J = 3,NP1
  120       W(LROW,J) = S2 * W(LROW,J-1) - W(LROW,J-2)
          ELSE
C                                      Monomial basis
            DO 100 J = 2,NP1
  100       W(LROW,J) = S * W(LROW,J-1)
          END IF
        END IF
C
C                  Accumulate new data row into triangular array.
c
         DO 145 IROW = 1,LROW-1
          CALL DROTMG(W(IROW,NP3),W(LROW,NP3),W(IROW,IROW),
     *                W(LROW,IROW),PARAM)
            IF(IROW .LT. NP2) CALL DROTM(NP2-IROW, W(IROW,IROW+1),
     *         IDIM, W(LROW,IROW+1), IDIM, PARAM)
  145    CONTINUE
  150 CONTINUE
C                                       END OF ACCUMULATION LOOP
C     ------------------------------------------------------------------
C
C     Replace Modified Givens weights by their square roots.
C
      DO 155 I = 1, MIN(NP2,M)
        W(I,NP3) = SQRT(W(I,NP3))
  155 CONTINUE
C
C     ------------------------------------------------------------------
C
C          Set IRANK = no. of leading diagonal elements of
c          the triangular matrix that are not extremely small
c          relative to the other elements in the same column.
c
         IRANK = MIN(NP1, M)
C                    Fortran 77 skips this loop if IRANK .le. 1.
         DO 240 J = 2,IRANK
            SIZE = ZERO
            DO 136 II = 1, J-1
               SIZE = MAX( SIZE, ABS(W(II,J)*W(II,NP3)) )
  136       CONTINUE
C
            IF( ABS(W(J,J)*W(J,NP3))  .LT. TENEPS * SIZE ) THEN
               IRANK = J-1
               GO TO 260
            END IF
  240    CONTINUE
  260 CONTINUE
C     ------------------------------------------------------------------
C
C         TEMPORARILY COPY RT-SIDE VECTOR INTO P()
C
      DO 160 I = 1,IRANK
  160 P(I+2) = W(I,NP2)
C     ------------------------------------------------------------------
C
C          Now we deal with 3 possible cases.
C          We must determine NSOLVE and SIGFAC for each of these cases.
c          NSOLVE is the no. of coefficients that will be computed.
c          We initially set NSOLVE = IRANK, but NSOLVE may be
c          reset to a smaller value in Case 3 below.
c     1. SEEKN = .false. and IRANK = NMAX+1
C             This is the simple case.
c     2. SEEKN = .false. and IRANK .lt. NMAX+1
C             Requires extra work to compute SIGFAC.
c     3. SEEKN = .true.
c             This requires more work to determine NSOLVE and SIGFAC.
C
      NSOLVE = IRANK
      IF( .NOT. SEEKN .AND. IRANK .EQ. NP1 ) THEN
C
C                                  Here for Case 1.
c
         TEMP = M - NP1
         IF( TEMP .EQ. ZERO ) THEN
           SIGFAC = ZERO
         ELSE
c                                      Here M .gt. NP1.
           SIGFAC = ABS(W(NP2,NP2)*W(NP2,NP3)) / SQRT( TEMP )
         END IF
         GO TO 235
      END IF
C     ------------------------------------------------------------------
C
C                       Here for Cases 2 and 3.
c
C                      Set SIZE = max abs value of elts in col NP2.
C     SIZE is used to scale quantities to avoid possible
c     trouble due to overflow or underflow.
C
      SIZE = ZERO
      DO 215 I = 1,MIN(NP2,M)
        W(I,NP2) = ABS(W(I,NP2)*W(I,NP3))
         SIZE = MAX( SIZE, W(I,NP2) )
  215 CONTINUE
c                        SIZE will be zero if and only if all of
c                        the given Y() data is zero.
c
      IF( SIZE .EQ. ZERO) THEN
         SIGFAC = ZERO
         NSOLVE = 1
         GO TO 235
      END IF
C     ------------------------------------------------------------------
C
c     Col NP2 now contains data from which sums of squares of
c     residuals can be computed for various possible settings
c     of NSOLVE.  We will set LIMIT = the largest row index to
c     be used in Col NP2 in analyzing residual norms.
c     In the usual case of M .ge. NP2, we set LIMIT = NP2.
c     Otherwise, when M .le. NP1, we set LIMIT = M+1 and set
c     W(LIMIT,NP2) = 0.  This reflects the fact that there is the
c     possibility of reducing the residual norm to zero by
c     exact interpolation when M .le. NP1.  The subr will do this
c     unless it must set NSOLVE smaller than M due to IRANK being
c     less than M.
C          Transform col NP2 so the (i+1)-st elt is
c          Sum(i) divided by SIZE**2, where Sum(i)
c          is the sum of squares of weighted residuals that
c          would be obtained if only the first i coefficients
c          were computed.
c
      IF( M .GE. NP2 ) THEN
         LIMIT = NP2
      ELSE
         LIMIT = M + 1
         W(LIMIT,NP2) = ZERO
      END IF
C
      W(LIMIT,NP2) = (W(LIMIT,NP2)/SIZE)**2
      DO 220 I = LIMIT-1, 1, -1
         W(I,NP2) = ( W(I,NP2)/SIZE )**2 + W(I+1,NP2)
  220 CONTINUE
c     ------------------------------------------------------------------
c
c     >     Do Case 3 if SEEKN is true, and Case 2 if SEEKN is false.
c
      IF( SEEKN ) THEN
c     >     Divide each W(i,NP2) by the no. of degrees of
c     >     freedom which is M - (i-1).
c     >     Then set CMIN = smallest of these quotients.
c     >     Then set NSOLVE.
c
         DENOM = M
         W(1,NP2) = W(1,NP2) / DENOM
         CMIN = W(1,NP2)
         DO 222 I = 2, IRANK + 1
            DENOM = MAX( DENOM-1, ONE)
            W(I,NP2) = W(I,NP2) / DENOM
            CMIN = MIN( CMIN, W(I,NP2) )
  222    CONTINUE
c
         TEMP = FAC * CMIN
         DO 230 I=1, IRANK+1
            IF(W(I,NP2) .LE. TEMP) THEN
               NSOLVE = I-1
               GO TO 232
            END IF
  230    CONTINUE
  232    CONTINUE
      ELSE
         DENOM = MAX(M-NSOLVE, 1)
         W(NSOLVE+1,NP2) = W(NSOLVE+1,NP2) / DENOM
      END IF
c
      SIGFAC = SIZE * SQRT( W(NSOLVE+1, NP2) )
C     ------------------------------------------------------------------
C
C                       Solve for NSOLVE coefficients.
C
  235 CONTINUE
      DO 290 I=NSOLVE,1,-1
         T = P(I+2)
c               Fortran 77 will skip this loop when I .EQ. NSOLVE.
         DO 270 J = I+1,NSOLVE
            T = T - W(I,J) * P(J+2)
  270    CONTINUE
         P(I+2) = T / W(I,I)
  290 CONTINUE
C     ------------------------------------------------------------------
C
C          Set missing high order coeffs to zero.
c
C         Counting on Fortran 77 skipping following loop if
c         NSOLVE .GE. NP1.
C
      DO 300 I = NSOLVE+1, NP1
  300 P(I+2) = ZERO
      NFIT = max(NSOLVE - 1, 0)
      RETURN
      END
