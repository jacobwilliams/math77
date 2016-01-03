      program DRDJACG1
c>>  2006-04-12 DRDJACG1 Hanson -- Reduced lengths of djacg work arrays.      
c>>  2003-07-08 DRDJACG1 Hanson -- Check for MODE < 0.
c>>  2003-07-07 DRDJACG1 Krogh -- Changed 3 arg max to 2 arg for C conv.
c>>  2002-06-21 DRDJACG1 R. J. Hanson Example 1 Code, with Download
c     Demo driver for DJACG, using numerical derivatives for a gradient.
c     ------------------------------------------------------------------
c--   D replaces "?": DR?JACG1, ?JACG

c
c     The function used for this demo is f(y_1,y_2) = a*exp(b*y_1)+c*y_1
C     *(y_2)**2.
c     Its gradient vector is (df/dy_1,df/dy_2)=
C     (a*b*exp(b*y_1)+c*(y_2)**2, 2*c*y_1*y_2).
C     This is used for comparison of the computed results with actual
C     results.

C     This driver shows how to:

C     A. Compute approximate derivatives using one-sided divided
C     differences
C     B. Use one-sided differences on the first component and
C     analytically compute the second component
C     C. Accumulate a known term of the first component with a
C     differenced term that is not known a priori
C     D. Use central differences on both components
C     (No checking for error conditions, i.e. MODE < 0.)
C     Define sizes and parameters.

      INTEGER I, MODE, M, N, LDFJAC, LWK, LIWK
      PARAMETER(M=1, N=2, LDFJAC=M, LWK=3*M+18, LIWK=21)
      INTEGER IOPT(04), IWK(LIWK)
      DOUBLE PRECISION Y(N), F(M), FJAC(LDFJAC,N), XSCALE(N),
     .     FAC(N), WK(LWK), ACTUAL(LDFJAC,N), RE(4,2)
      DOUBLE PRECISION A, B, C, F2, U, MAXERR, D1MACH
      CHARACTER*55 WHAT(4)
      CHARACTER*1 uv
      DATA WHAT /
     .     'One sided partials, default settings',
     .     'One sided partial, second partial known and skipped',
     .     'One sided partials, first partial accumulated',
     .     'Central difference partials' /

C     Define data and point of evaluation:
      A=2.5D0
      B=3.4D0
      C=4.5D0

      Y(1)=2.1D0
      Y(2)=3.2D0
C     Machine precision, for measuring errors
      U=D1MACH(4)
C     Set defaults for increments and scaling:
      FAC(1)=0.D0
      XSCALE(1)=0.D0
C     Compute true values of partials.
      ACTUAL(1,1)=A*B*EXP(B*Y(1))+C*Y(2)**2
      ACTUAL(1,2)=2*C*Y(1)*Y(2)

C     A. No variable gets special treatment
      IOPT(1)=0
C     Start each problem with MODE=0.  Other starting
C     values are errors.  Values < 0 or > N are caught.
      MODE=0

 10   CONTINUE
      WK(1)=A*EXP(B*Y(1))+C*Y(1)*Y(2)**2
C      This sets the function value used in forming one-sided differences.
      IF(MODE .eq. 0) THEN
         F(1)=WK(1)
      END IF
      CALL DJACG(MODE,M,N,Y,F,
     .     FJAC,LDFJAC,XSCALE,FAC,IOPT,
     .     WK,LWK,IWK,LIWK)
      IF(MODE .gt. 0) GO TO 10
C     Check for an error condition.
      IF(MODE .lt. 0) THEN
         PRINT '('' Initial error in argument number '',I2)',-MODE
         GO TO 60
      END IF

C     Check the relative accuracy of one-sided differences.
C     They should be good to about half-precision.
      FJAC(1,1)=(FJAC(1,1)-ACTUAL(1,1))/ACTUAL(1,1)
      FJAC(1,2)=(FJAC(1,2)-ACTUAL(1,2))/ACTUAL(1,2)
      RE(1,1)=FJAC(1,1)/sqrt(U)
      RE(1,2)=FJAC(1,2)/sqrt(U)

C     B. Skip variable number 2.
      IOPT(1)= 1
      IOPT(2)=-4
      IOPT(3)= 2
      MODE=0

 20   CONTINUE
      WK(1)=A*EXP(B*Y(1))+C*Y(1)*Y(2)**2
      IF(MODE .eq. 0) THEN
         F(1)=WK(1)
C     The second component partial is skipped,
C     since it is known analytically
         FJAC(1,2)=2.D0*C*Y(1)*Y(2)
      END IF
      CALL DJACG(MODE,M,N,Y,F,
     .     FJAC,LDFJAC,XSCALE,FAC,IOPT,
     .     WK,LWK,IWK,LIWK)
      IF(MODE .gt. 0) GO TO 20

C     Check for an error condition.
      IF(MODE .lt. 0) THEN
        PRINT '('' Initial error in argument number '',I2)',-MODE
        GO TO 60
      END IF
      FJAC(1,1)=(FJAC(1,1)-ACTUAL(1,1))/ACTUAL(1,1)
      FJAC(1,2)=(FJAC(1,2)-ACTUAL(1,2))/ACTUAL(1,2)
      RE(2,1)=FJAC(1,1)/sqrt(U)
      RE(2,2)=FJAC(1,2)/sqrt(U)

C     C. Accumulate a part of the first partial.
      IOPT(1)=-3
      IOPT(2)= 1
C        Shift to using one-sided differences for the
C        rest of the variables.
      IOPT(3)=-1
      IOPT(4)=0

      MODE=0

 30   CONTINUE
C     Since part of the partial is known, evaluate what is
C     to be differenced.
      IF(MODE .ne. 2) WK(1)=A*EXP(B*Y(1))
      IF(MODE .eq. 0) THEN
C     Start with part of the derivative that is known.
         F(1)=WK(1)
         FJAC(1,1)=C*Y(2)**2
C     This is the function value for the partial wrt y_2.
         F2=C*Y(1)*Y(2)**2
      END IF

      IF(MODE .eq. 2) THEN
C     The function value for the second partial has the part removed
C     that depends on the first variable only.
         F(1)=F2
         WK(1)=C*Y(1)*Y(2)**2
      END IF
      CALL DJACG(MODE,M,N,Y,F,
     .     FJAC,LDFJAC,XSCALE,FAC,IOPT,
     .     WK,LWK,IWK,LIWK)

      IF(MODE .gt. 0) GO TO 30
C     Check for an error condition.
      IF(MODE .lt. 0) THEN
        PRINT '('' Initial error in argument number '',I2)',-MODE
        GO TO 60
      END IF

      FJAC(1,1)=(FJAC(1,1)-ACTUAL(1,1))/ACTUAL(1,1)
      FJAC(1,2)=(FJAC(1,2)-ACTUAL(1,2))/ACTUAL(1,2)
      RE(3,1)=FJAC(1,1)/sqrt(U)
      RE(3,2)=FJAC(1,2)/sqrt(U)

C     D. Use central differences and get more accuracy.
C        Twice the function evaluations are needed.
      IOPT(1)=-2
      IOPT(2)= 0

C     Set the increment used at the default value.
C     This value must be assigned when using central differences.
      WK(3*M+3)=0.D0

      MODE=0
 40   CONTINUE
      WK(1)=A*EXP(B*Y(1))+C*Y(1)*Y(2)**2
      IF(MODE .eq. 0) THEN
         F(1)=WK(1)
      END IF
      CALL DJACG(MODE,M,N,Y,F,
     .     FJAC,LDFJAC,XSCALE,FAC,IOPT,
     .     WK,LWK,IWK,LIWK)
      IF(MODE .gt. 0) GO TO 40
C     Check for an error condition.
      IF(MODE .lt. 0) THEN
         PRINT '('' Initial error in argument number '',I2)',-MODE
         GO TO 60
      END IF

C     Check the relative accuracy of central differences.
C     They should be good to about two thirds-precision.
      FJAC(1,1)=(FJAC(1,1)-ACTUAL(1,1))/ACTUAL(1,1)
      FJAC(1,2)=(FJAC(1,2)-ACTUAL(1,2))/ACTUAL(1,2)
      F2=(3.D0*U)**(2.D0/3.D0)
      RE(4,1)=FJAC(1,1)/F2
      RE(4,2)=FJAC(1,2)/F2

C     Output the results and what is expected.

      PRINT '(''Rel Err of partials, f= a*exp(b*y_1)+c*y_1*(y_2)**2.''/
     .''Case  df/dy_1  df/dy_2, u=macheps**.5, v=(3*macheps)**(2/3)'')'

      MAXERR=0.D0
      UV='u'
      DO 50 I=1,4
         MAXERR=max(MAXERR, max(ABS(RE(I,1)), ABS(RE(I,2))))
         IF(I .eq. 4) uv='v'
         PRINT '(I3, 2(F7.2,A1,2x), A55)',
     .            I,RE(I,1), uv, RE(I,2), uv, WHAT(I)
 50   CONTINUE
C     All expected relative errors (in units of truncation error)
C     should not exceed 8.  If they do there may be an error.
      IF(MAXERR .le. 8.D0) THEN
         PRINT '('' Numbers above with absolute values .le. 8 are '',
     . ''considered acceptable.'')'
      ELSE
         PRINT '('' Numbers above with absolute values .gt. 8 are '',
     . ''considered unacceptable.'')'
      END IF
 60   CONTINUE
      END 
