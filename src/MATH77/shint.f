      real             function SHINT (X,NDERIV,NTAB,XTAB,YTAB,YPTAB)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 SHINT  Krogh   Declared all vars.
C>> 1994-10-20 SHINT  Krogh  Changes to use M77CON
C>> 1987-12-09 SHINT  Lawson  Initial code.
c--S replaces "?": ?HINT
c
c          This subprogram does look-up and Hermite cubic interpolation
c     to evaluate a function at X defined by the data
c     NTAB, XTAB(), YTAB(), & YPTAB().
c     The values in XTAB() must be either strictly increasing or
c     strictly decreasing.
c     Optionally this subprogram will evaluate either the function or
c     its first, second, or third derivative.
c
c     The look-up method is a linear search beginning with the index,
c     LOOK, saved internally from the previous reference to this
c     subprogram.  LOOK is reset if it exceeds the current NTAB.
c     Evaluation at an interior tabular point uses data from the
c     interval in the direction of decreasing abcissa values.
c     Evaluation outside the table (extrapolation) uses data from the
c     nearest tabular interval.
c     ------------------------------------------------------------------
c     Based on subprograms FXDP and DYDXDP by Lawson, Hanson, Lang, and
c     Campbell, JPL, 1968-1974.
c     Modified July 1984 and July 1987 by C. L. Lawson, JPL, for
c     inclusion in the MATH 77 library.
c     ------------------------------------------------------------------
c     X  [in]  Argument at which interpolation is to be done.
c           For best results X should be between XTAB(1) and XTAB(NTAB),
c           however this subprogram will give an extrapolated
c           value when X is outside this range.
c
c     NDERIV  [in]   = 0 to compute function value.
c                    = 1 to compute value of first derivative.
c                    = 2 to compute value of second derivative.
c                    = 3 to compute value of third derivative.
c
c     NTAB  [in]  No. of points in XTAB(), YTAB(), and YPTAB().
c                 Require NTAB .ge. 2.
c
c     XTAB()  [in]  Array of abcissas.  Values must either be strictly
c                   increasing or strictly decreasing.
c
c     YTAB()  [in]  Array of function values.
c                   YTAB(i) = f(XTAB(i))
c
c     YPTAB()  [in]  Array of first derivative values.
c                    YPTAB(i) = f'(XTAB(i))
c     ------------------------------------------------------------------
      integer NDERIV, NTAB, LOOK, LOOK1
      real             XTAB(NTAB), YTAB(NTAB), YPTAB(NTAB)
      real             ONE, TWO, THREE, SIX, Q0, Q1, Q2, Q3, Q4, Q5,R,X
      parameter(ONE = 1.0E0, TWO = 2.0E0, THREE = 3.0E0, SIX = 6.0E0)
      save LOOK
      data  LOOK/1/
c     ------------------------------------------------------------------
C
C
      if(XTAB(1) .lt. XTAB(2)) then
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                             Here when values in XTAB() are increasing.
      IF( LOOK .GE. NTAB ) LOOK = max(1,NTAB/2)
C                                      Here  1 .le. LOOK .le. NTAB-1
      IF(X .gt. XTAB(LOOK+1)) GO TO 30
C                 Search toward decreasing abcissae, decreasing indices.
   25 IF(LOOK .eq. 1 ) GO TO 40
      IF(X .gt. XTAB(LOOK)) GO TO 40
      LOOK=LOOK-1
      GO TO 25
C                 Search toward increasing abcissae, increasing indices.
   30 IF(LOOK .eq. NTAB-1) GO TO 40
   35 LOOK=LOOK+1
      IF(LOOK .eq. NTAB-1) GO TO 40
      IF(X .gt. XTAB(LOOK+1)) GO TO 35
   40 LOOK1 = LOOK+1
      else
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                             Here when values in XTAB() are decreasing.
      IF( LOOK .gt. NTAB .or. LOOK .lt. 2 ) LOOK = max(2,NTAB/2)
C
C                         Here  2 .le. LOOK .le. NTAB
C
      IF(X .gt. XTAB(LOOK-1)) GO TO 50
C                 Search toward decreasing abcissae, increasing indices.
   45 IF(LOOK .eq. NTAB ) GO TO 60
      IF(X .gt. XTAB(LOOK)) GO TO 60
      LOOK=LOOK+1
      GO TO 45
C                 Search toward increasing abcissae, decreasing indices.
   50 IF(LOOK .eq. 2) GO TO 60
   55 LOOK=LOOK-1
      IF(LOOK .eq. 2) GO TO 60
      IF(X .gt. XTAB(LOOK-1)) GO TO 55
   60 LOOK1 = LOOK-1
      endif
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c              Here XTAB(LOOK) < XTAB(LOOK1) and X is in this interval
c              unless we are extrapolating.
c              Evaluate the Hermite interpolation formulas.
C
      R=XTAB(LOOK1)-XTAB(LOOK)
      Q0=(X-XTAB(LOOK))/R
      Q1=Q0-ONE
C
      go to (100, 101, 102, 103), NDERIV+1
C
  100 Q4=TWO*Q0
      Q2=ONE+Q4
      Q3=THREE-Q4
      Q4=Q1**2
      Q5=Q0**2
      SHINT  =Q4*(Q2*YTAB(LOOK)+R*Q0*YPTAB(LOOK))+
     *        Q5*(Q3*YTAB(LOOK1)+R*Q1*YPTAB(LOOK1))
      return
C
  101 Q2=THREE*Q0
      Q3=SIX*Q0/R
      SHINT  =Q1*(Q3*(YTAB(LOOK)-YTAB(LOOK1))+
     *        (Q2-ONE)*YPTAB(LOOK)) + Q0*(Q2-TWO)*YPTAB(LOOK1)
      return
c
  102 Q2 = Q1 + Q0
      SHINT = (TWO/R) * ( (THREE/R) * (YTAB(LOOK)-YTAB(LOOK1)) * Q2 +
     *        YPTAB(LOOK)*(Q2+Q1) + YPTAB(LOOK1)*(Q2+Q0) )

      return
c
  103 SHINT = (SIX/R**2) * ( (TWO/R) * (YTAB(LOOK)-YTAB(LOOK1)) +
     *                       YPTAB(LOOK) + YPTAB(LOOK1) )
      return
C
      end
