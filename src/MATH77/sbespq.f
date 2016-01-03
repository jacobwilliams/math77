      subroutine SBESPQ(X,V,P,Q)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2001-05-25 SBESPQ Krogh Minor change for making .f90 version.
C>> 1998-10-29 SBESPQ Krogh  Moved external statement up for mangle.
C>> 1995-11-13 SBESPQ Krogh  Converted SFTRAN to Fortran
C>> 1994-10-19 SBESPQ Krogh  Changes to use M77CON
C>> 1994-04-19 SBESPQ CLL  Edited to make DP & SP files similar.
C>> 1992-03-13 SBESPQ FTK  Removed implicit statements.
C>> 1986-03-18 SBESPQ Lawson  Initial code.
c--S replaces "?": ?BESPQ, ?ERM1, ?ERV1
C
c     This subr evaluates asymptotic series for P and Q.
c     These can be used to compute Bessel functions by the formulas
c
c          J = sqrt(2/(pi*X)) * (P * cos(chi) - Q * sin(chi))
c
c          Y = sqrt(2/(pi*X)) * (P * sin(chi) + Q * cos(chi))
c     where
c          chi = X - (0.5 * V + 0.25) * pi
c
C          Reference: NBS AMS55 Eqs 9,2.9 and 9,2.10
c
c          We assume V is limited to the range [0,2].
c          To compute P with a relative accuracy of at least
c     10**(-s), X must be restricted to be not less than XPQ,
c     where XPQ = 1.1293 * s - 0.59
c     (This formula for XPQ was determined for s in the range from
c     5 to 25, and limiting V to [0,2].)
c          Let s0 = -log10( machine_eps )
c     and let s1 = s0 + .3,  s2 = s0 + .6
c     We will set XPQ using s2, and then sum the series till a
c     term less than 10**(-s1) is reached.
c     By setting XPQ using s2 we provide some tolerance to assure
c     that the series will contain a term less than 10**(-s1).
c        We add the constant term of each series in last to
c     reduce the amount of accumulated rounding error.
c
c     1984 Apr 2, JPL, C. L. Lawson and S. Chan.
c     ------------------------------------------------------------------
c
c     >     Let the terms of these two series be numbered
c     1, 3, 5,... in the P series, and 2, 4, 6,... in the Q series.
c     For X >> V these terms decrease in magnitude in the order 1, 2,
c     3, 4,... to some smallest term, say number N, and then
c     following terms increase in magnitude.
c     >     For V = 2 and for given X, let N be the number of the
c     smallest term, and let SIZE be the magitude of this
c     smallest term.  Here are some values of X, N, and -LOG10(SIZE):
c
c               X = 5    10     15     20     25     30     35
c               N =12    22     32     42     52     62     72
c     -LOG10(SIZE)= 4.79  9.35  13.81  18.23  22.63  27.02  31.40
c     ------------------------------------------------------------------
      external         R1MACH, SERM1, SERV1
      real             R1MACH
      real             A0,A1,A2,B,C11293,C59,C9,CP3,EIGHT,EMU
      real             FOUR,HALF,ONE,P,PSUM,Q,Q1,QSUM,SIG,SMALL,TERM
      real             TWO,V,X,X8,XPQ,ZERO
      parameter ( ZERO = 0.E0)
      save XPQ, SMALL
c     ----------
C
      data ONE,TWO,FOUR,EIGHT,C9 / 1.E0,2.E0,4.E0,8.E0,9.E0 /
      data XPQ, C11293, CP3, C59 / ZERO, 1.1293E0, 0.30103E0, 0.59E0/
      data HALF / 0.5E0 /
c     ------------------------------------------------------------------
C
      if ( XPQ .EQ. ZERO ) then
         SMALL = HALF * R1MACH(3)
         XPQ = C11293 * (CP3 - log10(SMALL)) - C59
      end if
c     ------------------------------------------------------------------
      if ( X .lt. XPQ  .or.  V .lt. ZERO  .or.  V .gt. TWO) then
         call SERM1('SBESPQ',1,0,
     * 'Require X .ge. XPQ and V in [0.,2.].',
     * 'X', X, ',')
         call SERV1( 'V', V, ',')
         call SERV1( 'XPQ', XPQ, '.')
         P = ZERO
         Q = ZERO
         return
      end if
c     ------------------------------------------------------------------
      EMU = FOUR * (V*V)
      X8 = EIGHT * X
      A0 = C9
      A1 = EIGHT
      A2 = EIGHT
      B = TWO
      SIG = -ONE
      PSUM = ZERO
      TERM = (EMU - ONE) / X8
      Q1 = TERM
      QSUM = ZERO
C
   20 continue
        TERM = TERM * (EMU-A0) / (B*X8)
        if ( ABS(TERM) .LE. SMALL ) go to 40
        PSUM = PSUM + SIG*TERM
        A1 = A1 + A2
        A0 = A0 + A1
        B = B + 1
c
        TERM = TERM * (EMU-A0) / (B*X8)
        if ( ABS(TERM) .LE. SMALL ) go to 40
        QSUM = QSUM + SIG*TERM
        A1 = A1 + A2
        A0 = A0 + A1
        B = B + 1
c
        SIG = -SIG
        go to 20
C
   40 continue
      P = HALF + ( HALF + PSUM )
      Q = Q1  + QSUM
      return
      end
