      double precision function DSQUAD(K, N, T, BCOEF, X1, X2)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 DSQUAD Krogh  Added external statement.
c>> 1994-10-20 DSQUAD Krogh  Changes to use M77CON
c>> 1992-11-12 DSQUAD C. L. Lawson, JPL Saving IL1 & IL2.
c>> 1992-10-27 C. L. Lawson, JPL
c>> 1988-04-01 C. L. Lawson, JPL
C
C   This subprogram computes the integral over [X1,X2] of a K-th order
C      B-spline using the B-representation (T,BCOEF,N,K).  Orders
C      K as high as 20 are permitted.  Permits X1 .le. X2 or X1 gt. X2.
C      The "proper interpolation interval" is from T(K) to T(N+1).
c      For most reliable results, X1 and X2 should lie in this
c      interval, however this subr will give a result even if this
c      is not the case by use of extrapolation.
C      This subr applies a 2, 6, or 10 point Gauss formula on
c      subintervals of [X1,X2] which are formed by the included
c      (distinct) knots.

C     DESCRIPTION OF ARGUMENTS
C         INPUT
C           K       - ORDER OF B-SPLINE, 2 .le. K .le. 20
c                     Note that the polynomial degree of the segments of
c                     the spline are one less than the order.
C           N       - LENGTH OF COEFFICIENT ARRAY
C           T()     - KNOT ARRAY OF LENGTH N+K
C           BCOEF() - B-SPLINE COEFFICIENT ARRAY OF LENGTH N
C           X1,X2   - END POINTS OF QUADRATURE INTERVAL.
C                     Permit X1 .le. X2 or X1 .gt. X2.
C         OUTPUT
C           DSQUAD  - INTEGRAL OF THE B-SPLINE OVER [X1,X2].
C
C     ERROR CONDITIONS: None
c     ------------------------------------------------------------------
C     Original code was called BQUAD or BSQUAD.  Written by D. E. Amos,
c     Sandia, June, 1979.  Documented in SAND79-1825.
c     Uses the (T, BCOEF, N, K) representation of a B-spline as
c     presented in A PRACTICAL GUIDE TO SPLINES by Carl De Boor,
c     Springer-Verlag, 1978.
c     Current version by C. L. Lawson, JPL, March 1988.
c     ------------------------------------------------------------------
c--D replaces "?": ?SQUAD, ?SFIND, ?SVAL
c     ------------------------------------------------------------------
c     GPTS(), GWTS()  Index 1 selects 2-point Gaussian formula.
c                     Indices 2-4 select 6-point Gaussian formula.
c                     Indices 5-9 select 10-point Gaussian formula.
c     ------------------------------------------------------------------
      external DSVAL
      integer I, IL1, IL2, JF, K, LEFT, M, MF, MODE, N
      double precision DSVAL
      double precision A, AA, B, BB, BCOEF(N), BMA, BPA, C1
      double precision GPTS(9), GWTS(9), HALF
      double precision Q, SUM(5), T(N+K), TA, TB, X1, X2, ZERO
      parameter(HALF = 0.50D0, ZERO = 0.0D0)
      save IL1, IL2
      data IL1, IL2 / 1,1/
C
*     DATA GPTS            / 5.77350269189626D-01, 2.38619186083197D-01,
*    1 6.61209386466265D-01, 9.32469514203152D-01, 1.48874338981631D-01,
*    2 4.33395394129247D-01, 6.79409568299024D-01, 8.65063366688985D-01,
*    3 9.73906528517172D-01/
C
*     DATA GWTS            / 1.00000000000000D+00, 4.67913934572691D-01,
*    1 3.60761573048139D-01, 1.71324492379170D-01, 2.95524224714753D-01,
*    2 2.69266719309996D-01, 2.19086362515982D-01, 1.49451349150581D-01,
*    3 6.66713443086880D-02/
c
c     Gaussian quadrature abcissae and weights to 40 decimal digits.
      data GPTS / 0.5773502691896257645091487805019574556476d0,
     2            0.2386191860831969086305017216807119354186d0,
     3            0.6612093864662645136613995950199053470064d0,
     4            0.9324695142031520278123015544939946091348d0,
     5            0.1488743389816312108848260011297199846176d0,
     6            0.4333953941292471907992659431657841622001d0,
     7            0.6794095682990244062343273651148735757693d0,
     8            0.8650633666889845107320966884234930485275d0,
     9            0.9739065285171717200779640120844520534283d0/
c
      data GWTS / 1.0d0,
     2            0.4679139345726910473898703439895509948120d0,
     3            0.3607615730481386075698335138377161116612d0,
     4            0.1713244923791703450402961421727328935273d0,
     5            0.2955242247147528701738929946513383294210d0,
     6            0.2692667193099963550912269215694693528586d0,
     7            0.2190863625159820439955349342281631924634d0,
     8            0.1494513491505805931457763396576973323908d0,
     9            0.06667134430868813759356880989333179285686d0/
c     ------------------------------------------------------------------
C
      AA= min(X1,X2)
      BB= max(X1,X2)
      if(AA .eq. BB) then
         Q = ZERO
         go to 90
      endif
C                  Selection of Gaussian quadrature formula:
c                  KORDER .le. 4          =>   2 point formula
c                  5 .le. KORDER .le. 12  =>  6 point formula
c                  KORDER .ge. 13         =>  10 point formula
c
      if(K .le. 4) then
         JF=0
         MF=1
      elseif(K .le. 12) then
         JF=1
         MF=3
      else
         JF=4
         MF=5
      endif
C
      do 15 I=1,MF
         SUM(I)= ZERO
   15 continue
      call DSFIND(T,K, N+1,AA,IL1,MODE)
      call DSFIND(T,K, N+1,BB,IL2,MODE)
      do 20 LEFT=IL1,IL2
         TA=T(LEFT)
         TB=T(LEFT+1)
         if(TA .ne. TB) then
            if(LEFT .eq. K) then
               A = AA
            else
               A= max(AA,TA)
            endif
            if(LEFT .eq. N) then
               B = BB
            else
               B= min(BB,TB)
            endif
            BMA= HALF *(B-A)
            BPA= HALF *(B+A)
            DO 25 M=1,MF
               C1=BMA*GPTS(JF+M)
               SUM(M)=SUM(M) + BMA * (DSVAL(K, N, T, BCOEF, BPA+C1, 0)+
     *                                DSVAL(K, N, T, BCOEF, BPA-C1, 0))
   25       continue
         endif
   20 continue
      Q= ZERO
      DO 30 M=1,MF
         Q=Q+GWTS(JF+M)*SUM(M)
   30 continue
      if(X1 .gt. X2) Q=-Q
   90 continue
      DSQUAD = Q
      return
      end
