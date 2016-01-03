      subroutine DSBASI(K, N, T, X1, X2, J1, J2, BASI)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 DSBASI Krogh  Changes to use M77CON
c>> 1993-01-12 DSBASI CLL  Using DSBASD in place of _SBAS.
c>> 1992-11-12 C. L. Lawson, JPL Made J1 & J2 inout.  Were out.
c>> 1992-11-02 C. L. Lawson, JPL
c>> 1988-04-01 C. L. Lawson, JPL
C
C   This subroutine computes the integral over [X1,X2] of each of the N
c   members of a Kth order family of B-spline basis functions defined
c   over the knot set T().  The values of these integrals will be
c   returned in BASI(j), j = 1, ..., N.  The indices J1 and J2 will be
c   set to indicate the subset of these that might be nonzero.  Thus
c   BASI(j), j = J1, ..., J2 might be nonzero, whereas the other
c   elements of BASI() will be zero.
c   Orders K as high as 20 are permitted.
c   Permits X1 .le. X2 or X1 gt. X2.  In the former case all returned
c   values will be .ge. 0, while in the latter case all will be .le. 0.
C      The "proper interpolation interval" is from T(K) to T(N+1).
c      For most reliable results, X1 and X2 should lie in this
c      interval, however this subr will give a result even if this
c      is not the case by use of extrapolation.
C      This subr applies a 2, 6, or 10 point Gauss formula on
c      subintervals of [X1,X2] which are formed by the included
c      (distinct) knots.
c     ------------------------------------------------------------------
C                           ARGUMENTS
c
C  K      [in] ORDER OF B-SPLINE, 2 .le. K .le. 20
c               Note that the polynomial degree of the segments of
c               the spline are one less than the order.
C  N      [in] LENGTH OF COEFFICIENT ARRAY
C  T()    [in] KNOT ARRAY OF LENGTH N+K
C  X1,X2  [in] END POINTS OF QUADRATURE INTERVAL.
C               Permit X1 .le. X2 or X1 .gt. X2.
c
c  J1, J2  [inout integers] On entry must contain integer values.
c     Will be passed to DSFIND for lookup.  Any integer values
c     are ok but previous returned values may aid efficiency.
c     On return will satisfy 1 .le. J1 .le. J2 .le. N.
c     Indicates that only the output values BASI(j), j = J1,...,J2
c     might be nonzero.
C  BASI()  [out, floating pt]  Array in which the N integrals will be
c           stored.
C
C     ERROR CONDITIONS: None
c     ------------------------------------------------------------------
C     Based on spline quadrature subroutines called BQUAD or BSQUAD
c     written by D. E. Amos,  Sandia, June, 1979.  Documented in
c     SAND79-1825.
c     Uses the (T, BCOEF, N, K) representation of a B-spline as
c     presented in A PRACTICAL GUIDE TO SPLINES by Carl De Boor,
c     Springer-Verlag, 1978.
c     Current version by C. L. Lawson, JPL, March 1988.
c     ------------------------------------------------------------------
C--D replaces "?": ?SBASI, ?SFIND, ?SBASD
c     ------------------------------------------------------------------
c     GPTS(), GWTS()  Index 1 selects 2-point Gaussian formula.
c                     Indices 2-4 select 6-point Gaussian formula.
c                     Indices 5-9 select 10-point Gaussian formula.
c     ------------------------------------------------------------------
      integer KMAX
      parameter(KMAX = 20)
      integer IK, IL1, IL2, J, J1, J2, JF, K, LEFT, M, MF, MODE, N
      double precision A, AA, B, BASI(N), BB
      double precision BMA, BPA, BVAL1(KMAX), BVAL2(KMAX), C1
      double precision FAC, GPTS(9), GWTS(9), HALF
      double precision T(N+K), TA, TB, X1, X2, ZERO
      parameter(HALF = 0.50D0, ZERO = 0.0D0)
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
      if(K .gt. KMAX) then
         call IERM1('DSBASI',1,2,'Require KORDER .le. KMAX.',
     *   'KORDER',K,',')
         call IERV1('KMAX',KMAX,'.')
         return
      endif
C
      AA= min(X1,X2)
      BB= max(X1,X2)
C
      IL1 = J1+K-1
      IL2 = J2
      call DSFIND(T,K, N+1,AA,IL1,MODE)
      call DSFIND(T,K, N+1,BB,IL2,MODE)
c
      if(BB .eq. T(IL2) .and. IL2 .gt. K .and. IL2 .gt. IL1)
     *    IL2 = IL2 -1
      J1 = IL1+1-K
      J2 = IL2
      do 15 J=1,N
         BASI(J) = ZERO
   15 continue
      if(AA .eq. BB) return
c
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
c
      do 40 LEFT=IL1,IL2
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
            DO 30 M=1,MF
               C1=BMA*GPTS(JF+M)
               FAC = GWTS(JF+M)*BMA
               call DSBASD(K, LEFT, T, BPA+C1, 0, BVAL1)
               call DSBASD(K, LEFT, T, BPA-C1, 0, BVAL2)
               J = LEFT-K
               do 20 IK = 1,K
                  J = J+1
                  BASI(J) = BASI(J)+FAC*(BVAL1(IK)+BVAL2(IK))
   20          continue
   30       continue
         endif
   40 continue
      if(X1 .gt. X2) then
         do 50 J=J1, J2
            BASI(J) = -BASI(J)
   50    continue
      endif
      return
      end
