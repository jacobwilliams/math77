c     program DRSUCOM2
c>> 1996-06-20 DRSUCOM2 Krogh Format changes for C conversion.
c>> 1996-05-28 DRSUCOM2 Krogh Added external statement.
c>> 1994-11-02 DRSUCOM2 Krogh  Changes to use M77CON
c>> 1994-08-12 DRSUCOM2 CLL New subroutine: SUSETN
c>> 1992-05-15 CLL Removed "stop '... finished'"
c>> 1992-04-21 CLL
c>> 1992-03-16 CLL
c>> 1992-03-12 CLL
c>> 1987-10-30 Original time stamp
c  Demo driver for the SUCOMP package.  This code was adapted from the
c  test driver to reduce the number of tests and the amount of output.
c  Here we have NCASES = 2 and MM(3:4) = (0,2), whereas the test driver
c  had NCASES = 12 and MM(3:4) = (0,0).
c     The SUCOMP package computes first and second
c     partial derivatives.
c     C.L.Lawson,JPL, 1969 Dec 4
c     CLL, JPL, Jan 1987, Modified for Fortran 77
c     CLL, JPL, Sept 1987, Added SUSINH, SUCOSH, SUTANH, SUTAN
c     1992-05-15 CLL Removed "stop '... finished'" to simplify
c     comparison of output from different systems.
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c--S replaces "?": DR?UCOM2, ?UCOMP,  ?MXDIF, ?USETN
c--&    ?COPY , ?UACOS, ?UASIN, ?UATAN, ?UATN2, ?UCOS
c--&    ?UCOSH, ?UDIF , ?UDIF1, ?UEXP , ?ULOG , ?UPRO , ?UPRO1
c--&    ?UPWRI, ?UQUO , ?UQUO1, ?USET , ?USIN , ?USINH, ?USQRT
c--&    ?USUM , ?USUM1, ?UTAN , ?UTANH
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer N, M1, M2
      integer I, IANG, IANG4, ICOUNT, IMAX, IPWR, IPWR7
      integer J, JMAX, KEY, KK, L, L2, LOC
      integer MM(24), NCASES
      parameter(IMAX = 10, JMAX = 72, NCASES = 12)
      external SMXDIF
      real             SMXDIF
      real             U(IMAX, JMAX), TEST(IMAX), ZERO, TEN, ONE
      real             A1, A2, ANG, C, C8, C9, ERRMAX, ERROR
      parameter( ZERO = 0.0e0, TEN = 10.0e0, ONE = 1.0e0)
      parameter(A1 = -2.62e0, A2 = 1.57 e0, C8 = 0.8e0, C9 = 0.9e0)
      data MM/-1,-1,  0,2,  1,1,2,2,-1,-1,0,0,1,2,-1,-1,
     *        0,1,2,2,-1,-1,0,2/
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      print'('' DRSUCOM2..  Demo driver for the SUCOMP package.''/
     *   '' Computation of partial derivatives.''/
     *   '' Will print the numerical error in various calculations.''/
     *   '' These errors should be zero or small.'')'
      C=TEN
      N=3
      L=1 + N + (N*(N+1))/2
      M1=0
      M2=2
c
      do 900  KK=1, 2*NCASES-1, 2
         if(MM(KK) .lt. 0) then
            print '(1x/'' Test U-computation.  Set U(,) = 0.'')'
 
c                                 Zero the U() array.
            do 20 I=1,IMAX
               do 10 J=1,JMAX
                  U(I,J)=ZERO
   10          continue
   20       continue
c                                 Store values into U(,1) and U(,2)
            U(1,1) = C8
            U(1,2) = -C9
 
            do 30 I=2,L
                U(I,1) = -C8 * U(I-1,1)
                U(I,2) = -C9 * U(I-1,2)
   30       continue
c
            go to 800
         endif
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                  Set M1 and M2 for new case.
      M1=MM(KK)
      M2=MM(KK+1)
      call SUSETN(N, M1, M2)
      print '(1x/,'' N, M1, M2 ='',3i10/1x)', N,M1,M2
      if(M2 .eq. 0) then
         L2 = 1
      elseif(M2 .eq. 1) then
         L2 = 1 + N
      elseif(M2 .eq. 2) then
         L2 = 1 + N + (N*(N+1))/2
      endif
      ERRMAX = ZERO
c
      call SUPRO(U(1,1), U(1,2), U(1,3) )
      call SUQUO (U(1,3),U(1,1),U(1,4) )
      ERROR =  SMXDIF(L2,U(1,4),1, U(1,2),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in (u1*u2)/u1 - u2               ='', g11.3)',
     *  ERROR
c
      call SUSQRT (U(1,1), U(1,5) )
      call SUPWRI (2,U(1,5), U(1,6) )
      ERROR =  SMXDIF(L2,U(1,6),1, U(1,1),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in  sqrt(u1)**2 - u1             ='', g11.3)',
     *  ERROR
c
      call SUSIN  (U(1,1),U(1,7))
      call SUCOS  (U(1,1),U(1,8))
      call SUATN2(U(1,7), U(1,8), U(1,9))
      ERROR =  SMXDIF(L2,U(1,9),1, U(1,1),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in atan2(sin(u1),cos(u1)) - u1   ='', g11.3)',
     *  ERROR
c
      call SUEXP  (U(1,1), U(1,10))
      call SULOG  (U(1,10), U(1,11))
      ERROR =  SMXDIF(L2,U(1,11),1, U(1,1),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in log(exp(u1) - u1              ='', g11.3)',
     *  ERROR
c
      call SUQUO (U(1,7), U(1,8), U(1,12) )
      call SUATAN (U(1,12), U(1,13))
      ERROR =  SMXDIF(L2,U(1,13),1, U(1,1),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in atan(sin(u1)/cos(u1)) - u1    ='', g11.3)',
     *  ERROR
c
      call SUTAN(U(1,1), U(1,44))
      ERROR =  SMXDIF(L2,U(1,12),1, U(1,44),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in sin(u1)/cos(u1) - tan(u1)     ='', g11.3)',
     *  ERROR
c
      call SUSINH(U(1,1), U(1,45))
      call SUCOSH(U(1,1), U(1,46))
      call SUTANH(U(1,1), U(1,47))
      call SUQUO (U(1,45), U(1,46), U(1,48) )
      ERROR =  SMXDIF(L2,U(1,48),1, U(1,47),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in sinh(u1)/cosh(u1) - tanh(u1)  ='',g11.3)',
     *  ERROR
c
      call SUSUM  (U(1,1), U(1,2), U(1,14))
      call SUDIF   (U(1,14), U(1,1), U(1,15))
c
      ERROR =  SMXDIF(L2,U(1,15),1, U(1,2),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '(1x,''Error in ((u1+u2)-u1) - u2             ='',g11.3)',
     *  ERROR
c
      call SUSUM1 (C,U(1,1),U(1,16))
      call SUDIF1 (C,U(1,16),U(1,17))
      call SUPRO1 (C,U(1,17),U(1,18))
      call SUQUO1 (C,U(1,18),U(1,19))
      call SUQUO1 (-ONE,U(1,19),U(1,20))
      ERROR =  SMXDIF(L2,U(1,20),1, U(1,1),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in -1/(c/(c*(c-(c+u1)))) - u1    ='',g11.3)',
     *  ERROR
c
      call SUASIN(U(1,7), U(1,21))
      ERROR =  SMXDIF(L2,U(1,21),1, U(1,1),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in asin(sin(u1)) - u1            ='',g11.3)',
     *  ERROR
c
      call SUACOS(U(1,8), U(1,22))
      ERROR =  SMXDIF(L2,U(1,22),1, U(1,1),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in acos(cos(u1)) - u1            ='',g11.3)',
     *  ERROR
c
c        Test of SUSET.  We first use SUSUM1 to copy U(,1)
c        to U(,23+KEY) so it will have nonzero contents.
c        After calling SUSET it should be all zero except for 1 or 2
c        elements.  C will be stored to U(1,23+KEY).
c        When KEY = 0, everything else will be set to zero.
c        When KEY = 1, 2, or 3, and it will also set
c        U(KEY+1,23+KEY) = 1.0
c
      do 50 KEY = 0,3
         call SUSUM1(ZERO, U(1,1), U(1,23+KEY))
         call SUSET(C,KEY, U(1,23+KEY))
         do 40 I = 1,L
            TEST(I) = ZERO
   40    continue
         TEST(1) = C
         if(KEY .gt. 0) TEST(KEY+1) = ONE
      ERROR =  SMXDIF(L2,U(1,23+KEY),1, TEST,1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in SUSET(C,KEY,...)              ='',g11.3)',
     *  ERROR
   50 continue
c
c     Check quadrant resolution of SUATN2.
c
c                                 Store values into U(,1).
      ANG = A1 - A2
      do 60 IANG = 1,4
         IANG4 = 4*(IANG-1)
         ANG = ANG + A2
         call SCOPY(L, U(1,1),1, U(1,27+IANG4),1)
         U(1,27+IANG4) = ANG
         call SUSIN(U(1,27+IANG4), U(1,28+IANG4))
         call SUCOS(U(1,27+IANG4), U(1,29+IANG4))
         call SUATN2(U(1,28+IANG4), U(1,29+IANG4), U(1,30+IANG4))
         ERROR =  SMXDIF(L2,U(1,30+IANG4),1, U(1,27+IANG4),1)
         ERRMAX = max(ERRMAX, ERROR)
         print '('' Error in atan2(sin(u1),cos(u1)) - u1   ='',g11.3)',
     *     ERROR
   60 continue
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                           Test of SUPWRI( ,0, , )
c
         call SUPWRI( 0, U(1,1), U(1,49))
         call SUSET ( ONE, 0, U(1,50))
      ERROR =  SMXDIF(L2,U(1,49),1, U(1,50),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in x**0 - 1                      ='',g11.3)',
     *  ERROR
c
c                                     Loop through tests of SUPWRI
         do 80 IPWR = 1,3
            IPWR7 = 7*(IPWR-1)
            print '(1x/'' Test SUPWRI using I = '',i3)', IPWR
            call SUPWRI( IPWR, U(1,1), U(1,51+IPWR7))
            call SCOPY(L2, U(1,1),1, U(1,56+IPWR7),1)
            LOC = 0
            do 70 ICOUNT = 2,IPWR
               call SUPRO( U(1,1), U(1,56+LOC+IPWR7), U(1,57+LOC+IPWR7))
               LOC = LOC + 1
   70       continue
            ERROR =  SMXDIF(L2,U(1,51+IPWR7),1, U(1,56+LOC+IPWR7),1)
            ERRMAX = max(ERRMAX, ERROR)
            print '('' Error in x**I - x * x * ... * x        ='',
     *         g11.3)', ERROR
            call SUPWRI( -IPWR, U(1,1), U(1,53+IPWR7))
            call SUPRO( U(1,53+IPWR7), U(1,51+IPWR7), U(1,54+IPWR7))
            call SUSET ( ONE, 0, U(1,55+IPWR7))
            ERROR =  SMXDIF(L2,U(1,54+IPWR7),1, U(1,55+IPWR7),1)
            ERRMAX = max(ERRMAX, ERROR)
            print '('' Error in x**(-I) * x**I - 1            ='',
     *         g11.3)', ERROR
   80    continue
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
         print '('' ***>>> ERRMAX ='',g11.3)', ERRMAX
  800    continue
  900 continue
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
c     ==================================================================
      real             function SMXDIF(N,X,INCX, Y,INCY)
c
c     Compute max norm of difference between the N-vectors x and y.
c     The vectors are stored with a storage increment of INCX and INCY
c     between successive components.
c     C. L. Lawson, JPL, Sept 1987.
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer I, N, INCX, INCY, IX, IY
      real             X(*), Y(*), ZERO, TEMP
      parameter(ZERO = 0.0e0)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      TEMP = ZERO
      IX = 1 - INCX
      IY = 1 - INCY
      do 10 I = 1,N
         IX = IX + INCX
         IY = IY + INCY
         TEMP = max(TEMP, abs(X(IX) - Y(IY)))
   10 continue
      SMXDIF = TEMP
      return
      end
