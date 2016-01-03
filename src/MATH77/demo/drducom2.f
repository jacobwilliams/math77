c     program DRDUCOM2
c>> 1996-06-20 DRDUCOM2 Krogh Format changes for C conversion.
c>> 1996-05-28 DRDUCOM2 Krogh Added external statement.
c>> 1994-11-02 DRDUCOM2 Krogh  Changes to use M77CON
c>> 1994-08-12 DRDUCOM2 CLL New subroutine: DUSETN
c>> 1992-05-15 CLL Removed "stop '... finished'"
c>> 1992-04-21 CLL
c>> 1992-03-16 CLL
c>> 1992-03-12 CLL
c>> 1987-10-30 Original time stamp
c  Demo driver for the DUCOMP package.  This code was adapted from the
c  test driver to reduce the number of tests and the amount of output.
c  Here we have NCASES = 2 and MM(3:4) = (0,2), whereas the test driver
c  had NCASES = 12 and MM(3:4) = (0,0).
c     The DUCOMP package computes first and second
c     partial derivatives.
c     C.L.Lawson,JPL, 1969 Dec 4
c     CLL, JPL, Jan 1987, Modified for Fortran 77
c     CLL, JPL, Sept 1987, Added DUSINH, DUCOSH, DUTANH, DUTAN
c     1992-05-15 CLL Removed "stop '... finished'" to simplify
c     comparison of output from different systems.
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c--D replaces "?": DR?UCOM2, ?UCOMP,  ?MXDIF, ?USETN
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
      external DMXDIF
      double precision DMXDIF
      double precision U(IMAX, JMAX), TEST(IMAX), ZERO, TEN, ONE
      double precision A1, A2, ANG, C, C8, C9, ERRMAX, ERROR
      parameter( ZERO = 0.0d0, TEN = 10.0d0, ONE = 1.0d0)
      parameter(A1 = -2.62d0, A2 = 1.57 d0, C8 = 0.8d0, C9 = 0.9d0)
      data MM/-1,-1,  0,2,  1,1,2,2,-1,-1,0,0,1,2,-1,-1,
     *        0,1,2,2,-1,-1,0,2/
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      print'('' DRDUCOM2..  Demo driver for the DUCOMP package.''/
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
      call DUSETN(N, M1, M2)
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
      call DUPRO(U(1,1), U(1,2), U(1,3) )
      call DUQUO (U(1,3),U(1,1),U(1,4) )
      ERROR =  DMXDIF(L2,U(1,4),1, U(1,2),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in (u1*u2)/u1 - u2               ='', g11.3)',
     *  ERROR
c
      call DUSQRT (U(1,1), U(1,5) )
      call DUPWRI (2,U(1,5), U(1,6) )
      ERROR =  DMXDIF(L2,U(1,6),1, U(1,1),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in  sqrt(u1)**2 - u1             ='', g11.3)',
     *  ERROR
c
      call DUSIN  (U(1,1),U(1,7))
      call DUCOS  (U(1,1),U(1,8))
      call DUATN2(U(1,7), U(1,8), U(1,9))
      ERROR =  DMXDIF(L2,U(1,9),1, U(1,1),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in atan2(sin(u1),cos(u1)) - u1   ='', g11.3)',
     *  ERROR
c
      call DUEXP  (U(1,1), U(1,10))
      call DULOG  (U(1,10), U(1,11))
      ERROR =  DMXDIF(L2,U(1,11),1, U(1,1),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in log(exp(u1) - u1              ='', g11.3)',
     *  ERROR
c
      call DUQUO (U(1,7), U(1,8), U(1,12) )
      call DUATAN (U(1,12), U(1,13))
      ERROR =  DMXDIF(L2,U(1,13),1, U(1,1),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in atan(sin(u1)/cos(u1)) - u1    ='', g11.3)',
     *  ERROR
c
      call DUTAN(U(1,1), U(1,44))
      ERROR =  DMXDIF(L2,U(1,12),1, U(1,44),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in sin(u1)/cos(u1) - tan(u1)     ='', g11.3)',
     *  ERROR
c
      call DUSINH(U(1,1), U(1,45))
      call DUCOSH(U(1,1), U(1,46))
      call DUTANH(U(1,1), U(1,47))
      call DUQUO (U(1,45), U(1,46), U(1,48) )
      ERROR =  DMXDIF(L2,U(1,48),1, U(1,47),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in sinh(u1)/cosh(u1) - tanh(u1)  ='',g11.3)',
     *  ERROR
c
      call DUSUM  (U(1,1), U(1,2), U(1,14))
      call DUDIF   (U(1,14), U(1,1), U(1,15))
c
      ERROR =  DMXDIF(L2,U(1,15),1, U(1,2),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '(1x,''Error in ((u1+u2)-u1) - u2             ='',g11.3)',
     *  ERROR
c
      call DUSUM1 (C,U(1,1),U(1,16))
      call DUDIF1 (C,U(1,16),U(1,17))
      call DUPRO1 (C,U(1,17),U(1,18))
      call DUQUO1 (C,U(1,18),U(1,19))
      call DUQUO1 (-ONE,U(1,19),U(1,20))
      ERROR =  DMXDIF(L2,U(1,20),1, U(1,1),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in -1/(c/(c*(c-(c+u1)))) - u1    ='',g11.3)',
     *  ERROR
c
      call DUASIN(U(1,7), U(1,21))
      ERROR =  DMXDIF(L2,U(1,21),1, U(1,1),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in asin(sin(u1)) - u1            ='',g11.3)',
     *  ERROR
c
      call DUACOS(U(1,8), U(1,22))
      ERROR =  DMXDIF(L2,U(1,22),1, U(1,1),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in acos(cos(u1)) - u1            ='',g11.3)',
     *  ERROR
c
c        Test of DUSET.  We first use DUSUM1 to copy U(,1)
c        to U(,23+KEY) so it will have nonzero contents.
c        After calling DUSET it should be all zero except for 1 or 2
c        elements.  C will be stored to U(1,23+KEY).
c        When KEY = 0, everything else will be set to zero.
c        When KEY = 1, 2, or 3, and it will also set
c        U(KEY+1,23+KEY) = 1.0
c
      do 50 KEY = 0,3
         call DUSUM1(ZERO, U(1,1), U(1,23+KEY))
         call DUSET(C,KEY, U(1,23+KEY))
         do 40 I = 1,L
            TEST(I) = ZERO
   40    continue
         TEST(1) = C
         if(KEY .gt. 0) TEST(KEY+1) = ONE
      ERROR =  DMXDIF(L2,U(1,23+KEY),1, TEST,1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in DUSET(C,KEY,...)              ='',g11.3)',
     *  ERROR
   50 continue
c
c     Check quadrant resolution of DUATN2.
c
c                                 Store values into U(,1).
      ANG = A1 - A2
      do 60 IANG = 1,4
         IANG4 = 4*(IANG-1)
         ANG = ANG + A2
         call DCOPY(L, U(1,1),1, U(1,27+IANG4),1)
         U(1,27+IANG4) = ANG
         call DUSIN(U(1,27+IANG4), U(1,28+IANG4))
         call DUCOS(U(1,27+IANG4), U(1,29+IANG4))
         call DUATN2(U(1,28+IANG4), U(1,29+IANG4), U(1,30+IANG4))
         ERROR =  DMXDIF(L2,U(1,30+IANG4),1, U(1,27+IANG4),1)
         ERRMAX = max(ERRMAX, ERROR)
         print '('' Error in atan2(sin(u1),cos(u1)) - u1   ='',g11.3)',
     *     ERROR
   60 continue
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                           Test of DUPWRI( ,0, , )
c
         call DUPWRI( 0, U(1,1), U(1,49))
         call DUSET ( ONE, 0, U(1,50))
      ERROR =  DMXDIF(L2,U(1,49),1, U(1,50),1)
      ERRMAX = max(ERRMAX, ERROR)
      print '('' Error in x**0 - 1                      ='',g11.3)',
     *  ERROR
c
c                                     Loop through tests of DUPWRI
         do 80 IPWR = 1,3
            IPWR7 = 7*(IPWR-1)
            print '(1x/'' Test DUPWRI using I = '',i3)', IPWR
            call DUPWRI( IPWR, U(1,1), U(1,51+IPWR7))
            call DCOPY(L2, U(1,1),1, U(1,56+IPWR7),1)
            LOC = 0
            do 70 ICOUNT = 2,IPWR
               call DUPRO( U(1,1), U(1,56+LOC+IPWR7), U(1,57+LOC+IPWR7))
               LOC = LOC + 1
   70       continue
            ERROR =  DMXDIF(L2,U(1,51+IPWR7),1, U(1,56+LOC+IPWR7),1)
            ERRMAX = max(ERRMAX, ERROR)
            print '('' Error in x**I - x * x * ... * x        ='',
     *         g11.3)', ERROR
            call DUPWRI( -IPWR, U(1,1), U(1,53+IPWR7))
            call DUPRO( U(1,53+IPWR7), U(1,51+IPWR7), U(1,54+IPWR7))
            call DUSET ( ONE, 0, U(1,55+IPWR7))
            ERROR =  DMXDIF(L2,U(1,54+IPWR7),1, U(1,55+IPWR7),1)
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
      double precision function DMXDIF(N,X,INCX, Y,INCY)
c
c     Compute max norm of difference between the N-vectors x and y.
c     The vectors are stored with a storage increment of INCX and INCY
c     between successive components.
c     C. L. Lawson, JPL, Sept 1987.
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer I, N, INCX, INCY, IX, IY
      double precision X(*), Y(*), ZERO, TEMP
      parameter(ZERO = 0.0d0)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      TEMP = ZERO
      IX = 1 - INCX
      IY = 1 - INCY
      do 10 I = 1,N
         IX = IX + INCX
         IY = IY + INCY
         TEMP = max(TEMP, abs(X(IX) - Y(IY)))
   10 continue
      DMXDIF = TEMP
      return
      end
