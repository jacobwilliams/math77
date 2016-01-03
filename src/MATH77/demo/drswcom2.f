c     program DRSWCOM2
c>> 1996-05-28 DRSWCOM2 Krogh Removed implicit statement.
c>> 1996-05-28 DRSWCOM2 Krogh Added external statement.
c>> 1995-10-24 DRSWCOM2 Krogh Changed misleading message.
c>> 1994-11-02 DRSWCOM2 Krogh Changes to use M77CON
c>> 1992-05-15 DRSWCOM2 CLL Removed "stop '... finished'"
c>> 1992-03-24 CLL Add parameter MXCASE.
c>> 1992-03-12 CLL
c>> 1987-10-28 Original time stamp
c  Demo driver for the SWCOMP package.  This code was adapted from the
c  test driver to reduce the number of tests and the amount of output.
c  Here we have NCASES = 1 and NN(1) = 6, whereas the test driver had
c  NCASES = 7 and NN(1) = 0.
c     The package SWCOMP computes values and derivatives using the
c     approach described by Wengert.
c     C. L. Lawson, JPL, 1971.  Revised for Fortran77, Jan 1987.
c     CLL 8/18/87. Added SWASIN & SWACOS to package.
c     CLL 8/31/87. Added SWRCHN and changed order of args in SWCHN.
c     CLL 9/1/87.  Added SWSINH, SWCOSH, SWTANH.
c     1992-05-15 CLL Removed "stop '... finished'" to simplify
c     comparison of output from different systems.
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c--S replaces "?": DR?WCOM2, ?WCOMP
c--&    ?COPY, ?MXDIF, ?PASCL, ?VECP, ?WACOS, ?WASIN
c--&    ?WATAN, ?WATN2, ?WCHN, ?WCOS, ?WCOSH, ?WDIF, ?WDIF1, ?WEXP
c--&    ?WLOG, ?WPRO, ?WPRO1, ?WPWRI, ?WQUO, ?WQUO1, ?WRCHN, ?WSET
c--&    ?WSIN, ?WSINH, ?WSQRT, ?WSUM, ?WSUM1, ?WTAN, ?WTANH
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer I, IA, ICASE, ICOUNT, II, J, MXCASE, N, NCASES, NMAX,
     1   NDIM, NP1, NW, NA, NX
      parameter(MXCASE = 7)
      parameter(NCASES = 1, NMAX = 6, NDIM = NMAX+1, NW = 20)
      parameter(NA = 4, NX = 10)
      external SMXDIF
      real             SMXDIF
      real             S(295),W(NDIM, NW), TEMP(NDIM), X(NDIM,NX)
      real             SAVE1(NDIM), SAVE2(NDIM), XEXP(NDIM), XLOG(NDIM)
      real             A(NA), FIX3(NA)
      real             ZERO, ONE, TWO, HALF, FOUR, PI, C9, C8, C7
      parameter(ZERO = 0.0e0, ONE = 1.0e0, TWO = 2.0e0, HALF = 0.50e0)
      parameter(FOUR = 4.0e0, C9 = 0.9e0, C8 = 0.8e0, C7 = 0.7e0)
      logical DETAIL, AS(NA), AC(NA)
      integer          NN(MXCASE), IPWR
      data NN/6,1,2,3,4,5,6/
      data A    / -2.0e0, -1.0e0, 0.75e0, 2.5e0/
      data FIX3 /  4*0.0e0 /
      data AS   /  .false., .true., .true., .false. /
      data AC   /  .false., .false., .true., .true. /
      data DETAIL / .false. /
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      print'(a)',
     *   ' DRSWCOM2..  Demo driver for the whole SWCOMP package.',
     *   ' Will print the numerical error in various calculations.'
      PI = ATAN(ONE) * FOUR
      FIX3(1) =  PI
      FIX3(4) = -PI
c
      if(DETAIL) then
      do 20 I=1,6
         do 10 J=1,40
            S(J)=ZERO
   10    continue
         call SPASCL(I-1,S)
         II=((I-1)*I)/2 + 1
         call SVECP  (S,II,  '0DPASCL')
   20    continue
      endif
c                                   Set X(*,1) and X(*,2)
      X(1,1) = C8
      X(1,2) = C7
      do 30 I = 2,NDIM
         X(I,1) = C8 * X(I-1,1)
         X(I,2) = -C7 * X(I-1,2)
   30 continue
c                              Loop through different values of N
      do 500 ICASE= 1, NCASES
         N=NN(ICASE)
         write(*,'(1x/1x,a,i3/1X)') '>>>>>> Tests with Nderiv = ', N
         NP1=N+1
c                                   Set W(*,1) and W(*,2)
            W(1,1) = C9
            W(1,2) = C9
            W(2,1) = C9*C9
            W(2,2) = -C9
            do 40 I = 3,NDIM
               W(I,1) = -C9*W(I-1,1)
               W(I,2) = ZERO
   40       continue
c                                                     Test SWSET
         call  SWSET  (N, C9, -C9, W(1,3))
         write(*,'(1x,a,g11.3)')
     *         'Error in SET                           =',
     *      SMXDIF(NP1,W(1,2),1, W(1,3),1)
c
c                             Test SWSUM, SWSUM1, SWDIF, SWDIF1, SWPRO1
c
      call SWSUM1(N, FOUR, X(1,1), X(1,3))
      call SWSUM (N, X(1,3), X(1,2), X(1,3))
      call SWDIF (N, X(1,3), X(1,1), X(1,3))
      call SWDIF1(N, FOUR, X(1,3), X(1,3))
      call SWPRO1(N, -ONE, X(1,3), X(1,3))
            write(*,'(1x,a,g11.3)')
     *         'Error in -(4-(((4+x)+y))-x) - y        =',
     *         SMXDIF(NP1,X(1,3),1, X(1,2),1)
c
c                                   Test SWQUO1, SWPRO1, and SWPWRI
c
      call SWQUO1(N,TWO, X(1,1), X(1,3))
      call SWPWRI(N,-1, X(1,3), X(1,4))
      call SWPRO1(N,TWO, X(1,4), X(1,5))
            write(*,'(1x,a,g11.3)')
     *         'Error in 2*((2/x)**-1) - x             =',
     *         SMXDIF(NP1,X(1,5),1, X(1,1),1)
c                                                 Test SWPRO and SWQUO
      call SWPRO(N, X(1,1), X(1,2), X(1,3))
      call SWQUO(N, X(1,3), X(1,2), X(1,4))
            write(*,'(1x,a,g11.3)')
     *         'Error in (X1*X2)/X2 - X1               =',
     *         SMXDIF(NP1,X(1,4),1, X(1,1),1)
      if(DETAIL) then
         print*,'X1, X2, X3 = X1 * X2, X4 = X3/X2'
         do 50 J = 1,4
            write(*,'(1x,I3,4g15.7/(4x,4g15.7))') J,(X(I,J),I=1,NP1)
   50    continue
      endif
      call SWQUO(N, X(1,3), X(1,2), X(1,3))
            write(*,'(1x,a,g11.3)')
     *         'Error in (X1*X2)/X2 - X1               =',
     *         SMXDIF(NP1,X(1,3),1, X(1,1),1)
      if(DETAIL) then
         print*,' '
         print*,'X3 = X3/X2'
         write(*,'(1x,I3,4g15.7)') 3,(X(I,3),I=1,NDIM)
      endif
c
c                             Test SWLOG and SWEXP, uses SWCHN
c
          call  SWLOG  (N,W(1,1),W(1,2))
          call  SWEXP (N,W(1,2),W(1,3))
         write(*,'(1x,a,g11.3)')
     *      'Error in Exp(Log(2)) - 2               =',
     *      SMXDIF(NP1,W(1,3),1, W(1,1),1)
         if(DETAIL) then
          call  SVECP(W(1,1),NP1,   '0W1 = T = 2.')
          call  SVECP (W(1,2),NP1,   '0W2 = LOG(W1)')
          call  SVECP (W(1,3),NP1,   '0W3 = EXP(W2). SHOULD MATCH W1.')
          call  SVECP (W(1,4),NP1,   '0W4 = 0.+W1. SCALAR ADD.')
         endif
c
c                                               Test SWCHN and SWRCHN
      call SWSET(N, HALF, ONE, X(1,8))
      call SWEXP(N, X(1,8), XEXP)
      call SWSET(N, HALF, ONE, X(1,3))
      call SWRCHN(N, XEXP, X(1,3))
      if(DETAIL) then
      print*,' '
      print*,'X1, X2 = EXP(X1), X3 = Reversion to Log'
      do 60 J = 1,3
         write(*,'(1x,I3,4g15.7)') J,(X(I,J),I=1,NDIM)
   60 continue
      endif
c
      call SWSET(N, XEXP(1), ONE, X(1,4))
      call SWLOG(N, X(1,4), XLOG)
      call SWSET(N, X(1,4), ONE, X(1,6))
      call SWRCHN(N, XLOG, X(1,6))
      if(DETAIL) then
         print*,' '
         print*,'X4, X5 = LOG(X4), X6 = Reversion to Exp'
         do 70 J = 4,6
            write(*,'(1x,I3,4g15.7)') J,(X(I,J),I=1,NDIM)
   70    continue
      endif
            write(*,'(1x,a,g11.3)')
     *         'Error in Log(x) - Rev. of Exp(x)       =',
     *         SMXDIF(NP1,XLOG,1, X(1,3),1)
            write(*,'(1x,a,g11.3)')
     *         'Error in Exp(x) - Rev. of Log(x)       =',
     *         SMXDIF(NP1,XEXP,1, X(1,6),1)
c
      call SCOPY(NP1, X(1,1),1, X(1,7),1)
      call SCOPY(NP1, X(1,7),1, SAVE1,1)
      call SWCHN (N, XEXP, X(1,7))
      call SCOPY(NP1, X(1,7),1, SAVE2,1)
      call SWRCHN(N, XEXP, X(1,7))
            write(*,'(1x,a,g11.3)')
     *         'Error in X7;CHN(XEXP,X7);RCHN(XEXP,X7) =',
     *         SMXDIF(NP1,SAVE1,1, X(1,7),1)
      if(DETAIL) then
         print*,' '
         print*,'X7 ='
         J = 7
         write(*,'(1x,I3,4g15.7)') J,(SAVE1(I),I=1,NDIM)
         print*,'call SWCHN ( N, X2, X7).  X7 ='
         write(*,'(1x,I3,4g15.7)') J,(SAVE2(I),I=1,NDIM)
         print*,'call SWRCHN ( N, X2, X7).  X7 ='
         write(*,'(1x,I3,4g15.7)') J,(X(I,J),I=1,NDIM)
      endif
c
c
      call SCOPY(NP1, X(1,1),1, X(1,7),1)
      call SCOPY(NP1, X(1,7),1, SAVE1,1)
      call SWCHN (N, XEXP, X(1,7))
      call SCOPY(NP1, X(1,7),1, SAVE2,1)
      call SWCHN(N, XLOG, X(1,7))
            write(*,'(1x,a,g11.3)')
     *         'Error in X7;CHN(XEXP,X7);CHN(XLOG,X7)  =',
     *         SMXDIF(NP1,SAVE1,1, X(1,7),1)
      if(DETAIL) then
         print*,' '
         print*,'X7 ='
         J = 7
         write(*,'(1x,I3,4g15.7)') J,(SAVE1(I),I=1,NDIM)
         print*,'call SWCHN ( N, X2, X7).  X7 ='
         write(*,'(1x,I3,4g15.7)') J,(SAVE2(I),I=1,NDIM)
         print*,'call SWCHN ( N, X5, X7).  X7 ='
         write(*,'(1x,I3,4g15.7)') J,(X(I,J),I=1,NDIM)
      endif
c
      call SCOPY(NP1, X(1,1),1, X(1,7),1)
      call SCOPY(NP1, X(1,7),1, SAVE1,1)
      call SWRCHN (N, XEXP, X(1,7))
      call SCOPY(NP1, X(1,7),1, SAVE2,1)
      call SWRCHN(N, XLOG, X(1,7))
            write(*,'(1x,a,g11.3)')
     *         'Error in X7;RCHN(XEXP,X7);RCHN(XLOG,X7)=',
     *         SMXDIF(NP1,SAVE1,1, X(1,7),1)
      if(DETAIL) then
         print*,' '
         print*,'X7 ='
         J = 7
         write(*,'(1x,I3,4g15.7)') J,(SAVE1(I),I=1,NDIM)
         print*,'call SWRCHN ( N, X2, X7).  X7 ='
         write(*,'(1x,I3,4g15.7)') J,(SAVE2(I),I=1,NDIM)
         print*,'call SWRCHN ( N, X5, X7).  X7 ='
         write(*,'(1x,I3,4g15.7)') J,(X(I,J),I=1,NDIM)
      endif
c                                         Test SWPRO and SWSQRT
c
         call  SCOPY (NP1, W(1,1),1, W(1,4),1)
         do 80 I=1,3
            call SWPRO (N,W(1,4),W(1,4),W(1,4))
            if(I .eq. 2) call SCOPY(NP1, W(1,4),1, TEMP,1)
            if(DETAIL) call SVECP  (W(1,4),NP1,   '0W4 = W4*W4')
   80    continue
c
          call SWSQRT (N, W(1,4), W(1,5) )
         write(*,'(1x,a,g11.3)')
     *         'Error in Sqrt(x*x) - x                 =',
     *      SMXDIF(NP1,TEMP,1, W(1,5),1)
         if(DETAIL) call SVECP  (W(1,5), NP1,     '0W5=SQRT(W4)')
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                           Test of SWPWRI( ,0, , )
c
         call SWPWRI(N, 0, X(1,1), X(1,3))
         call SWSET (N, ONE, ZERO, X(1,4))
         write(*,'(1x,a,g11.3)')
     *         'Error in x**0 - 1                      =',
     *      SMXDIF(NP1,X(1,3),1, X(1,4),1)
c
c                                     Loop through tests of SWPWRI
         do 100 IPWR = 1,3
            write(*,'(1x/1x,a,i3)') 'Test SWPWRI using I = ',IPWR
            call SWPWRI(N, IPWR, X(1,1), X(1,3))
            call SCOPY(NP1, X(1,1),1, X(1,4),1)
            do 90 ICOUNT = 2,IPWR
               call SWPRO(N, X(1,1), X(1,4), X(1,4))
   90       continue
            write(*,'(1x,a,g11.3)')
     *         'Error in x**I - x * x * ... * x        =',
     *      SMXDIF(NP1,X(1,3),1, X(1,4),1)
            call SWPWRI(N, -IPWR, X(1,1), X(1,5))
            call SWPRO(N, X(1,5), X(1,4), X(1,6))
            call SWSET (N, ONE, ZERO, X(1,7))
            write(*,'(1x,a,g11.3)')
     *         'Error in x**(-I) * x**I - 1            =',
     *      SMXDIF(NP1,X(1,6),1, X(1,7),1)
  100    continue
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                        Loop through quadrants to test trig functions.
c
         do 110 IA=1,NA
            write(*,'(1x/1x,a,f11.4)') 'Using angle argument,',A(IA)
            call SCOPY (NP1, W(1,1),1, W(1,5),1)
            W(1,5) = A(IA)
c                                              Test SWSIN and SWASIN
            call SWSIN  (N,W(1,5),W(1,6))
            if(AS(IA)) then
            call SWASIN  (N,W(1,6),W(1,13))
            write(*,'(1x,a,g11.3)')
     *         'Error in Asin(Sin(x)) - x              =',
     *         SMXDIF(NP1,W(1,5),1, W(1,13),1)
            endif
c                                              Test SWCOS and SWACOS
            call SWCOS  (N,W(1,5),W(1,7))
            if(AC(IA)) then
            call SWACOS  (N,W(1,7),W(1,14))
            write(*,'(1x,a,g11.3)')
     *         'Error in Acos(Cos(x)) - x              =',
     *         SMXDIF(NP1,W(1,5),1, W(1,14),1)
            endif
c                                               Test SWTAN
            call SWTAN(N, W(1,5), W(1,8))
            call SWQUO (N,W(1,6),W(1,7),W(1,9))
            write(*,'(1x,a,g11.3)')
     *         'Error in Tan(x) - Sin(x)/Cos(x)        =',
     *         SMXDIF(NP1,W(1,8),1, W(1,9),1)
c                                                Test SWATN2, SWATAN
            call SWATN2(N,W(1,6),W(1,7),W(1,9))
            call SWATAN (N,W(1,8),W(1,10))
            call SCOPY(NP1,W(1,9),1, TEMP,1)
            TEMP(1) = TEMP(1) + FIX3(IA)
            write(*,'(1x,a,g11.3)')
     *         'Error in FIX + Atan2(y,x) - Atan(y/x)  =',
     *         SMXDIF(NP1,TEMP,1, W(1,10),1)
            call SWSET(N,ONE,ZERO,W(1,11) )
            call SWATN2(N,W(1,8), W(1,11), W(1,12))
            write(*,'(1x,a,g11.3)')
     *         'Error in Atan2(y/x, 1) - Atan(y/x)     =',
     *         SMXDIF(NP1,W(1,12),1, W(1,10),1)
 
         if(DETAIL) then
            call SVECP  (W(1,5),NP1,   '0W5 = ANGLE(IA)')
            call SVECP  (W(1,6),NP1,   '0W6 = SIN(W5)')
            call SVECP  (W(1,13),NP1,
     *                     '0W13 = ASIN(W6).  Compare with W5.')
            call SVECP  (W(1,7),NP1,   '0W7 = COS(W5)')
            call SVECP  (W(1,14),NP1,
     *                  '0W13 = Acos(W7).  Compare with W5.')
            call SVECP  (W(1,8),NP1,   '0W8 = W6/W7')
            call SVECP  (W(1,9),NP1,
     *              '0W9 = ATAN2(W6,W7).   SHOULD MATCH W5')
            call SVECP  (W(1,10),NP1,
     *  '0W10 = ATAN(W8).     SHOULD MATCH W5 OR DIFFER BY 3.14159265')
            call SVECP(W(1,12),NP1,   '0W12=ATAN2(W8,1.)' )
         endif
  110    continue
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c                                    Test SWSINH, SWCOSH,  TANH
c     First check identity: cosh**2 = sinh**2 + 1
c
      call SWSINH(N, X(1,1), X(1,3))
      call SWPRO(N, X(1,3), X(1,3), X(1,6))
      call SWSUM1(N, ONE, X(1,6), X(1,8))
      call SWCOSH(N, X(1,1), X(1,4))
      call SWPRO(N, X(1,4), X(1,4), X(1,7))
            write(*,'(1x,a,g11.3)')
     *         'Error in cosh**2 - sinh**2 - 1         =',
     *         SMXDIF(NP1,X(1,7),1, X(1,8),1)
c
c                                 Check identity: Tanh = Sinh/Cosh
c
      call SWQUO(N, X(1,3), X(1,4), X(1,9))
      call SWTANH(N, X(1,1), X(1,5))
            write(*,'(1x,a,g11.3)')
     *         'Error in tanh - sinh/cosh              =',
     *         SMXDIF(NP1,X(1,5),1, X(1,9),1)
  500 continue
      end
c     ==================================================================
      real             function SMXDIF(N,X,INCX, Y,INCY)
c
c     Compute max norm of difference between the N-vectors x and y.
c     The vectors are stored with a storage increment of INCX and INCY
c     between successive components.
c     C. L. Lawson, JPL, Sept 1987.
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer I, INCX, INCY, IX, IY, N
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
