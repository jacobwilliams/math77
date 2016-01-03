c     program DRSUCOMP
c>> 1994-10-19 DRSUCOMP Krogh  Changes to use M77CON
c>> 1994-08-04 DRSUCOMP CLL New subroutine: SUSETN
c>> 1992-02-17 CLL
c>> 1990-12-13 CLL Added demo of SUREV.
c>> 1987-12-09 DRSUCOMP Lawson  Initial Code.
c     Demo driver for the SUCOMP package, including SUREV.
c     The SUCOMP package computes partial derivatives.
c     SUREV does series reversion involving 1st and 2nd partial
c     derivatives of N functions of N variables.
c     ------------------------------------------------------------------
c--S replaces "?": DR?UCOMP, ?UCOMP, ?UREV, ?USETN, ?USET, ?UATN2
c--   &         ?UPRO, ?USUM, ?USQRT, ?UQUO, ?UATAN, ?UCOS, ?USIN, ?COPY
c     ------------------------------------------------------------------
      integer I, LDIM, NMAX
      parameter(NMAX = 3, LDIM = ((NMAX+2)*(NMAX+1))/2)
      integer IWORK(NMAX)
      real             X(LDIM), Y(LDIM), Z(LDIM)
      real             R(LDIM), PHI(LDIM), THETA(LDIM)
      real             SSQ(LDIM), S(LDIM), TEMP(LDIM)
      real             X2(LDIM), Y2(LDIM), Z2(LDIM)
      real             XSQ(LDIM), YSQ(LDIM), ZSQ(LDIM)
      real             XVAL, YVAL, ZVAL, RSQ(LDIM), ZBYS(LDIM)
      real             SP(LDIM), CP(LDIM), ST(LDIM), CT(LDIM)
      real             WORK(NMAX,NMAX,3), RCOND
      real             UT(LDIM,NMAX), TU(LDIM,NMAX)
      real             X1(LDIM), X3(LDIM), T1(LDIM), T2(LDIM), T3(LDIM)
      parameter(XVAL = 0.1E0, YVAL = 0.2E0, ZVAL = 0.3E0)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                   Set N, M1, and M2.
      call SUSETN(NMAX, 0, 2)
c                                   Initialize independent variables.
      call SUSET(XVAL,1,X)
      call SUSET(YVAL,2,Y)
      call SUSET(ZVAL,3,Z)
      print'(1x,a)',
     * 'DRSUCOMP..  Demo driver for the SUCOMP package.',
     * 'This demo first transforms (x,y,z) to (r,theta,phi),',
     * 'and then transforms back, including computation of',
     * '1st and 2nd partial derivatives.'
      write(*,'(1x/8x,10a)') '  VALUE','     D1','     D2','     D3',
     *   '    D11','    D21','    D22','    D31','    D32','    D33'
      write(*,'(1x/(1x,a,10f7.3))')
     *  '    X =',X,'    Y =',  Y,'    Z =',Z
c
c                                Transform from (x,y,z) to (r,theta,phi)
      call SUATN2(Y, X, PHI)
      call SUPRO(X, X, XSQ)
      call SUPRO(Y, Y, YSQ)
      call SUPRO(Z, Z, ZSQ)
      call SUSUM(XSQ, YSQ, SSQ)
      call SUSUM(SSQ, ZSQ, RSQ)
      call SUSQRT(RSQ, R)
      call SUSQRT(SSQ, S)
      call SUQUO(Z, S, ZBYS)
      call SUATAN(ZBYS, THETA)
      write(*,'(1x/(1x,a,10f7.3))')
     *  '    R =',R,'  PHI =',PHI,'THETA =',THETA
c
c                           Transform back from (r,theta,phi) to (x,y,z)
      call SUCOS(PHI, CP)
      call SUCOS(THETA, CT)
      call SUPRO(CP, CT, TEMP)
      call SUPRO(TEMP, R, X2)
      call SUSIN(PHI, SP)
      call SUPRO(SP, CT, TEMP)
      call SUPRO(TEMP, R, Y2)
      call SUSIN(THETA, ST)
      call SUPRO(ST, R, Z2)
      write(*,'(1x/(1x,a,10f7.3))')
     *  '    X =',X2,'    Y =',  Y2,'    Z =',Z2
c
c                          Set data to call SUREV.
c
      call SCOPY(LDIM, R,1, UT(1,1),1)
      call SCOPY(LDIM, PHI,1, UT(1,2),1)
      call SCOPY(LDIM, THETA,1, UT(1,3),1)
      TU(1,1) = X(1)
      TU(1,2) = Y(1)
      TU(1,3) = Z(1)
      call SUREV( UT, TU, LDIM, RCOND, IWORK, WORK)
 
      write(*,'(a)') ' ',
     * ' To demo SUREV we store (R, PHI, THETA) including',
     * ' the first and second derivatives w.r.t. (X,Y,Z) into UT(),',
     * ' and set TU() = (X, Y, Z).',
     * ' Then compute (TU1,TU2,TU3) using SUREV.',
     * ' For comparison compute (T1,T2,T3) using the known functional',
     * ' definition of (X, Y, Z) as a function of (R, PHI, THETA).'
 
      write(*,'(1x/(1x,a,10f7.3))')
     *  '  TU1 =',(TU(I,1),I=1,LDIM),
     *  '  TU2 =',(TU(I,2),I=1,LDIM),
     *  '  TU3 =',(TU(I,3),I=1,LDIM)
c
c              For comparison set X1, X2, X3, and transform them to
c              T1, T2, T3.  These are the same operations as
c              transforming from (r,theta,phi) to (x,y,z).
c
      call SUSET(R(1),1,X1)
      call SUSET(PHI(1),2,X2)
      call SUSET(THETA(1),3,X3)
c
      call SUCOS(X2, CP)
      call SUCOS(X3, CT)
      call SUPRO(CP, CT, TEMP)
      call SUPRO(TEMP, X1, T1)
      call SUSIN(X2, SP)
      call SUPRO(SP, CT, TEMP)
      call SUPRO(TEMP, X1, T2)
      call SUSIN(X3, ST)
      call SUPRO(ST, X1, T3)
      write(*,'(1x/(1x,a,10f7.3))')
     *  '   T1 =',T1,'   T2 =',  T2,'   T3 =',T3
      end
