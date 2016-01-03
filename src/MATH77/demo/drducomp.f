c     program DRDUCOMP
c>> 1994-10-19 DRDUCOMP Krogh  Changes to use M77CON
c>> 1994-08-04 DRDUCOMP CLL New subroutine: DUSETN
c>> 1992-02-17 CLL
c>> 1990-12-13 CLL Added demo of DUREV.
c>> 1987-12-09 DRDUCOMP Lawson  Initial Code.
c     Demo driver for the DUCOMP package, including DUREV.
c     The DUCOMP package computes partial derivatives.
c     DUREV does series reversion involving 1st and 2nd partial
c     derivatives of N functions of N variables.
c     ------------------------------------------------------------------
c--D replaces "?": DR?UCOMP, ?UCOMP, ?UREV, ?USETN, ?USET, ?UATN2
c--   &         ?UPRO, ?USUM, ?USQRT, ?UQUO, ?UATAN, ?UCOS, ?USIN, ?COPY
c     ------------------------------------------------------------------
      integer I, LDIM, NMAX
      parameter(NMAX = 3, LDIM = ((NMAX+2)*(NMAX+1))/2)
      integer IWORK(NMAX)
      double precision X(LDIM), Y(LDIM), Z(LDIM)
      double precision R(LDIM), PHI(LDIM), THETA(LDIM)
      double precision SSQ(LDIM), S(LDIM), TEMP(LDIM)
      double precision X2(LDIM), Y2(LDIM), Z2(LDIM)
      double precision XSQ(LDIM), YSQ(LDIM), ZSQ(LDIM)
      double precision XVAL, YVAL, ZVAL, RSQ(LDIM), ZBYS(LDIM)
      double precision SP(LDIM), CP(LDIM), ST(LDIM), CT(LDIM)
      double precision WORK(NMAX,NMAX,3), RCOND
      double precision UT(LDIM,NMAX), TU(LDIM,NMAX)
      double precision X1(LDIM), X3(LDIM), T1(LDIM), T2(LDIM), T3(LDIM)
      parameter(XVAL = 0.1D0, YVAL = 0.2D0, ZVAL = 0.3D0)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                   Set N, M1, and M2.
      call DUSETN(NMAX, 0, 2)
c                                   Initialize independent variables.
      call DUSET(XVAL,1,X)
      call DUSET(YVAL,2,Y)
      call DUSET(ZVAL,3,Z)
      print'(1x,a)',
     * 'DRDUCOMP..  Demo driver for the DUCOMP package.',
     * 'This demo first transforms (x,y,z) to (r,theta,phi),',
     * 'and then transforms back, including computation of',
     * '1st and 2nd partial derivatives.'
      write(*,'(1x/8x,10a)') '  VALUE','     D1','     D2','     D3',
     *   '    D11','    D21','    D22','    D31','    D32','    D33'
      write(*,'(1x/(1x,a,10f7.3))')
     *  '    X =',X,'    Y =',  Y,'    Z =',Z
c
c                                Transform from (x,y,z) to (r,theta,phi)
      call DUATN2(Y, X, PHI)
      call DUPRO(X, X, XSQ)
      call DUPRO(Y, Y, YSQ)
      call DUPRO(Z, Z, ZSQ)
      call DUSUM(XSQ, YSQ, SSQ)
      call DUSUM(SSQ, ZSQ, RSQ)
      call DUSQRT(RSQ, R)
      call DUSQRT(SSQ, S)
      call DUQUO(Z, S, ZBYS)
      call DUATAN(ZBYS, THETA)
      write(*,'(1x/(1x,a,10f7.3))')
     *  '    R =',R,'  PHI =',PHI,'THETA =',THETA
c
c                           Transform back from (r,theta,phi) to (x,y,z)
      call DUCOS(PHI, CP)
      call DUCOS(THETA, CT)
      call DUPRO(CP, CT, TEMP)
      call DUPRO(TEMP, R, X2)
      call DUSIN(PHI, SP)
      call DUPRO(SP, CT, TEMP)
      call DUPRO(TEMP, R, Y2)
      call DUSIN(THETA, ST)
      call DUPRO(ST, R, Z2)
      write(*,'(1x/(1x,a,10f7.3))')
     *  '    X =',X2,'    Y =',  Y2,'    Z =',Z2
c
c                          Set data to call DUREV.
c
      call DCOPY(LDIM, R,1, UT(1,1),1)
      call DCOPY(LDIM, PHI,1, UT(1,2),1)
      call DCOPY(LDIM, THETA,1, UT(1,3),1)
      TU(1,1) = X(1)
      TU(1,2) = Y(1)
      TU(1,3) = Z(1)
      call DUREV( UT, TU, LDIM, RCOND, IWORK, WORK)

      write(*,'(a)') ' ',
     * ' To demo DUREV we store (R, PHI, THETA) including',
     * ' the first and second derivatives w.r.t. (X,Y,Z) into UT(),',
     * ' and set TU() = (X, Y, Z).',
     * ' Then compute (TU1,TU2,TU3) using DUREV.',
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
      call DUSET(R(1),1,X1)
      call DUSET(PHI(1),2,X2)
      call DUSET(THETA(1),3,X3)
c
      call DUCOS(X2, CP)
      call DUCOS(X3, CT)
      call DUPRO(CP, CT, TEMP)
      call DUPRO(TEMP, X1, T1)
      call DUSIN(X2, SP)
      call DUPRO(SP, CT, TEMP)
      call DUPRO(TEMP, X1, T2)
      call DUSIN(X3, ST)
      call DUPRO(ST, X1, T3)
      write(*,'(1x/(1x,a,10f7.3))')
     *  '   T1 =',T1,'   T2 =',  T2,'   T3 =',T3
      end
