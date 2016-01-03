      subroutine DUSETN ( Nin, M1in, M2in)
c  Base name of file: DUCOMP
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2001-06-18 DUCOMP Krogh  Replaced "M1 .eq.. 0" with "M1 .eq. 0"
C>> 1996-04-30 DUCOMP Krogh  Removed redundant save statement.
C>> 1994-11-02 DUCOMP Krogh  Changes to use M77CON
C>> 1994-08-04 DUCOMP CLL New subrs: [D|S]USETN & [D|S]UGETN.
C>> 1993-11-11 CLL Changed Entries to separate Subroutines.
C>> 1990-01-23 CLL Replace M with MPWR in call to IERM1
C>> 1987-12-07 DUCOMP Lawson  Initial code.
c
c  The file DUCOMP contains a set of program units to perform
c  computation on arrays representing the value of a quantity
c  and (optionally) its first and second partial derivatives
c  with respect to a set of N independent variables.
c
c  The lowest and highest order derivs desired are specified by
c  the integers M1 and M2 which must satisfy
c               0 .le. M1 .le. M2 .le. 2
c  The integers N, M1, and M2 must be set by the user by a call
c  to [D|S]USETN, which will put these values in the COMMON block UCOM1.
c
c  Each array containing a U-variable for which second partial
c  derivs are to be carried must be dimensioned at least
C  ((N+2)*(N+1))/2    EXAMPLES..
C              N = 1  2  3  4  5  6  7  8  9 10 11 12  13  14  15  16
C      DIMENSION = 3  6 10 15 21 28 36 45 55 66 78 91 105 120 136 153
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c        Subroutines
c
c    subroutine DUSETN ( N, M1, M2)
c    subroutine DUGETN ( N, M1, M2, L1, L2)
c    subroutine DUSET  ( VAL, KEY, Y9)
c
c    subroutine DUPRO  ( U,V,Y)
c    subroutine DUQUO  ( U1,V1,Y1)
c    subroutine DUSUM  ( U3,V3,Y3)
c    subroutine DUDIF  ( U4,V4,Y4)
c    subroutine DUSUM1 ( C5,V5,Y5)
c    subroutine DUDIF1 ( C6,V6,Y6)
c    subroutine DUPRO1 ( C7,V7,Y7)
c    subroutine DUQUO1 ( C8,V8,Y8)
c
c    subroutine DUSQRT (U,Y)
c    subroutine DUEXP  (U1,Y1)
c    subroutine DULOG  (U2,Y2)
c    subroutine DUPWRI (MPWR,U3,Y3)
c
c    subroutine DUSIN  (U,Y)
c    subroutine DUCOS  (U ,Y )
c    subroutine DUSINH (U ,Y )
c    subroutine DUCOSH (U ,Y )
c    subroutine DUATAN (U1,Y1)
c    subroutine DUATN2 (V2,U2,Y2)
c    subroutine DUASIN (U, Y)
c    subroutine DUACOS (U, Y)
c    subroutine DUTAN  (U, Y)
c    subroutine DUTANH (U, Y)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c--D replaces "?": ?UCOMP, ?UACOS, ?UASIN, ?UATAN, ?UATN2, ?UCOS
c--   & ?UCOSH, ?UDIF, ?UDIF1, ?UEXP, ?UGETN, ?ULOG, ?UPRO, ?UPRO1
c--   & ?UPWRI, ?UQUO, ?UQUO1, ?USET, ?USETN, ?USIN, ?USINH, ?USQRT
c--   & ?USUM, ?USUM1, ?UTAN, ?UTANH, ?UACS
c     Subprograms referecnced by both versions: ERMSG
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c     C. L. Lawson, JPL, 1969 Dec 4
C     References:
c     1.   Wengert, R. E., A simple automatic derivative evaluation
c          program, Comm. ACM, 1, Aug 1964, 463-464.
c     2.   C. L. Lawson, Computing Derivatives using W-arithmetic and
c          U-arithmetic., JPL Appl Math TM 289, Sept 1971.
c     Revised by CLL for Fortran 77 in Jan 1987, Sept 1987.
c     ==================================================================
c     subroutine DUSETN ( Nin, M1in, M2in)
c
c  Nin [in]  Specifies the number of independent variables.
c     Require Nin .ge. 1.
c  M1in, M2in [in]  These specify the lowest and highest order derivs
c     desired.  These must satisfy 0 .le. M1in .le. M2in .le. 2.
c
c  This subr copies Nin, M1in, and M2in into common variables N, M1, and
c  M2.
c  It also computes L1 and L2 based on N, M1, and M2, and stores
c  L1 and L2 into common.  L1 and L2 specify the first and last
c  locations in a U-variable array that will be operated on when
c  dealing with derivs of orders M1 through M2 and
c  N independent variables.
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer Nin, M1in, M2in
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      integer L1, L2
      common/UCOM2/L1,L2
      save /UCOM1/,/UCOM2/
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      N = Nin
      M1 = M1in
      M2 = M2in
C                                      SET L1 AND L2
      if(M1 .eq. 0) then
         L1 = 1
      elseif(M1 .eq. 1) then
         L1 = 2
      else
         L1 = N+2
      endif

      if(M2 .eq. 0) then
         L2 = 1
      elseif(M2 .eq. 1) then
         L2 = N+1
      else
         L2=1+N+((N*(N+1))/2)
      endif
      end
c     ==================================================================
      subroutine DUGETN ( Nout, M1out, M2out, L1out, L2out)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer Nout, M1out, M2out, L1out, L2out
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      integer L1, L2
      common/UCOM2/L1,L2
      save /UCOM1/,/UCOM2/
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      Nout = N
      M1out = M1
      M2out = M2
      L1out = L1
      L2out = L2
      end
c     ==================================================================
      subroutine DUSET( VAL, KEY, Y9)
c
c     A convenience routine for assigning an initial value to the
c     U-variable, Y9().  The parts of Y9() assigned will
c     depend on M1 and M2.
c
c     If M1 .le. 0 .le. M2, Y9() will be assigned the value, VAL.
c
c     If M1 .le. 1 .le. M2, the first partial derivs of Y9() will be
c     assigned depending on KEY.  KEY must satisfy 0 .le. KEY .le. N.
c     If KEY .eq.. 0, the U-variable is to be constant relative to the N
c     independent variables.  Thus all first partial derivs will be set
c     to zero.
c     If KEY is in [1,N], Y9() is being set as th KEYth independent
c     variable.  Thus the KEYth first partial deriv will be set to 1.0
c     and all other first partial derivs will be set to zero.
c
c     If M1 .le. 2 .le. M2, the second partial derivs of Y9() will be
c     set to zero.
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer i, j, k
      integer KEY
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision VAL, Y9(*)
      double precision       ONE, ZERO
      parameter( ONE = 1.0D0, ZERO = 0.0D0)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if(M1-1)230,240,250
C                                      VALUE
  230 Y9(1)=VAL
      if(M2 .eq. 0) return
C                                      1ST PARTIALS
  240 continue
          do 245 I=1,N
  245     Y9(I+1) = ZERO
      if(KEY .ge. 1 .and. KEY .le. N) Y9(KEY+1) = ONE
      if(M2  .eq. 1) return
C                                      2ND PARTIALS
  250 continue
      K=N+2
          do 254 I=1,N
               do 252 J=1,I
               Y9(K)= ZERO
  252          K=K+1
  254     continue
      return
      end
c     ==================================================================
      subroutine DUPRO( U, V, Y)
c     Computes Y = U * V
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer i, j, k
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision       U(*),V(*),Y(*)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if(M2-1)60,40,10
C                                      2ND PARTIALS
   10 continue
      K=N+2
          do 30 I=1,N
               do 20 J=1,I
               Y(K)=U(1)*V(K)+U(K)*V(1)+U(I+1)*V(J+1)+U(J+1)*V(I+1)
               K=K+1
   20          continue
   30     continue
      if(M1 .eq. 2) return
C                                      1ST PARTIALS
   40 continue
          do 50 I=1,N
   50     Y(I+1)=U(1)*V(I+1) + U(I+1)*V(1)
      if(M1 .eq. 1) return
C                                      VALUE
   60 continue
      Y(1)=U(1)*V(1)
      return
      end
C
C
      subroutine DUQUO ( U1,V1,Y1)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer i, j, k
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision       U1(*),V1(*),Y1(*)
      double precision       VINV
      double precision       ONE, ZERO
      parameter( ONE = 1.0D0, ZERO = 0.0D0)
C
C     COMPUTE..        Y=U/V
C
      if(V1(1) .eq. ZERO) THEN
         call ERMSG('DUQUO',5,0,'Denominator is zero.','.')
         return
      end if
      if(M1-1)80,90,100
C                                      VALUE
   80 continue
      Y1(1)=U1(1)/V1(1)
      if(M2 .eq. 0) return
C                                      1ST PARTIAL DERIVS
   90 continue
      VINV=ONE/V1(1)
          do 95 I=1,N
   95     Y1(I+1)=(U1(I+1)-Y1(1)*V1(I+1))*VINV
      if(M2 .eq.1) return
      go to 110
C                                      2ND PARTIAL DERIVS
  100 VINV=ONE/V1(1)
  110 continue
      K=N+2
          do 114 I=1,N
               do 112 J=1,I
               Y1(K)= (U1(K)- Y1(1)*V1(K)-Y1(I+1)*V1(J+1)
     *                                   -Y1(J+1)*V1(I+1))*VINV
               K=K+1
  112          continue
  114     continue
      return
      end
C
C
C
      subroutine DUSUM  ( U3,V3,Y3)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer K
      integer L1, L2
      common/UCOM2/L1, L2
      save /UCOM2/
      double precision       U3(*),V3(*),Y3(*)
C
C     COMPUTE          Y=U+V      AND PARTIAL DERIVS
C
      do 148 K = L1,L2
  148     Y3(K)=U3(K)+V3(K)
      return
      end
C
C
      subroutine DUDIF  ( U4,V4,Y4)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer K
      integer L1, L2
      common/UCOM2/L1, L2
      save /UCOM2/
      double precision       U4(*),V4(*),Y4(*)
C
C     COMPUTE          Y=U-V      AND PARTIAL DERIVS
C
      do 154 K=L1,L2
  154     Y4(K)=U4(K)-V4(K)
      return
      end
C
C
      subroutine DUSUM1 ( C5,V5,Y5)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer K
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      integer L1, L2
      common/UCOM2/L1, L2
      save /UCOM1/,/UCOM2/
      double precision       C5,V5(*),Y5(*)
C
C     COMPUTE         Y=C+V   WHERE C IS A CONSTANT
C
      do 160 K=L1,L2
  160    Y5(K)=   V5(K)
      if(M1.eq.0) Y5(1)=C5+Y5(1)
      return
      end
C
C
C
      subroutine DUDIF1 ( C6,V6,Y6)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer K
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      integer L1, L2
      common/UCOM2/L1, L2
      save /UCOM1/,/UCOM2/
      double precision       C6,V6(*),Y6(*)
C
C     COMPUTE          Y=C-V  WHERE C IS A CONSTANT
C
      do 168 K=L1,L2
  168     Y6(K)=  -V6(K)
c
c         Using + in following statement because have just set
c         Y6(1) = -V6(1).
c
      IF (M1.eq.0) Y6(1)=C6+Y6(1)
      end
C
C
C
      subroutine DUPRO1( C7,V7,Y7)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer K
      integer L1, L2
      common/UCOM2/L1, L2
      save /UCOM2/
      double precision       C7,V7(*),Y7(*)
C
C     COMPUTE          Y=C*V  WHERE C IS A CONSTANT
C
      do 176 K=L1,L2
  176     Y7(K)=C7*V7(K)
      return
      end
C     ==================================================================
      subroutine DUQUO1(C8,V8,Y8)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer i, j, k
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision       C8, FAC, V8(*),Y8(*)
      double precision       ONE, ZERO
      parameter( ONE = 1.0D0, ZERO = 0.0D0)
C
C     COMPUTE          Y=C/V  WHERE C IS A CONSTANT
C
      if(V8(1) .eq. ZERO) then
         call ERMSG('DUQUO1',1,0,'Denominator is zero.','.')
         return
      endif
      if(M1-1)204,208,214
C                                      VALUE
  204 Y8(1)=C8/V8(1)
      if(M2 .eq. 0) return
C                                      1ST PARTIALS
  208 continue
      FAC = -Y8(1)/V8(1)
          do 212 I=1,N
  212     Y8(I+1) = V8(I+1)*FAC
      if(M2  .eq. 1) return
C                                      2ND PARTIALS
  214 FAC=-ONE/V8(1)
      K=N+2
          do 224 I=1,N
               do 220 J=1,I
               Y8(K)=(Y8(1)*V8(K)+Y8(I+1)*V8(J+1)+Y8(J+1)*V8(I+1))*FAC
  220          K=K+1
  224     continue
      return
      end
c     ------------------------------------------------------------------
      subroutine DUSQRT (U,Y)
C
C     COMPUTE..                                  Y=SQRT(U)
C
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer I, J, K
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision U(*),Y(*)
      double precision D1, FAC, FAC2
      double precision ZERO, ONE, HALF
      parameter(ZERO = 0.0D0, ONE = 1.0D0, HALF = 0.5D0)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if(M1-1) 4, 6,16
    4 Y(1)= SQRT(U(1))
      if(M2 .eq. 0) return
C
    6 if(Y(1) .eq. ZERO) then
         call ERMSG('DUSQRT',2,0,
     *      'Deriv of sqrt(x) is infinite at x = 0','.')
         return
      endif
c
      D1 = HALF/Y(1)
          do 12 I=1,N
   12     Y(I+1)=U(I+1)*D1
      if(M2 .eq. 1)return
      go to 18
C
   16 continue
      D1 = HALF/Y(1)
   18 K=N+1
      FAC=ONE/Y(1)
          do 24 I=1,N
            FAC2 = FAC * Y(I+1)
               do 20 J=1,I
               K=K+1
   20          Y(K) = D1*U(K) - Y(J+1)*FAC2
   24     continue
      return
      end
C
C
C
      subroutine DUEXP  (U1,Y1)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer i, j, k
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision U1(*),Y1(*)
C                                                Y=EXP(U)
      if(M1-1)30,31,34
   30 Y1(1)= EXP(U1(1))
      if(M2 .eq. 0)return
C
   31 continue
          do 32 I=1,N
   32     Y1(I+1)=Y1(1)*U1(I+1)
      if(M2 .eq. 1)return
C
   34 continue
      K=N+1
          do 36 I=1,N
               do 36 J=1,I
               K=K+1
   36          Y1(K)=Y1(1) * (U1(K) + U1(I+1)*U1(J+1))
      return
      end
C
C
C
      subroutine DULOG  (U2,Y2)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer i, j, k
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision D1, U2(*),Y2(*)
      double precision ONE
      parameter( ONE = 1.0D0)
C                                      Y=LOG(U)
      if(M1-1)40,42,48
   40 Y2(1) = LOG(U2(1))
      if(M2 .eq. 0) return
C
   42 D1 = ONE/U2(1)
          do 44 I=1,N
   44     Y2(I+1)=U2(I+1) * D1
      if(M2 .eq. 1)return
      go to 49
C
   48 D1 = ONE/U2(1)
   49 continue
      K=N+1
          do 50 I=1,N
               do 50 J=1,I
               K=K+1
   50          Y2(K) = D1 * U2(K) - Y2(I+1) * Y2(J+1)
      return
      end
C
C
C
      subroutine DUPWRI (MPWR,U3,Y3)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer i, j, K, K1, K2
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      integer MPWR
      double precision FAC, FAC2, FM, U3(*),Y3(*)
      double precision ZERO, ONE, TWO
      parameter(ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0)
C                                      Y=U**MPWR    MPWR IS A CONSTANT
C     MPWR MAY BE POS.,ZERO, OR NEG.  IF MPWR = 0 THEN Y=1. INDEPENDENT
C     OF U.  IF MPWR IS NEG. THEN ERROR STOP IF U IS ZERO.
C
      if(MPWR  .eq. 0   ) go to 56
      if(U3(1) .NE. ZERO) go to 100
C                                      U = 0.  OR  MPWR = 0
      if(MPWR .lt. 0) then
         call IERM1('DUPWRI',4,0,
     *   'U**M is infinite when U = 0. and M < 0','M',MPWR,'.')
         return
      endif
C                            U=0. AND MPWR .GE. 0
   56 if(M1-1)64,68,84
   64 Y3(1)=ZERO
      if(MPWR .eq. 0) Y3(1)=ONE
      if(M2 .eq. 0) return
C
   68 if(MPWR .eq. 1) go to 74
          do 70 I=1,N
   70     Y3(I+1)=ZERO
      go to 80
   74     do 78 I=1,N
   78     Y3(I+1)=U3(I+1)
   80 if(M2 .eq. 1) return
C
   84 continue
      if(MPWR .eq. 2) then
         K=N+2
          do 94 I=1,N
               do 94 J=1,I
               Y3(K)=TWO*U3(I+1)*U3(J+1)
   94          K=K+1
         return
      endif

      K1 = N+1
      K2=K1-1+(N*(N+1))/2
      if(MPWR .eq. 1) then
         do 98 K=K1,K2
   98    Y3(K)=U3(K)
      else
         do 86 K=K1,K2
   86    Y3(K)=ZERO
      endif
      return
C                                 END..  U .eq. 0. .OR. MPWR .eq. 0
c
C                                 BEGIN..  U .NE. 0. .AND. MPWR .NE.0
  100 continue
      if(M1-1)104,108,112
  104 Y3(1) = U3(1)**MPWR
      if(M2 .eq. 0) return
C
  108 FM=MPWR
      FAC=FM*(Y3(1)/U3(1))
          do 110 I=1,N
  110     Y3(I+1) = FAC * U3(I+1)
      if(M2 .eq. 1) return
      go to 114
  112 FM = MPWR
      FAC= FM*(Y3(1)/U3(1))
  114 FAC2 = (FM-ONE)/U3(1)
      K=N+2
          do 116 I=1,N
               do 116 J=1,I
               Y3(K) = FAC*(FAC2*U3(I+1)*U3(J+1) + U3(K))
  116          K=K+1
      return
C                                  END..  U .NE. 0. .AND. MPWR .NE. 0
      END
c     ------------------------------------------------------------------
      subroutine DUSIN  (U,Y)
C                                       computes   Y=SIN(U)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer I, J, K, N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision  D1, D2, FAC, U(*),Y(*)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if(M1 .LE. 0) THEN
         Y(1)= SIN(U(1))
         if(M2 .eq. 0) return
      end if
      D2 = -Y(1)
      D1 = COS(U(1))
      if(M1 .le. 1) go to 16
      go to 24
C
C                                      1ST PARTIALS
   16     do 20 I=1,N
   20     Y(I+1) = D1*U(I+1)
          if(M2 .eq. 1) return
C                                      2ND PARTIALS
   24 K=N+2
          do 28 I=1,N
            FAC = D2*U(I+1)
               do 28 J=1,I
               Y(K)= FAC * U(J+1) + D1*U(K)
   28          K=K+1
      return
      end
C
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine DUCOS  (U ,Y )
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer i, j, k
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision  D1, D2, FAC, U(*),Y(*)
C
C     COMPUTE..                        Y=COS(U)
C
      if(M1 .LE. 0) THEN
         Y(1) = COS(U(1))
         if(M2 .eq. 0) return
      end if
      D2 = -Y(1)
      D1 = -SIN(U(1))
      if(M1 .le. 1) go to 16
      go to 24
C                                      1ST PARTIALS
   16     do 20 I=1,N
   20     Y(I+1) = D1*U(I+1)
          if(M2 .eq. 1) return
C                                      2ND PARTIALS
   24 K=N+2
          do 28 I=1,N
            FAC = D2*U(I+1)
               do 28 J=1,I
               Y(K)= FAC * U(J+1) + D1*U(K)
   28          K=K+1
      return
      end
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine DUSINH  (U ,Y )
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer i, j, k
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision  FAC, D1, D2, U(*),Y(*)
C
C     COMPUTE..                        Y=SINH(U)
C
      if(M1 .LE. 0) THEN
         Y(1) = SINH(U(1))
         if(M2 .eq. 0) return
      end if
      D2 = Y(1)
      D1 = COSH(U(1))
      if(M1 .le. 1) go to 16
      go to 24
C                                      1ST PARTIALS
   16     do 20 I=1,N
   20     Y(I+1) = D1*U(I+1)
          if(M2 .eq. 1) return
C                                      2ND PARTIALS
   24 K=N+2
          do 28 I=1,N
            FAC = D2*U(I+1)
               do 28 J=1,I
               Y(K)= FAC * U(J+1) + D1*U(K)
   28          K=K+1
      return
      end
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine DUCOSH  (U ,Y )
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer i, j, k
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision  FAC, D1, D2, U(*),Y(*)
C
C     COMPUTE..                        Y=COSH(U)
C
      if(M1 .LE. 0) THEN
         Y(1) = COSH(U(1))
         if(M2 .eq. 0) return
      end if
      D2 = Y(1)
      D1 = SINH(U(1))
      if(M1 .le. 1) go to 16
      go to 24
C                                      1ST PARTIALS
   16     do 20 I=1,N
   20     Y(I+1) = D1*U(I+1)
          if(M2 .eq. 1) return
C                                      2ND PARTIALS
   24 K=N+2
          do 28 I=1,N
            FAC = D2*U(I+1)
               do 28 J=1,I
               Y(K)= FAC * U(J+1) + D1*U(K)
   28          K=K+1
      return
      end
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine DUATAN (U1,Y1)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer i, j, k
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision  D1, FAC, FAC2, U1(*),Y1(*)
      double precision       ONE
      parameter(ONE = 1.0D0)
C
C     COMPUTE..                        Y= ATAN(U)
C
      if(M1-1)30,32,36
   30 Y1(1) = ATAN(U1(1))
      if(M2 .eq. 0) return
C
   32 D1 = ONE/(ONE+U1(1)**2)
          do 34 I=1,N
   34     Y1(I+1)=D1*U1(I+1)
      if(M2 .eq. 1) return
      go to 38
C
   36 D1 = ONE/(ONE+U1(1)**2)
   38 FAC = -(U1(1)+U1(1))
      K=N+2
          do 40 I=1,N
            FAC2 = FAC * Y1(I+1)
               do 40 J=1,I
               Y1(K)= D1*U1(K) + FAC2 * Y1(J+1)
   40          K=K+1
      return
      end
C
C
C
      subroutine DUATN2(V2,U2,Y2)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer i, j, k
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision  FAC1, FAC2, R, U2(*),V2(*),Y2(*)
      double precision       ONE
      parameter(ONE = 1.0D0)
C
C     COMPUTE   Y= ATAN2(V,U) = ATAN(V/U)  with 4-quadrant resolution.
C
      if(M1 .eq. 0) then
         Y2(1)= ATAN2(V2(1),U2(1))
         if(M2 .eq. 0) return
      endif
C
      if(abs(V2(1)) .ge. abs(U2(1))) then
         R=U2(1)/V2(1)
         FAC2=ONE/(V2(1)+R*U2(1))
         FAC1=R*FAC2
      else
         R=V2(1)/U2(1)
         FAC1=ONE/(V2(1)*R + U2(1))
         FAC2=R*FAC1
      endif
      if(M1 .eq. 2) go to 54
C                                      FIRST PARTIALS
          do 52 I=1,N
   52     Y2(I+1)=FAC1*V2(I+1)-FAC2*U2(I+1)
      if(M2 .eq. 1) return
C                                      SECOND PARTIALS
   54 K=N+2
          do 56 I=1,N
               do 56 J=1,I
               Y2(K)=FAC1*(V2(K) - U2(I+1)*Y2(J+1) - U2(J+1)*Y2(I+1) )
     *              -FAC2*(U2(K) + V2(I+1)*Y2(J+1) + V2(J+1)*Y2(I+1) )
   56          K=K+1
      return
      end
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine DUASIN (U1,Y1)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision  U1(*),Y1(*)
C
C     COMPUTE..                        Y= ASIN(U)
C
      call DUACS (.false., U1,Y1)
      return
      end
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine DUACOS (U1,Y1)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision  U1(*),Y1(*)
C
C     COMPUTE..                        Y= ACOS(U)
C
      call DUACS (.true., U1,Y1)
      return
      end
c     ==================================================================
      subroutine DUACS (ACOSIN, U1,Y1)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      logical ACOSIN
      integer i, j, k
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision  FAC, FAC2, S1, U1(*),Y1(*)
      double precision       ONE, ZERO
      parameter(ONE = 1.0D0, ZERO = 0.0D0)
C
C     COMPUTE..                    Y= ACOS(U) if ACOSIN .eq. .true. and
C                                  Y= ASIN(U) if ACOSIN .eq. .false.
C
      if(M1 .eq. 0)then
         if(ACOSIN) then
            Y1(1) = ACOS(U1(1))
         else
            Y1(1) = ASIN(U1(1))
         endif
      endif
      if(M2 .eq. 0) return
C
      S1  = ONE - U1(1)**2
      if(S1 .eq. ZERO) then
c                                     Error condition.
         if(ACOSIN) then
            call ERMSG('DUACOS',1,0,
     *      'Deriv of ACOS(x) is infinite at x = -1 or +1','.')
         else
            call ERMSG('DUASIN',1,0,
     *      'Deriv of ASIN(x) is infinite at x = -1 or +1','.')
         endif
         return
      endif
      FAC = ONE/ sqrt(S1)
      if(ACOSIN) FAC = -FAC
      if(M1 .le. 1 .and. 1 .le. M2) then
c                                             First partials
          do 130 I=1,N
  130     Y1(I+1)=FAC*U1(I+1)
      endif
      if(M2 .eq. 1) return
C                                             Second partials
      K=N+2
          do 150 I=1,N
            FAC2 = U1(1) * Y1(I+1)
               do 140 J=1,I
               Y1(K)= FAC*( U1(K) + FAC2*Y1(J+1) )
               K=K+1
  140          continue
  150     continue
      return
      end
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine DUTAN  (U ,Y )
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer i, j, k
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision  D1, D2, FAC2, U(*),Y(*)
      double precision       ONE, TWO
      parameter(ONE = 1.0D0, TWO = 2.0D0)
C
C     COMPUTE..                        Y=TAN(U)
C
      if(M1 .LE. 0) THEN
         Y(1) = TAN(U(1))
         if(M2 .eq. 0) return
      end if
      D1 = (ONE/COS(U(1)))**2
      D2 = TWO * Y(1) * D1
      if(M1 .le. 1) go to 216
      go to 224
C                                      1ST PARTIALS
  216     do 220 I=1,N
  220     Y(I+1) = D1*U(I+1)
          if(M2 .eq. 1) return
C                                      2ND PARTIALS
  224 K=N+2
         do 228 I=1,N
            FAC2 = D2 * U(I+1)
            do 228 J=1,I
               Y(K)= FAC2 * U(J+1) + D1 * U(K)
  228          K=K+1
      return
      end
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine DUTANH  (U ,Y )
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer i, j, k
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      double precision  D1, D2, FAC2, U(*),Y(*)
      double precision       ONE, TWO
      parameter(ONE = 1.0D0, TWO = 2.0D0)
C
C     COMPUTE..                        Y=TANH(U)
C
      if(M1 .LE. 0) THEN
         Y(1) = TANH(U(1))
         if(M2 .eq. 0) return
      end if
      D1 = (ONE/COSH(U(1)))**2
      D2 = -TWO * Y(1) * D1
      if(M1 .le. 1) go to 216
      go to 224
C                                      1ST PARTIALS
  216     do 220 I=1,N
  220     Y(I+1) = D1*U(I+1)
          if(M2 .eq. 1) return
C                                      2ND PARTIALS
  224 K=N+2
         do 228 I=1,N
            FAC2 = D2 * U(I+1)
            do 228 J=1,I
               Y(K)= FAC2 * U(J+1) + D1 * U(K)
  228          K=K+1
      return
      end
