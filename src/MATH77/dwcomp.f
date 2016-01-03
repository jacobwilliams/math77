      subroutine      DWATAN (N5,U5,Y5)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2001-06-18 DWCOMP Krogh   Replaced ". and." with " .and."
C>> 1996-04-15 DWCOMP Krogh   Changed single common block name to SWCOM.
C>> 1995-11-21 DWCOMP Krogh   Removed multiple entries for C conversion.
C>> 1994-11-11 DWCOMP Krogh   Declared all vars.
C>> 1994-10-31 DWCOMP Krogh  Changes to use M77CON
C>> 1987-12-07 DWCOMP Lawson  Initial code.
c--D replaces "?": ?WCOMP,?WATAN,?WASIN,?WACOS,?WATN2,?WSUM,?WDIF,
c--&   ?WSQRT,?WEXP,?WSIN,?WCOS,?WTAN,?WSINH,?WCOSH,?WTANH,?WSET,?WSUM1,
c--&   ?WDIF1,?WPRO1,?WQUO1,?WLOG,?WPWRI,?WCHN,?WPRO,?WQUO,?PASCL,
c--&   ?WRCHN,?WACSI,?WCOM
c
C     File DWCOMP [- Wengert derivative COMPutation]
c     The file DWCOMP contains a set of program units to perform
c     computation on (N+1)-tuples representing the value of a quantity
c     and its first N derivatives with respect to a single variable.
c     In the comments we use T as the name of the ultimate independent
c     variable.
c          The parameter NMAX must be set in program units
c                   DWATAN, DWSUM, and DWPRO
c     to establish the largest order of derivative the package
c     will be able to handle.
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c     C. L. Lawson, JPL, 1971.
C     References:
c     1.   Wengert, R. E., A simple automatic derivative evaluation
c          program, Comm. ACM, 1, Aug 1964, 463-464.
c     2.   C. L. Lawson, Computing Derivatives using W-arithmetic and
c          U-arithmetic., JPL Appl Math TM 289, Sept 1971.
c     Revised by CLL for Fortran 77 in Jan 1987.  Deleted subr WSTART.
c     Put a first time flag in WPRO.  Now user does not need to make
c     any initialization call.
c     9/18/87, CLL. Added new entry names DWASIN & DWACOS.
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     NMAX        Setting NMAX in the parameter statement determines
c           the highest order derivative this program unit will be able
c           to handle.  The following arrays have dimensions depending
c           on NMAX:  S1(), S2(), S3(), S4(), C()
c
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                         ERROR HANDLING
c
c     All detected error conditions are essentially fatal for the
c     requested operation.  We do not stop; rather, we issue an error
c     message and return.  By use of the the MATH77 library subroutine,
c     ERMSET, the user can change the error processing action to cause a
c     STOP following the error message.
c
c     Error  Called
c      No.    name   Explanation
c
c       1    DWASIN  Infinite deriv when arg = -1 or +1
c       1    DWACOS  Infinite deriv when arg = -1 or +1
c       2    DWSQRT  Infinite deriv when arg = 0
c       3    DWQUO1  Zero divisor
c       4    DWPWRI  U**M is infinite when U = 0 and M < 0
c       5    DWPRO   Require dimension NMAX .ge. NDERIV
c       6    DWQUO   Require dimension NMAX .ge. NDERIV
c       7    DWQUO   Zero divisor
c       8    DPASCL  Require N .ge. 0
c       9    DWRCHN  Require U(2) .ne. 0.
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c               PROGRAM UNITS, entry NAMES, and calls
c
c     All of these subroutine and entry names may be called directly by
c     a user, however, it is expected that DWCHN, DWRCHN, and DPASCL
c     would only be used to augment the package with new functions.
c
C     Subroutine names:                       Calls to:
c
c     subroutine DWATAN  (N5,U5,Y5)           DWPWRI,DWCHN
c     subroutine DWASIN (N16,U16,Y16)         DWACSI
c     subroutine DWACOS (N16,U16,Y16)         DWACSI
c     subroutine DWACSI (N16,U16,Y16,ACOSIN)  DWSQRT,DWPWRI,DWPRO1,DWCHN
c     subroutine DWATN2 (N9,Y9,X9,A9)      DWSUM,DWDIF,DWPRO,DWQUO,DWCHN
c
c     subroutine DWSUM  (NDERIV,U,V,W)        None
c     subroutine DWDIF  (N2,U2,V2,W2)         None
c     subroutine DWSQRT (N15,U15,Y15)         DWCHN
c     subroutine DWEXP  (N4,U4,Y4)            DWCHN
c     subroutine DWSIN  (N7,U7,Y7)            DWCHN
c     subroutine DWCOS  (N7,U7,Y7)            DWCHN
c     subroutine DWTAN  (N7,U7,Y7)            DWCHN
c     subroutine DWSINH (N7,U7,Y7)            DWCHN
c     subroutine DWCOSH (N7,U7,Y7)            DWCHN
c     subroutine DWTANH (N7,U7,Y7)            DWCHN, DWPRO, DWQUO
c     subroutine DWSET  (N10,UVAL,UDER,U10)   None
c     subroutine DWSUM1 (N11,C11,U11,Y11)     None
c     subroutine DWDIF1 (N12,C12,U12,Y12)     None
c     subroutine DWPRO1 (N13,C13,U13,Y13)     None
c     subroutine DWQUO1 (N14,C14,U14,Y14)     DWQUO
c
c     subroutine DWLOG  (NDERIV,U,Y)          DWCHN
c     subroutine DWPWRI (NDERIV,MPWR,U,Y)     DWCHN
c
c     subroutine DWCHN  (NDERIV,U,F)          DWPRO
c
c     subroutine DWPRO  (NDERIV,U,V,W)        DPASCL
c     subroutine DWQUO  (NDERIV,U,V,W)        DPASCL
c
c     subroutine DPASCL (N   ,C)              None
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                   Dependencies between program units
c
c     The progran units are DWATAN,DWSUM,DWLOG,DWCHN,DWPRO,DPASCL
c     DWATAN has calls into        DWSUM,DWLOG,DWCHN,DWPRO
c     DWSUM has calls into               DWLOG,DWCHN,DWPRO
c     DWLOG has calls into                     DWCHN
c     DWCHN has calls into                           DWPRO
c     DWPRO has calls into                                 DPASCL
c     DPASCL has no calls
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
      integer N5
      double precision U5(*),Y5(*)
      integer NMAX, NMAXP1
      parameter (NMAX=10, NMAXP1=NMAX+1)
      integer I, NS
      double precision S1(NMAXP1)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     COMPUTE..              Y5=ATAN(U5)    and derivs w.r.t. T
C
      Y5(1)= ATAN (U5(1))
      IF(N5 .eq. 0) return
C
C     deriv of Y w.r.t. U IS  1./(1. + U**2)
C     START BY CONSTRUCTING 1.+U**2 AND ITS derivs IN S1().
C
      S1(1)=1.0D0 + U5(1)**2
      NS=N5-1
      IF(NS .eq. 0) go to 52
      S1(2)= U5(1)+U5(1)
      IF(NS .eq. 1) go to 52
      S1(3)= 2.0D0
      IF(NS .eq. 2) go to 52
          DO 50 I=3,NS
   50     S1(I+1) = 0.0D0
C
C     NOW S1() = 1.+U**2 and derivs w.r.t. U .
C     COMPUTE 1./S1 and derivs w.r.t. U.  STORE RESULT STARTING IN Y(2)
C
   52 call DWPWRI (NS,-1,S1,Y5(2))
C
C     Convert Y from derivs w.r.t. U TO derivs w.r.t. T
      call DWCHN(N5,U5,Y5)
      return
      end
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine DWASIN(N16,U16,Y16)
C
C     COMPUTES..        Y = ASIN(U) and derivs w.r.t. T
C
      integer N16
      double precision U16(*), Y16(*)
c
      Y16(1)= ASIN (U16(1))
      if (N16 .ne. 0) call DWACSI(N16,U16,Y16,.FALSE.)
      return
      end
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine DWACOS(N16,U16,Y16)
C
C     COMPUTES..        Y = ACOS(U) and derivs w.r.t. T
C
      integer N16
      double precision U16(*), Y16(*)
c
      Y16(1)= ACOS (U16(1))
      if (N16 .ne. 0) call DWACSI(N16,U16,Y16,.TRUE.)
      return
      end
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine DWACSI(N16,U16,Y16,ACOSIN)
c      Common code for DWACOS and DWASIN  -- ACOSIN - .true. for DWACOS
      integer N16
      double precision U16(*), Y16(*)
      logical ACOSIN
c
      integer I, NS
      integer NMAX, NMAXP1
      parameter (NMAX=10, NMAXP1=NMAX+1)
      double precision S1(NMAXP1),S2(NMAXP1)
c
C
C     Deriv of Y w.r.t. U is  (+,-)1.0/sqrt(1.0 - U**2)
C     Start by constructing 1.0 - U**2 and its derivs in S1().
C
      S1(1)=1.0D0 - U16(1)**2
      if(S1(1) .eq. 0.0D0) then
c                                     Error condition.
         if(ACOSIN) then
            call ERMSG('DWACOS',1,0,
     *      'Deriv of ACOS(x) is infinite at x = -1 or +1','.')
         else
            call ERMSG('DWASIN',1,0,
     *      'Deriv of ASIN(x) is infinite at x = -1 or +1','.')
         endif
         return
      endif
c
      NS=N16-1
      IF(NS .eq. 0) go to 66
      S1(2)= -2.0D0 * U16(1)
      IF(NS .eq. 1) go to 66
      S1(3)= -2.0D0
      IF(NS .eq. 2) go to 66
          DO 64 I=3,NS
   64     S1(I+1) = 0.0D0
C
C     Now S1() = 1.0 - U**2 and derivs w.r.t. U.
c     Compute S2() = sqrt(1.0 - U**2) and derivs w.r.t. U.
C     Then change sign if doing Arc Cosine.
c
   66 continue
      call DWSQRT(NS,S1,S2)
      if(ACOSIN) call DWPRO1(NS,-1.0D0,S2,S2)
c
C     Compute 1.0/S2 and derivs w.r.t. U.  Store result starting in Y(2)
c
      call DWPWRI (NS,-1,S2,Y16(2))
C
C     Convert Y from derivs w.r.t. U to derivs w.r.t. T
      call DWCHN(N16,U16,Y16)
      return
      end
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine DWATN2(N9,Y9,X9,A9)
C
C     COMPUTES..        A = ATAN2(Y,X) = ATAN(Y/X) and derivs w.r.t. T
C
      integer N9
      double precision Y9(*),X9(*),A9(*)
      integer I, NM1, NP1
      double precision BIG, XX, YY
      integer NMAX, NMAXP1
      parameter (NMAX=10, NMAXP1=NMAX+1)
      double precision S1(NMAXP1),S2(NMAXP1),S3(NMAXP1),S4(NMAXP1)
c
      A9(1)= ATAN2(Y9(1),X9(1))
      IF(N9 - 1) 90,92,94
   90 return
C                            SPECIAL FOR N9 = 1
   92 continue
      BIG= max( ABS(X9(1)), ABS(Y9(1)))
      YY  =Y9(1)/BIG
      XX    =X9(1)/BIG
      A9(2)=(XX*(Y9(2)/BIG) - YY*(X9(2)/BIG) )/(XX**2 + YY**2)
      return
C                            GENERAL CASE    N9 .ge. 2
   94 continue
      NP1=N9+1
      BIG= max( ABS(X9(1)), ABS(Y9(1)))
          DO 96 I=1,NP1
          S1(I)=X9(I)/BIG
   96     S2(I)= Y9(I)/BIG
      NM1=N9-1
      call DWPRO (NM1,S1,S2(2),S3)
      call DWPRO (NM1,S2,S1(2),S4)
      call DWDIF  (NM1,S3,S4,S3)
      call DWPRO (NM1,S1  ,S1  ,S1  )
      call DWPRO (NM1,S2,S2,S2)
      call DWSUM  (NM1,S1  ,S2,S1 )
      call DWQUO (NM1,S3,S1  ,A9(2))
      return
      end

      subroutine DWSUM  (NDERIV,U,V,W)
C
C     COMPUTE..              W=U+V      and derivs w.r.t. T
C
      integer NDERIV
      double precision U(*),V(*),W(*)
      integer I, NP1
c
      NP1=NDERIV+1
          DO 10 I=1,NP1
   10     W(I)=U(I)+V(I)
      return
      end
C
C
C
      subroutine DWDIF  (N2,U2,V2,W2)
C
C     COMPUTE..              W2=U2-V2   and derivs w.r.t. T
      integer N2
      double precision U2(*),V2(*),W2(*)
      integer I, NP1
c
      NP1= N2+1
          DO 20 I=1,NP1
   20     W2(I)=U2(I)-V2(I)
      return
      end
C
C
C
      subroutine DWSQRT (N15,U15,Y15)
C
C     COMPUTE..        Y= SQRT(U)      and derivs w.r.t. T
C
      integer N15
      double precision U15(*),Y15(*)
      integer I
      double precision  FAC, SCALE, SQRTU
      integer NMAX, NMAXP1
      parameter (NMAX=10, NMAXP1=NMAX+1)
      double precision S1(NMAXP1)
c
      SQRTU = SQRT(U15(1))
      if(N15 .eq. 0) go to 152
      if(SQRTU .eq. 0.0D0) then
         call ERMSG('DWSQRT',2,0,
     *    'Deriv of sqrt(x) is infinite at x = 0','.')
         do 155 I = 2,N15+1
            Y15(I) = 0.0D0
  155    continue
         return
      endif
      if(N15 .gt. 1) go to 154
         Y15(2)=U15(2)/(SQRTU+SQRTU)
  152    Y15(1)=SQRTU
         return
C                            GENERAL CASE FOR N .ge. 2
  154 continue
      SCALE=1.0D0/U15(1)
      Y15(1)=1.0D0
      S1(1)  =1.0D0
      FAC=0.5D0
          DO 156 I=1,N15
          Y15(I+1)=Y15(I)*FAC
          FAC=FAC-1.0D0
  156     S1(I+1)=U15(I+1)*SCALE
      call DWCHN(N15,S1,Y15)
      Y15(1)=SQRTU
          DO 158 I=1,N15
  158     Y15(I+1)=Y15(I+1)*SQRTU
      return
      end
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C
      subroutine DWEXP  (N4,U4,Y4)
C
C     COMPUTE..              Y4=EXP(U4)     and derivs w.r.t. T
C
      integer N4
      double precision U4(*),Y4(*)
      integer I
C
      Y4(1)=EXP(U4(1))
      IF(N4 .eq. 0) return
          DO 40 I=1,N4
   40     Y4(I+1)=Y4(I)
      call DWCHN(N4,U4,Y4)
      return
      end
C
C
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine DWSIN(N7,U7,Y7)
C
C     COMPUTE..              Y7=SIN(U7) and derivs w.r.t. T
C
      integer N7
      double precision U7(*),Y7(*)
      integer I
C
      Y7(1) = SIN(U7(1))
      IF(N7 .eq. 0) return
      Y7(2) = COS(U7(1))
      if(N7 .eq.1) then
         Y7(2) = U7(2) * Y7(2)
      else
         DO 70 I=2,N7
            Y7(I+1)=-Y7(I-1)
   70    continue
         call DWCHN(N7,U7,Y7)
      end if
      return
      end
C
C
C
      subroutine DWCOS  (N7,U7,Y7)
C
C     COMPUTE..              Y7=COS(U7)      and derivs w.r.t. T
C
      integer N7
      double precision U7(*),Y7(*)
      integer I
C
      Y7(1)= COS (U7(1))
      IF(N7 .eq. 0) return
      Y7(2)= -SIN(U7(1))
      if(N7 .eq.1) then
         Y7(2) = U7(2) * Y7(2)
      else
         DO 70 I=2,N7
            Y7(I+1)=-Y7(I-1)
   70    continue
         call DWCHN(N7,U7,Y7)
      end if
      return
      end
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine DWTAN  (N7,U7,Y7)
C
C     COMPUTE..              Y7=TAN(U7)      and derivs w.r.t. T
C     The first deriv is 1/cos(u)**2
c
      integer N7
      double precision U7(*),Y7(*)
      integer I, N7M1
      double precision CS, SN
      integer NMAX, NMAXP1
      parameter (NMAX=10, NMAXP1=NMAX+1)
      double precision S1(NMAXP1)
c
      if(N7 .eq. 0) then
         Y7(1) = tan(U7(1))
         return
      end if
      SN = SIN(U7(1))
      CS = COS(U7(1))
      Y7(1)= SN/CS
c
c           Compute Cos(U) and its derivs w.r.t. U in S1(2..N+1)
c
      S1(2) = CS
      S1(3) = -SN
          DO 76 I=4,N7+1
   76     S1(I) = -S1(I-2)
      N7M1 = N7-1
c                                       Convert to derivs w.r.t. t
      call DWCHN(N7M1,U7,S1(2))
c                               Compute Cos(U)**2 & derivs w.r.t. t
      call DWPRO(N7M1, S1(2), S1(2), S1(2))
c                          Divide first deriv of U by Cos(U)**2,
c                          both with derivs w.r.t. t
      call DWQUO(N7M1, U7(2), S1(2), Y7(2))
      return
      end
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine DWSINH(N7,U7,Y7)
C
C     COMPUTE..              Y7=SINH(U7) and derivs w.r.t. T
C
      integer N7
      double precision U7(*),Y7(*)
      integer I
C
      Y7(1) = SINH(U7(1))
      IF(N7 .eq. 0) return
      Y7(2) = COSH(U7(1))
      if(N7 .eq.1) then
         Y7(2) = Y7(2) * U7(2)
      else
         DO 80 I=2,N7
            Y7(I+1) = Y7(I-1)
   80    CONTINUE
         call DWCHN(N7,U7,Y7)
      end if
      return
      end
C
C
C
      subroutine DWCOSH  (N7,U7,Y7)
C
C     COMPUTE..              Y7=COSH(U7)      and derivs w.r.t. T
C
      integer N7
      double precision U7(*),Y7(*)
      integer I
C
      Y7(1)= COSH (U7(1))
      IF(N7 .eq. 0) return
      Y7(2)= SINH(U7(1))
      if(N7 .eq.1) then
         Y7(2) = Y7(2) * U7(2)
      else
         DO 80 I=2,N7
            Y7(I+1) = Y7(I-1)
   80    CONTINUE
         call DWCHN(N7,U7,Y7)
      end if
      return
      end
C
C
C
      subroutine DWTANH  (N7,U7,Y7)
C
C     COMPUTE..              Y7=TANH(U7)      and derivs w.r.t. T
C     The first deriv is 1/cosh(u)**2
c
      integer N7
      double precision U7(*),Y7(*)
      integer I, N7M1
      double precision CH, SH
      integer NMAX, NMAXP1
      parameter (NMAX=10, NMAXP1=NMAX+1)
      double precision S1(NMAXP1)
C
      if(N7 .eq. 0) then
         Y7(1) = tanh(U7(1))
         return
      endif
      SH = SINH(U7(1))
      CH = COSH(U7(1))
      Y7(1)= SH/CH
c
c           Compute Cosh(U) and its derivs w.r.t. U in S1(2..N+1)
c
      S1(2) = CH
      S1(3) = SH
          DO 94 I=4,N7+1
   94     S1(I) = S1(I-2)
      N7M1 = N7-1
c                                       Convert to derivs w.r.t. t
      call DWCHN(N7M1,U7,S1(2))
c                               Compute Cosh(U)**2 & derivs w.r.t. t
      call DWPRO(N7M1, S1(2), S1(2), S1(2))
c                          Divide first deriv of U by Cosh(U)**2,
c                          both with derivs w.r.t. t
      call DWQUO(N7M1, U7(2), S1(2), Y7(2))
      return
      end
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine DWSET  (N10,UVAL,UDER,U10)
C
C     INITIALIZE THE ARRAY U10()..  SET VALUE=UVAL, 1-ST DERIV = UDER,
C                                   AND HIGHER derivs = ZERO
      integer N10
      double precision U10(*), UVAL, UDER
      integer I
C
      IF(N10-1) 104,102,100
  100     DO 101 I=2, N10
  101     U10(I+1)= 0.0D0
  102 U10(2)=UDER
  104 U10(1)=UVAL
      return
      end
C
C
C
      subroutine DWSUM1 (N11,C11,U11,Y11)
C
C     COMPUTE..        Y=C+U      WHERE C IS A SCALAR
C
      integer N11
      double precision C11, U11(*), Y11(*)
      integer I
C
      Y11(1)=C11+U11(1)
          DO 110 I=1,N11
  110     Y11(I+1)=U11(I+1)
      return
      end
C
C
C
      subroutine DWDIF1 (N12,C12,U12,Y12)
C
C     COMPUTE..        Y=C-U      WHERE C IS A SCALAR
C
      integer N12
      double precision C12, U12(*), Y12(*)
      integer I
C
      Y12(1)=C12-U12(1)
          DO 120 I=1,N12
  120     Y12(I+1)=-U12(I+1)
      return
      end
C
C
C
      subroutine DWPRO1(N13,C13,U13,Y13)
C
C     COMPUTE..        Y=C*U      WHERE C IS A SCALAR
C
      integer N13
      double precision C13, U13(*), Y13(*)
      integer I
C
          DO 130 I=1,N13+1
  130     Y13(I) = C13*U13(I)
      return
      end
C
C
C
      subroutine DWQUO1(N14,C14,U14,Y14)
C
C     COMPUTE..        Y=C/U      WHERE C IS A SCALAR
C
      integer N14
      double precision C14, U14(*), Y14(*)
      integer I
      integer NMAX, NMAXP1
      parameter (NMAX=10, NMAXP1=NMAX+1)
      double precision S1(NMAXP1)
C
      if(U14(1) .eq. 0.0D0) then
         call ERMSG('DWQUO1',3,0, 'Zero divisor.','.')
         return
      endif
      S1(1) = C14
      do 160 I = 2, N14+1
         S1(I) = 0.0D0
  160 continue
      call DWQUO (N14,S1,U14,Y14)
      return
      end

      subroutine DWLOG  (NDERIV,U,Y)
C     C. L. LAWSON, JPL, 1971 NOV 19
C
C     COMPUTE..              Y= LOG BASE E OF U   and derivs w.r.t. T
C     ERROR STOP IN this subr IF U(1) .le. 0.
C
      integer         II, NDERIV
      double precision             U(*),Y(*)
      double precision FAC, UINV
C
      Y(1)= LOG(U(1))
      if (NDERIV .le. 1) then
         if (NDERIV .eq. 1) Y(2)= U(2)/U(1)
         return
      end if
      UINV=1.0D0/U(1)
      Y(2)=UINV
      FAC=-UINV
      DO 120 II = 2, NDERIV
         Y(II+1)=Y(II)*FAC
         FAC=FAC-UINV
  120 continue
C
C     Y() NOW CONTAINS VALUE and derivs w.r.t. U
C     USE CHAIN RULE TO convert TO derivs w.r.t. T
C
      call DWCHN(NDERIV,U,Y)
      return
      end
C
      subroutine DWPWRI (NDERIV,MPWR,U,Y)
C
C     COMPUTE..              Y = U**MPWR  and derivs w.r.t. T
C     MPWR is an integer, POS.,NEG.,OR ZERO.
C     MPWR does not depend ON T.
C     U may depend on T
C     Issues an error message if both (U(1) .eq. 0.) and (MPWR .lt. 0)
C     If MPWR .eq. 0 then set Y(1) = 1. and all derivs = 0.
C
      integer         I, II, II1, II2, J, M, MPWR, NDERIV
      double precision             U(*),Y(*)
      logical          SHORT
      double precision D, FAC, UINV
C
      II2=NDERIV
      SHORT = .FALSE.
      M=MPWR
      IF(M     ) 110,35,45
C                                      HERE MPWR .eq. 0
   35 Y(1)= 1.0D0
      IF(NDERIV .le. 0) return
          DO 40 J=1,NDERIV
   40     Y(J+1) = 0.0D0
      return
C                                      HERE MPWR IS POSITIVE
   45 IF(U(1) .ne. 0.0D0) go to 50
      Y(1)=0.0D0
      IF(NDERIV .eq. 0) return
      IF(M .le. NDERIV) go to 46
      II=1
      go to 130
C
   46     DO 47 I=1,NDERIV
   47     Y(I+1) = 0.0D0
      FAC=1.0D0
      D  =0.0D0
          DO 48 I=1,M
          D=D+1.0D0
   48     FAC=FAC*D
      Y(M+1) = FAC
      go to 150
C
   50 IF( M .ge. NDERIV) go to 112
      II2=M
      SHORT = .TRUE.
      go to 112
C
C                                      HERE MPWR .lt 0
  110 continue
      if(U(1) .eq. 0.0D0) then
            call IERM1('DWPWRI',4,0,
     *      'U**M is infinite when U = 0. and M < 0',
     *      'M',M,'.')
         do 111 I = 1,NDERIV+1
            Y(I) = 0.0D0
  111    continue
         return
      endif
  112 continue
      Y(1)=U(1)**M
      if(NDERIV .eq. 0) return
      UINV=1.0D0/U(1)
      FAC=UINV*M
      II1=1
          DO 120 II=II1,II2
          Y(II+1)=Y(II)*FAC
          FAC=FAC-UINV
  120     continue
      if(.NOT. SHORT) go to 150
      II=II2+1
C
C                            SET HIGHER derivs TO ZERO.
C
  130 continue
          DO 140 J=II,NDERIV
  140     Y(J+1)=0.0D0
C
C     Y() NOW CONTAINS VALUE and derivs w.r.t. U
C     USE CHAIN RULE TO convert TO derivs w.r.t. T
C
  150 call DWCHN(NDERIV,U,Y)
      return
      end

      subroutine DWCHN(NDERIV,U,Y)
C
c     Implements the chain rule.
c     Given values of y(u) and its derivs with respect to u in Y(), and
c     u(x) and its derivs with respect to x in U(), this subr replaces
c     the contents of Y() with y(u(x)) and its derivs with respect to x.
c
C     NDERIV                 HIGHEST ORDER DERIVATIVE
c
C     (U(I),I=1,NDERIV+1)    INPUT.. value of u and derivs w.r.t. x
C
C     (Y(I),I=1,NDERIV+1)    INPUT.. value of y and derivs w.r.t. u
C                            OUTPUT.. (Y(I+1),I=1,NDERIV) will be
C                                     replaced by derivs w.r.t. x
      integer        I, L, NDERIV, NP1
      double precision     U(*), Y(*)
C
      if(NDERIV .eq. 0) return
      if(U(2) .ne. 1.) go to 20
C                             U(2) .eq. 1.,  See if higher derivs are 0.
      if(NDERIV .eq. 1) return
          DO 10 I=2,NDERIV
          if(U(I+1) .ne. 0.) go to 50
   10     continue
      return
C                                   Test if all derivs of U are 0.
   20     DO 30 I=1,NDERIV
          if(U(I+1) .ne. 0.) go to 50
   30     continue
C
          DO 40 I=1,NDERIV
   40     Y(I+1)=0.
      return
C
C         Here for the general case when U is not T and not constant.
C
   50     NP1=NDERIV+1
          DO 60 L=0,NDERIV-1
   60     call DWPRO (L, Y(NP1-L), U(2), Y(NP1-L))
      return
      end

      subroutine DWRCHN(NDERIV, U, Y)
C
c     Implements the "reverse" chain rule.
c     Given values of y(u(x)) and its derivs with respect to x in Y(),
c     and u(x) and its derivs with respect to x in U(), this subr replac
c     the contents of Y() with y(u) and its derivs with respect to u.
c     Requires u'(x) .ne. 0.
c
c     Note that this subr can be used to compute a representation of
c     the inverse function of a given function.  This transformation
c     is called "reversion" of a power series.  To do this let y() be
c     the inverse function of u() in a neighborhood of a point, x0.
c     Thus y(u(x)) = x for all x in a neighborhood of x0.
c     Then the value of y() and its derivs with respect to x, evaluated
c     at x0 are (x0, 1.0, 0.0, ..., 0.0).
c     If Y() contains these values on entry, and U() contains u(x) and
c     its derivs w.r.t. x, evaluated at x0, then on return Y() will
c     contain y(u) and its derivs w.r.t. u, evaluated at u0 = u(x0).
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     NDERIV  [in]  Highest order derivative to be considered.
C
C     (U(I),I=1,NDERIV+1)  [in]  Contains the value of u(x) and its
c           first NDERIV derivatives with respect to x, evaluated at x0.
c           Require U(2) .ne. 0.
c
C     (Y(I),I=1,NDERIV+1)  [inout]  On entry, Y() must contain y(u(x))
c           and its first NDERIV derivatives w.r.t. x, evaluated at x0.
c           On return, Y() will contain y(u) and its first NDERIV
c           derivatives w.r.t. u, evaluated at u0 = u(x0).
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer NDERIV, NP1, L
      double precision  U(*), Y(*)
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      NP1=NDERIV+1
c
      if( U(2) .eq. 0.0D0 .and. NP1 .ge. 2) then
         call ERMSG('DWRCHN',9,0,'U(2) is zero.','.')
         return
      endif
c
      do 60 L=2, NP1
         CALL DWQUO (NP1 - L, Y(L), U(2), Y(L))
   60 continue
      return
      end

      subroutine DWPRO(NDERIV,U,V,W)
C                      COMPUTE W(1)=U(1)*V(1) AND DERIVATIVES .
C     NDERIV           HIGHEST ORDER DERIVATIVE TO BE COMPUTED
C                      NDERIV .ge. 0
C     (U(I),I=1,NDERIV+1)    INPUT ARRAY..
C                                       U(1)= VALUE
C                                       U(I+1)= I-TH DERIVATIVE OF U(1)
C     (V(I),I=1,NDERIV+1)    INPUT ARRAY..      SAME FORMAT AS U().
C
C     (W(I),I=1,NDERIV+1)    OUTPUT ARRAY..
C                                            W(1)= U(1)*V(1)
C                                            W(I+1)=Ith deriv of W(1)
c     It is permissible for W() to occupy the same storage locations as
c     U() and/or V().
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     NMAX        Setting NMAX in the parameter statement determines
c           the highest order derivative this program unit will be able
c           to handle.  The dimension of the internal saved array C()
c           will be set as a function of NMAX.
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer I, ID2, IMID, IP1, J, JC, NDERIV, NP1
      double precision             U(*),V(*),W(*)
      integer NMAX, NCOEF
      parameter(NMAX = 10, NCOEF = 1 + (NMAX*(NMAX+1))/2 )
      double precision    C(NCOEF)
      double precision    TEMP
      logical FIRST
      common / DWCOM / C
      save FIRST, /DWCOM/
      data FIRST /.TRUE. /
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if(FIRST) then
         call DPASCL(NMAX, C)
         FIRST = .FALSE.
      endif
      if( NDERIV .gt. NMAX) then
         call IERM1('DWPRO',5,0,
     *   'Dimension NMAX in DWATAN, DWSUM, & DWPRO must be .ge. NDERIV',
     *   'NMAX',NMAX,',')
         call IERV1('NDERIV',NDERIV,'.')
         return
      endif
      NP1=NDERIV+1
c     JC = (NDERIV*(NDERIV-1))/2
c     DO 20 I=NP1,1,-1
c         TEMP=0.0D0
c         IP1=I+1
c              DO 10 J=1,I
c              TEMP = TEMP + U(J)*V(IP1-J)*C(JC+J)
c  10     continue
c         W(I) = TEMP
c         JC=JC- I+2
c  20 continue
c
c     The following code does the same as the above commented-out
c     code but reduces the number of multiplies.
c     Treats the cases of NDERIV = 0, 1, 2, 3, and 4 special for
c     efficiency, since it is expected that most usage will involve
c     small values of NDERIV.  In particular the chain rule subroutine,
c     _WCHN, will call this subr with a sequence of values of NDERIV
c     going down to 0.
c     Treats NDERIV > 4 in a general way, but takes account of
c     the fact that the C() terms in the "do 10" loop above are
c     symmetric about the middle term and the first and last term
c     are each = 1.
c
      go to (41,42,43,44,45), NP1
c
c     Fortran 77 specifies that a Computed Go To drops through is the
c     index is out of range.  Thus we arrive here if NP1 < 1 or > 5.
c     NP1 < 1 would be an error.  We choose not to use time to test for
c     it however.
c     NP1 > 5 is valid. In that case we do the following loop for I
c     running down from NP1 to 6, and then do the special statements
c     for 5, 4, 3, 2, and 1.
c
      JC = (NDERIV*(NDERIV-1))/2
      DO 20 I=NP1,6,-1
          TEMP = U(1) * V(I) + U(I) * V(1)
          IP1=I+1
          ID2 = I/2
          DO 10 J=2,ID2
               TEMP = TEMP + C(JC+J) * (U(J)*V(IP1-J) + U(IP1-J)*V(J))
   10     continue
          if(2*ID2 .eq. I) then
             W(I) = TEMP
          else
             IMID = ID2 + 1
             W(I) = TEMP + C(JC+IMID) * U(IMID) * V(IMID)
          endif
          JC=JC- I+2
   20 continue
c
   45 W(5) = U(1)*V(5) + 4.0D0*(U(2)*V(4) + U(4)*V(2)) +
     *                   6.0D0*U(3)*V(3) + U(5)*V(1)
   44 W(4) = U(1) * V(4) + 3.0D0*(U(2)*V(3) + U(3) * V(2)) + U(4)*V(1)
   43 W(3) = U(1) * V(3) + 2.0D0 * U(2) * V(2) + U(3) * V(1)
   42 W(2) = U(1) * V(2) + U(2) * V(1)
   41 W(1) = U(1) * V(1)
      return
      end
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      subroutine DWQUO (NDERIV, U ,V ,W )
C
C     COMPUTE..          W = U / V  and derivs w.r.t. T
c
c     V() must be distinct in storage from W(), but U() can be the same
c     array as W().
c
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      integer NDERIV
      double precision U(*),V(*),W(*)
      integer I, ID2, IMID, IP1, J, JC, NP1
      integer NMAX, NCOEF
      parameter(NMAX = 10, NCOEF = 1 + (NMAX*(NMAX+1))/2 )
      double precision    C(NCOEF)
      double precision    FAC, TEMP
      logical FIRST
      common / DWCOM / C
      save FIRST, /DWCOM/
      data FIRST /.TRUE. /
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(FIRST) THEN
         CALL DPASCL(NMAX, C)
         FIRST = .FALSE.
      END IF
      if( NDERIV .GT. NMAX) then
         call IERM1('DWQUO',6,0,
     *   'Dimension NMAX in DWATAN, DWSUM, & DWPRO must be .ge. NDERIV',
     *   'NMAX',NMAX,',')
         call IERV1('NDERIV',NDERIV,'.')
         return
      endif
c
      if( V(1) .eq. 0.0D0) then
         call ERMSG('DWQUO',7,0,'Zero divisor.','.')
         return
      endif
c
      FAC = 1.0D0/V(1)
      W(1) = U(1) * FAC
      NP1=NDERIV+1
      JC = 0
c         DO 40 I=2,NP1
c            TEMP = U(I)
c            IP1=I+1
c            DO 30 J=2,I
c               TEMP = TEMP - V(J)*W(IP1-J)*C(JC+J)
c  30        CONTINUE
c            W(I) = TEMP * FAC
c            JC=JC + I-1
c  40     CONTINUE
      DO 40 I= 2, NP1
          TEMP = U(I) - W(1) * V(I)
          IP1=I+1
          ID2 = I/2
          DO 30 J=2,ID2
               TEMP = TEMP - C(JC+J) * (W(J)*V(IP1-J) + W(IP1-J)*V(J))
   30     continue
          if(2*ID2 .ne. I) then
             IMID = ID2 + 1
             TEMP = TEMP - C(JC+IMID) * W(IMID) * V(IMID)
          endif
          W(I) = TEMP * FAC
          JC=JC + I-1
   40 continue
      return
      end

      subroutine DPASCL(N   ,C)
C        C.L.LAWSON,JPL, 1969 DEC 10
C
C     N                      MAXIMUM ORDER DERIVATIVE TO BE
C                            COMPUTED USING W-ARITHMETIC
C
C     (C(I),I=1,NC)          FIRST N+1 ROWS OF PASCAL TRIANGLE PACKED
C                            WITH 1-ST DIAGONAL OMITTED AFTER 1-ST ROW.
C                            NC= (N*(N+1)/2) + 1
C     EXAMPLE..    IF N=3 then TRIANGLE IS       1
C                                              1    1
C                                            1   2    1
C                                          1   3    3   1
C
C     WHICH WILL BE STORED AS.. 1,1,2,1,3,3,1
C
      integer          I, J, K, L, N
      logical          DONE
      double precision C(*)
      save             DONE
      data DONE / .false. /
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if (DONE) return
      DONE = .true.
      if(N .lt. 0) then
            call IERM1('DPASCL',8,0, 'Require N .ge. 0','N',N,'.')
            return
      endif
      C(1)=1.0D0
      if(N .eq. 0) return
      C(2)=1.0D0
      if(N .eq. 1) return
      K=3
      DO 520 I=1,N-1
          L=I+1
          DO 510 J=1,I
               C(K)=C(K-L)+C(K-L+1)
               K=K+1
  510     continue
          C(K)=1.0D0
          K=K+1
  520 continue
      return
      end
