      double precision function DERFI (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C--D replaces "?": ?ERFI, ?ERFCI, ?ERFIX, ?ERM1
c>> 1998-11-01 DERFI Krogh  Removed some equivalence for "mangle".
c>> 1996-06-18 DERFI Krogh  Changes to use .C. and C%%. J not changed.
c>> 1996-03-30 DERFI Krogh  Added external statements.
C>> 1995-11-28 DERFI Krogh  Removed multiple entries.
C>> 1995-11-03 DERFI Krogh  Removed blanks in numbers for C conversion.
C>> 1994-10-20 DERFI Krogh  Changes to use M77CON
C>> 1994-04-20 DERFI CLL Edited type stmts to make DP & SP files similar
C>> 1987-10-29 DERFI Snyder  Initial code.
c
c     For -1.0 .LT. X .LT. 1.0 calculate the inverse of the error
c     function.  That is, X = ERF(ERFI).
c
c     For 0.0 .LT. X .LT. 2.0 calculate the inverse of the
c     complementary error function.  that is, X = ERFC(ERFCI).  This
c     calculation is carried out by invoking the alternate entry *ERFCI.
c
c     If X is out of range, program execution is terminated by calling
c     the error message processor.
c
c     This subprogram uses approximations due to A. Strecok from
c     Mathematics of Computation 22, (1968) pp 144-158.
c
      external DERFIX
      double precision DERFIX
      double precision X
      double precision ARG, D(6), FSIGN, S
      integer J
c
c     *****     Parameters     *****************************************
c
c MAX...  is the position in C of the last coefficient of a Chebyshev
c         polynomial expansion.
c MIN...  is the position in C of the first coefficient of a Chebyshev
c         polynomial expansion.
c NC      is the upper dimension of the array of coefficients.
c NDELTA  is the number of coefficients of the Chebyshev polynomial
c         expansion used to approximate R(X) in the range
c         0.9975 .LT. X .LE. 1-5.0D-16
c NLAMDA  is the number of coefficients of the Chebyshev polynomial
c         expansion used to approximate R(X) in the range
c         0.8 .LT. X .LE. 0.9975.
c NMU     is the number of coefficients of the Chebyshev polynomial
c         expansion used to approximate R(X) in the range
c         5.0D-16 .GT. 1-X .GE. 1.D-300.
c NXI     is the number of coefficients of the Chebyshev polynomial
c         expansion used to approximate DERFCI(X)/X in the
c         range 0.0 .LE. X .LE. 0.8.
c
c
c     *****     External References     ********************************
c
c D1MACH   Provides the round-off level.  Used to calculate the number
c          of coefficients to retain in each Chebyshev expansion.
c DERM1    Prints an error message and stops if X .LE. -1.0 or
c          X .GE. 1.0 (ERFI) or X .LE. 0.0 or X .GE. 2.0 (ERFCI).
c LOG      Calculates the natural logarithm.
c SQRT     Calculates the square root.
c
c
c     *****     Local Variables      ***********************************
c
c ARG     If ERFI or ERFCI is being approximated by a Chebyshev
c         expansion then ARG is the argument of ERFI or the argument
c         that would be used if ERFCI(X) were computed as ERFC(1-X),
c         that is, ARG = X if ERFI is being computed, or ARG = 1-X if
c         ERFCI is being computed.  If ERFI or ERFCI is being computed
c         using the initial approximation ERFI=SQRT(-LOG((1-X)*(1+X))),
c         then ARG is that initial approximation.
c C       contains the coefficients of polynomial expansions.  They are
c         stored in C in the order DELTA(0..37), LAMDA(0..26),
c         MU(0..25), XI(0..38).
c D       are used to scale the argument of the Chebyshev polynomial
c         expansion in the range 1.D-300 .LT. 1-X .LT. 0.2.
c DELTA   are coefficients of the Chebyshev polynomial expansion of R(X)
c         for 0.9975 .LT. X .LE. 1-5.0D-16.
c FIRST   is a logical SAVE variable indicating whether it is necessary
c         to calculate the number of coefficients to use for each
c         Chebyshev expansion.
c FSIGN   is X or 1.0 - X.  It is used to remember the sign to be
c         assigned to the function value.
c I, J    are used as indices.
c IMIN    is the minimum index of a coefficient in the Chebyshev
c         polynomial expansion to be used.
c JIX     is an array containing MINXI, MAXXI, MINLAM, MAXLAM, MINDEL,
c         MAXDEL, MINMU, MAXMU in locations -1..6
c LAMDA   are coefficients of the Chebyshev polynomial expansion of R(X)
c         for 0.8 .LT. X .LE. 0.9975.
c MU      are coefficients of the Chebyshev polynomial expansion of R(X)
c         for 5.0D-16 .GT. 1-X .GE. 1.D-300.
c S2      is 2.0 * S.
c S       is the argument of the Chebyshev polynomial expansion.
c W1..W3  are adjacent elements of the recurrence used to evaluate the
c         Chebyshev polynomial expansion.
c XI      are coefficients of the Chebyshev polynomial expansion of
c         ERFC(X)/X for 0.0 .LE. X .LE. 0.8.
c
      data D /-1.548813042373261659512742D0
     2,        2.565490123147816151928163D0
     3,       -.5594576313298323225436913D0
     4,        2.287915716263357638965891D0
     5,       -9.199992358830151031278420D0
     6,        2.794990820124599493768426D0/
c
      fsign = x
      arg = abs(x)
      if (arg.lt.0.0d0 .or. arg.ge.1.0d0)then
         call derm1 ('DERFI',1,2,'Argument out of range','X',x,'.')
c     In case the error level is shifted to zero by the caller:
         derfi = 0.0d0
         return
      end if
      if (arg.eq.0.0d0) then
         derfi = 0.0d0
         return
      end if
      if (arg.le.0.8d0) then
         s = 3.125d0*arg*arg - 1.0d0
         j = -1
      else
         if (arg.le.0.9975d0) then
            j = 1
         else
            j = 3
         end if
         arg = sqrt(-log((1.0d0-arg)*(1.0d0+arg)))
         s = d(j)*arg + d(j+1)
      end if
      DERFI = sign(arg*DERFIX(s, j), fsign)
      return
      end
c
c     *****     entry ERFCI     ****************************************
c
      double precision function DERFCI(X)
c     Calculate the inverse of the complementary error function.
c
      external DERFIX
      double precision DERFIX
      double precision X
      double precision ARG, D(6), FSIGN, S
      integer J
      data D /-1.548813042373261659512742D0
     2,        2.565490123147816151928163D0
     3,       -.5594576313298323225436913D0
     4,        2.287915716263357638965891D0
     5,       -9.199992358830151031278420D0
     6,        2.794990820124599493768426D0/
c
c     Decide which approximation to use, and calculate the argument of
c     the Chebyshev polynomial expansion.
c
      if (x.le.0.0d0 .or. x.ge.2.0d0) then
         call derm1('DERFCI',1,2,'Argument out of range','X',x,'.')
c     In case the error level is shifted to zero by the caller:
         derfci = 0.0d0
      end if
      if (x.eq.1.0d0) then
         derfci = 0.0d0
         return
      end if
      fsign = 1.0d0 - x
      arg = abs(fsign)
      if (arg.le.0.8d0) then
         s = 3.125d0*arg*arg - 1.0d0
         j = -1
      else
         arg = 2.0d0 - x
         if (x.lt.1.0d0) then
            s = x
         else
            s = arg
         end if
         arg = sqrt(-log(x*arg))
         if (s.lt.5.0d-16) then
            j = 5
            s = d(5)/sqrt(arg) + d(6)
         else
            if (s.ge.0.0025d0) then
               j = 1
            else if (s.ge.5.0d-16) then
               j = 3
            end if
            s = d(j)*arg + d(j+1)
         end if
      end if
      DERFCI = sign(arg*DERFIX(s, j), fsign)
      return
      end
c
      double precision function DERFIX(S, J)
c             Subroutine where most of calculations are done.
      external D1MACH
      integer MAXDEL, MAXLAM, MAXMU, MAXXI, MINDEL, MINLAM
      integer MINMU, MINXI, NC, NDELTA, NLAMDA, NMU, NXI
      parameter (MINDEL = 0)
      parameter (NDELTA = 37)
      parameter (MAXDEL = MINDEL + NDELTA)
      parameter (MINLAM = MAXDEL + 1)
      parameter (NLAMDA = 26)
      parameter (MAXLAM = MINLAM + NLAMDA)
      parameter (MINMU = MAXLAM + 1)
      parameter (NMU = 25)
      parameter (MAXMU = MINMU + NMU)
      parameter (MINXI = MAXMU + 1)
      parameter (NXI = 38)
      parameter (MAXXI = MINXI + NXI)
      parameter (NC = MAXXI)
      double precision D1MACH
      double precision C(0:NC)
      logical FIRST
      save FIRST
      integer I, J, JIX(-1:6)
      save JIX
      integer IMIN
      double precision S, S2
      double precision W1, W2, W3
c
c     *****     Equivalence Statements     *****************************
c
c Equivalence statements connecting arrays DELTA, LAMDA, MU, XI removed
c by FTK to make "mangle" work.  All references to these arrays have
c been replaced by references to C.
c
c     *****     Data Statements     ************************************
c
c     DELTA(J), J = 0..NDELTA, then
c     LAMDA(J), J = 0..NLAMDA, then
c     MU(J), J = 0..NMU, then
c     XI(J), J = 0..NXI
c
c++ With first index 0, save data by elements if ~.C.
      data C(0) /  .9566797090204925274526373D0 /
      data C(1) / -.0231070043090649036999908D0 /
      data C(2) / -.0043742360975084077333218D0 /
      data C(3) / -.0005765034226511854809364D0 /
      data C(4) / -.0000109610223070923931242D0 /
      data C(5) /  .0000251085470246442787982D0 /
      data C(6) /  .0000105623360679477511955D0 /
      data C(7) /  .0000027544123300306391503D0 /
      data C(8) /  .0000004324844983283380689D0 /
      data C(9) /   -.0000000205303366552086916D0 /
      data C(10) / -.0000000438915366654316784D0 /
      data C(11) / -.0000000176840095080881795D0 /
      data C(12) / -.0000000039912890280463420D0 /
      data C(13) / -.0000000001869324124559212D0 /
      data C(14) /  .0000000002729227396746077D0 /
      data C(15) /  .0000000001328172131565497D0 /
      data C(16) /  .0000000000318342484482286D0 /
      data C(17) /  .0000000000016700607751926D0 /
      data C(18) / -.0000000000020364649611537D0 /
      data C(19) / -.0000000000009648468127965D0 /
      data C(20) / -.0000000000002195672778128D0 /
      data C(21) / -.0000000000000095689813014D0 /
      data C(22) /  .0000000000000137032572230D0 /
      data C(23) /  .0000000000000062538505417D0 /
      data C(24) /  .0000000000000014584615266D0 /
      data C(25) /  .0000000000000001078123993D0 /
      data C(26) / -.0000000000000000709229988D0 /
      data C(27) / -.0000000000000000391411775D0 /
      data C(28) / -.0000000000000000111659209D0 /
      data C(29) / -.0000000000000000015770366D0 /
      data C(30) /  .0000000000000000002853149D0 /
      data C(31) /  .0000000000000000002716662D0 /
      data C(32) /  .0000000000000000000957770D0 /
      data C(33) /  .0000000000000000000176835D0 /
      data C(34) / -.0000000000000000000009828D0 /
      data C(35) / -.0000000000000000000020464D0 /
      data C(36) / -.0000000000000000000008020D0 /
      data C(37) / -.0000000000000000000001650D0 /
      data C(38) /  .9121588034175537733059200D0 /
      data C(39) / -.0162662818676636958546661D0 /
      data C(40) /  .0004335564729494453650589D0 /
      data C(41) /  .0002144385700744592065205D0 /
      data C(42) /  .0000026257510757648130176D0 /
      data C(43) / -.0000030210910501037969912D0 /
      data C(44) / -.0000000124060618367572157D0 /
      data C(45) /  .0000000624066092999917380D0 /
      data C(46) / -.0000000005401247900957858D0 /
      data C(47) /   -.0000000014232078975315910D0 /
      data C(48) /  .0000000000343840281955305D0 /
      data C(49) /  .0000000000335848703900138D0 /
      data C(50) / -.0000000000014584288516512D0 /
      data C(51) / -.0000000000008102174258833D0 /
      data C(52) /  .0000000000000525324085874D0 /
      data C(53) /  .0000000000000197115408612D0 /
      data C(54) / -.0000000000000017494333828D0 /
      data C(55) / -.0000000000000004800596619D0 /
      data C(56) /  .0000000000000000557302987D0 /
      data C(57) /  .0000000000000000116326054D0 /
      data C(58) / -.0000000000000000017262489D0 /
      data C(59) / -.0000000000000000002784973D0 /
      data C(60) /  .0000000000000000000524481D0 /
      data C(61) /  .0000000000000000000065270D0 /
      data C(62) / -.0000000000000000000015707D0 /
      data C(63) / -.0000000000000000000001475D0 /
      data C(64) /  .0000000000000000000000450D0 /
      data C(65) /  .9885750640661893136460358D0 /
      data C(66) /  .0108577051845994776160281D0 /
      data C(67) / -.0017511651027627952494825D0 /
      data C(68) /  .0000211969932065633437984D0 /
      data C(69) /  .0000156648714042435087911D0 /
      data C(70) / -.0000005190416869103124261D0 /
      data C(71) / -.0000000371357897426717780D0 /
      data C(72) /  .0000000012174308662357429D0 /
      data C(73) / -.0000000001768115526613442D0 /
      data C(74) /   -.0000000000119372182556161D0 /
      data C(75) /  .0000000000003802505358299D0 /
      data C(76) / -.0000000000000660188322362D0 /
      data C(77) / -.0000000000000087917055170D0 /
      data C(78) / -.0000000000000003506869329D0 /
      data C(79) / -.0000000000000000697221497D0 /
      data C(80) / -.0000000000000000109567941D0 /
      data C(81) / -.0000000000000000011536390D0 /
      data C(82) / -.0000000000000000001326235D0 /
      data C(83) / -.0000000000000000000263938D0 /
      data C(84) /  .0000000000000000000005341D0 /
      data C(85) / -.0000000000000000000022610D0 /
      data C(86) /  .0000000000000000000009552D0 /
      data C(87) / -.0000000000000000000005250D0 /
      data C(88) /  .0000000000000000000002487D0 /
      data C(89) / -.0000000000000000000001134D0 /
      data C(90) /  .0000000000000000000000420D0 /
      data C(91) /  .9928853766189408231495800D0 /
      data C(92) /  .1204675161431044864647846D0 /
      data C(93) /  .0160781993420999447257039D0 /
      data C(94) /  .0026867044371623158279591D0 /
      data C(95) /  .0004996347302357262947170D0 /
      data C(96) /  .0000988982185991204409911D0 /
      data C(97) /  .0000203918127639944337340D0 /
      data C(98) /  .0000043272716177354218758D0 /
      data C(99) /  .0000009380814128593406758D0 /
      data C(100) /  .0000002067347208683427411D0 /
      data C(101) /  .0000000461596991054300078D0 /
      data C(102) /  .0000000104166797027146217D0 /
      data C(103) /  .0000000023715009995921222D0 /
      data C(104) /  .0000000005439284068471390D0 /
      data C(105) /  .0000000001255489864097987D0 /
      data C(106) /  .0000000000291381803663201D0 /
      data C(107) /  .0000000000067949421808797D0 /
      data C(108) /  .0000000000015912343331469D0 /
      data C(109) /  .0000000000003740250585245D0 /
      data C(110) /  .0000000000000882087762421D0 /
      data C(111) /  .0000000000000208650897725D0 /
      data C(112) /  .0000000000000049488041039D0 /
      data C(113) /  .0000000000000011766394740D0 /
      data C(114) /  .0000000000000002803855725D0 /
      data C(115) /  .0000000000000000669506638D0 /
      data C(116) /  .0000000000000000160165495D0 /
      data C(117) /  .0000000000000000038382583D0 /
      data C(118) /  .0000000000000000009212851D0 /
      data C(119) /  .0000000000000000002214615D0 /
      data C(120) /  .0000000000000000000533091D0 /
      data C(121) /  .0000000000000000000128488D0 /
      data C(122) /  .0000000000000000000031006D0 /
      data C(123) /  .0000000000000000000007491D0 /
      data C(124) /  .0000000000000000000001812D0 /
      data C(125) /  .0000000000000000000000439D0 /
      data C(126) /  .0000000000000000000000106D0 /
      data C(127) /  .0000000000000000000000026D0 /
      data C(128) /  .0000000000000000000000006D0 /
      data C(129) /  .0000000000000000000000002D0 /
c
      data FIRST /.TRUE./
c
      data JIX /MINXI, MAXXI, MINLAM, MAXLAM, MINDEL, MAXDEL
     1,         MINMU, MAXMU/
c
c     *****     Procedures     *****************************************
c
c     Decide which approximation to use, and calculate the argument of
c     the Chebyshev polynomial expansion.
c
c
c     If this is the first call, calculate the degree of each expansion.
c
      if (first) then
         first = .false.
         s2 = 0.5d0*d1mach(3)
         do 120 imin = -1, 5, 2
            do 110 i = jix(imin), jix(imin+1)
               if (abs(c(i)).lt.s2) then
                  jix(imin+1) = i
                  go to 120
               end if
110         continue
120      continue
      end if
c
c     Evaluate the Chebyshev polynomial expansion.
c
      s2 = s + s
      w1 = 0.0d0
      w2 = 0.0d0
      imin = jix(j)
      i = jix(j+1)
200      w3 = w2
         w2 = w1
         w1 = (s2*w2 - w3) + c(i)
         i = i - 1
         if (i.gt.imin) go to 200
      derfix = (s*w1 - w2) + c(imin)
      return
      end
