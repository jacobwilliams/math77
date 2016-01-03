c>> 2006-03-11 DRSSPGE  Krogh  Initial code.
c--S replaces "?": DR?SPGE, ?SPGE
c Demo driver for DRSPGE
      integer N, LA, LIA
      parameter (N=6, LA=300, LIA=LA+5*N)
      integer IA(LA+5*N), ISPEC(7), I, J, K
      real             A(LA),OPT(3),SIZE,B(N),BB(N),X(N),ERRX,ERRRES
      data IA(1),A(1), IA(2),A(2), IA(3),A(3) /2,0.E0, 1,-2.E0, 4,1.E0/
      data IA(4),A(4), IA(5),A(5) /1,0.E0, 3,-2.E0/
      data IA(6),A(6), IA(7),A(7), IA(8),A(8) /2,0.E0, 2,-2.E0, 5,-2.E0/
      data IA( 9),A( 9), IA(10),A(10), IA(11),A(11), IA(12),A(12)  /
     1  3,0.E0, 1,-1.E0, 5,-1.E0, 6,2.E0 /
      data IA(13),A(13), IA(14),A(14), IA(15),A(15), IA(16),A(16),
     1  IA(17),A(17), IA(18),A(18) / 5,0.E0, 1,-1.E0, 2,-2.E0, 3,2.E0,
     2  4,-1.E0, 6,-2.E0 /
      data IA(19),A(19), IA(20),A(20), IA(21),A(21), IA(22),A(22) /
     1  3,0.E0,  1,-2.E0, 3,2.E0, 6,1.E0 /
      data X / -2.0E0, -1.0E0, -2.0E0, -1.0E0, -1.0E0, -1.0E0 /
      data B /  8.0E0,  6.0E0, -2.0E0, -1.0E0,  5.0E0, -1.0E0 /
      data ISPEC / 0, LIA, LA, 0, 3, 4, 0 /
 
      do 100 I = 1, N
         BB(I) = B(I)
 100  continue
c              Call SSPGE to get the solution
      call SSPGE(N, ISPEC, IA, A, B, OPT)
c            Get maximal errors in the residual and the solution.
      ERRRES = 0.E0
      SIZE = 0.E0
      K = 1
      do 280 J = 1, N
        do 270 I = K+1, K + IA(K)
          SIZE = max(SIZE, abs(A(I) * X(J)))
          BB(IA(I)) = BB(IA(I)) - A(I) * B(J)
 270    continue
        K = I
 280  continue
      do 300 J = 1, N
        ERRRES = max(ERRRES, abs(BB(J)))
 300  continue
      ERRRES = ERRRES / SIZE
      ERRX = 0.E0
      do 320 I = 1, N
        ERRX = max(ERRX, abs(X(I) - B(I)))
 320  continue
 
      I = int(log10(abs(OPT(2))))
      if (I .gt. 0) I = I + 1
      OPT(2) = OPT(2)*10.E0**(-I)
      I = I + nint(OPT(3))
      print '('' Problem     N   RESERR     XERR    Unused    Used'',
     1  ''    RCOND      DET   x10^?'')'
      print '(''DEMOTEST     6'',1P,2E10.2,2I8,E10.2,0PF9.5,I6)',
     1  ERRRES,ERRX,ISPEC(4),ISPEC(3)-ISPEC(4),OPT(1),OPT(2),I
      stop
      end
