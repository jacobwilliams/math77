c     program DRDPRPL2
c>> 1996-06-27 DRDPRPL2  Krogh Special code for C conversion.
c>> 1994-10-19 DRDPRPL2  Krogh  Changes to use M77CON
c>> 1992-02-14 DRDPRPL2  CLL
c     ------------------------------------------------------------------
c--D replaces "?": DR?PRPL2, ?PRPL2
c     ------------------------------------------------------------------
      integer I, IERR, KC, LINE, NCMAX, NLMAX
      parameter(KC = 3, NLMAX = 45, NCMAX = 110)
      integer JX(KC), JY(KC), NP(KC)
      double precision DX, S, X, XY(15,4)
      character SYMBOL(KC)
c%%    long int ierr, line;
c%%    char image[45][111], image2[45][80];
      character IMAGE(NLMAX)*(NCMAX)
      data JX/1,1,1/
      data JY/2,3,4/
      data NP/15,15,15/
      data SYMBOL/'A','B','C'/
c     ------------------------------------------------------------------
      X = -3.14d0
      DX = 21.0d0/49.0d0
      do 10 I=1,15
        S = sin(X)
        XY(I,1) = X
        XY(I,2) = S+1.0d0 + 0.5d0*X
        XY(I,3) = 1.0d0 + 0.5d0*X
        XY(I,4) = S + 2.0d0 +X
        X = X+DX
   10 continue

c++ Code for ~.C. is active
      call DPRPL2(XY,15,KC,JX,JY,NP,SYMBOL,
     *   'Demo of DPRPL2 with NLINES = 45 and NCHARS = 110',
     *   'X VALUES','Y VALUES', 45, 110, IMAGE, IERR)
      print'(1x,a)',IMAGE
      print'(1x/1x)'
c
      call DPRPL2(XY,15,KC,JX,JY,NP,SYMBOL,
     *   'Demo of DPRPL2 with NLINES = 22 and NCHARS = 79',
     *   'X VALUES','Y VALUES', 22, 79, IMAGE, IERR)
      print'(1x,a)',(IMAGE(LINE)(1:79), LINE = 1,22)
c++ Code for .C. is inactive
c%%   dprpl2( (double*)xy, 15, KC, jx, jy, np, symbol,
c%%      "Demo of DPRPL2 with NLINES = 45 and NCHARS = 110",
c%%      "X VALUES", "Y VALUES", 45, 111, (byte*)image, &ierr );
c%%   for(line=0; line < 45; line++)
c%%      printf( "\n%111.111s", image[line] );
c%%   printf( " \n\n" );
c%%   dprpl2( (double*)xy, 15, KC, jx, jy, np, symbol,
c%%   "Demo of DPRPL2 with NLINES = 22 and NCHARS = 79",
c%%   "X VALUES", "Y VALUES", 22, 80, (byte*)image2, &ierr );
c%%   for (line = 0; line < 22; line++)
c%%      printf( "\n%80.80s", image2[line]);
c%%   printf( "\n" );
c++ End
      stop
      end
