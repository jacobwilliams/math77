catc     program DRDPRPL1
c>> 1996-06-27 DRDPRPL1  Krogh Special code for C conversion.
c>> 1994-10-19 DRDPRPL1  Krogh  Changes to use M77CON
c>> 1992-02-14 DRDPRPL1  CLL
c     ------------------------------------------------------------------
c--D replaces "?": DR?PRPL1, ?PRPL1
c     ------------------------------------------------------------------
      integer I, IERR, LINE, NLMAX, NCMAX, NP
      parameter(NLMAX = 45, NCMAX = 110, NP = 101)
      double precision ALPHA, BETA, DX, X(NP),XT, Y(NP)
      parameter(ALPHA = 0.15d0, BETA = 0.015d0, DX = 1.0d0)
c%%    long int ierr, line;
c%%    char image[45][111], image2[45][80];
      character IMAGE(NLMAX)*(NCMAX)
c     ------------------------------------------------------------------
      XT= -36.0d0
      do 10 I=1,NP
        X(I)=XT
        Y(I)=X(I)*SIN(ALPHA*X(I)+BETA)
        XT=XT+DX
   10 continue
c
c++ Code for ~.C. is active
      call DPRPL1(X,Y,NP,
     *   'Demo of DPRPL1 with NLINES = 45 and NCHARS = 110',
     *   'X VALUES','Y VALUES', 45, 110, IMAGE, IERR)
      print'(1x,a)',IMAGE
      print'(1x/1x)'
c
      call DPRPL1(X,Y,NP,
     *   'Demo of DPRPL1 with NLINES = 22 and NCHARS = 79',
     *   'X VALUES','Y VALUES', 22, 79, IMAGE, IERR)
      print'(1x,a)',(IMAGE(LINE)(1:79), LINE = 1,22)
c++ Code for .C. is inactive
c%%   dprpl1( x,y,NP,"Demo of DPRPL1 with NLINES = 45 and NCHARS = 110",
c%%    "X VALUES","Y VALUES",45,111,(byte*)image,&ierr );
c%%   for(line=0; line < 45; line++)
c%%           printf( "\n%111.111s", image[line]);
c%%   printf( "\n" );
c%%   printf( " \n \n" );
c%%   dprpl1( x,y,NP,"Demo of DPRPL1 with NLINES = 22 and NCHARS = 79",
c%%    "X VALUES","Y VALUES",22,80,(byte*)image2,&ierr );
c%%   printf( " " );
c%%   for (line = 0; line < 22; line++)
c%%     printf( "\n%80.80s", image2[line] );
c%%   printf( "\n" );
c++ End
      stop
      end
