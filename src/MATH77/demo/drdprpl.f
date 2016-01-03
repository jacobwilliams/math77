c     program  DRDPRPL
c>> 1996-07-12 DRDPRPL1  Krogh Special code for C conversion.
c>> 1994-10-19 DRDPRPL
c>> 1989-04-26 DRDPRPL
c--D replaces "?": DR?PRPL, ?PRPL
c     Demo driver for DPRPL.
c     ------------------------------------------------------------------
      character*36 IMAGE
      double precision X, Y
      integer I
c     ------------------------------------------------------------------
      print '(10X,''X'',6X,''Y'',21X,''Y'')'
      do 20002 I=1,21
        X = dble(I-11)/10.0d0
        Y = 2.0d0 * X * X - 1.0d0
        call DPRPL(Y, '*', IMAGE, 36, -1.0d0, 1.0d0, .true.)
c%%        printf( "  %10.1f%8.2f %36.36s\n", x, y, image );
        print '(2x,f10.1,f8.2,1x,a)', X,Y,IMAGE
20002 continue
      stop
      end
