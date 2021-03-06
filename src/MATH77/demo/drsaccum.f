c     program DRSACCUM
c>> 1996-06-18 DRSACCUM Krogh Special code for C conversion.
c>> 1996-05-28 DRSACCUM Krogh Added external state. & moved up formats
c>> 1995-09-15 DRSACCUM Krogh Remove '0' in format (again?)
c>> 1994-10-19 DRSACCUM Krogh  Changes to use M77CON
c>> 1994-08-09 DRSACCUM WVS Removed '0' in formats
c>> 1991-11-20 DRSACCUM CLL Edited for Fortran 90.
c>> 1987-12-09 DRSACCUM Lawson  Initial Code.
c     Demonstration driver for SACCUM.
c--S replaces "?": DR?ACCUM, ?ACCUM, ?HFTI, ?COPY, ?MPVAL
c     ------------------------------------------------------------------
      external SMPVAL
      real             SMPVAL
      integer NMAX, LDIM, LPMAX, NB, MDATA
      parameter(NMAX = 8, LDIM = NMAX + 2, NB = 1, MDATA = 12)
      parameter(LPMAX = NMAX + 2)
      real             X(MDATA),Y(MDATA),P(LPMAX)
      real             A(LDIM,NMAX),B(LDIM)
      real             RNORM(NB), WORK(NMAX)
      real             DOF, R, SIGFAC, TAU, U, YFIT
      integer I, IP(NMAX), IR1, IROW, J, KRANK, N, NCOUNT, NDEG, NROWS
      parameter(TAU = 1.0E-5)
c     ------------------------------------------------------------------
      data X / 2.0E0,  4.0E0,  6.0E0,  8.0E0,  10.0E0,  12.0E0,
     *        14.0E0, 16.0E0, 18.0E0, 20.0E0,  22.0E0,  24.0E0/
      data Y / 2.2E0,  4.0E0,  5.0E0,  4.6E0,  2.8E0,    2.7E0,
     *         3.8E0,  5.1E0,  6.1E0,  6.3E0,  5.0E0,    2.0E0/
      data P(1), P(2) / 13.0E0, 11.0E0 /
c
c     ------------------------------------------------------------------
      N = NMAX
      NDEG = N - 1
      IR1 = 1
      NROWS = 1
c
      do 20 IROW = 1,MDATA
        U = (X(IROW)-P(1)) / P(2)
        I = IR1
        A(I,1) = 1.
        do 10 J = 2, NDEG+1
            A(I,J) = A(I,J-1)*U
   10   continue
        B(I) = Y(IROW)
        call SACCUM(A, LDIM, N, B, LDIM, NB, IR1, NROWS, NCOUNT)
   20 continue
c
      print*,'DRSACCUM..  Demo driver for SACCUM.'
      print '(1x,a,i4,a,i4)','MDATA = ',MDATA,', NCOUNT = ',NCOUNT
      call SHFTI(A,LDIM,IR1-1,N,B,LDIM,NB,TAU,KRANK,RNORM,WORK,IP)
      print '(1x,a,I4)','KRANK = ',KRANK
c
c                      The following stmt does a type conversion.
      DOF = NCOUNT - N
      SIGFAC = RNORM(1) / sqrt(DOF)
      call SCOPY(N, B, 1, P(3), 1)
c++ Code for .C. is inactive
c%%   printf(
c%%      "\n NDEG =%2ld          RNORM =%8.4f          SIGFAC =%8.4f",
c%%       ndeg, Rnorm[1], sigfac );
c%%   printf(
c%%   "\n\n P(1),P(2) =         %15.5f%15.5f\n\n P(3),...,P(NDEG+3) =",
c%%   p[0], p[1]);
c%%   for (i = 2; i < (n + 2); i+=3){
c%%      for (j = i; j <= (i < n ? i+2 : n+1); j++)
c%%           printf("%15.5f", p[j] );
c%%           if (i < n-1) printf("\n                     ");}
c%%   printf( "\n" );
c++ Code for ~.C. is active
      print
     *'(/'' NDEG ='',I2,10X,''RNORM ='',F8.4,10X,''SIGFAC ='',F8.4//
     *'' P(1),P(2) ='',9X,2F15.5//'' P(3),...,P(NDEG+3) ='',3F15.5/
     *(21X,3F15.5))', NDEG,RNORM(1),SIGFAC,(P(I),I=1,N+2)
c++ End
      print '(1X/''  I    X       Y       YFIT  R=Y-YFIT''/1X)'
      do 30 I=1,MDATA
        YFIT=SMPVAL(P,NDEG,X(I))
        R=Y(I)-YFIT
        print '(1X,I2,F6.0,2F9.3,F10.3)', I,X(I),Y(I),YFIT,R
   30 continue
      stop
c
      end
