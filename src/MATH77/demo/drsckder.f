c     program DrSCKDER
c>> 2007-01/02 DRSCKDER Krogh  Put commas around ':' in formats.
c>> 1996-06-28 DrSCKDER Krogh  Format changes for conversion to C.
c>> 1994-11-02 DrSCKDER Krogh  Changes to use M77CON
c>> 1992-04-15 DrSCKDER CLL
c>> 1992-01-13 C. L. Lawson, JPL.
c  DRSCKDER..  Demo driver for SCKDER.  Checks derivative calculation.
c     ------------------------------------------------------------------
c--S replaces "?": Dr?CKDER, ?CKDER, ?TRG11
c     ------------------------------------------------------------------
      integer I,IMAX, J, JMAX, M,N,LDFJAC,MODE, NMAX
      parameter(LDFJAC = 5, NMAX = 5)
      real             FVEC(15),FJAC(LDFJAC,NMAX)
      real             TEST(LDFJAC,NMAX), TSTMAX, X(NMAX)
      data M, N / LDFJAC, NMAX /
      data X / 0.13e0, 0.14e0, 0.15e0, 0.16e0, 0.17e0 /
c     ------------------------------------------------------------------
      print*,'Program DrSCKDER..  Demo driver for SCKDER.'
      call STRG11(N, X, FVEC ,FJAC, 2)
      MODE = 1
   10 continue
      call SCKDER(MODE, M, N, X, FVEC, FJAC, LDFJAC,
     *            TEST, IMAX, JMAX, TSTMAX)
      if(MODE .eq. 2) then
         call STRG11(N, X, FVEC ,FJAC, 1)
         go to 10
      endif
c
      call STRG11(N, X, FVEC ,FJAC, 1)
      print'(/11x,''X(J) ='',5g11.3,:,/(17x,5g11.3))',(X(J),J=1,N)
      print'(/1x,''  I    FVEC(I)   .....................'',
     *   ''FJAC(I,J)........................''/)'
      do 20 I = 1,M
         print'(1x,i3,1x,g11.3,1x,5g11.3,:,/(17x,5g11.3))',
     *         I,FVEC(I),(FJAC(I,J),J=1,N)
   20 continue
c
      print'(/1x,''TEST(,):''/)'
      do 30 I = 1,M
         print'(1x,i3,13x,5g11.3,:,/(17x,5g11.3))',I,(TEST(I,J),J=1,N)
   30 continue
      print'(/1x,''IMAX ='',i3,'',    JMAX ='',i3,'',    TSTMAX ='',
     *   g11.3)', IMAX, JMAX, TSTMAX
      stop
      end
c     ==================================================================
      subroutine STRG11(N, X, FVEC ,FJAC, IFLAG)
c     Trigonometric test case No. 11 from MINPACK test set developed by
c     J. J. More', B. S. Garbow, and K. E. Hillstrom, Argonne National
c     Laboratories, 1980.
c     ------------------------------------------------------------------
      integer I, IFLAG, J, N
      real             FJAC(N,N), FVEC(N), SUM, TEMP, X(N)
c     ------------------------------------------------------------------
      if (IFLAG .eq. 1) then
c                                        Compute function vector.
         SUM = 0.0e0
         do 10 J = 1, N
            FVEC(J) = cos(X(J))
            SUM = SUM + FVEC(J)
  10     continue
         do 20 J = 1, N
            FVEC(J) = real(N+J) - sin(X(J)) - SUM - real(J)*FVEC(J)
  20     continue
      elseif (IFLAG .eq. 2) then
c                                        Compute Jacobian matrix.
         do 40 J = 1, N
            TEMP = sin(X(J))
            do 30 I = 1, N
               FJAC(I,J) = TEMP
  30        continue
            FJAC(J,J) = real(J+1)*TEMP - cos(X(J))
  40     continue
      endif
      return
      end
