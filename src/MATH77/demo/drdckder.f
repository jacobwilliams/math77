c     program DrDCKDER
c>> 2007-01/02 DRDCKDER Krogh  Put commas around ':' in formats.
c>> 1996-06-28 DrDCKDER Krogh  Format changes for conversion to C.
c>> 1994-11-02 DrDCKDER Krogh  Changes to use M77CON
c>> 1992-04-15 DrDCKDER CLL
c>> 1992-01-13 C. L. Lawson, JPL.
c  DRDCKDER..  Demo driver for DCKDER.  Checks derivative calculation.
c     ------------------------------------------------------------------
c--D replaces "?": Dr?CKDER, ?CKDER, ?TRG11
c     ------------------------------------------------------------------
      integer I,IMAX, J, JMAX, M,N,LDFJAC,MODE, NMAX
      parameter(LDFJAC = 5, NMAX = 5)
      double precision FVEC(15),FJAC(LDFJAC,NMAX)
      double precision TEST(LDFJAC,NMAX), TSTMAX, X(NMAX)
      data M, N / LDFJAC, NMAX /
      data X / 0.13d0, 0.14d0, 0.15d0, 0.16d0, 0.17d0 /
c     ------------------------------------------------------------------
      print*,'Program DrDCKDER..  Demo driver for DCKDER.'
      call DTRG11(N, X, FVEC ,FJAC, 2)
      MODE = 1
   10 continue
      call DCKDER(MODE, M, N, X, FVEC, FJAC, LDFJAC,
     *            TEST, IMAX, JMAX, TSTMAX)
      if(MODE .eq. 2) then
         call DTRG11(N, X, FVEC ,FJAC, 1)
         go to 10
      endif
c
      call DTRG11(N, X, FVEC ,FJAC, 1)
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
      subroutine DTRG11(N, X, FVEC ,FJAC, IFLAG)
c     Trigonometric test case No. 11 from MINPACK test set developed by
c     J. J. More', B. S. Garbow, and K. E. Hillstrom, Argonne National
c     Laboratories, 1980.
c     ------------------------------------------------------------------
      integer I, IFLAG, J, N
      double precision FJAC(N,N), FVEC(N), SUM, TEMP, X(N)
c     ------------------------------------------------------------------
      if (IFLAG .eq. 1) then
c                                        Compute function vector.
         SUM = 0.0d0
         do 10 J = 1, N
            FVEC(J) = cos(X(J))
            SUM = SUM + FVEC(J)
  10     continue
         do 20 J = 1, N
            FVEC(J) = dble(N+J) - sin(X(J)) - SUM - dble(J)*FVEC(J)
  20     continue
      elseif (IFLAG .eq. 2) then
c                                        Compute Jacobian matrix.
         do 40 J = 1, N
            TEMP = sin(X(J))
            do 30 I = 1, N
               FJAC(I,J) = TEMP
  30        continue
            FJAC(J,J) = dble(J+1)*TEMP - cos(X(J))
  40     continue
      endif
      return
      end
