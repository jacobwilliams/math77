c     program DRSBLAS2
c>> 1996-06-18 DRSBLAS2 Krogh  Minor format change for C conversion.
c>> 1994-10-19 DRSBLAS2 Krogh  Changes to use M77CON
c>> 1991-11-27 DRSBLAS2 CLL
c>> 1987-12-09 Lawson  Initial Code.
c
c     Demonstrates the use of BLAS subroutines SROTG, SROT, SAXPY,
c     and SCOPY to implement an algorithm for solving a linear
c     least squares problem using sequential accumulation of the
c     data and Givens orthogonal transformations.
c     YTAB() contains rounded values of -2 + 2*X + 3*Exp(-X)
c     -----------------------------------------------------------------
c--S replaces "?": DR?BLAS2, ?ROTG, ?ROT, ?AXPY, ?COPY
c     -----------------------------------------------------------------
      integer MC, MC1, MXY
      parameter ( MC=3, MC1=MC+1, MXY=11 )
      integer IXY, J, NC, NC1, NXY
      real             X, XTAB(MXY), Y, YTAB(MXY), W(MC1)
      real             C, RG(MC1,MC1), S
      real             COEF(MC), DIV, ESTSD, ZERO(1)
c
      data XTAB / 0.0e0,  .1e0,  .2e0,    .3e0,   .4e0,  .5e0,
     *             .6e0,  .7e0,  .8e0,    .9e0,  1.0e0 /
      data YTAB / 1.00e0, .91e0, .86e0,   .82e0,  .81e0, .82e0,
     *             .85e0, .89e0, .95e0,  1.02e0, 1.10e0 /
      data NXY, NC / MXY, MC /
      data ZERO(1) / 0.0e0 /
c     -----------------------------------------------------------------
      NC1 = NC + 1
      call SCOPY(MC1*MC1, ZERO, 0, RG, 1)
      do 20 IXY = 1, NXY
        X = XTAB(IXY)
        Y = YTAB(IXY)
c                                      Build new row of [A:B] in W().
        W(1) = 1.0e0
        W(2) = X
        W(3) = exp(-X)
        W(4) = Y
c                                      Process W() into [R:G].
        do 10 J = 1, NC
          call SROTG(RG(J,J),W(J),C,S)
          call SROT(NC1-J,RG(J,J+1),MC1,W(J+1),1,C,S)
   10   continue
        call SROTG(RG(NC1,NC1),W(NC1),C,S)
   20 continue
c                                      Begin: Solve triangular system.
        call SCOPY(NC,RG(1,NC1),1,COEF,1)
        do 30 J = NC, 1, -1
          DIV = RG(J,J)
          if (DIV .eq. 0.0e0) then
            print '(''ERROR:ZERO DIVISOR AT J ='', I2)', J
            stop
          end if
          COEF(J) = COEF(J) / DIV
          call SAXPY(J-1,-COEF(J),RG(1,J),1,COEF,1)
   30   continue
c                                        End: Solve triangular system.
c
      print'('' Solution: COEF() = '',3f8.3)',(COEF(J),J=1,NC)
      ESTSD = abs(RG(NC1,NC1)) / sqrt(REAL(NXY-NC))
      print'(/'' Estimated Std. Dev. of data errors ='',f9.5)', ESTSD
      stop
      end
