c     program DRDBLAS2
c>> 1996-06-18 DRDBLAS2 Krogh  Minor format change for C conversion.
c>> 1994-10-19 DRDBLAS2 Krogh  Changes to use M77CON
c>> 1991-11-27 DRDBLAS2 CLL
c>> 1987-12-09 Lawson  Initial Code.
c
c     Demonstrates the use of BLAS subroutines DROTG, DROT, DAXPY,
c     and DCOPY to implement an algorithm for solving a linear
c     least squares problem using sequential accumulation of the
c     data and Givens orthogonal transformations.
c     YTAB() contains rounded values of -2 + 2*X + 3*Exp(-X)
c     -----------------------------------------------------------------
c--D replaces "?": DR?BLAS2, ?ROTG, ?ROT, ?AXPY, ?COPY
c     -----------------------------------------------------------------
      integer MC, MC1, MXY
      parameter ( MC=3, MC1=MC+1, MXY=11 )
      integer IXY, J, NC, NC1, NXY
      double precision X, XTAB(MXY), Y, YTAB(MXY), W(MC1)
      double precision C, RG(MC1,MC1), S
      double precision COEF(MC), DIV, ESTSD, ZERO(1)
c
      data XTAB / 0.0d0,  .1d0,  .2d0,    .3d0,   .4d0,  .5d0,
     *             .6d0,  .7d0,  .8d0,    .9d0,  1.0d0 /
      data YTAB / 1.00d0, .91d0, .86d0,   .82d0,  .81d0, .82d0,
     *             .85d0, .89d0, .95d0,  1.02d0, 1.10d0 /
      data NXY, NC / MXY, MC /
      data ZERO(1) / 0.0d0 /
c     -----------------------------------------------------------------
      NC1 = NC + 1
      call DCOPY(MC1*MC1, ZERO, 0, RG, 1)
      do 20 IXY = 1, NXY
        X = XTAB(IXY)
        Y = YTAB(IXY)
c                                      Build new row of [A:B] in W().
        W(1) = 1.0d0
        W(2) = X
        W(3) = exp(-X)
        W(4) = Y
c                                      Process W() into [R:G].
        do 10 J = 1, NC
          call DROTG(RG(J,J),W(J),C,S)
          call DROT(NC1-J,RG(J,J+1),MC1,W(J+1),1,C,S)
   10   continue
        call DROTG(RG(NC1,NC1),W(NC1),C,S)
   20 continue
c                                      Begin: Solve triangular system.
        call DCOPY(NC,RG(1,NC1),1,COEF,1)
        do 30 J = NC, 1, -1
          DIV = RG(J,J)
          if (DIV .eq. 0.0d0) then
            print '(''ERROR:ZERO DIVISOR AT J ='', I2)', J
            stop
          end if
          COEF(J) = COEF(J) / DIV
          call DAXPY(J-1,-COEF(J),RG(1,J),1,COEF,1)
   30   continue
c                                        End: Solve triangular system.
c
      print'('' Solution: COEF() = '',3f8.3)',(COEF(J),J=1,NC)
      ESTSD = abs(RG(NC1,NC1)) / sqrt(DBLE(NXY-NC))
      print'(/'' Estimated Std. Dev. of data errors ='',f9.5)', ESTSD
      stop
      end
