c     program DRSBLAS4
c>> 1994-10-19 DRSBLAS4 Krogh  Changes to use M77CON
c>> 1992-05-12 DRSBLAS4 CLL Improved consistency of various versions.
c>> 1992-04-15 CLL
c>> 1992-03-16 CLL
c>> 1991-12-02 DRSBLAS4 CLL
c>> 1991-07-31 DRSBLAS4 CLL
c
c     Demonstrates the use of BLAS subroutines SROTMG, SROTM, SAXPY,
c     and SCOPY to implement an algorithm for solving a linear
c     least squares problem using sequential accumulation of the
c     data and Modified (Fast) Givens orthogonal transformations.
c     YTAB() contains rounded values of -2 + 2*X + 3*Exp(-X)
c
c     ------------------------------------------------------------------
c--S replaces "?": DR?BLAS4, ?AXPY, ?ROTMG, ?ROTM, ?COPY, ?VECP
c     ------------------------------------------------------------------
      integer MC, MC1, MC2, MXY
      parameter ( MC=3, MC1=MC+1, MC2=MC+2, MXY=11 )
      real             XTAB(MXY), YTAB(MXY), W(MC2), RG(MC1,MC2)
      real             C(MC), ZERO(1), X, Y, ESTSD, DIV, PARAM(5)
      real             ONE(1)
      integer NXY, NC, NC1, NC2, IXY, J
c
      data XTAB / 0.0e0, .1e0, .2e0, .3e0, .4e0, .5e0, .6e0, .7e0,
     *           .8e0, .9e0, 1.0e0 /
      data YTAB / 1.00e0, .91e0, .86e0, .82e0, .81e0, .82e0, .85e0,
     * .89e0, .95e0, 1.02e0, 1.10e0 /
      data NXY, NC / 11, 3 /
      data ONE(1), ZERO(1) / 1.0e0, 0.0e0 /
c     ------------------------------------------------------------------
      print'(a/)',' DRSBLAS4..  Demo for SROTMG and SROTM.'
      NC1 = NC + 1
      NC2 = NC1 + 1
      call SCOPY(MC1*MC2, ZERO, 0, RG, 1)
      call SCOPY(NC1, ONE, 0, RG(1,NC2), 1)
      do 20 IXY = 1, NXY
        X = XTAB(IXY)
        Y = YTAB(IXY)
c
c          Set W() to be the next row of the least-squares problem.
c
        W(1) = 1.0e0
        W(2) = X
        W(3) = EXP(-X)
        W(4) = Y
        W(5) = 1.0E0
c
c        Accumulate W() into the triangular matrix [R:G]
c        using Givens rotations.
c
        do 10 J = 1, NC1
          call SROTMG(RG(J,NC2), W(NC2), RG(J,J), W(J), PARAM)
          if(J .lt. NC1) call SROTM(NC1-J,RG(J,J+1),MC1,W(J+1),1,PARAM)
   10   continue
c
   20 continue
c
c               Solve the triangular system.
c
        call SCOPY(NC,RG(1,NC1),1,C,1)
        do 30 J = NC, 1, -1
          DIV = RG(J,J)
          if (DIV .EQ. 0.0e0) then
            print*,'ERROR:ZERO DIVISOR AT J =',J
            stop
          end if
          C(J) = C(J) / DIV
          call SAXPY(J-1,-C(J),RG(1,J),1,C,1)
   30 continue
c
      call SVECP(C,NC,'0 Solution: C()=')
      print*,' '
      ESTSD = abs(RG(NC1,NC1)) * sqrt(RG(NC1,NC2)) / sqrt(REAL(NXY-NC))
      print*,'Estimated STD. DEV. of data errors =',ESTSD
      stop
      end
