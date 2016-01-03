      real             function SLNREL (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>>   1994-10-20 SLNREL Krogh  Changes to use M77CON
c>>   1990-11-27 SLNREL WV Snyder Initial Code.
c
c     Compute LOG(1+X).
c
c     This procedure uses approximation 2707 from Hart, et. al. when
c     1/sqrt(2) < 1+x < sqrt(2), and otherwise uses the Fortran
c     intrinsic logarithm routine.
c
c--S replaces "?": ?LNREL
      real             X
      real             P0, P1, P2, P3, P4, Q0, Q1, Q2, Q3, Z, ZSQ
      real             BOT, TOP
      parameter (P0 = +0.75040 94990 77712 22174 55611 007 E+2 )
      parameter (P1 = -0.13456 69115 05043 02353 18253 537 E+3 )
      parameter (P2 = +0.74137 19213 24860 25127 79336 47  E+2 )
      parameter (P3 = -0.12772 49755 01233 08199 84385     E+2 )
      parameter (P4 = +0.33271 08381 08768 69381 44        E00 )
      parameter (Q0 = +0.37520 47495 38856 11087 27775 374 E+2 )
      parameter (Q1 = -0.79790 28073 71500 48794 39951 583 E+2 )
      parameter (Q2 = +0.56161 26132 11825 72920 58560 36  E+2 )
      parameter (Q3 = -0.14508 68091 85808 26853 62325     E+2 )
c     parameter (Q4 = +1.0                                 E00 )
      parameter (BOT = .70710 67811 86547 52440 E0 - 1.0E0 )
      parameter (TOP = 1.4142 13562 37309 50488 E0 - 1.0E0 )
c
      if (x .lt. bot .or. x .gt. top) then
         slnrel = log(x+1.0e0)
      else
         z = x / (x+2.0e0)
         zsq = z*z
         slnrel = z * ((((p4*zsq+p3)*zsq+p2)*zsq+p1)*zsq+p0)/
     1                ((((   zsq+q3)*zsq+q2)*zsq+q1)*zsq+q0)
      end if
      return
      end
