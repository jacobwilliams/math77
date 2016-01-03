c     program DRSWCOMP
c>> 1994-10-19 DRSWCOMP Krogh  Changes to use M77CON
c>> 1987-12-09 DRSWCOMP Lawson  Initial Code.
c--S replaces "?": DR?WCOMP, ?WSET, ?VECP, ?WSQRT, ?WLOG, ?WPRO1, ?WEXP,
c--&               ?WDIF, ?WCOMP
c     Demo driver for the SWCOMP package.  Derivative arithmetic.
c     ------------------------------------------------------------------
      integer N, NDIM, NMAX
      parameter(NMAX = 3, NDIM = NMAX+1)
      real             T(NDIM), X1(NDIM), Z1(NDIM), X2(NDIM), Z2(NDIM)
      real             DIFF(NDIM), ONE, TWO
      parameter(ONE = 1.0E0, TWO = 2.0E0)
c     ------------------------------------------------------------------
      print*,'DRSWCOMP..  Demo driver for the SWCOMP package.'
      N = NMAX
c                                      Set T = 2.0
      call SWSET(N, TWO, ONE, T)
      call SVECP(T,N+1,'0 T =')
c                                      Compute Z1 = log(sqrt(T))
      call SWSQRT(N, T, X1)
      call SWLOG(N, X1, Z1)
      call SVECP(Z1,N+1,'0 Z1 = log(sqrt(T)) =')
c
c                                      Compute Z2 = exp(2.0 * Z1)
      call SWPRO1(N, TWO, Z1, X2)
      call SWEXP(N, X2, Z2)
      call SVECP(Z2,N+1,'0 Z2 = exp(2.0 * Z1) =')
c
c                                      Diff = Z2 - T
      call SWDIF(N, Z2, T, DIFF)
      call SVECP(DIFF,N+1,'0 DIFF = Z2 - T =')
      stop
      end
