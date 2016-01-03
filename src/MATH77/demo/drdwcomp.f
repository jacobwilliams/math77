c     program DRDWCOMP
c>> 1994-10-19 DRDWCOMP Krogh  Changes to use M77CON
c>> 1987-12-09 DRDWCOMP Lawson  Initial Code.
c--D replaces "?": DR?WCOMP, ?WSET, ?VECP, ?WSQRT, ?WLOG, ?WPRO1, ?WEXP,
c--&               ?WDIF, ?WCOMP
c     Demo driver for the DWCOMP package.  Derivative arithmetic.
c     ------------------------------------------------------------------
      integer N, NDIM, NMAX
      parameter(NMAX = 3, NDIM = NMAX+1)
      double precision T(NDIM), X1(NDIM), Z1(NDIM), X2(NDIM), Z2(NDIM)
      double precision DIFF(NDIM), ONE, TWO
      parameter(ONE = 1.0D0, TWO = 2.0D0)
c     ------------------------------------------------------------------
      print*,'DRDWCOMP..  Demo driver for the DWCOMP package.'
      N = NMAX
c                                      Set T = 2.0
      call DWSET(N, TWO, ONE, T)
      call DVECP(T,N+1,'0 T =')
c                                      Compute Z1 = log(sqrt(T))
      call DWSQRT(N, T, X1)
      call DWLOG(N, X1, Z1)
      call DVECP(Z1,N+1,'0 Z1 = log(sqrt(T)) =')
c
c                                      Compute Z2 = exp(2.0 * Z1)
      call DWPRO1(N, TWO, Z1, X2)
      call DWEXP(N, X2, Z2)
      call DVECP(Z2,N+1,'0 Z2 = exp(2.0 * Z1) =')
c
c                                      Diff = Z2 - T
      call DWDIF(N, Z2, T, DIFF)
      call DVECP(DIFF,N+1,'0 DIFF = Z2 - T =')
      stop
      end
