c     program DRCWOFZ
c>> 2009-10-27 DRCWOFZ Krogh/Snyder Added equivalence for Nag compiler.
c>> 1996-06-10 DRCWOFZ Krogh Moved formats up, M77CON changes for C.
c>> 1992-03-13 DRCWOFZ WV Snyder Created from DRCWOFZ
c>> 1991-11-20 DRCWOFZ WV Snyder Original Code
c Conversion should only be done from "Z" to "C" for processing to C.
c--C replaces "?": DR?WOFZ, ?WOFZ
c
c     Demonstration driver for CWOFZ.
c
      complex Z, W
      real ZR(2), WR(2)
      equivalence ( Z, ZR ), ( W, WR )
      integer IFLAG
      integer IX, IY
c
c     Evaluate w(z) at 25 points [0..4] x [0..4] in the complex plane.
c
10    format ('  x  y        Re w            Im w')
20    format (2i3, 2g17.8)
      print 10
      do 40 ix = 0, 4
         do 30 iy = 0, 4
            z = cmplx(real(ix), real(iy))
            call cwofz (zr, wr, iflag)
            print 20, ix, iy, w
30       continue
40    continue
      stop
      end

