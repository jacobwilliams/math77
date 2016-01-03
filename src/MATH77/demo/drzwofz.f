c     program DRZWOFZ
c>> 1996-06-10 DRZWOFZ Krogh Moved formats up, M77CON changes for C.
c>> 1992-03-13 DRZWOFZ WV Snyder Created from DRCWOFZ
c>> 1991-11-20 DRZWOFZ WV Snyder Original Code
c Conversion should only be done from "Z" to "C" for processing to C.
c--Z replaces "?": DR?WOFZ, ?WOFZ
c
c     Demonstration driver for ZWOFZ.
c
      double precision Z(2), W(2)
      integer IFLAG
      integer IX, IY
10    format ('  x  y             Re w                      Im w')
20    format (2i3, 2g27.18)
c
c     Evaluate w(z) at 25 points [0..4] x [0..4] in the complex plane.
c
      print 10
      do 40 ix = 0, 4
         do 30 iy = 0, 4
            z(1) = dble(ix)
            z(2) = dble(iy)
            call zwofz (z, w, iflag)
            print 20, ix, iy, w
30       continue
40    continue
      stop
      end

