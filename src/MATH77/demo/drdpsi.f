      program DRDPSI
c>> 1996-06-19 DRDPSI Krogh  Minor change in format for C conversion.
c>> 1994-10-19 DRDPSI Krogh  Changes to use M77CON
c>> 1994-09-01 DRDPSI CLL
c>> 1994-07-21 DRDPSI WVS Original Code
c--D replaces "?": DR?PSI, ?PSI, ?PSIE
      double precision E, P, DPSI, X
      integer I, IE
      external DPSI
      print '(''          X        DPSI(X)     FLAG  EST.ABS.ERR'')'
      x = -4.5d0
      do 10 i = 1, 14
         p = dpsi(x)
         call dpsie (e, ie)
         print '(2x,2g14.8,i4,2x,g14.8)', x, p, ie, e*abs(p)
         x = x + 1.0d0
   10 continue
      stop
      end
