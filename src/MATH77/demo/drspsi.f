      program DRSPSI
c>> 1996-06-19 DRSPSI Krogh  Minor change in format for C conversion.
c>> 1994-10-19 DRSPSI Krogh  Changes to use M77CON
c>> 1994-09-01 DRSPSI CLL
c>> 1994-07-21 DRSPSI WVS Original Code
c--S replaces "?": DR?PSI, ?PSI, ?PSIE
      real             E, P, SPSI, X
      integer I, IE
      external SPSI
      print '(''          X        SPSI(X)     FLAG  EST.ABS.ERR'')'
      x = -4.5e0
      do 10 i = 1, 14
         p = spsi(x)
         call spsie (e, ie)
         print '(2x,2g14.8,i4,2x,g14.8)', x, p, ie, e*abs(p)
         x = x + 1.0e0
   10 continue
      stop
      end
