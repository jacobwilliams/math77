c     program DRSASINH
c>> 2001-05-22 DRSASINH Krogh Minor change for making .f90 version.
c>> 1996-06-26 DRSASINH Krogh Changed code to simplify conversion to C.
c>> 1996-05-28 DRSASINH Krogh Added external state. & moved up formats
c>> 1994-10-19 DRSASINH Krogh  Changes to use M77CON
c>> 1988-11-17 DRSASINH CLL
c--S replaces "?": DR?ASINH,?ASINH,?ACOSH,?ATANH,?ACTNH,?ASECH,?ACSCH
c
      integer I
      real             X1(3),X2(3)
      external SASINH,SACOSH,SATANH,SACTNH,SASECH,SACSCH
      real             SASINH,SACOSH,SATANH,SACTNH,SASECH,SACSCH
c
      data X1/0.1e0, 0.5e0, 0.9e0/
      data X2/1.1e0, 10.0e0, 100.0e0/
c
   10 format('     X',11X,'SASINH',10X,'SATANH',10X,
     *  'SASECH',10X,'SACSCH')
   20 format('    ',3('-'),10X,4(6('-'),10X))
   30 format('     X',11X,'SASINH',10X,'SACOSH',10X,
     *  'SACTNH',10X,'SACSCH')
c
      print 10
      print 20
      do 40 I = 1, 3
         print '(1X,F6.2,1X,4F16.6)', X1(I),SASINH(X1(I)),
     *      SATANH(X1(I)), SASECH(X1(I)),SACSCH(X1(I))
   40 continue
      print '('' ''/)'
      print 30
      print 20
      do 50 I = 1, 3
         print  '(1X,F6.2,1X,4F16.6)', X2(I),SASINH(X2(I)),
     *      SACOSH(X2(I)), SACTNH(X2(I)),SACSCH(X2(I))
   50 continue
      stop
      end
