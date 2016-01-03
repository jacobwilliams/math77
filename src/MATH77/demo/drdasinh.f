c     program DRDASINH
c>> 2001-05-22 DRDASINH Krogh Minor change for making .f90 version.
c>> 1996-06-26 DRDASINH Krogh Changed code to simplify conversion to C.
c>> 1996-05-28 DRDASINH Krogh Added external state. & moved up formats
c>> 1994-10-19 DRDASINH Krogh  Changes to use M77CON
c>> 1988-11-17 DRDASINH CLL
c--D replaces "?": DR?ASINH,?ASINH,?ACOSH,?ATANH,?ACTNH,?ASECH,?ACSCH
c
      integer I
      double precision X1(3),X2(3)
      external DASINH,DACOSH,DATANH,DACTNH,DASECH,DACSCH
      double precision DASINH,DACOSH,DATANH,DACTNH,DASECH,DACSCH
c
      data X1/0.1d0, 0.5d0, 0.9d0/
      data X2/1.1d0, 10.0d0, 100.0d0/
c
   10 format('     X',11X,'DASINH',10X,'DATANH',10X,
     *  'DASECH',10X,'DACSCH')
   20 format('    ',3('-'),10X,4(6('-'),10X))
   30 format('     X',11X,'DASINH',10X,'DACOSH',10X,
     *  'DACTNH',10X,'DACSCH')
c
      print 10
      print 20
      do 40 I = 1, 3
         print '(1X,F6.2,1X,4F16.6)', X1(I),DASINH(X1(I)),
     *      DATANH(X1(I)), DASECH(X1(I)),DACSCH(X1(I))
   40 continue
      print '('' ''/)'
      print 30
      print 20
      do 50 I = 1, 3
         print  '(1X,F6.2,1X,4F16.6)', X2(I),DASINH(X2(I)),
     *      DACOSH(X2(I)), DACTNH(X2(I)),DACSCH(X2(I))
   50 continue
      stop
      end
