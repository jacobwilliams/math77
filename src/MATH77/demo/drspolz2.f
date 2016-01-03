c     Program DRSPOLZ2
c>> 2009-10-28 DRZPOLZ2 Krogh Mods to get comples used in single prec.
c>> 1996-07-03 DRSPOLZ2 Krogh Set for deriving single precision C vers.
c>> 1995-05-28 DRSPOLZ2 Krogh Moved formats up.
c>> 1994-08-09 DRSPOLZ2 WVS Removed '0' in format
c>> 1991-11-20 DRSPOLZ2 CLL Editing for Fortran 90.
c>> 1987-12-09 DRSPOLZ2 Lawson  Initial Code.
c Conversion should only be done from "D" to "S" for processing to C.
c--S replaces "?": DR?POLZ2, ?POLZ2
c     Demonstration driver for SPOLZ2.
c
      real              DC(3,3)
c++ CODE for .D. | .C. is inactive 
C      real             Z(2,2)
c++ CODE for .S. & ~.C. is active
      complex Z(2)
c++ END
 
      integer  I, J, K, N
c
      data (DC(I,1),I=1,3) / 1.E0, 1.E0, -1.E0 /
      data (DC(I,2),I=1,3) / 2.E0, -12.E0, 26.E0 /
      data (DC(I,3),I=1,3) / 1.E0, -4.E0, 4.E0 /
      data N / 2 /
c
  100 format(' ','Degree =',I2/' ',
     *       'Coefficients =',6X,3(F10.4,1X))
  200 format(' ','Roots ='/(2(1X:'(',1X,F8.5,',',1X,F8.5,2X,')',2X)))
  300 format(//' ')
c
      do 40 J = 1,3
         print 100, N, (DC(I, J),I=1,3)
        call SPOLZ2(DC(1,J),Z)
c++ CODE for .D. & ~.C. is inactive 
C        print '('' Roots =''/(2(1X:''('',1X,F8.5,'','',1X,F8.5,2X,
C     *    '')'',2X)))', (Z(1,K),Z(2,K),K=1,2)
c++ CODE for .S. & ~.C. is active
        print '('' Roots =''/(2(1X:''('',1X,F8.5,'','',1X,F8.5,2X,
     *    '')'',2X)))', (Z(K),K=1,2)
c++ CODE for .C. is inactive
c%%        printf( "\n Roots =\n  ( %8.5f, %8.5f )  ( %8.5f, %8.5f ) ",
c%%           z[0][0], z[0][1], z[1][0], z[1][1] );
c++ End
        print 300
   40 continue
      end
