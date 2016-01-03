      SUBROUTINE STGPRG(X,Y,NP,TRIANG,B,NB,NT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1997-07-01 STGPRG Krogh Remove MB (CLL suggestion)
c>> 1997-06-15 STGPRG CLL Now assuming list in B() is circular.
c>> 1996-06-24 STGPRG  Krogh   Format changes for conversion to C.
c>> 1996-03-30 STGPRG  Krogh   MIN0 => MIN
C>> 1996-02-02 STGPRG CLL
C>> 1995-09-26 STGPRG CLL Editing for inclusion into MATH77.
C
c     C.L.LAWSON, JPL, 1976 NOV 30
c  This subroutine prints the (x,y) data from X() and Y(), and the
c  pointers defining the structure of a triangular grid from TRIANG(),
c  and the pointers defining the boundary of the convex hull of the
c  triangular grid from B(,).
c  All subroutine arguments are of INTENT IN.
c     ------------------------------------------------------------------
c--S replaces "?": ?TGPRG, ?TGGET
c     ------------------------------------------------------------------
      integer b(4, *), BPR(5,20)
      integer i, i1, i2, index, IPR(14), j, k, len, limit, line
      INTEGER nb, ni, NP, nt
      integer TRIANG(1:*)
      character label(5)*8
      REAL             X(NP),Y(NP)
      DATA LABEL  / 'INDEX'   , 'FORWARD' , 'BACKWARD',
     *              'VERTEX'  , 'TRIANGLE'/
c     ------------------------------------------------------------------

      NI= NP-NB
      print '(/'' DATA AND POINTERS DEFINING A TRIANGULAR GRID''//
     * '' NP ='',i5,'', NB ='',i5,'', NI ='',i5,'', NT ='',i5///
     * ''     (X,Y)-COORDINATES OF VERTICES..'')', NP, NB, NI, NT
c                                            PRINT X(),Y()
      I1 = 1
      do 20 i1 = 1, np, 6
         I2= MIN(I1+5,NP)
         LEN=I2-I1+1
C
        DO 10 I=1,LEN
            IPR(I)= I1-1+I
   10   continue
C
         print '(/1x,10I12)', (IPR(I),I=1,LEN)
         print '(1x,10F12.5)', (X(I),I=I1,I2)
         print '(1x,10F12.5)', (Y(I),I=I1,I2)
   20 continue
c                                            PRINT TRIANG()
C
      print '(//'' POINTERS DEFINING TRIANGLES..''//
     * '' INDEX   ADJACENT TRIANGLES  *****VERTICES*****'')'
      DO 30 I=1,NT
         IPR(1)=I
         CALL STGGET(I,IPR(2), TRIANG)
            print '( 1x,I5,3X,3I6,2X,3I6)', (IPR(J),J=1,7)
   30 continue
c                                                 PRINT B()
      LIMIT = NB+5
      print '(//'' BOUNDARY LIST..''/)'
      INDEX= 1
c                             Begin loop to print boundary list.
   40 continue
         LEN=0
         DO 60 I=1,20
            LIMIT = LIMIT-1
            IF(LIMIT .LE. 0) go to 70
            LEN=I
            BPR(1,I) = INDEX
            DO 50 J=1,4
               BPR(J+1,I)=  B(J, INDEX)
   50       continue
            INDEX = B(1, INDEX)
            IF(INDEX .EQ. 0 .or. index .eq. 1)  go to 80
   60    continue
   70    continue
   80    continue
         IF(LEN .EQ. 0) go to 100
         DO 90 LINE=1,5
            print '(1X,a8,2X,20I5)', LABEL(LINE),(BPR(LINE,K),K=1,LEN)
   90    continue
         IF (LIMIT .LE. 0) THEN
            print '(/'' ERROR IN BOUNDARY LIST.  QUITTING.'')'
            go to 110
         END IF
         IF(INDEX .ne. 0 .and. index .ne. 1)  go to 40
c                             End loop to print boundary list.
  100 continue
  110 continue
      RETURN
      END
