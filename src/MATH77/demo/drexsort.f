c>>   1996-06-24  DREXSORT  Krogh   Added code for conversion to C.
c>>   1995-05-28  DREXSORT  Krogh   Converted SFTRAN to Fortran
c>>   1990-02-09  DREXSORT  Snyder  Initial code.
c
c     Test driver for EXSORT.
c
c     Sort LDATA=10000 random numbers using EXSORT.
c     Check whether they are in order.
c
      integer LENBUF
      parameter (LENBUF=1000)
      integer L(1:LENBUF),OPTION,OUTFIL
      parameter (OPTION=1)
      external DATAOP
      real R(1:LENBUF)
      common /RCOM/ R
c
      call exsort (dataop,LENBUF,l,option,outfil)
      stop
      end

      subroutine DATAOP (IOP, I, J, IFLAG)
      integer LDATA, LENBUF
      parameter (LDATA=10000, LENBUF=1000)
      integer IOP, I, J, IFLAG
      logical ISopen(4)
      save ISOPEN
      integer NCOMP, NDATA
      logical OK
      save NCOMP, NDATA, OK
      real PREV,R(1:LENBUF),SRANU
      save PREV
      external SRANU
      common /RCOM/ R
      data ISOPEN, NCOMP, NDATA, OK, PREV /4*.FALSE., 0, 0, .TRUE., -1./
c++ CODE for .C. is inactive
c%% static float end_of_seq[1] = {-1.0e0};
c%% static char *fname[4]={"scratch1","scratch2","scratch3","scratch4"};
c%% static FILE *fp[4];
c++ END
c
      go to (10, 20, 30, 40, 50, 60, 70, 80), IOP
      return
c      case 1 @ initial input into record J
   10  continue
         ndata = ndata + 1
         if (ndata.le.LDATA) then
            r(j)=sranu()
            iflag=0
         else
            iflag=1
         end if
         return
c      case 2 @ write scratch from J onto file I
   20  continue
c%%      fwrite( &rcom.r[j-1], sizeof(rcom.r[0]), 1L, fp[i-1]);
         write (i+10) r(j)
         return
c      case 3 @ write end-of-sequence onto file I
   30  continue
c%%      fwrite( end_of_seq, sizeof(rcom.r[0]), 1L, fp[i-1]);
         write (i+10) -1.0E0
         return
c      case 4 @ read scratch into J from file I
   40  continue
c%%      fread( &rcom.r[j-1], sizeof(rcom.r[0]), 1L, fp[i-1]);
         read (i+10) r(j)
         iflag=0
         if (r(j).lt.0.0) iflag=1
         return
c      case 5 @ rewind file I
   50  continue
         if (.not. isopen(i)) then
c%%         fp[i-1] = fopen(fname[i-1], "wb+");
            open (i+10, status='scratch',form='unformatted')
            isopen(i)=.true.
         end if
c%%      rewind( fp[i-1] );
         rewind (i+10)
         return
c      case 6 @ output from record J
   60  continue
         if (i.ne.0) then
            if (ok) then
               print '('' EXSORT succeeded using'',i7,'' compares'')',
     1            ncomp
            else
               print *,'EXSORT failed'
            end if
         else
            if (r(j).lt.prev) ok=.false.
            prev=r(j)
         end if
         return
c      case 7 @ move record I to record J
   70  continue
         r(j)=r(i)
         return
c      case 8 @ iflage I and J
   80  continue
         ncomp=ncomp+1
         if (r(i)-r(j)) 110,120,130
  110    iflag=-1
         return
  120    iflag=0
         return
  130    iflag=+1
         return
c
      end
