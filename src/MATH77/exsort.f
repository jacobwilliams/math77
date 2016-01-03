      subroutine EXSORT (DATAOP,MAXX,LIST,OPTION,OUTFIL)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>>   1996-05-15 EXSORT Krogh   Changes to use .C. and C%%.
c>>   1995-11-17 EXSORT Krogh   Convert SFTRAN to Fortran 77.
c>>   1990-02-07 EXSORT WV Snyder at JPL, 91109, Convert to SFtran
      external DATAOP
      integer MAXX, LIST(*), OPTION, OUTFIL
c
c DATAOP is a user-coded subroutine used to perform all operations on
c     the data.  The operations include acquiring data from outside of
c     the EXSORT interface, manipulating scratch files and performing
c     input and output on scratch files, moving data from one memory
c     area to another, returning sorted data from the EXSORT interface
c     to the user program, and comparing one datum with another.
c
c     calling sequence for DATAOP:
c     call DATAOP (IOP,I1,I2,IFLAG)
c     where all arguments are integers,
c     IOP defines the operation to be performed,
c     I1 is usually an index (1-4) of a file upon which to operate,
c     I2 is usually an index (1-maxx) of the data area to use.
c     IFLAG is a flag to be set by DATAOP.
c     the values of IOP and the corresponding required actions are
c     detailed below.
c
c IOP  ACTION
c
c 1   Place a datum from the set to be sorted in the record area indexed
c     by I2.  I1 is irrelevant.  Set the value of IFLAG to zero if a
c     datum is available.  Set the value of IFLAG to any non-zero value
c     if the entire data set has been provided by this avenue.
c
c 2   Write the datum in the record area indexed by I2 on the intermedi-
c     ate scratch file indexed by I1.  The value of IFLAG is irrelevant.
c
c 3   Place an end-of-string mark (eof, unique datum, etc), to be
c     recognized during performance of operation 4 (below), on the
c     intermediate scratch file indexed by I1.  The values of I2 and
c     IFLAG are irrelevant.
c
c 4   Read a datum from the intermediate scratch file indexed by I1 into
c     the record area indexed by I2.  Set the value of IFLAG to zero if
c     a datum is available.  Set the value of IFLAG to any non-zero
c     value if an end-of-string mark created by operation 3 is detected.
c
c 5   Rewind the intermediate scratch file indexed by I1.  The values of
c     I2 and IFLAG are irrelevant.
c
c 6   If I1 is zero, a datum from the sorted data set is in the record
c     area indexed by I2.  If I1 is non-zero, the entire sorted data set
c     has been provided by this avenue.  The value of IFLAG is irrele-
c     vant.
c
c 7   Move the datum in the record area indexed by I1 to the record area
c     indexed by I2.  The value of IFLAG is irrelevant.
c
c 8   Compare the datum in the record area indexed by I1 to the datum in
c     the record area indexed by I2.  Set IFLAG to some negative value
c     if the datum in the record area indexed by I1 is to be sorted
c     before the datum in the record area indexed by I2;  set the value
c     of IFLAG to zero if the order of the records is immaterial; set
c     IFLAG to some positive number if the datum in the record area
c     indexed by I1 is to be sorted after the datum in the record area
c     indexed by I2.
c
c MAXX is the number of record areas available.  The in-core sort will
c     use MAXX, MAXX-1 or MAXX-2 record areas, so MAXX must be at least
c     4.
c
c LIST is the array used by INSORT for pointers.  LIST must be at least
c     MAXX words long.
c
c OPTION specifies the action to take if the data are initially ordered
c     or at worst disordered in blocks of less than MAXX, but cannot be
c     entirely sorted in core.  If OPTION is zero and the data are init
c     ially ordered, the value of OUTFIL will be the index of the file
c     containing the ordered data.  If OPTION is zero but the data are
c     not initially ordered, the value of OUTFIL will be zero, and the
c     data will have been passed via DATAOP (6,I1,I2,IFLAG).  If OPTION
c     is non-zero, the data will always be passed via DATAOP (6,...),
c     and the value of OUTFIL will be irrelevant.
c
c     *****     External References     ********************************
c
c INSRTX  sorts a block of data in memory.  INSRTX is a special entry
c         in INSORT that allows using DATAOP instead of INSORT's usual
c         2-argument compare routine.
c PVEC    converts the list produced by INSRTX to a permutation vector.
c         This is done to allow a binary search in the sorted data set.
c
c
c     *****     Local Variables     ************************************
c
      integer END, HEAD, I, IFLAG, IN(4), IN1, IN2, IN3, IN4, J
      integer KODE, L, M, MAXDAT, MAXIND, MAXSTR, MININD, MINSTR, MX1
      integer N, NBS, NSTRNG(4), OUT(2), OUTAPE, OUT1, OUT2, SPLIT, TOP
      equivalence (IN(1),IN1), (IN(2),IN2), (IN(3),IN3), (IN(4),IN4)
      equivalence (OUT(1),OUT1), (OUT(2),OUT2)
C%%   long int dum;
      integer DUM
c
c     *****     Executable Statements     ******************************
c
c     Initialize.
c
      outfil=0
      n=0
      kode=2
      maxdat=maxx-1
      mx1=maxx+1
      nbs=0
      minstr=1
      maxstr=2
      minind=maxx
      maxind=maxx-1
c
c     Fill the user's data area and sort it.  If end-of-input does not
c     occur, write the data on a scratch file and do a merge later,
c     if necessary.
c
c        Start forever block
   20 continue
         if (n.lt.maxdat) then
            n=n+1
C%%           (*dataop)( 1, 0, n, &end );
            call dataop (1,0,n,end)
            if (end .eq. 0) go to 20
            n=n-1
            if (n.eq.0) then
               if (nbs.ne.0) then
                  if (nbs.ne.1) go to 200
c                 One block contained all the data.  Emit the data from
c                 memory, instead of reading it from scratch.
C%%                 (*dataop)( 5, 1, 0, &dum );
                  call dataop (5,1,0,dum)
c                      Ready for final output from memory
                  kode=6
                  outape=0
                  go to 120
               end if
C%%              (*dataop)( 6, 1, 0, &dum );
               call dataop (6,1,0,dum)
               return
            end if
         end if
         call insrtx (dataop,n,list,head)
         nbs=nbs+1
         outape=minstr
         if (nbs.eq.1) then
            if (end.ne.0) then
c                      Ready for final output from memory
               kode=6
               outape=0
            else
               do 40 i=1,4
C%%              (*dataop)( 5, i, 0, &dum );
                  call dataop (5,i,0,dum)
                  nstrng(i)=0
   40          continue
               nstrng(1)=1
            end if
         else
c
c           Another block has been sorted.  See if it will fit on an
c           existing string.
c
            iflag=-1
C%%                if (Nstrng[2] != 0)
C%%                        (*dataop)( 8, head, maxind, &iflag );
            if (nstrng(2).ne.0) call dataop (8,head,maxind,iflag)
            if (iflag.lt.0) then
C%%              (*dataop)( 8, head, minind, &dum );
               call dataop (8,head,minind,iflag)
            else
               outape=maxstr
            end if
            if (iflag .lt. 0) then
c
c              The sorted string won't fit on an existing string.  Will
c              part of it fit?
c
               call pvec (list,head)
C%%              (*dataop)( 8, List[n], minind, &iflag );
               call dataop (8,list(n),minind,iflag)
               if (iflag .lt. 0) then
c
c                 None of the list will fit.  Handle the list similarly
c                 to the part that won't fit.
c
                  top=n
               else
c
c                 Some of it will fit.  Find out how much.
c
                  i=1
                  j=n
c                    Start while block
   60             if (j-i.gt.1) then
                     split=(j+i)/2
C%%                     (*dataop)( 8, List[split], minind, &iflag );
                     call dataop (8,list(split),minind,iflag)
                     if (iflag .ge. 0) then
                        j=split
                     else
                        i=split
                     end if
                     go to 60
c                    End while block
                  end if
                  split=j
c
c                 Write the part that will fit on intermediate scratch.
c
                  do 80 j=split,n
C%%                   (*dataop)( 2, minstr, List[j], &dum );
                     call dataop (2,minstr,list(j),dum)
   80             continue
C%%                 (*dataop)( 7, List[n], minind, &dum );
                  call dataop (7,list(n),minind,dum)
                  top=split-1
               end if
               if (nstrng(2).ne.0) then
c
c                 Determine which intermediate scratch file to use for
c                 the part that won't fit.  The rule is to use the file
c                 with the least strings.  If the number of strings is
c                 the same, use the file with the maximum final datum.
c
                  if (nstrng(1).ne.nstrng(2)) then
                     if (nstrng(outape).ge.nstrng(3-outape))
     1                  outape=3-outape
                  else
                     outape=maxstr
                  end if
C%%                 (*dataop)( 3, outape, 0, &dum );
                  call dataop (3,outape,0,dum)
               else
c
c                 If we are writing the first string on file 2, we must
c                 decrease the available space for sorting.
c
                  outape=2
                  maxdat=maxdat-1
               end if
               nstrng(outape)=nstrng(outape)+1
c
c              Write the part that won't fit on intermediate scratch.
c
               do 100 j=1,top
C%%                 (*dataop)( 2, outape, List[j], &dum );
                  call dataop (2,outape,list(j),dum)
  100          continue
               top=list(top)
c
c              The sorted block has been written on intermediate
c              scratch.c
               go to 200
            end if
         end if
  120    m=head
c           Start while block
  140    if (m.ne.0) then
c                     Output the block
C%%           (*dataop)( kode, outape, m, &iflag );
            call dataop (kode,outape,m,iflag)
            top=m
            m=list(top)
            go to 140
c           End while block
         end if
c                    End of Output the block
         if (outape .eq. 0) then
c                            All done
C%%          (*dataop)( 6, 1, 0, &dum );
            call dataop (6,1,0,dum)
            return
         end if
c                     Continue with sort
  200    continue
c  Test END to see if we need to sort more or we exit the forever block.
         if (end .ne. 0) go to 220
C%%        (*dataop)( 7, top, mx1 - outape, &dum );
         call dataop (7,top,mx1-outape,dum)
         n=0
         if (nstrng(2).ne.0) then
C                              Determine MINSTR etc.
C%%           (*dataop)( 8, minind, maxind, &iflag );
            call dataop (8,minind,maxind,iflag)
            if (iflag .ge. 0) then
               i=maxstr
               maxstr=minstr
               minstr=i
               i=maxind
               maxind=minind
               minind=i
            end if
         end if
      go to 20
c                End forever block
  220 continue
c
c     All of the data have been block-sorted.  Determine whether we
c     need to do a merge.
c
C%%    (*dataop)( 3, 1, 0, &dum );
      call dataop (3,1,0,dum)
C%%    (*dataop)( 5, 1, 0, &dum );
      call dataop (5,1,0,dum)
      if (nstrng(2).eq.0) then
c
c        All of the data are on scratch 1.
c        See what the user wants to do.
c
         if (option.eq.0) then
            outfil=1
            return
         end if
c                Start forever block
  240    continue
C%%          (*dataop)( 4, 1, 1, &iflag );
            call dataop (4,1,1,iflag)
            if (iflag .ne. 0) go to 260
C%%          (*dataop)( 6, 0, 1, &dum );
            call dataop (6,0,1,dum)
         go to 240
c                End forever block
  260    continue
C%%        (*dataop)( 5, 1, 0, &dum );
         call dataop (5,1,0,dum)
      else
c
c        We must do a merge.  Set some values, and then check what
c        kind of output we do for this pass.
c
C%%       (*dataop)( 3, 2, 0, &dum );
         call dataop (3,2,0,dum)
C%%       (*dataop)( 5, 2, 0, &dum );
         call dataop (5,2,0,dum)
         in1=1
c        IN1 is to be the file with the most strings.
         if (nstrng(1).lt.nstrng(2)) in1=2
         in2=3-in1
         out1=3
         out2=4
         m=2
c                Start forever block
  280    continue
            if (nstrng(in1).ne.1) then
               i=in1
               in1=in2
               in2=i
            else
               kode=6
               out1=0
            end if
            outape=1
c
c           Read one record from each file to start the merge.  Sort
c           these records.  Then do the merge by writing the lowest
c           record, reading a new record from the lowest file and
c           re-ordering the records with a partial in-core merge.
c
c                Start forever block
  300       continue
C%%             (*dataop)( 4, *in1, 1, &iflag );
               call dataop (4,in1,1,iflag)
C%%             (*dataop)( 4, *in2, 2, &iflag );
               call dataop (4,in2,2,iflag)
               if (m .ne. 2) then
C%%                (*dataop)( 4, *in3, 3, &iflag );
                  call dataop (4,in3,3,iflag)
C%%                 if (m == 4)
C%%                     (*dataop)( 4, *in4, 4, &iflag );
                  if (m .eq. 4) call dataop (4,in4,4,iflag)
               end if
c              Sort set of first records from each file
               call insrtx (dataop,m,list,head)
c
c              Write current lowest record,and then read a new record
c              from the same file.
c
c                Start forever block
  320          continue
C%%                (*dataop)( kode, Out[outape], head, &dum );
                  call dataop (kode,out(outape),head,dum)
C%%                (*dataop)( 4, In[head], head, &end );
                  call dataop (4,in(head),head,end)
                  i=list(head)
                  if (end .eq. 0) then
c                    if i=0, head is only remaining file
                     if (i.ne.0) then
C%%                      (*dataop)( 8, head, i, &iflag );
                        call dataop (8,head,i,iflag)
                        if (iflag .gt. 0) then
c
c                          Head is no longer lowest.  Merge it with
c                          chain.
c
                           l=head
                           head=i
c                             Start forever block
  340                      continue
                              j=list(i)
                              if (j.eq.0) go to 360
C%%                            (*dataop)( 8, l, j, &iflag );
                              call dataop (8,l,j,iflag)
                              if (iflag .le. 0) go to 360
                              i=j
                              go to 340
c                             End forever block
  360                      continue
                           list(i)=l
                           list(l)=j
                        end if
                     end if
                  else
c
c                    A string has terminated.
c
                     l=in(head)
                     nstrng(l)=nstrng(l)-1
C%%                   if (Nstrng[l] == 0)
C%%                   (*dataop)( 5, l, 0, &dum );
                     if (nstrng(l).eq.0) call dataop (5,l,0,dum)
                     if (i.eq.0) go to 380
                     head=i
                  end if
                  go to 320
c                 End forever block
  380          continue
c
c              All strings have terminated.  If we are doing final
c              output we are done.
c
               if (kode.eq.6) go to 420
c
c              Determine whether to continue the current merge pass or
c              start a new one.
c
               l=out(outape)
               nstrng(l)=nstrng(l)+1
C%%                (*dataop)( 3, l, 0, &dum );
               call dataop (3,l,0,dum)
               j=nstrng(in1)+nstrng(in2)
               if (j.eq.2) then
                  if (nstrng(out1).eq.1) then
c
c                    The total remaining input string count is 2.  The
c                    total output string count is 1 or 2.  We will do
c                    final output with a merge order of 3 or 4
c                    depending on whether the total output string count
c                    is 1 or 2.
c
                     in3=out1
                     m=4
C%%                   (*dataop)( 5, *out1, 0, &dum );
                     call dataop (5,out1,0,dum)
                     if (nstrng(out2).ne.0) then
                        m=5
                        in4=out2
C%%                      (*dataop)( 5, *out2, 0, &dum );
                        call dataop (5,out2,0,dum)
                     end if
                     kode=6
                     out1=0
                     outape=2
                  end if
               else if (nstrng(in1).eq.0) then
                  go to 400
               end if
               outape=3-outape
               m=max(m-1,2)
               go to 300
c              End forever block
  400       continue
c
c           We must start a new merge pass.  Swap input and output
c           files.  If the total remaining input string count is 1,
c           the merge order can be temporarily raised to 3.
c
            m=2
c           NSTRNG(IN2) is always .ge. NSTRNG(IN1).
C%%          (*dataop)( 5, *out1, 0, &dum );
            call dataop (5,out1,0,dum)
            if (nstrng(out2).ne.0) then
               if (nstrng(in2).ne.0) then
                  m=3
                  in3=in2
               end if
C%%             (*dataop)( 5, *out2, 0, &dum );
               call dataop (5,out2,0,dum)
               i=in2
               in2=out2
               out2=i
            end if
            i=in1
            in1=out1
            out1=i
            go to 280
c           End forever block
  420    continue
      end if
c
C%%    (*dataop)( 6, 1, 0, &dum );
      call dataop (6,1,0,dum)
      return
      end
