Thanks to Caltech for agreeing to make this software open source.  See
the file "license" for details.  Please familiarize yourself with the
license.

Presumably you have just downloaded the README file (this one) from
/http://netlib.org/math

In the directory docpdf you can start by looking at toc.pdf.  This
gives the table of contents for the libraries.  We would like to point
out the following routines as being state of the art, or of having
functionality beyond what is normal: 8.1, 12.1, 12.2, 12.4, 13.0,
13.1, 13.2, 14.1, 14.2, and 14.3.  The rest are all still useful, and
some may meet needs you would have trouble finding elsewhere.

=====================

This package is designed to make up libraries.  For the Fortran
libraries, download math77.tgz, then "tar xzf math77.tgz" will make up
a subdirectory MATH77 which contains all the Fortran codes, and
demonstration drivers.  In the MATH77 directory, edit "makefile" for
your environment.  It comes set up for gfortran, debugging on, and no
optimization.  You will probably want to make some changes.

Next, "make", will make up the library (libmath77.a).  You can verify
that the demonstration driver that is relevant to your interests works
by typing

make demo code=<name>

where <name> is the root name.  Thus to run the demonstration driver
demo/drdzero.f, you would type "make demo code=dzero", since dzero is
the root name.

To link these routines into your software, in the link step add
"-L?/MATH77 -lmath77" (no quotes, and ? is where you put this).

=====================

For the C libraries, things work much the same.  Download mathc90.tgz
"tar xzf mathc90.tgz" will make up a subdirectory mathc90 which
contains all the c codes and demonstration drivers.  In the mathc90
directory, edit "makefile" for your environment.  It comes set up for
gcc, debugging on, and no optimization.  You will probably want to
make some changes.

Next, "make", will make up the library (libmathc90.a).  You can verify
that the demonstration driver that is relevant to your interests works
by typing

make demo code=<name>

where <name> is the root name.  Thus to run the demonstration driver
demo/drdzero.f, you would type "make demo code=dzero", since dzero is
the root name.

To link these routines into your software, in the link step add
"-L?/mathc90 -lmathc90" (no quotes, and ? is where you put this).

=====================

For documentation, ordinarily you would just download the documents
for the codes you are interested in from docpdf.  But if you should
want to edit the documentation, or would like .dvi files, or would
just like the full set, download doctex.tgz, "tar xzf doctex.tgz", and
from inside the doctex subdirectory type "./texall".  (You may have
set this file executable before trying to execute it.)  This is a bash
script that will make up all the documentation for the library, with
.dvi, .pdf, and .ps files.  Give it a little time, it has a lot to do.

=========

For an unknown, limited time, you can pick up the documentation and code for
individual codes at http://mathalacarte.com.  You are also welcome to throw
large sums of money to Math a la Carte.

Complaints should be directed to fkrogh@mathalacarte.com

