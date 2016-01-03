# This makefile uses features of GNU Make.  Uses bash on Linux.
# Meant to be run from the directory containing the math77 Fortran directory.

# You may need to change the first line above, and the definitions of FC,
# std_flags, and FFLAGS below.  If code is to be used in production,
# you will want to change the optimization level.  (For example change
# -ggdb to -O3 or -O2.

FC = /usr/bin/gfortran
std_flags=-march=native -fimplicit-none -fno-f2c -fno-backslash\
 -funroll-loops  -ffpe-trap=zero,overflow,invalid -finit-real=NAN\
 -ftree-vectorize -floop-interchange -floop-strip-mine -floop-block -pipe

FFLAGS = -ggdb $(std_flags) -Wall
#
objm77 = $(patsubst %.f,%.o, $(wildcard *.f))
#
.PHONY: all
all: libmath77.a
.SUFFIXES:
.SUFFIXES: .f .o
%.o: %.f
	$(FC) -c $(FFLAGS) $(FOPTS) -o $@ $*.f &>>compile.msg

libmath77.a: $(objm77)
	ar r $@ $(objm77) &>>compile.msg

demo: libmath77.a makefile demo/dr$(code).f
	 $(FC) $(FFLAGS) -o demo/dr$(code) demo/dr$(code).f -L. -lmath77;\
 demo/dr$(code);\
 touch makefile

clean:
	rm -f *.o *.a *.msg
# touch lets one run different codes without make thinking all is up to date
