#
# Pull in all the platform specific definitions; for example,
# the name of the Fortran compiler, the required compiler
# options, etc.
# 
# See the annotated Makefile.inc for further details.
#
include ../../Makefile.inc

#
# vpath allows other directories to the searched for the source
# files required to build specific targets. Here we pick up
# basic modules like set_precision from Chapter 2
#
vpath %.f90 ../Chapter02

#
# Define a list of all the target executables that we require for this
# chapter. This is a simple use of make; we could extend this to
# run the executables and compare the results obtained by a set
# of oracle files.
#
EXECUTABLES = QuickMain QuickMainIn qstypeSepMain qstypeExtMain \
     concreteMain concreteDriver

#
# Many targets defined in a Makefile are actual files that are
# created by executing the prescribed commands. Some targets
# however just cause other actions to occur. We mark these as
# PHONY to signify that no file of the target name is to be
# generated. See also the clean target below.
#
.PHONY: all
all: $(EXECUTABLES)

#
# Each of the following targets represents a single executable.
# The set of .o files specified are the dependencies associated
# with the target; these are the .o file needed to be linked
# together to form the executable. (Note: it is assumed that the
# main driver program is in a file called target.f90 or
# target.f95 or target.f. One of the rules in Makefile.inc
# defines how an executable target is made from target.o which
# in turn is generated from a source file target.{f95,f90,f}.
# This process is known as chaining and is a very powerful
# feature of make.
# 
# The order of the .o files is important in some systems. If a
# source file USEs modules contained in other source files then
# the corresponding .o files should appear before the .o file
# that USEs them.
# 
# For example:
#   MODULE set_precision does not USE any other modules.
#
#   MODULE sortDef USEs set_precision
#
#   MODULE usersort USEs sortdef
#
#   MODULE qstypeExt USEs both sortdef and set_precision
#
#   hence the order of .o files given for qstypeExtMain below
#
QuickMain: set_precision.o quicksort.o

QuickMainIn: set_precision.o quicInline.o

qstypeSepMain: set_precision.o sortdef.o qstypeSep.o

qstypeExtMain: set_precision.o sortdef.o usersort.o qstypeExt.o

concreteMain: set_precision.o sortElements.o concrete.o 

concreteDriver: set_precision.o sortElements.o concrete.o 

# Now dependencies of the .o files

concrete.o: set_precision.o sortElements.o

qstypeExt.o: set_precision.o sortdef.o

qstypeSep.o: set_precision.o sortdef.o

quickInline.o: set_precision.o

quicksort.o: set_precision.o

sortdef.o: set_precision.o

usersort.o: sortdef.o
#
# Most makefiles should include a clean target (tagged as PHONY
# as no file called clean is generated -- making the target just
# results in the associated commands being executed).
#
# This gets rid of all the intermediate and associated files
# that have been generated when making the targets. Effectively
# it returns the directory to its original state as far as file
# contents are concerned.
#
# Note that this is important if, for example, optimized
# versions of executables are required after performing testing
# with run-time checking switched on. We should optimise all the
# required .o files.
#
.PHONY: clean
clean:
# First get rid of the .o files and any other detritus the
# compiler has generated.
	rm -f *.o *.mod *.g90
# Now get rid of all the executables
	rm -f $(EXECUTABLES)
