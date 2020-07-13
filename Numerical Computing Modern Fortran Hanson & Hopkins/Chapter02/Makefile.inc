# Makefile.inc
#
# Define Fortran compiler; assume that the same compiler will compile
# Fortran 77 and Fortran 95 provided the file suffices are ok.
#
# Set (possibly different) flags for the compilation and link stages.
# Note that many compilers use the same flags and just use the file suffix (.f)
# to identify fixed format Fortran.
#
# Provide relevant pattern rules.

# Define Fortran compiler
F95 = nagfor
F77 = $(F95)

# Flags for optimized code
F95OptFlags = -O3
F77SeqOptFlags = -O3 

# Flags for debug code
F95DebugFlags = -gline -u -dcfuns -nan -save -C -C=undefined 
F77SeqDebugFlags = -gline -u -dcfuns -nan -save -C -C=undefined 

F95OPTS = $(F95OptFlags)
F77OPTS = $(F77SeqOptFlags)

# Linking flags: by default use the compilation flags
F95LINKOPTS = $(F95OptFlags)
F77LINKOPTS = $(F77OptFlags)

# May need to define LIBS for Fortran 77 libraries for Lapack and Blas
LIBS =

# Pattern rules for compiling Fortran 95 and Fortran 77 files

# Switch off all suffix rules
.SUFFIXES:

# generate executable depending on .o files
%: %.o
	$(F95) $(F95LINKOPTS) -o $@ $^ $(LIBS)

# Generate .o files from .f95 or .f90
%.o: %.f90
	$(F95) $(F95OPTS) -o $@ -c $<

%.o: %.f95
	$(F95) $(F95OPTS) -o $@ -c $<

%.o: %.f
	$(F77) $(F77OPTS) -o $@ -c $<
