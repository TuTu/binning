# Defining variables ===============================
BINROOT = ../out
EX = binning
EXE = $(BINROOT)/$(EX)
FC = undefined
#MODFLAG = -J
#FFLAGS = $(MODFLAG)$(MODDIR)

#SRCDIR = ./
#OBJDIR = ./obj/
#MODDIR = ../mod/
#LIBDIR = $(SRCDIR)lib/

#PROGRAM = $(OUTDIR)binning

#vpath %.f90 $(SRCDIR)
#vpath %.o $(SRCDIR)
#vpath % $(OUTDIR)

# Define object files
OBJ_SRC = binning.o

#=====================================================================
# Define targets
all:
	@echo "Error - please specify a Fortran compiler"
	@echo "Permissible compilers for this Makefile are:"
	@echo "                                          "
	@echo "  ifort"
	@echo "  gfortran"
	@echo ""

#================== GNU Fortran  ==============================
gfortran:
	$(MAKE) FC="gfortran" LD="gfortran -o" \
	LDFLAGS="-O2" \
	FFLAGS="-c -O2" \
	EX=$(EX) BINROOT=$(BINROOT) exec

#================== Intel Fortran  ==============================
ifort:
	$(MAKE) FC="ifort" LD="ifort -o" \
	LDFLAGS="-O2" \
	FFLAGS="-c -O2" \
	EX=$(EX) BINROOT=$(BINROOT) exec

#=====================================================================
# Default code for parallel (MPI) execution

exec: check $(OBJ_SRC)
	$(LD) $(EX) $(LDFLAGS) $(OBJ_SRC)
	mv $(EX) $(EXE)

#=====================================================================
# Check that a machine has been specified
check:
	@if test $(FC) = "undefined";\
	then echo "You must specify a compiler!"; \
	exit 99;\
	fi

#=====================================================================
# Declare dependencies
%.o: %.f90
	$(FC) $(FFLAGS) $*.f90

#=====================================================================
.PHONY: clean
clean :
	rm -f $(OBJ_SRC) *.mod
