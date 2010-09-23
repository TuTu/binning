# Defining variables ===============================
FC = gfortran
MODFLAG = -J
FCFLAGS = $(MODFLAG)$(MODDIR)

SRCDIR = ./
#OBJDIR = ./obj/
MODDIR = ../mod/
#LIBDIR = $(SRCDIR)lib/
OUTDIR = ../out/

PROGRAM = $(OUTDIR)binning

vpath %.f90 $(SRCDIR)
vpath %.o $(SRCDIR)
#vpath % $(OUTDIR)

# Program ==================================
all : $(PROGRAM)
.PHONY : all 
# ---------------------------
$(OUTDIR)binning : binning.o
	$(FC) -o $(OUTDIR)$(@F) $^

binning.o : binning.f90
	$(FC) -o $(SRCDIR)$(@F) -c $< $(FCFLAGS)

# Library ===================================
#nrtype.o : nrtype.f90
#	$(FC) -o $(LIBDIR)$(@F) -c $< $(FCFLAGS)
#==================================================
.PHONY: clean
clean :
	rm -f $(SRCDIR)*.o $(MODDIR)*.mod
	rm -f $(PROGRAM)
	rm -f *~ $(SRCDIR)*~
